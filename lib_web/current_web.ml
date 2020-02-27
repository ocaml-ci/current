open Lwt.Infix
open Astring

let src = Logs.Src.create "current_web" ~doc:"OCurrent web interface"
module Log = (val Logs.src_log src : Logs.LOG)

module Metrics = struct
  open Prometheus

  let collection_duration_seconds =
    { MetricInfo.
      name = MetricName.v "ocurrent_web_metrics_collection_duration_seconds";
      metric_type = Gauge;
      help = "Time taken to collect Prometheus metrics";
      label_names = [];
    }
end

module Server = Cohttp_lwt_unix.Server

type webhook = string * (Cohttp_lwt.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t)

let dot_to_svg = ("", [| "dot"; "-Tsvg" |])

let errorf fmt =
  fmt |> Fmt.kstrf @@ fun msg -> Error (`Msg msg)

let respond_error status body =
  let headers = Cohttp.Header.init_with "Content-Type" "text/plain" in
  Server.respond_error ~status ~headers ~body ()

type actions = <
  rebuild : (unit -> string) option;
  cancel : (unit -> unit) option;
>

let lookup_actions ~engine job_id =
  let state = Current.Engine.state engine in
  let jobs = state.Current.Engine.jobs in
  match Current.Job_map.find_opt job_id jobs with
  | Some a -> (a :> actions)
  | None ->
    object
      method rebuild = None
      method cancel = None
    end

let get_job ~actions job_id =
  match Current.Job.log_path job_id with
  | Error (`Msg msg) -> respond_error `Bad_request msg
  | Ok path ->
    match Bos.OS.File.read path with
    | Error (`Msg msg) -> respond_error `Internal_server_error msg
    | Ok log ->
      let body = Job.render ~actions ~job_id ~log in
      let headers =
        (* Otherwise, an nginx reverse proxy will wait for the whole log before sending anything. *)
        Cohttp.Header.init_with "X-Accel-Buffering" "no"
      in
      Server.respond ~status:`OK ~headers ~body ()

let cancel_job ~actions _job_id =
  match actions#cancel with
  | None -> respond_error `Bad_request "Job does not support cancel (already finished?)"
  | Some cancel ->
    cancel ();
    Server.respond_redirect ~uri:(Uri.of_string "/") ()

let rebuild_job ~actions _job_id =
  match actions#rebuild with
  | None -> respond_error `Bad_request "Job does not support rebuild"
  | Some rebuild ->
    let new_id = rebuild () in
    Server.respond_redirect ~uri:(Uri.of_string ("/job/" ^ new_id)) ()

let start_job j =
  Current.Job.approve_early_start j;
  let id = Current.Job.id j in
  Server.respond_redirect ~uri:(Uri.of_string ("/job/" ^ id)) ()

let render_svg a =
  let url id = Some (Fmt.strf "/job/%s" id) in
  let dotfile = Fmt.to_to_string (Current.Analysis.pp_dot ~url) a in
  let proc = Lwt_process.open_process_full dot_to_svg in
  Lwt_io.write proc#stdin dotfile >>= fun () ->
  Lwt_io.close proc#stdin >>= fun () ->
  Lwt_io.read proc#stdout >>= fun svg ->
  proc#status >|= function
  | Unix.WEXITED 0 -> Ok svg
  | Unix.WEXITED i -> errorf "dot failed (exit status %d) - is graphviz installed?" i
  | Unix.WSTOPPED i
  | Unix.WSIGNALED i -> errorf "dot crashed (signal %d)" i

let set_confirm config data =
  match List.assoc_opt "level" data |> Option.value ~default:[] with
  | ["none"] ->
    Current.Config.set_confirm config None;
    Server.respond_redirect ~uri:(Uri.of_string "/") ()
  | [level] ->
    begin match Current.Level.of_string level with
      | Error (`Msg msg) -> respond_error `Bad_request msg
      | Ok level ->
        Current.Config.set_confirm config (Some level);
        Server.respond_redirect ~uri:(Uri.of_string "/") ()
    end
  | _ -> respond_error `Bad_request "Missing level"

let handle_request ~engine ~webhooks _conn request body =
  match Lwt.state (Current.Engine.thread engine) with
  | Lwt.Fail ex ->
    let body = Fmt.strf "Engine has crashed: %a" Fmt.exn ex in
    respond_error `Internal_server_error body
  | Lwt.Return `Cant_happen -> assert false
  | Lwt.Sleep ->
    let meth = Cohttp.Request.meth request in
    let uri = Cohttp.Request.uri request in
    let path = Uri.path uri in
    Log.info (fun f -> f "HTTP %s %S" (Cohttp.Code.string_of_method meth) path);
    let path = String.cuts ~sep:"/" ~empty:false path in
    match meth, path with
    | `POST, ["webhooks"; hook] ->
      begin match List.assoc_opt hook (webhooks : webhook list) with
        | Some f -> f request body
        | None -> Server.respond_not_found ()
      end
    | `POST, path ->
      begin
        Cohttp_lwt.Body.to_string body >>= fun data ->
        let data = Uri.query_of_encoded data in
        match List.assoc_opt "csrf" data |> Option.value ~default:[] with
        | [got] when got = Main.csrf_token ->
          begin match path with
            | ["job"; date; log; "rebuild"] ->
              let job_id = Fmt.strf "%s/%s" date log in
              let actions = lookup_actions ~engine job_id in
              rebuild_job ~actions job_id
            | ["job"; date; log; "cancel"] ->
              let job_id = Fmt.strf "%s/%s" date log in
              let actions = lookup_actions ~engine job_id in
              cancel_job ~actions job_id
            | ["job"; date; log; "start"] ->
              let job_id = Fmt.strf "%s/%s" date log in
              begin match Current.Job.lookup_running job_id with
                | Some j -> start_job j
                | None -> respond_error `Bad_request "Job is not awaiting confirmation"
              end
            | ["set"; "confirm"] ->
              set_confirm (Current.Engine.config engine) data
            | ["log-rules"] ->
              Log_rules.handle_post data
            | _ ->
              Server.respond_not_found ()
          end
        | _ ->
          respond_error `Bad_request "Bad CSRF token"
      end
    | `GET, ([] | ["index.html"]) ->
      let body = Main.dashboard engine in
      Server.respond_string ~status:`OK ~body ()
    | `GET, ["job"; date; log] ->
      let job_id = Fmt.strf "%s/%s" date log in
      let actions = lookup_actions ~engine job_id in
      get_job ~actions job_id
    | `GET, ["css"; "style.css"] ->
      Style.get ()
    | `GET, ["pipeline.svg"] ->
      begin
        let state = Current.Engine.state engine in
        render_svg state.Current.Engine.analysis >>= function
        | Ok body ->
          let headers = Cohttp.Header.init_with "Content-Type" "image/svg+xml" in
          Server.respond_string ~status:`OK ~headers ~body ()
        | Error (`Msg msg) ->
          respond_error `Internal_server_error msg
      end
    | `GET, ["query"] ->
      let body = Query.render uri in
      Server.respond_string ~status:`OK ~body ()
    | `GET, ["log-rules"] ->
      Log_rules.render ()
    | `GET, ["metrics"] ->
      Current.Engine.(update_metrics (state engine));
      let data =
        let open Prometheus in
        let t0 = Unix.gettimeofday () in
        let data = Prometheus.CollectorRegistry.(collect default) in
        let t1 = Unix.gettimeofday () in
        let value = Sample_set.sample (t1 -. t0) in
        MetricFamilyMap.add Metrics.collection_duration_seconds (LabelSetMap.singleton [] [value]) data
      in
      let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data in
      let headers = Cohttp.Header.init_with "Content-Type" "text/plain; version=0.0.4" in
      Server.respond_string ~status:`OK ~headers ~body ()
    | (`HEAD | `PUT | `OPTIONS | `CONNECT | `TRACE | `DELETE | `PATCH | `Other _), _ ->
      respond_error `Bad_request "Bad method"
    | _ ->
      Server.respond_not_found ()

let pp_mode f mode =
  Sexplib.Sexp.pp_hum f (Conduit_lwt_unix.sexp_of_server mode)

let default_mode = `TCP (`Port 8080)

let show_webhook (name, _) =
  Logs.info (fun f -> f "Registered webhook at /webhooks/%s" name)

let run ?(mode=default_mode) ?(webhooks=[]) engine =
  let config = Server.make ~callback:(handle_request ~engine ~webhooks) () in
  Log.info (fun f -> f "Starting web server: %a" pp_mode mode);
  List.iter show_webhook webhooks;
  Lwt.try_bind
    (fun () -> Server.create ~mode config)
    (fun () -> Lwt.return @@ Error (`Msg "Web-server stopped!"))
    (function
      | Unix.Unix_error(Unix.EADDRINUSE, "bind", _) ->
        let msg = Fmt.strf "Web-server failed.@ Another program is already using this port %a." pp_mode mode in
        Lwt.return @@ Error (`Msg msg)
      | ex -> Lwt.fail ex
    )

open Cmdliner

let port =
  Arg.value @@
  Arg.opt Arg.int 8080 @@
  Arg.info
    ~doc:"The port on which to listen for incoming HTTP connections."
    ~docv:"PORT"
    ["port"]

let make port = `TCP (`Port port)

let cmdliner =
  Term.(const make $ port)
