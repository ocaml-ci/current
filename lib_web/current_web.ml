open Lwt.Infix

module User = User
module Role = Role
module Site = Site
module Context = Context

let metrics ~engine = object
  inherit Resource.t

  val! can_get = `Monitor

  method! private get _ctx =
    Current.Engine.(update_metrics engine);
    Prometheus.CollectorRegistry.(collect default) >>= fun data ->
    let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data in
    let headers = Cohttp.Header.init_with "Content-Type" "text/plain; version=0.0.4" in
    Utils.Server.respond_string ~status:`OK ~headers ~body ()
end

let set_confirm ~engine = object
  inherit Resource.t

  method! private post ctx body =
    let data = Uri.query_of_encoded body in
    let config = Current.Engine.config engine in
    match List.assoc_opt "level" data |> Option.value ~default:[] with
    | ["none"] ->
      Current.Config.set_confirm config None;
      Utils.Server.respond_redirect ~uri:(Uri.of_string "/") ()
    | [level] ->
      begin match Current.Level.of_string level with
        | Error (`Msg msg) -> Context.respond_error ctx `Bad_request msg
        | Ok level ->
          Current.Config.set_confirm config (Some level);
          Utils.Server.respond_redirect ~uri:(Uri.of_string "/") ()
      end
    | _ -> Context.respond_error ctx `Bad_request "Missing level"
end

let routes engine =
  Routes.[
    nil @--> Main.r ~engine;
    s "index.html" /? nil @--> Main.r ~engine;
    s "pipeline.svg" /? nil @--> Pipeline.r ~engine;
    s "query" /? nil @--> Query.r ~engine;
    s "log-rules" /? nil @--> Log_rules.r;
    s "log-rules" / s "rules.csv" /? nil @--> Log_rules.rules_csv;
    s "metrics" /? nil @--> metrics ~engine;
    s "set" / s "confirm" /? nil @--> set_confirm ~engine;
    s "jobs" /? nil @--> Jobs.r;
    s "logout" /? nil @--> Resource.logout;
    s "css" / s "ansi.css" /? nil @--> Resource.static ~content_type:"text/css" Ansi.css;
    s "css" / str /? nil @--> Resource.crunch ~content_type:"text/css";
    s "js" / str /? nil @--> Resource.crunch ~content_type:"text/javascript";
    s "img" / str /? nil @--> Resource.crunch;
  ] @ Job.routes ~engine

let handle_request ~site _conn request body =
  let meth = Cohttp.Request.meth request in
  let uri = Cohttp.Request.uri request in
  let path = Uri.path uri |> Uri.pct_decode in
  Log.info (fun f -> f "HTTP %s %S" (Cohttp.Code.string_of_method meth) path);
  match Routes.match' site.Site.router ~target:path with
  | Routes.NoMatch -> Utils.Server.respond_not_found ()
  | (FullMatch resource) | (MatchWithTrailingSlash resource) ->
    match meth with
    | `GET -> resource#get_raw site request
    | `POST -> resource#post_raw site request body
    | (`HEAD | `PUT | `OPTIONS | `CONNECT | `TRACE | `DELETE | `PATCH | `Other _) ->
      Utils.Server.respond_error ~status:`Bad_request ~body:"Bad method" ()

let pp_mode f (host, mode) =
  let modes = Conduit_lwt_unix.sexp_of_server mode in
  Sexplib.Sexp.pp_hum f (Sexplib0.Sexp.List [(match host with None -> Atom "*:" | Some host -> Atom (host ^ ":")); modes])

let default_mode = `TCP (`Port 8080)

let ctx_of_host host = 
  match host with
  | None -> Lwt.return None
  | Some host ->
   Lwt.bind (Conduit_lwt_unix.init ~src:host ()) 
      (fun ctx -> Lwt.return (Some (Cohttp_lwt_unix.Net.init ~ctx ())))

let run ?(mode=None, default_mode) site =
  let host, port = mode in
  let callback = handle_request ~site in
  let config = Utils.Server.make ~callback () in
  Log.info (fun f -> f "Starting web server: %a" pp_mode mode);
  Lwt.try_bind
    (fun () -> Lwt.bind (ctx_of_host host) (fun ctx -> Utils.Server.create ?ctx ~mode:port config))
    (fun () -> Lwt.return @@ Error (`Msg "Web-server stopped!"))
    (function
      | Unix.Unix_error(Unix.EADDRINUSE, "bind", _) ->
         Lwt.return @@ Fmt.error_msg "Web-server failed.@ Another program is already using this port %a." pp_mode mode
      | ex -> Lwt.reraise ex
    )

open Cmdliner

let host =
  Arg.value @@
  Arg.(opt (some Arg.string) None) @@
  Arg.info
    ~doc:"The hostname on which to listen for incoming HTTP connections."
    ~docv:"HOST"
    ["host"]

let port =
  Arg.value @@
  Arg.opt Arg.int 8080 @@
  Arg.info
    ~doc:"The port on which to listen for incoming HTTP connections."
    ~docv:"PORT"
    ["port"]
    
let make host port = host, `TCP (`Port port)

let cmdliner =
  Term.(const make $ host $ port)

module Resource = Resource
