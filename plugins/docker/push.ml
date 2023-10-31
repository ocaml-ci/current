open Lwt.Infix

type auth = string * string

type t = auth option

let ( >>!= ) = Lwt_result.bind

let id = "docker-push"

module Key = struct
  type t = {
    tag : string;
    docker_context : string option;
  } [@@deriving to_yojson]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = struct
  type t = {
    image : Image.t;
  }

  let digest { image } =
    Yojson.Safe.to_string @@ `Assoc [
      "image", `String (Image.hash image);
    ]
end

module Outcome = Current.String (* [S.repo_id] *)

let tag_cmd { Key.tag; docker_context } { Value.image } =
  Cmd.docker ~docker_context ["tag"; Image.hash image; tag]

let publish auth job key value =
  Current.Job.start job ~level:Current.Level.Dangerous >>= fun () ->
  Prometheus.Gauge.inc_one Metrics.docker_push_events;
  Current.Process.exec ~cancellable:true ~job (tag_cmd key value) >>= (function
  | Error _ as e -> Lwt.return e
  | Ok () ->
    let { Key.tag; docker_context } = key in
    begin match auth with
      | None -> Lwt.return (Ok ())
      | Some (user, password) ->
        let cmd = Cmd.login ~docker_context user in
        Current.Process.exec ~cancellable:true ~job ~stdin:password cmd
    end >>!= fun () ->
    let cmd = Cmd.docker ~docker_context ["push"; tag] in
    Current.Process.exec ~cancellable:true ~job cmd >>!= fun () ->
    let cmd = Cmd.docker ~docker_context ["image"; "inspect"; tag; "-f"; "{{index .RepoDigests 0}}"] in
    Current.Process.check_output ~cancellable:false ~job cmd >|= Stdlib.Result.map @@ fun id ->
    let repo_id = String.trim id in
    Current.Job.log job "Pushed %S -> %S" tag repo_id;
    repo_id)
  >|= (fun res -> Prometheus.Gauge.inc_one Metrics.docker_push_events; res)

let pp f (key, value) =
  Fmt.pf f "%a; docker push %S"
    Cmd.pp (tag_cmd key value)
    key.Key.tag

let auto_cancel = false
