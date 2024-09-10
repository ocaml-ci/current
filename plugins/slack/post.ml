open Lwt.Infix

type t = Uri.t

let id = "slack-post"

module Key = Current.String
module Value = Current.String
module Outcome = Current.Unit

let publish t job _key message =
  Current.Job.start job ~level:Current.Level.Above_average >>= fun () ->
  let headers = Cohttp.Header.of_list [
      "Content-type", "application/json";
    ]
  in
  let body = `Assoc [
      "text", `String message;
    ]
    |> Yojson.to_string
    |> Cohttp_lwt.Body.of_string
  in
  Cohttp_lwt_unix.Client.post ~headers ~body t >>= fun (resp, _body) ->
  match resp.Cohttp_lwt.Response.status with
  | `OK -> Lwt.return @@ Ok ()
  | err ->
     Lwt.return @@ Fmt.error_msg "Slack post failed: %s" (Cohttp.Code.string_of_status err)


let pp f (key, value) = Fmt.pf f "Post %s: %s" key value

let auto_cancel = false
