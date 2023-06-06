type t = Eio.Process.mgr

let id = "docker-service"

module Key = struct
  type t = {
    name : string;
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

module Outcome = Current.Unit

let cmd { Key.name; docker_context } { Value.image } =
  Cmd.docker ~docker_context ["service"; "update"; "--image"; Image.hash image; name]

let publish mgr job key value =
  Current.Job.start job ~level:Current.Level.Dangerous;
  Current.Process.exec ~cancellable:true ~job mgr (cmd key value)

let pp f (key, value) =
  Cmd.pp f (cmd key value)

let auto_cancel = false
