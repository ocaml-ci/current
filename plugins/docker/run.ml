type t = {
  proc : Eio.Process.mgr;
  pool : unit Current.Pool.t option;
}

let id = "docker-run"

module Key = struct
  type t = {
    image : Image.t;
    args : string list;
    docker_context : string option;
    run_args : string list;
  }

  let pp_args = Fmt.(list ~sep:sp (quote string))

  let cmd { image; args; docker_context; run_args } =
    Cmd.docker ~docker_context @@ ["run"] @ run_args @ ["--rm"; "-i"; Image.hash image] @ args

  let pp f t = Cmd.pp f (cmd t)

  let digest { image; args; docker_context; run_args } =
    Yojson.Safe.to_string @@ `Assoc [
      "image", `String (Image.hash image);
      "args", [%derive.to_yojson:string list] args;
      "docker_context", [%derive.to_yojson:string option] docker_context;
      "run_args", [%derive.to_yojson:string list] run_args;
    ]
end

module Value = Current.Unit

let build { pool; proc } job key =
  Current.Job.start job ?pool ~level:Current.Level.Average;
  Current.Process.exec ~cancellable:true ~job proc (Key.cmd key)

let pp = Key.pp

let auto_cancel = true
