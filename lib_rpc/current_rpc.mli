module Job : sig
  (** Client-side API to contact a job service. *)

  type t = [`Job_8397ef9078537247] Capnp_rpc_lwt.Capability.t
  type id = string

  type status = {
    id : id;
    description : string;
    can_cancel : bool;
    can_rebuild : bool;
  }

  val log : start:int64 -> t -> (string * int64, [> `Capnp of Capnp_rpc.Error.t]) result
  (** [log ~start t] returns bytes from the log starting at offset [start]. *)

  val cancel : t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  val status : t -> (status, [> `Capnp of Capnp_rpc.Error.t]) result

  val rebuild : t -> t
  (** [rebuild t] requests a rebuild of [t] and returns the new job. *)

  val approve_early_start : t -> (unit, [> `Capnp of Capnp_rpc.Error.t]) result
  (* Mark the job as approved to start even if the global confirmation threshold
     would otherwise prevent it. Calling this more than once has no effect. *)
end

module Engine : sig
  (** Client-side API to contact an engine service. *)

  type t = [`Engine_f0961466d2f9bbf5] Capnp_rpc_lwt.Capability.t

  val active_jobs : t -> (Job.id list, [> `Capnp of Capnp_rpc.Error.t]) result
  (** [active_jobs t] lists the OCurrent jobs that are still being used in the pipeline.
      This includes completed jobs, as long as OCurrent is still ensuring they are up-to-date. *)

  val job : t -> Job.id -> Job.t
  (** [job t id] is the job with the given ID. This does not have to be an active job (but only
      active jobs can be rebuilt). If the job ID is unknown, this operation will resolve to a
      suitable error. *)
end

module Impl (Current : S.CURRENT) : sig
  (** This is used on the server-side to provide access to the OCurrent Engine.
      Create an instance of the functor with [module Rpc = Current_rpc.Impl(Current)].
      We use a functor here just to avoid having Current_rpc depend on Current,
      which would be annoying for RPC clients. *)

  val job : engine:Current.Engine.t -> Job.id -> Job.t
  (** [job ~engine id] is a Cap'n Proto job service backed by [engine]. *)

  val engine : Current.Engine.t -> Engine.t
  (** [engine e] is a Cap'n Proto engine service backed by [e]. *)
end
