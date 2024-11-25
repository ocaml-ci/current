(* Public API; see Current_github.mli for details of these: *)

module Status : sig
  type t
  type state = [`Error | `Failure | `Pending | `Success ]

  val v : ?description:string -> ?url:Uri.t -> state -> t
end

module CheckRunStatus : sig
  type t
  type action
  type conclusion = [`Failure of string | `Success | `Skipped of string]
  type state = [`Queued | `InProgress | `Completed of conclusion]

  val action: label:string -> description:string -> identifier:string -> action
  val v : ?text:string -> ?summary:string -> ?url:Uri.t -> ?actions:action list -> ?identifier:string -> state -> t
end


module Commit : sig
  type t
  val id : ?ssh:bool -> t -> Current_git.Commit_id.t
  val repo_id : t -> Repo_id.t
  val owner_name : t -> string
  val hash : t -> string
  val committed_date : t -> string
  val message : t -> string
  val pp : t Fmt.t
  val pp_short : t Fmt.t
  val compare : t -> t -> int
  val set_status : t Current.t -> string -> Status.t Current.t -> unit Current.t
  val uri : t -> Uri.t
  val pr_name : t -> string option
  val branch_name : t -> string option
  val pr_fork_branch_name : t -> string option
  val pr_fork_with_owner : t -> string option
end


module CheckRun : sig
  type t

  val set_status : Commit.t Current.t -> string -> CheckRunStatus.t Current.t -> unit Current.t
end

module Ref : sig
  type pr_info = {
    id: int;
    base: string;
    title: string;
    isDraft: bool;
    labels: string list;
    bodyHTML: string;
    branch_name: string;
    fork: string;
  }
  type t = [ `Ref of string | `PR of pr_info ]
  type id = [ `Ref of string | `PR of int ]
  val compare : t -> t -> int
  val pp : t Fmt.t
  val to_git : t -> string
end

module Ref_map : Map.S with type key = Ref.t

type t
type refs
val of_oauth : token:string -> webhook_secret:string -> t
val exec_graphql : ?variables:(string * Yojson.Safe.t) list -> t -> string -> Yojson.Safe.t Lwt.t
val head_commit : t -> Repo_id.t -> Commit.t Current.t
val refs : t -> Repo_id.t -> refs Current.Primitive.t
val default_ref : refs -> string
val webhook_secret : t -> string
val all_refs : refs -> Commit.t Ref_map.t
val head_of : t -> Repo_id.t -> Ref.id -> Commit.t Current.t
val ci_refs : ?staleness:Duration.t -> t -> Repo_id.t -> Commit.t list Current.t
val cmdliner : t Cmdliner.Term.t
val cmdliner_opt : t option Cmdliner.Term.t
val webhook_secret_file : string Cmdliner.Term.t

module Repo : sig
  type nonrec t = t * Repo_id.t

  val id : t -> Repo_id.t
  val ci_refs : ?staleness:Duration.t -> t Current.t -> Commit.t list Current.t
  val head_commit : t Current.t -> Commit.t Current.t
  val pp : t Fmt.t
  val compare : t -> t -> int
end

module type GRAPHQL_QUERY = sig
  type result
  val name : string
  val query : string
  val of_yojson : t -> Repo_id.t -> Yojson.Safe.t -> result
end

module Monitor (Query : GRAPHQL_QUERY) : sig
  val get : t -> Repo_id.t -> Query.result Current.Primitive.t
end

module Anonymous : sig
  val head_of : Repo_id.t -> Ref.t -> Current_git.Commit_id.t Current.t
end

(* Private API *)

val read_file : string -> string
(** [read_file path] is the contents of the file at [path] (just a utility function; should be moved elsewhere). *)

type token = {
  token : (string, [`Msg of string]) result;
  (** A token to include in the "Authorization" header, or an error if we failed to get a token. *)

  expiry : float option;
  (** [token] is valid until this time.
      If [token] is an [Error] then this is the earliest time to try again.
      If [None], [token] does not expire. *)
}

val get_token : t -> (string, [`Msg of string]) result Lwt.t
(** [get_token t] returns the cached token for [t], or fetches a new one if it has expired. *)

val rebuild_webhook : engine:Current.Engine.t
                      -> event:(Webhook_event.checks_api_event)
                      -> get_job_ids:(owner:string -> name:string -> hash:string -> string list)
                      -> Yojson.Safe.t -> unit
(** Call this when we get a "check_run" or "check_suite" webhook event. *)

val input_webhook : Yojson.Safe.t -> unit
(** Call this when we get a "pull_request", "push" or "create" webhook event. *)

val v : get_token:(unit -> token Lwt.t) -> ?app_id:string -> account:string -> webhook_secret:string -> unit -> t
(** [v ~get_token ?app_id] is a configuration that uses [get_token] when it needs to get or
    refresh the API token.
    Note: [get_token] can return a failed token, in which case the expiry time says when to try again.
          If [get_token] instead raises an exception, this is turned into an error token with a 1 minute expiry.
    @param account This is a string used to label point counters in Prometheus. *)
