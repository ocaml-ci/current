(** Integration with GitHub. *)

val webhook : engine:Current.Engine.t
              -> get_job_ids:(owner:string -> name:string -> hash:string -> string list)
              -> webhook_secret:string
              -> Current_web.Resource.t
(** GitHub webhook endpoint. This MUST be added to {!Current_web.routes} so that we get notified of events. This webhook handles the events:
 - installation_repositories
 - installation
 - pull_request
 - push
 - create
 - check_run
 - check_suite

Webhook payloads are validated against [webhook_secret].

See {{:https://docs.github.com/en/developers/webhooks-and-events/webhooks/securing-your-webhooks}}

Note that the endpoint is supplied with a callback `get_job_ids.` This is used to determine what job_ids correspond to a commit.
The checks API specifies the repository and the commit (that the action is being requested against) - this callback is
used to determine what jobs to apply the action to.

Permissions are managed by the checks API so that re-runs can only be requested by anyone with write
permission to a repository.
 *)

(** Identifier for a repository hosted on GitHub. *)
module Repo_id : sig
  type t = {
    owner : string;
    name : string;
  }

  val pp : t Fmt.t

  val compare : t -> t -> int

  val cmdliner : t Cmdliner.Arg.conv
end

(** Access to the GitHub API. *)
module Api : sig
  type t
  (** Configuration for accessing GitHub. *)

  val webhook_secret : t -> string
  (** Webhook secret to validate payloads from GitHub *)

  type refs
  (** Reference information for the repository *)

  module Status : sig
    type t
    (** GitHub commit context status type. *)

    type state = [`Error | `Failure | `Pending | `Success ]

    val v : ?description:string -> ?url:Uri.t -> state -> t
    (** Construct a Status.t *)
  end

  module CheckRunStatus : sig
    type t
    (** CheckRun status type. *)

    type action

    type conclusion = [`Failure of string | `Success | `Skipped of string]
    (** Sub-set of conclusions from GitHub.
        Not supported are action_required, cancelled, neutral, stale, or timed_out. *)

    type state = [`Queued | `InProgress | `Completed of conclusion]

    val action: label:string -> description:string -> identifier:string -> action

    val v : ?text:string -> ?summary:string -> ?url:Uri.t -> ?actions:action list -> ?identifier:string -> state -> t
    (** [v ?text ?summary ?url ?actions ?identifier state] creates a CheckRunStatus with [?text] description, a link to
        the build details at [?url] and an [?identifier] for triggering a rebuild of a job.

        Both [text] and [summary] are limited to 65535 by GitHub. This field will be truncated to this length.
        [actions] is limited to three actions by GitHub.
     *)
  end

  module Commit : sig
    type t

    val id : ?ssh:bool -> t -> Current_git.Commit_id.t
    (** The commit ID, which can be used to fetch it. *)

    val set_status : t Current.t -> string -> Status.t Current.t -> unit Current.t
    (** [set_status commit context status] sets the status of [commit]/[context] to [status]. *)

    val owner_name : t -> string
    (** [owner_name t] is the "owner/name" string identifying the repository. *)

    val repo_id : t -> Repo_id.t
    (** Like [owner_name], but as a [Repo_id.t]. *)

    val hash : t -> string
    (** [hash t] is the Git commit hash of [t]. *)

    val committed_date : t -> string
    (** [committed_date t] is the datetime when [t] was committed *)

    val message : t -> string
    (** [message t] is the Git commit message of [t]. *)

    val pp : t Fmt.t
    (** [pp t] pretty print the commit as "owner/name id hash committed_date (Commit message)" *)

    val pp_short : t Fmt.t
    (** [pp_short t] pretty print the commit as "owner/name hash" *)

    val compare : t -> t -> int

    val uri : t -> Uri.t
    (** [uri t] is a URI for the GitHub web page showing [t]. *)

    val pr_name : t -> string option
    (** [pr_name t] is the name of the ref that the commit belongs to if it is a PR, and None if it is a branch *)

    val branch_name : t -> string option
    (** [branch_name t] is the name of the ref that the commit belongs to if it is a branch, and None if it is a PR *)

    val pr_fork_branch_name : t -> string option
    (** [pr_fork_branch_name t] is the name of the branch of the fork from which the PR originated from, and None if it is a branch *)

    val pr_fork_with_owner : t -> string option
    (** [pr_fork_with_owner t] is the name as "owner/name" of the fork from which the PR originated from, and None if it is a branch *)
  end

  module CheckRun : sig
    type t

    val set_status : Commit.t Current.t -> string -> CheckRunStatus.t Current.t -> unit Current.t
    (** [set_status commit check_name status] sets the status of check_run for [commit]/[context] to [status]. *)
  end

  module Repo : sig
    type nonrec t = t * Repo_id.t

    val id : t -> Repo_id.t
    val pp : t Fmt.t
    val compare : t -> t -> int

    val ci_refs : ?staleness:Duration.t -> t Current.t -> Commit.t list Current.t
    (** [ci_refs t] evaluates to the list of branches and open PRs in [t], excluding gh-pages.
        @param staleness If given, commits older than this are excluded.
                         Note: the main branch commit is always included, even if stale. *)

    val head_commit : t Current.t -> Commit.t Current.t
    (** [head_commit t] evaluates to the commit at the head of the default branch in [t]. *)
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

    val pp : t Fmt.t

    val compare : t -> t -> int

    val to_git : t -> string
    (** [to_git t] is the Git-format string of the ref, e.g."refs/pull/%d/head" *)
  end

  module Ref_map : Map.S with type key = Ref.t

  val of_oauth : token:string -> webhook_secret:string -> t
  (** [of_oauth ~token ~webhook_secret] is a configuration that authenticates to GitHub using [token]. *)

  val exec_graphql : ?variables:(string * Yojson.Safe.t) list -> t -> string -> Yojson.Safe.t Lwt.t
  (** [exec_graphql t query] executes [query] on GitHub. *)

  val head_commit : t -> Repo_id.t -> Commit.t Current.t
  (** [head_commit t repo] evaluates to the commit at the head of the default branch in [repo]. *)

  val head_of : t -> Repo_id.t -> Ref.id -> Commit.t Current.t
  (** [head_of t repo id] evaluates to the commit at the head of [id] in [repo].
      e.g. [head_of t repo (`Ref "refs/heads/master")] *)

  val ci_refs : ?staleness:Duration.t -> t -> Repo_id.t -> Commit.t list Current.t
  (** [ci_refs t repo] evaluates to the list of branches and open PRs in [repo], excluding gh-pages.
      @param staleness If given, commits older than this are excluded.
                       Note: the main branch commit is always included, even if stale. *)

  val refs : t -> Repo_id.t -> refs Current.Primitive.t
  (** [refs t repo] is the primitive for all the references in [repo].
      This is the low-level API for getting the refs.
      It is used internally by [ci_refs] and [head_of] but in some cases you may want to use it directly,
      [default_ref] and [all_refs] will expose useful information for you.
      The result is cached (so calling it twice will return the same primitive). *)

  val default_ref : refs -> string
  (** [default_ref refs] will return the full name of the repository's default branch ref *)

  val all_refs : refs -> Commit.t Ref_map.t
  (** [all_refs refs] will return a map of all the repository's refs *)


  (** A GraphQL query to be monitored for changes *)
  module type GRAPHQL_QUERY = sig
    type result
    (** The result type produced by the query *)

    val name : string
    (** A short name describing the query (for logging) *)

    val query : string
    (** The GraphQL query that will be monitored. The variable [$owner] and [$name]
        are available and bound to the repository's owner and name.

        Furthermore, this [query] will be wrapped inside a template to also
        report the rate limitations:

          {[
            query($owner: String!, $name: String!) {
              rateLimit { ... }
              <<query>>
            }
          ]}
    *)

    val of_yojson : t -> Repo_id.t -> Yojson.Safe.t -> result
    (** [of_yojson t repo json] parses the [json] into a [result]. *)
  end

  (** Monitor a GraphQL query for changes on webhooks. *)
  module Monitor (Query : GRAPHQL_QUERY) : sig
    val get : t -> Repo_id.t -> Query.result Current.Primitive.t
    (** [get t repo] is the primitive for observing the result of the GraphQL {!Query.query}.
        The result is cached (so calling it twice will return the same primitive),
        with the same lifetime as [t]. *)
  end

  (** Perform Anonymous request to GitHub. *)
  module Anonymous : sig
    val head_of : Repo_id.t -> Ref.t -> Current_git.Commit_id.t Current.t
    (** [head_of repo ref] is the head commit of [repo]/[ref]. No API token is used to access this,
        so it only works for public repositories. You are responsible for adding a web-hook so
        that [input_webhook] gets called whenever the commit changes. *)
  end

  val cmdliner : t Cmdliner.Term.t
  (** Command-line options to generate a GitHub configuration. *)

  val cmdliner_opt : t option Cmdliner.Term.t
  (** Like [cmdliner], but the argument is optional. *)
end

(** Installation of a GitHub application. *)
module Installation : sig
  type t
  (** Details about a specific installation of a GitHub app. *)

  val account : t -> string
  (** The GitHub account name (organisation or owner) for this installation. *)

  val api : t -> Api.t
  (** Provides access to the API as this installation. *)

  val pp : t Fmt.t
  (** The GitHub account that installed the app. *)

  val repositories : ?include_archived:bool -> t Current.t -> Api.Repo.t list Current.t
  (** [repositories t] evaluates to the list of repositories which the user
      configured for this installation.
      @param include_archived If [false] (the default) then filter out archived repositories. *)

  val compare : t -> t -> int
  (** Order by installation ID. *)
end

(** A GitHub Application. *)
module App : sig
  type t
  (** Configuration for a GitHub application. *)

  val webhook_secret : t -> string
  (** Webhook secret to validate payloads from GitHub. *)

  val cmdliner : t Cmdliner.Term.t
  (** Command-line options to generate a GitHub app configuration. *)

  val cmdliner_opt : t option Cmdliner.Term.t
  (** Like [cmdliner], but the arguments are all optional. *)

  val installation : t -> account:string -> int -> Installation.t
  (** [installation t ~account id] gives access to the API for installation [id].
      Note: this generates a fresh value on each call and should not be used inside
      a pipeline. It is intended to be called once from your [main] function, for
      apps with only a single installation.
      @param account The GitHub account that installed the application. *)

  val installations : t -> Installation.t list Current.t
  (** [installations t] evaluates to the list of installations for this app. *)
end

(** Use GitHub to authenticate users. *)
module Auth : sig
  type t
  (** Configuration for GitHub OAuth single-sign-on. *)

  val v : ?scopes:string list -> client_id:string -> client_secret:string -> unit -> t
  (** Create a configuration using the details provided by GitHub. *)

  val make_login_uri : t -> csrf:string -> Uri.t
  (** Use this as your [~authn] in {!Current_web.Site.v}. *)

  val login : t option -> Current_web.Resource.t
  (** The callback page for logins. Add a route to this from the URL you
      configured when you set up your GitHub OAuth app.
      If [t = None] then the page will tell you how to configure it. *)

  val cmdliner : t option Cmdliner.Term.t
  (** Get the configuration from the command-line. *)
end
