module User : sig
  type t
  (** Information about a site user. *)

  val v : string -> (t, [> `Msg of string]) result
  (** [v id] is a user with ID [id]. *)

  val v_exn : string -> t
  (** [v_exn id] is a user with ID [id]. *)

  val id : t -> string
  (** [id t] is the user ID (e.g. "github:alice") *)
end

module Role : sig
  type t = [
    | `Viewer   (** Read-only access, suitable for the general public. *)
    | `Builder  (** Can cancel and rebuild jobs. *)
    | `Monitor  (** Can read the metrics. *)
    | `Admin    (** Can manage log matcher rules. *)
  ]

  val pp : t Fmt.t
end

module Site : sig
  type t
  (** Site configuration settings. *)

  class type raw_resource = object
    method get_raw : t -> Cohttp.Request.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
    (** Handle an HTTP GET request. *)

    method post_raw : t -> Cohttp.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
    (** Handle an HTTP POST request. *)

    method nav_link : string option
    (** The link label to display in the site navigation bar to this page, if any.
        Note: this is only used for routes without parameters. *)
  end

  val allow_all : (User.t option -> Role.t -> bool)
  (** [allow_all] grants every role to every user. *)

  val v :
    ?name:string ->
    ?authn:(csrf:string -> Uri.t) ->
    ?secure_cookies:bool ->
    ?refresh_pipeline:int ->
    has_role:(User.t option -> Role.t -> bool) ->
    raw_resource Routes.route list -> t
  (** [v ~name ~authn ~has_role routes] is a site named [name] (used for the HTML title, etc)
      that uses [authn] to authenticate users and [has_role] to control what they can do.
      @param authn A link to a login page.
      @param secure_cookies Set secure cookie attribute (turn on if public site uses https).
      @param refresh_pipeline Refresh the pipeline graphs and jobs page each
        [refresh_pipeline] seconds. Defaults to never.
   *)
end

module Context : sig
  type t
  (** The context of a single web request. *)

  val of_request : site:Site.t -> Cohttp.Request.t -> t Lwt.t

  val request : t -> Cohttp.Request.t

  val csrf : t -> string
  (** [csrf t] is the user's CSRF token to include in POST forms. *)

  val set_user : t -> User.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
  (** [set_user t user] records a successful login by [user] and redirects the
      user back to the page they came from. *)

  val respond_ok : t -> ?refresh:int -> [< Html_types.div_content_fun ] Tyxml.Html.elt list -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
  (** [respond_ok ctx refresh content] returns a successful page with [content] inserted into the site template.
    If [refresh] is [Some s], the page is refreshed every [s] seconds. *)

  val respond_redirect : t -> Uri.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
  (** [respond_redirect ctx uri] redirects the user to [uri]. *)

  val respond_error : t -> Cohttp.Code.status_code -> string -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
  (** [respond_error ctx code msg] returns an error message to the user, inside the site template. *)
end

module Resource : sig
  (* A single HTTP resource in the web UI. *)
  class virtual t : object
    inherit Site.raw_resource

    val can_get : Role.t
    (** The role the client needs in order to make a GET request. *)

    val can_post : Role.t
    (** The role the client needs in order to make a POST request. *)

    method private get : Context.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
    (** Concrete resources should override this method to handle GET requests.
        {!get_raw} checks that the caller has the {!can_get} role and then calls this.
        The default method returns a [`Bad_request] error. *)

    method private post : Context.t -> string -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
    (** Concrete resources should override this method to handle POSTs.
        {!get_post} checks that the caller has the {!can_post} role, reads the
        body, checks the CSRF token, and then calls this.
        The default method returns a [`Bad_request] error. *)
  end
end

val routes : Current.Engine.t -> Resource.t Routes.route list
(** [routes engine] is the default routes for a web interface to [engine]. *)

val run : ?mode:(string option * Conduit_lwt_unix.server) -> Site.t -> ('a, [`Msg of string]) result Lwt.t
(** [run ~mode site] runs a web-server (with configuration [mode]) that handles incoming requests for [site]. *)

val cmdliner : (string option * Conduit_lwt_unix.server) Cmdliner.Term.t
