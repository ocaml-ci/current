(* Utilties for generating dot files. *)

val digraph : Format.formatter -> ?fontname:string -> string -> unit

val node : Format.formatter -> ?style:string -> ?shape:string -> ?bg:string -> ?url:string -> ?tooltip:string -> int -> string -> unit
val edge : Format.formatter -> ?style:string -> ?color:string -> int -> int -> unit
val pp_option : (string * string) Fmt.t

val begin_cluster : Format.formatter -> ?label:string -> int -> unit
val end_cluster : Format.formatter -> unit
