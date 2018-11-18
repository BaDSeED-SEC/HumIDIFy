module Ast = LangAst

module IntM    : Map.S with type key = int
module M       : Map.S with type key = string
module S       : Set.S with type elt = string
module DigestS : Set.S with type elt = Digest.t
module DigestM : sig
  type t

  val empty : t

  val add   : string -> Digest.t -> t -> t
  val mem   : string -> Digest.t -> t -> bool
  val iter  : (string -> DigestS.t -> unit) -> t -> unit
end
module Option  : sig

  val bind      : 'a option -> ('a -> 'b option) -> 'b option
  val return    : 'a -> 'a option

  val (>>=)     : 'a option -> ('a -> 'b option) -> 'b option
  val (>>|)     : 'a option -> ('a -> 'b) -> 'b option

  val value     : default:'a -> 'a option -> 'a
  val value_exn : 'a option -> 'a

end
module List    : sig

  include module type of List

  val take       : int -> 'a list -> 'a list
  val drop_while : ('a -> bool) -> 'a list -> 'a list
  val take_while : ('a -> bool) -> 'a list -> 'a list
  val fold_while : ('a -> 'b -> [< `Ok of 'a | `Stop of 'a ]) -> 'a -> 'b list -> 'a

end
module String   : sig

  include module type of String

  val filter      : (char -> bool) -> string -> string
  val drop_prefix : string -> int -> string

end
module Log      : sig

  val pr_error : ?code:int -> string -> 'a
  val pr_warn  : ?verbose:bool -> string -> unit
  val pr_info  : ?verbose:bool -> string -> unit

  val pr       : string -> unit
  val pr_bold  : string -> unit

end

val input_lines  : string -> string list
val with_in_file : ?binary:bool -> (in_channel -> 'a) -> string -> 'a

exception Error of string * int * int * int * int

val parse_string   : string -> Ast.t
val parse_channel  : in_channel -> Ast.t
val parse_function : (string -> int -> int) -> Ast.t
val parse_file     : string -> Ast.t
