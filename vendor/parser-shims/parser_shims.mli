module List : sig
  include module type of struct include List end

  val find_map : ('a -> 'b option) -> 'a list -> 'b option
  (** @since ocaml-4.10 *)
end

module Int : sig
  include module type of struct include Int end

  val min : int -> int -> int
  (** @since ocaml-4.13.0 *)

  val max : int -> int -> int
  (** @since ocaml-4.13.0 *)
end

module Misc : sig
  include module type of struct include Misc end

  module Color : sig
    include module type of struct include Color end

    val default_setting : setting
    (** @since ocaml-4.09 *)
  end

  module Error_style : sig
    include module type of struct include Error_style end

    val default_setting : setting
    (** @since ocaml-4.09 *)
  end
end

module Clflags : sig
  val include_dirs : string list ref
  val debug : bool ref
  val unsafe : bool ref
  val open_modules : string list ref
  val absname : bool ref
  val use_threads : bool ref
  val principal : bool ref
  val recursive_types : bool ref
  val applicative_functors : bool ref
  val for_package : string option ref
  val transparent_modules : bool ref
  val locations : bool ref
  val unsafe_string : bool ref
  val color : Misc.Color.setting option ref
  val error_style : Misc.Error_style.setting option ref
  val unboxed_types : bool ref
end
