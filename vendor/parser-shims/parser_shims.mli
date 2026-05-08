module Format_doc = Format_doc

module List : sig
  include module type of struct include List end

  val find_map : ('a -> 'b option) -> 'a list -> 'b option
  (** @since ocaml-4.10 *)
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

  module Stdlib : sig
    include module type of struct include Stdlib end

    module Int : sig
      include module type of struct include Int end

      val min : int -> int -> int
      (** @since ocaml-4.13.0 *)

      val max : int -> int -> int
      (** @since ocaml-4.13.0 *)
    end
  end

  module Style : sig
    include module type of struct include Misc.Style end

    val as_inline_code: 'a Format_doc.printer -> 'a Format_doc.printer
    val inline_code: string Format_doc.printer
  end

  (** Propositional equality *)
  type (_, _) eq = Refl : ('a, 'a) eq

  val print_see_manual : int list Format_doc.printer
end

module Clflags : sig
  type visible_include =
    { path : string;
      cmx_guaranteed : bool;
    }

  val include_dirs : visible_include list ref
  val hidden_include_dirs : string list ref
  val debug : bool ref
  val unsafe : bool ref
  val open_modules : string list ref
  val absname : bool ref
  val locs : bool ref
  val use_threads : bool ref
  val principal : bool ref
  val recursive_types : bool ref
  val applicative_functors : bool ref
  val for_package : string option ref
  val transparent_modules : bool ref
  val locations : bool ref
  val color : Misc.Color.setting option ref
  val error_style : Misc.Error_style.setting option ref
  val unboxed_types : bool ref
  val no_std_include : bool ref
  val no_auto_include_otherlibs : bool ref
end

module Load_path : sig
  type dir
  type auto_include_callback =
    (dir -> string -> string option) -> string -> string
  type paths =
    { visible : Clflags.visible_include list;
      hidden : string list }
  val init :
    auto_include:auto_include_callback -> visible:Clflags.visible_include list ->
    hidden:string list -> unit
  val get_paths : unit -> paths
  val auto_include_otherlibs : (string -> unit) -> auto_include_callback
end

module Pprintast : sig
  module Doc : sig
    val tyvar : string Format_doc.printer
  end
end
