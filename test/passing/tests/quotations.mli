#syntax quotations on

type t = <[int]>

type 'a t = <[$'a list -> $'a option]>

val foo : <[int]> expr

type 'a t = <[$'a]>

type 'a t = $(<['a]>)

type 'a t =
  <[    ($'a -> $'a -> $'a -> $'a)
     -> ($'a -> $'a -> $'a -> $'a)
     -> ($'a -> $'a -> $'a -> $'a)
     -> ($'a -> $'a -> $'a -> $'a) ]>

(* These cases are probably impossible *)
type 'a t = $(int * float)

type 'a t = $(#(int * float))

(** Quotes **)

(* Attributes *)

type t = <[(int[@attr])]>

type 'a t = <[('a list[@attr])]>

(* Comments *)

type t = <[int (* post *)]>

type t = <[(* pre *) int]>

type t = <[(* pre *) int (* post *)]>

type t = <[int (* in *) list]>

(* Attributes & comments *)

type t = <[(int[@attr] (* post *))]>

type t = <[((* pre *) int[@attr])]>

(** Splices **)

(* Attributes *)

type t = <[$(int[@attr])]>

type 'a t = <[$('a list[@attr])]>

type 'a t =
  <[($'a option[@attr]) -> ($'a option[@attr]) -> ($'a list[@attr])]>

(* Comments *)

type t = $int (* post *)

type t = $(* pre *) int

type t = $(* pre *) int (* post *)

type t = $(int (* in *) list)

(* Attributes & comments *)

type t = $(int[@attr] (* post *))

type t = $((* pre *) int[@attr])
