(* This test exercises the rewrite of legacy [local_ T] in type position to
   the modal form [T @ local]. After ocamlformat runs, no [local_] should
   appear anywhere in a type. Value-level uses of [local_] are not exercised
   here -- they are covered by [local.ml]. *)

(* Nolabel domain *)
type t1 = local_ string -> int

(* Labelled domain *)
type t2 = a:local_ int -> string

(* Optional domain *)
type t3 = ?a:local_ int -> string

(* Local return *)
type t4 = int -> local_ string

(* Chained legacy local_ *)
type t5 = local_ a -> local_ b -> local_ c

(* Mixed with existing @ modes: [local] must be inserted in sorted position.
   [contended] < [local], [local] < [once], [local] < [unique]. *)
type t6 = local_ a @ once -> b @ unique
type t7 = local_ a @ contended -> b
type t8 = local_ a @ contended once -> b

(* Inside a [val] signature *)
module type S = sig
  val f : local_ int -> int
  val g : a:local_ int -> b:local_ string -> int
end

(* Inside a let-binding type constraint *)
let h : local_ int -> int = fun x -> x

(* Inside a nested arrow *)
type t9 = (local_ int -> int) -> int

(* Local return preserved through a labelled chain *)
type t10 = a:local_ int -> b:local_ string -> local_ char list
