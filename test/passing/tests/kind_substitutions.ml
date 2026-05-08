(* Top-level *)

(* Simple substitutions *)

module type S1 = S with kind_ k = kkkk
module type S1' = S with kind_ k := kkkk

module type S2 = S with kind_ k = kkkk and kind_ kk = kkkkk
module type S2' = S with kind_ k := kkkk and kind_ kk := kkkkk

(* Multiple kind substitutions *)

module type S3 = S with kind_ k = kkkk and kind_ kk = kkkkk and kind_ kkk = kkkkkk
module type S3' = S with kind_ k := kkkk and kind_ kk := kkkkk and kind_ kkk := kkkkkk

(* Mix of [=] and [:=] kind substitutions *)
module type S4 = S with kind_ k = kkkk and kind_ kk := kkkkk
module type S4' = S with kind_ k := kkkk and kind_ kk = kkkkk

(* Combined with type constraints *)

module type S5 = S with type t = int and kind_ k = kkkk
module type S5' = S with type t := int and kind_ k := kkkk

module type S6 = S with kind_ k = kkkk and type t = int
module type S6' = S with kind_ k := kkkk and type t := int

(* Combined with module substitutions *)

module type S7 = S with module M = N and kind_ k = kkkk
module type S7' = S with module M := N and kind_ k := kkkk

(* Combined with module type substitutions *)

module type S8 = S with module type T = T2 and kind_ k = kkkk
module type S8' = S with module type T := T2 and kind_ k := kkkk

(* All four kinds of substitutions on one module type *)

module type S9 =
  S with type t = int and module M = N and module type T = T2 and kind_ k = kkkk
module type S9' =
  S with type t := int and module M := N and module type T := T2 and kind_ k := kkkk

(* Mixed [=] and [:=] across substitution kinds *)
module type S10 =
  S with type t = int and module M := N and module type T = T2 and kind_ k := kkkk

(* Longident on lhs *)

module type S11 = S with kind_ M.N.k = kkkk
module type S11' = S with kind_ M.N.k := kkkk

(* Various rhs kind expressions *)

module type S12 = S with kind_ k = (kind_of_ tttttttttt mod mmmmmmmmmmmmmmmmm)
module type S12' = S with kind_ k := (kind_of_ tttttttttt mod mmmmmmmmmmmmmmmmm)

module type S13 = S with kind_ k = (k1 & k2 & k3 mod mmmmm) & k4
module type S13' = S with kind_ k := (k1 & k2 & k3 mod mmmmm) & k4

module type S14 = S with kind_ k = _
module type S14' = S with kind_ k := _

module type S15 = S with kind_ k = (kind_of_ ttttttttttttt -> ttttttttttttttttttttttttttt)
module type S15' = S with kind_ k := (kind_of_ ttttttttttttt -> ttttttttttttttttttttttttttt)

(* Long lines that break *)

module type S16 = S with kind_ k = (((kkkkkkkkkkkkkkkkkkkkkkk & kind_of_ tttttttttttttttttttttttttttttttttt & kind_of_ tttttttttttttttttttttttttttttttttt) mod mmmmmmmmmmmmm mmmmmmmmmmmmmmmm) & _ & kkkkkkkkkkkkkkkkkk)
module type S16' = S with kind_ k := (((kkkkkkkkkkkkkkkkkkkkkkk & kind_of_ tttttttttttttttttttttttttttttttttt & kind_of_ tttttttttttttttttttttttttttttttttt) mod mmmmmmmmmmmmm mmmmmmmmmmmmmmmm) & _ & kkkkkkkkkkkkkkkkkk)

module type S17 =
  S
    with kind_ k1 = (kind_of_ tttttttttttttttttttttttttttttt mod mmmmmmmmmmmmmmm)
    and kind_ k2 = (k1 & k2 & kind_of_ ttttttttttttttttt) mod mmmmmmmmmmmmmmmmmmm
    and type t = (int -> string)
module type S17' =
  S
    with kind_ k1 := (kind_of_ tttttttttttttttttttttttttttttt mod mmmmmmmmmmmmmmm)
    and kind_ k2 := (k1 & k2 & kind_of_ ttttttttttttttttt) mod mmmmmmmmmmmmmmmmmmm
    and type t := (int -> string)

(* Attributes on the surrounding module type *)

module type S18 = S with kind_ k = kkkk [@@deprecated]
module type S18' = S with kind_ k := kkkk [@@deprecated]

module type S19 = (S with kind_ k = kkkk) [@@deprecated]
module type S19' = (S with kind_ k := kkkk) [@@deprecated]

module type S20 = S with kind_ k = kkkk [@@warning "-32"] [@@some_long_attribute_name_that_should_make_this_line_break]
module type S20' = S with kind_ k := kkkk [@@warning "-32"] [@@some_long_attribute_name_that_should_make_this_line_break]

module type S26 = S [@deprecated] with kind_ k = kkkk
module type S26' = S [@deprecated] with kind_ k := kkkk

module type S27 = S [@deprecated] [@warning "-32"] with kind_ k = kkkk [@@some_long_attribute_name_that_should_make_this_line_break]
module type S27' = S [@deprecated] [@warning "-32"] with kind_ k := kkkk [@@some_long_attribute_name_that_should_make_this_line_break]

(* Now with comments *)

module type S21 = S with kind_ (* 1 *) k (* 2 *) = (* 3 *) kkkk (* 4 *)
module type S21' = S with kind_ (* 1 *) k (* 2 *) := (* 3 *) kkkk (* 4 *)

module type S22 = S (* a *) with (* b *) kind_ (* c *) k (* d *) = (* e *) kkkk (* f *)
module type S22' = S (* a *) with (* b *) kind_ (* c *) k (* d *) := (* e *) kkkk (* f *)

module type S23 = S with type t = (* tc *) int and (* and *) kind_ k = (* kc *) kkkk
module type S23' = S with type t := (* tc *) int and (* and *) kind_ k := (* kc *) kkkk

module type S24 = S
  with kind_ k = ( (* paren *) kind_of_ ttttttttt (* k1 *) & (* k2 *) k1 mod (* m1 *) mmmm (* m2 *) )
module type S24' = S
  with kind_ k := ( (* paren *) kind_of_ ttttttttt (* k1 *) & (* k2 *) k1 mod (* m1 *) mmmm (* m2 *) )

module type S25 = S with kind_ (* before lid *) M.N.k (* after lid *) = (* manifest *) (kkkk & kind_of_ ttt)
module type S25' = S with kind_ (* before lid *) M.N.k (* after lid *) := (* manifest *) (kkkk & kind_of_ ttt)

(* [@@ocamlformat "disable"] preserves source formatting *)

module type S_disabled = S with kind_     k     =
        (kkkkkkkk
       & kind_of_ ttttttttttt
              & _)
   mod    mmmmm     mmmmmm
[@@ocamlformat "disable"]

module type S_disabled' = S with kind_     k     :=
        (kkkkkkkk
       & kind_of_ ttttttttttt
              & _)
   mod    mmmmm     mmmmmm
[@@ocamlformat "disable"]
