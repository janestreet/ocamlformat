(** Some characters get escaped in comments.

    Exactly one of the following must be misindented to trigger the problem. *)

 (* Here, [my_list=[]]. *)
(* spruce-dev@ *)

(** Unexpected line wrapping. *)

(* xxxxxxxxxxxxxxxxxxxxxxxxxxx [xxxxxxx
   xxxx] xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx [xxxxxxx] *)

(* 0_________10________20________30________40________50________60________70________80
   This is quite a long line, and ocamlformat doesn't auto-wrap it at the end of the max line length until I put a bracket pair like [this] and now we're on a new line.
*)
