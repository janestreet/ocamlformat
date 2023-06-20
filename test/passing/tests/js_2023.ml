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

(** Unexpected blank lines. *)

(*
   List 1:
   - hi
   - bye

   List 2:
   + hello
   + goodbye
*)

(*
   My code:
   {[
     let x = 1
   ]}
   See the code?
*)

(** Unexpected line break. *)

let f =
  g (fun x -> function
    | A -> 1
    | B -> 2)
;;

let f =
  Some
    (fun x -> function
       | A -> 1
       | B -> 2)
;;
