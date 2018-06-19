let _ = f a (*comment*) ~b (*comment*) ~c:(*comment*) c' ?d ?e ()

let _ =
  let _ =
    f
      (*comment*)
      (let open M in
      let x = x in
      e)
  in
  ()
;;

(** test *)
let x =
  1;
  2

(** testf1 *)

(** testf2 *)

(** test *)
and y =
  2;
  3

(** test *)
and z = 5
;;
