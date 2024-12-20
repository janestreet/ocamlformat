(* This comment moves for similar reason to existing behavior as shown below.
   In particular, the operator ([@@] / [->]) does not carry a source locaion,
   and the comment attaches to the location of the identifier on the right side
   of the operator rather than the left due to the parenthesis. Fixing this would
   require messing with the comment association logic, which is difficult. *)
type t = A of t1 @@ m1 m2 * t2 @@ m3 m4 * (t3 @ m5 -> t4 @ m6) (* cmt *) @@ m7 m8
type t = A of (t -> u) (* cmt *) @@ m
type t = A of ((t -> u) (* cmt *) -> m)

(* This comment attaches to the [f]; the syntax [(f @ mode1)] doesn't have its own
   location to latch onto. This seems like a rare position to put a comment, so it doesn't
   seem worth changing the parser to be able to differentiate the following two. *)
let (* cmt *) (f @ mode1) (arg1 @ mode2) (arg2 @ mode3) : typ = x
let (* cmt *) (f @ mode) arg = x
let ((* cmt *) f @ mode) arg = x

(* This comment moves due to existing behavior as shown below. *)
let (f @ mode1) (arg1 @ mode2) (arg2 @ mode3) (* cmt *) : typ = x
let f (arg @ mode) (* cmt *) : typ = x
let f (arg : typ) (* cmt *) : typ = x
