(* Note that this file cannot contain legacy local annotation syntax. If a
   file with legacy local annotations wants to have its syntax erased, call
   ocamlformat once without the --erase-jane-syntax flag to rewrite it into
   the new syntax and then call ocamlformat a second time to erase the
   syntax. *)
let f a b c = 1

let f (local_ a) ~foo:(local_ b) ?foo:(local_ c = 1) ~(local_ d) = ()

let punned_pattern ~(src_pos : [%src_pos]) () = src_pos

let ignored_pattern ~src_pos:(_ : [%src_pos]) () = 1

let destructured_pattern ~src_pos:({pos_fname; _ } : [%src_pos]) () = ()

let in_a_type : src_pos:[%src_pos] -> unit -> Lexing.position = punned_pattern

let in_an_expression = [%src_pos]
