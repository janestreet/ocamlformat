(* RHS annotated with a type *)
let f (type a) = (() : unit)
let (* 1 *) f (* 2 *) (type a) (* 3 *) = (* 4 *)
  (* 5 *) ((* 6 *) () (* 7 *) : (* 8 *) unit (* 9 *)) (* 10 *)
let[@a1] f (type a) = (()[@a2] : unit[@a3])

let f (type a : immediate) = (() : unit)
let (* 1 *) f (type a (* 2 *) : (* 3 *) immediate (* 4 *)) (* 5 *) = (* 6 *)
  (* 7 *) ( (* 8 *) () (* 9 *) : (* 10 *) unit (* 11 *) ) (* 12 *)
let[@a1] f (type a : immediate) = (()[@a2] : unit[@a3])

let f x y = (() : unit)
let (* 1 *) f (* 2 *) x (* 3 *) y (* 4 *) =
  (* 5 *) ((* 6 *)() (* 7 *) : (* 8 *) unit (* 9 *)) (* 10 *)
let[@a1] f (x[@a2]) (y[@a3]) = (()[@a4] : unit[@a5])

(* RHS annotated with a type and mode *)
let f (type a) = (() : unit @@ local)
let (* 1 *) f (* 2 *) ((* 3 *) type (* 4 *) a (* 5 *)) (* 6 *) = (* 7 *)
  (* 8 *) ( (* 9 *) () : (* 10 *) unit (* 11 *) @@ (* 12 *) local (* 13 *) )
let[@a1] f (type a) = (()[@a2] : unit [@a3] @@ local)

let f (type a : value) = (() : unit @@ local)
let (* 1 *) f (* 2 *) ((* 3 *) type (* 4 *) a (* 5 *) : (* 6 *) value (* 7 *)) (* 8 *) =
  (* 9 *) ((* 10 *) () (* 11 *) : (* 12 *) unit (* 13 *) @@ (* 14 *) local (* 15 *))
  (* 16 *)
let[@a1] f (type a : value) = (()[@a2] : unit[@a3] @@ local)

let f x y = (() : unit @@ local)
let (* 1 *) f (* 2 *) x (* 3 *) y (* 4 *) = (* 5 *)
  ((* 6 *) () (* 7 *) : (* 8 *) unit (* 9 *) @@ (* 10 *) local (* 11 *)) (* 12 *)
let[@a1] f (x[@a2]) (y[@a3]) = (()[@a4] : unit[@a5] @@ local)

(* LHS and RHS annotated with the same type *)
let f (type a) : unit = (() : unit)
let (* 1 *) f (* 2 *) ((* 3 *) type (* 4 *) a (* 5 *)) (* 6 *) : (* 7 *) unit (* 8 *) =
  (* 9 *) ((* 10 *) () (* 11 *) : (* 12 *) unit (* 13 *)) (* 14 *)
let[@a1] f (type a) : unit[@a2] = (()[@a3] : unit[@a4])

let f (type a : value) : unit = (() : unit)
let (* 1 *) f (* 2 *) ((* 3 *) type (* 4 *) a (* 5 *) : (* 6 *) value (* 7 *)) (* 8 *)
  : (* 9 *) unit (* 10 *) =
  (* 11 *) ((* 12 *) () (* 13 *) : (* 14 *) unit (* 15 *)) (* 16 *)
let[@a1] f (type a : value) : unit[@a2] = (()[@a3] : unit[@a4])

let f x y : unit = (() : unit)
let (* 1 *) f (* 2 *) x (* 3 *) y (* 4 *) : (* 5 *) unit (* 6 *) =
  (* 7 *) ((* 8 *) () (* 9 *) : (* 10 *) unit (* 11 *)) (* 12 *)
let[@a1] f (x[@a2]) (y[@a3]) : unit[@a4] = (()[@a5] : unit[@a6])

(* LHS and RHS annotated with different types *)
let f (type a) : unit = (() : int)
let (* 1 *) f (* 2 *) ((* 3 *) type (* 4 *) a (* 5 *)) (* 6 *) : (* 7 *) unit (* 8 *) =
  (* 9 *) ((* 10 *) () (* 11 *) : (* 12 *) int (* 13 *)) (* 14 *)
let[@a1] f (type a) : unit[@a2] = (()[@a3] : int[@a4])

let f (type a : value) : unit = (() : int)
let (* 1 *) f (* 2 *) ((* 3 *) type (* 4 *) a (* 5 *) : (* 6 *) value (* 7 *)) (* 8 *)
  : (* 9 *) unit (* 10 *) =
  (* 11 *) ((* 12 *) () (* 13 *) : (* 14 *) int (* 15 *)) (* 16 *)
let[@a1] f (type a : value) : unit[@a2] = (()[@a3] : int[@a4])

let f x y : unit = (() : int)
let (* 1 *) f (* 2 *) x (* 3 *) y (* 4 *) : (* 5 *) unit (* 6 *) =
  (* 7 *) ( (* 8 *) () (* 9 *) : (* 10 *) int (* 11 *) ) (* 12 *)
let[@a1] f (x[@a2]) (y[@a3]) : unit[@a4] = (()[@a5] : int[@a6])

(* LHS annotated with a mode, RHS annotated with a type *)
let (f @ local) (type a) = (() : unit)
let (* 1 *) ((* 2 *) f (* 3 *) @ (* 4 *) local (* 5 *))
              (* 6 *) ((* 7 *) type (* 8 *) a (* 9 *) ) (* 10 *) =
  (* 11 *) ((* 12 *) () (* 13 *) : (* 14 *) unit (* 15 *)) (* 16 *)
let[@a1] (f @ local) (type a) = (()[@a2] : unit[@a3])

let (f @ local) (type a : value) = (() : unit)
let (* 1 *) ((* 2 *) f (* 3 *) @ (* 4 *) local (* 5 *)) (* 6 *)
              ((* 7 *) type (* 8 *) a (* 9 *) : (* 10 *) value (* 11 *)) (* 12 *) =
  (* 13 *) ((* 14 *) () (* 15 *) : (* 16 *) unit (* 17 *)) (* 18 *)
let[@a1] (f @ local) (type a : value) = (()[@a2] : unit[@a3])

let (f @ local) x y  = (() : unit)
let (* 1 *) ((* 2 *) f (* 3 *) @ (* 4 *) local (* 5 *)) (* 6 *) x (* 7 *) y (* 8 *) =
  (* 9 *) ((* 10 *) () (* 11 *) : (* 12 *) unit (* 13 *) ) (* 14 *)
let[@a1] (f @ local) (x[@a2]) (y[@a3]) = (()[@a4] : unit[@a5])

(* Nested funs *)
let _ = f (fun x -> fun y -> z)
let (* 1 *) _ (* 2 *) =
  (* 3 *) f (* 4 *) ((* 5 *) fun (* 6 *) x (* 7 *) ->
  (* 8 *) fun (* 9 *) y (* 10 *) -> (* 11 *) z (* 12 *) ) (* 13 *)
let[@a1] _ = (f[@a2]) (fun (x[@a3]) -> fun (y[@a4]) -> z[@a5])

let _ = f (fun x : unit -> fun y -> z)
let (* 1 *) _ (* 2 *) =
  (* 3 *) f (* 4 *) ((* 5 *) fun (* 6 *) x (* 7 *) : (* 8 *) unit (* 9 *) ->
  (* 10 *) fun (* 11 *) y (* 12 *) -> (* 13 *) z (* 14 *)) (* 15 *)
let[@a1] _ = (f[@a2]) (fun (x[@a3]) : (unit[@a4]) -> fun (y[@a5]) -> z[@a6])

let _ = f (fun x -> fun y : unit -> z)
let (* 1 *) _ (* 2 *) =
  (* 3 *) f (* 4 *) ((* 5*) fun (* 6 *) x (* 7 *) ->
  (* 8 *) fun (* 9 *) y (* 10 *) : (* 11 *) unit (* 12 *) -> (* 13 *) z (* 14 *)) (* 15 *)
let[@a1] _ = (f[@a2]) (fun (x[@a3]) -> fun (y[@a4]) : (unit[@a5]) -> z[@a6])

let _ = f (fun x : unit -> fun y : unit -> z)
let (* 1 *) _ (* 2 *) =
  (* 3 *) f (* 4 *) ((* 5*) fun (* 6 *) x (* 7 *) : (* 8 *) unit (* 9 *) ->
  (* 10 *) fun (* 11 *) y (* 12 *) : (* 13 *) unit (* 14 *) -> (* 15 *) z)
  (* 17 *)
let[@a1] _ = (f[@a2]) (fun (x[@a3]) : (unit[@a4]) -> fun (y[@a5]) : (unit[@a6]) -> z[@a7])

(* CR: Some of the comment movement above are kind of sad. For
   example this:
     let f (* 2 *) (type (* 3 *) a) = (() : unit @@ local)
   becomes:
     let f (type (* 3 *) a) = (* 2 *) (() : unit @@ local)

   Why does (* 2 *) get moved out to the rhs? *)

(* CR: there is a live bug that drops [@a4] in examples like

     let f (type a) = (() : unit) [@a4]

   Add that attribute back to the relevant examples in this file once the bug is fixed.
*)
(* CR the last comments test in this file seems to hit some scale limitation.  If you add
   back the "missing" (* 16 *) after [z], ocamlformat hits its iteration limit:

let (* 1 *) _ (* 2 *) =
  (* 3 *) f (* 4 *) ((* 5*) fun (* 6 *) x (* 7 *) : (* 8 *) unit (* 9 *) ->
  (* 10 *) fun (* 11 *) y (* 12 *) : (* 13 *) unit (* 14 *) -> (* 15 *) z (* 16 *))
  (* 17 *)
*)
