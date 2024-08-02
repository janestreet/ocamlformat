(* RHS annotated with a type *)
let f (type a) = (() : unit)

let f (type a : immediate) = (() : unit)

let f x y = (() : unit)

(* RHS annotated with a type and mode *)
let f (type a) = (() : unit @@ local)

let f (type a : value) = (() : unit @@ local)

let f x y = (() : unit @@ local)

(* LHS and RHS annotated with the same type *)
let f (type a) : unit = (() : unit)

let f (type a : value) : unit = (() : unit)

let f x y : unit = (() : unit)

(* LHS and RHS annotated with different types *)
let f (type a) : unit = (() : int)

let f (type a : value) : unit = (() : int)

let f x y : unit = (() : int)

(* LHS annotated with a mode, RHS annotated with a type *)
let (f @ local) (type a) = (() : unit)

let (f @ local) (type a : value) = (() : unit)

let (f @ local) x y  = (() : unit)
