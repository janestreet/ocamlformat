(* Tests making sure comments and attributes are handled reasonably by
   unboxed tuple printing. *)

(* Attrs around expressions *)
let y = #(z, (z [@attr]))

let y = #(z, z) [@@attr]

let y = #(((42 [@attr]) : int), 42)

(* Comments around expressions *)
let _ = (* baz *) #(42, y)

let _ = #((* baz *) 42, y)

let _ = #(42 (* baz *), y)

let _ = #(42, (* baz *) y)

let _ = #(42, y (* baz *))

let _ = #(42, y) (* baz *)

let _ = (* baz *) #(z, (y : int))

let _ = #((* baz *) z, (y : int))

let _ = #(z (* baz *), (y : int))

let _ = #(z, (* baz *) (y : int))

let _ = #(z, ((* baz *) y : int))

let _ = #(z, (y : (* baz *) int))

let _ = #(z, (y : int (* baz *)))

let _ = #(z, (y : int) (* baz *))

let _ = #(z, (y : int)) (* baz *)

(* Attrs around types *)
type t = #((int[@attr]) * bool)

type t = #(int * (bool[@attr]))

type t = (#(int * bool)[@attr])

type t = #(int * bool) [@@attr]

(* Comments around types *)
type t = (* baz *) #(int * bool)

type t = #((* baz *) int * bool)

type t = #(z (* baz *) int * bool)

type t = #(int (* baz *) * bool)

type t = #(int * (* baz *) bool)

type t = #(int * y (* baz *) bool)

type t = #(int * bool (* baz *))

type t = #(int * bool) (* baz *)

(* Attrs around patterns *)
let #((z [@attr]), y) = ()

let #(z, (42 [@attr])) = ()

let (#(z, 42) [@attr]) = ()

(* Comments around patterns *)
let (* baz *) #(z, y) = ()

let #((* baz *) z, y) = ()

let #(z (* baz *), y) = ()

let #(z, (* baz *) y) = ()

let #(z, y (* baz *)) = ()

let #(z, y) (* baz *) = ()

let (* baz *) #(42, (y : int)) = ()

let #((* baz *) 42, (y : int)) = ()

let #(42 (* baz *), (y : int)) = ()

let #(42, ((* baz *) y : int)) = ()

let #(42, (y : (* baz *) int)) = ()

let #(42, (y : int (* baz *))) = ()

let #(42, (y : int) (* baz *)) = ()

let #(42, (y : int)) (* baz *) = ()
