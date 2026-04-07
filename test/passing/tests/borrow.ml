let f y = borrow_ y

let x = ref (borrow_ x)

let x = ignore_local (borrow_ x)

let f (r : r) = ref (borrow_ r.x)

let f (r : r) = ignore_local (borrow_ r.x) [@nontail]

let f () = borrow_ (3, 5)

let f () =
    let g = borrow_ x in
    g

let f () = List.length (borrow_ [1; 2; 3])

let x = borrow_ (borrow_ x)

let x = borrow_ (let y = 1 in Some y)

let x = borrow_ c#x

let _ = borrow_ (X foo) [@bar]

let _ = borrow_ (X (foo [@bar]))

let _ = borrow_ ((fun x -> x) [@bar])

(* Constructor without arg *)
let x = borrow_ Foo

(* Polymorphic variant *)
let x = borrow_ `Foo

let x = borrow_ (`Bar 42)

(* Array *)
let x = borrow_ [| 1; 2; 3 |]

(* Record *)
let x = borrow_ { x = 1 }

(* Object *)
class foo cla = object method bar = borrow_ {< >} end

let f () = borrow_ (object end)

let f () = borrow_ (new cla)

(* Lazy *)
let x = borrow_ (lazy "hello")

(* Uminus/uplus — not simple_expr, need parens *)
let x = borrow_ (-x)

let x = borrow_ (-42)

let x = borrow_ (+x)

let x = borrow_ (+42)

let x = borrow_ (-(-x))

let x = borrow_ (-(-42))

(* If/then/else — needs parens *)
let x = borrow_ (if x then y else z)

(* Assignment — needs parens *)
let x = borrow_ (r.x <- x)

(* Comments *)
let x = (* 1 *) borrow_ (* 2 *) x
