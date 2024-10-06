(* These tests from the campiler test suite *)

let f = ref (stack_ (fun x -> x))

let f = ref (stack_ (2, 3))

let f = ignore_local (stack_ (2, 3))

let f = ref (stack_ Foo)

let f = ref (stack_ (Bar 42))

let f = ignore_local (stack_ (Bar 42))

let f = ref (stack_ `Foo)

let f = ref (stack_ (`Bar 42))

let f = ignore_local (stack_ (`Bar 42))

let f = ref (stack_ {x = "hello"})

let f = ref (stack_ {x = "hello"})

let f = ignore_local (stack_ {x = "hello"})

let f (r : r) = ref (stack_ r.x)

let f (r : r) = ref (stack_ r.x)

let f (r : r) = ignore_local (stack_ r.x) [@nontail]

let f = ref (stack_ [| 42; 56 |])

let f = ignore_local (stack_ [| 42; 56 |])

let f () = stack_ (3, 5)

let f () = exclave_ stack_ (3, 5)

let f () =
    let g = stack_ (fun x -> x) in
    g 42

let f () =
    (stack_ (fun x -> x)) 42

let f () =
    List.length (stack_ [1; 2; 3])

let f () = stack_ [i for i = 0 to 9]

let f () = stack_ [|i for i = 0 to 9|]

let f () = stack_ (new cla)

class foo cla = object method bar = stack_ {< >} end

let f() = stack_ (object end)

let f() = stack_ (lazy "hello")

module M = struct end
module type S = sig end

let f() = stack_ (module M : S)

let f () =
  let r = ref "hello" in
  let _ = stack_ (r.contents, r.contents) in
  r.contents

let f () =
  let r = "hello" in
  let _ = stack_ (r, r) in
  r

let mk () =
  let r = stack_ { x = [1;2;3]; y = [4;5;6] } in
  r.y

let mk () =
  let r = stack_ { x = [1;2;3]; y = [4;5;6] } in
  r.x
