let f a b c = 1

let f (local_ a) ~foo:(local_ b) ?foo:(local_ c = 1) ~(local_ d) = ()

let f ~(local_ x) ~(local_ y : string) ?(local_ z : string) = ()

let xs = [(fun (local_ a) (type b) ~(local_ c) -> local_ 1)]

let f () = local_
  let a = [local_ 1] in
  let local_ r = 1 in
  let local_ f : 'a. 'a -> 'a = fun x -> local_ x in
  let local_ g a b c : int = 1 in
  let () = g (local_ fun () -> ()) in
  local_ "asdfasdfasdfasdfasdfasdfasdf"

let f () =
  exclave_
  (let a = [exclave_ 1] in
   let local_ r = 1 in
   let local_ f : 'a. 'a -> 'a = fun x -> exclave_ x in
   let local_ g a b c : int = 1 in
   let () = g (exclave_ (fun () -> ())) in
   exclave_ "asdfasdfasdfasdfasdfasdfasdf" )

type 'a r = {mutable a: 'a; b: 'a; global_ c: 'a}

type 'a r =
  | Foo of global_ 'a
  | Bar of 'a * global_ 'a
  | Baz of global_ int * string * global_ 'a

type ('a, 'b) cfn =
  a:local_ 'a -> ?b:local_ b -> local_ 'a -> (int -> local_ 'b)

type loc_attrs = (string[@ocaml.local]) -> (string[@ocaml.local])

let _ = local_ ()

let () = local_ x

let {b} = local_ ()

let () = local_ r

let local_ x : string = "hi"
let (x : string) = local_ "hi"
let x = local_ ("hi" : string)

let x : 'a . 'a -> 'a = local_ "hi"
let local_ f : 'a. 'a -> 'a = "hi"
