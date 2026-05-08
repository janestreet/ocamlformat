(* basic syntax *)

let idx_r () = (.foo)
let idx_r_r () = (.foo.#foo)
let idx_imm x = (.idx_imm(x))
let idx_mut x = (.idx_mut(x))

(* long chains *)

let idx_r () = (.Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890)
let idx_imm x = (.idx_imm(x).#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890)
let idx_mut x = (.idx_mut(x).#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890.#Foo.Bar.baz1234567890)

(* more interesting indices *)

let idx_imm x = (.idx_imm(if x then y else z))
let idx_mut x = (.idx_mut(let%foo x = y in z))
let idx_imm x = (.idx_imm((module M)))
let idx_mut x = (.idx_mut(assert false))

(* more interesting contexts *)

let idx_r () = (.foo), ~foo:(.foo), ~bar:(.foo)
let idx_r_r () = (.foo.#foo) + (let x = y in z)
let idx_imm x = lazy (.idx_imm(x))
let idx_mut x = !(.idx_mut(x))

(* attributes *)

let idx_r () = (.foo) [@attr]
let idx_r_r () = (.foo.#foo) [@attr] [@bttr] [@cttr defg]
let idx_imm x = (.idx_imm(x)) [@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaattr] [@bttr] [@cttr defg]
let idx_mut x = (.idx_mut(x)) [@aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaattr] [@bttr] [@cttr defg]

(* attributes inside *)

let idx_imm x = (.idx_imm(if[@attr] x then y else z))
let idx_mut x = (.idx_mut(let%foo[@attr] x = y in z))
let idx_imm x = (.idx_imm((module M)[@attr]))
let idx_mut x = (.idx_mut((assert false)[@attr]))

(* comments *)

let idx_r () = (* 01 *) ( (* 02 *) . (* 03 *) foo (* 04 *) ) (* 05 *)
let idx_r_r () = (* 06 *) ( (* 07 *) . (* 08 *) foo (* 09 *) .# (* 10 *) foo (* 11 *) ) (* 12 *)
let idx_imm x = ( (* 32 *) .idx_imm (* 33 *) ( (* 34 *) x (* 35 *)) (* 36 *))
let idx_mut x = ( (* 37 *) .idx_mut (* 38 *) ( (* 39 *) x (* 40 *)) (* 41 *))
