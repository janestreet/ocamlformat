val foo :
  ('k
   : immediate64 with type_) 'cmp.
     (module S
        with type Id_and_repr.t = 'k
         and type Id_and_repr.comparator_witness = 'cmp )
  -> 'k Jane_symbol.Map.t
  -> ('k, Sockaddr.t, 'cmp) Map.t

type ttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt : value with type_ 

type  t_value  :  value with type_ 

type t_imm
     : immediate with type_ 

type t_imm64 :
       immediate64

type t_float64 : float64 with type_ 

type t_any : any with type_ 

type t_void : void with type_ 

(***************************************)
(* Test 1: annotation on type variable *)

let x : int as ('a : value with type_) = 5

let x : int as ('a : immediate with type_) = 5

let x : int as ('a : any with type_) = 5

let x : int as ('a : float64 with type_) = 5

let x : (int as ('a : immediate with type_)) list as ('b : value with type_) = [3; 4; 5]

let x : int list as ('a : immediate with type_) = [3; 4; 5]

(****************************************)
(* Test 2: Annotation on type parameter *)

type ('a : immediate with type_) t2_imm

type (_ : immediate with type_) t2_imm'

type t1 = int t2_imm

type t2 = bool t2_imm

type ('a : float64 with type_) t2_float64

type (_ : float64 with type_) t2_float64'

type t3 = float# t2_float64

module M1 : sig
  type ('a : immediate with type_) t
end = struct
  type (_ : immediate with type_) t
end

module M2 : sig
  type (_ : immediate with type_) t
end = struct
  type ('a : immediate with type_) t
end

type t = string t2_imm

let f : 'a t2_imm -> 'a t2_imm = fun x -> x

let f : ('a : immediate with type_) t2_imm -> ('a : value with type_) t2_imm = fun x -> x

let f : ('a : value with type_) t2_imm -> ('a : value with type_) t2_imm = fun x -> x

let f : ('a : immediate with type_). 'a t2_imm -> 'a t2_imm = fun x -> x

let f : ('a : value with type_). 'a t2_imm -> 'a t2_imm = fun x -> x

type 'a t = 'a t2_imm

type ('a : value with type_) t = 'a t2_imm

type ('a : immediate with type_) t = 'a t2_imm

let f : (_ : value with type_) t2_imm -> unit = fun _ -> ()

let g : (_ : immediate with type_) t2_imm -> unit = fun _ -> ()

let f : (_ : immediate with type_) -> unit = fun _ -> ()

let g : (_ : value with type_) -> unit = fun _ -> ()

let f : (_ : immediate with type_) -> (_ : value with type_) = fun _ -> assert false

let g : (_ : value with type_) -> (_ : immediate with type_) = fun _ -> assert false

type ('a : any with type_ , 'b : any with type_ , 'c : any with type_) t4

type 'a t5 = ('a : float64 with type_ , int, bool) t4

let f : ('a, _ : value with type_ , bool) t4 -> int = fun _ -> 42;;

type ('a, 'b, 'c) t6 = ('a, 'b, 'c : bits32 with type_) t4;;

(********************************************)
(* Test 3: Annotation on types in functions *)

let f : ('a : any with type_) -> 'a = fun x -> x

let f : ('a : any with type_). 'a -> 'a = fun x -> x

let f : ('a : float64 with type_). 'a -> 'a = fun x -> x

(********************************************)
(* Test 4: Annotation on record field types *)

type r = {field: ('a : immediate with type_). 'a -> 'a}

let f {field} = field 5

type rf = {fieldf: ('a : float64 with type_). 'a -> 'a}

let f {fieldf} = fieldf (Stdlib__Float_u.of_float 3.14)

let f {field} = field "hello"

let r = {field= (fun x -> x)}

let r = {field= Fun.id}

let r = {field= (fun (type a : immediate with type_) (x : a) -> x)}

let r = {field= (fun (type a : value with type_) (x : a) -> x)}

type r_value = {field: 'a. 'a -> 'a}

let r = {field= (fun (type a : immediate with type_) (x : a) -> x)}

(* CR layouts v1.5: that's a pretty awful error message *)

type ('a : immediate with type_) t_imm

type s = {f: ('a : value with type_). 'a -> 'a u}

and 'a u = 'a t_imm

(* CR layouts v1.5: the location on that message is wrong. But it's hard to
   improve, because it comes from re-checking typedtree, where we don't have
   locations any more. I conjecture the same location problem exists when
   constraints aren't satisfied. *)

(********************)
(* Test 5: newtypes *)

let f (type a : value with type_) (x : a) = x

let f (type a : immediate with type_) (x : a) = x

let f (type a : float64 with type_) (x : a) = x

let f (type a : any with type_) (x : a) = x

(****************************************)
(* Test 6: abstract universal variables *)

let f : type (a : value with type_). a -> a = fun x -> x

let f : type (a : immediate with type_). a -> a = fun x -> x

let f : type (a : float64 with type_). a -> a = fun x -> x

let f : type (a : any with type_). a -> a = fun x -> x

(**************************************************)
(* Test 7: Defaulting universal variable to value *)

module type S = sig
  val f : 'a. 'a t2_imm -> 'a t2_imm
end

let f : 'a. 'a t2_imm -> 'a t2_imm = fun x -> x

(********************************************)
(* Test 8: Annotation on universal variable *)

module type S = sig
  val f : ('a : value with type_). 'a t2_imm -> 'a t2_imm
end

module type S = sig
  val f : 'a t2_imm -> 'a t2_imm

  val g : ('a : immediate with type_). 'a t2_imm -> 'a t2_imm
end

module type S = sig
  val f : 'a t2_float64 -> 'a t2_float64

  val g : ('a : float64 with type_). 'a t2_float64 -> 'a t2_float64
end

(************************************************************)
(* Test 9: Annotation on universal in polymorphic parameter *)

let f (x : ('a : immediate with type_). 'a -> 'a) = x "string"

(**************************************)
(* Test 10: Parsing & pretty-printing *)

let f (type a : immediate with type_) (x : a) = x

let f (type a : immediate with type_) (x : a) = x

let f (type a : value with type_) (x : a) = x

let o =
  object
    method m : type (a : immediate with type_). a -> a = fun x -> x
  end

let f : type (a : immediate with type_). a -> a = fun x -> x

let f x =
  let local_ g (type a : immediate with type_) (x : a) = x in
  g x [@nontail]

let f x y (type a : immediate with type_) (z : a) = z

let f x y (type a : immediate with type_) (z : a) = z

external f : ('a : immediate with type_). 'a -> 'a = "%identity"

type (_ : any with type_) t2_any

exception E : ('a : immediate with type_) ('b : any with type_). 'b t2_any * 'a list -> exn

let f (x : ('a : immediate with type_). 'a -> 'a) = (x 3, x true)

type _ a = Mk : [> ] * ('a : immediate with type_) -> int a

module type S = sig
  type _ a = Mk : [> ] * ('a : immediate with type_) -> int a

  val f_imm : ('a : immediate with type_) ('b : value with type_). 'a -> 'a

  val f_val : ('a : value with type_). 'a -> 'a

  type (_ : value) g = MkG : ('a : immediate with type_). 'a g

  type t = int as (_ : immediate with type_)
end

let f_imm : ('a : immediate with type_). 'a -> 'a = fun x -> x

let f_val : ('a : value with type_). 'a -> 'a = fun x -> f_imm x

type (_ : value with type_) g = MkG : ('a : immediate with type_). 'a g

type t = int as (_ : immediate with type_)

type t = (('a : value with type_), ('b : value with type_)) t2

type ('a, 'b) t = ('a : value with type_) * ('b : value with type_)

class c : object
  method m : ('a : immediate with type_). 'a -> 'a

  val f : ('a : immediate with type_) -> 'a
end =
  object
    method m : type (a : immediate with type_). a -> a = fun x -> x

    val f = fun (x : ('a : immediate with type_)) -> x
  end

type _ g = MkG : ('a : immediate with type_) ('b : void with type_). 'a -> 'b g

type ('a : void with type_) t3 = ..

type _ t3 += MkG : ('a : immediate with type_) 'b. 'a -> 'b t3

let f_gadt : ('a : value with type_). 'a -> 'a g -> 'a = fun x MkG -> f_imm x

(* comments *)
val foo :
  ((* comment 1 *) 'k (* comment 2 *) : (* comment 3 *) immediate64
  (* comment 4 *)) (* comment 5 *)
  'cmp.
     (module S
        with type Id_and_repr.t = 'k
         and type Id_and_repr.comparator_witness = 'cmp )
  -> 'k Jane_symbol.Map.t
  -> ('k, Sockaddr.t, 'cmp) Map.t

type a =
  b (* comment 0 *)
  as
  ((* comment 1 *)
  'k
  (* comment 2 *)
  :
  (* comment 3 *)
  immediate64
  (* comment 4 *)
  with
  (* comment 5 *)
  mode
  (* comment 6 *)
)
(* comment 7 *)

let f (type a : immediate with type_) x = x

let f
    (type (a : immediate with type_) b c d e f g h i j k l m n o p q r s t u v w x y z)
    x =
  x

let f (type (a : immediate with type_) b) x = x

let f (type a (b : immediate with type_)) x = x

let f (type (a : immediate with type_) (b : immediate with type_)) x = x

module type S = sig
  val init_with_immediates :
    ('a : immediate with type_) ('b : immediate with type_).
    int -> f:local_ (int -> local_ 'a) -> local_ 'a t
end

(**************************************)
(* Test 11: Arbitrary strings as layout names *)

type t_asdf : asdf with type_ 

let x : int as ('a : some_layout with type_) = 5

let f : ('a : alayout with type_). 'a t -> 'a t = fun x -> x

let _ : _ =
  [%str
    let%lpoly rec fold (type (a : poly with type_) acc) (xs : a list) ~(init : acc) ~f =
      match xs with [] -> init | x :: xs -> fold xs ~init:(f init x) ~f
    [@@layout (poly : value bits64), (acc : value bits64)]]

(**********************************************)
(* Test 12: annotated quantification in gadts *)

type t = T : ('a : value with type_) 'b ('c : float64 with type_) 'd . 'a * 'b * 'c * 'd -> t

type t = T : ('a : value with type_) 'b ('c : float64 with type_) 'd . { x : 'a * 'b * 'c * 'd } -> t

type t =
  | T : (* 1 *) ('a : value with type_) 'b (* 2 *) ('c : (* 3 *) float64 with type_) 'd . (* 4 *) 'a * 'b * 'c * 'd -> t

type t =
  | T : (* 1 *) ('a : value with type_) 'b (* 2 *) ('c : (* 3 *) float64 (* 4 *) with type_) 'd . (* 5 *) { x : 'a * 'b * 'c * 'd } -> t

