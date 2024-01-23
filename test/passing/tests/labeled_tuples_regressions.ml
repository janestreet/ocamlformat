let foo a =
  match a with
  | ( ~a:None
    , ~b:(Some _)
    , ~c:[ 1; 2 ]
    , ~d:(3 :: [])
    , ~e:{ x : _; y : _ }
    , ~f:42
    , ~g:_
    , ~h:`Baz
    , ~i:(`Bar _)
    , ~j:(1 | 2)
    , ~k:[| 1; 2 |]
    , ~l:(3 : int)
    , ~m:(lazy _)
    , ~n:(module M)
    , ~o:(exception _)
    , ~p:[%bar baz]
    , ~q:M.(A)
    , ~r:M.(A 42) ) -> false
;;

let bar =
  ( ~a:foo
  , ~b:42
  , ~c:(let x = 18 in
        x)
  , ~d:(function
         | x -> x)
  , ~e:(fun x -> x)
  , ~f:(foo 42)
  , ~g:(match () with
        | () -> ())
  , ~h:(try () with
        | _ -> ())
  , ~i:(1, 2)
  , ~j:(~x:1, ~y:2)
  , ~k:None
  , ~l:(Some 42)
  , ~m:`A
  , ~n:(`B 42)
  , ~o:{ x = 42; z = false }
  , ~p:foo.lbl
  , ~q:((foo 42).lbl)
  , ~r:(foo.lbl <- 42)
  , ~s:[| 1; 2 |]
  , ~t:[: 1; 2 :]
  , ~u:[ 1; 2 ]
  , ~v:[ a for a = 1 to 10 ]
  , ~w:(if true then true else false)
  , ~x:(();
        ())
  , ~y:(while true do
          ()
        done)
  , ~z:(for i = 1 to 2 do
          ()
        done)
  , ~z:(42 : int)
  , ~y:(42 :> int)
  , ~x:(42 : int :> bool)
  , ~w:foo#bar
  , ~v:foo #~# bar
  , ~u:(new M.c)
  , ~t:(x <- 2)
  , ~s:{<x = 42; y = false>}
  , ~r:(let module M = N in
       ())
  , ~q:(let exception Ex in
       ())
  , ~p:(assert true) )
;;
