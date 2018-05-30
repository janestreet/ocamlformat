let _ = {!e with a; b= c}

let _ = {!(f e) with a; b= c}

let _ =
  { !looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    with a; b= c }

let _ =
  { !looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
    with
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  ; b= c }

let _ = {(a : t) with a; b; c}

let _ = {(f a) with a; b; c}

let _ = {(a ; a) with a; b; c}

type _t =
  { mutable a : int
  ; bb        : string
  ; ccc       : float
  ; a_very_very_long_field_name : int
  }

let _ = { a   = 1
        ; bb  = "2"
        ; ccc = 3.
        ; a_very_very_long_field_name = 4
        }

let _ =
  match x with
  | { a   = 1
    ; bb  = "2"
    ; ccc = 3.
    ; a_very_very_long_field_name = 4
    } ->
    true
  | { a; bb; ccc; a_very_very_long_field_name } ->
    ignore (a, bb, ccc, a_very_very_long_field_name);
    false

let _f { a; bb } = ()

let _f { a
       ; bb = b
       } = ()
