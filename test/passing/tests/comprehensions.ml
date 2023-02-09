(*********************************************************************
 * Lists *)

(* Pythagorean triples with components from 1 to 10, no duplicate triples *)
let pyth =
  [ (a, b, c)
      for a = 1 to 10
      for b = a to 10
      for c = b to 10
      when (a * a) + (b * b) = c * c ]

(* Let's describe some objects *)
let descriptions =
  [ Printf.sprintf "a %s %s" adjective noun
      for noun in ["light"; "pepper"]
      and adjective in ["red"; "yellow"; "green"] ]

(* Compute a list of reciprocals in increasing order *)
let reciprocals = [1. /. Float.of_int x for x = 5 downto 0 when x <> 0]

(* Flatten a nested array *)
let flattened =
  let sentences =
    [ ["hello"; "world"]
    ; ["how"; "are"; "you"; "doing"]
    ; ["please"; "enjoy"; "these"; "comprehensions"] ]
  in
  [word for sentence in sentences for word in sentence]

(* We could use comprehensions to reimplement map... *)
let map' ~f l = [f x for x in l]

(* ...and filter *)
let filter' ~f l = [x for x in l when f x]

(*********************************************************************
 * Arrays *)

(* Pythagorean triples with components from 1 to 10, no duplicate triples *)
let pyth =
  [| (a, b, c)
       for a = 1 to 10
       for b = a to 10
       for c = b to 10
       when (a * a) + (b * b) = c * c |]

(* Let's describe some objects *)
let descriptions =
  [| Printf.sprintf "a %s %s" adjective noun
       for noun in [|"light"; "pepper"|]
       and adjective in [|"red"; "yellow"; "green"|] |]

(* Compute a list of reciprocals in increasing order *)
let reciprocals = [|1. /. Float.of_int x for x = 5 downto 0 when x <> 0|]

(* Flatten a nested array *)
let flattened =
  let sentences =
    [| [|"hello"; "world"|]
     ; [|"how"; "are"; "you"; "doing"|]
     ; [|"please"; "enjoy"; "these"; "comprehensions"|] |]
  in
  [|word for sentence in sentences for word in sentence|]

(* We could use comprehensions to reimplement map... *)
let map' ~f l = [|f x for x in l|]

(* ...and filter *)
let filter' ~f l = [|x for x in l when f x|]

(*********************************************************************
 * Immutable arrays *)

(* Pythagorean triples with components from 1 to 10, no duplicate triples *)
let pyth =
  [: (a, b, c)
       for a = 1 to 10
       for b = a to 10
       for c = b to 10
       when (a * a) + (b * b) = c * c :]

(* Let's describe some objects *)
let descriptions =
  [: Printf.sprintf "a %s %s" adjective noun
       for noun in [:"light"; "pepper":]
       and adjective in [:"red"; "yellow"; "green":] :]

(* Compute a list of reciprocals in increasing order *)
let reciprocals = [:1. /. Float.of_int x for x = 5 downto 0 when x <> 0:]

(* Flatten a nested array *)
let flattened =
  let sentences =
    [: [:"hello"; "world":]
     ; [:"how"; "are"; "you"; "doing":]
     ; [:"please"; "enjoy"; "these"; "comprehensions":] :]
  in
  [:word for sentence in sentences for word in sentence:]

(* We could use comprehensions to reimplement map... *)
let map' ~f l = [:f x for x in l:]

(* ...and filter *)
let filter' ~f l = [:x for x in l when f x:]
