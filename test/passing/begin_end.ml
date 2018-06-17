let _ =
  match a with
  | Some _ -> begin
    match x with A -> match y with B -> true
    end
  | None -> ()

let _ =
  match a with
  | Some _ -> begin
    match x with
    | A -> false
    | B -> true
    | C -> true
    | D -> false
    | E -> false
    end
  | None ->
    match x with
    | A -> false
    | B -> true
    | C -> true
    | D -> false
    | E -> false

let _ =
  try a with
  | Some _ -> begin
    try x with A -> try y with B -> true
    end
  | None -> ()

let _ =
  if b then
    if b then x xxxxxxx xxxxxxxxx
    else if b then xxxxxxxxxxxx
    else yyyyyyyyyyyyyy
  else begin a ; b end

let _ =
  if b then begin
    if b then xxxxxxx xxxxxxxx else if b then x xxxxxxxx xxxxxxxxx end
  else yyyyyyyyyy
