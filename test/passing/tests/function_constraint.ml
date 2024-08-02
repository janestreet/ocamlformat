let f = fun (type a) -> (() : unit)

let f = fun (type a : immediate) -> (() : unit)

let f = fun (type a) x -> (() : unit)

let f = fun x y -> (() : unit)

let f = fun (type a : value) x -> (() : unit)

let f = fun (type a) -> (() : unit @@ local)

let f = fun (type a : immediate) -> (() : unit @@ local)

let f = fun (type a) x -> (() : unit @@ local)

let f = fun x y -> (() : unit @@ local)

let f = fun (type a : value) x -> (() : unit @@ local)
