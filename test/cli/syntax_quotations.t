  $ echo profile=default > .ocamlformat

  $ echo 'let c = <[123]> in <[42 + $c]>' > a.ml
  $ echo '#syntax quotations on
  > let c = <[123]> in <[42 + $c]>' > bon.ml
  $ echo '#syntax quotations off
  > let c = <[123]> in <[42 + $c]>' > boff.ml
  $ ocamlformat --syntax-quotations a.ml
  let c = <[123]> in
  <[42 + $c]>

  $ ocamlformat a.ml
  ocamlformat: ignoring "a.ml" (syntax error)
  File "a.ml", line 1, characters 8-9:
  1 | let c = <[123]> in <[42 + $c]>
              ^
  Error: Syntax error
  [1]
  $ ocamlformat --syntax-quotations a.ml
  let c = <[123]> in
  <[42 + $c]>
  $ ocamlformat boff.ml
  ocamlformat: ignoring "boff.ml" (syntax error)
  File "boff.ml", line 2, characters 8-9:
  2 | let c = <[123]> in <[42 + $c]>
              ^
  Error: Syntax error
  [1]

  $ ocamlformat --syntax-quotations boff.ml
  ocamlformat: ignoring "boff.ml" (syntax error)
  File "boff.ml", line 2, characters 8-9:
  2 | let c = <[123]> in <[42 + $c]>
              ^
  Error: Syntax error
  [1]

  $ ocamlformat bon.ml
  #syntax quotations on;;
  
  let c = <[123]> in
  <[42 + $c]>

  $ ocamlformat --syntax-quotations bon.ml
  #syntax quotations on;;
  
  let c = <[123]> in
  <[42 + $c]>
