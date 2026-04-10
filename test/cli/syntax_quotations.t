  $ echo profile=default > .ocamlformat

  $ echo 'let c = <[123]> in <[42 + $c]>' > a.ml
  $ echo '#syntax quotations on
  > let c = <[123]> in <[42 + $c]>' > qon.ml
  $ echo '#syntax quotations off
  > let c = <[123]> in <[42 + $c]>' > qoff.ml
  $ echo '#syntax quotations on
  > let c = $xyz' > son.ml
  $ echo '#syntax quotations off
  > let c = $xyz' > soff.ml
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
  $ ocamlformat qoff.ml
  ocamlformat: ignoring "qoff.ml" (syntax error)
  File "qoff.ml", line 2, characters 8-9:
  2 | let c = <[123]> in <[42 + $c]>
              ^
  Error: Syntax error
  [1]

  $ ocamlformat --syntax-quotations qoff.ml
  ocamlformat: ignoring "qoff.ml" (syntax error)
  File "qoff.ml", line 2, characters 8-9:
  2 | let c = <[123]> in <[42 + $c]>
              ^
  Error: Syntax error
  [1]

  $ ocamlformat qon.ml
  #syntax quotations on;;
  
  let c = <[123]> in
  <[42 + $c]>

  $ ocamlformat --syntax-quotations qon.ml
  #syntax quotations on;;
  
  let c = <[123]> in
  <[42 + $c]>
  $ ocamlformat soff.ml
  ocamlformat: ignoring "soff.ml" (syntax error)
  File "soff.ml", line 2, characters 8-9:
  2 | let c = $xyz
              ^
  Error: Syntax error
  [1]

  $ ocamlformat --syntax-quotations soff.ml
  ocamlformat: ignoring "soff.ml" (syntax error)
  File "soff.ml", line 2, characters 8-9:
  2 | let c = $xyz
              ^
  Error: Syntax error
  [1]

  $ ocamlformat son.ml
  #syntax quotations on
  
  let c = $xyz

  $ ocamlformat --syntax-quotations son.ml
  #syntax quotations on
  
  let c = $xyz
