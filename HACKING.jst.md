Jane Street `ocamlformat` instructions
======================================

This file describes the shape of the `ocamlformat` repo, with a
specific eye toward making it easy to update `ocamlformat` to deal
with Jane Street extensions.

Overall structure
-----------------

The main implementation of `ocamlformat` lives in the `lib` directory.
This is where the `ocamlformat` executable is driven.

The most important file for our purposes here is `lib/Fmt_ast.ml`, which
defines the functions that actually print the formatted AST.

`ocamlformat` also includes *four* copies of the OCaml parser, all in
`vendor`:

* `parser-upstream` is just a copy of the upstream parser. It does not get
  built in this repo. Instead, it lives here only so that it can be diffed
  with `parser-standard`. This diff is automatically produced in
  `vendor/diff-parsers-upstream-std.patch`. The diff is used to monitor the
  drift between the standard parser and the upstream parser; the patch file
  has been mangled (it is produced by `tools/diff-ocaml` which uses `sed` to
  alter the output of `diff`) and thus cannot be applied by any known tool.
  
    You will not have to interact with `parser-upstream`.
  
* `parser-standard` is meant to be very similar to the parser used in the
  OCaml compiler. Its role in `ocamlformat` is (only) to provide a safety-check:
  we want to make sure that the formatting does not change the AST as parsed by
  the compiler. So after formatting, the parsed AST is checked against the
  original parsed AST to make sure they are the same (modulo some normalization,
  as written in `lib/Normalize_std_ast.ml`). Key point: this parser and its
  representation are *not* pretty-printed and never interact with the
  pretty-printer. The *only* reason this is here is to mimic the behavior of the
  OCaml compiler. Accordingly, it should be as close to the OCaml compiler's
  parser as possible.
  
* `parser-extended` uses an extended parsetree, capable of storing extra
  information useful in preserving the structure of the user's code. It is this
  extended parsetree that gets pretty-printed. The parser here forms part of the
  core implementation of `ocamlformat`.
  
* `parser-recovery` is used for partial parsing, so that a bogus input source
  file does not get mangled. It was an experiment that has been discontinued by
  upstream and is not used with Jane Street. It uses the same parsetree as
  `parser-extended`. A patchfile tracking the changes between `parser-extended`
  and `parser-recovery` is generated. Just accept any changes that accrue there.
  
The directory `vendor/ocaml-common` contains files that are shared between
`parser-standard` and `parser-extended`, like `location.ml`.
  
Testing is done in the `test` directory. TODO: Write more about the testing
setup.

Design considerations
---------------------

Because the value of `parser-standard` is entirely in its ability to mimic the
compiler's parser, we want to keep this parser as close as possible to the
compiler's. We will want to copy over any changes made to the compiler's parser
into this version of `ocamlformat`'s parser.

On the other hand, the `parser-extended` can go off in its own direction: its
parsetree should be designed to make pretty-printing easy. In addition, we want
to make sure that incorporating upstream changes is easy. We thus feel free to
edit the parsetree in `parser-extended`, but we do so by adding new
constructors, not modifying existing ones. In addition, new code should be
marked off with comments like `(* Jane Street extension *)` and `(* End Jane
Street extension *)`. Because of the ability to extend the parsetree here, we do
*not* use jane-syntax in `parser-extended`.

How to update `ocamlformat`
---------------------------

The base branch to work from is called `jane`. Create a branch off of `jane`.

1. Take the patch you wish to support (i.e. some PR in `flambda-backend`).
   Apply any changes to the `ocaml/parsing` directory to the files in
   `vendor/parser-standard`. Remember: this "standard" parser should be as
   close as possible to the compiler's.
   
2. Get `ocamlformat` compiled and passing the tests. If the patch to
   `flambda-backend` was backward compatible, then this should be
   straightforward.
   
3. Edit the parsetree in `vendor/parser-extended/parsetree.mli` to support your
   new syntax. Copy over any changes to the parser and lexer from the
   `flambda-backend` patch, updating the parser's semantic actions as necessary.

4. Edit the pretty-printer in `lib/Fmt_ast.ml` to format your new syntax nicely.

5. Make the minimal changes to `parser-recovery` in order to get `ocamlformat`
   to compile. We do not use this feature within Jane Street (and it will be
   removed when merging with upstream), and so we're just keeping it on life
   support. Expend no extra effort here!

6. Add tests. Get them to pass. TODO: Write more about the testing setup.
