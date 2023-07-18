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
defines the functions that actually print the formatted AST. Other modules
of interest in `lib/`:

* `Fmt.ml` has combinators useful for printing. You're likely to use this
  module but not super likely to change it.
  
* `Ast.ml` has various helpers, mainly for (a) constructing contexts, and (b)
  analyzing whether terms need parentheses. It's common to need to change the
  latter for new syntax.

* `Cmts.ml` deals with comments (not the `cmt` file format!). Comments are not
  places in the extended AST, but rather maintained in a table that is checked
  at various points using the helpers in this module.
  
* `Sugar.ml` has various bits of support code for sugaring/desugaring syntax
  (e.g., "multi-argument" functions).

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
`parser-standard` and `parser-extended`, like `location.ml`.  The `test`
directory contains tests (see the Testing section below).

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

`ocamlformat` routinely checks, at various places in `Fmt_ast`, that the thing
it's about to print is an exact subterm of a "context" we're within (some larger
term that we're in the middle of printing). Make sure you've designed your
additions to `parser-extended` so that, while printing, you'll always recur on
exact structural subterms. If you, for example, print an attribute specially
and then try to recur on a term without that attribute, you're in for a bad
time.

How to update `ocamlformat`
---------------------------

The base branch to work from is called `jane`. Create a branch off of `jane`.

1. Take the patch you wish to support (i.e. some PR in `flambda-backend`).
   Apply any changes to the `ocaml/parsing` directory to the files in
   `vendor/parser-standard`. Remember: this "standard" parser should be as
   close as possible to the compiler's.
   
    Note that some files used by both parsers are stored in
   `vendor/ocaml-common` and may need to be updated.  Further, when
   incorporating new support files from the compiler, consider whether than can
   be shared in that directory rather than copied into each of the parser
   directories.  This is typically the case if the support module doesn't depend
   on the parsetree.
   
2. Get `ocamlformat` compiled and passing the tests. If the patch to
   `flambda-backend` was backward compatible, then this should be
   straightforward. (If your changes affect files in `vendor/ocaml-common`, this
   might not be so easy. That's OK. Just move on to the next step.)
   
3. Edit the parsetree in `vendor/parser-extended/parsetree.mli` to support your
   new syntax. Copy over any changes to the parser and lexer from the
   `flambda-backend` patch, updating the parser's semantic actions as necessary.

4. Edit the pretty-printer in `lib/Fmt_ast.ml` to format your new syntax nicely.
   This may require changes to other `lib/` modules, such as `Ast.ml` and
   `Sugar.ml`.

5. Make the minimal changes to `parser-recovery` in order to get `ocamlformat`
   to compile. We do not use this feature within Jane Street (and it will be
   removed when merging with upstream), and so we're just keeping it on life
   support. Expend no extra effort here!

6. Add tests. Get them to pass. See the "Testing" section below.

Testing
-------

Run the tests with `make test`.

First, this will will check is whether the `ocamlformat` sources themselves are
correctly formatted.  You can also check that explicitly by running `make fmt`.
To reformat files that are incorrect, run `dune build @fmt --auto-promote`.

Next it will run the tests.  There are two kinds of tests

1) Correctly formatted files, which ocamlformat is run on to check that there
   are no changes.  We have historically mainly added these, but not for any
   particularly good reason.
2) Incorrectly formatted files, for which the output of ocamlformat is checked
   against a reference.

To add a test, you add one, two or three files depending on what kind of test it
is:

- (Always) Add `tests/passing/tests/foo.ml` (where foo is the name of your new
  test).  This is the file ocamlformat will be run on.
- (Optional) If your file is incorrectly formatted, write the correctly
  formatted version in `tests/passing/tests/foo.ml.ref`.
- (Optional) If it is expected `ocamlformat` will print information to stderr
  when running your test (uncommon) write that output to
  `tests/passing/tests/foo.ml.err`.

Then update the file `tests/passing/dune.inc` to run your test.  Your best bet
is to just find some existing test for which the same collection of optional
files was used, and cargo cult the dune rules from that test.  There will be
three rules (one which runs ocamlformat, one which checks its output, and one
which checks what was printed to stderr).  For example, when supported was added
for `include functor` tests (which use neither optional file), these rules were
added:

```
(rule
 (deps tests/.ocamlformat )
 (package ocamlformat)
 (action
  (with-stdout-to include_functor.ml.stdout
   (with-stderr-to include_functor.ml.stderr
     (run %{bin:ocamlformat} --margin-check %{dep:tests/include_functor.ml})))))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/include_functor.ml include_functor.ml.stdout)))

(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/include_functor.ml.err include_functor.ml.stderr)))
```

Note there is a rule mentioning `include_functor.ml.err` even though no such
file exists.  This checks that there is no stderr output.

If the test involved reformatting an incorrectly formatted file, the second rule
would use the ref file in the diff, like this:

```
(rule
 (alias runtest)
 (package ocamlformat)
 (action (diff tests/include_functor.ml.ref include_functor.ml.stdout)))
```
