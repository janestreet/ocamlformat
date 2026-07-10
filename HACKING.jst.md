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

`ocamlformat` also includes two copies of the OCaml parser, all in
`vendor`:

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

Before building
---------------

You will need to create a switch on ocaml5.2.1 and install several
libraries. These commands may work:

```
opam switch create ocamlformat 5.2.1
opam install menhir.20210419 fix ocp-indent bechamel-js alcotest camlp-streams fpath either dune-build-info uuseg ocaml-version stdio cmdliner.2.0.0
```

Building
--------

To build, run `dune build`.

How to update `ocamlformat`
---------------------------

The base branch to work from is called `jane`. Create a branch off of `jane`.

1. The first step is to update `vendor/parser-standard` with the appropriate
   changes from the compiler's `parsing` directory. Don't do this by hand! Have
   a look at `vendor/README.md` for instructions on using our script. The short
   version is to `cd vendor` and then run:
   ```
   $ ./parser-jane/repatch.sh {path-to-oxcaml-repo}
   ```

   You may encounter merge conflicts, or need to manually apply changes the
   script didn't pick up. Remember: this "standard" parser should be as close as
   possible to the compiler's.

   The script will update files in both `vendor/parser-standard/` and
   `vendor/ocaml-common/`. When incorporating new support files from the
   compiler, consider whether than can be shared in `ocaml-common` rather than
   copied into each of the parser directories.  This is typically the case if
   the support module doesn't depend on the parsetree.

2. Get `ocamlformat` compiled and passing the tests. If the patch to `oxcaml`
   was backward compatible, then this should be straightforward. (If your
   changes affect files in `vendor/ocaml-common`, this might not be so
   easy. That's OK. Just move on to the next step.)

3. Edit the parsetree in `vendor/parser-extended/parsetree.mli` to support your
   new syntax. Copy over any changes to the parser and lexer from the `oxcaml`
   patch, updating the parser's semantic actions as necessary. This part is
   manual - there's no script to help.

4. Edit the pretty-printer in `lib/Fmt_ast.ml` to format your new syntax nicely.
   This may require changes to other `lib/` modules, such as `Ast.ml` and
   `Sugar.ml`.

5. Add tests. Get them to pass. See the "Testing" section below.

Testing
-------

To just run your built ocamlformat against a file manually, run
`dune exec ocamlformat -- --enable-outside-detected-project path/to/your/file.ml`.

Run the tests with `dune test`. There are two kinds of tests:

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

Now, run `dune test`. It will discover your new file and suggest edits to
the generated `tests/passing/dune.inc` file to run your new tests. Run
`dune promote` to update `dune.inc`. This will *not* accept your new tests -- it
just allows you to run your new tests.

Then, run `dune test` again to actually run your tests. You will see any changes
necessary to make the tests pass. You can run `dune promote` to accept those
changes.

If you get some cryptic error output with a few lines of the `dune.inc` file, it
is likely that ocamlformat has crashed (e.g. with a parser error) while looking
at your test. The cryptic error output will mention the name of the test. Run
ocamlformat manually on your test file to see the actual error.

Debugging
---------

- To see the `parser-extended` ast coming out of the parser for a given source
  file, use `dune exec tools/printast/printast.exe -- path/to/your/file.ml`.

- It can be useful to visualize the _boxes_ the pretty printer creates to align
  and group code.  To see these, run `ocamlformat` with the `-g` flag.  E.g.,:

  ```
  dune exec ocamlformat -- -g --enable-outside-detected-project testfile.ml
  ```

  This will create a file `/tmp/testfile_boxes.html`, which can be opened in a
  browser to see a visual representation of the boxes.

Validity checking
-----------------

The ocamlformat repo has (at least) two validity checks for repo health:

* The ocamlformat sources themselves must be formatted. You can run this check
with `make fmt` (which will also auto-format `dune-project`).  To reformat files
that are incorrect, run `dune build @fmt --auto-promote`. Running `make test`
runs `make fmt` and `dune runtest` together.  The CI will check both the
formatting check and the ocamlformat tests (but will not update `dune-project`).

* All commits must be signed off. This is easy. When you're done with your
sequence of commits and it's all ready to merge, just run
`git rebase <starting commit> --signoff`, where `<starting commit>` is the
commit before any of your edits. You can often say something like `origin/jane`
or `HEAD~4` or similar.

Fuzzing
-------

The continuous integration (CI) process uses
[OCamlgrammarfuzzer](https://github.com/shogan-ai/ocamlgrammarfuzzer.git) to
prevent regressions in syntax coverage.

### Fuzzing Locally

To fuzz `ocamlformat` locally, you must first install **`ocamlgrammarfuzzer`**.
It's recommended to use the same version as the CI (v0.1) to ensure reproducible
results:

```bash
opam pin add ocamlgrammarfuzzer https://github.com/shogan-ai/ocamlgrammarfuzzer.git#v0.1
```

The fuzzer is integrated into the Makefile's **`fuzz`** target:

```bash
make fuzz
```

This target will build `ocamlformat`, run the fuzzer, display statistics, and
generate two key report files:

* [build/default/test/fuzzer/report.md](build/default/test/fuzzer/report.md):
  classifies failures in the current version of `ocamlformat`.
* [build/default/test/fuzzer/regressions_report.md](build/default/test/fuzzer/regressions_report.md):
  classifies regressions in the current version compared to the previously saved
  state.

If there were no regressions, it update the state file.
This logic is implemented in [`test/fuzzer/run.sh`](test/fuzzer/run.sh).

### Updating Coverage State

Regression detection is performed by comparing the latest fuzzing run against a
previous run, saved in `test/fuzzer/state.dat`.

To set a new baseline (reference state) for comparison, this file must be
updated using the `fuzz-update-state` target:

```bash
make fuzz-update-state
```

The beginning of the `state.dat` file contains metadata that summarizes the run:

```
version: OCAMLGRAMMARFUZZER0
hash: 1f9752ec82afce3e0946465b84a6e5f2
sentences: 490799
valid sentences: 391730
syntax errors: 59851
comment errors: 8552
comments dropped: 8638
internal errors: 32723
---
...
```

The initial lines (`version: ...`, `hash: ...`, `sentences: ...`) identify
the **fuzzer version** and the sentence set being tested, to ensure consistency.
The subsequent lines summarize `ocamlformat`'s behavior against the generated
sentences by counting the number of successes and failures per error class.

When reviewing a pull request that updates the state, a quick look at these
lines can help visually confirm the absence of regressions. The remainder of the
file is fuzzer-specific data.

### Updating Grammar

The fuzzer operates against a fixed grammar saved in
[`test/fuzzer/parser.mly`](test/fuzzer/parser.mly).

To update this fixed grammar file from the source
([`vendor/parser-jane/for-parser-standard/parser.mly`](vendor/parser-jane/for-parser-standard/parser.mly)),
use the **`fuzz-update-grammar`** target:

```bash
make fuzz-update-grammar
```

This should be run **after** any upstream changes to the parser.

### Dune Aliases

The [`test/fuzzer/dune`](test/fuzzer/dune) file defines several aliases for
accessing the fuzzer through `dune`:

* `WITH_FUZZER=true dune build @fuzzer` runs the fuzzer and generates the report files.
* `WITH_FUZZER=true dune build @fuzzer-no-regression` provides a summary and exits with a
  **non-zero code** if any regressions are detected.
* `WITH_FUZZER=true dune build @fuzzer-update-state` checks if the current fuzzer state is
  up-to-date. If a new state is generated, you must run `WITH_FUZZER=true dune promote` to apply
  the changes to `test/fuzzer/state.dat`.

Note: the rules related to fuzzing are enabled only if `WITH_FUZZER=true` to
prevent dune from executing them in default builds (e.g. `dune build`).
