#!/bin/bash
set -euo pipefail

if [[ "$#" == "1" ]] ; then
    flambda_backend_dir="$1"
else
    echo "Wrong number of arguments"
    exit 1
fi

cd $(dirname $0)
cd ..

cleanup() {
    rm -rf parser-jane-old
}
trap cleanup ERR EXIT

# Remember the old compiler to get diff3 merging later
cp -r parser-jane parser-jane-old

# Update parser-jane to the new compiler
./parser-jane/update.sh $flambda_backend_dir

for dir in parser-standard ocaml-common; do
    for file in $(ls parser-jane/for-$dir); do
        diff3 -m -L old-tip -L old-compiler -L new-compiler \
              $dir/$file \
              parser-jane-old/for-$dir/$file \
              parser-jane/for-$dir/$file \
              | sed '/^<<<<<<< old-compiler/,/^>>>>>>> new-compiler/{/^<<<<<<< old-compiler/,/^=======/d;/^>>>>>>> new-compiler/d}' \
              > $dir/$file.tmp \
              || true

        mv $dir/$file.tmp $dir/$file
    done

    hg revert $dir/{jbuild,.hgignore.in,LICENSE}
done

cp parser-standard/asttypes.ml{i,}
cp parser-standard/parsetree.ml{i,}
