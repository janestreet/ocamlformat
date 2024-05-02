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
    rm -f changes.patch
}
trap cleanup ERR EXIT

commands=(
    "diff -ruN parser-jane/ parser-standard/ > changes.patch || true"
    "./parser-jane/update.sh $flambda_backend_dir"
    "rm -rf parser-standard/"
    "cp -r parser-jane/ parser-standard/"
    "patch -p1 -d parser-standard/ < changes.patch"
)

for cmd in "${commands[@]}"
do
    echo "> $cmd"
    eval $cmd
done
