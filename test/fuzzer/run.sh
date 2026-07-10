#!/usr/bin/env sh

export WITH_FUZZER=true
if dune build @fuzzer-no-regression; then
    if dune build @fuzzer-update-state; then
        echo ""
        echo "No regressions, baseline has not changed."
    else
        echo '$ dune promote'
        dune promote || exit 1
        echo ""
        echo "There were no regressions, the baseline has been updated."
    fi
else
    echo "There are some regressions to fix."
fi
