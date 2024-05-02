#!/bin/bash
set -euo pipefail

if [[ "$#" == "1" ]] ; then
    flambda_backend_dir="$1"
else
    echo "Wrong number of arguments"
    exit 1
fi

parsing_dir="${flambda_backend_dir}/ocaml/parsing"
utils_dir="${flambda_backend_dir}/ocaml/utils"
lex_dir="${flambda_backend_dir}/ocaml/lex"

cd $(dirname $0)
cp "$parsing_dir"/asttypes.mli .
cp "$parsing_dir"/ast_helper.ml .
cp "$parsing_dir"/ast_mapper.ml .
cp "$parsing_dir"/docstrings.ml .
cp "$parsing_dir"/jane_asttypes.ml .
cp "$parsing_dir"/jane_asttypes.mli .
cp "$parsing_dir"/jane_syntax.ml .
cp "$parsing_dir"/jane_syntax.mli .
cp "$parsing_dir"/jane_syntax_parsing.ml .
cp "$parsing_dir"/jane_syntax_parsing.mli .
cp "$utils_dir"/language_extension.ml .
cp "$utils_dir"/language_extension.mli .
cp "$utils_dir"/language_extension_kernel.ml .
cp "$utils_dir"/language_extension_kernel.mli .
cp "$lex_dir"/lexer.mll .
cp "$parsing_dir"/parse.ml .
cp "$parsing_dir"/parser.mly .
cp "$parsing_dir"/parsetree.mli .
cp "$parsing_dir"/printast.ml .
