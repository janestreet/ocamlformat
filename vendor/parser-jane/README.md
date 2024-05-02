# parser-jane
This directory contains a direct copy of files from Jane Street's compiler's
parser. The code is not used in [ocamlformat] at all; it only exists as a base
to perform a merge off of.

## How to merge changes from the compiler's parser
First, in the [vendor/] directory, generate a patchfile
```
diff -ruN parser-jane/ parser-standard/ > changes.patch
```
Then, update the files in [parser-jane/] by running the update script
```
./parser-jane/update.sh {path-to-flambda-backend}
```
Finally, create the new [parser-standard/] by copying [parser-jane/] and applying the patchfile
```
rm -rf parser-standard/
cp -r parser-jane parser-standard/
patch -p1 -d parseer-standard < changes.patch
rm changes.patch
```
