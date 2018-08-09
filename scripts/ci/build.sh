#!/bin/sh
set -e

source_dir="$(mktemp --tmpdir -d "ghc-syb.sdistXXXXXXXXX")"
build_dir="$(mktemp --tmpdir -d "ghc-syb.distXXXXXXXXX")"
mkdir -p "$source_dir"
mkdir -p "$build_dir"

cabal update

cd utils/
cabal sdist --builddir="$build_dir" --output-directory="$source_dir"

cd "$source_dir"
cabal new-build --enable-tests
