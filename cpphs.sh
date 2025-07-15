#!/bin/bash

mkdir -p examples/mini-haskell/standalone/
cpphs --noline --strip -DEMBEDDED   examples/mini-haskell/original/classy.hs > hs/src/HelVM/HelPS/MiniHaskell/Classy.hs
cpphs --noline --strip -DSTANDALONE examples/mini-haskell/original/classy.hs > examples/mini-haskell/standalone/classy.hs

mkdir -p examples/compiler/standalone/
cpphs --noline --strip -DEMBEDDED   examples/compiler/original/typically.hs > hs/src/HelVM/HelPS/Compiler/Compiler/Typically.hs
cpphs --noline --strip -DSTANDALONE examples/compiler/original/typically.hs > examples/compiler/standalone/typically.hs

cpphs --noline --strip -DEMBEDDED   examples/compiler/original/classy.hs > hs/src/HelVM/HelPS/Compiler/Compiler/Classy.hs
cpphs --noline --strip -DSTANDALONE examples/compiler/original/classy.hs > examples/compiler/standalone/classy.hs
