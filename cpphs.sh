#!/usr/bin/env bash

mkdir -p examples/mini-haskell/standalone/
cpphs --noline --strip -DEMBEDDED   examples/mini-haskell/original/classy.hs > hs/src/HelVM/Hel/MiniHaskell/Classy.hs
cpphs --noline --strip -DSTANDALONE examples/mini-haskell/original/classy.hs > examples/mini-haskell/standalone/classy.hs

mkdir -p examples/compiler/standalone/
cpphs --noline --strip -DEMBEDDED   examples/compiler/original/typically.hs > hs/src/HelVM/Hel/Compiler/Typically.hs
cpphs --noline --strip -DSTANDALONE examples/compiler/original/typically.hs > examples/compiler/standalone/typically.hs

cpphs --noline --strip -DEMBEDDED   examples/compiler/original/classy.hs > hs/src/HelVM/Hel/Compiler/Classy.hs
cpphs --noline --strip -DSTANDALONE examples/compiler/original/classy.hs > examples/compiler/standalone/classy.hs

cpphs --noline --strip -DEMBEDDED   examples/compiler/original/barely.hs > hs/src/HelVM/Hel/Compiler/Barely.hs
cpphs --noline --strip -DSTANDALONE examples/compiler/original/barely.hs > examples/compiler/standalone/barely.hs

cpphs --noline --strip -DEMBEDDED   examples/compiler/original/effectively.hs > hs/src/HelVM/Hel/Compiler/Effectively.hs
cpphs --noline --strip -DSTANDALONE examples/compiler/original/effectively.hs > examples/compiler/standalone/effectively.hs
