#!/bin/bash

cpphs --noline --strip -DEMBEDDED   examples/mini-haskell/original/classy.hs > hs/src/HelVM/HelPS/MiniHaskell/Classy.hs
cpphs --noline --strip -DSTANDALONE examples/mini-haskell/original/classy.hs > examples/mini-haskell/standalone/classy.hs
