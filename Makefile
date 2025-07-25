.PHONY: all bench build check check-whitespace clean configure exec fast golden haddock hlint hpack install main output repl report run sdist stan stylish test tix update

all: update cpphs fast blynn check_compile

bench:
	rm -f helps-benchmark.tix
	cabal new-bench --jobs -f ghcoptions

build:
	cabal new-build --jobs --enable-profiling -f ghcoptions

check:
	cabal check

check-whitespace:
	git check-whitespace

clean:
	cabal new-clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi
	if test -d .hie; then rm -r .hie; fi

configure:
	rm -f cabal.project.local*
	cabal configure --enable-benchmarks --enable-coverage --enable-tests --disable-library-coverage -f ghcoptions

exec:
	make tix
	cabal new-exec --jobs helps

fast: main report sdist install

golden:
	if test -d .output/golden; then rm -r .output/golden; fi

haddock:
	cabal new-haddock

hlint:
	./hlint.sh

hpack:
	curl -sSL https://github.com/sol/hpack/raw/main/get-hpack.sh | bash

install:
	cabal install all --overwrite-policy=always

main:
	make stylish configure check build test

output:
	if test -d .output; then rm -r .output; fi

repl:
	cabal new-repl lib:helps

report:
	make haddock stan hlint
	./report.sh

run:
	make tix
	cabal new-run --jobs helps

sdist:
	cabal sdist

stan:
	./stan.sh
	mv stan.html docs/reports/stan.html

stylish:
	#curl -sL https://raw.github.com/haskell/stylish-haskell/master/scripts/latest.sh | sh -s "-r -v -i hs"
	stylish-haskell -r -v -i hs

test:
	cabal new-test --jobs --test-show-details=streaming -f ghcoptions

tix:
	rm -f helps.tix

update:
	cabal update

blynn:
	mkdir -p build
	gcc -ansi -O3 -o build/blynn c/blynn.c

cpphs:
	./cpphs.sh

check_compile:
	./check_compile.sh examples/mini-haskell/standalone/classy.hs
	echo 'n' | ./build/blynn .output/actual/mini-haskell/bignum .output/actual/mini-haskell/bignum build/bugnum
