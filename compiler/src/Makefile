LIB=lib/
DOC=doc/
SRC=src/

all:
	rm jlc; cabal clean && cabal build && ln -s dist-newstyle/build/x86_64-linux/ghc-9.6.4/brainlette-0.1.0.0/x/brainlette/build/brainlette/brainlette jlc
clean:
	rm -rf dist-newstyle/
	rm jlc
