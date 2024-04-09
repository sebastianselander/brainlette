LIB=lib/
DOC=doc/
SRC=src/
JLC=../jlc

all:
	rm $(JLC); cabal clean && cabal build && cp dist-newstyle/build/x86_64-linux/ghc-9.6.4/brainlette-0.1.0.0/x/brainlette/build/brainlette/brainlette $(JLC)
clean:
	cabal clean
	rm $(JLC)

# Run with custom program like this: "make run prg=inputs/program.bl"
run:
	cabal run brainlette -- $(prg)

debug:
	cabal run brainlette --ghc-options=-DDEBUG -- $(prg)

