LIB=lib/
DOC=doc/
SRC=src/
JLC=../jlc

all:
	rm $(JLC); cabal clean && cabal install --installdir=. --overwrite-policy=always && cp ./brainlette $(JLC)
clean:
	cabal clean
	rm $(JLC)

# Run with custom program like this: "make run prg=inputs/program.bl"
run:
	cabal build
	cabal run brainlette -- $(prg)
# llvm-link output.ll llvm/runtime.ll -S > output-linked.ll
	cat output.ll | opt -O3 -S |  clang++ -x ir -o output -
	./output

debug:
	cabal run brainlette --ghc-options=-DDEBUG -- $(prg)

link:
	cabal build && cabal run brainlette -- $(prg) | clang -fno-rtti clib/lib.c -w -g -stdlib=libstdc++ -O3 -Iheaders -x ir -o main -
