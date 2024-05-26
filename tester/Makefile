all:
	cd ../compiler && sed -i "s/cpp-options: -DDEBUG/-- cpp-options: -DDEBUG/" src/brainlette.cabal && tar -czvf jlc.tar.gz Makefile doc/ src/ lib/ && mv jlc.tar.gz ../tester && sed -i "s/-- cpp-options: -DDEBUG/cpp-options: -DDEBUG/" src/brainlette.cabal
	python testing.py jlc.tar.gz --llvm -x pointers arrays1 arrays2 functions
