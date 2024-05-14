test:
	cd ../compiler && tar -czvf jlc.tar.gz Makefile doc/ src/ lib/ && mv jlc.tar.gz ../tester
	python testing.py jlc.tar.gz

llvm:
	cd ../compiler && tar -czvf jlc.tar.gz Makefile doc/ src/ lib/ && mv jlc.tar.gz ../tester
	python testing.py jlc.tar.gz --llvm

all:
	cd ../compiler && tar -czvf jlc.tar.gz Makefile doc/ src/ lib/ && mv jlc.tar.gz ../tester
	python testing.py jlc.tar.gz --llvm -x pointers arrays1 arrays2
