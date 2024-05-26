LIB=lib/
DOC=doc/
SRC=src/
JLC=../jlc

all:
	rm $(JLC); cabal clean && cabal install --installdir=. --overwrite-policy=always && cp ./brainlette $(JLC)
clean:
	cabal clean
	rm $(JLC)
	rm output.ll
	rm output.bmm
	rm output
