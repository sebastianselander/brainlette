LIB=lib/
DOC=doc/
SRC=src/
JLC=../jlc

all: clean
	cabal install --installdir=. --overwrite-policy=always && cp ./brainlette $(JLC) && rm ./brainlette
clean:
	cabal clean; rm -f $(JLC); rm -f output.ll; rm -f output.bmm
