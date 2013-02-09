.PHONY: all test clean

all: example tests

test: tests
	./tests

clean: 
	$(RM) *.hi
	$(RM) *.o
	$(RM) Data/*.hi
	$(RM) Data/*.o
	$(RM) example
	$(RM) tests

example: example.hs Data/*.hs
	ghc example

tests: tests.hs Data/*.hs
	ghc tests


