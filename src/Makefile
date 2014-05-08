SOURCES = $(wildcard *.hs) Grammar/

all: ../jlc

../jlc: $(SOURCES)
	ghc -Wall --make Compiler.hs -o ../jlc -odir build -hidir build

Grammar/: Grammar.cf
	bnfc -d Grammar.cf
	rm Grammar/Doc.txt
	mv Grammar/Doc.tex ../doc/Grammar.tex
	alex -g Grammar/Lex.x
	happy -gca Grammar/Par.y
	rm Grammar/*.bak

.PHONY: clean distclean test
clean:
	-rm -rf build
	-rm -f jlc
	-rm -f a.out
	-rm -rf tmp
	-rm -rf graderTestSuite
distclean: clean
	
