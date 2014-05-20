SOURCES = $(wildcard *.hs) build/Grammar

all: ../jlc

../jlc: $(SOURCES)
	ghc -Wall --make Compiler.hs -o ../jlc -odir build -hidir build

build/Grammar: Grammar/
	ghc Grammar/*.hs -odir build -hidir build

Grammar/: Grammar.cf
	bnfc -d Grammar.cf
	rm Grammar/Doc.txt
	mv Grammar/Doc.tex ../doc/Grammar.tex
	alex -g Grammar/Lex.x
	happy -gca Grammar/Par.y
	rm -f Grammar/*.bak

.PHONY: clean distclean test
clean:
	-rm -rf build
	-rm -f ../jlc
	-rm -f *.out
	-rm -rf tmp
	-rm -f Grammar/Test
distclean: clean
	
