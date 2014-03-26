SOURCES = $(wildcard *.hs) Grammar/

jlc: $(SOURCES)
	ghc --make Compiler.hs -o jlc -odir build -hidir build

Grammar/: Grammar.cf
	bnfc -d Grammar.cf
	rm Grammar/Doc.txt
	mv Grammar/Doc.tex ../doc/Grammar.tex
	alex -g Grammar/Lex.x
	happy -gca Grammar/Par.y
	rm Grammar/*.bak

.PHONY: clean distclean package
clean:
	-rm -f **/*.hi **/*.o
	-rm -f *.hi *.o
distclean: clean
	-rm -rf build
	-rm -f jlc
