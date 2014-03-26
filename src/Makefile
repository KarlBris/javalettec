SOURCES = $(wildcard *.hs) Grammar/

jlc: $(SOURCES)
	ghc --make Compiler.hs -o jlc -odir build -hidir build

Grammar/: Grammar.cf
	bnfc -d Grammar.cf
	mv Grammar/*.tex Grammar/*.txt ../doc/
	alex -g Grammar/Lex.x
	happy -gca Grammar/Par.y

.PHONY: clean distclean package
clean:
	-rm -f **/*.hi **/*.o
	-rm -f *.hi *.o
	-rm -rf build
distclean: clean
	-rm -f javalettec
	-rm -f jlc
package: distclean
	(cd ..; tar -cvz --exclude-vcs --exclude *.tar.gz -f javalettec.tar.gz ../javalettec)

