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
	-rm -f Grammar/*.hi Grammar/*.o
	-rm -f *.hi *.o
	-rm -rf build/
distclean: clean
	-rm -f Grammar/Doc.* Grammar/Lex.* Grammar/Par.* Grammar/Layout.* Grammar/Skel.* Grammar/Print.* Grammar/Test.* Grammar/Abs.* Grammar/Test Grammar/ErrM.* Grammar/SharedString.* Grammar/Grammar.dtd Grammar/XML.*
	-rmdir -p Grammar/
	-rm -f javalettec
	-rm -f jlc
package: distclean
	(cd ..; tar -cvz --exclude-vcs --exclude *.tar.gz -f javalettec.tar.gz ../javalettec)

