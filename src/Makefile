BNFC_GEN = Abs.hs ErrM.hs Lex.x Par.y Print.hs Skel.hs Test.hs
BNFC_FILES = $(patsubst %,Javalette/%,$(BNFC_GEN))

all: $(BNFC_FILES) Compiler.hs
	ghc --make Compiler.hs -o javalettec

$(BNFC_FILES): Javalette.cf
	bnfc -d Javalette.cf
	mv Javalette/*.tex Javalette/*.txt ../doc/
Javalette/Par.hs: Javalette/Par.y
	happy -gca Javalette/Par.y
Javalette/Lex.hs: Javalette/Lex.x
	alex -g Javalette/Lex.x

clean:
	-rm -f Javalette/*.hi Javalette/*.o
	-rm -f *.hi *.o
distclean: clean
	-rm -f Javalette/Doc.* Javalette/Lex.* Javalette/Par.* Javalette/Layout.* Javalette/Skel.* Javalette/Print.* Javalette/Test.* Javalette/Abs.* Javalette/Test Javalette/ErrM.* Javalette/SharedString.* Javalette/Javalette.dtd Javalette/XML.* Makefile*
	-rmdir -p Javalette/
