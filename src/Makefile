BNFC_GEN = Abs.hs ErrM.hs Lex.x Par.y Print.hs Skel.hs Test.hs
BNFC_FILES = $(patsubst %,Grammar/%,$(BNFC_GEN))
GENERATED_DEPS = Grammar/Lex.hs Grammar/Par.hs

all: $(BNFC_FILES) $(GENERATED_DEPS) Compiler.hs
	ghc --make Compiler.hs -o javalettec -odir build -hidir build

$(BNFC_FILES): Grammar.cf
	bnfc -d Grammar.cf
	mv Grammar/*.tex Grammar/*.txt ../doc/
Grammar/Par.hs: Grammar/Par.y
	happy -gca Grammar/Par.y
Grammar/Lex.hs: Grammar/Lex.x
	alex -g Grammar/Lex.x

clean:
	-rm -f Grammar/*.hi Grammar/*.o
	-rm -f *.hi *.o
	-rm -rf build/
distclean: clean
	-rm -f Grammar/Doc.* Grammar/Lex.* Grammar/Par.* Grammar/Layout.* Grammar/Skel.* Grammar/Print.* Grammar/Test.* Grammar/Abs.* Grammar/Test Grammar/ErrM.* Grammar/SharedString.* Grammar/Grammar.dtd Grammar/XML.* Makefile*
	-rmdir -p Grammar/
