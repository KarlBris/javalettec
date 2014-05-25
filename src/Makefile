SOURCES = $(wildcard *.hs) build/Grammar

all: ../jlc ../lib/runtime.bc

../jlc: $(SOURCES)
	ghc -Wall --make Compiler.hs -o ../jlc -odir build -hidir build

build/Grammar: Grammar/
	ghc Grammar/*.hs -odir build -hidir build

../lib/runtime.bc: ../lib/runtime.ll
	cd ../lib; llvm-link runtime.ll > runtime.bc

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
	-rm -rf *.o *.hi
	-rm -rf **/*.o **/*.hi
	-rm -f ../jlc
	-rm -f *.out
	-rm -rf tmp
	-rm -f Grammar/Test
	-rm -f ../lib/runtime.bc
distclean: clean
