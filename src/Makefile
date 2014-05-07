SOURCES = $(wildcard *.hs) Grammar/

jlc: $(SOURCES)
	ghc -Wall --make Compiler.hs -o jlc -odir build -hidir build

Grammar/: Grammar.cf
	bnfc -d Grammar.cf
	rm Grammar/Doc.txt
	mv Grammar/Doc.tex ../doc/Grammar.tex
	alex -g Grammar/Lex.x
	happy -gca Grammar/Par.y
	rm Grammar/*.bak

# Usage: make test FILE=../tests/testsuite/good/core021.jl
test: jlc
	@rm -rf tmp/
	@mkdir tmp
	@echo "######### ${FILE}\n"
	@cat ${FILE} && \
	./jlc ${FILE} > tmp/test.llvm && \
	echo "\n\n######### tmp/test.llvm\n" && cat tmp/test.llvm && \
	opt -std-compile-opts -mem2reg tmp/test.llvm > tmp/test.bc && \
  llvm-dis tmp/test.bc && echo "\n\n######### tmp/test.ll\n" && cat tmp/test.ll

.PHONY: clean distclean test
clean:
	-rm -rf build
	-rm -f jlc
	-rm -rf tmp
distclean: clean
	
