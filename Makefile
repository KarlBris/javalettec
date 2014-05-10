jlc: src/*
	cd src; make

.PHONY: clean test grade

clean: 
	cd src; make clean
	-rm -rf graderTestSuite
	-rm -f jlc

grade: jlc
	cd tests; make && ./Grade -b LLVM . ..

# Usage: make test FILE=tests/testsuite/good/core021.jl
test: jlc
	@rm -rf tmp/
	@mkdir tmp
	@echo "######### ${FILE}\n"
	@cat ${FILE} && \
	./jlc ${FILE} > tmp/test.llvm && \
	echo "\n\n######### tmp/test.llvm\n" && cat tmp/test.llvm && \
	opt -std-compile-opts -mem2reg tmp/test.llvm > tmp/test.bc && \
  llvm-dis tmp/test.bc && echo "\n\n######### tmp/test.ll\n" && cat tmp/test.ll

package:
	cd src && make jlc distclean
	tar -cvz --exclude-vcs --exclude .gitkeep --exclude *.tar.gz -f partB-1.tar.gz doc lib src