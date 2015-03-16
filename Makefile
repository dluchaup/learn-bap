.PHONY: build clean utest.native


test: build test4 #utest 

build:
	bapbuild -pkg oUnit -pkg ocamlgraph kstrings.plugin
	#bapbuild -pkg oUnit -pkg ocamlgraph utest.native


kstrings.plugin:
	bapbuild -pkg oUnit -pkg ocamlgraph kstrings.plugin

utest.native:
	bapbuild -pkg oUnit -pkg ocamlgraph utest.native

utest: out.utest.native
	 @(diff out.utest.native gold.utest.native > diff.utest.native && \
	  echo "utest PASSED" && rm out.utest.native diff.utest.native)\
       || echo "utest FAILED"

out.utest.native: utest.native
	./utest.native > out.utest.native


test4: out.test4.32.x
	 @(diff out.test4.32.x gold.test4.32.x > diff.test4.32.x && \
	  echo "test4 PASSED" && rm out.test4.32.x diff.test4.32.x)\
       || echo "test4 FAILED"



out.test4.32.x: kstrings.plugin test4.32.x
	bap-objdump test4.32.x -lkstrings --use-ida > out.test4.32.x


clean:
	rm -rf kstrings.plugin utest.native _build
