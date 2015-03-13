all: test4

kstrings.plugin: kstrings.ml
	bapbuild -pkg oUnit -pkg ocamlgraph kstrings.plugin

test4: out.test4.32.x
	 @(diff out.test4.32.x gold.test4.32.x > diff.test4.32.x && \
	  echo "test4 PASSED" && rm out.test4.32.x diff.test4.32.x)\
       || echo "test4 FAILED"


out.test4.32.x: kstrings.plugin test4.32.x
	bap-objdump test4.32.x -lkstrings --use-ida > out.test4.32.x


clean:
	rm -rf kstrings.plugin _build
