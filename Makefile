all: test4

analysis.plugin: analysis.ml
	bapbuild -pkg ocamlgraph analysis.plugin


test4: out.test4.32.x
	 (diff out.test4.32.x gold.test4.32.x > diff.test4.32.x && \
	  echo "test4 PASSED" && rm out.test4.32.x diff.test4.32.x)\
       || echo "test4 FAILED"


out.test4.32.x: analysis.plugin test4.32.x
	bap-objdump test4.32.x -lanalysis --use-ida > out.test4.32.x


clean:
	rm -rf analysis.plugin _build
