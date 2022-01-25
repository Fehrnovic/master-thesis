clean:
	rm -r ./ebin

setup:
	mkdir -p ./ebin && mkdir -p ./plt

compile: clean setup
	erl -make

dialyzer: compile
	dialyzer --plt ./plt/default.plt -r ./ebin --dump_callgraph ./callgraph.dot

build-plt: setup
	dialyzer --build_plt --apps erts kernel stdlib mnesia crypto compiler --output_plt ./plt/default.plt

symlink-dialyzer-src: setup
	rm ./src/dialyzer && ln -s "../../../otp/lib/dialyzer/src" ./src/dialyzer

compile-dialyzer:
	cd ../../otp && $(MAKE) install

recompile: compile compile-dialyzer

