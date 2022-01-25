clean:
	rm -rf ./ebin

setup:
	mkdir -p ./ebin && mkdir -p ./plt

compile:
	$(MAKE) clean
	$(MAKE) setup
	erl -make

dialyzer:
	dialyzer --plt ./plt/default.plt -r ./ebin --dump_callgraph ./callgraph.dot

build-plt:
	$(MAKE) setup
	(dialyzer --check_plt --plt ./plt/default.plt) || (dialyzer --build_plt --apps erts kernel stdlib mnesia crypto compiler --output_plt ./plt/default.plt)

symlink-dialyzer-src:
	$(MAKE) setup
	rm -rf ./src/dialyzer && ln -s "../../../otp/lib/dialyzer/src" ./src/dialyzer

compile-dialyzer:
	cd ../../otp && $(MAKE) install.dirs && $(MAKE) install.libs ONLY_DIALYZER=true  && $(MAKE) install.Install && $(MAKE) install.bin

recompile: compile compile-dialyzer

initial-configure: build-plt symlink-dialyzer-src recompile

