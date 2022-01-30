clean:
	rm -rf ./ebin

setup:
	mkdir -p ./ebin && mkdir -p ./plt

compile:
	$(MAKE) clean
	$(MAKE) setup
	erlc +debug_info -o ./ebin ./src/*.erl

dialyzer:
	dialyzer --plt ./plt/default.plt -r ./ebin

build-plt:
	$(MAKE) setup
	(dialyzer --check_plt --plt ./plt/default.plt) || (dialyzer --build_plt --apps erts kernel stdlib mnesia crypto compiler --output_plt ./plt/default.plt)

symlink-dialyzer-src:
	$(MAKE) setup
	rm -rf ./src/dialyzer && ln -s "../../../otp/lib/dialyzer/src" ./src/dialyzer

compile-dialyzer:
	cd ../../otp && $(MAKE) install.dirs && $(MAKE) install.libs ONLY_DIALYZER=true  && $(MAKE) install.Install && $(MAKE) install.bin

# Might not be needed?
run-erlang-port-mapper-deamon:
	erl -sname foo -s init stop -noshell

recompile: compile-dialyzer compile

initial-configure: build-plt symlink-dialyzer-src recompile
