clean:
	rm -rf ./ebin

setup:
	mkdir -p ./ebin/discrepancies
	mkdir -p ./plt

compile:
	erlc +debug_info -o ./ebin/discrepancies ./src/discrepancies/*.erl

compile-runner:
	erlc +debug_info -o ./ebin ./src/runner.erl

dialyze:
	dialyzer --plt ./plt/default.plt -r ./ebin/discrepancies

build-plt:
	(dialyzer --check_plt --plt ./plt/default.plt) || (dialyzer --build_plt --apps erts kernel stdlib mnesia crypto compiler --output_plt ./plt/default.plt)

symlink-dialyzer-src:
	rm -rf ./src/dialyzer
	ln -s "../../../otp/lib/dialyzer/src" ./src/dialyzer

compile-dialyzer:
	cd ../../otp && $(MAKE) install.dirs && $(MAKE) install.libs ONLY_DIALYZER=true  && $(MAKE) install.Install && $(MAKE) install.bin

# Might not be needed?
run-erlang-port-mapper-deamon:
	erl -sname foo -s init stop -noshell

recompile: clean setup compile-dialyzer compile compile-runner

initial-configure: clean setup build-plt symlink-dialyzer-src compile-dialyzer compile compile-runner
