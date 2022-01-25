setup:
	mkdir -p ./ebin && mkdir -p ./plt

compile: setup
	erl -make

dialyzer: compile
	dialyzer --plt ./plt/default.plt -r ./ebin --dump_callgraph ./callgraph.dot

build-plt: setup
	dialyzer --build_plt --apps erts kernel stdlib mnesia crypto compiler --output_plt ./plt/default.plt
