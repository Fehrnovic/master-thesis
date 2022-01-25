-module(runner).
-export([runner/0]).
-import(dialyzer, [run/1]).

runner() ->
  Test = dialyzer:run([
    {plts, ["./plt/default.plt"]},
    {files_rec, ["./ebin"]}
  ]),
  Test.
