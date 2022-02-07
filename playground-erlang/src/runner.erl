-module(runner).
-export([runner/0]).
-import(dialyzer, [run/1]).

runner() ->
  observer:start(),
  try run_dialyzer() of
    [] -> io:fwrite("No discrepancies found");
    DiscrepancyMsg -> io:fwrite(DiscrepancyMsg)
  catch
    ErrorMessage -> io:fwrite(ErrorMessage)
  end.

run_dialyzer() ->
  dialyzer:run([
    {plts, ["./plt/default.plt"]},
    {files_rec, ["./ebin/examples/test.beam"]}
  ]).


