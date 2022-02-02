-module(runner).
-export([runner/0]).
-import(dialyzer, [run/1]).

runner() ->
  try run_dialyzer() of
    [] -> io:fwrite("No discrepancies found");
    DiscrepancyMsg -> io:fwrite(DiscrepancyMsg)
  catch
    ErrorMessage -> io:fwrite(ErrorMessage)
  end.

run_dialyzer() ->
  dialyzer:run([
    {plts, ["./plt/default.plt"]},
    {files_rec, ["./ebin/discrepancies/discrepancy1.beam"]}
  ]).


