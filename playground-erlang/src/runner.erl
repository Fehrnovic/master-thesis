-module(runner).
-export([runner/0]).
-import(dialyzer, [run/1]).

runner() ->
  try run_dialyzer() of
    [] -> io:fwrite("No discrepancies found");
    DiscrepancyMsg -> io:fwrite(dialyzer:format_warning(DiscrepancyMsg))
  catch
    ErrorMessage -> ErrorMessage
  end.

run_dialyzer() ->
  observer:start(),
  Output = dialyzer:run([
    {plts, [
      "../plt/default.plt"
    ]},
    {files_rec, [
      "../ebin/examples/test4.beam"
    ]}
  ]),
  observer:stop(),
  Output.


