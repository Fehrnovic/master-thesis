-module(runner).
-export([runner/0]).
-import(dialyzer, [run/1]).

runner() ->
  try run_dialyzer() of
    [] -> io:fwrite("No discrepancies found");
    DiscrepancyMessages -> lists:foreach(
      fun(DiscrepancyMessage) ->
        io:fwrite(dialyzer:format_warning(DiscrepancyMessage))
      end,
      DiscrepancyMessages
    )
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
      %"../ebin/examples/aaaa.beam"
      "../ebin/discrepancies/discrepancy1.beam",
      "../ebin/discrepancies/discrepancy2.beam"
    ]}
  ]),
  observer:stop(),
  Output.


