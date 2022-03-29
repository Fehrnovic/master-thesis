-module('discrepancy1-test').
-export([test/1]).

test(Pid) ->
  discrepancy1:handle_stuff(Pid, {my_api_server, 1, 2}).