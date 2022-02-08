-module(test4).
-export([my_api/2, handle_call/3]).

handle_call({my_api_server, _Arg}, _From, State) ->
  {reply, 2, State}.

my_api(Pid, Arg) ->
  gen_server:call(Pid, {my_api_server, Arg}).
