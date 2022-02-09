-module(test4).
-export([my_api/2, handle_call/3, beer_func/2]).

beer_func(_Pid, {my_api_server, _Arg}) ->
  2.

-spec my_api(pid(),any()) -> boolean().
my_api(Pid, Arg) ->
  beer_func(Pid, {my_api_server, Arg}),
  gen_server:call(Pid, {my_api_server, Arg}).

handle_call({my_api_server, _Arg}, _From, State) ->
  {reply, 2, State}.
