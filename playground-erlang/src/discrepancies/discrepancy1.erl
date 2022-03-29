-module(discrepancy1).
-export([handle_stuff/2, handle_call/3, my_api/2]).

-record(state, {num :: atom(), maskine :: number()}).
-record(batman, {num :: atom()}).

handle_stuff(Pid, _Arg) when is_atom(_Arg) ->
  my_api(Pid, {my_api_server, abe, abe11});
handle_stuff(Pid, _Arg2) ->
  my_api(Pid, {my_api_server, abe}).

my_api(Pid, Command) ->
  gen_server:call(Pid, Command).

handle_call({my_api_server, _Arg}, _From, _State) when is_atom(_Arg) ->
  {reply, atomman, _State};
handle_call({my_api_server, _Arg}, _From, _State) ->
  {reply, bob, _State}.
