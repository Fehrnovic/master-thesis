-module(discrepancy1).
-export([handle_stuff/2, handle_call/3]).

-record(state, {num :: atom(), maskine :: number()}).
-record(batman, {num :: atom()}).

handle_stuff(Pid, #state.num = _Var) when is_atom(_Var) ->
  my_api(Pid, {my_api_server, abe, abe11});
handle_stuff(Pid, #batman.num = _Var) ->
  my_api(Pid, {my_api_server, abe, abe}).

my_api(Pid, Command) ->
  gen_server:call(Pid, Command).

handle_call({my_api_server, _Arg, #state.num}, _From, _State) ->
  {reply, atomman, _State};
handle_call({my_api_server, _Arg, #batman.num}, _From, _State) ->
  {reply, bob, _State}.
