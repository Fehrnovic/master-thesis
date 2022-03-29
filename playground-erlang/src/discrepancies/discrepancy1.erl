-module(discrepancy1).
-export([handle_stuff/2, handle_stuff2/1, handle_call/3]).

handle_stuff(Pid, Command) ->
  my_api(Pid, Command).

handle_stuff2(Pid) ->
  my_api(Pid, {my_api_server, 1, 3}).

-spec my_api(pid(), any()) -> 6.
my_api(Pid, Command) ->
  gen_server:call(Pid, Command).

handle_call({my_api_server, _Arg}, _From, _State) ->
  {reply, 6, _State};
handle_call({my_api_server, _Arg, _Arg2}, _From, _State) ->
  _Result = _Arg + _Arg2,
  {reply, bob, _State}.