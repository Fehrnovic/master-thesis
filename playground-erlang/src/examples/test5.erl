-module(test5).
-export([my_api1/2, my_api2/2, handle_call/3]).

my_api1(Pid, Arg) ->
  gen_server:call(Pid, {my_api_server1, Arg}).
my_api2(Pid, Arg) ->
  gen_server:call(Pid, {my_api_server2, Arg}).

handle_call({my_api_server1, _Arg}, _From, State) ->
  {reply, 2, State};
handle_call({my_api_server2, _Arg}, _From, State) ->
  {reply, true, State}.