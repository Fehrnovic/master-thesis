-module(handle_call_catch_all).
-export([my_api/2, handle_call/3]).

% client APIs
-spec my_api(pid(), atom()) -> a.
my_api(_Pid, Arg) ->
  gen_server:call(_Pid, {my_api_server, Arg}).

% server implementation
handle_call({my_api_server, a}, _From, State) ->
  {reply, a, State};
handle_call({my_api_server, _Atom}, _From, State) ->
  {reply, b, State}.
