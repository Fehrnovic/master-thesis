-module(handle_call_union).
-export([my_api/1, handle_call/3]).

% client APIs
-spec my_api(pid()) -> a.
my_api(Pid) ->
  gen_server:call(Pid, {my_api_server2}).

% server implementation
handle_call({my_api_server}, _From, State) ->
  {reply, a, State};
handle_call({my_api_server2}, _From, State) ->
  {reply, b, State}.
