-module(handle_call_same_atom_arity_with_discrep).
-export([my_api/2, handle_call/3]).

% client APIs
-spec my_api(pid(), atom()) -> a | b | c.
my_api(Pid, Arg) ->
  case gen_server:call(Pid, {my_api_server, Arg}) of
    a -> a;
    b -> b;
    c -> c
  end.

% server implementation
handle_call({my_api_server, a}, _From, State) ->
  {reply, a, State};
handle_call({my_api_server, b}, _From, State) ->
  {reply, b, State}.
