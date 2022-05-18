-module(handle_cast_input_mismatch).
-export([my_api/2, handle_cast/2]).

% client APIs
-spec my_api(pid(), atom()) -> ok.
my_api(Pid, Arg) ->
    gen_server:cast(Pid, {my_api_server, Arg}).

% server implementation
-spec handle_cast({my_api_server, integer()}, my_state) -> {noreply, my_state}.
handle_cast({my_api_server, Arg}, State) ->
    _Res = Arg + 1,
    {noreply, State}.