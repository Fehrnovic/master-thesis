-module(discrepancy2).
-export([my_api/2, handle_call/3]).

% client APIs
-spec my_api(pid(), integer()) -> unicode:chardata().
my_api(Pid, Arg) ->
    gen_server:call(Pid, {my_api_server, Arg}).

% server implementation
-spec handle_call({my_api_server, integer()}, _From, my_state) -> {reply, integer(), my_state}.
handle_call({my_api_server, Arg}, _From, State) ->
    Res = Arg + 1,
    {reply, Res, State}.