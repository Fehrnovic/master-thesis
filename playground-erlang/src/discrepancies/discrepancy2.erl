-module(discrepancy2).
-behaviour(gen_server).
-export([start_link/0, my_api/2, init/1, handle_call/3, handle_cast/2]).

% client APIs

-spec start_link() -> pid().
start_link() ->
    {ok, Pid} = gen_server:start_link({local, discrepancy2}, discrepancy2, [ok], []),
    Pid.

-spec my_api(pid(), integer()) -> unicode:chardata().
my_api(Pid, Arg) ->
    handle_call({my_api_server, Arg}, nil, my_state),
    gen_server:call(Pid, {my_api_server, Arg}).

% server implementation

init(_Args) ->
    {ok, my_state}.

-spec handle_call({my_api_server, integer()}, _From, my_state) -> {reply, integer(), my_state}.
handle_call({my_api_server, Arg}, _From, State) ->
    Res = Arg + 1,
    {reply, Res, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
