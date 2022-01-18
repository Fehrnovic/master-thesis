-module(discrepancy2).
-behaviour(gen_server).
-export([start_link/0, my_api/1, init/1, handle_call/3]).

start_link() ->
    gen_server:start_link({local, discrepancy2erlang}, discrepancy2erlang, [], []).

-spec my_api(integer()) -> string().
my_api(Arg) ->
    gen_server:call(discrepancy2erlang, {my_api, Arg}).

init(_Args) ->
    {ok, my_state}.

-spec handle_call({my_api, integer()}, _From, my_state) -> {reply, integer(), my_state}.
handle_call({my_api, Arg}, _From, State) ->
    Res = Arg + 1,
    {reply, Res, State}.
