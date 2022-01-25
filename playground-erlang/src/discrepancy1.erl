-module(discrepancy1).
-behaviour(gen_server).
-export([start_link/0, my_api/2, init/1, handle_call/3, handle_cast/2]).

% client APIs

-spec start_link() -> pid().
start_link() ->
    {ok, Pid} = gen_server:start_link({local, discrepancy1}, discrepancy1, [ok], []),
    Pid.

-spec my_api(pid(), integer()) -> string().
my_api(Pid, Arg) ->
    gen_server:call(Pid, {my_api, Arg}).

% server implementation

init(ok) ->
    {ok, my_state}.

handle_call({my_api, Arg}, _From, State) ->
    Res = uppercase(Arg),
    {reply, Res, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec uppercase(string()) -> string().
uppercase(Arg) ->
    string:uppercase(Arg).
