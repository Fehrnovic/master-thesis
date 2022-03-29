-module(aaaa1).
-export([my_api/2, handle_call/3]).

% client APIs

-spec my_api(pid(), integer()) -> integer().
my_api(Pid, Arg) ->
    gen_server:call(Pid, {my_api_server, Arg}).

% server implementation

handle_call({my_api_server, _Arg}, _From, State) ->
    {reply, true, State};
handle_call({my_api_server, _Argh}, _From, State) ->
    {reply, false, State};
handle_call(list, _From, State) ->
    {reply, abebobvonfrank, State};
handle_call(-1, _From, State) ->
    {reply, abebobvonfrank, State};
handle_call({my_api_server, _Arg}, _From, State) ->
    {reply, true, State};
handle_call({my_api_server, _Argh}, _From, State) ->
    {reply, false, State};
handle_call(list, _From, State) ->
    {reply, abebobvonfrank, State};
handle_call(-1, _From, State) ->
    {reply, abebobvonfrank, State};
handle_call({my_api_server, _Arg}, _From, State) ->
    {reply, true, State};
handle_call({my_api_server, _Argh}, _From, State) ->
    {reply, false, State};
handle_call(list, _From, State) ->
    {reply, abebobvonfrank, State};
handle_call(-1, _From, State) ->
    {reply, abebobvonfrank, State}.