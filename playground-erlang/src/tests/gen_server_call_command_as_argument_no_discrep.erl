-module(gen_server_call_command_as_argument_no_discrep).
-export([my_api/2, handle_call/3]).

% client APIs
-spec my_api(pid(), any()) -> integer().
my_api(Pid, Command) ->
    gen_server:call(Pid, Command).

% server implementation
handle_call({my_api_server, _Arg}, _From, State) ->
    Res = "Arg + 1",
    {reply, Res, State};
handle_call({my_api_server1, Arg}, _From, State) ->
    Res = Arg + 1,
    {reply, Res, State}.