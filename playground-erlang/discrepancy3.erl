-module(discrepancy3).
-export([safe_div/2, cheese/0]).

%-spec safe_div(integer(), integer()) -> {ok, integer()} | {error, string()}.
safe_div(1.0, 0) ->
    {error, 0};
safe_div(X, Y) ->
    {ok, X div Y}.

cheese() ->
    safe_div("5", 3).
