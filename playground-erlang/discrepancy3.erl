-module(discrepancy3).
-export([safe_div/2]).

-spec safe_div(integer(), integer()) -> {ok, integer()} | {error, string()}.
safe_div(_, 0) ->
    {error, 0};
safe_div(X, Y) ->
    {ok, X div Y}.
