-module(otp27).

-include_lib("stdlib/include/assert.hrl").

-export([break/0]).

break() ->
    _ = scan:string(
        ~"""
      This is valid code.
    """
    ),

    Fun = fun() -> ok end,
    ?assertMatch(
        {ok, _} when
            is_function(Fun, 0),
        {ok, 'no'}
    ).
