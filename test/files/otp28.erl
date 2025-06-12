-module(otp28).

-if(?OTP_RELEASE >= 28).

-moduledoc """
Here we go
""".

-export([valid/0]).

%% erlfmt:ignore-begin

-doc """
There and back again.
""".
valid() ->
    % list strict generator
    [Integer || {Integer, _} <:- [{1, 2}, {3, 4}]],

    % binary strict generator
    [Word || <<Word:16>> <:= <<16#1234:16, 16#ABCD:16>>],

    % map strict generator
    #{K => V + 1 || K := V <:- #{a => 1, b => 2}},

    % zip generators
    [A + B || A <- [1, 2, 3] && B <- [4, 5, 6]],

    % binaries
    ~"This is a UTF-8 binary",

    % map comprehensions
    #{ K => V || K := V <- #{john => wick}}.

%% erlfmt:ignore-end

-endif.
