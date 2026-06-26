-module(otp29).

-if(?OTP_RELEASE >= 29).

-export([valid/0]).

%% native record declaration (no parentheses, hash before name)
-record #point{x :: integer(), y :: integer()}.

%% nominal type
-nominal meters() :: number().

%% erlfmt:ignore-begin

valid() ->
    %% comprehension assignment (compr_assign experimental feature)
    Pairs = [{1, a}, {2, b}, {3, c}],
    [N || {N, _} = _Pair <- Pairs],

    %% native record construction
    _P = #point{x = 1, y = 2},
    ok.

%% erlfmt:ignore-end

-endif.
