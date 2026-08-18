-module(otp29).

-if(?OTP_RELEASE >= 29).

-export([valid/0]).

-import_record(otp29, [point]).

%% erlfmt:ignore-begin

%% native record declaration (no parentheses, hash before name)
-record #point{x :: integer(), y :: integer()}.

valid() ->
    %% comprehension assignment (compr_assign experimental feature)
    Pairs = [{1, a}, {2, b}, {3, c}],
    _L = [N, tuple_size(Pair) || Pair <- Pairs, N = element(1, Pair), N > 0],

    %% native record construction
    _P = #point{x = 1, y = 2},
    Q = #opt29:point{x = 3, y = 4},
    ok.

%% erlfmt:ignore-end

-endif.
