-module(otp28).

-if(?OTP_RELEASE >= 28).

-export([valid/0]).

%% erlfmt:ignore-begin
valid() ->
    % zip generators
    [A + B || A <- [1, 2, 3] && B <- [4, 5, 6]].
%% erlfmt:ignore-end

-endif.
