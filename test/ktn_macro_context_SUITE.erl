-module(ktn_macro_context_SUITE).

-export([all/0, nested_arguments/1, remote_calls/1, legacy_macro/1, scanner_tokens/1]).

-behaviour(ct_suite).

-type config() :: [{atom(), term()}].
-export_type([config/0]).

-spec all() -> [atom()].
all() ->
    [nested_arguments, remote_calls, legacy_macro, scanner_tokens].

-spec nested_arguments(config()) -> ok.
nested_arguments(_Config) ->
    {ok, [Function]} = parse(
        "f() -> ?MACRO([1,2], <<1>>, begin ok end, if true -> ok end, "
        "case x of x -> ok end, receive X -> X after 0 -> ok end, "
        "try ok after ok end).",
        []
    ),
    [Clause] = erl_syntax:function_clauses(Function),
    [Macro] = erl_syntax:clause_body(Clause),
    macro = erl_syntax:type(Macro),
    7 = length(erl_syntax:macro_arguments(Macro)),
    ok.

-spec remote_calls(config()) -> ok.
remote_calls(_Config) ->
    Sources = [
        "f() -> mod:?NAME(a).",
        "f() -> try ok catch error:?PATTERN(X) -> X end.",
        "f() -> try ok catch error:?PATTERN(X) when is_atom(X) -> X end.",
        "f() -> try ok catch error:?PATTERN(X):Stack -> {X,Stack} end.",
        "?NAME(X) -> X.",
        "f() -> \"prefix\" ?MID \"suffix\"."
    ],
    lists:foreach(
        fun(Source) ->
            {ok, [Function]} = parse(Source, [clever]),
            function = erl_syntax:type(Function)
        end,
        Sources
    ),
    {ok, [Attribute]} = parse("-elif(false).", []),
    elif = erl_syntax:atom_value(erl_syntax:attribute_name(Attribute)),
    {ok, [Undef]} = parse("-undef(NAME).", []),
    undef = erl_syntax:atom_value(erl_syntax:attribute_name(Undef)),
    {ok, Conditional} = parse("-ifdef(NAME).\n-ifndef(OTHER).\n-endif.\n-endif.", []),
    [ifdef, ifndef, endif, endif] = [
        erl_syntax:atom_value(erl_syntax:attribute_name(Form))
     || Form <- Conditional
    ],
    {ok, [Text]} = parse("f() -> \"prefix\" ?SUFFIX().", [no_fail]),
    text = erl_syntax:type(Text),
    ok.

-spec legacy_macro(config()) -> ok.
legacy_macro(_Config) ->
    {ok, [Function]} = parse("f() -> '? <macro> ('('NAME').", []),
    [Clause] = erl_syntax:function_clauses(Function),
    [Macro] = erl_syntax:clause_body(Clause),
    macro = erl_syntax:type(Macro),
    'NAME' = erl_syntax:atom_value(erl_syntax:macro_name(Macro)),
    ok.

-spec scanner_tokens(config()) -> ok.
scanner_tokens(_Config) ->
    %% Exercise reconstruction even when the runtime scanner emits no sigil tokens.
    lists:foreach(
        fun(Token) -> "" = ktn_dodger:tokens_to_string([Token]) end,
        [{sigil_prefix, 1, b}, {sigil_suffix, 1, []}]
    ),
    "\"joined\"" = lists:flatten(ktn_dodger:tokens_to_string([{string_concat, 1, "joined"}])),
    "fn(" = lists:flatten(ktn_dodger:tokens_to_string([{var, 1, fn}, {'(', 1}])),
    ".\nnext" = ktn_dodger:tokens_to_string([{dot, 1}, {atom, 2, next}]),
    ok.

parse(Source, Options) ->
    Device = ktn_io_string:new(Source),
    try
        ktn_dodger:parse(Device, 1, Options)
    after
        ok = file:close(Device)
    end.
