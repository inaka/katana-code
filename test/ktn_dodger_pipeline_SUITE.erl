-module(ktn_dodger_pipeline_SUITE).

-export([all/0, form_repair/1, term_repair/1, scanner_boundaries/1, stringify_macros/1]).

-behaviour(ct_suite).

-type config() :: [{atom(), term()}].
-export_type([config/0]).

-spec all() -> [atom()].
all() ->
    [form_repair, term_repair, scanner_boundaries, stringify_macros].

-spec form_repair(config()) -> ok.
form_repair(_Config) ->
    NoFix = fun(_) -> no_fix end,
    {ok, Broken, _} = erl_scan:string("f() -> ->."),
    Replacement = {attribute, 1, module, repaired},
    Replacement = ktn_dodger:parse_tokens(
        Broken,
        NoFix,
        fun(_) -> {form, Replacement} end,
        NoFix
    ),
    {ok, Valid, _} = erl_scan:string("f() -> ok."),
    {function, _, f, 0, _} = ktn_dodger:parse_tokens(
        Broken,
        NoFix,
        fun(_) -> {retry, Valid, NoFix} end,
        NoFix
    ),
    no_fix = ktn_dodger:fix_form([]),
    no_fix = ktn_dodger:fix_form([
        {atom, 1, '?preprocessor declaration?'},
        {'(', 1},
        {')', 1},
        {'->', 1},
        {atom, 1, define},
        {'(', 1}
    ]),
    {retry, _, _} = ktn_dodger:fix_form([
        {atom, 1, '?preprocessor declaration?'},
        {'(', 1},
        {')', 1},
        {'->', 1},
        {atom, 1, define},
        {'(', 1},
        {var, 1, 'NAME'},
        {dot, 1}
    ]),
    ok.

-spec term_repair(config()) -> ok.
term_repair(_Config) ->
    NoFix = fun(_) -> no_fix end,
    {ok, Tokens, _} = erl_scan:string("#{key => value}."),
    Replacement = erl_syntax:atom(repaired),
    Replacement = ktn_dodger:parse_tokens_as_terms(
        Tokens,
        fun(_) -> {form, Replacement} end,
        NoFix
    ),
    BadTokens = [{'->', 1}, {dot, 1}],
    Prefix = fun
        ([{'->', _} | _]) -> {retry, Tokens};
        (_) -> no_fix
    end,
    Repaired = ktn_dodger:parse_tokens_as_terms(BadTokens, Prefix, NoFix),
    form_list = erl_syntax:type(Repaired),
    Replacement = ktn_dodger:parse_tokens_as_terms(
        BadTokens,
        NoFix,
        fun(_) -> {form, Replacement} end
    ),
    Retried = ktn_dodger:parse_tokens_as_terms(
        BadTokens,
        NoFix,
        fun(_) -> {retry, Tokens, NoFix} end
    ),
    form_list = erl_syntax:type(Retried),
    ok.

-spec scanner_boundaries(config()) -> ok.
scanner_boundaries(_Config) ->
    "source" = ktn_dodger:consumed_chars("source", eof),
    "head" = ktn_dodger:consumed_chars("headrest", "rest"),
    [] = ktn_dodger:consumed_chars(eof, eof),
    {9, 4} = ktn_dodger:start_pos([], {9, 4}),
    true = ktn_dodger:reserved_word('else'),
    true = ktn_dodger:reserved_word('maybe'),
    false = ktn_dodger:reserved_word(ordinary_atom),
    [{atom, _, '?preprocessor declaration?'} | _] =
        ktn_dodger:quickscan_form([{'-', 1}, {atom, 1, 'else'}, {dot, 1}]),
    Device = ktn_io_string:new(""),
    ok = file:close(Device),
    {eof, 1} = ktn_dodger:parse_form(Device, 1, [no_fail]),
    ok.

-spec stringify_macros(config()) -> ok.
stringify_macros(_Config) ->
    %% Stringification may encounter both internally rewritten variable names
    %% and ordinary atom tokens; it must preserve their distinct meanings.
    {retry, [{atom, 1, '?,?NAME'}], _} =
        ktn_dodger:fix_stringyfied_macros([{'?', 1}, {atom, 1, '?,NAME'}]),
    OrdinaryTokens = [{'?', 1}, {atom, 1, name}],
    {retry, OrdinaryTokens, _} = ktn_dodger:fix_stringyfied_macros(OrdinaryTokens),
    ok.
