-module(ktn_dodger_api_SUITE).

%% A pre_fixer may use this exception to report a parser diagnostic.
-elvis([{elvis_style, no_throw, disable}]).

-export([
    all/0,
    quick_macros/1,
    quick_files/1,
    directives/1,
    compact_strings/1,
    fixer_callbacks/1,
    diagnostics/1,
    encodings/1,
    token_roundtrip/1,
    parsing_wrappers/1,
    escript_header/1
]).

-behaviour(ct_suite).

-type config() :: [{atom(), term()}].
-export_type([config/0]).

-spec all() -> [atom()].
all() ->
    [
        quick_macros,
        quick_files,
        directives,
        compact_strings,
        fixer_callbacks,
        diagnostics,
        encodings,
        token_roundtrip,
        parsing_wrappers,
        escript_header
    ].

-spec quick_macros(config()) -> ok.
quick_macros(_Config) ->
    Bodies = [
        "f() -> ?VALUE.",
        "f() -> ?VALUE().",
        "f() -> \"prefix\" ?VALUE.",
        "f() -> ?VALUE \"suffix\".",
        "f() -> mod:?NAME(a).",
        "f() -> try ok catch error:?PATTERN(X) -> X end.",
        "f() -> try ok catch error:?PATTERN(X) when is_atom(X) -> X end.",
        "f() -> try ok catch error:?PATTERN(X):S -> {X,S} end.",
        "?NAME(X) -> X."
    ],
    lists:foreach(
        fun(Source) ->
            {ok, [Function]} = with_device(Source, fun ktn_dodger:quick_parse/1),
            function = erl_syntax:type(Function)
        end,
        Bodies
    ),
    {ok, [Function]} = with_device("f() -> ?VALUE.", fun ktn_dodger:quick_parse/1),
    [Clause] = erl_syntax:function_clauses(Function),
    [Value] = erl_syntax:clause_body(Clause),
    '?VALUE' = erl_syntax:atom_value(Value),
    ok.

-spec quick_files(config()) -> ok.
quick_files(Config) ->
    Source =
        "-module(x).\n-define(X, ok).\n-undef(X).\n-include(\"x.hrl\").\n"
        "-include_lib(\"x.hrl\").\n-ifdef(X).\n-ifndef(Y).\n-if(true).\n"
        "-elif(false).\n-else.\n-endif.\n-feature(maybe_expr, enable).\nf() -> ok.\n",
    with_file(Config, Source, fun(Path) ->
        {ok, Forms} = ktn_dodger:quick_parse_file(Path),
        {ok, Forms} = ktn_dodger:quick_parse_file(Path, []),
        [attribute, function] = [erl_syntax:type(F) || F <- Forms]
    end),
    Missing = filename:join(proplists:get_value(priv_dir, Config), "missing.erl"),
    {error, {0, file, enoent}} = ktn_dodger:parse_file(Missing),
    {error, {0, file, enoent}} = ktn_dodger:quick_parse_file(Missing),
    ok.

-spec directives(config()) -> ok.
directives(_Config) ->
    {ok, [Include, IncludeLib]} = parse("-include(\"a.hrl\").\n-include_lib(\"b.hrl\").", []),
    include = erl_syntax:atom_value(erl_syntax:attribute_name(Include)),
    include_lib = erl_syntax:atom_value(erl_syntax:attribute_name(IncludeLib)),
    {ok, [Error, Warning]} = parse("-error(\"bad\").\n-warning(\"careful\").", []),
    {_, ktn_dodger, {error, ErrorText}} = erl_syntax:error_marker_info(Error),
    {_, ktn_dodger, {warning, WarningText}} = erl_syntax:error_marker_info(Warning),
    true = lists:prefix("-error", lists:flatten(ErrorText)),
    true = lists:prefix("-warning", lists:flatten(WarningText)),
    {ok, [Attribute]} = parse("-?NAME(value).", []),
    attribute = erl_syntax:type(Attribute),
    {ok, [QuickAttribute]} = with_device("-?NAME(value).", fun ktn_dodger:quick_parse/1),
    attribute = erl_syntax:type(QuickAttribute),
    ok.

-spec compact_strings(config()) -> ok.
compact_strings(_Config) ->
    lists:foreach(
        fun(Options) ->
            {ok, [Function]} = parse("f() -> \"hello\" \"world\".", Options),
            [Clause] = erl_syntax:function_clauses(Function),
            [Text] = erl_syntax:clause_body(Clause),
            "helloworld" = erl_syntax:string_value(Text),
            {ok, [Definition]} = parse("-define(X, \"hello\"\n\"world\").", Options),
            attribute = erl_syntax:type(Definition)
        end,
        [
            [parse_macro_definitions],
            [compact_strings],
            [compact_strings, {scan_opts, [text]}],
            [compact_strings, parse_macro_definitions],
            [compact_strings, parse_macro_definitions, {scan_opts, [text]}]
        ]
    ),
    {ok, [Function]} = parse("f() -> \"prefix\" ?MID \"suffix\".", [clever]),
    function = erl_syntax:type(Function),
    ok.

-spec fixer_callbacks(config()) -> ok.
fixer_callbacks(_Config) ->
    Prefix = fun(Tokens) ->
        case Tokens of
            [{atom, _, replace_me} | Rest] -> {retry, Rest};
            _ -> no_fix
        end
    end,
    {ok, [Function]} = parse("replace_me f() -> ok.", [{pre_fixer, Prefix}]),
    f = erl_syntax:atom_value(erl_syntax:function_name(Function)),
    Postfix = fun({function, Pos, Name, Arity, Clauses}) ->
        {form, {function, Pos, list_to_atom("new_" ++ atom_to_list(Name)), Arity, Clauses}}
    end,
    {ok, [Renamed]} = parse("f() -> ok.", [{post_fixer, Postfix}]),
    new_f = erl_syntax:atom_value(erl_syntax:function_name(Renamed)),
    {ok, [Definition]} = parse("-define(NAME, ok).", [
        {pre_fixer, fun(Tokens) -> {retry, Tokens} end}
    ]),
    define = erl_syntax:atom_value(erl_syntax:attribute_name(Definition)),
    {ok, [Error]} = parse("f() -> ok.", [{pre_fixer, fun(_) -> throw({error, custom}) end}]),
    {_, ktn_dodger, custom} = erl_syntax:error_marker_info(Error),
    {ok, [Unexpected]} = parse("f() -> ok.", [{pre_fixer, fun(_) -> error(custom) end}]),
    {_, ktn_dodger, {unknown, custom}} = erl_syntax:error_marker_info(Unexpected),
    ok.

-spec diagnostics(config()) -> ok.
diagnostics(_Config) ->
    {ok, [Error]} = parse("f() -> ?MACRO(.", []),
    {_, ktn_dodger, macro_args} = erl_syntax:error_marker_info(Error),
    Text = lists:flatten(ktn_dodger:format_error(macro_args)),
    true = string:find(Text, "missing end parenthesis") =/= nomatch,
    "error text" = ktn_dodger:format_error({error, "error text"}),
    "warning text" = ktn_dodger:format_error({warning, "warning text"}),
    [_ | _] = lists:flatten(ktn_dodger:format_error({unknown, custom})),
    {ok, [SyntaxError]} = parse("f() -> ->.", []),
    error_marker = erl_syntax:type(SyntaxError),
    {ok, [Recovered]} = parse("f() -> ->.", [no_fail]),
    text = erl_syntax:type(Recovered),
    ok.

-spec encodings(config()) -> ok.
encodings(Config) ->
    with_file(Config, <<"f() -> \"", 255, "\".">>, fun(Path) ->
        {ok, [Function]} = ktn_dodger:parse_file(Path),
        function = erl_syntax:type(Function)
    end),
    with_file(Config, <<"%% coding: utf-8\nf() -> \"", 255, "\".">>, fun(Path) ->
        {ok, [Error | _]} = ktn_dodger:parse_file(Path),
        error_marker = erl_syntax:type(Error)
    end),
    ok.

-spec token_roundtrip(config()) -> ok.
token_roundtrip(_Config) ->
    Source = "{atom, 'quoted-name', \"string\", $a, 1.25, 42, Variable, #rec{key = value}}.",
    {ok, Tokens, _} = erl_scan:string(Source),
    {ok, Restored, _} = erl_scan:string(lists:flatten(ktn_dodger:tokens_to_string(Tokens))),
    Symbols = [erl_scan:symbol(T) || T <- Tokens],
    Symbols = [erl_scan:symbol(T) || T <- Restored],
    ok.

-spec parsing_wrappers(config()) -> ok.
parsing_wrappers(_Config) ->
    lists:foreach(
        fun(Parser) ->
            {ok, [Form]} = with_device("f() -> ok.", Parser),
            function = erl_syntax:type(Form)
        end,
        [
            fun ktn_dodger:parse/1,
            fun(D) -> ktn_dodger:parse(D, 1) end,
            fun(D) -> ktn_dodger:quick_parse(D, 1) end,
            fun(D) -> ktn_dodger:quick_parse(D, 1, []) end
        ]
    ),
    {ok, _, _} = with_device("f() -> ok.", fun(D) -> ktn_dodger:parse_form(D, 1) end),
    {ok, _, _} = with_device("f() -> ok.", fun(D) -> ktn_dodger:quick_parse_form(D, 1) end),
    ok.

-spec escript_header(config()) -> ok.
escript_header(_Config) ->
    {ok, [Forms]} = parse("#!/usr/bin/env escript\nmain(_) -> ok.\n", []),
    form_list = erl_syntax:type(Forms),
    [Header, Function] = erl_syntax:form_list_elements(Forms),
    text = erl_syntax:type(Header),
    function = erl_syntax:type(Function),
    {ok, [Error]} = parse("#!/usr/bin/env escript\nmain() -> ?UNFINISHED(.\n", []),
    {_, ktn_dodger, macro_args} = erl_syntax:error_marker_info(Error),
    ok.

parse(Source, Options) ->
    with_device(Source, fun(Device) -> ktn_dodger:parse(Device, 1, Options) end).

with_device(Source, Check) ->
    Device = ktn_io_string:new(Source),
    try
        Check(Device)
    after
        ok = file:close(Device)
    end.

with_file(Config, Source, Check) ->
    Path = filename:join(proplists:get_value(priv_dir, Config), "dodger_api.erl"),
    ok = file:write_file(Path, Source),
    try
        Check(Path)
    after
        ok = file:delete(Path)
    end.
