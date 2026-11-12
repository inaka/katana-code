-module(ktn_macro_clause_SUITE).

-export([all/0, dodger_macro_clause/1, code_macro_clause/1]).

-behaviour(ct_suite).

-type config() :: [{atom(), term()}].

-export_type([config/0]).

-spec all() -> [atom()].
all() ->
    [dodger_macro_clause, code_macro_clause].

-spec dodger_macro_clause(config()) -> ok.
dodger_macro_clause(Config) ->
    Path = filename:join(proplists:get_value(priv_dir, Config), "macros.erl"),
    ok = file:write_file(Path, macro_source()),
    try
        lists:foreach(
            fun(Options) ->
                {ok, Device} = file:open(Path, [read]),
                try
                    {ok, Module, Next} = ktn_dodger:parse_form(Device, 1, Options),
                    attribute = erl_syntax:type(Module),
                    {ok, Define, Start} = ktn_dodger:parse_form(Device, Next, Options),
                    define = erl_syntax:atom_value(erl_syntax:attribute_name(Define)),
                    {ok, Function, End} = ktn_dodger:parse_form(Device, Start, Options),
                    function = erl_syntax:type(Function),
                    is_adult = erl_syntax:atom_value(erl_syntax:function_name(Function)),
                    1 = erl_syntax:function_arity(Function),
                    [Clause] = erl_syntax:function_clauses(Function),
                    [Macro] = erl_syntax:clause_patterns(Clause),
                    macro = erl_syntax:type(Macro),
                    'MATCH_NAME' = erl_syntax:variable_name(erl_syntax:macro_name(Macro)),
                    [] = erl_syntax:macro_arguments(Macro),
                    [Body] = erl_syntax:clause_body(Clause),
                    false = erl_syntax:atom_value(Body),
                    {eof, _} = ktn_dodger:parse_form(Device, End, Options)
                after
                    ok = file:close(Device)
                end
            end,
            [[{scan_opts, [text]}], [{scan_opts, [text]}, no_fail], []]
        )
    after
        ok = file:delete(Path)
    end,
    ok.

-spec code_macro_clause(config()) -> ok.
code_macro_clause(_Config) ->
    Source = macro_source(),
    lists:foreach(
        fun(Input) ->
            #{type := root, content := [Module, Define, Function]} = ktn_code:parse_tree(Input),
            module = ktn_code:type(Module),
            define = ktn_code:type(Define),
            #{
                type := function,
                attrs := #{name := is_adult, arity := 1},
                content := [
                    #{
                        type := clause,
                        node_attrs := #{
                            pattern := [
                                #{
                                    type := macro, attrs := #{name := "MATCH_NAME"}, content := []
                                }
                            ]
                        },
                        content := [#{type := atom, attrs := #{value := false}}]
                    }
                ]
            } = Function
        end,
        [Source, list_to_binary(Source)]
    ),
    ok.

macro_source() ->
    "-module(macros).\n"
    "-define(MATCH_NAME(), #{name := \"Per\"}).\n"
    "is_adult(?MATCH_NAME()) ->\n"
    "    false.\n".
