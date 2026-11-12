-module(ktn_abstract_SUITE).

-export([all/0, annotations/1, legacy_forms/1, diagnostics/1, invalid_forms/1]).

-behaviour(ct_suite).

-type config() :: [{atom(), term()}].
-export_type([config/0]).

-spec all() -> [atom()].
all() ->
    [annotations, legacy_forms, diagnostics, invalid_forms].

-spec annotations(config()) -> ok.
annotations(_Config) ->
    Inputs = [
        {1, {1, 1}, undefined},
        {{2, 3}, {2, 3}, ""},
        {[{line, 4}, {column, 5}, {text, "ok"}], {4, 5}, "ok"},
        {[{location, {6, 7}}], {6, 7}, ""},
        {[], {-1, -1}, ""},
        {unknown, {-1, -1}, ""}
    ],
    lists:foreach(
        fun({Annotation, Location, Text}) ->
            #{type := atom, attrs := #{value := ok, location := Location, text := Text}} =
                ktn_code:to_map({atom, Annotation, ok})
        end,
        Inputs
    ),
    ok.

-spec legacy_forms(config()) -> ok.
legacy_forms(_Config) ->
    #{type := function, attrs := #{name := f, arity := 2}} =
        ktn_code:to_map({function, f, 2}),
    #{type := function, attrs := #{module := m, name := f, arity := 2}} =
        ktn_code:to_map({function, m, f, 2}),
    #{type := query, content := [#{type := atom}]} =
        ktn_code:to_map({query, 1, [{atom, 1, value}]}),
    #{type := native_record, attrs := #{name := point}, content := []} =
        ktn_code:to_map({attribute, 1, native_record, {point, []}}),
    lists:foreach(
        fun(Anno) ->
            #{type := type_map_field, node_attrs := #{key := #{type := atom}}} =
                ktn_code:to_map(
                    {type, Anno, map_field_assoc, {atom, 1, key}, {type, 1, integer, []}}
                )
        end,
        [1, [{location, {1, 1}}]]
    ),
    #{type := nominal, attrs := #{name := id}} =
        ktn_code:to_map({attribute, 1, nominal, {id, {type, 1, integer, []}, []}}),
    #{type := type, attrs := #{name := '...'}} = ktn_code:to_map({type, 1, any}),
    #{type := record_field, node_attrs := #{type := #{type := atom}}} =
        ktn_code:to_map({paren_type, 1, [{atom, 1, wrapped}]}),
    VariableAttribute = erl_syntax:attribute(erl_syntax:variable('NAME'), []),
    #{} = ktn_code:to_map(VariableAttribute),
    UnknownType = erl_syntax:variable('OPAQUE_ENCODING'),
    UnknownType = ktn_code:decode_type_term(UnknownType),
    #{type := atom, attrs := #{value := non_reversible_form}} =
        ktn_code:to_map(erl_syntax:text("unparsed source")),
    #{type := macro, attrs := #{name := "NAME"}, content := []} =
        ktn_code:to_map({macro, 1, erl_syntax:variable('NAME'), none}),
    ok.

-spec diagnostics(config()) -> ok.
diagnostics(_Config) ->
    #{type := error, attrs := #{value := bad}} = ktn_code:to_map({error, bad}),
    #{type := warning, attrs := #{value := warning}} = ktn_code:to_map({warning, warning}),
    #{type := eof, attrs := #{location := {9, 1}}} = ktn_code:to_map({eof, 9}),
    ok.

-spec invalid_forms(config()) -> ok.
invalid_forms(_Config) ->
    lists:foreach(
        fun({Input, Expected}) ->
            try ktn_code:to_map(Input) of
                Value -> ct:fail({unexpected_success, Value})
            catch
                throw:Expected -> ok
            end
        end,
        [
            {{unsupported, 1}, {unhandled_abstract_form, {unsupported, 1}}},
            {unsupported, {unexpected_abstract_form, unsupported}}
        ]
    ),
    ok.
