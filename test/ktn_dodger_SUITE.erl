-module(ktn_dodger_SUITE).

-export([
    all/0,
    unterminated_string_no_fail/1,
    unterminated_string_strict/1,
    unterminated_string_stream/1,
    no_fail_valid_source/1,
    no_fail_other_scan_error/1
]).

-behaviour(ct_suite).

-type config() :: [{atom(), term()}].

-export_type([config/0]).

-spec all() -> [atom()].
all() ->
    [
        unterminated_string_no_fail,
        unterminated_string_strict,
        unterminated_string_stream,
        no_fail_valid_source,
        no_fail_other_scan_error
    ].

-spec unterminated_string_no_fail(config()) -> ok.
unterminated_string_no_fail(Config) ->
    Bodies = [
        "x() ->\n"
        "    \"This is an unterminated string,\n"
        "    some:more(stuff).\n",
        "x() -> \"unterminated at end of file",
        "x() -> \"escaped \\\" quote is not a terminator\n"
        "    some:more(stuff).\n"
    ],
    lists:foreach(
        fun(Body) ->
            with_unterminated_string_file(Config, Body, fun(Path) ->
                lists:foreach(
                    fun(Options) ->
                        {ok, [Module, Export, Text]} = ktn_dodger:parse_file(Path, Options),
                        assert_recipe_attributes(Module, Export),
                        text = erl_syntax:type(Text),
                        %% Keep the whole form, not merely the scanner's error preview.
                        Body = string:trim(erl_syntax:text_string(Text), leading, "\r\n"),
                        4 = erl_anno:line(erl_syntax:get_pos(Text))
                    end,
                    [[no_fail], [{no_fail, true}], [no_fail, {scan_opts, [text]}]]
                )
            end)
        end,
        Bodies
    ),
    ok.

-spec unterminated_string_strict(config()) -> ok.
unterminated_string_strict(Config) ->
    Body = "x() ->\n    \"This is an unterminated string,\n    some:more(stuff).\n",
    with_unterminated_string_file(Config, Body, fun(Path) ->
        {ok, Default} = ktn_dodger:parse_file(Path),
        {ok, Default} = ktn_dodger:parse_file(Path, [{no_fail, false}]),
        [Module, Export, Error] = Default,
        assert_recipe_attributes(Module, Export),
        error_marker = erl_syntax:type(Error),
        {_, erl_scan, Reason} = erl_syntax:error_marker_info(Error),
        true =
            case Reason of
                {unterminated, string, _} -> true;
                {string, $", _} -> true
            end
    end),
    ok.

-spec unterminated_string_stream(config()) -> ok.
unterminated_string_stream(Config) ->
    %% Cross multiple file IO chunks and keep Unicode and CRLF characters.
    Body = "x() ->\r\n    \"" ++ lists:duplicate(20000, $a) ++ [16#3BB] ++ "\r\nstill open",
    with_unterminated_string_file(Config, unicode:characters_to_binary(Body), fun(Path) ->
        {ok, [_, _, Text]} = ktn_dodger:parse_file(Path, [no_fail]),
        Body = string:trim(erl_syntax:text_string(Text), leading, "\r\n")
    end),
    Device = ktn_io_string:new(Body),
    try
        {ok, StreamText, {6, _}} = ktn_dodger:parse_form(Device, {4, 1}, [no_fail]),
        Body = erl_syntax:text_string(StreamText),
        {4, 1} = erl_syntax:get_pos(StreamText),
        {eof, _} = ktn_dodger:parse_form(Device, {6, 1}, [no_fail])
    after
        ok = file:close(Device)
    end,
    ok.

-spec no_fail_valid_source(config()) -> ok.
no_fail_valid_source(Config) ->
    with_unterminated_string_file(Config, "x() -> \"valid\".\ny() -> ok.\n", fun(Path) ->
        {ok, Expected} = ktn_dodger:parse_file(Path),
        {ok, Expected} = ktn_dodger:parse_file(Path, [no_fail]),
        [attribute, attribute, function, function] =
            [erl_syntax:type(Form) || Form <- Expected]
    end),
    ok.

-spec no_fail_other_scan_error(config()) -> ok.
no_fail_other_scan_error(Config) ->
    with_unterminated_string_file(Config, "x() -> 2#2.\ny() -> ok.\n", fun(Path) ->
        {ok, Expected} = ktn_dodger:parse_file(Path),
        {ok, Expected} = ktn_dodger:parse_file(Path, [no_fail]),
        [_, _, Error | Remaining] = Expected,
        error_marker = erl_syntax:type(Error),
        function = erl_syntax:type(lists:last(Remaining))
    end),
    ok.

with_unterminated_string_file(Config, Body, Check) ->
    Path = filename:join(proplists:get_value(priv_dir, Config), "unterminated_string.erl"),
    Source = ["-module(recipe_15).\n-export([x/0]).\n\n", Body],
    ok = file:write_file(Path, Source),
    try
        Check(Path)
    after
        ok = file:delete(Path)
    end.

assert_recipe_attributes(Module, Export) ->
    attribute = erl_syntax:type(Module),
    attribute = erl_syntax:type(Export),
    module = erl_syntax:atom_value(erl_syntax:attribute_name(Module)),
    [ModuleName] = erl_syntax:attribute_arguments(Module),
    recipe_15 = erl_syntax:atom_value(ModuleName),
    export = erl_syntax:atom_value(erl_syntax:attribute_name(Export)),
    [Exports] = erl_syntax:attribute_arguments(Export),
    [{Name, Arity}] = erl_syntax:concrete(Exports),
    x = erl_syntax:atom_value(Name),
    0 = erl_syntax:integer_value(Arity),
    ok.
