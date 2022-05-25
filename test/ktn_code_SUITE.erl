-module(ktn_code_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([consult/1, beam_to_string/1, parse_tree/1, parse_tree_otp/1, latin1_parse_tree/1,
         to_string/1]).

-if(?OTP_RELEASE >= 25).

-export([parse_maybe/1]).

-endif.

-define(EXCLUDED_FUNS, [module_info, all, test, init_per_suite, end_per_suite]).

-type config() :: [{atom(), term()}].

-if(?OTP_RELEASE >= 23).

-behaviour(ct_suite).

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec consult(config()) -> ok.
consult(_Config) ->
    [{a}, {b}] = ktn_code:consult("{a}. {b}."),
    [] = ktn_code:consult(""),
    [{a}, {b}, {c, d, e}] = ktn_code:consult("{a}. {b}. {c, d, e}."),
    [{a}, {b}, {c, d, e}] = ktn_code:consult("{a}.\r\n{b}.\r\n{c, d, e}."),

    [{'.'}] = ktn_code:consult("{'.'}.\n"),
    [{<<"ble.bla">>}, {"github.com"}] =
        ktn_code:consult("{<<\"ble.bla\">>}.\n{\"github.com\"}.\r\n"),
    ok.

-spec beam_to_string(config()) -> ok.
beam_to_string(_Config) ->
    {error, beam_lib, _} = ktn_code:beam_to_string(<<>>),
    BaseDir = code:lib_dir(katana_code),
    BeamDir = filename:join(BaseDir, "ebin/ktn_code.beam"),
    {ok, _} = ktn_code:beam_to_string(BeamDir),
    ok.

-spec parse_tree(config()) -> ok.
parse_tree(_Config) ->
    ModuleNode =
        #{type => module,
          attrs =>
              #{location => {1, 2},
                text => "module",
                value => x}},

    #{type := root, content := _} = ktn_code:parse_tree("-module(x)."),

    #{type := root, content := [ModuleNode]} = ktn_code:parse_tree("-module(x)."),

    ok.

%% @doc Parse a 100 random OTP modules
-spec parse_tree_otp(config()) -> ok.
parse_tree_otp(_Config) ->
    OTP = code:root_dir(),
    Paths = filelib:wildcard(OTP ++ "/**/*.erl"),
    ShuffledPaths = shuffle(Paths),

    _ = [begin
             {ok, Source} = file:read_file(Path),
             ktn_code:parse_tree(Source)
         end
         || Path <- lists:sublist(ShuffledPaths, 1, 100)],

    ok.

-spec latin1_parse_tree(config()) -> ok.
latin1_parse_tree(_Config) ->
    error =
        try
            ktn_code:parse_tree(<<"%% �\n-module(x).">>)
        catch
            error:_ ->
                error
        end,
    #{type := root, content := _} =
        ktn_code:parse_tree(<<"%% -*- coding: latin-1 -*-\n"
                              "%% �"
                              "-module(x).">>),

    ok.

-spec to_string(config()) -> ok.
to_string(_Config) ->
    "1" = ktn_code:to_str(1),
    "hello" = ktn_code:to_str(<<"hello">>),
    "atom" = ktn_code:to_str(atom),

    ok.

-if(?OTP_RELEASE >= 25).

-spec parse_maybe(config()) -> ok.
parse_maybe(_Config) ->
    %% Note that to pass this test case, the 'maybe_expr' feature must be enabled.
    #{type := root,
      content :=
          [#{type := function, content := [#{type := clause, content := [#{type := maybe}]}]}]} =
        ktn_code:parse_tree(<<"foo() -> maybe ok ?= ok else _ -> ng end.">>),

    ok.

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec shuffle(any()) -> [any()].
shuffle(List) ->
    Items = [{rand:uniform(), X} || X <- List],
    [X || {_, X} <- lists:sort(Items)].
