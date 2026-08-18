-module(ktn_code_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    guards/1,
    consult/1,
    beam_to_string/1,
    parse_tree/1,
    parse_tree_otp/1,
    latin1_parse_tree/1,
    parse_macro_in_type/1,
    to_string/1
]).

-export([parse_maybe/1, parse_maybe_else/1]).

-export([otp27_features/1]).

-if(?OTP_RELEASE >= 28).

-export([otp28_features/1, parse_macro_in_nominal/1]).

-if(?OTP_RELEASE >= 29).

-export([otp29_features/1]).

-endif.
-endif.

-define(EXCLUDED_FUNS, [module_info, all, test, init_per_suite, end_per_suite]).

-type config() :: [{atom(), term()}].

-export_type([config/0]).

-behaviour(ct_suite).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    %% Do not test maybe expressions if the feature is not enabled in the emulator
    DisabledMaybeTests =
        case lists:member(maybe_expr, enabled_features()) of
            true ->
                [];
            false ->
                [parse_maybe, parse_maybe_else]
        end,
    Excluded = ?EXCLUDED_FUNS ++ DisabledMaybeTests,
    [F || {F, 1} <- Exports, not lists:member(F, Excluded)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    Config.

enabled_features() ->
    try
        erl_features:enabled()
    catch
        error:undef ->
            []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec guards(config()) -> ok.
guards(_Config) ->
    %% One guard
    [[#{type := atom, attrs := #{value := single_guard}}]] =
        extract_guards(<<"f() -> case x of x when single_guard -> x end.">>),
    %% Join with , - and - andalso
    [
        [
            #{type := atom, attrs := #{value := first_guard}},
            #{type := atom, attrs := #{value := second_guard}}
        ]
    ] = extract_guards(<<"f() -> case x of x when first_guard, second_guard -> x end.">>),
    [
        [
            #{
                type := op,
                content := [
                    #{type := atom, attrs := #{value := first_guard}},
                    #{type := atom, attrs := #{value := second_guard}}
                ],
                attrs := #{operation := 'andalso'}
            }
        ]
    ] =
        extract_guards(<<"f() -> case x of x when first_guard andalso second_guard -> x end.">>),
    [
        [
            #{
                type := op,
                content := [
                    #{type := atom, attrs := #{value := first_guard}},
                    #{type := atom, attrs := #{value := second_guard}}
                ],
                attrs := #{operation := 'and'}
            }
        ]
    ] = extract_guards(<<"f() -> case x of x when first_guard and second_guard -> x end.">>),
    %% Join with ; - or - orelse
    [
        [#{type := atom, attrs := #{value := first_guard}}],
        [#{type := atom, attrs := #{value := second_guard}}]
    ] = extract_guards(<<"f() -> case x of x when first_guard; second_guard -> x end.">>),
    [
        [
            #{
                type := op,
                content := [
                    #{type := atom, attrs := #{value := first_guard}},
                    #{type := atom, attrs := #{value := second_guard}}
                ],
                attrs := #{operation := 'orelse'}
            }
        ]
    ] =
        extract_guards(<<"f() -> case x of x when first_guard orelse second_guard -> x end.">>),
    [
        [
            #{
                type := op,
                content := [
                    #{type := atom, attrs := #{value := first_guard}},
                    #{type := atom, attrs := #{value := second_guard}}
                ],
                attrs := #{operation := 'or'}
            }
        ]
    ] = extract_guards(<<"f() -> case x of x when first_guard or second_guard -> x end.">>),
    %% Combine ...
    [
        [#{type := atom, attrs := #{value := first_guard}}],
        [
            #{type := atom, attrs := #{value := second_guard}},
            #{type := atom, attrs := #{value := third_guard}}
        ]
    ] = extract_guards(
        <<"f() -> case x of x when first_guard; second_guard, third_guard -> x end.">>
    ),
    [
        [
            #{
                type := op,
                content := [
                    #{type := atom, attrs := #{value := first_guard}},
                    #{
                        type := op,
                        content := [
                            #{type := atom, attrs := #{value := second_guard}},
                            #{type := atom, attrs := #{value := third_guard}}
                        ],
                        attrs := #{operation := 'andalso'}
                    }
                ],
                attrs := #{operation := 'orelse'}
            }
        ]
    ] =
        extract_guards(
            <<
                "f() ->\n"
                "case x of x when first_guard orelse second_guard andalso third_guard -> x end.\n"
                ""
            >>
        ),
    [
        [
            #{
                type := op,
                content := [
                    #{type := atom, attrs := #{value := first_guard}},
                    #{
                        type := op,
                        content := [
                            #{type := atom, attrs := #{value := second_guard}},
                            #{type := atom, attrs := #{value := third_guard}}
                        ],
                        attrs := #{operation := 'and'}
                    }
                ],
                attrs := #{operation := 'or'}
            }
        ]
    ] =
        extract_guards(
            <<"f() -> case x of x when first_guard or second_guard and third_guard -> x end.">>
        ),
    ok.

-spec consult(config()) -> ok.
consult(_Config) ->
    [{a}, {b}] = ktn_code:consult("{a}. {b}."),
    [] = ktn_code:consult(""),
    Expected = [{a}, {b}, {c, d, e}],
    Expected = ktn_code:consult("{a}. {b}. {c, d, e}."),
    Expected = ktn_code:consult("{a}.\r\n{b}.\r\n{c, d, e}."),

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
        #{
            type => module,
            attrs =>
                #{
                    location => {1, 2},
                    text => "module",
                    value => x
                }
        },

    #{type := root, content := _} = ktn_code:parse_tree("-module(x)."),

    #{type := root, content := [ModuleNode]} = ktn_code:parse_tree("-module(x)."),

    ok.

%% @doc Parse a 100 random OTP modules
-spec parse_tree_otp(config()) -> ok.
parse_tree_otp(_Config) ->
    OTP = code:root_dir(),
    Paths = filelib:wildcard(OTP ++ "/**/*.erl"),
    ShuffledPaths = shuffle(Paths),

    _ = [parse_tree_from(Path) || Path <- lists:sublist(ShuffledPaths, 1, 100)],

    ok.

parse_tree_from(Path) ->
    {ok, Source} = file:read_file(Path),
    ktn_code:parse_tree(Source).

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
        ktn_code:parse_tree(<<
            "%% -*- coding: latin-1 -*-\n"
            "%% �"
            "-module(x)."
        >>),

    ok.

-spec to_string(config()) -> ok.
to_string(_Config) ->
    "1" = ktn_code:to_str(1),
    "hello" = ktn_code:to_str(<<"hello">>),
    "atom" = ktn_code:to_str(atom),

    ok.

-spec parse_maybe(config()) -> ok.
parse_maybe(_Config) ->
    %% Note that to pass this test case, the 'maybe_expr' feature must be enabled.
    #{
        type := root,
        content :=
            [#{type := function, content := [#{type := clause, content := [#{type := 'maybe'}]}]}]
    } =
        ktn_code:parse_tree(<<"foo() -> maybe ok ?= ok end.">>),

    ok.

-spec parse_maybe_else(config()) -> ok.
parse_maybe_else(_Config) ->
    %% Note that to pass this test case, the 'maybe_expr' feature must be enabled.
    #{
        type := root,
        content :=
            [#{type := function, content := [#{type := clause, content := [#{type := 'maybe'}]}]}]
    } =
        ktn_code:parse_tree(<<"foo() -> maybe ok ?= ok else _ -> ng end.">>),

    ok.

otp27_features(_Config) ->
    #{} = ktn_code:parse_tree(~|two_sigils() -> [~B'a sigil', ~"another sigil"].|),
    {ok, Bin} = file:read_file("../../lib/katana_code/test/files/otp27.erl"),
    #{} = ktn_code:parse_tree(Bin),
    {ok, _} =
        ktn_dodger:parse_file(
            "../../lib/katana_code/test/files/otp27.erl",
            [no_fail, parse_macro_definitions]
        ).

-if(?OTP_RELEASE >= 28).

otp28_features(_Config) ->
    {ok, Bin} = file:read_file("../../lib/katana_code/test/files/otp28.erl"),
    ct:log("OTP28:\n~p", [ktn_code:parse_tree(Bin)]),
    #{} = ktn_code:parse_tree(Bin),
    {ok, _} =
        ktn_dodger:parse_file(
            "../../lib/katana_code/test/files/otp28.erl",
            [no_fail]
        ).

-spec parse_macro_in_nominal(config()) -> ok.
parse_macro_in_nominal(_Config) ->
    %% A macro in a `-nominal' body must still classify as `nominal' and expose
    %% the `{Name, Type, Params}' value rather than a raw token list, including
    %% with multiple, deeply nested macros.
    #{type := nominal, attrs := #{value := {my_nominal, _, []}}} =
        type_def_node(<<"-nominal my_nominal() :: {?MODULE, list()}.">>),
    #{type := nominal, attrs := #{value := {nested, _, []}}} =
        type_def_node(<<"-nominal nested() :: {?A, [?B], #{?C => ?D}}.">>),
    ok.

-if(?OTP_RELEASE >= 29).

otp29_features(_Config) ->
    %% Native record declaration maps to type `native_record' with a `{Name, Fields}' value.
    #{type := native_record, attrs := #{value := {point, _}}} =
        native_record_node(<<"-record #point{x :: integer(), y :: integer()}.">>),

    %% The full fixture file (native records + compr_assign comprehension) must parse cleanly.
    {ok, Bin} = file:read_file("../../lib/katana_code/test/files/otp29.erl"),
    ct:log("OTP29:\n~p", [ktn_code:parse_tree(Bin)]),
    #{} = ktn_code:parse_tree(Bin),
    {ok, _} =
        ktn_dodger:parse_file(
            "../../lib/katana_code/test/files/otp29.erl",
            [no_fail, parse_macro_definitions]
        ).

-endif.
-endif.

-spec parse_macro_in_type(config()) -> ok.
parse_macro_in_type(_Config) ->
    %% A macro in a type body must still classify the attribute correctly (so
    %% `-type' stays `type_attr', not the raw `type') and expose a stable
    %% `{Name, Type, Params}' value instead of a raw token list.
    #{type := type_attr, attrs := #{name := my_type}} =
        type_def_node(<<"-type my_type() :: {?MODULE, list()}.">>),
    #{type := opaque, attrs := #{value := {my_opaque, _, []}}} =
        type_def_node(<<"-opaque my_opaque() :: {?MODULE, list()}.">>),
    %% The normalisation is driven by the shape erl_syntax produces for any
    %% macro-in-body type, so it holds no matter how many macros appear, how
    %% deeply they nest, or whether they sit in tuples, lists, maps (incl. as
    %% keys), remote types, or macro calls.
    Bodies = [
        <<"{?MODULE, [?MODULE], #{?MODULE => ?MODULE}}">>,
        <<"#{?KEY := value, key := ?VALUE}">>,
        <<"?MOD:remote()">>,
        <<"?WRAP({?MODULE, list()})">>,
        <<"[?A | ?B]">>
    ],
    lists:foreach(
        fun(Body) ->
            Source = <<"-opaque o() :: ", Body/binary, $.>>,
            #{type := opaque, attrs := #{value := {o, _, []}}} = type_def_node(Source)
        end,
        Bodies
    ),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type_def_node(Source) ->
    #{content := Content} = ktn_code:parse_tree(Source),
    [Node] =
        [N || N <- Content, lists:member(ktn_code:type(N), [type_attr, type, opaque, nominal])],
    Node.

-if(?OTP_RELEASE >= 29).

native_record_node(Source) ->
    #{content := Content} = ktn_code:parse_tree(Source),
    [Node] = [N || N <- Content, ktn_code:type(N) =:= native_record],
    Node.

-endif.

-spec shuffle([string()]) -> [[any()]].
shuffle(List) ->
    Items = [{rand:uniform(), X} || X <- List],
    [X || {_, X} <- lists:sort(Items)].

extract_guards(Source) ->
    extract_guards([ktn_code:parse_tree(Source)], []).

extract_guards([], Guards) ->
    Guards;
extract_guards([Node | Nodes], Guards) ->
    extract_guards(
        ktn_code:content(Node) ++
            lists:flatten([maps:values(maps:get(node_attrs, Node, #{}))]) ++
            Nodes,
        case ktn_code:node_attr(guards, Node) of
            undefined -> Guards;
            Gs -> Guards ++ Gs
        end
    ).
