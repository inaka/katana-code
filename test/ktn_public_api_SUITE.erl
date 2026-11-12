-module(ktn_public_api_SUITE).

-export([all/0, evaluation/1, getters/1, beam_files/1, source_encodings/1]).

-behaviour(ct_suite).

-type config() :: [{atom(), term()}].
-export_type([config/0]).

-spec all() -> [atom()].
all() ->
    [evaluation, getters, beam_files, source_encodings].

-spec evaluation(config()) -> ok.
evaluation(_Config) ->
    42 = ktn_code:eval("6 * 7."),
    [a, b] = ktn_code:eval(<<"[a, b].">>),
    [#{key := value}] = ktn_code:consult("#{key => value}."),
    ok.

-spec getters(config()) -> ok.
getters(_Config) ->
    undefined = ktn_code:type(undefined),
    undefined = ktn_code:attr(missing, #{attrs => #{}}),
    undefined = ktn_code:attr(missing, #{}),
    undefined = ktn_code:attr(missing, undefined),
    undefined = ktn_code:node_attr(missing, #{node_attrs => #{}}),
    undefined = ktn_code:node_attr(missing, #{}),
    undefined = ktn_code:node_attr(missing, undefined),
    [] = ktn_code:content(#{type => atom}),
    ok.

-spec beam_files(config()) -> ok.
beam_files(Config) ->
    Path = filename:join(proplists:get_value(priv_dir, Config), "decompiled.erl"),
    BeamPath = filename:join([code:lib_dir(katana_code), "ebin", "ktn_code.beam"]),
    try
        ok = ktn_code:beam_to_erl(BeamPath, Path),
        {ok, Source} = file:read_file(Path),
        {_, _} = binary:match(Source, <<"parse_tree">>),
        {error, beam_lib, _} = ktn_code:beam_to_erl(<<>>, Path)
    after
        ok = file:delete(Path)
    end,
    ok.

-spec source_encodings(config()) -> ok.
source_encodings(_Config) ->
    Source = <<"%% coding: utf-8\n-module(x).">>,
    #{type := root, content := [_, #{type := module}]} = ktn_code:parse_tree(Source),
    ok.
