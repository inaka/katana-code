-module(ktn_syntax_corpus_SUITE).

-export([all/0, expression_forms/1, type_forms/1]).

-behaviour(ct_suite).

-type config() :: [{atom(), term()}].
-export_type([config/0]).

-spec all() -> [atom()].
all() ->
    [expression_forms, type_forms].

-spec expression_forms(config()) -> ok.
expression_forms(_Config) ->
    Sources = [
        "f(X) -> {1, 1.5, $a, \"text\", [a | X], [], <<X:8/integer>>}.",
        "f(X) -> X = -1, X + 2.",
        "f(X) -> #{key => X}, X#{key := value}.",
        "f(X) -> if X -> true; true -> false end.",
        "f(X) -> case X of {a, Y} when is_atom(Y) -> Y; _ -> ok end.",
        "f() -> try run() catch error:R -> R after cleanup() end.",
        "f() -> try run() of R -> R catch error:E -> E after cleanup() end.",
        "f() -> catch run().",
        "f() -> receive M -> M end.",
        "f() -> receive M -> M after 0 -> timeout end.",
        "f(X) -> fun local/1, fun mod:remote/2, fun(X) -> X end, fun Loop(A) -> Loop(A) end.",
        "f(X) -> #r{key = X}, X#r{key = 1}, X#r.key, #r.key.",
        "f(X) -> [A || A <- X, A > 0].",
        "f(X) -> << <<A>> || <<A>> <= X >>.",
        "f(X) -> #{A => A || A <- X}.",
        "f() -> ?constant, ?VARIABLE, ?CALL(a, b).",
        "f(X) -> X(a), mod:f(X)."
    ],
    lists:foreach(
        fun(Source) ->
            #{content := [#{type := function, content := [_ | _]}]} = ktn_code:parse_tree(Source)
        end,
        Sources
    ),
    ok.

-spec type_forms(config()) -> ok.
type_forms(_Config) ->
    Sources = [
        "-record(r, {key = ok :: atom()}).",
        "-type t() :: atom() | integer().",
        "-type t(A) :: {A, [atom()], #{atom() => integer()}}.",
        "-type t() :: fun((atom()) -> integer()).",
        "-type t() :: fun((...) -> any()).",
        "-type t() :: remote:type(atom()).",
        "-type t() :: <<_:8, _:_*8>>.",
        "-type t() :: 1..10.",
        "-opaque t() :: atom().",
        "-spec f(A) -> A when A :: atom().",
        "-spec mod:f(atom()) -> integer().",
        "-type t() :: ?WRAP({1, atom()})."
    ],
    lists:foreach(
        fun(Source) ->
            #{content := [#{type := Kind}]} = ktn_code:parse_tree(Source),
            true = lists:member(Kind, [record_attr, type_attr, opaque, spec])
        end,
        Sources
    ),
    ok.
