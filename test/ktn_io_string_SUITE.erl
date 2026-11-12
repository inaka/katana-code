-module(ktn_io_string_SUITE).

-export([all/0, characters/1, lines/1, positions/1, skip/1, idle_timeout/1]).

-behaviour(ct_suite).

-type config() :: [{atom(), term()}].
-export_type([config/0]).

-spec all() -> [atom()].
all() ->
    [characters, lines, positions, skip, idle_timeout].

-spec characters(config()) -> ok.
characters(_Config) ->
    Device = ktn_io_string:new(<<"abcdef">>),
    try
        "a" = io:get_chars(Device, "", 1),
        "bcd" = io:get_chars(Device, "", 3),
        "ef" = lists:flatten(io:get_chars(Device, "", 10)),
        eof = io:get_chars(Device, "", 1),
        {error, request} = io:request(Device, unsupported_request)
    after
        ok = file:close(Device)
    end,
    ok.

-spec lines(config()) -> ok.
lines(_Config) ->
    Device = ktn_io_string:new("one\r\ntwo\nthree\rfour"),
    try
        "one" = io:get_line(Device, ""),
        "two" = io:get_line(Device, ""),
        "three" = io:get_line(Device, ""),
        "four" = io:get_line(Device, ""),
        eof = io:get_line(Device, "")
    after
        ok = file:close(Device)
    end,
    ok.

-spec positions(config()) -> ok.
positions(_Config) ->
    Device = ktn_io_string:new("abcdef"),
    try
        Device ! ignored_message,
        {ok, 3} = file:position(Device, 3),
        "def" = io:get_chars(Device, "", 3),
        {ok, 0} = file:position(Device, 0),
        "abc" = io:get_chars(Device, "", 3)
    after
        ok = file:close(Device)
    end,
    ok.

-spec skip(config()) -> ok.
skip(_Config) ->
    {more, First} = ktn_io_string:skip("abcdef", ignored, 3),
    {more, Second} = ktn_io_string:skip(First, 3),
    {more, Third} = ktn_io_string:skip(Second, 3),
    {more, Fourth} = ktn_io_string:skip(Third, 3),
    {done, 3, "def"} = ktn_io_string:skip(Fourth, 3),
    {done, eof, []} = ktn_io_string:skip({cont, 1, []}, 1),
    {done, 0, "abc"} = ktn_io_string:skip({cont, 0, "abc"}, 0),
    ok.

-spec idle_timeout(config()) -> ok.
idle_timeout(_Config) ->
    State = #{buffer => "abc", original => "abc", idle_timeout => 50},
    {Device, Ref} = spawn_monitor(ktn_io_string, loop, [State]),
    %% Each recursive receive must retain this device's short timeout.
    "a" = io:get_chars(Device, "", 1),
    {ok, 0} = file:position(Device, 0),
    Device ! ignored_message,
    "a" = io:get_chars(Device, "", 1),
    receive
        {'DOWN', Ref, process, Device, timeout} -> ok
    after 5000 ->
        exit(Device, kill),
        ct:fail(idle_device_did_not_exit)
    end.
