-module(testmodule3).
-export([start/0, world/0]).

%% Main entry point
start() ->
    io:format("Hello from AtomVM!~n"),
    io:format("Module: testmodule3~n"),
    world(),
    {ok, testmodule3}.

%% Example function
world() ->
    io:format("Hello, World from Testmodule3!~n"),
    io:format("Running in browser via WebAssembly~n"),
    {ok, browser, ready}.
