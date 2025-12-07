-module(testmodule).
-export([start/0, world/0]).

%% Main entry point
start() ->
    io:format("Hello from AtomVM!~n"),
    io:format("Module: testmodule~n"),
    world(),
    {ok, testmodule}.

%% Example function
world() ->
    io:format("Hello, World from Testmodule!~n"),
    io:format("Running in browser via WebAssembly~n"),
    {ok, browser, ready}.
