-module(testmodule2).
-export([start/0, world/0]).

%% Main entry point
start() ->
    io:format("Hello from AtomVM!~n"),
    io:format("Module: testmodule2~n"),
    world(),
    {ok, testmodule2}.

%% Example function
world() ->
    io:format("Hello, World from Testmodule2!~n"),
    io:format("Running in browser via WebAssembly~n"),
    {ok, browser, ready}.
