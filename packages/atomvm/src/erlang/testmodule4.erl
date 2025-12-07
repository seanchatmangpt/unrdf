-module(testmodule4).
-export([start/0, world/0]).

%% Main entry point
start() ->
    io:format("Hello from AtomVM!~n"),
    io:format("Module: testmodule4~n"),
    world(),
    {ok, testmodule4}.

%% Example function
world() ->
    io:format("Hello, World from Testmodule4!~n"),
    io:format("Running in browser via WebAssembly~n"),
    {ok, browser, ready}.
