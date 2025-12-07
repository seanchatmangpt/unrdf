-module(process-test).
-export([start/0, world/0]).

%% Main entry point
start() ->
    io:format("Hello from AtomVM!~n"),
    io:format("Module: process-test~n"),
    world(),
    {ok, process-test}.

%% Example function
world() ->
    io:format("Hello, World from Process-test!~n"),
    io:format("Running in browser via WebAssembly~n"),
    {ok, browser, ready}.
