-module(boardroom-intent).
-export([start/0, world/0]).

%% Main entry point
start() ->
    io:format("Hello from AtomVM!~n"),
    io:format("Module: boardroom-intent~n"),
    world(),
    {ok, boardroom-intent}.

%% Example function
world() ->
    io:format("Hello, World from Boardroom-intent!~n"),
    io:format("Running in browser via WebAssembly~n"),
    {ok, browser, ready}.
