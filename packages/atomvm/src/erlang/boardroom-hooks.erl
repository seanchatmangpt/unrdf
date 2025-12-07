-module(boardroom-hooks).
-export([start/0, world/0]).

%% Main entry point
start() ->
    io:format("Hello from AtomVM!~n"),
    io:format("Module: boardroom-hooks~n"),
    world(),
    {ok, boardroom-hooks}.

%% Example function
world() ->
    io:format("Hello, World from Boardroom-hooks!~n"),
    io:format("Running in browser via WebAssembly~n"),
    {ok, browser, ready}.
