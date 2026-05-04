-module(boardroom-swarm).
-export([start/0, world/0]).

%% Main entry point
start() ->
    io:format("Hello from AtomVM!~n"),
    io:format("Module: boardroom-swarm~n"),
    world(),
    {ok, boardroom-swarm}.

%% Example function
world() ->
    io:format("Hello, World from Boardroom-swarm!~n"),
    io:format("Running in browser via WebAssembly~n"),
    {ok, browser, ready}.
