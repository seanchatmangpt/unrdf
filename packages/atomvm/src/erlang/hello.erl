-module(hello).
-export([start/0, world/0]).

%% Main entry point
start() ->
    io:format("Hello from AtomVM!~n"),
    io:format("Module: hello~n"),
    world(),
    {ok, hello}.

%% Example function
world() ->
    io:format("Hello, World from Hello!~n"),
    io:format("Running in browser via WebAssembly~n"),
    {ok, browser, ready}.
