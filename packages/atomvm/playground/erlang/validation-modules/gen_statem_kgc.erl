-module(gen_statem_kgc).
-export([start/0, run_test/0]).

%% gen_statem with KGC-4D and JavaScript Bridge Integration
%%
%% Demonstrates:
%% - State machine with KGC-4D event logging
%% - JavaScript bridge control
%% - State transitions tracked in KGC-4D

start() ->
    io:format("=== gen_statem + KGC-4D + JS Bridge ===~n"),
    
    %% Start state machine
    {ok, Pid} = gen_statem:start(),
    io:format("State machine started: ~p~n", [Pid]),
    
    %% Wait a bit for initialization
    timer:sleep(100),
    
    %% Test 1: Press buttons via Erlang API
    io:format("~n=== Test 1: Erlang API ===~n"),
    io:format("Pressing buttons: 1, 2, 3, 4~n"),
    gen_statem:button(1),
    timer:sleep(100),
    gen_statem:button(2),
    timer:sleep(100),
    gen_statem:button(3),
    timer:sleep(100),
    gen_statem:button(4),
    timer:sleep(200),
    
    {ok, State1, Data1} = gen_statem:get_state(),
    io:format("Final state: ~p, data: ~p~n", [State1, Data1]),
    
    %% Test 2: Get state
    io:format("~n=== Test 2: State Query ===~n"),
    {ok, State2, Data2} = gen_statem:get_state(),
    io:format("Current state: ~p, data: ~p~n", [State2, Data2]),
    
    %% Test 3: Wrong code (test lockout)
    io:format("~n=== Test 3: Wrong Code (Lockout Test) ===~n"),
    io:format("Pressing wrong buttons: 9, 9, 9, 9 (3 times to trigger lockout)~n"),
    gen_statem:button(9),
    timer:sleep(100),
    gen_statem:button(9),
    timer:sleep(100),
    gen_statem:button(9),
    timer:sleep(100),
    gen_statem:button(9),
    timer:sleep(200),
    
    %% Repeat to trigger lockout
    gen_statem:button(9),
    timer:sleep(100),
    gen_statem:button(9),
    timer:sleep(100),
    gen_statem:button(9),
    timer:sleep(100),
    gen_statem:button(9),
    timer:sleep(200),
    
    {ok, State3, Data3} = gen_statem:get_state(),
    io:format("State after wrong codes: ~p, data: ~p~n", [State3, Data3]),
    
    %% Test 4: Stop
    io:format("~n=== Test 4: Stop ===~n"),
    gen_statem:stop(),
    timer:sleep(100),
    
    io:format("~n=== Test Complete ===~n"),
    io:format("Check KGC-4D events for: STATE_TRANSITION, CODE_CORRECT, CODE_INCORRECT, BUTTON_PRESSED~n"),
    ok.

%% Run test
run_test() ->
    start().

