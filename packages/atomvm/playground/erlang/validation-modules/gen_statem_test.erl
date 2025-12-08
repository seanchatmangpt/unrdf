-module(gen_statem_test).
-export([start/0, run_test/0]).

%% Test module for gen_statem (Production)
%% Demonstrates production state machine behavior

start() ->
    io:format("=== gen_statem Production Test ===~n"),
    
    %% Start state machine
    {ok, Pid} = gen_statem:start(),
    io:format("State machine started: ~p~n", [Pid]),
    
    %% Test 1: Get initial state (should be locked)
    {ok, State1, Data1} = gen_statem:get_state(),
    io:format("Initial state: ~p, data: ~p~n", [State1, Data1]),
    
    %% Test 2: Press wrong buttons
    io:format("~nPressing wrong code: 5, 6, 7, 8~n"),
    gen_statem:button(5),
    timer:sleep(100),
    gen_statem:button(6),
    timer:sleep(100),
    gen_statem:button(7),
    timer:sleep(100),
    gen_statem:button(8),
    timer:sleep(100),
    
    {ok, State2, Data2} = gen_statem:get_state(),
    io:format("After wrong code: state=~p, data=~p~n", [State2, Data2]),
    
    %% Test 3: Press correct code (1, 2, 3, 4)
    io:format("~nPressing correct code: 1, 2, 3, 4~n"),
    gen_statem:button(1),
    timer:sleep(100),
    gen_statem:button(2),
    timer:sleep(100),
    gen_statem:button(3),
    timer:sleep(100),
    gen_statem:button(4),
    timer:sleep(200),
    
    {ok, State3, Data3} = gen_statem:get_state(),
    io:format("After correct code: state=~p, data=~p~n", [State3, Data3]),
    
    %% Test 4: Press button while open (should be ignored)
    io:format("~nPressing button while open: 9~n"),
    gen_statem:button(9),
    timer:sleep(100),
    
    {ok, State4, Data4} = gen_statem:get_state(),
    io:format("After button while open: state=~p, data=~p~n", [State4, Data4]),
    
    %% Test 5: Stop state machine
    io:format("~nStopping state machine...~n"),
    gen_statem:stop(),
    timer:sleep(100),
    
    io:format("~n=== Test Complete ===~n"),
    ok.

%% Run test (entry point)
run_test() ->
    start().
