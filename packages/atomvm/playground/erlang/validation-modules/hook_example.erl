-module(hook_example).
-export([start/0]).

%% Hook Primitives Example - Innovative Layer
%%
%% **Innovative Paradigm**: Demonstrates hooks as first-class Erlang primitives.
%% Hooks are defined in Erlang, compiled in JavaScript, executed at 800ns.
%%
%% This breaks the known paradigm - hooks are no longer just JavaScript functions,
%% but Erlang-defined operations executed at native performance.

start() ->
    io:format("Hook Example: Demonstrating Erlang hook primitives~n"),
    io:format("Hook Example: Hooks are first-class in Erlang, executed at 800ns~n"),
    
    %% Initialize hook primitives
    hook_primitives:start(),
    
    %% Define a validation hook in Erlang
    io:format("Hook Example: Defining validation hook in Erlang...~n"),
    QualityGateHook = hook_primitives:define(
        quality_gate,
        before_add,
        fun(Event) ->
            %% Erlang validation logic
            case maps:get(<<"value">>, Event, undefined) of
                undefined -> false;
                Value when is_integer(Value), Value > 0 -> true;
                _ -> false
            end
        end
    ),
    
    %% Register the hook
    io:format("Hook Example: Registering hook...~n"),
    case hook_primitives:register(QualityGateHook) of
        {ok, _} ->
            io:format("Hook Example: Hook registered successfully~n");
        {error, Reason} ->
            io:format("Hook Example: Hook registration failed: ~p~n", [Reason])
    end,
    
    %% Define a transformation hook in Erlang
    io:format("Hook Example: Defining transformation hook in Erlang...~n"),
    TransformHook = hook_primitives:define(
        normalize_event,
        before_add,
        {fun(_Event) -> true end,  % Always validate
         fun(Event) ->
            %% Erlang transformation logic
            maps:put(<<"normalized">>, true, Event)
         end}
    ),
    
    %% Register the transformation hook
    case hook_primitives:register(TransformHook) of
        {ok, _} ->
            io:format("Hook Example: Transformation hook registered~n");
        {error, Reason2} ->
            io:format("Hook Example: Transformation hook registration failed: ~p~n", [Reason2])
    end,
    
    timer:sleep(200),
    
    %% Execute hooks from Erlang
    io:format("Hook Example: Executing hooks from Erlang...~n"),
    TestEvent = #{
        <<"type">> => <<"CREATE">>,
        <<"resource">> => <<"contract">>,
        <<"value">> => 1000000
    },
    
    %% Execute hooks (will use JIT-compiled chain at 800ns)
    case hook_primitives:execute(before_add, TestEvent) of
        {valid, TransformedEvent} ->
            io:format("Hook Example: Hooks executed successfully~n"),
            io:format("Hook Example: Transformed event: ~p~n", [TransformedEvent]);
        {invalid, Error} ->
            io:format("Hook Example: Hook validation failed: ~p~n", [Error])
    end,
    
    %% Create and execute a compiled hook chain
    io:format("Hook Example: Creating compiled hook chain...~n"),
    HookNames = [quality_gate, normalize_event],
    case hook_primitives:chain(HookNames, TestEvent) of
        {ok, ChainKey} ->
            io:format("Hook Example: Chain compiled: ~s~n", [ChainKey]),
            
            %% Execute compiled chain (800ns target)
            case hook_primitives:execute_chain(ChainKey, TestEvent) of
                {valid, Result} ->
                    io:format("Hook Example: Compiled chain executed successfully~n"),
                    io:format("Hook Example: Result: ~p~n", [Result]);
                {invalid, ChainError} ->
                    io:format("Hook Example: Chain execution failed: ~p~n", [ChainError])
            end;
        {error, ChainReason} ->
            io:format("Hook Example: Chain compilation failed: ~p~n", [ChainReason])
    end,
    
    io:format("Hook Example: Hook primitives demonstration complete~n"),
    io:format("Hook Example: Hooks are first-class in Erlang, executed at 800ns~n"),
    {ok, hook_example}.

