-module(gen_statem_bridge).
-export([start/0, handle_bridge_command/1]).

%% Bridge Handler for gen_statem from JavaScript
%%
%% **Production**: Listens for bridge commands and forwards to state machine
%% This bridges the gap between JavaScript commands and Erlang message passing

start() ->
    io:format("gen_statem Bridge: Starting bridge handler~n"),
    %% Start state machine first
    {ok, _Pid} = gen_statem:start(),
    io:format("gen_statem Bridge: State machine started~n"),
    ok.

%% Handle bridge command from JavaScript
%% Format: GEN_STATEM_BUTTON:<Digit>
%% Format: GEN_STATEM_GET_STATE
%% Format: GEN_STATEM_STOP
handle_bridge_command(Command) ->
    case string:split(Command, ":", all) of
        ["GEN_STATEM_BUTTON", DigitStr] ->
            Digit = list_to_integer(DigitStr),
            gen_statem:button(Digit),
            ok;
        ["GEN_STATEM_GET_STATE"] ->
            case gen_statem:get_state() of
                {ok, State, Data} ->
                    io:format("GEN_STATEM_STATE:~p:~p~n", [State, Data]),
                    ok;
                {error, Reason} ->
                    io:format("GEN_STATEM_STATE_ERROR:~p~n", [Reason]),
                    {error, Reason}
            end;
        ["GEN_STATEM_STOP"] ->
            gen_statem:stop(),
            ok;
        _ ->
            io:format("gen_statem Bridge: Unknown command: ~s~n", [Command]),
            {error, unknown_command}
    end.

