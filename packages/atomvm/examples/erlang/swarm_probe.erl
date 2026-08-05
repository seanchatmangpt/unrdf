-module(swarm_probe).
-export([start/0]).

%% Minimal tracer bullet executed by the real AtomVM Generic UNIX runtime.
%% Generic UNIX reports process success only when start/0 returns the atom ok.
%% The marker is consumed by AtomVMProcessBroker and bound into its receipt.
start() ->
    erlang:display({atomvm_swarm_alive, ok}),
    ok.
