%% Precompiled (test/fixtures/beams/fallback_probe.beam) so the packer-fallback test needs no compiler.
-module(fallback_probe).
-export([start/0]).
start() ->
    erlang:display({atomvm_module_alive, fallback_probe}),
    ok.
