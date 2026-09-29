%% Integers above 2^27 are boxed on 32-bit WASM; prove arithmetic on them is right.
-module(big_ints).
-export([start/0]).
start() ->
    erlang:display({big, 100000000 * 3, 250000000 * 4}),
    ok.
