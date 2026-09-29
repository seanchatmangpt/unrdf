%% Never terminates. Used to prove the broker's timeout kills runaway programs.
-module(loop_forever).
-export([start/0]).
start() -> loop(0).
loop(N) -> loop((N + 1) rem 1000).
