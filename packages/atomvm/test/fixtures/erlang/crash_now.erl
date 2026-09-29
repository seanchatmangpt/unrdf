%% Crashes immediately. Used to prove crashes surface as non-zero exit, never as success.
-module(crash_now).
-export([start/0]).
start() -> erlang:error(intentional_crash).
