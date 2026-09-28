%% Spawns five workers that each send back a partial result; sums them.
%% Exercises real multi-process scheduling and message passing.
-module(spawn_workers).
-export([start/0]).
start() ->
    Me = self(),
    Pids = [spawn(fun() -> Me ! {result, self(), N * N} end) || N <- [1, 2, 3, 4, 5]],
    Sum = collect(Pids, 0),
    erlang:display({workers_sum, Sum}),
    ok.
collect([], Acc) -> Acc;
collect([Pid | Rest], Acc) ->
    receive
        {result, Pid, V} -> collect(Rest, Acc + V)
    after 2000 -> erlang:error(worker_timeout)
    end.
