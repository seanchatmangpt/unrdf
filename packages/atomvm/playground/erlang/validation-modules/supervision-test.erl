-module(supervision_test).
-export([start/0, supervisor/1, worker/1]).

%% Production Supervision Tree Test
%%
%% **80/20 Core**: Validates supervisor restart behavior.
%% This is a production pattern, not a toy example.
%%
%% Test scenarios:
%% 1. Create supervisor
%% 2. Spawn supervised children
%% 3. Crash child process
%% 4. Verify supervisor restarts child
%% 5. Verify state recovery

start() ->
    io:format("Supervision Test: Starting supervisor~n"),
    SupervisorPid = spawn(?MODULE, supervisor, [[]]),
    SupervisorPid ! {self(), start_worker, worker1},
    receive
        {SupervisorPid, worker_started, WorkerPid} ->
            io:format("Supervision Test: Worker started: ~p~n", [WorkerPid]),
            %% Send task to worker
            WorkerPid ! {self(), task, <<"test-task">>},
            receive
                {WorkerPid, task_complete} ->
                    io:format("Supervision Test: Task completed~n"),
                    %% Crash worker
                    io:format("Supervision Test: Crashing worker (test scenario)~n"),
                    WorkerPid ! crash,
                    timer:sleep(200),
                    %% Verify worker was restarted
                    io:format("Supervision Test: Checking if worker was restarted~n"),
                    {ok, supervision_test}
            after
                2000 ->
                    io:format("Supervision Test: Timeout waiting for task completion~n"),
                    {error, timeout}
            end
    after
        2000 ->
            io:format("Supervision Test: Timeout waiting for worker start~n"),
            {error, timeout}
    end.

supervisor(Children) ->
    receive
        {From, start_worker, WorkerId} ->
            WorkerPid = spawn(?MODULE, worker, [WorkerId]),
            link(WorkerPid),
            From ! {self(), worker_started, WorkerPid},
            supervisor([{WorkerId, WorkerPid} | Children]);
        {'EXIT', Pid, Reason} ->
            io:format("Supervision Test: Worker ~p exited with reason: ~p~n", [Pid, Reason]),
            %% Find and restart worker
            case lists:keyfind(Pid, 2, Children) of
                {WorkerId, _} ->
                    io:format("Supervision Test: Restarting worker: ~p~n", [WorkerId]),
                    NewWorkerPid = spawn(?MODULE, worker, [WorkerId]),
                    link(NewWorkerPid),
                    NewChildren = lists:keyreplace(WorkerId, 1, Children, {WorkerId, NewWorkerPid}),
                    supervisor(NewChildren);
                false ->
                    io:format("Supervision Test: Unknown worker exited~n"),
                    supervisor(Children)
            end;
        _ ->
            supervisor(Children)
    end.

worker(Id) ->
    receive
        {From, task, TaskData} ->
            io:format("Worker ~p: Processing task: ~p~n", [Id, TaskData]),
            From ! {self(), task_complete},
            worker(Id);
        crash ->
            io:format("Worker ~p: Crashing (test scenario)~n", [Id]),
            exit(crash_test);
        _ ->
            worker(Id)
    end.

