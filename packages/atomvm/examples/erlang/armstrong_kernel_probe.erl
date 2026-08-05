-module(armstrong_kernel_probe).
-export([start/0]).

marker(Name) -> io:format("{armstrong_kernel,~p,ok}~n", [Name]).

start() ->
    isolated_state(),
    sender_order(),
    selective_receive(),
    crash_isolation(),
    links_and_trap_exit(),
    monitors_down(),
    restart_after_failure(),
    tail_recursive_server(),
    io:format("{atomvm_armstrong_kernel_alive,8}~n", []),
    ok.

isolated_state() ->
    Parent = self(),
    Pid = spawn(fun() -> isolated_loop(0, Parent) end),
    Pid ! increment,
    Pid ! get,
    receive {isolated_state, Pid, 1} -> ok after 1000 -> erlang:error(isolation_failed) end,
    Pid ! stop,
    marker(isolated_state).

isolated_loop(State, Parent) ->
    receive
        increment -> isolated_loop(State + 1, Parent);
        get -> Parent ! {isolated_state, self(), State}, isolated_loop(State, Parent);
        stop -> ok
    end.

sender_order() ->
    Parent = self(),
    Pid = spawn(fun() -> ordered_receive(Parent) end),
    Pid ! {sequence, 1},
    Pid ! {sequence, 2},
    Pid ! {sequence, 3},
    receive {sender_order, Pid, [1, 2, 3]} -> ok after 1000 -> erlang:error(sender_order_failed) end,
    marker(sender_order).

ordered_receive(Parent) ->
    receive {sequence, A} ->
        receive {sequence, B} ->
            receive {sequence, C} -> Parent ! {sender_order, self(), [A, B, C]} end
        end
    end.

selective_receive() ->
    Parent = self(),
    Pid = spawn(fun() -> selective_loop(Parent) end),
    Pid ! {normal, Parent},
    Pid ! {urgent, Parent},
    receive {selected, urgent} -> ok after 1000 -> erlang:error(urgent_not_selected) end,
    receive {selected, normal} -> ok after 1000 -> erlang:error(normal_not_retained) end,
    marker(selective_receive).

selective_loop(Parent) ->
    receive {urgent, Parent} -> Parent ! {selected, urgent} end,
    receive {normal, Parent} -> Parent ! {selected, normal} end.

crash_isolation() ->
    Parent = self(),
    spawn(fun() -> Parent ! child_started, exit(deliberate_crash) end),
    receive child_started -> ok after 1000 -> erlang:error(child_not_started) end,
    Parent ! parent_alive,
    receive parent_alive -> ok after 1000 -> erlang:error(parent_did_not_survive) end,
    marker(crash_isolation).

links_and_trap_exit() ->
    Previous = process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> exit(linked_failure) end),
    receive {'EXIT', Pid, linked_failure} -> ok after 1000 -> erlang:error(link_exit_missing) end,
    process_flag(trap_exit, Previous),
    marker(links_and_trap_exit).

monitors_down() ->
    Pid = spawn(fun() -> receive stop -> ok end end),
    Ref = monitor(process, Pid),
    Pid ! stop,
    receive {'DOWN', Ref, process, Pid, normal} -> ok after 1000 -> erlang:error(down_missing) end,
    marker(monitors_down).

restart_after_failure() ->
    Parent = self(),
    Supervisor = spawn(fun() -> restart_supervisor(Parent, 0) end),
    receive {child_ready, Supervisor, First} -> First ! fail,
        receive {child_restarted, Supervisor, First, Second} when First =/= Second ->
            Second ! stop,
            Supervisor ! stop
        after 1000 -> erlang:error(restart_missing)
        end
    after 1000 -> erlang:error(child_not_ready)
    end,
    marker(restart_after_failure).

restart_supervisor(Parent, Generation) ->
    Child = spawn(fun restart_child/0),
    Ref = monitor(process, Child),
    Parent ! {child_ready, self(), Child},
    restart_supervisor_wait(Parent, Generation, Child, Ref).

restart_supervisor_wait(Parent, Generation, Child, Ref) ->
    receive
        {'DOWN', Ref, process, Child, deliberate_failure} ->
            Next = spawn(fun restart_child/0),
            NextRef = monitor(process, Next),
            Parent ! {child_restarted, self(), Child, Next},
            restart_supervisor_wait(Parent, Generation + 1, Next, NextRef);
        {'DOWN', Ref, process, Child, normal} ->
            receive stop -> ok after 0 -> ok end;
        stop -> Child ! stop, ok
    end.

restart_child() ->
    receive
        fail -> exit(deliberate_failure);
        stop -> ok
    end.

tail_recursive_server() ->
    Parent = self(),
    Server = spawn(fun() -> counter_server(0) end),
    Server ! increment,
    Server ! increment,
    Server ! {get, Parent},
    receive {counter_value, Server, 2} -> ok after 1000 -> erlang:error(tail_server_failed) end,
    Server ! stop,
    marker(tail_recursive_server).

counter_server(State) ->
    receive
        increment -> counter_server(State + 1);
        {get, From} -> From ! {counter_value, self(), State}, counter_server(State);
        stop -> ok
    end.
