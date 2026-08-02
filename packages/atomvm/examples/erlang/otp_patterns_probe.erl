-module(otp_patterns_probe).
-export([start/0]).

start() ->
    pattern_01_immutable_messages(),
    pattern_02_closed_protocols(),
    pattern_03_state_as_value(),
    pattern_04_result_railway(),
    pattern_05_domain_types(),
    pattern_06_pure_handlers(),
    pattern_07_compose_by_purpose(),
    pattern_08_railway_composition(),
    pattern_09_test_without_framework(),
    pattern_10_skinny_left_margin(),
    pattern_11_process_boundary(),
    pattern_12_tell(),
    pattern_13_ask_timeout(),
    pattern_14_stable_reference(),
    pattern_15_named_process(),
    pattern_16_trap_exits(),
    pattern_17_let_it_crash(),
    pattern_18_supervision_tree(),
    pattern_19_restart_intensity(),
    pattern_20_supervised_startup(),
    pattern_21_links(),
    pattern_22_monitors(),
    pattern_23_retry_fresh_state(),
    pattern_24_state_machine(),
    pattern_25_event_broadcasting(),
    pattern_26_timed_messages(),
    pattern_27_fan_out_fail_fast(),
    pattern_28_process_introspection(),
    pattern_29_assemble_application(),
    pattern_30_test_boundary(),
    erlang:display({atomvm_otp_patterns_alive, 30}),
    ok.

marker(Name) -> erlang:display({otp_pattern, Name, ok}).
assert(true) -> ok;
assert(false) -> erlang:error(assertion_failed).

pattern_01_immutable_messages() ->
    Msg = {gps_update, vehicle_1, {34.1, -118.2, 1}},
    assert(Msg =:= {gps_update, vehicle_1, {34.1, -118.2, 1}}),
    marker(immutable_messages).

handle_command({State, _Route}, {assign_route, Route}) -> {en_route, Route};
handle_command({_State, Route}, emergency_stop) -> {stopped, Route};
handle_command(State, request_status) -> State.

pattern_02_closed_protocols() ->
    assert(handle_command({idle, none}, {assign_route, route_7}) =:= {en_route, route_7}),
    assert(handle_command({en_route, route_7}, emergency_stop) =:= {stopped, route_7}),
    marker(sealed_message_protocols).

with_fuel({vehicle_state, Id, Position, _Fuel, Status}, Fuel) ->
    {vehicle_state, Id, Position, Fuel, Status}.

pattern_03_state_as_value() ->
    Initial = {vehicle_state, vehicle_3, {0, 0}, 100, idle},
    Next = with_fuel(Initial, 48),
    assert(Initial =:= {vehicle_state, vehicle_3, {0, 0}, 100, idle}),
    assert(Next =:= {vehicle_state, vehicle_3, {0, 0}, 48, idle}),
    marker(state_as_value).

result_map({ok, Value}, Fun) -> {ok, Fun(Value)};
result_map({error, Reason}, _Fun) -> {error, Reason}.
result_flat_map({ok, Value}, Fun) -> Fun(Value);
result_flat_map({error, Reason}, _Fun) -> {error, Reason}.

pattern_04_result_railway() ->
    Result = result_flat_map(result_map({ok, 2}, fun(V) -> V * 3 end),
        fun(V) when V =:= 6 -> {ok, complete}; (_) -> {error, bad_value} end),
    assert(Result =:= {ok, complete}),
    marker(result_railway).

vehicle_id(Value) when is_atom(Value) -> {vehicle_id, Value}.
fuel_percent(Value) when Value >= 0, Value =< 100 -> {fuel_percent, Value}.

pattern_05_domain_types() ->
    assert(vehicle_id(vehicle_5) =:= {vehicle_id, vehicle_5}),
    assert(fuel_percent(75) =:= {fuel_percent, 75}),
    marker(domain_types_over_primitives).

telemetry_handler({Lat, Lng, Fuel}, {gps, NewLat, NewLng}) -> {NewLat, NewLng, Fuel};
telemetry_handler({Lat, Lng, _Fuel}, {fuel, NewFuel}) -> {Lat, Lng, NewFuel}.

pattern_06_pure_handlers() ->
    assert(telemetry_handler({0, 0, 75}, {gps, 40, -74}) =:= {40, -74, 75}),
    marker(pure_state_handlers).

apply_gps({_, _, Fuel}, {gps, Lat, Lng}) -> {Lat, Lng, Fuel}.
apply_fuel({Lat, Lng, _}, {fuel, Fuel}) -> {Lat, Lng, Fuel}.
composed_handler(State, {gps, _, _} = Msg) -> apply_gps(State, Msg);
composed_handler(State, {fuel, _} = Msg) -> apply_fuel(State, Msg).

pattern_07_compose_by_purpose() ->
    assert(composed_handler({0, 0, 75}, {gps, 51, 0}) =:= {51, 0, 75}),
    marker(compose_by_purpose).

validate_request({request, Vehicle, Part}) -> {ok, {valid, Vehicle, Part}}.
lookup_vehicle({valid, vehicle_8, Part}) -> {ok, {vehicle, vehicle_8, Part}};
lookup_vehicle(_) -> {error, unknown_vehicle}.
check_parts({vehicle, Vehicle, brake_pad}) -> {ok, {parts, Vehicle, brake_pad}};
check_parts(_) -> {error, part_unavailable}.
create_work_order({parts, Vehicle, Part}) -> {ok, {work_order, Vehicle, Part}}.

pattern_08_railway_composition() ->
    Result = result_flat_map(validate_request({request, vehicle_8, brake_pad}),
        fun(V) -> result_flat_map(lookup_vehicle(V),
            fun(Vehicle) -> result_flat_map(check_parts(Vehicle), fun create_work_order/1) end) end),
    assert(Result =:= {ok, {work_order, vehicle_8, brake_pad}}),
    marker(railway_composition).

pattern_09_test_without_framework() ->
    assert(telemetry_handler({0, 0, 1}, {fuel, 10}) =:= {0, 0, 10}),
    marker(test_without_framework).

check_quantity({order, Id, Quantity}) when Quantity > 0 -> {ok, {confirmed, Id, Quantity}};
check_quantity(_) -> {error, bad_quantity}.
process_order(Order) -> result_flat_map({ok, Order}, fun check_quantity/1).

pattern_10_skinny_left_margin() ->
    assert(process_order({order, order_10, 2}) =:= {ok, {confirmed, order_10, 2}}),
    marker(skinny_left_margin).

counter_loop(State) ->
    receive
        increment -> counter_loop(State + 1);
        {call, From, Ref, get} -> From ! {reply, Ref, State}, counter_loop(State);
        {call, From, Ref, increment} ->
            Next = State + 1,
            From ! {reply, Ref, Next},
            counter_loop(Next);
        {sys, From, Ref, state} -> From ! {sys_reply, Ref, State}, counter_loop(State);
        crash -> exit(intentional_crash);
        stop -> ok
    end.

call(Pid, Request, Timeout) ->
    Ref = make_ref(),
    Pid ! {call, self(), Ref, Request},
    receive {reply, Ref, Reply} -> Reply after Timeout -> timeout end.

pattern_11_process_boundary() ->
    Pid = spawn(fun() -> counter_loop(0) end),
    Pid ! increment,
    assert(call(Pid, get, 1000) =:= 1),
    Pid ! stop,
    marker(process_as_boundary).

pattern_12_tell() ->
    Pid = spawn(fun() -> counter_loop(0) end),
    Pid ! increment,
    Pid ! increment,
    Pid ! increment,
    assert(call(Pid, get, 1000) =:= 3),
    Pid ! stop,
    marker(tell_dont_block).

silent_loop() -> receive stop -> ok end.

pattern_13_ask_timeout() ->
    Pid = spawn(fun silent_loop/0),
    assert(call(Pid, get, 10) =:= timeout),
    Pid ! stop,
    marker(ask_with_timeout).

ref_loop(Current) ->
    receive
        {tell, Msg} -> Current ! Msg, ref_loop(Current);
        {call, From, Ref, Request} ->
            From ! {reply, Ref, call(Current, Request, 1000)},
            ref_loop(Current);
        {swap, Next} -> ref_loop(Next);
        stop -> Current ! stop, ok
    end.

ref_call(RefPid, Request) ->
    Ref = make_ref(),
    RefPid ! {call, self(), Ref, Request},
    receive {reply, Ref, Reply} -> Reply after 1000 -> timeout end.

pattern_14_stable_reference() ->
    First = spawn(fun() -> counter_loop(0) end),
    RefPid = spawn(fun() -> ref_loop(First) end),
    RefPid ! {tell, increment},
    assert(ref_call(RefPid, get) =:= 1),
    Second = spawn(fun() -> counter_loop(0) end),
    RefPid ! {swap, Second},
    RefPid ! {tell, increment},
    assert(ref_call(RefPid, get) =:= 1),
    First ! stop,
    RefPid ! stop,
    marker(stable_references).

pattern_15_named_process() ->
    Pid = spawn(fun() -> counter_loop(0) end),
    true = register(fleet_coordinator, Pid),
    assert(whereis(fleet_coordinator) =:= Pid),
    unregister(fleet_coordinator),
    Pid ! stop,
    marker(named_processes).

pattern_16_trap_exits() ->
    Previous = process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> exit(sensor_overflow) end),
    receive {'EXIT', Pid, sensor_overflow} -> ok after 1000 -> erlang:error(exit_not_trapped) end,
    process_flag(trap_exit, Previous),
    marker(trap_exits).

pattern_17_let_it_crash() ->
    Pid = spawn(fun() -> receive go -> exit(corrupt_telemetry) end end),
    Ref = monitor(process, Pid),
    Pid ! go,
    receive {'DOWN', Ref, process, Pid, corrupt_telemetry} -> ok after 1000 -> erlang:error(crash_not_observed) end,
    marker(let_it_crash).

one_for_one_sup(Parent) ->
    Child = spawn(fun() -> counter_loop(0) end),
    Ref = monitor(process, Child),
    Parent ! {child, Child},
    receive
        {'DOWN', Ref, process, Child, _Reason} ->
            Next = spawn(fun() -> counter_loop(0) end),
            Parent ! {restarted, Child, Next},
            one_for_one_sup_wait(Next)
    end.
one_for_one_sup_wait(Child) -> receive stop -> Child ! stop, ok end.

pattern_18_supervision_tree() ->
    Parent = self(),
    Sup = spawn(fun() -> one_for_one_sup(Parent) end),
    receive {child, Child} -> Child ! crash,
        receive {restarted, Child, Next} ->
            assert(call(Next, get, 1000) =:= 0),
            Sup ! stop
        after 1000 -> erlang:error(child_not_restarted) end
    after 1000 -> erlang:error(child_not_started) end,
    marker(supervision_trees).

restart_until_exceeded(Count, Max) when Count > Max -> exceeded;
restart_until_exceeded(Count, Max) ->
    Pid = spawn(fun() -> receive go -> exit(crash_loop) end end),
    Ref = monitor(process, Pid),
    Pid ! go,
    receive {'DOWN', Ref, process, Pid, crash_loop} -> restart_until_exceeded(Count + 1, Max) end.

pattern_19_restart_intensity() ->
    assert(restart_until_exceeded(0, 1) =:= exceeded),
    marker(restart_intensity).

startup_child(Parent) ->
    Parent ! {init_ack, self()},
    counter_loop(0).

pattern_20_supervised_startup() ->
    Parent = self(),
    Pid = spawn_link(fun() -> startup_child(Parent) end),
    receive {init_ack, Pid} -> ok after 1000 -> erlang:error(init_ack_timeout) end,
    Pid ! stop,
    marker(supervised_startup).

linked_victim(Observer) ->
    Right = spawn(fun() -> counter_loop(0) end),
    Ref = monitor(process, Right),
    Left = spawn(fun() -> link(Right), exit(shared_fate) end),
    receive {'DOWN', Ref, process, Right, shared_fate} -> Observer ! {shared_fate_ok, Left, Right}
    after 1000 -> Observer ! shared_fate_failed end.

pattern_21_links() ->
    Observer = self(),
    spawn(fun() -> linked_victim(Observer) end),
    receive {shared_fate_ok, _Left, _Right} -> ok; shared_fate_failed -> erlang:error(link_failed)
    after 1000 -> erlang:error(link_timeout) end,
    marker(links_shared_fate).

pattern_22_monitors() ->
    Pid = spawn(fun() -> receive stop -> ok end end),
    Ref = monitor(process, Pid),
    Pid ! stop,
    receive {'DOWN', Ref, process, Pid, normal} -> ok after 1000 -> erlang:error(monitor_failed) end,
    marker(monitors_observation).

attempt(Parent, Attempt) ->
    receive go ->
        case Attempt of
            3 -> Parent ! {attempt_ok, Attempt};
            _ -> exit({transient, Attempt})
        end
    end.

retry_attempt(Attempt, Max) when Attempt > Max -> {error, exhausted};
retry_attempt(Attempt, Max) ->
    Parent = self(),
    Pid = spawn(fun() -> attempt(Parent, Attempt) end),
    Ref = monitor(process, Pid),
    Pid ! go,
    receive
        {attempt_ok, Attempt} -> demonitor(Ref), {ok, Attempt};
        {'DOWN', Ref, process, Pid, _Reason} -> retry_attempt(Attempt + 1, Max)
    after 1000 -> {error, timeout}
    end.

pattern_23_retry_fresh_state() ->
    assert(retry_attempt(1, 3) =:= {ok, 3}),
    marker(retry_fresh_state).

state_machine(State, Trips) ->
    receive
        {event, start_tracking} when State =:= idle -> state_machine(tracking, Trips + 1);
        {event, pause_tracking} when State =:= tracking -> state_machine(idle, Trips);
        {event, begin_maintenance} when State =:= tracking -> state_machine(maintenance, Trips);
        {call, From, Ref, get} -> From ! {reply, Ref, {State, Trips}}, state_machine(State, Trips);
        stop -> ok;
        _ -> state_machine(State, Trips)
    end.

pattern_24_state_machine() ->
    Pid = spawn(fun() -> state_machine(idle, 0) end),
    Pid ! {event, start_tracking},
    Pid ! {event, pause_tracking},
    Pid ! {event, start_tracking},
    Pid ! {event, begin_maintenance},
    assert(call(Pid, get, 1000) =:= {maintenance, 2}),
    Pid ! stop,
    marker(state_machines).

event_handler(Parent, Name, CrashOn) ->
    receive
        {event, Event} when Event =:= CrashOn -> exit(handler_crash);
        {event, Event} -> Parent ! {handled, Name, Event}, event_handler(Parent, Name, CrashOn);
        stop -> ok
    end.

event_manager(Handlers) ->
    receive
        {notify, Event} ->
            [Pid ! {event, Event} || Pid <- Handlers],
            event_manager(Handlers);
        stop -> [Pid ! stop || Pid <- Handlers], ok
    end.

pattern_25_event_broadcasting() ->
    Parent = self(),
    A = spawn(fun() -> event_handler(Parent, a, boom) end),
    B = spawn(fun() -> event_handler(Parent, b, never) end),
    Manager = spawn(fun() -> event_manager([A, B]) end),
    Manager ! {notify, hello},
    receive {handled, a, hello} -> ok after 1000 -> erlang:error(handler_a_missing) end,
    receive {handled, b, hello} -> ok after 1000 -> erlang:error(handler_b_missing) end,
    Manager ! {notify, boom},
    receive {handled, b, boom} -> ok after 1000 -> erlang:error(healthy_handler_missing) end,
    Manager ! stop,
    marker(event_broadcasting).

pattern_26_timed_messages() ->
    _Timer = erlang:send_after(10, self(), heartbeat),
    receive heartbeat -> ok after 1000 -> erlang:error(timer_failed) end,
    marker(timed_messages).

fan_task(Parent, Name, Result) ->
    case Result of
        fail -> exit({task_failed, Name});
        Value -> Parent ! {task_ok, Name, Value}
    end.

pattern_27_fan_out_fail_fast() ->
    Parent = self(),
    Slow = spawn(fun() -> receive cancel -> ok after 1000 -> Parent ! {task_ok, slow, done} end end),
    Failing = spawn(fun() -> fan_task(Parent, failing, fail) end),
    Ref = monitor(process, Failing),
    receive {'DOWN', Ref, process, Failing, {task_failed, failing}} -> Slow ! cancel
    after 1000 -> erlang:error(fail_fast_missing) end,
    marker(fan_out_fail_fast).

pattern_28_process_introspection() ->
    Pid = spawn(fun() -> counter_loop(7) end),
    Ref = make_ref(),
    Pid ! {sys, self(), Ref, state},
    receive {sys_reply, Ref, 7} -> ok after 1000 -> erlang:error(sys_state_missing) end,
    Pid ! stop,
    marker(process_introspection).

app_root(Parent) ->
    Worker = spawn(fun() -> counter_loop(0) end),
    Bus = spawn(fun() -> event_manager([]) end),
    Parent ! {application_started, Worker, Bus},
    receive stop -> Worker ! stop, Bus ! stop, ok end.

pattern_29_assemble_application() ->
    Parent = self(),
    Root = spawn(fun() -> app_root(Parent) end),
    receive {application_started, Worker, _Bus} ->
        assert(call(Worker, get, 1000) =:= 0),
        Root ! stop
    after 1000 -> erlang:error(application_start_failed) end,
    marker(assemble_application).

pattern_30_test_boundary() ->
    Parent = self(),
    Sup = spawn(fun() -> one_for_one_sup(Parent) end),
    receive {child, Child} ->
        Child ! increment,
        assert(call(Child, get, 1000) =:= 1),
        Child ! crash,
        receive {restarted, Child, Next} ->
            Next ! increment,
            assert(call(Next, get, 1000) =:= 1),
            Sup ! stop
        after 1000 -> erlang:error(boundary_restart_failed) end
    after 1000 -> erlang:error(boundary_child_missing) end,
    marker(test_the_boundary).
