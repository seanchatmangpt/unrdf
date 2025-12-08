-module(process_supervisor).
-export([
    start_supervisor/2,
    start_child/2,
    terminate_supervisor/1,
    supervisor_loop/3
]).

%% Process Supervisor - Erlang State Management
%%
%% **Architecture**: Supervision state stored in Erlang
%% JavaScript provides only callbacks via bridge commands
%%
%% **Poka-Yoke**: State validation in Erlang prevents invalid operations

%% Start supervisor
%% Name: supervisor name
%% Strategy: restart strategy (one_for_one, one_for_all, rest_for_one)
start_supervisor(Name, Strategy) ->
    Pid = spawn(?MODULE, supervisor_loop, [Name, Strategy, #{}]),
    {ok, Pid}.

%% Start child process under supervisor
%% SupervisorPid: Supervisor process PID
%% ChildSpec: Child specification {name, callbackId, options}
start_child(SupervisorPid, ChildSpec) ->
    SupervisorPid ! {start_child, ChildSpec},
    receive
        {child_started, ChildPid} ->
            {ok, ChildPid};
        {child_error, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

%% Terminate supervisor and all children
terminate_supervisor(SupervisorPid) ->
    SupervisorPid ! terminate,
    ok.

%% Supervisor main loop
supervisor_loop(Name, Strategy, Children) ->
    receive
        {start_child, ChildSpec} ->
            {name, ChildName, callbackId, Options} = ChildSpec,
            
            %% Spawn child process using process_framework
            case process_framework:spawn_process(ChildName, callbackId, Options) of
                {ok, ChildPid} ->
                    %% Monitor child
                    process_framework:monitor_process(ChildPid, self()),
                    
                    %% Track child
                    NewChildren = maps:put(ChildName, {ChildPid, ChildSpec, 0}, Children),
                    
                    %% Send confirmation
                    self() ! {child_started, ChildPid},
                    
                    supervisor_loop(Name, Strategy, NewChildren);
                {error, Reason} ->
                    self() ! {child_error, Reason},
                    supervisor_loop(Name, Strategy, Children)
            end;
        
        {down, ChildPid, Reason} ->
            %% Child process exited, handle restart
            handle_child_exit(ChildPid, Reason, Name, Strategy, Children);
        
        terminate ->
            %% Terminate all children
            maps:fold(fun(_Name, {Pid, _Spec, _RestartCount}, _) ->
                process_framework:exit_process(Pid, shutdown)
            end, ok, Children),
            ok;
        
        _ ->
            supervisor_loop(Name, Strategy, Children)
    end.

%% Handle child exit
handle_child_exit(ChildPid, Reason, Name, Strategy, Children) ->
    %% Find child by PID
    {ChildName, ChildSpec, RestartCount} = find_child_by_pid(ChildPid, Children),
    
    if
        RestartCount >= 5 ->
            %% Max restarts exceeded, don't restart
            NewChildren = maps:remove(ChildName, Children),
            supervisor_loop(Name, Strategy, NewChildren);
        true ->
            %% Restart child based on strategy
            NewRestartCount = RestartCount + 1,
            NewChildren = maps:put(ChildName, {ChildPid, ChildSpec, NewRestartCount}, Children),
            
            case Strategy of
                one_for_one ->
                    %% Restart only this child
                    restart_child(ChildName, ChildSpec, Name, Strategy, NewChildren);
                one_for_all ->
                    %% Restart all children
                    restart_all_children(Name, Strategy, NewChildren);
                rest_for_one ->
                    %% Restart this child and all children started after it
                    restart_rest_children(ChildName, Name, Strategy, NewChildren)
            end
    end.

%% Find child by PID
find_child_by_pid(ChildPid, Children) ->
    maps:fold(fun(Name, {Pid, Spec, RestartCount}, Acc) ->
        if
            Pid =:= ChildPid ->
                {Name, Spec, RestartCount};
            true ->
                Acc
        end
    end, {undefined, undefined, 0}, Children).

%% Restart single child
restart_child(ChildName, ChildSpec, Name, Strategy, Children) ->
    {name, ChildName, callbackId, Options} = ChildSpec,
    
    case process_framework:spawn_process(ChildName, callbackId, Options) of
        {ok, NewChildPid} ->
            process_framework:monitor_process(NewChildPid, self()),
            NewChildren = maps:put(ChildName, {NewChildPid, ChildSpec, 0}, Children),
            supervisor_loop(Name, Strategy, NewChildren);
        {error, Reason} ->
            %% Restart failed, try again later
            timer:sleep(1000),
            restart_child(ChildName, ChildSpec, Name, Strategy, Children)
    end.

%% Restart all children
restart_all_children(Name, Strategy, Children) ->
    maps:fold(fun(ChildName, {_Pid, ChildSpec, _RestartCount}, Acc) ->
        restart_child(ChildName, ChildSpec, Name, Strategy, Acc)
    end, Children, Children).

%% Restart rest of children (this child and all after it)
restart_rest_children(ChildName, Name, Strategy, Children) ->
    %% Get list of child names
    ChildNames = maps:keys(Children),
    
    %% Find index of failed child
    Index = find_index(ChildName, ChildNames),
    
    %% Restart this child and all after it
    lists:foldl(fun(ChildNameToRestart, Acc) ->
        case maps:get(ChildNameToRestart, Acc, undefined) of
            {_Pid, ChildSpec, _RestartCount} ->
                restart_child(ChildNameToRestart, ChildSpec, Name, Strategy, Acc);
            _ ->
                Acc
        end
    end, Children, lists:nthtail(Index, ChildNames)).

%% Find index of element in list
find_index(Element, List) ->
    find_index(Element, List, 0).

find_index(Element, [Element | _], Index) ->
    Index;
find_index(Element, [_ | Tail], Index) ->
    find_index(Element, Tail, Index + 1);
find_index(_, [], _) ->
    -1.

