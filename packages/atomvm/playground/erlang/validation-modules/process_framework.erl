-module(process_framework).
-export([
    spawn_process/3,
    send_message/2,
    link_processes/2,
    monitor_process/2,
    exit_process/2,
    process_loop/4,
    init_ets/0
]).

%% Process Framework - Erlang State Management
%%
%% **Architecture**: All process state (mailbox, links, monitors) stored in Erlang
%% JavaScript provides only callbacks via bridge commands
%%
%% **Poka-Yoke**: State validation in Erlang prevents invalid operations

%% Initialize ETS tables for state storage
init_ets() ->
    %% Process registry: name -> pid
    ets:new(process_registry, [named_table, set, public]),
    
    %% Process mailboxes: pid -> queue
    ets:new(process_mailboxes, [named_table, set, public]),
    
    %% Process links: pid -> set of linked pids
    ets:new(process_links, [named_table, set, public]),
    
    %% Process monitors: pid -> set of {ref, monitor_pid}
    ets:new(process_monitors, [named_table, set, public]),
    
    %% Process monitored_by: pid -> set of monitoring pids
    ets:new(process_monitored_by, [named_table, set, public]),
    
    %% Process states: pid -> state (initialized, running, waiting, terminated, error)
    ets:new(process_states, [named_table, set, public]),
    
    %% Callback registry: callbackId -> pid (for routing callback results)
    ets:new(callback_registry, [named_table, set, public]),
    
    ok.

%% Spawn a new process
%% Name: process name (must be unique)
%% CallbackId: JavaScript callback ID for init/handle functions
%% Options: process options (mailbox_max_size, etc.)
spawn_process(Name, CallbackId, Options) ->
    %% Poka-yoke: Check if name already exists
    case ets:lookup(process_registry, Name) of
        [] ->
            %% Create process
            Pid = spawn(?MODULE, process_loop, [Name, CallbackId, Options, initialized]),
            
            %% Register process
            ets:insert(process_registry, {Name, Pid}),
            
            %% Initialize mailbox
            MailboxMaxSize = proplists:get_value(mailbox_max_size, Options, 1000),
            ets:insert(process_mailboxes, {Pid, queue:new()}),
            
            %% Initialize links and monitors
            ets:insert(process_links, {Pid, sets:new()}),
            ets:insert(process_monitors, {Pid, sets:new()}),
            ets:insert(process_monitored_by, {Pid, sets:new()}),
            ets:insert(process_states, {Pid, initialized}),
            
            %% Send message to process loop to start initialization
            Pid ! {start_init, CallbackId},
            {ok, Pid};
        [_] ->
            {error, {name_exists, Name}}
    end.

%% Send message to process by name
send_message(Name, Message) ->
    case ets:lookup(process_registry, Name) of
        [{Name, Pid}] ->
            %% Poka-yoke: Check if process is alive
            case ets:lookup(process_states, Pid) of
                [{Pid, State}] when State =/= terminated, State =/= error ->
                    %% Get mailbox
                    [{Pid, Mailbox}] = ets:lookup(process_mailboxes, Pid),
                    MailboxMaxSize = 1000, %% TODO: Get from options
                    
                    %% Check mailbox size
                    case queue:len(Mailbox) >= MailboxMaxSize of
                        true ->
                            {error, mailbox_overflow};
                        false ->
                            %% Add message to mailbox
                            NewMailbox = queue:in(Message, Mailbox),
                            ets:insert(process_mailboxes, {Pid, NewMailbox}),
                            
                            %% Wake up process if waiting
                            Pid ! {message, Message},
                            ok
                    end;
                _ ->
                    {error, process_dead}
            end;
        [] ->
            {error, {process_not_found, Name}}
    end.

%% Link two processes
link_processes(Pid1, Pid2) ->
    %% Poka-yoke: Check both processes are alive
    case {ets:lookup(process_states, Pid1), ets:lookup(process_states, Pid2)} of
        {[{Pid1, State1}], [{Pid2, State2}]} when State1 =/= terminated, State1 =/= error,
                                                     State2 =/= terminated, State2 =/= error ->
            %% Get links for both processes
            [{Pid1, Links1}] = ets:lookup(process_links, Pid1),
            [{Pid2, Links2}] = ets:lookup(process_links, Pid2),
            
            %% Add bidirectional link
            NewLinks1 = sets:add_element(Pid2, Links1),
            NewLinks2 = sets:add_element(Pid1, Links2),
            
            ets:insert(process_links, {Pid1, NewLinks1}),
            ets:insert(process_links, {Pid2, NewLinks2}),
            
            ok;
        _ ->
            {error, process_dead}
    end.

%% Monitor a process
monitor_process(Pid, MonitorPid) ->
    %% Poka-yoke: Check target process is alive
    case ets:lookup(process_states, Pid) of
        [{Pid, State}] when State =/= terminated, State =/= error ->
            %% Create monitor reference
            Ref = make_ref(),
            
            %% Add to monitors
            [{MonitorPid, Monitors}] = ets:lookup(process_monitors, MonitorPid),
            NewMonitors = sets:add_element({Ref, Pid}, Monitors),
            ets:insert(process_monitors, {MonitorPid, NewMonitors}),
            
            %% Add to monitored_by
            [{Pid, MonitoredBy}] = ets:lookup(process_monitored_by, Pid),
            NewMonitoredBy = sets:add_element(MonitorPid, MonitoredBy),
            ets:insert(process_monitored_by, {Pid, NewMonitoredBy}),
            
            {ok, Ref};
        _ ->
            {error, process_dead}
    end.

%% Exit a process
exit_process(Pid, Reason) ->
    %% Update state
    ets:insert(process_states, {Pid, terminated}),
    
    %% Propagate exit to linked processes
    case ets:lookup(process_links, Pid) of
        [{Pid, Links}] ->
            sets:fold(fun(LinkedPid, _) ->
                exit_process(LinkedPid, Reason)
            end, ok, Links);
        [] ->
            ok
    end,
    
    %% Notify monitoring processes
    case ets:lookup(process_monitored_by, Pid) of
        [{Pid, MonitoredBy}] ->
            sets:fold(fun(MonitorPid, _) ->
                MonitorPid ! {down, Pid, Reason}
            end, ok, MonitoredBy);
        [] ->
            ok
    end,
    
    ok.

%% Process main loop
process_loop(Name, CallbackId, Options, State) ->
    receive
        {start, InitResult} ->
            %% Process started, enter message loop
            process_message_loop(Name, CallbackId, Options, InitResult);
        {message, Message} ->
            %% Message received, process it
            process_message_loop(Name, CallbackId, Options, undefined);
        {exit, Reason} ->
            %% Exit requested
            exit_process(self(), Reason),
            ok;
        _ ->
            %% Unknown message, continue
            process_loop(Name, CallbackId, Options, State)
    end.

%% Message processing loop
process_message_loop(Name, CallbackId, Options, State) ->
    %% Update state to running
    ets:insert(process_states, {self(), running}),
    
    %% Get mailbox
    [{self(), Mailbox}] = ets:lookup(process_mailboxes, self()),
    
    case queue:out(Mailbox) of
        {{value, Message}, NewMailbox} ->
            %% Update mailbox
            ets:insert(process_mailboxes, {self(), NewMailbox}),
            
            %% Invoke JavaScript message handler callback
            case invoke_js_callback(handle, CallbackId, [Message]) of
                {ok, HandleResult} ->
                    %% Continue processing
                    process_message_loop(Name, CallbackId, Options, HandleResult);
                {error, Reason} ->
                    %% Error in handler, exit process
                    ets:insert(process_states, {self(), error}),
                    exit_process(self(), Reason),
                    ok
            end;
        {empty, _} ->
            %% No messages, wait
            ets:insert(process_states, {self(), waiting}),
            receive
                {message, NewMessage} ->
                    %% New message arrived, add to mailbox and process
                    NewMailbox = queue:in(NewMessage, Mailbox),
                    ets:insert(process_mailboxes, {self(), NewMailbox}),
                    process_message_loop(Name, CallbackId, Options, State);
                {exit, Reason} ->
                    exit_process(self(), Reason),
                    ok
            after 1000 ->
                %% Timeout, check mailbox again
                process_message_loop(Name, CallbackId, Options, State)
            end
    end.

%% Invoke JavaScript callback via bridge
%% Type: init | handle
%% CallbackId: JavaScript callback ID
%% Args: Arguments to pass to callback
invoke_js_callback(Type, CallbackId, Args) ->
    %% Send bridge command to invoke JavaScript callback
    case Type of
        init ->
            io:format("JS_CALLBACK:init:~p~n", [CallbackId]);
        handle ->
            %% Serialize message for bridge
            MessageStr = encode_message(Args),
            io:format("JS_CALLBACK:handle:~p:~s~n", [CallbackId, MessageStr])
    end,
    
    %% Wait for result (bridge will send result back via message)
    receive
        {js_callback_result, CallbackId, Result} ->
            {ok, Result}
    after 5000 ->
        {error, timeout}
    end.

%% Encode message for bridge (simple term_to_binary encoding)
encode_message([Message]) ->
    %% Simple encoding: convert to string representation
    lists:flatten(io_lib:format("~p", [Message])).

%% Parse result from JavaScript (simple parsing)
parse_result(ResultStr) ->
    %% Try to parse as term, fallback to binary
    try
        {ok, Tokens, _} = erl_scan:string(ResultStr),
        {ok, Term} = erl_parse:parse_term(Tokens),
        Term
    catch
        _:_ ->
            list_to_binary(ResultStr)
    end.

