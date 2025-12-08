-module(hook_process).
-export([
    start/0,
    hook_process/3,
    init_ets/0
]).

%% Hook Process - Process-Hook Fusion
%%
%% **Innovative Paradigm**: Processes can BE hooks, not just execute them.
%% A process that IS a hook enables supervision, monitoring, and fault tolerance.
%%
%% **Pattern**: Hook execution becomes a process message, enabling Erlang's
%% supervision model to manage hook lifecycle.

%% Initialize ETS tables
init_ets() ->
    %% Hook process registry: hook_name -> pid
    ets:new(hook_process_registry, [named_table, set, public]),
    ok.

%% Start a hook process
%% Name: hook name
%% Trigger: hook trigger
%% ValidateFun: validation function
start() ->
    io:format("Hook Process: Initializing process-hook fusion~n"),
    init_ets(),
    io:format("Hook Process: Processes can now BE hooks~n"),
    {ok, hook_process}.

%% Spawn a process that IS a hook
%% Name: hook name
%% Trigger: hook trigger
%% ValidateFun: validation function (fun(Data) -> boolean() end)
spawn_hook_process(Name, Trigger, ValidateFun) when is_function(ValidateFun, 1) ->
    %% Check if hook process already exists
    case ets:lookup(hook_process_registry, Name) of
        [] ->
            %% Spawn hook process
            Pid = spawn(?MODULE, hook_process, [Name, Trigger, ValidateFun]),
            
            %% Register hook process
            ets:insert(hook_process_registry, {Name, Pid}),
            
            %% Register hook in hook_primitives (so it can be executed)
            HookDef = hook_primitives:define(Name, Trigger, ValidateFun),
            hook_primitives:register(HookDef),
            
            {ok, Pid};
        [{Name, ExistingPid}] ->
            {error, {hook_process_exists, ExistingPid}}
    end.

%% Hook process loop - process that IS a hook
%% Name: hook name
%% Trigger: hook trigger
%% ValidateFun: validation function
hook_process(Name, Trigger, ValidateFun) ->
    receive
        %% Execute hook validation
        {execute_hook, Data, From} ->
            Result = ValidateFun(Data),
            From ! {hook_result, Name, Result},
            hook_process(Name, Trigger, ValidateFun);
        
        %% Get hook info
        {get_info, From} ->
            From ! {hook_info, Name, Trigger},
            hook_process(Name, Trigger, ValidateFun);
        
        %% Shutdown hook process
        shutdown ->
            io:format("Hook Process: Shutting down hook ~s~n", [Name]),
            ets:delete(hook_process_registry, Name),
            exit(normal);
        
        %% Unknown message
        Unknown ->
            io:format("Hook Process: Unknown message: ~p~n", [Unknown]),
            hook_process(Name, Trigger, ValidateFun)
    end.

%% Execute a hook via process message
%% Name: hook name
%% Data: data to validate
execute_hook(Name, Data) ->
    case ets:lookup(hook_process_registry, Name) of
        [] ->
            {error, {hook_not_found, Name}};
        [{Name, Pid}] ->
            %% Send execute message to hook process
            Pid ! {execute_hook, Data, self()},
            
            %% Wait for result
            receive
                {hook_result, Name, Result} ->
                    case Result of
                        true -> {valid, Data};
                        false -> {invalid, validation_failed};
                        Other -> {invalid, Other}
                    end
            after
                5000 ->
                    %% Poka-yoke: Timeout error handling
                    io:format("Hook Process: Timeout waiting for hook ~s result~n", [Name]),
                    {error, {timeout, Name}}
            end
    end.

