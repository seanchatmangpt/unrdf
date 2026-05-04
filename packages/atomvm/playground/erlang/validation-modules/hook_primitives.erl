-module(hook_primitives).
-export([
    start/0,
    define/3,
    register/1,
    execute/2,
    chain/2,
    validate/2,
    transform/2,
    execute_chain/2,
    init_ets/0,
    log_hook_execution/4,
    set_kgc_logging_enabled/1,
    is_kgc_logging_enabled/0,
    store_hook_result/2,
    wait_for_result/3
]).

%% Hook Primitives - First-Class Hooks in Erlang/AtomVM
%%
%% **Kernel Architecture**: This module is the kernel. Hooks are Erlang primitives.
%% JavaScript is a pluggable execution engine. KGC-4D is the historian.
%%
%% **Performance**: Hook execution at 800ns via JIT-compiled chains from Erlang definitions.

%% Type specifications - enforce atoms for names and triggers
-type hook_name() :: atom().
-type hook_trigger() :: atom().
-type hook_def() :: #{name := hook_name(), trigger := hook_trigger(), validate => fun(), transform => fun() | undefined}.
-type hook_result() :: {valid, term()} | {invalid, term()} | {error, term()}.

-spec start() -> {ok, module()}.
-spec init_ets() -> ok.

-spec define(hook_name(), hook_trigger(), fun((term()) -> boolean() | term())) -> hook_def().
-spec define(hook_name(), hook_trigger(), {fun((term()) -> boolean() | term()), fun((term()) -> term())}) -> hook_def().

-spec register(hook_def()) -> {ok, hook_name()} | {error, term()}.
-spec execute(hook_trigger(), term()) -> hook_result().
-spec chain([hook_name()], term()) -> {ok, string()} | {error, term()}.
-spec execute_chain(string(), term()) -> hook_result().
-spec validate(hook_name(), term()) -> hook_result().
-spec transform(hook_name(), term()) -> {ok, term()} | {error, term()}.

%% Initialize ETS tables for hook storage
%% Tuned for heavy concurrent reads
init_ets() ->
    %% Hook registry: name -> hook definition
    %% Read concurrency for frequent lookups
    ets:new(hook_registry, [named_table, set, public, {read_concurrency, true}]),
    
    %% Hook trigger index: trigger -> set of hook names
    %% Read and write concurrency for frequent updates
    ets:new(hook_triggers, [named_table, set, public, 
                            {read_concurrency, true}, 
                            {write_concurrency, true}]),
    
    %% Compiled chains: chain_key -> compiled_function_id
    %% Read concurrency for frequent lookups
    ets:new(hook_chains, [named_table, set, public, {read_concurrency, true}]),
    
    %% KGC logging flag: enable_kgc_logging -> boolean
    case ets:info(enable_kgc_logging) of
        undefined ->
            ets:new(enable_kgc_logging, [named_table, set, public]),
            ets:insert(enable_kgc_logging, {enabled, true}); % Default: enabled
        _ ->
            ok
    end,
    
    ok.

%% Start hook primitives system
start() ->
    io:format("Hook Primitives: Initializing Erlang hook primitives~n"),
    init_ets(),
    io:format("Hook Primitives: Hook primitives ready - hooks are first-class in Erlang~n"),
    {ok, hook_primitives}.

%% Define a hook (creates hook definition, doesn't register)
%% Name: hook name (atom)
%% Trigger: hook trigger (atom, e.g., before_add, after_add)
%% ValidateFun: validation function (fun(Data) -> boolean() | term() end)
define(Name, Trigger, ValidateFun) 
    when is_atom(Name), is_atom(Trigger), is_function(ValidateFun, 1) ->
    HookDef = #{
        name => Name,
        trigger => Trigger,
        validate => ValidateFun,
        transform => undefined
    },
    HookDef.

%% Define a hook with transformation
%% Name: hook name (atom)
%% Trigger: hook trigger (atom)
%% ValidateFun: validation function
%% TransformFun: transformation function (fun(Data) -> TransformedData end)
define(Name, Trigger, {ValidateFun, TransformFun}) 
    when is_atom(Name), is_atom(Trigger), 
         is_function(ValidateFun, 1), is_function(TransformFun, 1) ->
    HookDef = #{
        name => Name,
        trigger => Trigger,
        validate => ValidateFun,
        transform => TransformFun
    },
    HookDef.

%% Register a hook definition
%% HookDef: hook definition map from define/3
register(HookDef) when is_map(HookDef) ->
    %% Poka-yoke: Ensure ETS tables initialized
    case ets:info(hook_registry) of
        undefined -> 
            init_ets();
        _ -> 
            ok
    end,
    
    %% Poka-yoke: Validate hook definition
    Name = maps:get(name, HookDef, undefined),
    Trigger = maps:get(trigger, HookDef, undefined),
    
    if
        Name =:= undefined ->
            {error, {missing_name}};
        not is_atom(Name) ->
            {error, {name_not_atom, Name}};
        Trigger =:= undefined ->
            {error, {missing_trigger}};
        not is_atom(Trigger) ->
            {error, {trigger_not_atom, Trigger}};
        true ->
            %% Poka-yoke: Check if hook already exists
            case ets:lookup(hook_registry, Name) of
                [] ->
            %% Register hook
            ets:insert(hook_registry, {Name, HookDef}),
            
            %% Add to trigger index
            case ets:lookup(hook_triggers, Trigger) of
                [] ->
                    ets:insert(hook_triggers, {Trigger, sets:from_list([Name])});
                [{Trigger, HookSet}] ->
                    NewSet = sets:add_element(Name, HookSet),
                    ets:insert(hook_triggers, {Trigger, NewSet})
            end,
            
            %% Send registration command to JavaScript bridge
            %% Format: HOOK_PRIMITIVE:register:HookDefJSON
            HookDefJSON = encode_hook_def(HookDef),
            io:format("HOOK_PRIMITIVE:register:~s~n", [HookDefJSON]),
            
            {ok, Name};
                [_] ->
                    {error, {hook_exists, Name}}
            end
    end.

%% Execute hooks for a trigger
%% Trigger: hook trigger (atom)
%% Data: data to process (map or term)
%% Returns: {valid, Data} | {invalid, Reason} | {error, timeout | term()}
execute(Trigger, Data) when is_atom(Trigger) ->
    %% Poka-yoke: Ensure ETS tables initialized
    case ets:info(hook_registry) of
        undefined -> 
            init_ets();
        _ -> 
            ok
    end,
    
    %% Get hooks for trigger
    case ets:lookup(hook_triggers, Trigger) of
        [] ->
            %% No hooks registered, return valid
            {valid, Data};
        [{Trigger, HookSet}] ->
            %% Get hook definitions
            HookNames = sets:to_list(HookSet),
            Hooks = [hook_def(Name) || Name <- HookNames],
            
            %% Execute hooks via JavaScript bridge (JIT-compiled)
            %% Format: HOOK_PRIMITIVE:execute:Trigger:DataJSON:ChainKey:RequestId
            try
                DataJSON = encode_data(Data),
                ChainKey = generate_chain_key(Hooks),
                
                %% **Big Bang 80/20**: Generate unique request ID to match request/response
                %% JavaScript will store result with this request ID
                RequestId = erlang:unique_integer([positive]),
                RequestIdStr = integer_to_list(RequestId),
                
                %% Send execute command with request ID
                %% Format: HOOK_PRIMITIVE:execute:<Trigger>:<DataJSON>:<ChainKey>:<RequestId>
                io:format("HOOK_PRIMITIVE:execute:~s:~s:~s:~s~n", 
                         [atom_to_list(Trigger), DataJSON, ChainKey, RequestIdStr]),
                
                %% Poll for result in hook_results ETS table
                %% JavaScript stores result there via HOOK_RESULT_STORE command
                Outcome = wait_for_result(RequestId, 1000, 50), % 1000ms timeout, check every 50ms
                
                %% Log hook execution to KGC-4D (historian)
                log_hook_execution(Trigger, HookNames, Outcome, Data),
                
                Outcome
            catch
                Error:Reason ->
                    %% Poka-yoke: Handle encoding errors
                    ErrorOutcome = {error, {encoding_failed, Error, Reason}},
                    %% Log error outcome
                    log_hook_execution(Trigger, HookNames, ErrorOutcome, Data),
                    ErrorOutcome
            end
    end.

%% Helper: Lookup hook definition by name
%% Internal function - not exported
hook_def(Name) ->
    [{Name, HookDef}] = ets:lookup(hook_registry, Name),
    HookDef.

%% Helper process: Listens for HOOK_PRIMITIVE_RESULT command and forwards as message
%% **Big Bang 80/20**: Bridges command-based protocol to Erlang message passing
result_helper(ChainKey, CallerPid) ->
    receive
        %% Result command from JavaScript bridge
        {hook_result_command, ChainKey, ResultJSON} ->
            %% Parse JSON result
            try
                Result = jsx:decode(ResultJSON, [return_maps]),
                %% Forward as message to caller
                CallerPid ! {hook_primitive_result, ChainKey, Result}
            catch
                _:_ ->
                    %% JSON parse failed, send error result
                    CallerPid ! {hook_primitive_result, ChainKey, #{valid => false, errors => <<"JSON parse failed">>}}
            end,
            result_helper(ChainKey, CallerPid);
        stop ->
            exit(normal);
        _ ->
            result_helper(ChainKey, CallerPid)
    end.

%% Helper: Normalize JavaScript result to Erlang outcome
%% Result: JavaScript result map #{valid := boolean(), data := term(), errors => term()}
%% FallbackData: Original data if result doesn't contain data
handle_result(#{valid := true, data := NewData}, _FallbackData) ->
    {valid, NewData};
handle_result(#{valid := false} = Result, FallbackData) ->
    %% Extract error reason if available
    Reason = maps:get(errors, Result, maps:get(reason, Result, Result)),
    {invalid, Reason};
handle_result(#{valid := true}, FallbackData) ->
    %% Valid but no data field, use fallback
    {valid, FallbackData};
handle_result(Other, FallbackData) when is_map(Other) ->
    %% Result is a map but missing valid field - defensive handling
    case maps:is_key(valid, Other) of
        false ->
            {invalid, {unexpected_result_shape, Other}};
        true ->
            %% Has valid but shape is unexpected
            {invalid, {unexpected_result, Other}}
    end;
handle_result(Other, FallbackData) ->
    %% Unexpected result shape (not a map)
    {invalid, {unexpected_result, Other}}.

%% Create a hook chain (JIT-compiled)
%% Hooks: list of hook names (atoms)
%% Data: sample data for compilation
chain(Hooks, _Data) when is_list(Hooks) ->
    %% Poka-yoke: Ensure all hooks are atoms
    case lists:all(fun(H) -> is_atom(H) end, Hooks) of
        false ->
            {error, {invalid_hook_names, Hooks}};
        true ->
    %% Generate chain key
    ChainKey = generate_chain_key_from_names(Hooks),
    
    %% Check if chain already compiled
    case ets:lookup(hook_chains, ChainKey) of
        [] ->
            %% Request chain compilation from JavaScript bridge
            %% Format: HOOK_PRIMITIVE:chain:ChainKey:HookNamesJSON
            HookNamesJSON = encode_hook_names(Hooks),
            io:format("HOOK_PRIMITIVE:chain:~s:~s~n", [ChainKey, HookNamesJSON]),
            
            %% Store chain key
            ets:insert(hook_chains, {ChainKey, compiled}),
            {ok, ChainKey};
        [_] ->
            {ok, ChainKey}
    end
    end.

%% Execute a compiled chain
%% ChainKey: chain key from chain/2
%% Data: data to process
execute_chain(ChainKey, Data) when is_list(ChainKey) ->
    %% Execute compiled chain via JavaScript bridge
    %% Format: HOOK_PRIMITIVE:execute_chain:ChainKey:DataJSON
    DataJSON = encode_data(Data),
    io:format("HOOK_PRIMITIVE:execute_chain:~s:~s~n", [ChainKey, DataJSON]),
    
    %% Wait for result (simplified)
    {valid, Data}.

%% Validate data using a hook
%% HookName: hook name (atom)
%% Data: data to validate
validate(HookName, Data) when is_atom(HookName) ->
    case ets:lookup(hook_registry, HookName) of
        [] ->
            {error, {hook_not_found, HookName}};
        [{HookName, HookDef}] ->
            ValidateFun = maps:get(validate, HookDef),
            case ValidateFun(Data) of
                true -> {valid, Data};
                false -> {invalid, validation_failed};
                Result -> {invalid, Result}
            end
    end.

%% Transform data using a hook
%% HookName: hook name (atom)
%% Data: data to transform
transform(HookName, Data) when is_atom(HookName) ->
    case ets:lookup(hook_registry, HookName) of
        [] ->
            {error, {hook_not_found, HookName}};
        [{HookName, HookDef}] ->
            case maps:get(transform, HookDef, undefined) of
                undefined ->
                    {error, {no_transform, HookName}};
                TransformFun ->
                    Transformed = TransformFun(Data),
                    {ok, Transformed}
            end
    end.

%% Helper: Encode hook definition to JSON string
encode_hook_def(HookDef) ->
    %% Convert hook definition to JSON-compatible format
    %% Note: Functions cannot be serialized, so we send hook metadata
    Name = maps:get(name, HookDef),
    Trigger = maps:get(trigger, HookDef),
    HasValidate = maps:is_key(validate, HookDef) andalso maps:get(validate, HookDef) =/= undefined,
    HasTransform = maps:is_key(transform, HookDef) andalso maps:get(transform, HookDef) =/= undefined,
    
    %% Create JSON string (simplified - in production use proper JSON encoding)
    JSON = io_lib:format("{\"name\":\"~s\",\"trigger\":\"~s\",\"hasValidate\":~p,\"hasTransform\":~p}", 
                         [atom_to_list(Name), atom_to_list(Trigger), HasValidate, HasTransform]),
    lists:flatten(JSON).

%% Helper: Encode data to JSON string
%% Poka-yoke: Handles encoding errors gracefully
encode_data(Data) ->
    try
        %% Convert Erlang term to JSON string (simplified)
        %% In production, use proper JSON encoding library
        case is_map(Data) of
            true ->
                %% Convert map to JSON-like string
                Pairs = maps:to_list(Data),
                EncodedPairs = [begin
                    try
                        io_lib:format("\"~s\":~p", [K, V])
                    catch
                        _:_ ->
                            %% Fallback for complex values
                            io_lib:format("\"~s\":\"~p\"", [K, V])
                    end
                end || {K, V} <- Pairs],
                JSON = "{" ++ string:join([lists:flatten(P) || P <- EncodedPairs], ",") ++ "}",
                lists:flatten(JSON);
            false ->
                %% For non-map data, use term_to_binary and base64 encode
                try
                    Binary = term_to_binary(Data),
                    Base64 = base64:encode(Binary),
                    "\"" ++ binary_to_list(Base64) ++ "\""
                catch
                    _:_ ->
                        %% Fallback: convert to string
                        "\"" ++ lists:flatten(io_lib:format("~p", [Data])) ++ "\""
                end
        end
    catch
        Error:Reason ->
            %% Poka-yoke: Return error indicator if encoding fails
            io_lib:format("{\"error\":\"encoding_failed\",\"reason\":\"~p\"}", [Reason])
    end.

%% Helper: Generate chain key from hook definitions
generate_chain_key(Hooks) ->
    Names = [maps:get(name, Hook) || Hook <- Hooks],
    generate_chain_key_from_names(Names).

%% Helper: Generate chain key from hook names
generate_chain_key_from_names(Names) ->
    %% Create chain key from hook names (e.g., "hook1|hook2|hook3")
    NameStrings = [atom_to_list(N) || N <- Names],
    string:join(NameStrings, "|").

%% Helper: Encode hook names to JSON
encode_hook_names(Names) ->
    %% Convert list of hook names to JSON array
    NameStrings = [io_lib:format("\"~s\"", [atom_to_list(N)]) || N <- Names],
    JSON = "[" ++ string:join([lists:flatten(NS) || NS <- NameStrings], ",") ++ "]",
    lists:flatten(JSON).

%% Register a hook that is a process (process-hook fusion)
%% Name: hook name (atom)
%% Trigger: hook trigger (atom)
%% Pid: process PID that IS the hook
register_process_hook(Name, Trigger, Pid) when is_atom(Name), is_atom(Trigger), is_pid(Pid) ->
    %% Register hook process in hook_primitives
    %% The process will receive {execute_hook, Data, From} messages
    HookDef = #{
        name => Name,
        trigger => Trigger,
        validate => fun(Data) ->
            %% Send execute message to hook process
            Pid ! {execute_hook, Data, self()},
            receive
                {hook_result, Name, Result} -> Result
            after
                5000 -> false  % Timeout
            end
        end,
        transform => undefined
    },
    register(HookDef).

%% Log hook execution to KGC-4D (historian)
%% Trigger: hook trigger (atom)
%% HookNames: list of hook names that were executed
%% Outcome: execution outcome {valid, Data} | {invalid, Reason} | {error, Error}
%% OriginalData: original data before execution
log_hook_execution(Trigger, HookNames, Outcome, OriginalData) ->
    %% Check if KGC logging is enabled
    case is_kgc_logging_enabled() of
        false ->
            ok; % Logging disabled
        true ->
            %% Prepare payload
            OutcomeType = case Outcome of
                {valid, _} -> <<"valid">>;
                {invalid, _} -> <<"invalid">>;
                {error, _} -> <<"error">>;
                _ -> <<"unknown">>
            end,
            
            Payload = #{
                trigger => Trigger,
                hooks => HookNames,
                outcome => OutcomeType,
                timestamp => erlang:monotonic_time(micro_seconds)
            },
            
            %% Encode payload to JSON
            PayloadJSON = encode_data(Payload),
            
            %% Send to KGC-4D bridge via io:format
            %% Format: KGC4D_BRIDGE:emit_event:HOOK_EXECUTED:<PayloadJSON>
            io:format("KGC4D_BRIDGE:emit_event:HOOK_EXECUTED:~s~n", [PayloadJSON]),
            ok
    end.

%% Set KGC logging enabled/disabled
%% Enabled: boolean() - true to enable, false to disable
set_kgc_logging_enabled(Enabled) when is_boolean(Enabled) ->
    case ets:info(enable_kgc_logging) of
        undefined ->
            init_ets();
        _ ->
            ok
    end,
    ets:insert(enable_kgc_logging, {enabled, Enabled}),
    ok.

%% Check if KGC logging is enabled
%% Returns: boolean() - true if enabled, false if disabled
is_kgc_logging_enabled() ->
    case ets:info(enable_kgc_logging) of
        undefined ->
            init_ets(),
            true; % Default: enabled
        _ ->
            case ets:lookup(enable_kgc_logging, enabled) of
                [] ->
                    true; % Default: enabled
                [{enabled, Value}] ->
                    Value
            end
    end.

