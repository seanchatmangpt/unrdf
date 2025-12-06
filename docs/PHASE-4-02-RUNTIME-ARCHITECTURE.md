# Phase 4.2: Runtime Architecture - Core/Environment Split

## Overview

The runtime is split into three layers to enable both OTP and AtomVM execution:

```
┌─────────────────────────────────────────────┐
│         APPLICATION CODE                     │
│  (Both runtime agnostic)                     │
└──────────────────┬──────────────────────────┘
                   │
┌──────────────────▼──────────────────────────┐
│      PROTOCOL LAYER (Identical)              │
│  - Message encoding/decoding                 │
│  - Session tracking                          │
│  - Error marshaling                          │
└──────────────────┬──────────────────────────┘
                   │
┌──────────────────▼──────────────────────────┐
│       DOMAIN LAYER (Mostly Shared)           │
│  - Business logic (gen_server, etc.)         │
│  - Pure Erlang state machines                │
│  - RDF query processing                      │
│  - Validation logic                          │
│  (AtomVM-compatible subset)                  │
└──────────────────┬──────────────────────────┘
                   │
┌──────────────────▼──────────────────────────┐
│    ENVIRONMENT LAYER (Swapped Per Target)    │
│                                              │
│  ┌─────────────────┐  ┌──────────────────┐ │
│  │ For OTP Node    │  │ For AtomVM WASM  │ │
│  ├─────────────────┤  ├──────────────────┤ │
│  │ gen_tcp         │  │ WebSocket API    │ │
│  │ cowboy          │  │ In-memory maps   │ │
│  │ ets             │  │ Browser storage  │ │
│  │ ssl             │  │ No distribution  │ │
│  │ mnesia          │  │ Stubs            │ │
│  │ File I/O        │  │                  │ │
│  │ NIFs            │  │                  │ │
│  └─────────────────┘  └──────────────────┘ │
└─────────────────────────────────────────────┘
```

---

## Layer 1: Protocol Layer

### Location
```
lib/unrdf_protocol/
├── src/
│   ├── protocol_message.erl        # Message encoding/decoding
│   ├── protocol_session.erl        # Request/response tracking
│   ├── protocol_error.erl          # Error marshaling
│   └── protocol_version.erl        # Version negotiation
├── include/
│   └── protocol.hrl                # Shared definitions
└── test/
    └── protocol_SUITE.erl          # Tests (run on both runtimes)
```

### Key Functions (Identical on Both Runtimes)

```erlang
% Message encoding
encode_message(#{
    type := command,
    id := Id,
    payload := Payload
}) ->
    Maps = maps:with([type, id, timestamp, version, payload, meta], Message),
    jsx:encode(Maps).

% Error encoding
encode_error(ErrorCode, Message) ->
    {
        <<"type">>, <<"error">>,
        <<"payload">>, #{
            <<"error_code">> => ErrorCode,
            <<"message">> => Message
        },
        <<"meta">>, #{
            <<"timestamp">> => iso8601_now(),
            <<"retryable">> => is_retryable(ErrorCode)
        }
    }.

% Session tracking
-record(session, {
    id :: binary(),
    request_map :: #{integer() => request()},
    subscriptions :: #{binary() => subscription()}
}).
```

### Invariant

**The protocol layer must produce identical output for identical input on both OTP and AtomVM.**

Test this with golden test vectors.

---

## Layer 2: Domain Layer

### Principle

Write pure Erlang, no external dependencies. Use only features supported by AtomVM.

### Supported (Both Runtimes)
```erlang
% ✓ Pure Erlang
% ✓ gen_server / gen_statem
% ✓ supervisor trees
% ✓ ETS (AtomVM supports basic operations)
% ✓ Maps, lists, binaries
% ✓ Pattern matching
% ✓ Recursion
% ✓ error_handler / exception handling

-module(rdf_store).
-behavior(gen_server).

init([]) ->
    {ok, #{
        graphs => maps:new(),
        indices => maps:new()
    }}.

handle_call({query, Query}, From, State) ->
    Result = execute_query(Query, State),
    {reply, Result, State}.
```

### Unsupported (OTP Only, Stub for AtomVM)

```erlang
% ✗ NIFs
% ✗ File I/O (mnesia, disc_copy)
% ✗ Distribution (net_kernel, rpc)
% ✗ Some ets operations (AtomVM has limited support)
% ✗ ssl/crypto (limited in AtomVM)
% ✗ External ports

% Solution: Conditional compilation
-ifdef(ATOMVM).
% AtomVM stub - in-memory only
-else.
% Full OTP implementation
-endif.
```

### Location

```
lib/unrdf_core/
├── src/
│   ├── rdf_store.erl               # Gen_server for store
│   ├── rdf_query.erl               # Pure query logic
│   ├── rdf_validation.erl          # Pure validation
│   ├── rdf_graph.erl               # Graph operations
│   └── rdf_supervisor.erl          # Supervisor tree
├── include/
│   └── rdf_types.hrl               # Shared types
└── test/
    └── rdf_core_SUITE.erl          # Tests (both runtimes)
```

### Dependency Injection

Environment-specific logic is injected via module parameters:

```erlang
% Caller specifies storage backend
start_link(StorageBackend) ->
    gen_server:start_link(
        ?MODULE,
        {StorageBackend},
        []
    ).

init({StorageBackend}) ->
    {ok, #{
        storage => StorageBackend,
        cache => storage:new_cache(StorageBackend)
    }}.

% Later, use the injected backend
handle_call(get, #{storage := Backend}, State) ->
    Value = Backend:get(key),  % Calls appropriate impl
    {reply, Value, State}.
```

---

## Layer 3: Environment Layer

### 3a. OTP Runtime Implementation

```
lib/unrdf_otp/
├── src/
│   ├── otp_storage.erl             # gen_tcp + file storage
│   ├── otp_cowboy.erl              # HTTP/WebSocket handler
│   ├── otp_cache.erl               # ETS-backed cache
│   ├── otp_distribution.erl        # Cluster support
│   └── otp_app.erl                 # Application entry point
└── include/
    └── otp_config.hrl              # OTP-specific config
```

Example: Storage backend for OTP

```erlang
% lib/unrdf_otp/src/otp_storage.erl
-module(otp_storage).

new_cache(_Config) ->
    ets:new(cache, [set, public, named_table]).

get(Key) ->
    case ets:lookup(cache, Key) of
        [{Key, Value}] -> {ok, Value};
        [] -> {error, not_found}
    end.

set(Key, Value) ->
    ets:insert(cache, {Key, Value}),
    ok.
```

### 3b. AtomVM Runtime Implementation

```
lib/unrdf_atomvm/
├── src/
│   ├── atomvm_storage.erl          # In-memory storage only
│   ├── atomvm_websocket.erl        # Browser WebSocket
│   ├── atomvm_cache.erl            # Map-based cache
│   ├── atomvm_stubs.erl            # Distribution stubs
│   └── atomvm_app.erl              # Application entry point
└── include/
    └── atomvm_config.hrl           # AtomVM-specific config
```

Example: Storage backend for AtomVM

```erlang
% lib/unrdf_atomvm/src/atomvm_storage.erl
-module(atomvm_storage).

% State kept in process dictionary or passed through calls
new_cache(_Config) ->
    maps:new().  % Returns a map instead of ETS reference

get(Key) ->
    % For AtomVM, we'd need to track cache in process state
    % This is handled by caller (gen_server state)
    ok.

set(Key, Value) ->
    % Also handled by caller
    ok.
```

### 3c. Application Selection

Choose which implementation to load based on runtime:

```erlang
% Top-level rebar.config
{erl_opts, [
    {d, 'RUNTIME_TARGET', 'otp'}  % or 'atomvm'
]}.

% Then conditionally:
-ifdef(ATOMVM).
  -include_lib("unrdf_atomvm/include/atomvm_config.hrl").
  init_storage() -> atomvm_storage:new_cache([]).
-else.
  -include_lib("unrdf_otp/include/otp_config.hrl").
  init_storage() -> otp_storage:new_cache([]).
-endif.
```

---

## Build Targets

### For OTP Node

```bash
# Standard rebar3 build
rebar3 compile

# Creates release
rebar3 release

# Run node
_build/default/rel/unrdf_otp/bin/unrdf_otp console
```

### For AtomVM WASM

```bash
# Compile Erlang to BEAM bytecode (as usual)
rebar3 compile

# Convert BEAM to AtomVM format
avm_compiler unrdf_core.avm lib/unrdf_core/ebin/*.beam

# Bundle with JavaScript
rebar3 atomvm:package
```

---

## Dependency Matrix

Which layer can depend on which:

```
Protocol    ✓ Can use Protocol       ✗ Cannot use Domain/Env
Domain      ✓ Can use Protocol       ✓ Can use Domain     ✗ Cannot use Env
Env(OTP)    ✓ Can use Protocol       ✓ Can use Domain     ✓ Can use OTP
Env(ATOM)   ✓ Can use Protocol       ✓ Can use Domain     ✓ Can use AtomVM
App         ✓ Can use any layer
```

If Domain code needs something from Environment layer, inject it via dependency injection.

---

## Configuration Management

### Environment Variables

```erlang
% Both runtimes read same config format
{runtime, Runtime} = application:get_env(unrdf, runtime),
case Runtime of
    otp ->
        % Load OTP modules
        application:start(unrdf_otp);
    atomvm ->
        % Load AtomVM modules
        application:start(unrdf_atomvm)
end.
```

### Feature Flags

```erlang
-ifdef(CLUSTERING_ENABLED).
% Full distribution features (OTP only)
-else.
% Stub implementations (works in both)
-endif.
```

---

## Code Organization

### Directory Structure

```
unrdf_project/
├── rebar.config                    # Build config (both targets)
├── lib/
│   ├── unrdf_protocol/             # Shared: message protocol
│   ├── unrdf_core/                 # Shared: business logic
│   ├── unrdf_otp/                  # OTP-specific: gen_tcp, cowboy, ets
│   ├── unrdf_atomvm/               # AtomVM-specific: WebSocket stubs
│   └── unrdf_app/                  # Top-level application
├── test/
│   ├── protocol_SUITE.erl          # Tests (run on both)
│   ├── core_SUITE.erl              # Tests (run on both)
│   ├── integration_SUITE.erl       # Golden tests (both runtimes)
│   └── otp_SUITE.erl               # OTP-only tests
├── wasm/                           # WASM packaging for AtomVM
│   └── build_atomvm.sh
└── docs/
    └── runtime-architecture.md     # This file
```

### Module Naming

```
unrdf_protocol_*         % Shared protocol layer
unrdf_core_*             % Shared domain layer
unrdf_otp_*              % OTP-specific environment
unrdf_atomvm_*           % AtomVM-specific environment
```

---

## Testing Both Runtimes

### Shared Tests (Run on Both)

```bash
# Test protocol layer (both should pass)
rebar3 eunit tests=unrdf_protocol_SUITE

# Test domain logic (both should pass)
rebar3 eunit tests=unrdf_core_SUITE

# Integration tests (both should pass)
rebar3 eunit tests=integration_SUITE
```

### Runtime-Specific Tests

```bash
# OTP-only features
rebar3 eunit tests=otp_SUITE   # Only runs on OTP build

# AtomVM-only features
rebar3 atomvm:test             # Only runs on AtomVM build
```

---

## Deployment

### OTP Deployment

```bash
# Build release
rebar3 release

# Deploy standard Erlang release
tar xzf unrdf_otp.tar.gz -C /opt/
/opt/unrdf_otp/bin/unrdf_otp start
```

### Browser Deployment

```bash
# Build WASM
rebar3 atomvm:package

# Include in web app
cp atomvm_bundle.avm public/
# JavaScript loads and runs it
```

---

## See Also

- **04-DUAL-ADAPTERS.md** - How JS adapters call into this layer
- **05-SETUP-ERLANG-SIDE.md** - Detailed setup instructions
- **07-INTEGRATION-TESTING.md** - Testing both runtimes
