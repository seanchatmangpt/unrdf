# Phase 4.9: Migration Guide - Adopting the Dual-Runtime Pattern

## Overview

If you have an existing UNRDF + Erlang system, follow this guide to adopt the multi-runtime pattern with AtomVM browser simulation.

**Expected effort:** 2-4 weeks for medium-sized systems

---

## Pre-Migration Checklist

### Assess Current System

- [ ] Document current code organization
- [ ] Identify Erlang modules
- [ ] Audit dependencies (what uses external APIs)
- [ ] Count NIFs and OS-specific code
- [ ] Document test coverage

### Identify Problem Areas

**High Priority - Must Refactor**
- [ ] File I/O (move to environment layer)
- [ ] Network code (TCP, distribution)
- [ ] NIFs and C code
- [ ] External system calls

**Medium Priority - Conditional Code**
- [ ] Mnesia usage (stub in AtomVM)
- [ ] gen_cluster functionality
- [ ] SSL/crypto heavy logic

**Low Priority - Will Work As-Is**
- [ ] Pure Erlang business logic
- [ ] gen_server behaviors
- [ ] Pattern matching and recursion
- [ ] List/map operations

---

## Phase 1: Architecture Assessment (1-2 days)

### Step 1: Draw Current Architecture

```
Current System:

┌─────────────────────────────┐
│   Monolithic Erlang App     │
│ ┌──────────────────────────┐│
│ │ Business Logic           ││
│ ├──────────────────────────┤│
│ │ Protocol (Custom)        ││
│ ├──────────────────────────┤│
│ │ Network (gen_tcp)        ││
│ ├──────────────────────────┤│
│ │ File I/O                 ││
│ ├──────────────────────────┤│
│ │ Distribution (net_kernel)││
│ └──────────────────────────┘│
└─────────────────────────────┘

     ↕ (JavaScript)

┌─────────────────────────────┐
│   JavaScript App            │
│ ┌──────────────────────────┐│
│ │ Application Code         ││
│ ├──────────────────────────┤│
│ │ Custom Protocol Layer    ││
│ ├──────────────────────────┤│
│ │ WebSocket/HTTP           ││
│ └──────────────────────────┘│
└─────────────────────────────┘
```

### Step 2: Target Architecture

```
Target System:

OTP Runtime:
┌──────────────────────────────┐
│ unrdf_otp (environment)      │
│ ├─ gen_tcp                   │
│ ├─ cowboy                    │
│ ├─ ets                       │
│ └─ mnesia                    │
└───────┬──────────────────────┘
        │
        ▼
┌──────────────────────────────┐
│ unrdf_core (domain)          │
│ ├─ Business logic            │
│ ├─ gen_server behaviors      │
│ └─ Pure Erlang               │
└───────┬──────────────────────┘
        │
        ▼
┌──────────────────────────────┐
│ unrdf_protocol (shared)      │
│ ├─ Message encoding          │
│ ├─ Session tracking          │
│ └─ Error marshaling          │
└──────────────────────────────┘

         ↕ (Protocol)

Browser:
┌──────────────────────────────┐
│ JavaScript App               │
│ ├─ Application Code          │
│ └─ Dual Adapters             │
└───────┬──────────────────────┘
        │ (Same Protocol)
        │
        ├─→ RealAdapter (OTP)
        │   WebSocket/HTTP
        │   to OTP node
        │
        └─→ SimAdapter (AtomVM)
            WebSocket
            to WASM
```

### Step 3: Dependency Analysis

Create a spreadsheet:

| Module | Type | OTP? | AtomVM? | Action |
|--------|------|------|---------|--------|
| rdf_store | Core | ✓ | ✓ | Keep |
| rdf_query | Core | ✓ | ✓ | Keep |
| network_server | Env | ✓ | ✗ | Move to unrdf_otp |
| file_handler | Env | ✓ | ✗ | Move to unrdf_otp |
| custom_nif | Env | ✓ | ✗ | Stub for AtomVM |

---

## Phase 2: Code Refactoring (1-2 weeks)

### Step 1: Create Library Structure

```bash
# Create new directory structure
mkdir -p lib/unrdf_protocol/src
mkdir -p lib/unrdf_core/src
mkdir -p lib/unrdf_otp/src
mkdir -p lib/unrdf_atomvm/src
mkdir -p lib/unrdf_app/src

# Create rebar.config for each
cd lib/unrdf_protocol && rebar3 new lib unrdf_protocol
cd lib/unrdf_core && rebar3 new lib unrdf_core
# ... etc
```

### Step 2: Extract Protocol Layer

Move protocol code from your main app to `lib/unrdf_protocol/`:

```erlang
% Before: src/my_protocol.erl
-module(my_protocol).

encode_message(Msg) -> ...
decode_message(Bin) -> ...

% After: lib/unrdf_protocol/src/protocol_message.erl
-module(protocol_message).

encode_message(Msg) -> ...  % IDENTICAL CODE
decode_message(Bin) -> ...  % IDENTICAL CODE
```

**Key rule:** Protocol layer code is 100% identical in both modules.

### Step 3: Move Core Business Logic

Move pure Erlang code to `lib/unrdf_core/`:

```erlang
% src/rdf_store.erl → lib/unrdf_core/src/rdf_store.erl
% src/rdf_query.erl → lib/unrdf_core/src/rdf_query.erl
% (Move ONLY the pure logic, remove env-specific code)
```

Remove any dependencies on:
- `gen_tcp`, `cowboy`, `ssl`
- File I/O
- NIFs
- Distribution

### Step 4: Create Environment Layer

Create `lib/unrdf_otp/src/otp_*.erl` with OTP-specific implementations:

```erlang
% lib/unrdf_otp/src/otp_storage.erl
-module(otp_storage).

% OTP-specific storage implementation
new_cache(Config) ->
    ets:new(cache, [set, public, named_table, {read_concurrency, true}]).
```

Create `lib/unrdf_atomvm/src/atomvm_*.erl` with stubs:

```erlang
% lib/unrdf_atomvm/src/atomvm_storage.erl
-module(atomvm_storage).

% AtomVM-compatible storage (maps instead of ETS)
new_cache(Config) ->
    maps:new().
```

### Step 5: Update Dependencies

Update `rebar.config`:

```erlang
{deps, [
    % Shared by all
    {jsx, "3.1.0"},

    % OTP-specific
    {cowboy, {git, "https://github.com/ninenines/cowboy", {tag, "2.9.0"}},
             {only, [otp]}},
    {ets, {only, [otp]}},

    % Avoid in AtomVM
    {ssl, {only, [otp]}}
]}.
```

### Step 6: Add Conditional Compilation

```erlang
% In key modules
-ifdef(ATOMVM).
  -define(STORAGE_BACKEND, atomvm_storage).
-else.
  -define(STORAGE_BACKEND, otp_storage).
-endif.

init(Config) ->
    Storage = ?STORAGE_BACKEND:new_cache(Config),
    {ok, #{storage => Storage}}.
```

---

## Phase 3: JavaScript Migration (3-5 days)

### Step 1: Create Protocol Client

```typescript
// src/client.ts
import { ProtocolClient } from './protocol-client';

// Existing code: update to use new interface
const client = new ProtocolClient(
  process.env.NODE_ENV === 'production' ? 'production' : 'development'
);

await client.connect();

// Existing queries now use new protocol
const results = await client.executeQuery(query);
```

### Step 2: Migrate to Dual Adapters

Replace custom protocol with dual adapters:

```typescript
// Before
import { customProtocol } from './custom-protocol';

await customProtocol.connect(server);
const results = await customProtocol.query(q);

// After
import { ProtocolClient } from './client';

const client = new ProtocolClient();
await client.connect();
const results = await client.executeQuery(q);
```

### Step 3: Test Against Both Runtimes

```typescript
// test/both-runtimes.test.ts
describe('Both Runtimes', () => {
  const modes = ['production', 'development'];

  modes.forEach(mode => {
    describe(`Mode: ${mode}`, () => {
      let client: ProtocolClient;

      beforeAll(async () => {
        client = new ProtocolClient(mode);
        await client.connect();
      });

      it('executes queries', async () => {
        const results = await client.executeQuery(
          'SELECT * WHERE { ?s ?p ?o }'
        );
        expect(Array.isArray(results)).toBe(true);
      });

      afterAll(async () => {
        await client.disconnect();
      });
    });
  });
});
```

---

## Phase 4: Testing & Validation (3-5 days)

### Step 1: Run Protocol Tests

```bash
# Test protocol layer on both runtimes
rebar3 eunit tests=protocol_SUITE
```

Both should pass identically.

### Step 2: Run Golden Tests

```bash
# Test against OTP
npm run test:golden -- --target=production

# Test against AtomVM
npm run test:golden -- --target=development

# Compare results
npm run test:compare-results
```

### Step 3: Fault Injection Tests

```bash
# Test recovery behavior
npm run test:faults
```

Verify same behavior on both runtimes.

### Step 4: Performance Baseline

```bash
# Measure both runtimes
npm run benchmark:otp
npm run benchmark:atomvm

# Compare results
npm run compare:performance
```

Document expected performance characteristics.

---

## Phase 5: Deployment (1-2 days)

### Step 1: Build Both Targets

```bash
# OTP release
rebar3 release

# AtomVM bundle
rebar3 atomvm:package

# JavaScript
npm run build
```

### Step 2: Update CI/CD

```yaml
# Add multi-runtime testing to CI
test-atomvm:
  script: npm run test:integration:atomvm

test-otp:
  script: npm run test:integration:otp

deploy:
  dependencies: [test-atomvm, test-otp]
```

### Step 3: Gradual Rollout

```
Week 1: Deploy to staging (both runtimes)
Week 2: Canary deployment (10% traffic)
Week 3: Full production deployment
```

---

## Rollback Plan

If issues arise:

### For OTP
```bash
# Revert to previous release
bin/unrdf_otp stop
rm -rf current-release/
ln -s previous-release/ current-release
bin/unrdf_otp start
```

### For JavaScript
```bash
# Revert to previous build
git revert <commit-hash>
npm run build
npm run deploy
```

---

## Common Migration Issues

### Issue 1: Code Duplication

**Problem:** Protocol and domain code duplicated in both `*_otp` and `*_atomvm`

**Solution:** Keep shared code in `unrdf_protocol` and `unrdf_core` only

### Issue 2: ETS Usage

**Problem:** AtomVM doesn't fully support ETS

**Solution:** Use maps in process state instead

```erlang
% Instead of ets:insert/2
insert_cache(Key, Value, State) ->
    Cache = maps:put(Key, Value, State#state.cache),
    State#state{cache = Cache}.
```

### Issue 3: Timing Differences

**Problem:** AtomVM may be slower, tests fail

**Solution:** Use relative timeouts, not absolute

```typescript
// ✗ Bad: Absolute timeout
const start = Date.now();
await operation();
expect(Date.now() - start).toBeLessThan(100);  // May fail on AtomVM

// ✓ Good: Relative comparison
const start1 = measure(operation, 'otp');
const start2 = measure(operation, 'atomvm');
expect(start2 / start1).toBeLessThan(2);  // AtomVM can be 2x slower
```

---

## Validation Checklist

- [ ] Protocol layer identical on both runtimes
- [ ] All golden tests pass on both
- [ ] Fault injection tests pass on both
- [ ] Performance acceptable on both
- [ ] Code review passed
- [ ] Documentation updated
- [ ] CI/CD configured for both runtimes
- [ ] Monitoring set up for both targets
- [ ] Rollback plan tested

---

## See Also

- **02-RUNTIME-ARCHITECTURE.md** - Architecture details
- **05-SETUP-ERLANG-SIDE.md** - Erlang setup
- **06-SETUP-JS-SIDE.md** - JavaScript setup
