# The AtomVM continuum: browser → edge → fog → cloud

Four tiers run real AtomVM and prove it to each other with receipts.

| Tier | Where | Role |
|------|-------|------|
| browser | `src/continuum/browser-client.mjs` (in a page) | produces first-mile data, consumes last-mile data; boots the WASM build in-page |
| edge | `TierNode({tier:'edge'})` | first hop from devices/browsers, warm cache for the last mile |
| fog | `TierNode({tier:'fog'})` | regional aggregation + cache |
| cloud | `TierNode({tier:'cloud'})` | root of trust, system of record (no upstream) |

## Bootstrapping

Nodes come up cloud → fog → edge → browser. `TierNode.start()` refuses (state `Refused`, never listens) unless:

1. topology is lawful (cloud has no upstream, everything else has one);
2. the `.avm` is structurally valid (`parseAvm`);
3. the AtomVM runtime boots (`AtomVMNodeRuntime`: native `AtomVM` if present, else the bundled `bin/atomvm-wasm.mjs`);
4. the tier witness prints exactly `witnessMarker(tier)` — right tier's program **and** a correct Adler-32 computed inside the VM;
5. the upstream answers `/health` as `Alive` **and** as the adjacent tier.

The browser does the equivalent in-page: boots `AtomVM-web-*.wasm`, checks the browser witness, then confirms the edge is an Alive edge.

## First mile (data travels up)

`POST /ingest` → each tier validates, runs its AtomVM witness through `AtomVMSwarmCluster`/`AtomVMProcessBroker` (a receipted actuation), seals a receipt linked to the previous hop, forwards upstream, and **stores only after the cloud acknowledged** (no write-behind). Replays are idempotent by canonical payload digest.

## Last mile (data travels down)

`GET /deliver/:digest` → served from the nearest tier holding it; otherwise fetched from upstream, **re-verified** (payload hash, receipt chain, adjacent-tier rule), cached, receipted, and returned. Warm data keeps being served if everything above is down; cold data fails loudly.

`verifyChain` (browser-safe, WebCrypto) is the single verifier used by nodes and by the browser.

## Running the tests

```bash
pnpm --filter @unrdf/atomvm run test:continuum   # real AtomVM, real sockets, real Chromium
pnpm --filter @unrdf/atomvm test                 # everything (vitest + node:test)
pnpm --filter @unrdf/atomvm run build:fixtures   # rebuild .avm fixtures (needs erlc)
CHROMIUM_PATH=/path/to/chrome ...                # if Chromium is not at Playwright's default
```

## Known limitation of the shipped WASM build

`spawn/1,3` never returns (the VM busy-waits at 100% CPU) in `public/AtomVM-{node,web}-agnostic.wasm`; adjusting the pthread pool or core count did not help. Programs run on it must not spawn processes (message passing to `self()`, `receive … after`, timers and binary arithmetic work). `AtomVMProcessBroker`'s `timeoutMs` kills a runaway program (tested). Integers ≥ 2^27 are not supported by this 32-bit build. `atomvm:read_priv/2` and `erlang:md5/1` also fail here. A rebuilt runtime is needed to lift these.
