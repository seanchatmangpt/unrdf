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

## Authoring AVM programs for this runtime

- **The trailer must be a full 12-byte zero entry.** AtomVM's lookup loops read the flags word of the *next* entry; with a shorter trailer any module-lookup miss reads past the buffer and the VM spins at 100% CPU forever instead of failing with `undef`. `src/avm-packer.mjs` writes and validates the correct trailer and AtomVM's flag meanings (START=1, CODE=2); `bin/atomvm-wasm.mjs` refuses malformed packs before starting the VM.
- **`spawn/1,3` need `erlang.beam` in the pack.** In AtomVM 0.6.x they are Erlang wrappers in estdlib's `erlang.erl` (`test/fixtures/estdlib/erlang.beam`). Forgetting it now fails fast with `Failed to open module: erlang.beam`; it is tested.
- **`atomvm:read_priv(App, Path)`** needs the file packed as entry `<app>/priv/<path>` **with a u32be length prefix in front of the content** (what the reference packer writes; `packAvm` does it for `{file: true}` entries). `nif_atomvm_read_priv` reads the first 4 data bytes as the file size; without the prefix, text like `hell` becomes a ~1.7 GB size and the 32-bit wasm build dies in `term_from_int32: unimplemented: term should be moved to heap` (any size >= 2^28). It was a packer bug, not a runtime bug. Second gotcha: AtomVM 0.6.6's `avmpack_find_section_by_name` stops scanning at the first entry whose *flags* are 0, so with flags-0 files only the first one is findable; the packer marks plain files with the unused flag bit `AVM_FLAG_FILE = 4`. `parseAvm` refuses prefix-less files.
- **`erlang:md5/1` is unavailable in the emscripten build.** It is `crypto:hash(md5, X)` in estdlib; the NIF lives in `libAtomVM/otp_crypto.c`, which only the generic-unix (MbedTLS), esp32 and rp2040 CMake targets compile. The emscripten target never builds it, so even with `crypto.beam` in the pack the call fails with `undef`. Verified: with a compiled `crypto.beam` added, `erlang:md5(<<"abc">>)` still crashes `undef`. Fixing it would need an MbedTLS build for wasm (not in Emscripten's ports); not attempted. Compute digests in JS (WebCrypto) at the tier boundary instead.
- **Exit latency.** The pthread wasm build runs scheduler threads in Node Worker isolates. V8 tiers the module up with TurboFan in background threads and Node cannot dispose a Worker isolate until those jobs finish, so process exit lagged 0.7-2 s behind the program's last output (the VM itself had already finished: `main` returned, `running_schedulers=0`). `bin/atomvm-wasm.mjs` now sets `--liftoff-only` before compiling; spawn programs take ~0.3-0.5 s and a 60M-iteration Erlang loop runs at the same speed. Exit codes and output are unaffected.
- `AtomVMProcessBroker`'s `timeoutMs` still kills a genuinely runaway program (tested with `loop_forever`).

History: an earlier version of this document blamed `spawn` hangs on the shipped WASM runtime. That was wrong - the hang was our own packer's 4-byte trailer, found by rebuilding AtomVM 0.6.6 from source and profiling it.
