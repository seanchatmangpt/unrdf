# How hooks execute Works

This document explains the design of `unrdf hooks execute`: the two-phase condition/effect model, what happens inside each phase, and why the JSON config schema (`KnowledgeHookSchema`) exists separately from the JavaScript `defineHook()` API.

## The Two-Phase Model

When you run `unrdf hooks execute --store store.ttl --config hooks.json`, the CLI runs two sequential phases for every hook in the config array.

### Phase 1 — Condition Evaluation

The CLI calls `evaluateCondition(hook.condition, rdfStore)` from the `@unrdf/hooks` package. This function dispatches to the appropriate evaluator based on `condition.kind`:

- `sparql-ask` — runs the SPARQL ASK query; result is `true` or `false`.
- `sparql-select` — runs the SELECT query; result is a binding array. Non-empty means satisfied.
- `shacl` — validates the store against a SHACL shapes graph.
- `delta`, `threshold`, `count`, `window`, `n3`, `datalog` — each has its own evaluator.

The hook is considered _satisfied_ when `evaluateCondition` returns `true` or a non-empty array. Unsatisfied hooks are recorded in the results with `satisfied: false` and no effect is applied.

Hooks where `enabled === false` are skipped entirely before Phase 1 runs.

### Phase 2 — Effect Execution

If and only if the condition is satisfied, the CLI applies the hook's effect. Currently `hooks execute` supports one effect kind: `sparql-construct`.

For `sparql-construct`, the CLI runs `rdfStore.query(hook.effect.query)`, which returns an iterable of quads. Each quad is added to the live in-memory store with `rdfStore.add(quad)`. The total number of quads added across all hooks is reported in the summary.

The store modification is in-memory only. It is not persisted back to the source file. To capture the resulting graph you would export it with `unrdf graph export` after running a load/execute pipeline.

## Priority and Order

Hooks are executed in the order they appear in the config array. The `priority` field is present in the schema and recorded in results, but `hooks execute` does not sort by priority — it respects the array order. Sort your config array if execution order matters.

## Why KnowledgeHookSchema (JSON) Is Different from defineHook() (JavaScript)

The `@unrdf/hooks` package exposes two separate ways to define a hook:

**`defineHook()` (JavaScript API)** — Takes a plain JavaScript object or closure. Conditions and effects can be arbitrary functions. This is the programmatic API used inside Node.js applications where full code execution is available. There is no schema validation at call time; the hook is trusted code.

**`KnowledgeHookSchema` (JSON config)** — A Zod schema that describes a serializable, data-only representation of a hook. Conditions are expressed as data (a SPARQL string, a reference to a shapes file, etc.) rather than code. Effects are similarly data-driven (`sparql-construct` with a query string).

The JSON schema exists for three reasons:

1. **Serialization** — JSON configs can be stored in files, version-controlled, and transmitted over a network. JavaScript closures cannot be serialized.
2. **Security** — Accepting arbitrary JavaScript functions from an external file would be a code-execution vulnerability. The JSON schema constrains what a hook can do to declared, enumerable operations.
3. **Governance** — The schema enforces that every hook has an identity (`id`), human-readable metadata (`meta.name`, `meta.version`), and explicit enablement (`enabled`). This makes hook inventories auditable.

When `hooks execute` processes a config file, it calls `KnowledgeHookSchema.parse(hookDef)` on each entry. Any field that violates the Zod schema causes that hook to fail with an error recorded in the results — it does not stop execution of the remaining hooks.

## What the Receipt Chain Records

When you pass `--output results.json`, the CLI saves each hook's execution result including timestamps, `satisfied` status, and effect outcomes. The `receipts` array in the output captures a hash-chained audit trail. Each receipt includes a hash of the current result plus the previous receipt's hash, forming a chain. `hooks receipts --verify` checks that no link in the chain was tampered with by recomputing and comparing hashes.

This design allows hook execution to be audited after the fact without re-running the hooks.

## Summary

- Phase 1 uses `evaluateCondition()` from `@unrdf/hooks` — pure read-only evaluation.
- Phase 2 applies SPARQL CONSTRUCT effects by mutating the in-memory store.
- The JSON schema is intentionally more restrictive than the JS API to enable serialization, safety, and auditability.
- Execution order follows the array order in the config file; `priority` is metadata only.
