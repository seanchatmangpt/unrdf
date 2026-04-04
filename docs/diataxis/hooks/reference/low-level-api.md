# Reference: Low-Level Hook API

All functions are synchronous unless otherwise noted. They are exported from `@unrdf/hooks`.

---

## `defineHook(config)`

Create a validated hook object.

**Parameters**

| Field       | Type                      | Required | Description                                   |
| ----------- | ------------------------- | -------- | --------------------------------------------- |
| `name`      | `string` (min 1)          | Yes      | Unique hook identifier                        |
| `trigger`   | `HookTrigger`             | Yes      | When the hook fires (see trigger table below) |
| `validate`  | `(quad: Quad) => boolean` | No\*     | Return `false` to reject the quad             |
| `transform` | `(quad: Quad) => Quad`    | No\*     | Return a new quad object                      |
| `metadata`  | `Record<string, any>`     | No       | Arbitrary metadata                            |

\*At least one of `validate`, `transform`, or `run` must be present.

**Returns** `Hook` — the validated hook object with pre-computed `_hasValidation` and
`_hasTransformation` flags.

**Throws** `ZodError` if config is invalid.

### HookTrigger values

Core: `before-add` `after-add` `before-query` `after-query` `before-remove` `after-remove`

Transaction: `before-commit` `after-commit` `before-rollback` `after-rollback`

Error/Event: `on-error` `on-validation-fail` `on-transform` `on-timeout` `on-circuit-open`

Async/IO: `before-fetch` `after-fetch` `before-sync` `after-sync` `before-import` `after-import`

Cron/Time: `on-schedule` `on-interval` `on-idle` `on-startup`

Quality: `quality-gate` `defect-detection` `continuous-improvement` `spc-control`
`capability-analysis` `root-cause` `kaizen-event` `audit-trail`

---

## `executeHook(hook, quad, options?)`

Execute a single hook against one quad.

**Parameters**

| Param                     | Type      | Description                                                  |
| ------------------------- | --------- | ------------------------------------------------------------ |
| `hook`                    | `Hook`    | Hook created by `defineHook`                                 |
| `quad`                    | `Quad`    | Must have `subject`, `predicate`, `object`                   |
| `options.warnPooledQuads` | `boolean` | Default `true`. Set `false` to suppress pooled-quad warnings |

**Returns** `HookResult`

```typescript
{
  valid: boolean;
  quad: Quad;          // original or transformed
  hookName: string;
  error?: string;      // set when valid === false
  warning?: string;    // set by POKA-YOKE guard on non-boolean validate() return
}
```

**Throws** `TypeError` if `quad` is missing required properties, or if `transform()` returns a
non-object or an object missing `subject`/`predicate`/`object`.

---

## `executeHookChain(hooks, quad)`

Execute an array of hooks in sequence. Stops at the first validation failure.
Transformations are chained: each hook receives the output of the previous.

**Parameters**

| Param   | Type     | Description            |
| ------- | -------- | ---------------------- |
| `hooks` | `Hook[]` | Ordered array of hooks |
| `quad`  | `Quad`   | Starting quad          |

**Returns** `ChainResult`

```typescript
{
  valid: boolean;
  quad: Quad;           // final quad after all transforms
  results: HookResult[]; // one entry per hook that ran
  error?: string;        // message from first failing hook
  failedHook?: string;   // name of first failing hook
}
```

---

## `executeHooksByTrigger(hooksOrRegistry, trigger, quad)`

Select hooks matching `trigger` from a registry (or array) and execute them as a chain.

**Returns** `ChainResult` — the same type as `executeHookChain`. **This is not an array.** Do
not use `result[0]`.

```javascript
const result = executeHooksByTrigger(registry, 'before-add', quad);
result.valid; // boolean
result.quad; // Quad
result.results; // HookResult[]
```

---

## `createHookRegistry()`

**Returns** `HookRegistry` — `{ hooks: Map<string, Hook>, triggerIndex: Map<HookTrigger, Set<string>> }`

---

## `registerHook(registry, hook)`

Add a hook to the registry. Indexes by trigger for O(1) lookup.

**Throws** `Error` if a hook with the same name is already registered.

---

## `validateBatch(hooks, quads)`

**Returns** `boolean[]` — one boolean per quad. True means all hooks passed.

Validation-only (transforms are not applied). Uses a Zod-free hot path for high throughput.

---

## `executeBatch(hooks, quads, options?)`

**Returns** `ChainResult[]` — one per quad.

`options.stopOnError` (default `false`) — if `true`, stops processing on the first failing quad.

---

## `transformBatch(hooks, quads, options?)`

**Returns** `Quad[]` — only quads that passed validation (if `validateFirst: true`) or all
transformed quads (default). Quads that throw during transformation are excluded.

`options.validateFirst` (default `false`) — run validation hooks before transform hooks.
