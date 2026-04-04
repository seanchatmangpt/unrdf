# Reference: XOR Split/Join

Precise specification of split and join types, the pattern registry, and all builder functions.

---

## `SPLIT_TYPE` constants

```javascript
SPLIT_TYPE = Object.freeze({
  SEQUENCE: 'sequence', // WP1  — single outgoing flow
  AND: 'and', // WP2  — all branches execute
  XOR: 'xor', // WP4  — exactly one branch
  OR: 'or', // WP6  — one or more branches
});
```

## `JOIN_TYPE` constants

```javascript
JOIN_TYPE = Object.freeze({
  SEQUENCE: 'sequence', // WP1  — single incoming flow
  AND: 'and', // WP3  — wait for all branches
  XOR: 'xor', // WP5  — first arrival proceeds
  OR: 'or', // WP7  — sync all activated branches
});
```

---

## `PATTERNS` registry

Each entry in `PATTERNS` describes one of Van der Aalst's workflow patterns.

| Key                        | `id`   | `wpNumber` | `splitType` | `joinType` | `minBranches` | `allowsCycles` |
| -------------------------- | ------ | ---------- | ----------- | ---------- | ------------- | -------------- |
| `SEQUENCE`                 | `WP1`  | 1          | `none`      | `none`     | 1             | false          |
| `PARALLEL_SPLIT`           | `WP2`  | 2          | `and`       | `none`     | 2             | false          |
| `SYNCHRONIZATION`          | `WP3`  | 3          | `none`      | `and`      | 2             | false          |
| `EXCLUSIVE_CHOICE`         | `WP4`  | 4          | `xor`       | `none`     | 2             | false          |
| `SIMPLE_MERGE`             | `WP5`  | 5          | `none`      | `xor`      | 2             | false          |
| `MULTI_CHOICE`             | `WP6`  | 6          | `or`        | `none`     | 2             | false          |
| `STRUCTURED_SYNC_MERGE`    | `WP7`  | 7          | `none`      | `or`       | 2             | false          |
| `MULTI_MERGE`              | `WP8`  | 8          | `none`      | `xor`      | 2             | false          |
| `STRUCTURED_DISCRIMINATOR` | `WP9`  | 9          | `none`      | `xor`      | 2             | false          |
| `ARBITRARY_CYCLE`          | `WP10` | 10         | `xor`       | `xor`      | 1             | **true**       |
| `IMPLICIT_TERMINATION`     | `WP11` | 11         | `none`      | `none`     | 0             | false          |
| `DEFERRED_CHOICE`          | `WP16` | 16         | `deferred`  | `none`     | 2             | false          |
| `CANCEL_TASK`              | `WP19` | 19         | `none`      | `none`     | 1             | false          |
| `CANCEL_CASE`              | `WP20` | 20         | `none`      | `none`     | 0             | false          |

Access by WP number: `getPatternByWPNumber(4)` returns the `EXCLUSIVE_CHOICE` entry.

---

## `TaskDefSchema` (patterns module)

Used when defining tasks via the lower-level `Workflow` class:

```typescript
TaskDefSchema = z.object({
  id: z.string().min(1).max(100),
  name: z.string().min(1).max(200).optional(),
  splitType: z.enum(['sequence', 'and', 'xor', 'or', 'deferred']).default('sequence'),
  joinType: z.enum(['sequence', 'and', 'xor', 'or']).default('sequence'),
  condition: z.function().optional(),
  timeout: z.number().positive().optional(),
  resource: z.string().min(1).max(100).optional(),
  role: z.string().min(1).max(100).optional(),
  cancellationRegion: z.string().min(1).max(100).optional(),
  priority: z.number().int().min(0).max(100).optional(),
});
```

## `FlowDefSchema`

```typescript
FlowDefSchema = z
  .object({
    from: z.string().min(1).max(100),
    to: z.string().min(1).max(100),
    condition: z.function().optional(), // (context) => boolean
    priority: z.number().int().default(0), // higher = evaluated first
    isCycle: z.boolean().optional(),
    deferred: z.boolean().optional(),
  })
  .refine(data => data.from !== data.to || data.isCycle === true, {
    message: 'from and to must differ unless isCycle is true',
  });
```

---

## Pattern builder functions

All builders are exported from `@unrdf/yawl` (also from `@unrdf/yawl/patterns`).

### `sequence(fromId, toId)`

WP1. Creates a single flow from source to target.

```typescript
function sequence(fromId: string, toId: string): FlowDef;
```

Returns a `FlowDef` object `{ from, to }`.

---

### `parallelSplit(sourceId, targetIds)`

WP2. Source task spawns all target tasks simultaneously (AND-split).

```typescript
function parallelSplit(
  sourceId: string,
  targetIds: string[] // min 2
): { pattern: 'PARALLEL_SPLIT'; sourceTask: { id; splitType: 'and' }; flows: FlowDef[] };
```

Throws `Error` if `targetIds.length < 2`.

Apply by spreading `.flows` into the workflow definition and setting the source task's `splitType: 'and'`.

---

### `synchronization(sourceIds, targetId)`

WP3. Target task waits until all source tasks complete (AND-join).

```typescript
function synchronization(
  sourceIds: string[], // min 2
  targetId: string
): { pattern: 'SYNCHRONIZATION'; targetTask: { id; joinType: 'and' }; flows: FlowDef[] };
```

Throws `Error` if `sourceIds.length < 2`.

---

### `exclusiveChoice(sourceId, branches)`

WP4. Exactly one branch is taken based on condition evaluation (XOR-split).

```typescript
function exclusiveChoice(
  sourceId: string,
  branches: Array<{
    taskId: string;
    condition?: (ctx: object) => boolean;
    priority?: number;
  }> // min 2
): { pattern: 'EXCLUSIVE_CHOICE'; sourceTask: { id; splitType: 'xor' }; flows: FlowDef[] };
```

Throws `Error` if `branches.length < 2`.

**Condition evaluation order**: branches are sorted by `priority` descending, then evaluated in order. The first branch whose condition returns `true` is taken. A branch with no condition function always evaluates to `true` and acts as the default (place it at the lowest priority).

---

### `simpleMerge(sourceIds, targetId)`

WP5. Target task is enabled by the first branch to complete (XOR-join).

```typescript
function simpleMerge(
  sourceIds: string[], // min 2
  targetId: string
): { pattern: 'SIMPLE_MERGE'; targetTask: { id; joinType: 'xor' }; flows: FlowDef[] };
```

---

### `multiChoice(sourceId, branches)`

WP6. One or more branches fire based on independent conditions (OR-split).

```typescript
function multiChoice(
  sourceId: string,
  branches: Array<{
    taskId: string;
    condition?: (ctx: object) => boolean;
    priority?: number;
  }> // min 2
): { pattern: 'MULTI_CHOICE'; sourceTask: { id; splitType: 'or' }; flows: FlowDef[] };
```

Unlike `exclusiveChoice`, multiple branches may be activated. All branches whose condition returns `true` fire.

---

### `structuredSyncMerge(sourceIds, targetId)`

WP7. Target task waits for all branches that were activated (OR-join).

```typescript
function structuredSyncMerge(
  sourceIds: string[], // min 2
  targetId: string
): { pattern: 'STRUCTURED_SYNC_MERGE'; targetTask: { id; joinType: 'or' }; flows: FlowDef[] };
```

---

### `arbitraryCycle(entryId, loopBackId, condition?)`

WP10. Creates a backward flow (cycle) from `entryId` back to `loopBackId`.

```typescript
function arbitraryCycle(
  entryId: string,
  loopBackId: string,
  condition?: (ctx: object) => boolean
): { pattern: 'ARBITRARY_CYCLE'; flows: [FlowDef] };
```

The generated flow has `isCycle: true`. Validation will not report it as a structural cycle error.

---

### `deferredChoice(sourceId, candidateIds)`

WP16. The first candidate to be externally started wins; the rest are cancelled.

```typescript
function deferredChoice(
  sourceId: string,
  candidateIds: string[] // min 2
): {
  pattern: 'DEFERRED_CHOICE';
  sourceTask: { id; splitType: 'deferred' };
  candidates: string[];
  flows: FlowDef[];
};
```

Generated flows carry `deferred: true`.

---

## `applyPattern(patternName, context, workflow?)`

Apply a named pattern with a context object:

```typescript
function applyPattern(
  patternName: keyof typeof PATTERNS,
  context: PatternContext,
  workflow?: Workflow
): PatternResult;
```

`PatternContextSchema`:

```typescript
{
  sourceId?:  string,
  targetId?:  string,
  targetIds?: string[],
  sourceIds?: string[],
  branches?:  Array<{ taskId: string, condition?: Function, priority?: number }>,
  loopCondition?: Function,
  data?:      Record<string, unknown>,
}
```

---

## Validation functions

| Function                                      | Signature                                                 | Description                                   |
| --------------------------------------------- | --------------------------------------------------------- | --------------------------------------------- |
| `validatePattern(patternName, context)`       | `(string, PatternContext) => PatternValidationResult`     | Check cardinality, required fields            |
| `validatePatternContext(patternDef, context)` | `(PatternDef, PatternContext) => PatternValidationResult` | Validate context against pattern requirements |
| `validateCardinality(patternDef, context)`    | `(PatternDef, PatternContext) => PatternValidationResult` | Check `minBranches` constraint                |
| `validateSplitJoinMatch(workflow)`            | `(Workflow) => PatternValidationResult`                   | Every split must have a matching join         |
| `detectCycles(workflow)`                      | `(Workflow) => string[]`                                  | Returns task IDs that form cycles             |
| `validateNoCycles(workflow)`                  | `(Workflow) => PatternValidationResult`                   | Fail if non-cycle-marked cycles exist         |

`PatternValidationResult`:

```typescript
{ valid: boolean, errors: string[], warnings: string[] }
```

---

## See also

- [Explanation: YAWL Pattern Language](../explanation/01-yawl-pattern-language.md)
- [Tutorial: Conditional Branching](../tutorials/02-conditional-branching.md)
- [Workflow Schema Reference](./workflow-schema.md)
