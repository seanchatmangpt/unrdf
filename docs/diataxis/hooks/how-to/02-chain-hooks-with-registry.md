# How To: Chain Hooks with a Registry

Use a hook registry when you want multiple hooks dispatched by trigger type rather than
composing them manually into an array.

## 1. Create a registry and register hooks

```javascript
import {
  createHookRegistry,
  registerHook,
  defineHook,
  validateIRIFormat,
  trimLiterals,
} from '@unrdf/hooks';

const registry = createHookRegistry();

// Register built-in hooks
registerHook(registry, validateIRIFormat);
registerHook(registry, trimLiterals);

// Register a custom hook
registerHook(
  registry,
  defineHook({
    name: 'reject-blank-subject',
    trigger: 'before-add',
    validate: quad => quad.subject.termType !== 'BlankNode',
  })
);
```

`createHookRegistry()` returns `{ hooks: Map, triggerIndex: Map }`. `registerHook()` indexes
each hook by its trigger so that `executeHooksByTrigger` can find only the relevant hooks
without scanning all registered hooks.

Registering a hook with a name that already exists throws:

```
Error: Hook already registered: validate-iri-format
```

## 2. Execute hooks by trigger

```javascript
import { executeHooksByTrigger } from '@unrdf/hooks';

const quad = {
  subject: { termType: 'NamedNode', value: 'http://example.org/s' },
  predicate: { termType: 'NamedNode', value: 'http://schema.org/name' },
  object: { termType: 'Literal', value: '  Alice  ', language: '', datatype: null },
  graph: { termType: 'DefaultGraph', value: '' },
};

const result = executeHooksByTrigger(registry, 'before-add', quad);
```

## 3. Read the ChainResult — NOT an array

`executeHooksByTrigger` returns a `ChainResult` object. It is **not** an array, and you must
not index it.

```javascript
// WRONG — result is not an array
const ok = result[0].valid; // undefined — silent bug

// CORRECT
const ok = result.valid; // boolean
const finalQuad = result.quad; // the (possibly transformed) quad
const perHook = result.results; // HookResult[] — individual hook outcomes
const errMsg = result.error; // string | undefined — first failure message
const who = result.failedHook; // string | undefined — name of failing hook
```

## 4. Gate the store write on the result

```javascript
if (!result.valid) {
  throw new Error(`Quad rejected by hook '${result.failedHook}': ${result.error}`);
}

// Use result.quad — it carries any transformations applied by the chain
store.add(result.quad);
```

## 5. Complete example — validate-before-store pattern

```javascript
import {
  createHookRegistry,
  registerHook,
  executeHooksByTrigger,
  validateIRIFormat,
  trimLiterals,
  standardValidation,
} from '@unrdf/hooks';

function buildRegistry() {
  const registry = createHookRegistry();
  registerHook(registry, validateIRIFormat);
  registerHook(registry, trimLiterals);
  registerHook(registry, standardValidation);
  return registry;
}

async function safeAdd(store, registry, quad) {
  const result = executeHooksByTrigger(registry, 'before-add', quad);

  if (!result.valid) {
    throw new Error(`Rejected [${result.failedHook}]: ${result.error}`);
  }

  store.add(result.quad); // use the transformed quad, not the original
  return result.quad;
}
```

## Notes

- Hooks registered for a different trigger are not run. A hook registered for `after-add`
  will not fire when `executeHooksByTrigger(registry, 'before-add', quad)` is called.
- Hook execution order within a trigger is insertion order (Map preserves insertion order,
  Set iteration is insertion order in V8).
- `executeHookChain` is the underlying implementation; `executeHooksByTrigger` is a
  convenience wrapper that selects the right subset.
