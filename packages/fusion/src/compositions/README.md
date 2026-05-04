# UNRDF Capability Compositions

This directory contains **high-level compositions** of UNRDF atomic capabilities. Each composition demonstrates emergent functionality not available from individual atoms.

## Available Compositions

### 1. Frozen Universe + Receipt (`freeze-receipt.mjs`)

**Atoms**: A08 (Universe Freezing) + A09 (Receipt Generation)  
**Emergent Capability**: Time-travel snapshots with cryptographic audit trail  
**Use Cases**: Compliance logging, blockchain anchoring, forensic analysis

```javascript
import { createFrozenReceipt } from '@unrdf/fusion/compositions';
import { KGCStore } from '@unrdf/kgc-4d';

const kgcStore = new KGCStore();
// ... log events ...

const { receipt, verification } = await createFrozenReceipt(kgcStore, {
  tag: 'audit-snapshot',
  metadata: { reason: 'quarterly-compliance-check' },
});

console.log('Receipt Hash:', receipt.hash);
console.log('Verified:', verification.valid);
```

### 2. Hook + Policy Gate (`hook-policy-gate.mjs`)

**Atoms**: A13 (Hook Execution) + A15 (Policy Validation)  
**Emergent Capability**: Gated quad insertion with pre-validation  
**Use Cases**: Data governance, SHACL-lite validation, access control

```javascript
import { createPolicyGatedStore } from '@unrdf/fusion/compositions';
import { defineHook } from '@unrdf/hooks';
import { dataFactory } from '@unrdf/oxigraph';

const iriPolicy = defineHook({
  id: 'iri-validator',
  trigger: 'quad.before-add',
  validate: quad => {
    if (!quad.subject.value.startsWith('https://')) {
      return { valid: false, error: 'HTTPS IRIs only' };
    }
    return { valid: true };
  },
});

const gatedStore = createPolicyGatedStore([iriPolicy]);
const result = gatedStore.addWithValidation(quad);
// result: { added: true } or { added: false, reason: '...' }
```

## Running Demos

**Prerequisites**: Install dependencies in monorepo root

```bash
cd /home/user/unrdf
pnpm install
```

**Run individual demo**:

```bash
node packages/fusion/src/compositions/freeze-receipt.mjs
node packages/fusion/src/compositions/hook-policy-gate.mjs
```

**Run all demos**:

```bash
node packages/fusion/src/compositions/index.mjs
```

## Composition Principles

1. **Same Runtime**: All atoms must be compatible (Node.js, Browser, or WASM)
2. **Type Safety**: Outputs match inputs or share common substrate (RDF Store)
3. **Acyclic Dependencies**: No circular package dependencies
4. **Emergent Value**: Composition provides capability neither atom has alone

## Adding New Compositions

1. Create `new-composition.mjs` in this directory
2. Export main function and demo function
3. Add to `index.mjs` exports
4. Document in this README
5. Add tests (optional but recommended)

## Related Documentation

- Full capability basis: `/home/user/unrdf/docs/capability-basis.md`
- Atom definitions: See package READMEs in `/home/user/unrdf/packages/`
- Integration guide: `/home/user/unrdf/docs/integration.md`
