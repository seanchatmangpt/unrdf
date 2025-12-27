# V6 CLI Integration Guide

## Overview

This guide explains how to integrate the V6 unified noun-verb CLI spine with the existing kgc-cli registry.

---

## Architecture Integration

### Current Architecture (V5)

```
kgc-cli/
├── src/
│   ├── cli.mjs              # Citty entry point
│   ├── lib/
│   │   └── registry.mjs     # Extension registry
│   └── manifest/
│       └── extensions.mjs   # Extension manifest (47 packages)
└── extensions/
    ├── kgc-4d.mjs          # KGC-4D commands
    ├── blockchain.mjs      # Blockchain/receipt commands
    ├── hooks.mjs           # Hook/policy commands
    └── ...                 # 44 more extensions
```

### V6 Enhancement

```
v6-core/
├── src/
│   └── cli/
│       ├── nouns.mjs          # 10 canonical nouns
│       ├── verbs.mjs          # 25 canonical verbs
│       ├── spine.mjs          # V6 spine builder
│       └── commands/
│           ├── receipt.mjs    # Receipt commands (IMPL)
│           └── delta.mjs      # Delta commands (IMPL)
└── package.json

Integration: v6-core commands register via kgc-cli registry
```

---

## Step 1: Add V6-Core to KGC-CLI Manifest

**File**: `/home/user/unrdf/packages/kgc-cli/src/manifest/extensions.mjs`

Add to the extensions array:

```javascript
export const extensions = [
  // ... existing extensions ...

  // ===== V6 CORE (5-6) =====
  {
    id: '@unrdf/v6-core/receipt',
    path: '../../v6-core/src/cli/commands/receipt.mjs',
    loadOrder: 5,
    enabled: true
  },
  {
    id: '@unrdf/v6-core/delta',
    path: '../../v6-core/src/cli/commands/delta.mjs',
    loadOrder: 6,
    enabled: true
  },

  // ===== HIGH PRIORITY (10-19) =====
  {
    id: '@unrdf/kgc-4d',
    path: '../extensions/kgc-4d.mjs',
    loadOrder: 10,
    enabled: true
  },
  // ... rest ...
];
```

**Rationale**: Load v6-core early (5-6) to establish canonical patterns before other extensions.

---

## Step 2: Update Extension Modules

### Before (V5 Pattern)

```javascript
// packages/kgc-cli/extensions/blockchain.mjs
export default {
  id: '@unrdf/blockchain',
  nouns: {
    receipt: {
      verbs: {
        verify: {
          description: 'Verify receipt',
          handler: async (args) => {
            // Old implementation
            return { verified: true };
          }
        }
      }
    }
  }
};
```

### After (V6 Pattern)

```javascript
// packages/kgc-cli/extensions/blockchain.mjs
import { createV6Extension } from '@unrdf/v6-core/cli/spine';
import { isValidCombination } from '@unrdf/v6-core/cli/verbs';

export default createV6Extension({
  id: '@unrdf/blockchain',
  nouns: {
    receipt: {
      verbs: {
        verify: {
          description: 'Verify receipt chain integrity',
          handler: async (args) => {
            // New implementation with receipt wrapping
            return { verified: true };
          },
          argsSchema: z.object({
            hash: z.string(),
            depth: z.number().optional()
          })
        }
      }
    }
  }
});
```

**Benefits**:
- ✅ Automatic receipt wrapping
- ✅ V6 ontology validation
- ✅ Zod schema enforcement
- ✅ Consistent error handling

---

## Step 3: Validate V6 Spine Coverage

**File**: `/home/user/unrdf/packages/kgc-cli/scripts/validate-v6-spine.mjs`

```javascript
#!/usr/bin/env node
import { Registry } from '../src/lib/registry.mjs';
import { loadManifest } from '../src/manifest/extensions.mjs';
import { buildV6Spine, generateSpineReport } from '@unrdf/v6-core/cli/spine';

async function main() {
  const registry = new Registry({ failOnCollision: true });
  await loadManifest(registry);

  const spine = buildV6Spine(registry);
  const report = generateSpineReport(spine);

  console.log(report);

  if (spine.validationErrors.length > 0) {
    console.error('\n❌ V6 validation failed');
    process.exit(1);
  }

  if (spine.stats.missingCommands > 20) {
    console.warn(`\n⚠️  ${spine.stats.missingCommands} commands not yet implemented`);
  }

  console.log('\n✅ V6 spine validated');
}

main().catch(e => {
  console.error(e);
  process.exit(1);
});
```

**Run**:
```bash
cd /home/user/unrdf/packages/kgc-cli
timeout 5s node scripts/validate-v6-spine.mjs
```

---

## Step 4: Migration Strategy

### Phase 1: V6 Core Commands (Weeks 1-2)
- ✅ receipt: verify, chain, anchor, export
- ✅ delta: propose, apply, verify, export
- **Status**: COMPLETE

### Phase 2: KGC-4D Migration (Weeks 3-4)
- Migrate: universe (create, freeze, restore, verify, export)
- Migrate: eventlog (append, replay, reconstruct, verify, export)
- Update: `@unrdf/kgc-4d` extension to use `createV6Extension()`

### Phase 3: Infrastructure Commands (Weeks 5-6)
- Implement: workflow (start, pause, resume, verify)
- Implement: resource (allocate, release, query)
- Update: `@unrdf/yawl`, `@unrdf/core` extensions

### Phase 4: Validation & Documentation (Weeks 7-8)
- Implement: grammar (validate, compile, parse)
- Implement: thesis (compile, render, validate)
- Implement: policy (validate, apply, test)
- Update: `@unrdf/validation`, `@unrdf/docs`, `@unrdf/hooks` extensions

### Phase 5: Package Management (Week 9)
- Implement: package (list, install, validate, export)
- Update: `@unrdf/kgc-cli` extension

---

## Existing Command Mapping

### KGC-4D Commands

| Current Command | V6 Equivalent | Migration Status |
|----------------|---------------|------------------|
| `kgc snapshot create` | `kgc universe create` | 🔄 Rename needed |
| `kgc snapshot freeze` | `kgc universe freeze` | 🔄 Rename needed |
| `kgc snapshot restore` | `kgc universe restore` | 🔄 Rename needed |
| `kgc snapshot verify` | `kgc universe verify` | 🔄 Rename needed |
| `kgc event append` | `kgc eventlog append` | 🔄 Rename needed |
| `kgc event replay` | `kgc eventlog replay` | 🔄 Rename needed |

**Action**: Update `packages/kgc-cli/extensions/kgc-4d.mjs` to export both old and new nouns during transition.

```javascript
export default createV6Extension({
  id: '@unrdf/kgc-4d',
  nouns: {
    // V6 canonical
    universe: {
      verbs: {
        create: { /* ... */ },
        freeze: { /* ... */ }
      }
    },
    // V5 compatibility (deprecated)
    snapshot: {
      verbs: {
        create: { /* alias to universe.create */ },
        freeze: { /* alias to universe.freeze */ }
      }
    }
  }
});
```

### Blockchain Commands

| Current Command | V6 Equivalent | Migration Status |
|----------------|---------------|------------------|
| (None) | `kgc receipt verify` | ✅ New in V6 |
| (None) | `kgc receipt chain` | ✅ New in V6 |
| (None) | `kgc receipt anchor` | ✅ New in V6 |

**Action**: No migration needed - these are new commands.

### Hooks Commands

| Current Command | V6 Equivalent | Migration Status |
|----------------|---------------|------------------|
| `kgc hooks validate` | `kgc policy validate` | 🔄 Rename needed |
| `kgc hooks apply` | `kgc policy apply` | 🔄 Rename needed |
| `kgc hooks test` | `kgc policy test` | 🔄 Rename needed |

**Action**: Update `packages/kgc-cli/extensions/hooks.mjs` to use `policy` noun.

### YAWL Commands

| Current Command | V6 Equivalent | Migration Status |
|----------------|---------------|------------------|
| `kgc yawl start` | `kgc workflow start` | 🔄 Rename needed |
| `kgc yawl pause` | `kgc workflow pause` | 🔄 Rename needed |
| `kgc yawl resume` | `kgc workflow resume` | 🔄 Rename needed |

**Action**: Update YAWL extensions to use `workflow` noun.

---

## Testing Strategy

### Unit Tests

```javascript
// packages/v6-core/test/cli/spine.test.mjs
import { describe, test } from 'node:test';
import assert from 'node:assert';
import { buildV6Spine } from '../../src/cli/spine.mjs';
import { Registry } from '@unrdf/kgc-cli/lib/registry';

describe('V6 Spine', () => {
  test('validates canonical nouns', () => {
    const registry = new Registry();
    const spine = buildV6Spine(registry);
    assert.equal(spine.validationErrors.length, 0);
  });

  test('detects invalid noun-verb combinations', () => {
    const registry = new Registry();
    // Register invalid combination
    registry.registerExtension({
      id: 'test',
      nouns: {
        receipt: {
          verbs: {
            invalid_verb: { handler: () => {}, description: 'Invalid' }
          }
        }
      }
    });

    const spine = buildV6Spine(registry);
    assert(spine.validationErrors.length > 0);
    assert.equal(spine.validationErrors[0].type, 'INVALID_COMBINATION');
  });
});
```

### Integration Tests

```javascript
// packages/kgc-cli/test/integration/v6-commands.test.mjs
import { describe, test } from 'node:test';
import assert from 'node:assert';
import { execSync } from 'child_process';

describe('V6 Commands', () => {
  test('kgc receipt verify returns JSON envelope', () => {
    const output = execSync(
      'node packages/kgc-cli/src/cli.mjs receipt verify --hash abc123 --json',
      { encoding: 'utf8' }
    );

    const result = JSON.parse(output);
    assert.equal(result.ok, true);
    assert(result._receipt);
    assert.equal(result._receipt.noun, 'receipt');
    assert.equal(result._receipt.verb, 'verify');
  });

  test('kgc delta propose validates schema', () => {
    try {
      execSync(
        'node packages/kgc-cli/src/cli.mjs delta propose --delta "invalid json"',
        { encoding: 'utf8' }
      );
      assert.fail('Should have thrown');
    } catch (e) {
      assert(e.message.includes('Invalid arguments'));
    }
  });
});
```

**Run Tests**:
```bash
cd /home/user/unrdf
timeout 10s pnpm test --filter @unrdf/v6-core
timeout 10s pnpm test --filter @unrdf/kgc-cli
```

---

## Error Handling

### V6 Error Envelope

All V6 commands return consistent error envelopes:

```json
{
  "ok": false,
  "code": "INVALID_COMBINATION",
  "message": "Verb 'invalid_verb' not applicable to noun 'receipt'",
  "details": {
    "noun": "receipt",
    "verb": "invalid_verb",
    "validVerbs": ["verify", "chain", "anchor", "export"]
  },
  "_receipt": {
    "noun": "receipt",
    "verb": "invalid_verb",
    "timestamp": "2025-12-27T10:30:00.000Z",
    "duration": 5,
    "status": "error"
  }
}
```

### Error Codes

| Code | Meaning | Example |
|------|---------|---------|
| `UNKNOWN_NOUN` | Noun not in canonical set | `kgc invalidnoun verb` |
| `INVALID_COMBINATION` | Verb not applicable to noun | `kgc receipt start` |
| `INVALID_ARGS` | Zod validation failed | `kgc delta propose --delta "bad"` |
| `COMMAND_ERROR` | Handler threw exception | Internal error |
| `NOT_ADMISSIBLE` | Delta admissibility check failed | Conflicting delta |

---

## Performance Considerations

### Command Execution Time

Target SLA: **<500ms for read operations, <2s for mutations**

| Command Type | Target | Timeout |
|-------------|--------|---------|
| Query (list, query) | <200ms | 2s |
| Validation (verify, validate) | <500ms | 5s |
| Mutation (apply, create) | <2s | 10s |
| Compilation (compile, render) | <5s | 15s |

### Optimization Strategies

1. **Lazy Loading**: Only import command modules when invoked
2. **Caching**: Cache noun-verb matrix at registry build time
3. **Streaming**: Use streams for large export operations
4. **Parallelization**: Run independent validations in parallel

---

## Documentation

### Auto-Generated Help

```bash
# List all nouns
kgc --help

# List verbs for noun
kgc receipt --help

# Show verb usage
kgc receipt verify --help
```

Output:
```
kgc receipt verify - Verify receipt chain integrity

Usage:
  kgc receipt verify --hash <sha256> [options]

Options:
  --hash <string>     SHA-256 hash to verify (required)
  --depth <number>    Chain depth to verify (default: 10)
  --json              Output in JSON envelope format

Examples:
  kgc receipt verify --hash abc123def456 --depth 10
  kgc receipt verify --hash xyz789 --json
```

---

## Rollout Checklist

### Pre-Rollout
- [ ] Add v6-core to kgc-cli manifest
- [ ] Run `pnpm install` to install dependencies
- [ ] Validate spine with `validate-v6-spine.mjs`
- [ ] Run unit tests: `pnpm test --filter @unrdf/v6-core`
- [ ] Run integration tests: `pnpm test --filter @unrdf/kgc-cli`

### Rollout
- [ ] Deploy v6-core commands (receipt, delta)
- [ ] Update documentation
- [ ] Announce new commands to users
- [ ] Monitor error rates

### Post-Rollout
- [ ] Migrate KGC-4D commands (Phase 2)
- [ ] Migrate infrastructure commands (Phase 3)
- [ ] Deprecate old noun names
- [ ] Complete full coverage (45/45 commands)

---

## Support & Troubleshooting

### Common Issues

**Issue**: `ERR_MODULE_NOT_FOUND: Cannot find package 'zod'`
**Solution**: Run `pnpm install --filter @unrdf/v6-core`

**Issue**: `INVALID_COMBINATION: Verb 'X' not applicable to noun 'Y'`
**Solution**: Check noun-verb matrix in `/home/user/unrdf/packages/v6-core/NOUN_VERB_MATRIX.md`

**Issue**: V6 validation fails with unknown noun
**Solution**: Ensure extension uses canonical noun names from `CANONICAL_NOUNS`

### Debug Mode

```bash
# Enable verbose logging
DEBUG=kgc:* kgc receipt verify --hash abc123

# Show spine coverage
node packages/kgc-cli/scripts/validate-v6-spine.mjs

# Dump full registry
kgc package list --json | jq '.data.extensions'
```

---

**Document Version**: 1.0
**Last Updated**: 2025-12-27
**Next Review**: After Phase 2 completion
