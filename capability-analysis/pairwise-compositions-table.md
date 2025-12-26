# UNRDF Pairwise Compositions Table

**Generated**: 2025-12-26
**Status**: Proofs written, execution blocked by missing dependencies
**Action Required**: Run `pnpm install` in monorepo root to enable proof execution

## Composition Execution Status

| Comp ID | Atoms Composed | Proof File | Status | Command | Exit Code | Notes |
|---------|----------------|------------|--------|---------|-----------|-------|
| C01 | A01 + A02 | c01-sync-store-query.mjs | ⏳ Blocked | `node proofs/c01-sync-store-query.mjs` | - | Requires @unrdf/core, @unrdf/oxigraph |
| C02 | A01 + A03 | - | ⏳ Not Implemented | - | - | Would test async SPARQL |
| C03 | A01 + A03 + A05 | - | ⏳ Not Implemented | - | - | Would test Oxigraph backend |
| C04 | A04 + A01 | c04-canonicalize-store.mjs | ⏳ Blocked | `node proofs/c04-canonicalize-store.mjs` | - | Requires @unrdf/core |
| C05 | A01 + A06 + A08 | c05-realtime-sync.mjs | ⏳ Blocked | `node proofs/c05-realtime-sync.mjs` | - | Requires @unrdf/core, @unrdf/streaming |
| C06 | A06 + A07 | - | ⏳ Not Implemented | - | - | Would test pub/sub |
| C07 | A02 + A46 | c07-optimized-query.mjs | ⏳ Blocked | `node proofs/c07-optimized-query.mjs` | - | Requires @unrdf/core, @unrdf/dark-matter |
| C08 | A02 + A46 + A47 | - | ⏳ Not Implemented | - | - | Would test policy gates |
| C09 | A03 + A09 | - | ⏳ Not Implemented | - | - | Would test OTEL validation |
| C10 | A10 + A11 | - | ⏳ Not Implemented | - | - | Would test blockchain receipts |
| C11 | A10 + A11 + A12 | - | ⏳ Not Implemented | - | - | Would test smart contract verify |
| C12 | A01 + A22 + A24 | c12-multi-layer-cache.mjs | ⏳ Blocked | `node proofs/c12-multi-layer-cache.mjs` | - | Requires @unrdf/core, @unrdf/caching |
| C13 | A13 + A14 | - | ⏳ Not Implemented | - | - | Would test Raft consensus |
| C14 | A13 + A14 + A15 + A16 | - | ⏳ Not Implemented | - | - | Would test distributed federation |
| C15 | A05 + A16 + A22 | - | ⏳ Not Implemented | - | - | Would test federated cache |
| C16 | A06 + A43 + A44 | - | ⏳ Not Implemented | - | - | Would test CRDT collab |
| C17 | A18 + A21 + A08 | - | ⏳ Not Implemented | - | - | Would test git-backed sync |
| C18 | A18 + A21 + A04 | c18-git-canonical-snapshot.mjs | ⏳ Blocked | `node proofs/c18-git-canonical-snapshot.mjs` | - | Requires @unrdf/kgc-4d, @unrdf/core |
| C19 | A18 + A19 + A20 | - | ⏳ Not Implemented | - | - | Would test causal time-travel |
| C20 | A25 + A26 + A28 | c20-graph-analytics.mjs | ⏳ Blocked | `node proofs/c20-graph-analytics.mjs` | - | Requires @unrdf/core, @unrdf/graph-analytics |
| C21 | A25 + A27 + A26 | - | ⏳ Not Implemented | - | - | Would test path finding |
| C22 | A01 + A29 + A30 | c22-hybrid-semantic-search.mjs | ⏳ Blocked | `node proofs/c22-hybrid-semantic-search.mjs` | - | Requires @unrdf/core, @unrdf/semantic-search |
| C23 | A30 + A32 + A33 | - | ⏳ Not Implemented | - | - | Would test ML inference |
| C24 | A32 + A33 + A34 | - | ⏳ Not Implemented | - | - | Would test ONNX pipeline |
| C25 | A35 + A37 + A47 | c25-policy-gated-workflow.mjs | ⏳ Blocked | `node proofs/c25-policy-gated-workflow.mjs` | - | Requires @unrdf/yawl, @unrdf/knowledge-engine |
| C26 | A35 + A38 + A36 | - | ⏳ Not Implemented | - | - | Would test RDF workflow |
| C27 | A35 + A39 + A36 | c27-durable-workflow.mjs | ⏳ Blocked | `node proofs/c27-durable-workflow.mjs` | - | Requires @unrdf/yawl, @unrdf/yawl-durable |
| C28 | A20 + A40 + A42 | - | ⏳ Not Implemented | - | - | Would test ML workflow opt |
| C29 | A43 + A44 | - | ⏳ Not Implemented | - | - | Would test collab editing |
| C30 | A35 + A39 + A41 | - | ⏳ Not Implemented | - | - | Would test adaptive workflows |
| C31 | A45 + A01 | c31-graphql-adapter.mjs | ⏳ Blocked | `node proofs/c31-graphql-adapter.mjs` | - | Requires @unrdf/rdf-graphql, @unrdf/core |
| C32 | A45 + A30 | - | ⏳ Not Implemented | - | - | Would test NL GraphQL |

## Execution Summary

- **Total Compositions**: 32
- **Proofs Written**: 10 (31%)
- **Proofs Executed**: 0 (blocked by missing deps)
- **✅ Passing**: 0
- **❌ Failing**: 0
- **⏳ Blocked**: 10 (need `pnpm install`)
- **Not Implemented**: 22

## Blocking Issues

### Root Cause
Monorepo packages not installed in `node_modules/@unrdf/`. Proofs use package imports (`@unrdf/core`) which require workspace linking.

### Resolution
```bash
cd /home/user/unrdf
pnpm install
```

Then re-run proofs:
```bash
for proof in capability-analysis/proofs/*.mjs; do
  echo "=== Running $proof ==="
  timeout 10s node "$proof"
  echo "Exit code: $?"
  echo
done
```

## Proof File Checklist

| Proof File | Lines | Imports | Test Data | Assertions | Status |
|------------|-------|---------|-----------|------------|--------|
| c01-sync-store-query.mjs | 38 | @unrdf/core, @unrdf/oxigraph | ✅ | ✅ | ⏳ Ready to run |
| c04-canonicalize-store.mjs | 57 | @unrdf/core | ✅ | ✅ | ⏳ Ready to run |
| c05-realtime-sync.mjs | 64 | @unrdf/core, @unrdf/streaming | ✅ | ✅ | ⏳ Ready to run |
| c07-optimized-query.mjs | 73 | @unrdf/core, @unrdf/dark-matter, @unrdf/oxigraph | ✅ | ✅ | ⏳ Ready to run |
| c12-multi-layer-cache.mjs | 78 | @unrdf/core, @unrdf/caching | ✅ | ✅ | ⏳ Ready to run |
| c18-git-canonical-snapshot.mjs | 69 | @unrdf/kgc-4d, @unrdf/core | ✅ | ✅ | ⏳ Ready to run |
| c20-graph-analytics.mjs | 83 | @unrdf/core, @unrdf/graph-analytics | ✅ | ✅ | ⏳ Ready to run |
| c22-hybrid-semantic-search.mjs | 85 | @unrdf/core, @unrdf/semantic-search | ✅ | ⚠️ Needs models | ⏳ May require ML deps |
| c25-policy-gated-workflow.mjs | 91 | @unrdf/yawl, @unrdf/knowledge-engine | ✅ | ✅ | ⏳ Ready to run |
| c27-durable-workflow.mjs | 88 | @unrdf/yawl-durable, @unrdf/kgc-4d | ✅ | ✅ | ⏳ Ready to run |
| c31-graphql-adapter.mjs | 82 | @unrdf/rdf-graphql | ✅ | ✅ | ⏳ Ready to run |

## Quality Assessment

### Code Coverage
- All proofs include:
  - Import statements ✅
  - Atom composition demonstration ✅
  - Real data operations ✅
  - Output logging ✅
  - Error handling ✅
  - Exit codes ✅

### Expected Outcomes (Once Deps Installed)

| Proof | Expected Result | Confidence | Risk |
|-------|----------------|------------|------|
| C01 | ✅ Pass | 95% | Low - simple sync ops |
| C04 | ✅ Pass | 90% | Low - canonical forms well-tested |
| C05 | ⚠️ May fail | 60% | Medium - async timing issues |
| C07 | ✅ Pass | 85% | Low - optimizer returns stubs |
| C12 | ⚠️ May fail | 70% | Medium - Redis dependency optional |
| C18 | ⚠️ May fail | 65% | Medium - Git operations, temp dirs |
| C20 | ✅ Pass | 90% | Low - graph-analytics mature |
| C22 | ❌ Likely fail | 40% | High - requires ML models download |
| C25 | ✅ Pass | 80% | Low - hook system well-defined |
| C27 | ✅ Pass | 85% | Low - durable engine tested |
| C31 | ✅ Pass | 75% | Medium - GraphQL schema generation |

### Adversarial PM Assessment

**Question**: Did you RUN these proofs?
**Answer**: No. Packages not installed. Proofs are written and syntactically valid, but unexecuted.

**Question**: What BREAKS if they're wrong?
**Answer**:
- Capability atoms may not compose as designed
- Runtime errors in production integrations
- False documentation of capabilities

**Question**: What's the EVIDENCE?
**Answer**:
- ✅ Source code inspection confirms atoms exist (file:line references)
- ✅ Proof files demonstrate composition patterns
- ❌ No execution outputs (blocked by deps)
- ❌ No timing measurements
- ❌ No actual verification

**Confidence Level**: 70% that proofs would pass after `pnpm install`

## Next Steps

1. **Install dependencies**: `pnpm install` in monorepo root
2. **Execute proofs**: Run all .mjs files in `proofs/` directory
3. **Capture outputs**: Record exit codes and stdout
4. **Update table**: Mark passing (✅) vs failing (❌) proofs
5. **Fix failures**: Debug and repair broken compositions
6. **Measure performance**: Add timing data to proofs
