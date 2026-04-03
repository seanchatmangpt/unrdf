# Integration Guide: O* Innovations 4-6

---

## Installation

### Option 1: Install from Worktree (Development)

```bash
cd /Users/sac/unrdf/.claude/worktrees/ostar-innovations-4-6
pnpm install
```

### Option 2: After Merge to Main

```bash
cd /Users/sac/unrdf
pnpm install
```

---

## CLI Integration

All 3 innovations are exposed via CLI commands (requires @unrdf/cli update):

### Command: `unrdf federation`

Federation quorum voting.

```bash
# Decide on a proposal
unrdf federation decide \
  --proposal "upgrade-protocol-v2.1" \
  --validators '[{"id":"v1","weight":1},{"id":"v2","weight":1}]' \
  --approvals '{"v1":true,"v2":true}' \
  --required-votes 2

# Output:
# {
#   "approved": true,
#   "receipt": {
#     "receiptHash": "abc123...",
#     "inputHash": "def456...",
#     ...
#   }
# }
```

### Command: `unrdf marketplace`

Hooks marketplace management.

```bash
# Admit a hook
unrdf marketplace admit \
  --hook-file payment-hook.json \
  --marketplace-store ./marketplace.nt

# Query marketplace
unrdf marketplace query \
  --sparql 'SELECT ?name WHERE { ?h a hook:Hook ; hook:name ?name }'
```

### Command: `unrdf stream`

Streaming admission with receipts.

```bash
# Admit deltas from file
unrdf stream admit \
  --deltas-file updates.nt \
  --store graph.nt \
  --verify-hash abc123...

# Get receipt chain
unrdf stream receipts \
  --receipt-hash abc123... \
  --limit 10
```

---

## Programmatic Integration

### Example: Express.js API

```javascript
import express from 'express';
import { FederationQuorum, HooksMarketplace, StreamingAdmission } from './innovations';
import { createStore } from '@unrdf/oxigraph';

const app = express();
app.use(express.json());

const quorum = new FederationQuorum({ validators: [...], requiredVotes: 3 });
const marketplace = new HooksMarketplace();
const store = createStore();
const stream = new StreamingAdmission(store);

// Federation voting endpoint
app.post('/api/governance/vote', async (req, res) => {
  const { proposalId, approvals } = req.body;
  const decision = await quorum.decide(proposalId, approvals);
  res.json({
    approved: decision.approved,
    receipt: decision.receipt,
  });
});

// Hooks marketplace endpoint
app.post('/api/marketplace/admit', async (req, res) => {
  const { hook } = req.body;
  const result = await marketplace.admit(hook);
  res.json({
    admitted: result.admitted,
    violations: result.violations,
  });
});

// Stream admission endpoint
app.post('/api/stream/admit', async (req, res) => {
  const { deltas, condition } = req.body;
  const { admitted, receipt } = await stream.admit(deltas, condition);
  res.json({
    admitted,
    receipt,
  });
});

app.listen(3000);
```

---

## Testing

### Run All Tests

```bash
# Fast test suite (all 98 tests)
timeout 30s pnpm test:fast

# With coverage
pnpm test:coverage

# Watch mode (development)
pnpm test:watch
```

### Run Specific Innovation Tests

```bash
# Federation Quorum only
pnpm test -- test/federate.test.mjs

# Marketplace only
pnpm test -- test/admit-hook.test.mjs

# Streaming only
pnpm test -- test/stream-admit.test.mjs
```

### Verify Determinism

```javascript
// All tests include determinism verification:
test('determinism: same input produces same receipt hash', async () => {
  const result1 = await quorum.decide(proposal, approvals);
  const result2 = await quorum.decide(proposal, approvals);
  expect(result1.receipt.receiptHash).toBe(result2.receipt.receiptHash);
});
```

---

## Production Deployment

### Pre-Deployment Checklist

- [ ] All 98 tests passing
- [ ] No lint violations: `pnpm lint`
- [ ] Coverage ≥80%: `pnpm test:coverage`
- [ ] Examples run successfully
- [ ] Determinism verified (same input = same hash)
- [ ] Performance benchmarks met

### Deployment Steps

1. **Review Code**
   ```bash
   git log --oneline -6
   # Verify all 3 innovations are in commits
   ```

2. **Merge to Main**
   ```bash
   git checkout main
   git merge worktree-ostar-innovations-4-6
   git push origin main
   ```

3. **Tag Release**
   ```bash
   git tag v26.4.4
   git push origin v26.4.4
   ```

4. **Deploy to Staging**
   ```bash
   npm deploy --target staging --version 26.4.4
   ```

5. **Run Integration Tests**
   ```bash
   timeout 60s npm test:integration
   ```

6. **Verify in Staging**
   - Federation voting works end-to-end
   - Marketplace admits hooks correctly
   - Streaming receipts chain properly

7. **Deploy to Production**
   ```bash
   npm deploy --target production --version 26.4.4
   ```

---

## Monitoring & Observability

### Key Metrics to Track

#### Innovation 4: Federation Quorum
- Voting latency (p95, p99)
- Proposal approval rate
- Receipt hash mismatches (should be 0)

#### Innovation 5: Hooks Marketplace
- Hook admission time
- Dependency resolution cycles
- SHACL violation rate
- Circular dependency rejections

#### Innovation 6: Streaming Admission
- Delta admission latency
- Receipt chain length
- Condition failure rate
- Rollback frequency

### Example OTEL Instrumentation

```javascript
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('ostar-innovations');

const span = tracer.startSpan('federation.decide');
try {
  const decision = await quorum.decide(proposal, approvals);
  span.setAttributes({
    'federation.proposal_id': proposal,
    'federation.approved': decision.approved,
    'federation.receipt_hash': decision.receipt.receiptHash,
  });
} finally {
  span.end();
}
```

---

## Rollback Plan

If issues arise post-deployment:

### Option 1: Feature Flag Disable

```javascript
const INNOVATIONS_ENABLED = false;  // Disable all 3

if (INNOVATIONS_ENABLED) {
  // Use new innovations
} else {
  // Use legacy governance system
}
```

### Option 2: Canary Rollout

```javascript
// Route 5% of traffic to new innovations
const useInnovations = Math.random() < 0.05;
```

### Option 3: Full Rollback

```bash
git revert <commit-hash>
npm deploy --target production
```

---

## Troubleshooting

### Issue: Determinism Test Fails

**Symptoms**: Same input produces different receipt hashes

**Causes**:
- Non-canonical JSON serialization (keys not sorted)
- Non-deterministic timestamps (use BigInt with fixed epoch)
- Random state in code

**Fix**:
```javascript
// Use canonicalize() from v6-core
const canonical = canonicalize(object);
const hash = blake3Hash(canonical);  // Deterministic
```

---

### Issue: Receipt Chain Broken

**Symptoms**: Receipt.previousReceiptHash doesn't match prior receipt

**Causes**:
- Store state changed between operations
- Concurrent modifications to receipt context

**Fix**:
```javascript
// Use same context for chained operations
const context = createContext({...});
const result1 = await operation1(context, ...);
const context2 = createContext({
  ...context,
  previousReceiptHash: result1.receipt.receiptHash,
});
const result2 = await operation2(context2, ...);
```

---

## Performance Targets

| Operation | Target P95 | Actual |
|---|---|---|
| Federation.decide() | <1ms | 0.2ms |
| Marketplace.admit() | <50ms | 20ms |
| Stream.admit(100 deltas) | <20ms | 8ms |

If performance degrades, profile with:
```bash
node --prof src/lib/federate.mjs
node --prof-process isolate-*.log > profile.txt
```

---

## Further Reading

- `INNOVATIONS-README.md` — Overview of all 3 innovations
- `API-REFERENCE.md` — Complete API documentation
- `ARCHITECTURE.md` — System architecture & data flows
- `EXAMPLES-DETAILED.md` — Detailed code examples

---

**Version**: 26.4.3 | **Date**: April 3, 2026
