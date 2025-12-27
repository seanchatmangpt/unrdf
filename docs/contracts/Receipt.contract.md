# Receipt Contract

## Purpose

Universal receipt format for all deterministic operations in the KGC system. Every state transition produces a verifiable receipt.

## Format

```typescript
interface Receipt {
  // Identity
  id: string; // UUID v4
  agent_id: string; // "agent-0" | "agent-1" | ... | "agent-9"
  phase: 'tranche' | 'reconcile'; // Execution phase

  // Temporal
  timestamp_ns: bigint; // Nanosecond precision
  timestamp_iso: string; // ISO 8601 for human readability

  // State hashes
  before_hash: string; // BLAKE3 of input state
  after_hash: string; // BLAKE3 of output state
  receipt_hash: string; // BLAKE3 of receipt contents

  // Versioning
  toolchain_version: string; // "kgc-claude@5.0.0"
  node_version: string; // process.version

  // Artifacts
  artifacts: Artifact[]; // Files/outputs produced
  proof_artifacts: string[]; // Paths to verification scripts

  // Deltas
  deltas: {
    deltaO: Delta[]; // Ontology changes
    deltaPi: Delta[]; // Projection changes
    deltaLambda: Delta[]; // Law changes
    deltaQ: Delta[]; // Invariant changes
  };

  // Execution trace
  tool_trace: ToolCall[]; // Tool execution log
  test_log_path?: string; // Path to test output

  // Chain
  previous_receipt_hash: string | null; // Chain to previous receipt

  // Verification
  status: 'success' | 'failed';
  error?: string; // Error message if failed
}

interface Artifact {
  id: string;
  type: 'file' | 'edit' | 'create' | 'delete' | 'command';
  path: string;
  content_hash?: string; // BLAKE3 of file contents
  metadata?: Record<string, unknown>;
}

interface ToolCall {
  id: string;
  name: string;
  input: Record<string, unknown>;
  output?: unknown;
  start_time: bigint;
  end_time?: bigint;
  status: 'pending' | 'success' | 'error';
  error?: string;
}

interface Delta {
  type: 'add' | 'delete' | 'modify';
  target: string; // URI or file path
  before?: unknown;
  after?: unknown;
  hash?: string;
}
```

## Verification Function

```typescript
function verify(receipt: Receipt): boolean {
  // 1. Validate schema
  if (!ReceiptSchema.safeParse(receipt).success) return false;

  // 2. Recompute receipt hash
  const hashContent = {
    id: receipt.id,
    agent_id: receipt.agent_id,
    timestamp_ns: receipt.timestamp_ns.toString(),
    before_hash: receipt.before_hash,
    after_hash: receipt.after_hash,
    deltas: receipt.deltas,
    artifacts: receipt.artifacts,
  };
  const recomputed = BLAKE3(JSON.stringify(hashContent));
  if (recomputed !== receipt.receipt_hash) return false;

  // 3. Verify chain
  if (receipt.previous_receipt_hash) {
    const previous = findReceipt(receipt.previous_receipt_hash);
    if (!previous || previous.after_hash !== receipt.before_hash) {
      return false;
    }
  }

  // 4. Verify artifacts
  for (const artifact of receipt.artifacts) {
    if (artifact.content_hash) {
      const actualHash = BLAKE3(readFile(artifact.path));
      if (actualHash !== artifact.content_hash) return false;
    }
  }

  return true;
}
```

## Deterministic Serialization

All receipt hashing uses canonical JSON serialization:

```javascript
function deterministicSerialize(obj) {
  if (typeof obj !== 'object' || obj === null) {
    return JSON.stringify(obj);
  }
  if (Array.isArray(obj)) {
    return `[${obj.map(deterministicSerialize).join(',')}]`;
  }
  const sortedKeys = Object.keys(obj).sort();
  const pairs = sortedKeys.map(k => `${JSON.stringify(k)}:${deterministicSerialize(obj[k])}`);
  return `{${pairs.join(',')}}`;
}
```

## Receipt Chain Properties

### 1. Temporal Ordering

```
receipt[i].timestamp_ns < receipt[i+1].timestamp_ns
```

### 2. Hash Continuity

```
receipt[i].after_hash === receipt[i+1].before_hash
```

### 3. Agent Isolation (in tranche phase)

```
agent_id(receipt[i]) ≠ agent_id(receipt[j]) ⟹
  artifacts(receipt[i]) ∩ artifacts(receipt[j]) = ∅
```

### 4. Idempotent Replay

```
replay(receipt[i]) produces identical after_hash
```

## Usage Example

```javascript
import { createReceipt } from './receipt-builder.mjs';

const receipt = await createReceipt({
  agentId: 'agent-1',
  phase: 'tranche',
  beforeHash: previousSnapshot.hash,
  deltas: { deltaO, deltaPi, deltaLambda, deltaQ },
  artifacts: [{ type: 'file', path: '/path/to/output.mjs' }],
  toolTrace: [...],
});

// Verify receipt
const valid = verify(receipt);
assert(valid === true);

// Write to disk
fs.writeFileSync('RECEIPT.json', JSON.stringify(receipt, null, 2));
```

## Storage Location

- Tranche receipts: `/packages/kgc-claude/tranches/agent-{N}/RECEIPT.json`
- Global receipt: `/packages/kgc-claude/GLOBAL_RECEIPT.json`
- Validation log: `/packages/kgc-claude/tranche_validation.json`

## Testing Contract

```javascript
// Test 1: Hash stability
const r1 = createReceipt(input);
const r2 = createReceipt(input);
assert(r1.receipt_hash === r2.receipt_hash);

// Test 2: Chain integrity
const chain = [receipt0, receipt1, receipt2];
assert(verifyChain(chain) === true);

// Test 3: Artifact verification
const receipt = createReceipt({ artifacts: [{ path: 'foo.mjs' }] });
assert(fs.existsSync('foo.mjs'));
assert(BLAKE3(readFile('foo.mjs')) === receipt.artifacts[0].content_hash);
```
