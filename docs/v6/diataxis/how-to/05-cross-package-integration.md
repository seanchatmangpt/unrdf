# How-To: Integrate Cross-Package Workflows

**Time**: ~45 minutes
**Difficulty**: Intermediate
**Prerequisites**: Understanding of [deltas](./02-compose-deltas.md) and [receipts](./03-verify-receipt-chain.md)

---

## What is Cross-Package Integration?

Cross-package integration allows you to compose operations from multiple UNRDF packages while preserving:

- **Receipt continuity**: Full provenance chain across packages
- **Type safety**: Zod schemas validate boundaries
- **Determinism**: Composable operations are deterministic
- **Performance**: Minimal overhead for composition

**Example Use Cases:**
- YAWL workflow + Hooks policy enforcement
- KGC-4D universe freeze + Blockchain anchoring
- Federation query + Streaming results
- Knowledge Engine reasoning + Semantic search

---

## Example 1: YAWL Workflow with Hooks

### Goal
Run a YAWL workflow with pre/post-execution hooks for validation.

### Step 1: Define Workflow

```javascript
import { defineWorkflow } from '@unrdf/yawl';
import { z } from 'zod';

const DataProcessingWorkflowSchema = z.object({
  inputFile: z.string(),
  outputFile: z.string(),
  config: z.object({
    validateInput: z.boolean().default(true),
    validateOutput: z.boolean().default(true)
  }).optional()
});

const dataProcessingWorkflow = defineWorkflow({
  name: 'data-processing',
  schema: DataProcessingWorkflowSchema,
  steps: [
    {
      name: 'load-data',
      handler: async (context) => {
        const data = await fs.readFile(context.inputFile, 'utf-8');
        return { data: JSON.parse(data) };
      }
    },
    {
      name: 'process-data',
      handler: async (context) => {
        const processed = context.data.map(item => ({
          ...item,
          processed: true,
          timestamp: context.timestamp
        }));
        return { data: processed };
      }
    },
    {
      name: 'save-data',
      handler: async (context) => {
        await fs.writeFile(context.outputFile, JSON.stringify(context.data, null, 2));
        return { success: true };
      }
    }
  ]
});
```

### Step 2: Define Hooks

```javascript
import { defineHook } from '@unrdf/hooks';

const preExecutionHook = defineHook({
  name: 'validate-input',
  schema: z.object({
    inputFile: z.string(),
    data: z.unknown()
  }),
  receipt: true,
  handler: async (context) => {
    console.log('🔍 Pre-execution validation...');

    const InputDataSchema = z.array(z.object({
      id: z.string(),
      value: z.number()
    }));

    try {
      InputDataSchema.parse(context.data);
      console.log('✅ Input validation passed');
      return { valid: true };
    } catch (error) {
      console.error('❌ Input validation failed:', error.message);
      throw error;
    }
  }
});

const postExecutionHook = defineHook({
  name: 'validate-output',
  schema: z.object({
    outputFile: z.string(),
    data: z.unknown()
  }),
  receipt: true,
  handler: async (context) => {
    console.log('🔍 Post-execution validation...');

    const OutputDataSchema = z.array(z.object({
      id: z.string(),
      value: z.number(),
      processed: z.literal(true),
      timestamp: z.number()
    }));

    try {
      OutputDataSchema.parse(context.data);
      console.log('✅ Output validation passed');
      return { valid: true };
    } catch (error) {
      console.error('❌ Output validation failed:', error.message);
      throw error;
    }
  }
});
```

### Step 3: Integrate Workflow + Hooks

```javascript
import { withReceipt } from '@unrdf/v6-core/receipts';
import { activateHook } from '@unrdf/hooks';

const runWorkflowWithHooks = withReceipt(async function runWorkflowWithHooks(input) {
  const receipts = [];

  // Load data
  const data = await fs.readFile(input.inputFile, 'utf-8');
  const parsedData = JSON.parse(data);

  // Pre-execution hook
  if (input.config?.validateInput) {
    const { receipt: preReceipt } = await activateHook(preExecutionHook, {
      inputFile: input.inputFile,
      data: parsedData
    });
    receipts.push(preReceipt);
    console.log('Pre-hook receipt:', preReceipt.hash);
  }

  // Execute workflow
  const { receipt: workflowReceipt, result } = await dataProcessingWorkflow.execute({
    ...input,
    data: parsedData,
    timestamp: Date.now()
  });
  receipts.push(workflowReceipt);
  console.log('Workflow receipt:', workflowReceipt.hash);

  // Post-execution hook
  if (input.config?.validateOutput) {
    const { receipt: postReceipt } = await activateHook(postExecutionHook, {
      outputFile: input.outputFile,
      data: result.data
    });
    receipts.push(postReceipt);
    console.log('Post-hook receipt:', postReceipt.hash);
  }

  return {
    result,
    receipts
  };
});

// Usage
const { result, receipts } = await runWorkflowWithHooks({
  inputFile: 'data/input.json',
  outputFile: 'data/output.json',
  config: {
    validateInput: true,
    validateOutput: true
  }
});

console.log('✅ Workflow complete!');
console.log('Receipt chain:', receipts.map(r => r.hash));
```

**Receipt Chain:**
```
pre-hook → workflow → post-hook
```

---

## Example 2: KGC-4D + Blockchain Anchoring

### Goal
Freeze a universe snapshot and anchor it to Ethereum for immutability.

### Step 1: Freeze Universe

```javascript
import { freeze } from '@unrdf/kgc-4d';

const { receipt: freezeReceipt } = await freeze({
  universeId: 'universe-abc123',
  description: 'Production snapshot before deployment'
});

console.log('Freeze receipt:', freezeReceipt.hash);
console.log('Snapshot hash:', freezeReceipt.outputs.snapshotHash);
```

### Step 2: Anchor to Blockchain

```javascript
import { anchor } from '@unrdf/blockchain';

const { receipt: anchorReceipt } = await anchor({
  merkleRoot: freezeReceipt.outputs.snapshotHash,
  blockchain: 'ethereum',
  network: 'mainnet'
});

console.log('Anchor receipt:', anchorReceipt.hash);
console.log('Ethereum TX:', anchorReceipt.outputs.transactionHash);
console.log('Block number:', anchorReceipt.outputs.blockNumber);
```

### Step 3: Verify Anchored Snapshot

```javascript
import { verifyAnchor } from '@unrdf/blockchain';

const { valid, proof } = await verifyAnchor({
  merkleRoot: freezeReceipt.outputs.snapshotHash,
  blockchain: 'ethereum',
  transactionHash: anchorReceipt.outputs.transactionHash
});

if (valid) {
  console.log('✅ Snapshot verified on-chain!');
  console.log('Block timestamp:', proof.blockTimestamp);
  console.log('Confirmations:', proof.confirmations);
} else {
  console.error('❌ Verification failed!');
}
```

**Receipt Chain:**
```
freeze → anchor → verify
```

---

## Example 3: Federation + Streaming

### Goal
Execute a federated SPARQL query and stream results to multiple consumers.

### Step 1: Execute Federated Query

```javascript
import { sparql, executeFederatedQuery } from '@unrdf/federation';
import { createStream } from '@unrdf/streaming';

const query = sparql`
  SELECT ?person ?name ?org
  WHERE {
    ?person rdf:type foaf:Person .
    ?person foaf:name ?name .
    ?person foaf:member ?org .
  }
`
  .timeout(10000)
  .receipt(true);

const { stream, receipt: queryReceipt } = await executeFederatedQuery({
  query,
  endpoints: [
    'http://dbpedia.org/sparql',
    'http://example.org/sparql'
  ]
});

console.log('Query receipt:', queryReceipt.hash);
```

### Step 2: Stream Results

```javascript
const processedStream = createStream(async function* processResults(source) {
  for await (const result of source) {
    // Transform result
    const transformed = {
      person: result.person.value,
      name: result.name.value,
      organization: result.org.value,
      timestamp: Date.now()
    };

    yield transformed;
  }
});

const { stream: finalStream, receipt: streamReceipt } = await processedStream(stream);

// Consume stream
for await (const item of finalStream) {
  console.log('Result:', item);
}

console.log('Stream receipt:', streamReceipt.hash);
```

**Receipt Chain:**
```
query → stream → consume
```

---

## Example 4: Knowledge Engine + Semantic Search

### Goal
Perform reasoning on a knowledge graph, then search results semantically.

### Step 1: Execute Reasoning

```javascript
import { reason } from '@unrdf/knowledge-engine';

const { receipt: reasoningReceipt, inferences } = await reason({
  universeId: 'universe-abc123',
  rules: [
    {
      name: 'transitivity',
      rule: `
        ?x rdfs:subClassOf ?y .
        ?y rdfs:subClassOf ?z .
        => ?x rdfs:subClassOf ?z .
      `
    }
  ]
});

console.log('Reasoning receipt:', reasoningReceipt.hash);
console.log('Inferences:', inferences.length);
```

### Step 2: Index for Semantic Search

```javascript
import { indexDocuments } from '@unrdf/semantic-search';

const { receipt: indexReceipt } = await indexDocuments({
  documents: inferences.map(inf => ({
    id: inf.id,
    text: `${inf.subject} ${inf.predicate} ${inf.object}`,
    metadata: {
      type: 'inference',
      confidence: inf.confidence
    }
  }))
});

console.log('Index receipt:', indexReceipt.hash);
```

### Step 3: Search

```javascript
import { search } from '@unrdf/semantic-search';

const { receipt: searchReceipt, results } = await search({
  query: 'subclass relationships',
  limit: 10,
  threshold: 0.8
});

console.log('Search receipt:', searchReceipt.hash);
console.log('Results:', results.length);

results.forEach(result => {
  console.log(`Score: ${result.score}, Text: ${result.text}`);
});
```

**Receipt Chain:**
```
reason → index → search
```

---

## Cross-Package Receipt Verification

Verify entire cross-package chain:

```bash
npx kgc receipt chain --hash sha256:final-receipt-hash --verify --cross-package
```

**Output:**
```
Receipt Chain (cross-package):
  1. sha256:hash1 (@unrdf/kgc-4d - freeze)
  2. sha256:hash2 (@unrdf/blockchain - anchor)
  3. sha256:hash3 (@unrdf/blockchain - verify)

✅ Cross-package chain verified
  - Packages: @unrdf/kgc-4d, @unrdf/blockchain
  - Total operations: 3
  - Chain depth: 3
  - All receipts valid
```

---

## Programmatic Cross-Package Composition

For reusable integrations, create a composition function:

```javascript
import { withReceipt } from '@unrdf/v6-core/receipts';
import { freeze } from '@unrdf/kgc-4d';
import { anchor } from '@unrdf/blockchain';

export const freezeAndAnchor = withReceipt(async function freezeAndAnchor(universeId, blockchain = 'ethereum') {
  // Freeze universe
  const { receipt: freezeReceipt, snapshot } = await freeze({ universeId });

  // Anchor to blockchain
  const { receipt: anchorReceipt } = await anchor({
    merkleRoot: snapshot.hash,
    blockchain
  });

  // Return composite receipt
  return {
    snapshot,
    receipts: [freezeReceipt, anchorReceipt],
    chainHash: anchorReceipt.hash
  };
});

// Usage
const { snapshot, receipts, chainHash } = await freezeAndAnchor('universe-abc123');
console.log('Composite operation complete!');
console.log('Receipt chain hash:', chainHash);
```

---

## Best Practices

1. **Validate at boundaries**: Use Zod schemas when data crosses package boundaries
2. **Preserve receipts**: Always chain receipts across packages
3. **Handle errors gracefully**: Use try-catch at integration points
4. **Test integrations**: Write integration tests for all cross-package flows
5. **Document dependencies**: Specify which packages are required

### Example: Boundary Validation

```javascript
import { z } from 'zod';

// Define schema at package boundary
const YAWLtoHooksSchema = z.object({
  workflowId: z.string().uuid(),
  status: z.enum(['pending', 'running', 'completed', 'failed']),
  context: z.unknown()
});

// Validate when passing data from YAWL to Hooks
const workflowData = YAWLtoHooksSchema.parse(workflowResult);
const { receipt } = await activateHook(hook, workflowData);
```

---

## Troubleshooting

### "Receipt chain broken - parent not found"

**Cause**: Intermediate receipt not preserved

**Solution**: Ensure all receipts are collected:
```javascript
const receipts = [];
const { receipt: r1 } = await operation1();
receipts.push(r1);

const { receipt: r2 } = await operation2();
receipts.push(r2);

// Save all receipts
await saveReceipts(receipts);
```

### "Cross-package type mismatch"

**Cause**: Schema incompatibility between packages

**Solution**: Use adapter:
```javascript
import { adaptSchema } from '@unrdf/v6-compat/adapters';

const adapted = adaptSchema(package1Output, Package2InputSchema);
const { receipt } = await package2Operation(adapted);
```

### "Performance degradation in cross-package workflows"

**Cause**: Too many sequential operations

**Solution**: Parallelize independent operations:
```javascript
// ❌ Sequential
const r1 = await op1();
const r2 = await op2();
const r3 = await op3();

// ✅ Parallel (if independent)
const [r1, r2, r3] = await Promise.all([op1(), op2(), op3()]);
```

---

## Summary

✅ Cross-package integration preserves receipt chains
✅ Validate at package boundaries with Zod
✅ Compose operations programmatically
✅ Verify entire cross-package chains
✅ Test integrations thoroughly
✅ Handle errors at integration points

**Next**: [Reference: CLI Command Matrix](../reference/01-cli-command-matrix.md)
