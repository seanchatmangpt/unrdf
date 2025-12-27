# Example: YAWL Workflow with Hooks Integration

**Use Case**: Data processing workflow with pre/post validation hooks
**Packages**: `@unrdf/yawl`, `@unrdf/hooks`, `@unrdf/v6-core`
**Complexity**: Intermediate
**Time to Complete**: ~30 minutes

---

## Overview

This example demonstrates a **real-world integration** of YAWL workflows with Hooks for governance:

1. **Pre-execution hook**: Validates input data before workflow starts
2. **Workflow execution**: Processes data through 3 steps
3. **Post-execution hook**: Validates output data after workflow completes
4. **Receipt chain**: Full provenance from start to finish

**Result**: Deterministic, auditable, production-ready data pipeline.

---

## Prerequisites

```bash
pnpm add @unrdf/yawl @unrdf/hooks @unrdf/v6-core @unrdf/oxigraph @unrdf/kgc-4d zod
```

---

## Step 1: Create Sample Data

Create `data/input.json`:

```json
[
  { "id": "user-001", "value": 100 },
  { "id": "user-002", "value": 250 },
  { "id": "user-003", "value": 175 }
]
```

---

## Step 2: Define Zod Schemas

Create `src/schemas.mjs`:

```javascript
import { z } from 'zod';

// Input schema
export const InputRecordSchema = z.object({
  id: z.string().regex(/^user-\d{3}$/),
  value: z.number().positive()
});

export const InputDataSchema = z.array(InputRecordSchema);

// Output schema (adds 'processed' flag and timestamp)
export const OutputRecordSchema = InputRecordSchema.extend({
  processed: z.literal(true),
  timestamp: z.number().int().positive()
});

export const OutputDataSchema = z.array(OutputRecordSchema);

// Workflow context schema
export const WorkflowContextSchema = z.object({
  inputFile: z.string(),
  outputFile: z.string(),
  data: z.unknown().optional(),
  timestamp: z.number().int().positive(),
  config: z.object({
    validateInput: z.boolean().default(true),
    validateOutput: z.boolean().default(true)
  }).optional()
});
```

---

## Step 3: Define Hooks

Create `src/hooks.mjs`:

```javascript
import { defineHook } from '@unrdf/hooks';
import { InputDataSchema, OutputDataSchema } from './schemas.mjs';
import fs from 'node:fs/promises';

/**
 * Pre-execution hook: Validates input data before workflow starts
 */
export const validateInputHook = defineHook({
  name: 'validate-input',
  description: 'Validate input data against schema',
  schema: WorkflowContextSchema.pick({ inputFile: true, data: true }),
  receipt: true,
  handler: async (context) => {
    console.log('🔍 [PRE-HOOK] Validating input data...');

    try {
      // Validate against schema
      const validatedData = InputDataSchema.parse(context.data);

      // Additional business logic validation
      const duplicateIds = validatedData
        .map(r => r.id)
        .filter((id, index, arr) => arr.indexOf(id) !== index);

      if (duplicateIds.length > 0) {
        throw new Error(`Duplicate IDs found: ${duplicateIds.join(', ')}`);
      }

      console.log(`✅ [PRE-HOOK] Input validation passed (${validatedData.length} records)`);
      return { valid: true, recordCount: validatedData.length };
    } catch (error) {
      console.error('❌ [PRE-HOOK] Input validation failed:', error.message);
      throw error;
    }
  }
});

/**
 * Post-execution hook: Validates output data after workflow completes
 */
export const validateOutputHook = defineHook({
  name: 'validate-output',
  description: 'Validate output data against schema',
  schema: WorkflowContextSchema.pick({ outputFile: true, data: true }),
  receipt: true,
  handler: async (context) => {
    console.log('🔍 [POST-HOOK] Validating output data...');

    try {
      // Validate against schema
      const validatedData = OutputDataSchema.parse(context.data);

      // Verify all records have 'processed' flag
      const unprocessed = validatedData.filter(r => !r.processed);
      if (unprocessed.length > 0) {
        throw new Error(`Unprocessed records found: ${unprocessed.length}`);
      }

      // Verify timestamps are recent (within 1 minute)
      const now = Date.now();
      const staleRecords = validatedData.filter(r => (now - r.timestamp) > 60000);
      if (staleRecords.length > 0) {
        throw new Error(`Stale timestamps found: ${staleRecords.length} records`);
      }

      console.log(`✅ [POST-HOOK] Output validation passed (${validatedData.length} records)`);
      return { valid: true, recordCount: validatedData.length };
    } catch (error) {
      console.error('❌ [POST-HOOK] Output validation failed:', error.message);
      throw error;
    }
  }
});
```

---

## Step 4: Define Workflow

Create `src/workflow.mjs`:

```javascript
import { defineWorkflow } from '@unrdf/yawl';
import { WorkflowContextSchema } from './schemas.mjs';
import fs from 'node:fs/promises';

export const dataProcessingWorkflow = defineWorkflow({
  name: 'data-processing',
  description: 'Process data with validation hooks',
  schema: WorkflowContextSchema,
  receipt: true,
  steps: [
    {
      name: 'load-data',
      description: 'Load data from input file',
      handler: async (context) => {
        console.log(`📂 [STEP 1] Loading data from ${context.inputFile}...`);

        const data = await fs.readFile(context.inputFile, 'utf-8');
        const parsedData = JSON.parse(data);

        console.log(`✅ [STEP 1] Loaded ${parsedData.length} records`);
        return { ...context, data: parsedData };
      }
    },
    {
      name: 'process-data',
      description: 'Transform and enrich data',
      handler: async (context) => {
        console.log(`⚙️  [STEP 2] Processing ${context.data.length} records...`);

        const processed = context.data.map(item => ({
          ...item,
          processed: true,
          timestamp: context.timestamp
        }));

        console.log(`✅ [STEP 2] Processed ${processed.length} records`);
        return { ...context, data: processed };
      }
    },
    {
      name: 'save-data',
      description: 'Save processed data to output file',
      handler: async (context) => {
        console.log(`💾 [STEP 3] Saving data to ${context.outputFile}...`);

        await fs.writeFile(
          context.outputFile,
          JSON.stringify(context.data, null, 2),
          'utf-8'
        );

        console.log(`✅ [STEP 3] Saved ${context.data.length} records`);
        return { ...context, success: true };
      }
    }
  ]
});
```

---

## Step 5: Integrate Workflow + Hooks

Create `src/index.mjs`:

```javascript
import { withReceipt } from '@unrdf/v6-core/receipts';
import { activateHook } from '@unrdf/hooks';
import { validateInputHook, validateOutputHook } from './hooks.mjs';
import { dataProcessingWorkflow } from './workflow.mjs';
import fs from 'node:fs/promises';

/**
 * Run workflow with pre/post hooks
 */
export const runWorkflowWithHooks = withReceipt(async function runWorkflowWithHooks(config) {
  console.log('🚀 Starting workflow with hooks...\n');

  const receipts = [];
  const startTime = Date.now();

  try {
    // Load input data
    const rawData = await fs.readFile(config.inputFile, 'utf-8');
    const inputData = JSON.parse(rawData);

    // Step 1: Pre-execution hook (validate input)
    if (config.config?.validateInput !== false) {
      const { receipt: preReceipt } = await activateHook(validateInputHook, {
        inputFile: config.inputFile,
        data: inputData
      });
      receipts.push(preReceipt);
      console.log(`📋 Pre-hook receipt: ${preReceipt.hash}\n`);
    }

    // Step 2: Execute workflow
    const { receipt: workflowReceipt, result } = await dataProcessingWorkflow.execute({
      ...config,
      data: inputData,
      timestamp: startTime
    });
    receipts.push(workflowReceipt);
    console.log(`📋 Workflow receipt: ${workflowReceipt.hash}\n`);

    // Step 3: Post-execution hook (validate output)
    if (config.config?.validateOutput !== false) {
      const outputData = JSON.parse(await fs.readFile(config.outputFile, 'utf-8'));

      const { receipt: postReceipt } = await activateHook(validateOutputHook, {
        outputFile: config.outputFile,
        data: outputData
      });
      receipts.push(postReceipt);
      console.log(`📋 Post-hook receipt: ${postReceipt.hash}\n`);
    }

    const endTime = Date.now();

    console.log('✅ Workflow completed successfully!');
    console.log(`⏱️  Total time: ${endTime - startTime}ms`);
    console.log(`📋 Receipt chain (${receipts.length} receipts):`);
    receipts.forEach((r, i) => {
      console.log(`  ${i + 1}. ${r.hash} (${r.operation})`);
    });

    return {
      success: true,
      receipts,
      duration: endTime - startTime,
      result
    };
  } catch (error) {
    console.error('❌ Workflow failed:', error.message);
    throw error;
  }
});

// Execute if run directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const { value, receipt } = await runWorkflowWithHooks({
    inputFile: 'data/input.json',
    outputFile: 'data/output.json',
    config: {
      validateInput: true,
      validateOutput: true
    }
  });

  console.log('\n📋 Final receipt:', receipt.hash);
  console.log('✅ All operations complete!');
}
```

---

## Step 6: Run the Example

```bash
node src/index.mjs
```

**Expected Output:**
```
🚀 Starting workflow with hooks...

🔍 [PRE-HOOK] Validating input data...
✅ [PRE-HOOK] Input validation passed (3 records)
📋 Pre-hook receipt: sha256:abc123...

📂 [STEP 1] Loading data from data/input.json...
✅ [STEP 1] Loaded 3 records
⚙️  [STEP 2] Processing 3 records...
✅ [STEP 2] Processed 3 records
💾 [STEP 3] Saving data to data/output.json...
✅ [STEP 3] Saved 3 records
📋 Workflow receipt: sha256:def456...

🔍 [POST-HOOK] Validating output data...
✅ [POST-HOOK] Output validation passed (3 records)
📋 Post-hook receipt: sha256:ghi789...

✅ Workflow completed successfully!
⏱️  Total time: 234ms
📋 Receipt chain (3 receipts):
  1. sha256:abc123... (validate-input)
  2. sha256:def456... (data-processing)
  3. sha256:ghi789... (validate-output)

📋 Final receipt: sha256:jkl012...
✅ All operations complete!
```

---

## Step 7: Verify Receipt Chain

```bash
# Verify entire receipt chain
npx kgc receipt chain --hash sha256:jkl012... --verify
```

**Output:**
```
Receipt Chain (depth 3):
  1. sha256:abc123... (validate-input) - @unrdf/hooks
  2. sha256:def456... (data-processing) - @unrdf/yawl
  3. sha256:ghi789... (validate-output) - @unrdf/hooks

Verification:
  ✅ All hashes verified
  ✅ All parent links valid
  ✅ Merkle proofs valid
  ✅ Signatures valid
  ✅ Chain of custody intact
  ✅ Cross-package chain verified (2 packages)
```

---

## Step 8: Inspect Output

Check `data/output.json`:

```json
[
  {
    "id": "user-001",
    "value": 100,
    "processed": true,
    "timestamp": 1704067200000
  },
  {
    "id": "user-002",
    "value": 250,
    "processed": true,
    "timestamp": 1704067200000
  },
  {
    "id": "user-003",
    "value": 175,
    "processed": true,
    "timestamp": 1704067200000
  }
]
```

---

## Advanced: Test Validation Failures

### Test Invalid Input

Modify `data/input.json`:
```json
[
  { "id": "user-001", "value": -50 }  // ❌ Negative value
]
```

Run:
```bash
node src/index.mjs
```

**Expected:**
```
🔍 [PRE-HOOK] Validating input data...
❌ [PRE-HOOK] Input validation failed: Expected positive, received -50
❌ Workflow failed: Expected positive, received -50
```

✅ **Hook prevented invalid data from entering the workflow!**

### Test Duplicate IDs

```json
[
  { "id": "user-001", "value": 100 },
  { "id": "user-001", "value": 200 }  // ❌ Duplicate ID
]
```

**Expected:**
```
❌ [PRE-HOOK] Input validation failed: Duplicate IDs found: user-001
```

---

## Summary

This example demonstrated:

✅ **YAWL workflow** with 3 steps (load, process, save)
✅ **Pre-execution hook** for input validation
✅ **Post-execution hook** for output validation
✅ **Zod schemas** at package boundaries
✅ **Receipt chain** across 2 packages (@unrdf/yawl, @unrdf/hooks)
✅ **Error handling** with validation failures
✅ **Deterministic execution** (same input → same output)

**Receipt Chain Flow:**
```
validate-input (hooks) → data-processing (yawl) → validate-output (hooks)
```

---

## Next Steps

1. **Extend workflow**: Add more steps (e.g., API calls, database writes)
2. **Custom hooks**: Create domain-specific validation hooks
3. **Error recovery**: Add retry logic and error handlers
4. **Performance**: Benchmark with large datasets
5. **Production**: Deploy with OTEL observability

**Related:**
- [How-To: Cross-Package Integration](../how-to/05-cross-package-integration.md)
- [How-To: Verify Receipt Chain](../how-to/03-verify-receipt-chain.md)
- [Reference: CLI Commands](../reference/01-cli-command-matrix.md)
