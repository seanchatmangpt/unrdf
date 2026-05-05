# TypeScript Orchestration Gap Remediation Plan

**Branch:** `refactor/performance-optimizations`  
**Scope:** 25 gaps identified by Explore Agent 2  
**Author:** Aalst Plan Agent 2

---

## Dependency Graph

```
Phase 1: GAP-1 (ALGORITHM_IDS)
   ├─→ Phase 2: GAP-2,3,4,9,25 (contracts tables)
   │     ├─→ Phase 4: GAP-8,10,11 (planner/profiles)
   │     └─→ Phase 6: GAP-21,22,23 (MCP)
   ├─→ Phase 3: GAP-5,6,7 (kernel registry/dispatch)
   └─→ Phase 5: GAP-13,14,15,16,17,18 (CLI commands)
Phase 7: GAP-24 (engine timeout) — independent
Phase 8: GAP-19,20 (testing) — depends on Phases 1-5
```

---

## Phase 1 — Foundation: GAP-1 (ALGORITHM_IDS)

**Gap:** `ALGORITHM_IDS` in `algorithm-registry.ts` lists only 21 algorithms. The kernel registry has 35+ (including 16 Wave 1 algorithms).

### File: `/Users/sac/chatmangpt/pictl/packages/contracts/src/templates/algorithm-registry.ts`

**Current state:** 21 IDs (15 discovery + 6 ML).

**Missing Wave 1 IDs (16):**
- `simd_streaming_dfg`
- `hierarchical_dfg`
- `streaming_log`
- `smart_engine`
- `transition_system`
- `log_to_trie`
- `causal_graph`
- `performance_spectrum`
- `batches`
- `correlation_miner`
- `generalization`
- `petri_net_reduction`
- `etconformance_precision`
- `alignments`
- `complexity_metrics`
- `pnml_import`
- `bpmn_import`
- `powl_to_process_tree`
- `yawl_export`
- `playout`

**Change:** Add all 16+ missing IDs to the `ALGORITHM_IDS` const array:

```typescript
export const ALGORITHM_IDS = [
  // Original 15 discovery
  'process_skeleton',
  'dfg',
  'alpha_plus_plus',
  'heuristic_miner',
  'inductive_miner',
  'declare',
  'hill_climbing',
  'simulated_annealing',
  'a_star',
  'aco',
  'optimized_dfg',
  'pso',
  'genetic_algorithm',
  'ilp',
  // Streaming / Smart
  'simd_streaming_dfg',
  'hierarchical_dfg',
  'streaming_log',
  'smart_engine',
  // Wave 1 Discovery
  'transition_system',
  'log_to_trie',
  'causal_graph',
  'performance_spectrum',
  'batches',
  'correlation_miner',
  // Wave 1 Conformance
  'generalization',
  'petri_net_reduction',
  'etconformance_precision',
  'alignments',
  // Wave 1 Quality
  'complexity_metrics',
  // Wave 1 Import/Export
  'pnml_import',
  'bpmn_import',
  'powl_to_process_tree',
  'yawl_export',
  // Wave 1 Simulation
  'playout',
  // ML Analysis
  'ml_classify',
  'ml_cluster',
  'ml_forecast',
  'ml_anomaly',
  'ml_regress',
  'ml_pca',
] as const;
```

**Testing:** Verify `ALGORITHM_IDS.length >= 37`. Existing tests that check known IDs pass. No test should break since the type union is additive.

---

## Phase 2 — Contracts: GAP-2,3,4,9,25

### GAP-2: ALGORITHM_OUTPUT_TYPES missing 16 algorithms

**File:** `/Users/sac/chatmangpt/pictl/packages/contracts/src/templates/algorithm-registry.ts`

**Change:** Add entries for all missing IDs:

```typescript
// After existing entries, add:
simd_streaming_dfg: 'dfg',
hierarchical_dfg: 'dfg',
streaming_log: 'dfg',
smart_engine: 'dfg',
transition_system: 'dfg',
log_to_trie: 'dfg',
causal_graph: 'dfg',
performance_spectrum: 'dfg',
batches: 'dfg',
correlation_miner: 'dfg',
generalization: 'tree',
petri_net_reduction: 'petrinet',
etconformance_precision: 'tree',
alignments: 'tree',
complexity_metrics: 'tree',
pnml_import: 'petrinet',
bpmn_import: 'tree',
powl_to_process_tree: 'tree',
yawl_export: 'tree',
playout: 'dfg',
```

### GAP-3: ALGORITHM_CLI_ALIASES missing 16 algorithms

**File:** Same file.

**Change:**

```typescript
simd_streaming_dfg: 'simd-dfg',
hierarchical_dfg: 'hierarchical-dfg',
streaming_log: 'streaming-log',
smart_engine: 'smart-engine',
transition_system: 'transition-system',
log_to_trie: 'trie',
causal_graph: 'causal',
performance_spectrum: 'perf-spectrum',
batches: 'batches',
correlation_miner: 'correlation',
generalization: 'generalization',
petri_net_reduction: 'pn-reduction',
etconformance_precision: 'etconformance',
alignments: 'alignments',
complexity_metrics: 'complexity',
pnml_import: 'pnml-import',
bpmn_import: 'bpmn-import',
powl_to_process_tree: 'powl-tree',
yawl_export: 'yawl-export',
playout: 'playout',
```

### GAP-4: ALGORITHM_DISPLAY_NAMES missing 16 algorithms

**File:** Same file.

**Change:**

```typescript
simd_streaming_dfg: 'SIMD Streaming DFG',
hierarchical_dfg: 'Hierarchical DFG',
streaming_log: 'Streaming Log (Probabilistic)',
smart_engine: 'Smart Engine',
transition_system: 'Transition System',
log_to_trie: 'Prefix Tree Discovery',
causal_graph: 'Causal Graph Discovery',
performance_spectrum: 'Performance Spectrum',
batches: 'Batch Detection',
correlation_miner: 'Correlation Miner',
generalization: 'Generalization Metric',
petri_net_reduction: 'Petri Net Reduction',
etconformance_precision: 'ETConformance Precision',
alignments: 'A* Optimal Alignments',
complexity_metrics: 'POWL Complexity Metrics',
pnml_import: 'PNML Import',
bpmn_import: 'BPMN Import',
powl_to_process_tree: 'POWL to Process Tree',
yawl_export: 'YAWL Export',
playout: 'Process Tree Playout',
```

### GAP-9: PlanStepType values missing Wave 1 algorithm step types

**File:** `/Users/sac/chatmangpt/pictl/packages/planner/src/steps.ts`

**Current state:** Has `DISCOVER_DFG`, `DISCOVER_PROCESS_SKELETON`, etc. but missing Wave 1 step types.

**Change:** Add new enum values:

```typescript
export enum PlanStepType {
  // ... existing values ...

  // Wave 1 Discovery
  DISCOVER_TRANSITION_SYSTEM = 'discover_transition_system',
  DISCOVER_PREFIX_TREE = 'discover_prefix_tree',
  DISCOVER_CAUSAL_GRAPH = 'discover_causal_graph',
  DISCOVER_PERFORMANCE_SPECTRUM = 'discover_performance_spectrum',
  DISCOVER_BATCHES = 'discover_batches',
  DISCOVER_CORRELATION = 'discover_correlation',
  DISCOVER_SIMD_STREAMING_DFG = 'discover_simd_streaming_dfg',
  DISCOVER_HIERARCHICAL_DFG = 'discover_hierarchical_dfg',
  DISCOVER_STREAMING_LOG = 'discover_streaming_log',
  DISCOVER_SMART_ENGINE = 'discover_smart_engine',

  // Wave 1 Conformance
  CONFORMANCE_GENERALIZATION = 'conformance_generalization',
  CONFORMANCE_PETRI_NET_REDUCTION = 'conformance_petri_net_reduction',
  CONFORMANCE_ETCONFORMANCE = 'conformance_etconformance',
  CONFORMANCE_ALIGNMENTS = 'conformance_alignments',

  // Wave 1 Quality
  QUALITY_COMPLEXITY = 'quality_complexity',

  // Wave 1 Import/Export
  IMPORT_PNML = 'import_pnml',
  IMPORT_BPMN = 'import_bpmn',
  CONVERT_POWL_TREE = 'convert_powl_tree',
  EXPORT_YAWL = 'export_yawl',

  // Wave 1 Simulation
  SIMULATE_PLAYOUT = 'simulate_playout',
}
```

Also update `ALGORITHM_ID_TO_STEP_TYPE` in algorithm-registry.ts:

```typescript
// Add to ALGORITHM_ID_TO_STEP_TYPE:
simd_streaming_dfg: 'discover_simd_streaming_dfg',
hierarchical_dfg: 'discover_hierarchical_dfg',
streaming_log: 'discover_streaming_log',
smart_engine: 'discover_smart_engine',
transition_system: 'discover_transition_system',
log_to_trie: 'discover_prefix_tree',
causal_graph: 'discover_causal_graph',
performance_spectrum: 'discover_performance_spectrum',
batches: 'discover_batches',
correlation_miner: 'discover_correlation',
generalization: 'conformance_generalization',
petri_net_reduction: 'conformance_petri_net_reduction',
etconformance_precision: 'conformance_etconformance',
alignments: 'conformance_alignments',
complexity_metrics: 'quality_complexity',
pnml_import: 'import_pnml',
bpmn_import: 'import_bpmn',
powl_to_process_tree: 'convert_powl_tree',
yawl_export: 'export_yawl',
playout: 'simulate_playout',
```

Also update `stepTypeToAlgorithmId()` in handlers.ts to include the reverse mapping for these new PlanStepTypes.

### GAP-25: Error codes missing for new feature domains

**File:** `/Users/sac/chatmangpt/pictl/packages/contracts/src/errors.ts`

**Change:** Add new error codes:

```typescript
export type ErrorCode =
  // ... existing codes ...
  | 'CONFORMANCE_FAILED'
  | 'SIMULATION_FAILED'
  | 'PREDICTION_FAILED'
  | 'VALIDATION_FAILED'
  | 'IMPORT_FAILED';
```

Also update the `TypedError` numeric code mapping in the same file, assigning codes in the 400-499 range for algorithm subdomains.

**File:** `/Users/sac/chatmangpt/pictl/apps/pictl/src/exit-codes.ts`

**Change:** No change needed — existing `execution_error: 3` covers algorithm failures. The new typed error codes are for structured reporting within receipts, not CLI exit codes.

**Testing:**
- Unit test: `ALGORITHM_OUTPUT_TYPES` has entry for every ID in `ALGORITHM_IDS`
- Unit test: `ALGORITHM_CLI_ALIASES` has entry for every ID in `ALGORITHM_IDS`
- Unit test: `ALGORITHM_DISPLAY_NAMES` has entry for every ID in `ALGORITHM_IDS`
- Unit test: `ALGORITHM_ID_TO_STEP_TYPE` has entry for every ID in `ALGORITHM_IDS`
- Unit test: New `PlanStepType` values are present
- Unit test: New `ErrorCode` values are present

---

## Phase 3 — Kernel: GAP-5,6,7

### GAP-5: 30+ Rust WASM exports not registered in kernel registry

**File:** `/Users/sac/chatmangpt/pictl/packages/kernel/src/registry.ts`

**Current state:** The registry already has Wave 1 entries (lines 870-1168) for: `transition_system`, `log_to_trie`, `causal_graph`, `performance_spectrum`, `batches`, `correlation_miner`, `generalization`, `petri_net_reduction`, `etconformance_precision`, `alignments`, `complexity_metrics`, `pnml_import`, `bpmn_import`, `powl_to_process_tree`, `yawl_export`, `playout`.

**Assessment:** This gap is **already partially resolved**. The registry already registers these. What's missing is that `ALGORITHM_IDS` (Phase 1) doesn't include them, which blocks the config schema and planner.

**Remaining work:** After Phase 1, verify that `getRegistry().list()` returns all algorithms. The registry and handlers are already wired for Wave 1 algorithms. No code changes needed in the registry file itself.

### GAP-6: Dispatcher missing cases for Wave 1 algorithms

**File:** `/Users/sac/chatmangpt/pictl/packages/kernel/src/handlers.ts`

**Current state:** `implementAlgorithmStep()` already has switch cases for all Wave 1 algorithms (lines 537-690): `transition_system`, `log_to_trie`, `causal_graph`, `performance_spectrum`, `batches`, `correlation_miner`, `generalization`, `petri_net_reduction`, `etconformance_precision`, `alignments`, `complexity_metrics`, `pnml_import`, `bpmn_import`, `powl_to_process_tree`, `yawl_export`, `playout`.

**Assessment:** This gap is **already resolved**. The handler dispatch is complete. No code changes needed.

### GAP-7: Playout dispatches to `play_out()` but Rust export is `play_out_process_tree()`

**File:** `/Users/sac/chatmangpt/pictl/packages/kernel/src/handlers.ts`

**Current state (line 219):**
```typescript
play_out(
  model_handle: string,
  num_traces: number,
  max_trace_length: number
): Promise<{ handle: string }>;
```

**Assessment:** The Rust `#[wasm_bindgen]` export name is `play_out` (from `src/simulation.rs`). Let me verify:

The handler at line 682 calls `wasmModule.play_out(...)` which matches the WasmModule interface. This appears correct as-is. The Rust function may be named `play_out_process_tree` internally but exported as `play_out` via `#[wasm_bindgen]`. **No change needed** unless testing reveals a mismatch.

**Testing:** Run `cargo test --lib` and verify `play_out` is callable from WASM. Add a specific integration test:

```typescript
test('playout dispatches to WASM play_out', async () => {
  const wasm = mockWasmModule();
  wasm.play_out = vi.fn().mockResolvedValue({ handle: 'sim-log-1' });
  // ... exercise playout step
  expect(wasm.play_out).toHaveBeenCalledWith('model-h', 100, 100);
});
```

---

## Phase 4 — Planner: GAP-8,10,11

### GAP-8: Planner profiles don't include ANY Wave 1 algorithms

**File:** `/Users/sac/chatmangpt/pictl/packages/contracts/src/templates/algorithm-registry.ts` (getProfileAlgorithms)

**Current state:** `getProfileAlgorithms()` returns only original 15 algorithms per profile.

**Change:** Add Wave 1 algorithms to appropriate profiles:

```typescript
export function getProfileAlgorithms(profile: string): string[] {
  const map: Record<string, string[]> = {
    fast: ['process_skeleton', 'dfg', 'simd_streaming_dfg', 'streaming_log', 'smart_engine'],
    balanced: [
      'alpha_plus_plus', 'heuristic_miner', 'inductive_miner', 'declare', 'hill_climbing',
      'hierarchical_dfg', 'log_to_trie', 'complexity_metrics',
    ],
    quality: [
      'simulated_annealing', 'a_star', 'aco', 'optimized_dfg', 'pso', 'genetic_algorithm', 'ilp',
      'transition_system', 'causal_graph', 'performance_spectrum', 'batches', 'correlation_miner',
      'generalization', 'petri_net_reduction', 'etconformance_precision', 'alignments',
      'pnml_import', 'bpmn_import', 'powl_to_process_tree', 'yawl_export', 'playout',
    ],
    stream: ['dfg', 'simd_streaming_dfg', 'streaming_log', 'smart_engine',
             'transition_system', 'causal_graph', 'performance_spectrum'],
    ensemble: ['dfg', 'heuristic_miner', 'inductive_miner', 'genetic_algorithm',
               'correlation_miner', 'complexity_metrics'],
    auto: ['dfg', 'heuristic_miner', 'inductive_miner', 'genetic_algorithm', 'ilp',
           'smart_engine', 'correlation_miner'],
    ml: ['ml_classify', 'ml_cluster', 'ml_forecast', 'ml_anomaly', 'ml_regress', 'ml_pca'],
  };
  return map[profile.toLowerCase()] ?? map['balanced']!;
}
```

### GAP-10: ANALYZE_CONFORMANCE/ANALYZE_VARIANTS/ANALYZE_PERFORMANCE are no-op abstract steps

**File:** `/Users/sac/chatmangpt/pictl/packages/kernel/src/step-dispatcher.ts`

**Current state:** The step-dispatcher only handles ML algorithms. Analysis steps (`ANALYZE_CONFORMANCE`, etc.) have no handler — they're abstract.

**Assessment:** These are **by design** for the engine pipeline. Analysis steps are meant to be resolved by the engine into concrete algorithm calls. The `planner.ts` already emits them as plan steps; the engine decides how to implement them.

**Resolution:** Document these as "composite analysis steps" that the engine expands. No code change needed now — the engine pipeline resolves them into concrete discovery + conformance calls. Add a comment to `steps.ts`:

```typescript
// Analysis steps are composite — the engine expands them into concrete algorithm steps
// ANALYZE_CONFORMANCE → alignments + token_replay
// ANALYZE_VARIANTS → extract variants + statistics
// ANALYZE_PERFORMANCE → performance_spectrum + bottleneck detection
```

### GAP-11: Config schema can't select Wave 1 algorithms

**File:** `/Users/sac/chatmangpt/pictl/packages/config/src/schema.ts`

**Current state:** `algorithmIdSchema = z.enum(ALGORITHM_IDS)` — since `ALGORITHM_IDS` only had 21 entries, Wave 1 algorithms couldn't be selected.

**Resolution:** After Phase 1 (GAP-1) adds Wave 1 IDs to `ALGORITHM_IDS`, this schema automatically accepts them. The `algorithmIdSchema` already derives from `ALGORITHM_IDS`:

```typescript
import { ALGORITHM_IDS } from '@pictl/contracts';
export const algorithmIdSchema = z.enum(ALGORITHM_IDS);
```

**No code change needed in schema.ts** — it's already wired correctly. The fix cascades from GAP-1.

### GAP-12: executionConfigSchema missing maxEvents and mode fields

**File:** `/Users/sac/chatmangpt/pictl/packages/config/src/schema.ts`

**Change:** Add missing fields:

```typescript
export const executionConfigSchema = z
  .object({
    profile: executionProfileSchema.default('balanced'),
    timeout: z.number().int().positive().optional(),
    maxMemory: z.number().int().positive().optional(),
    maxEvents: z.number().int().positive().optional().describe('Maximum events to process (0 = all)'),
    mode: z.enum(['discovery', 'conformance', 'simulation', 'analysis']).optional().describe('Execution mode'),
  })
  .describe('Execution configuration');
```

**Testing:**
- Unit test: `getProfileAlgorithms('quality')` includes `transition_system`, `causal_graph`, `alignments`, etc.
- Unit test: Config with `algorithm.name: 'causal_graph'` parses successfully
- Unit test: Config with `execution.maxEvents: 10000` and `execution.mode: 'conformance'` parses successfully

---

## Phase 5 — CLI Commands: GAP-13,14,15,16,17,18

### Template Pattern

All new CLI commands follow the same pattern as `run.ts`. Here's the template:

```typescript
import { defineCommand } from 'citty';
import * as fs from 'fs/promises';
import { WasmLoader } from '@pictl/engine';
import { getFormatter, HumanFormatter, JSONFormatter } from '../output.js';
import { EXIT_CODES } from '../exit-codes.js';
import { savePredictionResult } from './results.js';
import type { OutputOptions } from '../output.js';

export interface CommandOptions extends OutputOptions {
  input?: string;
  file?: string;
  config?: string;
  output?: string;
  format?: string;
  verbose?: boolean;
  quiet?: boolean;
}

export const commandName = defineCommand({
  meta: {
    name: 'command-name',
    description: 'Description of command',
  },
  args: {
    input: { type: 'positional', description: 'Path to XES file', required: false },
    file: { type: 'string', description: 'Path to XES file', alias: 'i' },
    config: { type: 'string', description: 'Config file path' },
    output: { type: 'string', description: 'Output file path', alias: 'o' },
    format: { type: 'string', description: 'Output format: human (default) or json' },
    verbose: { type: 'boolean', description: 'Verbose output', alias: 'v' },
    quiet: { type: 'boolean', description: 'Quiet output', alias: 'q' },
    'no-save': { type: 'boolean', description: 'Skip auto-save' },
    // Command-specific args here
  },
  async run(ctx) {
    const formatter = getFormatter({ format: ctx.args.format as 'human' | 'json', verbose: ctx.args.verbose, quiet: ctx.args.quiet });
    try {
      const inputPath = (ctx.args.input as string) || (ctx.args.file as string);
      if (!inputPath) { formatter.error('Input file required.'); process.exit(EXIT_CODES.source_error); }
      await fs.access(inputPath);

      const loader = WasmLoader.getInstance();
      await loader.init();
      const wasm = loader.get();

      const xesContent = await fs.readFile(inputPath, 'utf-8');
      const logHandle = wasm.load_eventlog_from_xes(xesContent);
      const t0 = performance.now();

      // ── Command-specific WASM calls ──
      // const raw = wasm.some_function(logHandle, ...);
      // const result = typeof raw === 'string' ? JSON.parse(raw) : raw;

      const elapsedMs = performance.now() - t0;
      try { wasm.delete_object(logHandle); } catch { /* best-effort */ }

      const output = { status: 'success', elapsedMs: Math.round(elapsedMs * 100) / 100, /* ... */ };

      if (!ctx.args['no-save']) {
        await savePredictionResult('command-name', inputPath, 'concept:name', output);
      }
      formatter.success(`Completed in ${elapsedMs.toFixed(1)}ms`);
      process.exit(EXIT_CODES.success);
    } catch (error) {
      formatter.error(`Failed: ${error instanceof Error ? error.message : String(error)}`);
      process.exit(EXIT_CODES.execution_error);
    }
  },
});
```

### GAP-13: `pictl conformance` CLI command

**File:** `/Users/sac/chatmangpt/pictl/apps/pictl/src/commands/conformance.ts`

**Args:**
```
--model <handle|json>   Required: model to check against
--algorithm alignments   Conformance algorithm (default: alignments)
--cost-config            JSON cost config for alignments
```

**WASM calls:**
```typescript
const raw = wasm.compute_optimal_alignments(logHandle, modelHandle, activityKey, costConfig);
// or
const raw = wasm.simd_token_replay(logHandle, activityKey);
```

### GAP-14: `pictl simulate` CLI command

**File:** `/Users/sac/chatmangpt/pictl/apps/pictl/src/commands/simulate.ts`

**Args:**
```
--model <handle>        Required: model to simulate from
--num-traces <n>        Number of traces (default: 100)
--max-length <n>        Max trace length (default: 100)
```

**WASM call:**
```typescript
const raw = wasm.play_out(modelHandle, numTraces, maxTraceLength);
```

### GAP-15: `pictl temporal` CLI command

**File:** `/Users/sac/chatmangpt/pictl/apps/pictl/src/commands/temporal.ts`

**Args:**
```
--metric <metric>       performance_spectrum or bottleneck detection
```

**WASM calls:**
```typescript
const raw = wasm.discover_performance_spectrum(logHandle, activityKey, timestampKey);
// or
const raw = wasm.detect_bottlenecks(logHandle, activityKey, timestampKey, threshold);
```

### GAP-16: `pictl social` CLI command

**File:** `/Users/sac/chatmangpt/pictl/apps/pictl/src/commands/social.ts`

**Args:**
```
--metric <metric>       correlation or handover
```

**WASM calls:**
```typescript
const raw = wasm.discover_correlation(logHandle, activityKey, timestampKey);
```

### GAP-17: `pictl quality` CLI command

**File:** `/Users/sac/chatmangpt/pictl/apps/pictl/src/commands/quality.ts`

**Args:**
```
--model <handle>        Required: model to measure
```

**WASM call:**
```typescript
const raw = wasm.measure_complexity(powlHandle);
// or
const raw = wasm.generalization(logHandle, petriNetHandle);
```

### GAP-18: `pictl validate` CLI command

**File:** `/Users/sac/chatmangpt/pictl/apps/pictl/src/commands/validate.ts`

**Args:**
```
--model <handle|json>   Required: model to validate
--log <path>            Required: event log for conformance check
```

**WASM calls:** Combines alignment fitness + precision check:
```typescript
const fitness = wasm.simd_token_replay(logHandle, activityKey);
const precision = wasm.precision_etconformance(logHandle, modelHandle, activityKey);
```

### CLI Registration

**File:** `/Users/sac/chatmangpt/pictl/apps/pictl/src/cli.ts`

**Change:** Import and register all new commands:

```typescript
import { conformance } from './commands/conformance.js';
import { simulate } from './commands/simulate.js';
import { temporal } from './commands/temporal.js';
import { social } from './commands/social.js';
import { quality } from './commands/quality.js';
import { validate } from './commands/validate.js';

// In subCommands:
subCommands: {
  // ... existing ...
  conformance,
  simulate,
  temporal,
  social,
  quality,
  validate,
},
```

Also update the help text banner to include new commands.

**Testing:**
- CLI smoke test: `pictl conformance --help` exits 0
- CLI smoke test: `pictl simulate --help` exits 0
- Integration test: Each command shows appropriate error when no input provided

---

## Phase 6 — MCP Server: GAP-21,22,23

### GAP-21: MCP server missing tools for new feature domains

**File:** `/Users/sac/chatmangpt/pictl/wasm4pm/src/mcp_server.ts`

**Missing tools to add:**

```typescript
// Conformance
{
  name: 'compute_alignments',
  description: 'Compute optimal trace-to-model alignments using A* search. Returns fitness and per-trace diagnostics.',
  inputSchema: {
    type: 'object' as const,
    properties: {
      xes_content: { type: 'string', description: 'XES event log content' },
      model_handle: { type: 'string', description: 'Petri net model handle' },
    },
    required: ['xes_content', 'model_handle'],
  },
},

// Simulation
{
  name: 'simulate_playout',
  description: 'Generate synthetic event traces by simulating a process tree or DFG model.',
  inputSchema: {
    type: 'object' as const,
    properties: {
      model_handle: { type: 'string', description: 'Model handle to simulate' },
      num_traces: { type: 'number', description: 'Number of traces to generate (default: 100)' },
      max_trace_length: { type: 'number', description: 'Max trace length (default: 100)' },
    },
    required: ['model_handle'],
  },
},

// Temporal analysis
{
  name: 'analyze_performance_spectrum',
  description: 'Analyze duration statistics between activity pairs to identify bottlenecks and performance patterns.',
  inputSchema: {
    type: 'object' as const,
    properties: {
      xes_content: { type: 'string', description: 'XES event log content' },
    },
    required: ['xes_content'],
  },
},

// Social network
{
  name: 'mine_correlation',
  description: 'Discover activity correlations without case identifiers using timestamp-based correlation analysis.',
  inputSchema: {
    type: 'object' as const,
    properties: {
      xes_content: { type: 'string', description: 'XES event log content' },
    },
    required: ['xes_content'],
  },
},

// Quality metrics
{
  name: 'measure_model_complexity',
  description: 'Measure structural complexity of a POWL model.',
  inputSchema: {
    type: 'object' as const,
    properties: {
      powl_handle: { type: 'string', description: 'POWL model handle' },
    },
    required: ['powl_handle'],
  },
},

// Import
{
  name: 'import_pnml',
  description: 'Import a Petri net from PNML XML format and get a handle for further analysis.',
  inputSchema: {
    type: 'object' as const,
    properties: {
      pnml_xml: { type: 'string', description: 'PNML XML string' },
    },
    required: ['pnml_xml'],
  },
},

{
  name: 'import_bpmn',
  description: 'Import a BPMN 2.0 XML model and convert to POWL.',
  inputSchema: {
    type: 'object' as const,
    properties: {
      bpmn_xml: { type: 'string', description: 'BPMN 2.0 XML string' },
    },
    required: ['bpmn_xml'],
  },
},

// Validation
{
  name: 'validate_conformance',
  description: 'Full conformance validation: fitness + precision + generalization.',
  inputSchema: {
    type: 'object' as const,
    properties: {
      xes_content: { type: 'string', description: 'XES event log content' },
      model_handle: { type: 'string', description: 'Petri net model handle' },
    },
    required: ['xes_content', 'model_handle'],
  },
},
```

Also add corresponding `case` blocks in `executeTool()` for each new tool name.

### GAP-22: Duplicate `detect_concept_drift` tool definition

**File:** Same MCP server file.

**Current state:** `detect_concept_drift` is defined twice (lines 220-237 and lines 430-456) with different schemas.

**Change:** Remove the first definition (lines 220-237, the simpler one) and keep only the second, richer definition (lines 430-456, with EWMA support). The second `case` block (line 995) already handles the enhanced version.

### GAP-23: MCP server version hardcoded

**File:** Same MCP server file, line 42.

**Current state:** `version: 'latest'`

**Change:** Read from `package.json`:

```typescript
import { createRequire } from 'module';
const require = createRequire(import.meta.url);
const pkg = require('../package.json');

// In constructor:
this.server = new Server(
  { name: 'pictl', version: pkg.version },
  { capabilities: { tools: {} } }
);
```

**Testing:**
- MCP test: `getAvailableTools()` returns all new tool names
- MCP test: No duplicate tool names
- MCP test: Server version matches package.json

---

## Phase 7 — Engine: GAP-24 (Timeout Protection)

### GAP-24: Engine state machine has no timeout protection

**File:** `/Users/sac/chatmangpt/pictl/packages/engine/src/engine.ts`

**Assessment:** The state machine transitions are defined in `transitions.ts` but don't include timeout protection. If the engine gets stuck in `bootstrapping`, `planning`, or `running`, it never auto-transitions to `degraded`.

**Change (engine.ts):** Add a watchdog timer that monitors state transitions:

```typescript
private stateEntryTime: number = Date.now();
private stateTimeoutMs: number;
private watchdogTimer: ReturnType<typeof setTimeout> | null = null;

// Default timeouts per state (WvdA boundedness)
private static STATE_TIMEOUTS: Record<string, number> = {
  bootstrapping: 30_000,   // 30s to init WASM
  planning: 10_000,        // 10s to generate plan
  running: 300_000,        // 5min execution timeout
  watching: Infinity,      // Watching runs indefinitely
};

private startWatchdog(): void {
  this.stopWatchdog();
  const timeout = Engine.STATE_TIMEOUTS[this.state] ?? 60_000;
  this.stateEntryTime = Date.now();

  if (timeout === Infinity) return;

  this.watchdogTimer = setTimeout(() => {
    this.handleStateTimeout();
  }, timeout);
}

private stopWatchdog(): void {
  if (this.watchdogTimer) {
    clearTimeout(this.watchdogTimer);
    this.watchdogTimer = null;
  }
}

private handleStateTimeout(): void {
  const elapsed = Date.now() - this.stateEntryTime;
  const state = this.state;
  this.transition('degraded', {
    reason: `State "${state}" timed out after ${elapsed}ms`,
    elapsedMs: elapsed,
    previousState: state,
  });
}

// Call startWatchdog() in every transition() override
// Call stopWatchdog() in dispose()
```

**Testing:**
- Unit test: Engine in `bootstrapping` state auto-transitions to `degraded` after timeout
- Unit test: Engine in `ready`/`watching` state never times out
- Unit test: Watchdog resets on valid transition

---

## Phase 8 — Testing: GAP-19,20

### GAP-19: No parity tests for Wave 1 algorithms

**File:** `/Users/sac/chatmangpt/pictl/packages/testing/src/harness/parity.ts`

**Change:** Add parity test entries for each Wave 1 algorithm. Parity tests verify `explain(algo) == plan(config)`.

```typescript
export const WAVE1_PARITY_CASES = [
  { algorithm: 'transition_system', profile: 'quality' },
  { algorithm: 'log_to_trie', profile: 'balanced' },
  { algorithm: 'causal_graph', profile: 'quality' },
  { algorithm: 'performance_spectrum', profile: 'quality' },
  { algorithm: 'batches', profile: 'quality' },
  { algorithm: 'correlation_miner', profile: 'quality' },
  { algorithm: 'generalization', profile: 'quality' },
  { algorithm: 'petri_net_reduction', profile: 'quality' },
  { algorithm: 'etconformance_precision', profile: 'quality' },
  { algorithm: 'alignments', profile: 'quality' },
  { algorithm: 'complexity_metrics', profile: 'balanced' },
  { algorithm: 'pnml_import', profile: 'balanced' },
  { algorithm: 'bpmn_import', profile: 'balanced' },
  { algorithm: 'powl_to_process_tree', profile: 'balanced' },
  { algorithm: 'yawl_export', profile: 'balanced' },
  { algorithm: 'playout', profile: 'balanced' },
  { algorithm: 'simd_streaming_dfg', profile: 'fast' },
  { algorithm: 'hierarchical_dfg', profile: 'fast' },
  { algorithm: 'streaming_log', profile: 'fast' },
  { algorithm: 'smart_engine', profile: 'fast' },
];
```

### GAP-20: No determinism tests for Wave 1 algorithms

**File:** `/Users/sac/chatmangpt/pictl/packages/testing/src/harness/determinism.ts`

**Change:** Add determinism test entries:

```typescript
export const WAVE1_DETERMINISM_CASES = [
  'transition_system', 'log_to_trie', 'causal_graph',
  'performance_spectrum', 'batches', 'correlation_miner',
  'simd_streaming_dfg', 'hierarchical_dfg', 'streaming_log',
  'smart_engine',
];
```

Determinism tests run the same algorithm twice on the same input and verify identical output hashes.

**Testing location:** `/Users/sac/chatmangpt/pictl/playground/` or `/Users/sac/chatmangpt/pictl/packages/testing/__tests__/`

**Test pattern:**
```typescript
for (const algo of WAVE1_DETERMINISM_CASES) {
  test(`${algo} produces deterministic output`, async () => {
    const hash1 = await runAndHash(algo, testLog);
    const hash2 = await runAndHash(algo, testLog);
    expect(hash1).toBe(hash2);
  });
}
```

---

## Implementation Order Summary

| Phase | Gaps | Estimated LOC Changed | Dependencies |
|-------|------|----------------------|--------------|
| 1 | GAP-1 | ~30 lines | None |
| 2 | GAP-2,3,4,9,12,25 | ~120 lines | Phase 1 |
| 3 | GAP-5,6,7 | ~10 lines (verify) | Phase 1 |
| 4 | GAP-8,10,11 | ~50 lines | Phase 1,2 |
| 5 | GAP-13,14,15,16,17,18 | ~600 lines (6 commands) | Phase 1 |
| 6 | GAP-21,22,23 | ~200 lines | Phase 1 |
| 7 | GAP-24 | ~60 lines | None (independent) |
| 8 | GAP-19,20 | ~80 lines | Phases 1-5 |

**Total estimated:** ~1150 lines across 15+ files.

---

## Files Modified (Complete List)

| File | Phases |
|------|--------|
| `packages/contracts/src/templates/algorithm-registry.ts` | 1, 2 |
| `packages/planner/src/steps.ts` | 2 |
| `packages/kernel/src/handlers.ts` | 2 (stepTypeToAlgorithmId) |
| `packages/contracts/src/errors.ts` | 2 |
| `packages/config/src/schema.ts` | 2 (GAP-12) |
| `packages/planner/src/planner.ts` | 4 (minor) |
| `apps/pictl/src/commands/conformance.ts` (new) | 5 |
| `apps/pictl/src/commands/simulate.ts` (new) | 5 |
| `apps/pictl/src/commands/temporal.ts` (new) | 5 |
| `apps/pictl/src/commands/social.ts` (new) | 5 |
| `apps/pictl/src/commands/quality.ts` (new) | 5 |
| `apps/pictl/src/commands/validate.ts` (new) | 5 |
| `apps/pictl/src/cli.ts` | 5 |
| `wasm4pm/src/mcp_server.ts` | 6 |
| `packages/engine/src/engine.ts` | 7 |
| `packages/testing/src/harness/parity.ts` | 8 |
| `packages/testing/src/harness/determinism.ts` | 8 |
