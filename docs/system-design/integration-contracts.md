# KGC Probe Integration Contracts

## Document Purpose

This document defines the exact API contracts between `@unrdf/kgc-probe` and its 6 integration dependencies. Each contract specifies:
- Import paths and module structure
- Function signatures with JSDoc types
- Expected behaviors and error conditions
- Usage examples within kgc-probe

---

## 1. @unrdf/v6-core Integration

### 1.1 Package Purpose

Provides receipt factories, Merkle tree operations, and delta proposal system for deterministic RDF operations with cryptographic proofs.

### 1.2 Import Contract

```javascript
// Primary imports used by kgc-probe
import {
  // Receipt capsule
  createReceipt,
  verifyReceipt,
  ReceiptSchema,

  // Delta capsule
  createDelta,
  applyDelta,
  DeltaSchema,

  // Status utilities
  getV6Status,
  isFeatureEnabled,
  V6_VERSION
} from '@unrdf/v6-core';

// Alternative subpath imports
import { createReceipt, verifyReceipt } from '@unrdf/v6-core/receipts';
import { createDelta, applyDelta } from '@unrdf/v6-core/delta';
```

### 1.3 API Signatures

```javascript
/**
 * Create a receipt for a set of observations
 * @param {Object} payload - Receipt payload
 * @param {string} payload.type - Receipt type ('probe_artifact')
 * @param {string} payload.universe_id - Universe identifier
 * @param {Object[]} payload.observations - Observations to receipt
 * @param {string} payload.hash - Blake3 hash of observations
 * @returns {Promise<Receipt>} Signed receipt with Merkle proof
 */
async function createReceipt(payload);

/**
 * Verify receipt integrity and signature
 * @param {Receipt} receipt - Receipt to verify
 * @returns {Promise<{ valid: boolean, errors: string[] }>}
 */
async function verifyReceipt(receipt);
```

### 1.4 Usage in kgc-probe

```javascript
// In artifact.mjs - creating artifact receipt
import { createReceipt, ReceiptSchema } from '@unrdf/v6-core';

export async function receiptArtifact(artifact) {
  const receipt = await createReceipt({
    type: 'probe_artifact',
    universe_id: artifact.universe_id,
    observations: artifact.observations,
    hash: artifact.integrity.checksum
  });

  return { ...artifact, receipt };
}
```

---

## 2. @unrdf/kgc-substrate Integration

### 2.1 Package Purpose

Provides the deterministic, hash-stable KnowledgeStore with immutable append-only log for RDF triple storage and BLAKE3-based snapshots.

### 2.2 Import Contract

```javascript
// Primary imports used by kgc-probe
import {
  KnowledgeStore,
  ReceiptChain,
  TamperDetector,
  validateStorageSnapshot,
  StorageSnapshotSchema
} from '@unrdf/kgc-substrate';

// Subpath imports
import { KnowledgeStore } from '@unrdf/kgc-substrate/KnowledgeStore';
import { validateStorageSnapshot } from '@unrdf/kgc-substrate/types';
```

### 2.3 API Signatures

```javascript
/**
 * KnowledgeStore - Deterministic triple storage with snapshots
 */
class KnowledgeStore {
  /**
   * Create new store instance
   * @param {Object} [options]
   * @param {string} [options.nodeId] - Node ID for vector clock
   * @param {string} [options.gitDir] - Git directory for snapshots
   */
  constructor(options);

  /**
   * Append triple to immutable log
   * @param {'add'|'delete'} operation - Operation type
   * @param {Term} subject - RDF subject
   * @param {Term} predicate - RDF predicate
   * @param {Term} object - RDF object
   * @param {Term} [graph] - RDF graph
   * @returns {Promise<{ index: bigint, timestamp_ns: bigint }>}
   */
  async appendTriple(operation, subject, predicate, object, graph);

  /**
   * Select triples matching pattern
   * @param {Object} pattern - Query pattern
   * @returns {Set<Quad>} Matching quads
   */
  selectTriples(pattern);

  /**
   * Generate deterministic snapshot
   * @returns {Promise<StorageSnapshot>}
   */
  async generateSnapshot();

  /**
   * Get state commitment hash
   * @returns {Promise<StateCommitment>}
   */
  async getStateCommitment();

  /**
   * Get current quad count
   * @returns {Promise<number>}
   */
  async getQuadCount();
}
```

### 2.4 Usage in kgc-probe

```javascript
// In orchestrator.mjs - passing store to agents
import { KnowledgeStore } from '@unrdf/kgc-substrate';

async scan(config) {
  // Store is provided externally or created
  const store = config.store || new KnowledgeStore({ nodeId: config.universe_id });

  // Generate snapshot for reproducibility
  const snapshot = await store.generateSnapshot();
  config.snapshot_id = snapshot.snapshot_id;

  // Pass store to each agent
  for (const agentId of agentIds) {
    await this.executeAgent(agentId, { ...config, store }, observations, errors);
  }
}
```

---

## 3. @unrdf/oxigraph Integration

### 3.1 Package Purpose

Provides the Oxigraph SPARQL engine wrapper for RDF quad storage and querying. **CRITICAL**: This is the ONLY source for RDF operations. Never import from 'n3' directly.

### 3.2 Import Contract

```javascript
// Primary imports used by kgc-probe
import {
  createStore,
  dataFactory,
  OxigraphStore
} from '@unrdf/oxigraph';

// Subpath imports
import { createStore } from '@unrdf/oxigraph/store';
import { dataFactory } from '@unrdf/oxigraph';
```

### 3.3 API Signatures

```javascript
/**
 * Create new Oxigraph-backed RDF store
 * @param {Quad[]} [quads] - Initial quads to load
 * @returns {OxigraphStore}
 */
function createStore(quads);

/**
 * RDF Data Factory for creating terms
 */
const dataFactory = {
  /**
   * Create named node (IRI)
   * @param {string} iri - IRI value
   * @returns {NamedNode}
   */
  namedNode(iri),

  /**
   * Create blank node
   * @param {string} [id] - Optional blank node ID
   * @returns {BlankNode}
   */
  blankNode(id),

  /**
   * Create literal
   * @param {string} value - Literal value
   * @param {string|NamedNode} [languageOrDatatype] - Language tag or datatype
   * @returns {Literal}
   */
  literal(value, languageOrDatatype),

  /**
   * Create default graph
   * @returns {DefaultGraph}
   */
  defaultGraph(),

  /**
   * Create quad
   * @param {Term} subject
   * @param {Term} predicate
   * @param {Term} object
   * @param {Term} [graph]
   * @returns {Quad}
   */
  quad(subject, predicate, object, graph)
};

/**
 * OxigraphStore - SPARQL-enabled RDF store
 */
class OxigraphStore {
  /**
   * Add quad to store
   * @param {Quad} quad
   */
  add(quad);

  /**
   * Match quads by pattern
   * @param {Term|null} subject
   * @param {Term|null} predicate
   * @param {Term|null} object
   * @param {Term|null} [graph]
   * @returns {Iterable<Quad>}
   */
  match(subject, predicate, object, graph);

  /**
   * Execute SPARQL query
   * @param {string} query - SPARQL query string
   * @returns {QueryResult}
   */
  query(query);

  /**
   * Get quad count
   * @returns {number}
   */
  get size();
}
```

### 3.4 Usage in kgc-probe

```javascript
// In agents/completion.mjs - SPARQL query for missing properties
import { dataFactory } from '@unrdf/oxigraph';

class CompletionAgent extends BaseAgent {
  async scan(context) {
    const { store } = context;

    // Use dataFactory for term creation
    const rdfsLabel = dataFactory.namedNode('http://www.w3.org/2000/01/rdf-schema#label');

    // Find entities missing rdfs:label
    const query = `
      SELECT ?s WHERE {
        ?s ?p ?o .
        MINUS { ?s <http://www.w3.org/2000/01/rdf-schema#label> ?label }
      }
    `;

    const results = store.query(query);
    // Process results into observations...
  }
}
```

---

## 4. @unrdf/hooks Integration

### 4.1 Package Purpose

Provides the policy definition and execution framework for validation, transformation, and quality hooks.

### 4.2 Import Contract

```javascript
// Primary imports used by kgc-probe
import {
  // Hook definition
  defineHook,
  HookSchema,

  // Hook execution
  executeHook,
  executeHookChain,
  HookResultSchema,

  // Hook registry
  createHookRegistry,
  registerHook,
  getHook,
  listHooks,

  // Built-in hooks
  standardValidation,

  // Quality metrics
  QualityMetricsCollector,
  createQualityHooks
} from '@unrdf/hooks';

// Subpath imports
import { defineHook } from '@unrdf/hooks/define';
import { executeHook } from '@unrdf/hooks/executor';
```

### 4.3 API Signatures

```javascript
/**
 * Define a validation/transformation hook
 * @param {Object} config - Hook configuration
 * @param {string} config.id - Unique hook identifier
 * @param {string} config.trigger - When hook fires ('before_add', 'after_query', etc.)
 * @param {Function} [config.validate] - Validation function
 * @param {Function} [config.transform] - Transformation function
 * @returns {Hook}
 */
function defineHook(config);

/**
 * Execute a hook against data
 * @param {Hook} hook - Hook to execute
 * @param {Object} context - Execution context
 * @param {*} data - Data to validate/transform
 * @returns {Promise<HookResult>}
 */
async function executeHook(hook, context, data);

/**
 * Execute a chain of hooks sequentially
 * @param {Hook[]} hooks - Hooks to execute
 * @param {Object} context - Execution context
 * @param {*} data - Initial data
 * @returns {Promise<ChainResult>}
 */
async function executeHookChain(hooks, context, data);

/**
 * Create a hook registry for managing hooks
 * @returns {HookRegistry}
 */
function createHookRegistry();
```

### 4.4 Usage in kgc-probe

```javascript
// In guards.mjs - using hooks for guard validation
import { defineHook, executeHook } from '@unrdf/hooks';

// Define probe guard as a hook
const qualityGuardHook = defineHook({
  id: 'probe:quality_check',
  trigger: 'after_scan',
  validate: (context, observations) => {
    const lowConfidence = observations.filter(o => o.metrics?.confidence < 0.6);
    return lowConfidence.length <= 50;
  }
});

// Execute guard hook
async function runGuardHook(observations) {
  const result = await executeHook(qualityGuardHook, {}, observations);
  return result.passed ? [] : [{ guard_id: 'quality_check', ... }];
}
```

---

## 5. @unrdf/kgc-cli Integration

### 5.1 Package Purpose

Provides the deterministic CLI extension registry for registering probe-specific commands.

### 5.2 Import Contract

```javascript
// Primary imports used by kgc-probe
import {
  Registry,
  ExtensionSchema,
  createEnvelope,
  loadManifest,
  extensions,
  buildCittyTree,
  initializeRegistry
} from '@unrdf/kgc-cli';

// Subpath imports
import { Registry, createEnvelope } from '@unrdf/kgc-cli/registry';
import { loadManifest, extensions } from '@unrdf/kgc-cli/manifest';
```

### 5.3 API Signatures

```javascript
/**
 * CLI Extension Registry
 */
class Registry {
  /**
   * Register a command extension
   * @param {string} name - Command name
   * @param {Object} definition - Command definition
   */
  register(name, definition);

  /**
   * Get registered command
   * @param {string} name - Command name
   * @returns {Object|undefined}
   */
  get(name);

  /**
   * List all registered commands
   * @returns {string[]}
   */
  list();
}

/**
 * Create command envelope for registration
 * @param {Object} config - Command configuration
 * @param {string} config.name - Command name
 * @param {string} config.description - Command description
 * @param {Object} config.args - Command arguments
 * @param {Function} config.run - Command handler
 * @returns {ExtensionEnvelope}
 */
function createEnvelope(config);

/**
 * Build citty command tree from registered extensions
 * @param {Registry} registry - Extension registry
 * @returns {CittyCommand}
 */
function buildCittyTree(registry);
```

### 5.4 Usage in kgc-probe

```javascript
// In cli/probe.mjs - registering probe commands
import { createEnvelope, Registry } from '@unrdf/kgc-cli';
import { runProbe, verifyArtifact, diffArtifacts } from '../index.mjs';

// Create probe command envelope
const probeCommand = createEnvelope({
  name: 'probe',
  description: 'Run integrity scan on knowledge graph',
  args: {
    universe: {
      type: 'string',
      required: true,
      description: 'Universe ID to scan'
    },
    agents: {
      type: 'string',
      description: 'Comma-separated agent IDs (default: all)'
    },
    json: {
      type: 'boolean',
      description: 'Output as JSON'
    }
  },
  async run(args) {
    const artifact = await runProbe({
      universe_id: args.universe,
      agents: args.agents?.split(',')
    });

    if (args.json) {
      console.log(JSON.stringify(artifact, null, 2));
    } else {
      console.log(`Scan complete: ${artifact.summary.total} observations`);
    }
  }
});

// Export for CLI registration
export const probeExtensions = [
  probeCommand,
  // ... more commands
];
```

---

## 6. @unrdf/yawl Integration

### 6.1 Package Purpose

Provides workflow orchestration primitives for complex multi-step operations with observability.

### 6.2 Import Contract

```javascript
// Primary imports used by kgc-probe
import {
  createWorkflow,
  WorkflowBuilder,
  StepSchema,
  executeWorkflow,
  WorkflowResultSchema
} from '@unrdf/yawl';
```

### 6.3 API Signatures

```javascript
/**
 * Create a workflow definition
 * @param {Object} config - Workflow configuration
 * @param {string} config.id - Workflow identifier
 * @param {string} config.name - Human-readable name
 * @param {Step[]} config.steps - Workflow steps
 * @returns {Workflow}
 */
function createWorkflow(config);

/**
 * WorkflowBuilder - Fluent API for building workflows
 */
class WorkflowBuilder {
  /**
   * Add a step to the workflow
   * @param {string} id - Step identifier
   * @param {Function} handler - Step handler
   * @returns {WorkflowBuilder}
   */
  step(id, handler);

  /**
   * Add parallel steps
   * @param {Step[]} steps - Steps to run in parallel
   * @returns {WorkflowBuilder}
   */
  parallel(steps);

  /**
   * Build the workflow
   * @returns {Workflow}
   */
  build();
}

/**
 * Execute a workflow
 * @param {Workflow} workflow - Workflow to execute
 * @param {Object} context - Initial context
 * @returns {Promise<WorkflowResult>}
 */
async function executeWorkflow(workflow, context);
```

### 6.4 Usage in kgc-probe

```javascript
// In orchestrator.mjs - using YAWL for scan orchestration
import { WorkflowBuilder, executeWorkflow } from '@unrdf/yawl';

const scanWorkflow = new WorkflowBuilder()
  .step('initialize', async (ctx) => {
    ctx.runId = crypto.randomUUID();
    ctx.startTime = Date.now();
  })
  .parallel(
    // Run all agents in parallel
    agentIds.map(id => ({
      id: `agent:${id}`,
      handler: async (ctx) => executeAgent(id, ctx)
    }))
  )
  .step('guards', async (ctx) => {
    // Run guards sequentially
    for (const guard of guards) {
      const violations = guard.validate(ctx.observations);
      ctx.violations.push(...violations);
    }
  })
  .step('finalize', async (ctx) => {
    ctx.artifact = createArtifact(ctx);
    await ctx.storage.saveArtifact(ctx.artifact);
  })
  .build();

// Execute scan as workflow
const result = await executeWorkflow(scanWorkflow, { config, storage });
```

---

## 7. Cross-Cutting Concerns

### 7.1 No N3 Imports

**CRITICAL**: The codebase MUST NOT import directly from 'n3'. All RDF operations go through `@unrdf/oxigraph`.

```javascript
// FORBIDDEN - will cause validation failure
import { DataFactory } from 'n3';  // DO NOT DO THIS

// CORRECT - use oxigraph wrapper
import { dataFactory } from '@unrdf/oxigraph';
```

### 7.2 Zod for Runtime Validation

All integration points use Zod schemas for runtime validation:

```javascript
import { z } from 'zod';

// Validate incoming data at integration boundaries
const validated = SomeSchema.parse(externalData);
```

### 7.3 Error Handling at Boundaries

Wrap all integration calls with try-catch and meaningful error messages:

```javascript
async function callIntegration(params) {
  try {
    return await externalCall(params);
  } catch (err) {
    throw new Error(`Integration with @unrdf/package failed: ${err.message}`);
  }
}
```

---

## 8. Verification Checklist

Before declaring integration complete, verify:

- [ ] All imports resolve correctly (no module not found)
- [ ] No direct 'n3' imports in kgc-probe source
- [ ] All Zod schemas validate correctly
- [ ] Storage backends implement full interface
- [ ] CLI commands register and execute
- [ ] Hooks execute with expected behavior
- [ ] Workflows complete without hanging

---

## Document Metadata

| Property | Value |
|----------|-------|
| Version | 1.0.0 |
| Created | 2025-12-27 |
| Status | Authoritative |
| Maintainer | Agent-3 (System Architect) |
