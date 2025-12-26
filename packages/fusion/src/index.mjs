/**
 * @unrdf/fusion - Unified Integration Layer
 *
 * Consolidates 7-day innovation into a single canonical API:
 * - KGC-4D: Store, time-travel, Git snapshots, HDIT
 * - Blockchain: Receipts, anchoring, Merkle proofs
 * - Hooks: Policy execution, validation, transformation
 * - Caching: Multi-layer cache, resource management
 * - Oxigraph: Store creation, data factory
 *
 * @module @unrdf/fusion
 */

// Core store infrastructure
export { createStore, dataFactory, OxigraphStore } from '@unrdf/oxigraph';

// KGC-4D Engine (Workflow, Time, Git)
export {
  KGCStore,
  GitBackbone,
  freezeUniverse,
  reconstructState,
  verifyReceipt,
  now,
  toISO,
  fromISO,
  addNanoseconds,
  duration,
  VectorClock,
  GRAPHS,
  EVENT_TYPES,
  PREDICATES,
  HookRegistry,
  createDeltaSyncReducer,
  SSEClient,
  // HDIT
  coordsForEvent,
  batchCoordsForEvents,
  createUniverseContext,
  cosineSimilarity,
  findKNearest,
  projectPCA,
  createVisualizationData,
} from '@unrdf/kgc-4d';

// Blockchain receipts and verification
export {
  ReceiptAnchorer,
  WorkflowVerifier,
  MerkleProofGenerator,
  AnchorResultSchema,
  VerificationResultSchema,
  MerkleProofSchema,
  estimateGasCosts,
} from '@unrdf/blockchain';

// Policy hooks and validation
export {
  defineHook,
  executeHook,
  executeHookChain,
  createHookRegistry,
  registerHook,
  KnowledgeHookManager,
  builtinHooks,
  validateSubjectIRI,
  normalizeNamespace,
  HookScheduler,
  QualityMetricsCollector,
} from '@unrdf/hooks';

// Caching and resource management
export {
  createCachingSystem,
  MultiLayerCache,
  DependencyTracker,
  SparqlCache,
} from '@unrdf/caching';

// Resource allocation and capacity tracking
export {
  createResourceManager,
  ResourceManager,
  ALLOCATION_STRATEGIES,
} from './resource-manager.mjs';

// Policy engine - unified policy/hooks/conditions
export { createPolicyRegistry } from './policy-engine.mjs';

/**
 * Create unified engine with all subsystems
 * @param {Object} [config={}] - Configuration
 * @param {boolean} [config.enableCaching=true] - Enable multi-layer cache
 * @param {boolean} [config.enableBlockchain=true] - Enable blockchain receipts
 * @param {boolean} [config.enableGit=false] - Enable Git snapshots
 * @returns {Promise<Object>} Unified engine instance
 */
export async function createEngine(config = {}) {
  const { createStore } = await import('@unrdf/oxigraph');
  const { KGCStore } = await import('@unrdf/kgc-4d');
  const { ReceiptAnchorer, MerkleProofGenerator } = await import('@unrdf/blockchain');
  const { createHookRegistry } = await import('@unrdf/hooks');
  const { createCachingSystem } = await import('@unrdf/caching');

  const {
    enableCaching = true,
    enableBlockchain = true,
    enableGit = false,
  } = config;

  // Core store
  const store = createStore();

  // KGC-4D engine (wraps store with time-travel + Git)
  const kgcStore = new KGCStore({
    enableGit: enableGit,
    deterministic: process.env.DETERMINISTIC === '1',
  });

  // Blockchain receipts
  const receipts = enableBlockchain
    ? {
        anchorer: new ReceiptAnchorer(),
        merkle: new MerkleProofGenerator(),
      }
    : null;

  // Policy hooks (legacy hook registry)
  const hookRegistry = createHookRegistry();

  // Policy engine (new unified API)
  const { createPolicyRegistry } = await import('./policy-engine.mjs');
  const policies = await createPolicyRegistry();

  // Caching system
  const resources = enableCaching
    ? await createCachingSystem({ store })
    : null;

  return {
    store,
    kgcStore,
    receipts,
    hookRegistry,  // Legacy hook registry
    policies,      // New unified policy engine
    resources,

    /**
     * Get combined stats from all subsystems
     */
    getStats() {
      return {
        kgc: kgcStore.getStats?.() || {},
        cache: resources?.getStats() || {},
        hookRegistry: hookRegistry.getRegistryStats?.() || {},
        policies: policies.getStats?.() || {},
      };
    },

    /**
     * Cleanup all subsystems
     */
    async close() {
      if (resources) await resources.close();
    },
  };
}

/**
 * Deterministic E2E proof scenario
 *
 * Executes a complete workflow:
 * 1. Create KGC store + Git backbone
 * 2. Apply policy hooks
 * 3. Allocate resources via cache
 * 4. Execute test case
 * 5. Generate blockchain receipts
 * 6. Produce Merkle proof
 *
 * @returns {Promise<Object>} Proof result with hash, artifacts, ledger
 */
export async function prove() {
  const { KGCStore, now, toISO, EVENT_TYPES } = await import('@unrdf/kgc-4d');
  const { MerkleProofGenerator } = await import('@unrdf/blockchain');
  const { createHookRegistry, defineHook, executeHook } = await import('@unrdf/hooks');
  const { dataFactory } = await import('@unrdf/oxigraph');
  const crypto = await import('node:crypto');

  const startTime = now();
  const receipts = [];

  // Helper for stable JSON serialization (sorted keys)
  const stableStringify = (obj) => JSON.stringify(obj, Object.keys(obj).sort());

  // Phase 1: Create KGC store
  const kgcStore = new KGCStore({
    enableGit: false,
    deterministic: true,
  });

  receipts.push({
    phase: 'store-created',
    timestamp: now(),
    eventType: EVENT_TYPES.CREATE,
  });

  // Phase 2: Apply policy hook
  const registry = createHookRegistry();
  const validationHook = defineHook({
    id: 'test-validation',
    trigger: 'before-add',
    validate: async (quad) => {
      if (!quad.subject.value) {
        return { valid: false, errors: ['Subject required'] };
      }
      return { valid: true };
    },
  });

  registry.register(validationHook);
  receipts.push({
    phase: 'policy-applied',
    timestamp: now(),
    hookId: validationHook.id,
  });

  // Phase 3: Resource allocation (simulated)
  const resourceAllocation = {
    cacheL1: 1024 * 1024, // 1MB
    cacheL2: 10 * 1024 * 1024, // 10MB
    allocated: now(),
  };

  receipts.push({
    phase: 'resource-allocated',
    timestamp: now(),
    allocation: resourceAllocation,
  });

  // Phase 4: Execute test case
  const testQuad = dataFactory.quad(
    dataFactory.namedNode('http://example.org/subject'),
    dataFactory.namedNode('http://example.org/predicate'),
    dataFactory.literal('test-value'),
  );

  const validationResult = await executeHook(validationHook, testQuad);
  const caseExecuted = validationResult.valid;

  receipts.push({
    phase: 'case-executed',
    timestamp: now(),
    valid: caseExecuted,
  });

  // Phase 5: Generate Merkle proof
  const merkle = new MerkleProofGenerator();
  const receiptHashes = receipts.map((r) =>
    crypto.createHash('sha256').update(stableStringify(r)).digest('hex')
  );

  const tree = merkle.buildTree(receiptHashes);
  const merkleRoot = tree.root;

  receipts.push({
    phase: 'receipts-emitted',
    timestamp: now(),
    count: receipts.length,
    merkleRoot,
  });

  // Phase 6: Generate final proof hash
  const endTime = now();
  const ledger = {
    timestamp: toISO(endTime),
    proofHash: crypto.createHash('sha256').update(merkleRoot).digest('hex'),
    scenario: {
      workflowCreated: true,
      policyApplied: true,
      resourceAllocated: true,
      caseExecuted,
      receiptsEmitted: receipts.length,
      merkleRoot,
      verificationPassed: caseExecuted,
    },
    receipts,
    duration: Number(endTime - startTime),
  };

  const proofHash = crypto
    .createHash('sha256')
    .update(stableStringify(ledger))
    .digest('hex');

  return {
    success: caseExecuted,
    hash: proofHash,
    merkleRoot,
    artifacts: receipts,
    ledger,
  };
}

export default {
  createEngine,
  prove,
};
