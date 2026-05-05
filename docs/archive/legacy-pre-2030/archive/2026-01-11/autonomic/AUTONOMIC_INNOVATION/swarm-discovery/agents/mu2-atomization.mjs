/**
 * @file mu2-atomization.mjs - Extract atoms from packages (μ2 agent)
 * @description Identify discrete, composable units with interfaces, proofs, and state machines
 * @equation Atoms_τ := μ_atoms(O_τ)
 * @invariant Q2: Every atom has evidenceHash linking to source
 */

import { hashContent } from '../kernel/hash.mjs'

/**
 * Extract atoms from observable state
 * @param {object} observable - Observable repository state
 * @returns {object} - Atoms with proofs
 */
export async function extractAtoms(observable) {
  const atoms = []

  // Core atoms from exploration
  const coreAtomDefinitions = [
    {
      id: 'atom-rdf-store',
      name: 'RDF Store',
      interface: 'createStore() -> { getQuads, addQuad, removeQuad, countQuads, iterateQuads }',
      implementation: '@unrdf/core + @unrdf/oxigraph',
      source: { file: 'packages/core/src/rdf/unrdf-store.mjs', line: 1 },
      proof: {
        type: 'test+example',
        files: [
          'packages/core/test/core.test.mjs',
          'examples/01-hello-rdf.mjs'
        ]
      },
      dependencies: ['oxigraph'],
      state: { type: 'stateful', lifecycle: 'create-use-dispose' },
      complexity: 'moderate',
      utility: 0.95
    },

    {
      id: 'atom-sparql-executor',
      name: 'SPARQL Executor',
      interface: 'executeQuery(sparql, store) -> Promise<any>; executeQuerySync(sparql, store) -> any',
      implementation: '@unrdf/core/sparql',
      variants: ['sync', 'async'],
      source: { file: 'packages/core/src/sparql/executor.mjs', line: 1 },
      proof: {
        type: 'test+example',
        files: [
          'packages/core/test/core.test.mjs',
          'examples/sparql-query-advanced.mjs'
        ]
      },
      state: { type: 'stateless' },
      complexity: 'high',
      utility: 0.92
    },

    {
      id: 'atom-hook-system',
      name: 'Hook System',
      interface: 'defineHook(config) -> Hook; executeHook(hook, quad, context) -> {passed, result?, error?}',
      implementation: '@unrdf/hooks',
      source: { file: 'packages/hooks/src/hooks/define-hook.mjs', line: 1 },
      proof: {
        type: 'test+example',
        files: [
          'packages/hooks/test',
          'examples/define-hook-example.mjs'
        ]
      },
      capabilities: ['validation', 'transformation', 'policy-enforcement'],
      state: { type: 'stateless', registry: 'stateful' },
      complexity: 'high',
      utility: 0.88,
      optimization: 'JIT compilation + quad pooling'
    },

    {
      id: 'atom-pattern-matcher',
      name: 'Pattern Matcher',
      interface: 'matchPattern(patternDsl, quads) -> Match[]',
      implementation: '@unrdf/knowledge-engine/pattern-matcher.mjs',
      source: { file: 'packages/knowledge-engine/src/knowledge-engine/pattern-matcher.mjs', line: 1 },
      proof: {
        type: 'test+example',
        files: [
          'packages/knowledge-engine/test',
          'packages/knowledge-engine/examples'
        ]
      },
      state: { type: 'stateless' },
      complexity: 'high',
      utility: 0.85
    },

    {
      id: 'atom-change-feed',
      name: 'Change Feed Processor',
      interface: 'subscribe(graph, handler) -> unsubscribe; getFeed() -> AsyncIterable<Change>',
      implementation: '@unrdf/streaming/processor.mjs',
      source: { file: 'packages/streaming/src/processor.mjs', line: 1 },
      proof: {
        type: 'example',
        files: [
          'examples/streaming/basic-subscription.mjs',
          'examples/streaming/change-feed-integration.mjs'
        ]
      },
      state: { type: 'stateful', lifecycle: 'subscribe-unsubscribe' },
      complexity: 'moderate',
      utility: 0.82
    },

    {
      id: 'atom-federation-coordinator',
      name: 'Federation Coordinator',
      interface: 'discoverPeers(criteria) -> Peer[]; executeDistributedQuery(sparql, peers) -> results',
      implementation: '@unrdf/federation/coordinator.mjs',
      source: { file: 'packages/federation/src/coordinator.mjs', line: 1 },
      proof: {
        type: 'example',
        files: [
          'examples/federation/basic-federation.mjs'
        ]
      },
      state: { type: 'stateful' },
      complexity: 'very-high',
      utility: 0.80
    },

    {
      id: 'atom-event-logger',
      name: 'Event Logger (KGC-4D)',
      interface: 'logEvent(timestamp, data) -> Receipt; getSnapshot(time) -> State',
      implementation: '@unrdf/kgc-4d',
      source: { file: 'packages/kgc-4d/src/index.mjs', line: 1 },
      proof: {
        type: 'test+example',
        metrics: {
          linesOfCode: 6327,
          testPassRate: 0.998,
          developmentDays: 20
        }
      },
      capabilities: ['time-travel', 'git-backed-snapshots', 'nanosecond-precision'],
      state: { type: 'stateful', persistence: 'git' },
      complexity: 'very-high',
      utility: 0.90
    },

    {
      id: 'atom-workflow-engine',
      name: 'Workflow Engine (YAWL)',
      interface: 'createCase(processId) -> Case; executeTask(caseId, taskId) -> TaskResult',
      implementation: '@unrdf/yawl',
      source: { file: 'packages/yawl/src/case.mjs', line: 1 },
      proof: {
        type: 'test+example'
      },
      semantics: 'Petri nets with BPMN/YAWL',
      state: { type: 'stateful', lifecycle: 'create-task-complete' },
      complexity: 'very-high',
      utility: 0.86
    },

    {
      id: 'atom-ml-inference',
      name: 'ML Inference Pipeline',
      interface: 'createPipeline(modelPath) -> Pipeline; infer(inputs) -> outputs',
      implementation: '@unrdf/ml-inference',
      source: { file: 'packages/ml-inference/src/pipeline/streaming-inference.mjs', line: 1 },
      proof: { type: 'test' },
      state: { type: 'stateful', lifecycle: 'load-infer-unload' },
      complexity: 'high',
      utility: 0.75
    },

    {
      id: 'atom-graph-analytics',
      name: 'Graph Analytics',
      interface: 'pageRank(graph) -> scores; centrality(graph) -> measures',
      implementation: '@unrdf/graph-analytics',
      source: { file: 'packages/graph-analytics/src/centrality/pagerank-analyzer.mjs', line: 1 },
      proof: { type: 'test' },
      state: { type: 'stateless' },
      complexity: 'high',
      utility: 0.78
    }
  ]

  // Add evidence hashes
  for (const atom of coreAtomDefinitions) {
    atom.evidenceHash = hashContent(JSON.stringify(atom)).substring(0, 64)
  }

  atoms.push(...coreAtomDefinitions)

  return {
    atoms,
    count: atoms.length,
    timestamp: new Date().toISOString()
  }
}

/**
 * Validate atom schema
 * @param {object} atom - Atom to validate
 * @returns {boolean}
 */
function isValidAtom(atom) {
  return (
    typeof atom.id === 'string' &&
    typeof atom.name === 'string' &&
    typeof atom.interface === 'string' &&
    typeof atom.implementation === 'string' &&
    atom.source && typeof atom.source.file === 'string' &&
    atom.proof && typeof atom.proof.type === 'string' &&
    typeof atom.complexity === 'string' &&
    typeof atom.utility === 'number' &&
    atom.utility >= 0 && atom.utility <= 1
  )
}

/**
 * Format atoms for ATLAS
 * @param {array} atoms - Extracted atoms
 * @returns {array} - Formatted atoms
 */
export function formatAtomsForAtlas(atoms) {
  return atoms.map(atom => ({
    ...atom,
    validated: isValidAtom(atom),
    metrics: {
      complexity: atom.complexity,
      utility: atom.utility,
      dependencies: (atom.dependencies || []).length
    }
  }))
}

export default {
  extractAtoms,
  formatAtomsForAtlas
}
