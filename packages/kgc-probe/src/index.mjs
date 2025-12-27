/**
 * @fileoverview KGC Probe - Public API
 *
 * High-level entry points for:
 * - Creating probe orchestrator instances
 * - Registering guards and agents
 * - Running scans and merging results
 * - Validating artifacts
 *
 * @module @unrdf/kgc-probe
 */

// Orchestrator
export { createProbeOrchestrator, ProbeOrchestrator } from './orchestrator.mjs';

// Guards Registry
export { createGuardRegistry, GuardRegistry } from './guards.mjs';

// Observation Validator
export { createObservationValidator, ObservationValidator } from './artifact.mjs';

// Agents (Factory Functions)
export {
  createAgentRegistry,
  createCompletionAgent,
  createConsistencyAgent,
  createConformanceAgent,
  createCoverageAgent,
  createCachingAgent,
  createCompletenessAgent,
  createCoherenceAgent,
  createClusteringAgent,
  createClassificationAgent,
  createCollaborationAgent
} from './agents/index.mjs';

// Storage Backends
export {
  createMemoryStorage,
  createFileStorage,
  createDatabaseStorage
} from './storage/index.mjs';

// Artifact Operations
export {
  mergeShards,
  diffArtifacts,
  verifyArtifact,
  serializeArtifact,
  deserializeArtifact,
  hashObservations
} from './artifact.mjs';

// Convenience: Full Scan
export { runProbe } from './probe.mjs';

// Types & Schemas
export * from './types.mjs';

// Default export (package metadata)
export default {
  name: '@unrdf/kgc-probe',
  version: '1.0.0',
  description: 'KGC Probe - Automated integrity scanning with 10 agents'
};
