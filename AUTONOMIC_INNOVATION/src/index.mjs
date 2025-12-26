/**
 * @fileoverview UNRDF Autonomic Innovation - Public API
 * Complete system with 5 new primitives for knowledge graph evolution
 */

// Agent 2: Capsule IR & Deterministic Hashing
export {
  createCapsule,
  planCapsule,
  canonicalizeCapsule,
  hashCapsule,
  verifyCapsule,
} from '../agent-2/src/index.mjs';

// Agent 3: Lens Compiler
export {
  defineLens,
  compileLens,
  executeLensToGraph,
  executeLensFromGraph,
  createStableIRI,
} from '../agent-3/src/index.mjs';

// Agent 4: Diff as Program - Impact Sets
export {
  computeImpactSet,
  formatImpactSummary,
  impactSetToJSON,
} from '../agent-4/src/index.mjs';

// Agent 5: Diff as Program - Commutativity & Conflicts
export {
  canReorder,
  conflictCertificate,
} from '../agent-5/src/index.mjs';

// Agent 6: Conventions Profile Compiler
export {
  defineProfile,
  compileProfile,
  validateFileLayout,
  validateNaming,
  validateErrors,
  validateLogging,
  diagnosticReport,
} from '../agent-6/src/index.mjs';

// Agent 7: Convention-Preserving Generator
export {
  generateFacade,
  formatCode,
  validateGeneratedCode,
} from '../agent-7/src/index.mjs';

// Agent 8: Store Adapter & Atomic Apply
export {
  createAtomicStore,
  applyCapsule,
  applyBatch,
  queryStore,
  replayCapsules,
} from '../agent-8/src/index.mjs';

// Agent 9: Shadow Modes & Mismatch Reports
export {
  shadowWrite,
  shadowRead,
  partialServe,
  canonicalMismatchReport,
  hashMismatchReport,
} from '../agent-9/src/index.mjs';

// Agent 10: Quality Gates & E2E Tests
export {
  runQualityGates,
  validateDeterminism,
  runE2E,
} from '../agent-10/src/index.mjs';

// Shared utilities
export {
  canonicalJSON,
  hashDeterministic,
  sortObjectKeys,
  sortStrings,
  sortByField,
  verifyDeterminism,
  removeTimestamp,
} from './shared/determinism.mjs';
