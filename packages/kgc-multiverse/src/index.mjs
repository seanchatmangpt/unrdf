/**
 * KGC Multiverse - Main Entry Point
 * Universe branching, forking, and morphism algebra for knowledge graphs
 *
 * @module @unrdf/kgc-multiverse
 */

// Universe Manager
export {
  UniverseState,
  generateQStarID,
  UniverseManager,
} from './universe-manager.mjs';

// Morphisms
export {
  MorphismType,
  generateMorphismID,
  createMorphism,
  applyMorphism,
  composeMorphisms,
  createIdentityMorphism,
  createPredicateRenameMorphism,
  createFilterMorphism,
  createMapMorphism,
} from './morphism.mjs';

// Schema Morphisms
export {
  createClassRenameMorphism,
  createPropertyRenameMorphism,
  createNamespaceMigrationMorphism,
  createDatatypeConversionMorphism,
  createPropertyChainMorphism,
} from './schema-morphism.mjs';

// Guards
export {
  guardStateTransition,
  guardMorphismApplication,
  guardMergePreconditions,
  guardFreezePreconditions,
  guardDeleteSafety,
  guardCanFork,
  guardNoUnfreeze,
  guardUniverseState,
  guardQStarID,
  guardUniverseHash,
  allGuards,
} from './guards.mjs';

// Q* Validation System
export {
  QStarErrorCode,
  QStarIDSchema,
  QStarRDFSchema,
  QStarPROVSchema,
  QStarSnapshotSchema,
  extractIRIs,
  computeCanonicalHash,
  createQStarSnapshot,
  QStarValidator,
  createQStarValidator,
  validateQStarSnapshot,
} from './q-star.mjs';

// Composition Engine
export {
  CompositionErrorCode,
  CompositionEngine,
  createCompositionEngine,
  quickCompose,
  verifyAlgebraicLaws,
} from './composition.mjs';

// Parallel Executor (Phase 4)
export {
  ParallelExecutor,
  createParallelExecutor,
  DEFAULT_CONFIG,
  benchmark10k,
} from './parallel-executor.mjs';

// Worker Tasks
export {
  TaskType,
} from './worker-task.mjs';
