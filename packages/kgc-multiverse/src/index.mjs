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
