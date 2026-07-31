/**
 * @file Project Engine - Main Entry Point
 * @module @unrdf/project-engine
 * @description
 * Golden structure ontologies and materialization plan application
 * for code generation and project scaffolding.
 */

// Golden structure generation
export { generateGoldenStructure } from './golden-structure.mjs';

// Materialization plan application
export {
  applyMaterializationPlan,
  rollbackMaterialization,
  previewPlan,
  checkPlanApplicability,
} from './materialize-apply.mjs';
