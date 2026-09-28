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

// Project initialization pipeline (used by `unrdf init`)
export { createProjectInitializationPipeline, inferEntityName } from './initialize.mjs';

// Building blocks
export { scanFileSystemToStore, DEFAULT_IGNORE_PATTERNS } from './fs-scan.mjs';
export { detectStackFromFs } from './stack-detect.mjs';
export { buildProjectModelFromFs } from './project-model.mjs';
export { classifyFiles, classifyPath, ROLE_PATTERNS } from './file-roles.mjs';
export { diffProjectStructure } from './project-diff.mjs';
export { ProjectStructureLens } from './lens/project-structure.mjs';
