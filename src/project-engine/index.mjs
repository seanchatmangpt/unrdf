/**
 * @file Project engine exports
 * @module project-engine
 */

export { scanFileSystemToStore } from './fs-scan.mjs'
export { buildProjectModelFromFs } from './project-model.mjs'
export { detectStackFromFs } from './stack-detect.mjs'
export { classifyFiles } from './file-roles.mjs'
export { generateGoldenStructure } from './golden-structure.mjs'
export { diffProjectStructure } from './project-diff.mjs'
export { materializeArtifacts } from './materialize.mjs'
export { ProjectStructureLens } from './lens/project-structure.mjs'
export {
  getProjectEngineConfig,
  ProjectEngineConfigSchema,
} from './project-config.mjs'
export { buildProjectReport } from './project-report.mjs'
export { createProjectInitializationPipeline } from './initialize.mjs'
export {
  deriveHooksFromStructure,
  analyzePatternViolations,
  createCustomPatternHook,
} from './policy-derivation.mjs'
export {
  inferDomainModel,
  inferDomainModelFromPath,
  DomainModelLens,
} from './domain-infer.mjs'
export {
  inferTemplatesFromProject,
  inferTemplatesWithDomainBinding,
  getTemplatesByKind,
  serializeTemplates,
} from './template-infer.mjs'

// Materialization planning and execution
export {
  planMaterialization,
  validatePlan,
  createEmptyPlan,
  mergePlans,
} from './materialize-plan.mjs'

export {
  applyMaterializationPlan,
  rollbackMaterialization,
  previewPlan,
  checkPlanApplicability,
} from './materialize-apply.mjs'

// Drift detection
export {
  createStructureSnapshot,
  computeDrift,
  createEmptyBaseline,
  serializeSnapshot,
  deserializeSnapshot,
} from './drift-snapshot.mjs'
