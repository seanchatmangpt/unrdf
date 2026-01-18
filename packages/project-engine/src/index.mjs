/**
 * @file Project Engine - Self-hosting Tools and Infrastructure
 * @module @unrdf/project-engine
 * @description Development tools for project structure inference and materialization
 */

export { generateGoldenStructure } from './golden-structure.mjs';
export {
  applyMaterializationPlan,
  rollbackMaterialization,
  previewPlan,
  checkPlanApplicability,
} from './materialize-apply.mjs';
