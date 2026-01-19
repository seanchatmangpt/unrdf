/**
 * @file Project Engine - Self-hosting Tools and Infrastructure
 * @module @unrdf/project-engine
 * @description Development tools for project structure inference and materialization
 */

export { generateGoldenStructure } from './golden-structure.mjs';

// NOTE: materialize-apply.mjs temporarily disabled - missing dependencies (fs-scan.mjs, project-diff.mjs)
// export {
//   applyMaterializationPlan,
//   rollbackMaterialization,
//   previewPlan,
//   checkPlanApplicability,
// } from './materialize-apply.mjs';

// Stub exports for compatibility
export const scanFileSystemToStore = () => {
  throw new Error('scanFileSystemToStore not yet implemented');
};

export const analyzeJsComplexity = () => {
  throw new Error('analyzeJsComplexity not yet implemented');
};
