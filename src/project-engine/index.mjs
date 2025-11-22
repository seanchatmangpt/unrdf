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
