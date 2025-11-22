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

// Hotspot analysis
export {
  analyzeHotspots,
  scoreFeature,
} from './hotspot-analyzer.mjs'

// Gap detection
export {
  findMissingRoles,
  scoreMissingRole,
} from './gap-finder.mjs'

// Type-safety auditing
export {
  auditTypeConsistency,
  auditEntityTypes,
  compareTypes,
  FieldInfoSchema,
  MismatchSchema,
  AuditResultSchema,
  CompareTypesResultSchema,
} from './type-auditor.mjs'

// Autonomic MAPEK Loop - Full autonomics with Knowledge Hooks
export {
  runMapekIteration,
  createAutonomicHooks,
  runContinuousMapekLoop,
  reportMapekStatus,
} from './autonomic-mapek.mjs'

// API Contract Validation
export {
  generateAPISchema,
  generateAllAPISchemas,
  validateAPIFiles,
  detectContractBreaks,
  detectAllContractBreaks,
  FieldSchemaSchema,
  EntitySchemaSchema,
  ViolationSchema,
  ValidationResultSchema,
  BreakingChangeSchema,
  ContractBreaksSchema,
} from './api-contract-validator.mjs'

// Stack-Aware Linter Rules
export {
  deriveLinterRules,
  analyzeCodePatterns,
  generateESLintConfig,
} from './stack-linter.mjs'

// Automated Refactoring Guide
export {
  planEntityRename,
  planEntityMerge,
  planServiceExtraction,
  validateRefactoringPlan,
} from './refactoring-guide.mjs'

// Feature Dependency Graph
export {
  buildDependencyGraph,
  detectCircularDependencies,
  topologicalSort,
  analyzeDependencyPath,
  getTransitiveDependencies,
  getTransitiveDependents,
  calculateImpactScore,
} from './dependency-graph.mjs'

// Auto-Test Generator
export {
  inferTestPatterns,
  generateTestSkeleton,
  scoreTestCoverage,
  generateTestFactory,
} from './auto-test-generator.mjs'

// Generative Documentation - auto-generate docs from domain model + project structure
export {
  generateEntityReference,
  generateAPIReference,
  generateArchitectureDiagram,
  generateCompleteDocumentation,
  DocGenerationResultSchema,
} from './doc-generator.mjs'

// Documentation Drift Checker - validate documentation consistency against domain model
export {
  checkDocConsistency,
  extractDocReferences,
  scoreDocDrift,
  checkDocDrift,
  DriftEntrySchema,
} from './doc-drift-checker.mjs'

// MAPEK Orchestration - Unified execution with all innovations
export {
  runFullMapekWithAllInnovations,
  runInnovationsParallel,
  aggregateInnovationFindings,
  ALL_INNOVATIONS,
} from './mapek-orchestration.mjs'
