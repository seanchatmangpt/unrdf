/**
 * @fileoverview LaTeX→PDF Pipeline - Main Export Module
 *
 * Public API for the pure JavaScript LaTeX compilation pipeline.
 * Coordinates 10-agent swarm implementation:
 *
 * - Agent 1 (Orchestrator): Integration and schemas
 * - Agent 2 (VFS): Project file collection
 * - Agent 3 (Engine): SwiftLaTeX WASM wrapper
 * - Agent 4 (Resolver): CTAN package resolution
 * - Agent 5 (Lockfile): Dependency tracking
 * - Agent 6 (Diagnostics): Error parsing and reporting
 * - Agents 7-9: CLI, testing, documentation
 * - Agent 10 (Integrator): compile.mjs orchestration
 *
 * @module lib/latex
 */

// =============================================================================
// Main Compilation API (Agent 10)
// =============================================================================

export {
  compileLatexToPdf,
  generateCacheKey
} from './compile.mjs';

// =============================================================================
// SwiftLaTeX Engine (Agent 3)
// =============================================================================

export {
  compileWithSwiftLatex,
  getSupportedEngines,
  validateVFS,
  createMinimalVFS
} from './swiftlatex-engine.mjs';

// =============================================================================
// VFS Collection (Agent 2)
// =============================================================================

export {
  collectProjectFiles
} from './project-files.mjs';

// =============================================================================
// CTAN Resolver (Agent 4)
// =============================================================================

export {
  resolveMissingInputs,
  augmentVfsWithResolvedPackages,
  clearCache,
  getCacheStats
} from './ctan-resolver.mjs';

// =============================================================================
// Lockfile Management (Agent 5)
// =============================================================================

export {
  loadLatexLock,
  saveLatexLock,
  createLatexLock,
  recordResolvedInput,
  validateCachedFile,
  getResolvedInput,
  isLockValid,
  mergeLocks,
  pruneLock,
  ResolvedInputSchema,
  LatexLockSchema
} from './latex-lock.mjs';

// =============================================================================
// Diagnostics (Agent 6)
// =============================================================================

export {
  LatexCompileError,
  parseMissingInputsFromLog,
  writeDiagnosticLog,
  writeLatexRunLog,
  extractErrorSummary,
  isCompileSuccessful,
  LogWriteOptionsSchema
} from './diagnostics.mjs';

// =============================================================================
// Validation Schemas (Agent 1)
// =============================================================================

export {
  // Core schemas
  EngineSchema,
  CompileOptionsSchema,
  CompileResultSchema,
  LockfileSchema,
  DiagnosticsSchema,
  DiagnosticEntrySchema,

  // CLI schemas
  CLIBuildArgsSchema,
  CLIDiagnoseArgsSchema,
  CLICacheAddArgsSchema,
  CLICacheVerifyArgsSchema,
  CLIBundleMakeArgsSchema,

  // Supporting schemas
  ResolvedDependencySchema,
  VFSValidationSchema,
  CTANPackageSchema,
  ResolutionResultSchema,
  CacheEntrySchema,
  CacheManifestSchema,
  EngineStatusSchema,

  // Utility functions
  parseCompileOptions,
  parseCLIBuildArgs,
  parseLockfile,
  createEmptyDiagnostics
} from './schemas.mjs';

// =============================================================================
// Utilities
// =============================================================================

export {
  normalizeToVFS,
  vfsToRelative,
  isValidVFSPath,
  sortVFSPaths
} from './path-normalize.mjs';

// =============================================================================
// Re-export Default
// =============================================================================

import { compileLatexToPdf } from './compile.mjs';

/**
 * Default export: main compilation function
 * @type {typeof compileLatexToPdf}
 */
export default compileLatexToPdf;
