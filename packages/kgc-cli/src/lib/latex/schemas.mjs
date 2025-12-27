/**
 * @fileoverview Global Zod schemas for LaTeX→PDF pipeline
 *
 * Centralized validation schemas for all agents:
 * - Agent 1 (Orchestrator): CompileOptions, CompileResult, CLIArgs
 * - Agent 5 (Lockfile): LockfileSchema
 * - Agent 6 (Diagnostics): DiagnosticsSchema
 * - Shared: VFS, Engine, paths
 *
 * Design:
 * - Runtime validation using Zod (no TypeScript in source)
 * - JSDoc types derived from schemas
 * - Strict validation with informative errors
 * - Deterministic defaults
 *
 * @module lib/latex/schemas
 */

import { z } from 'zod';

// =============================================================================
// Core Primitive Schemas
// =============================================================================

/**
 * LaTeX engine identifier
 * SwiftLaTeX supports xetex and pdftex
 */
export const EngineSchema = z.enum(['xetex', 'pdftex']);

/**
 * Absolute file path validation
 */
export const AbsolutePathSchema = z.string().min(1).refine(
  (path) => path.startsWith('/'),
  { message: 'Path must be absolute (start with /)' }
);

/**
 * Relative file path validation
 */
export const RelativePathSchema = z.string().min(1).refine(
  (path) => !path.startsWith('/'),
  { message: 'Path must be relative (not start with /)' }
);

/**
 * SHA-256 hash (64 hex chars)
 */
export const Sha256HashSchema = z.string().regex(/^[a-f0-9]{64}$/i, {
  message: 'Must be 64-character SHA-256 hex hash'
});

/**
 * Short hash (16 hex chars)
 */
export const ShortHashSchema = z.string().regex(/^[a-f0-9]{16}$/i, {
  message: 'Must be 16-character hex hash'
});

/**
 * ISO 8601 timestamp
 */
export const TimestampSchema = z.string().datetime();

// =============================================================================
// VFS (Virtual File System) Schemas
// =============================================================================

/**
 * Virtual file system entry
 */
export const VFSEntrySchema = z.object({
  path: RelativePathSchema,
  content: z.instanceof(Uint8Array),
  size: z.number().int().nonnegative(),
  hash: Sha256HashSchema.optional()
});

/**
 * VFS validation result
 */
export const VFSValidationSchema = z.object({
  valid: z.boolean(),
  errors: z.array(z.string()),
  warnings: z.array(z.string()).optional(),
  stats: z.object({
    totalFiles: z.number().int().nonnegative(),
    totalSize: z.number().int().nonnegative(),
    texFiles: z.number().int().nonnegative(),
    imageFiles: z.number().int().nonnegative()
  }).optional()
});

// =============================================================================
// Compilation Schemas (Agent 1 + Agent 3)
// =============================================================================

/**
 * Main compilation options schema for compileLatexToPdf()
 *
 * Used by:
 * - CLI commands (latex build)
 * - Direct API calls
 * - Integration tests
 */
export const CompileOptionsSchema = z.object({
  /** Absolute path to main .tex file */
  inputTexPath: z.string().min(1),

  /** Absolute path to project directory */
  projectDir: z.string().min(1),

  /** LaTeX engine (xetex or pdftex) */
  engine: EngineSchema.default('xetex'),

  /** Cache directory (absolute or relative to projectDir) */
  cacheDir: z.string().optional(),

  /** Number of compilation passes (for cross-refs, ToC, etc.) */
  passes: z.number().int().min(1).max(5).default(2),

  /** CTAN mirror URL for package resolution */
  ctanMirror: z.string().url().optional(),

  /** Enable verbose logging */
  verbose: z.boolean().default(false),

  /** Fail fast on first error (vs. trying to resolve) */
  failFast: z.boolean().default(false)
});

/**
 * Compilation result schema (return type of compileLatexToPdf)
 *
 * Success case: { ok: true, pdfBytes, log, diagnostics }
 * Failure case: { ok: false, log, diagnostics, error }
 */
export const CompileResultSchema = z.object({
  /** Compilation succeeded */
  ok: z.boolean(),

  /** Generated PDF bytes (present if ok=true) */
  pdfBytes: z.instanceof(Uint8Array).optional(),

  /** Compilation log output */
  log: z.string(),

  /** Structured diagnostics */
  diagnostics: z.object({
    errors: z.array(z.string()),
    warnings: z.array(z.string()),
    missingInputs: z.array(z.string())
  }),

  /** Intermediate artifacts (.aux, .toc, .bbl, etc.) */
  artifacts: z.record(z.string(), z.instanceof(Uint8Array)).optional(),

  /** Error message (present if ok=false) */
  error: z.string().optional(),

  /** Metadata */
  metadata: z.object({
    engine: EngineSchema,
    passes: z.number().int(),
    cycles: z.number().int(),
    timestamp: TimestampSchema,
    cacheKey: ShortHashSchema.optional()
  }).optional()
});

// =============================================================================
// CLI Args Schemas (Agent 1)
// =============================================================================

/**
 * CLI args: latex build
 */
export const CLIBuildArgsSchema = z.object({
  input: z.string().describe('Path to main .tex file'),
  output: z.string().default('dist/output.pdf').describe('Output PDF path'),
  engine: EngineSchema.default('xetex').describe('LaTeX engine'),
  cacheDir: z.string().optional().describe('Cache directory'),
  passes: z.number().int().min(1).max(5).default(2).describe('Compilation passes'),
  projectRoot: z.string().optional().describe('Project root directory'),
  verbose: z.boolean().default(false).describe('Verbose logging')
});

/**
 * CLI args: latex diagnose
 */
export const CLIDiagnoseArgsSchema = z.object({
  input: z.string().describe('Path to main .tex file'),
  projectRoot: z.string().optional().describe('Project root directory'),
  output: z.string().optional().describe('Diagnostic report output path')
});

/**
 * CLI args: latex cache add
 */
export const CLICacheAddArgsSchema = z.object({
  package: z.string().describe('Package name (e.g., amsmath, geometry)'),
  cacheDir: z.string().optional().describe('Cache directory'),
  ctanMirror: z.string().url().optional().describe('CTAN mirror URL')
});

/**
 * CLI args: latex cache verify
 */
export const CLICacheVerifyArgsSchema = z.object({
  cacheDir: z.string().optional().describe('Cache directory to verify'),
  fix: z.boolean().default(false).describe('Automatically fix issues')
});

/**
 * CLI args: latex bundle make
 */
export const CLIBundleMakeArgsSchema = z.object({
  input: z.string().describe('Path to main .tex file'),
  output: z.string().default('bundle.zip').describe('Output bundle path'),
  includeCache: z.boolean().default(false).describe('Include cached packages'),
  projectRoot: z.string().optional().describe('Project root directory')
});

// =============================================================================
// Lockfile Schemas (Agent 5)
// =============================================================================

/**
 * Resolved dependency entry in lockfile
 */
export const ResolvedDependencySchema = z.object({
  /** Input file name (e.g., amsmath.sty, article.cls) */
  inputName: z.string(),

  /** SHA-256 hash of resolved content */
  hash: Sha256HashSchema,

  /** Source URL (CTAN or local) */
  sourceUrl: z.string().url().optional(),

  /** Cached VFS path */
  cachedPath: z.string().optional(),

  /** Resolution timestamp */
  resolvedAt: TimestampSchema.optional(),

  /** Package version (if available) */
  version: z.string().optional()
});

/**
 * Lockfile structure (latex.lock.json)
 *
 * Format:
 * {
 *   "version": "1.0.0",
 *   "engine": "xetex",
 *   "createdAt": "2025-01-15T10:30:00Z",
 *   "updatedAt": "2025-01-15T10:30:00Z",
 *   "resolved": { ... }
 * }
 */
export const LockfileSchema = z.object({
  /** Lockfile format version */
  version: z.string().default('1.0.0'),

  /** LaTeX engine used */
  engine: EngineSchema,

  /** Creation timestamp */
  createdAt: TimestampSchema,

  /** Last update timestamp */
  updatedAt: TimestampSchema,

  /** Resolved dependencies (filename -> metadata) */
  resolved: z.record(z.string(), ResolvedDependencySchema).default({}),

  /** Lockfile metadata */
  metadata: z.object({
    projectRoot: z.string().optional(),
    totalDependencies: z.number().int().nonnegative().optional()
  }).optional()
});

// =============================================================================
// Diagnostics Schemas (Agent 6)
// =============================================================================

/**
 * Single diagnostic entry (error or warning)
 */
export const DiagnosticEntrySchema = z.object({
  /** Severity level */
  level: z.enum(['error', 'warning', 'info']),

  /** Error/warning message */
  message: z.string(),

  /** Source file (if known) */
  file: z.string().optional(),

  /** Line number (if known) */
  line: z.number().int().positive().optional(),

  /** Column number (if known) */
  column: z.number().int().positive().optional(),

  /** Error code (e.g., 'missing-file', 'undefined-control-sequence') */
  code: z.string().optional(),

  /** Additional context */
  context: z.string().optional()
});

/**
 * Diagnostics collection schema
 */
export const DiagnosticsSchema = z.object({
  /** All errors */
  errors: z.array(DiagnosticEntrySchema),

  /** All warnings */
  warnings: z.array(DiagnosticEntrySchema),

  /** Missing input files detected */
  missingInputs: z.array(z.string()),

  /** Summary statistics */
  summary: z.object({
    totalErrors: z.number().int().nonnegative(),
    totalWarnings: z.number().int().nonnegative(),
    totalMissing: z.number().int().nonnegative()
  }),

  /** Timestamp of diagnostics generation */
  timestamp: TimestampSchema,

  /** Log file path (if written) */
  logFile: z.string().optional()
});

// =============================================================================
// CTAN Resolver Schemas (Agent 4)
// =============================================================================

/**
 * CTAN package metadata
 */
export const CTANPackageSchema = z.object({
  /** Package name */
  name: z.string(),

  /** Package version */
  version: z.string().optional(),

  /** Download URL */
  url: z.string().url(),

  /** File size in bytes */
  size: z.number().int().nonnegative().optional(),

  /** SHA-256 hash (if available) */
  hash: Sha256HashSchema.optional(),

  /** Package description */
  description: z.string().optional()
});

/**
 * Resolution result for missing inputs
 */
export const ResolutionResultSchema = z.object({
  /** Successfully resolved inputs (VFS path -> content) */
  resolved: z.record(z.string(), z.instanceof(Uint8Array)),

  /** Failed to resolve */
  failed: z.array(z.string()),

  /** Resolution metadata */
  metadata: z.object({
    ctanMirror: z.string().url().optional(),
    totalResolved: z.number().int().nonnegative(),
    totalFailed: z.number().int().nonnegative(),
    timestamp: TimestampSchema
  })
});

// =============================================================================
// Cache Schemas
// =============================================================================

/**
 * Cache entry metadata
 */
export const CacheEntrySchema = z.object({
  /** Cache key (hash) */
  key: ShortHashSchema,

  /** Cached file path */
  path: z.string(),

  /** Content hash */
  hash: Sha256HashSchema,

  /** File size */
  size: z.number().int().nonnegative(),

  /** Creation timestamp */
  createdAt: TimestampSchema,

  /** Last access timestamp */
  accessedAt: TimestampSchema.optional(),

  /** Access count */
  accessCount: z.number().int().nonnegative().default(0)
});

/**
 * Cache manifest (cache/manifest.json)
 */
export const CacheManifestSchema = z.object({
  version: z.string().default('1.0.0'),
  entries: z.record(z.string(), CacheEntrySchema),
  metadata: z.object({
    totalEntries: z.number().int().nonnegative(),
    totalSize: z.number().int().nonnegative(),
    lastCleanup: TimestampSchema.optional()
  }).optional()
});

// =============================================================================
// WASM Engine Schemas (Agent 3)
// =============================================================================

/**
 * Engine instance status
 */
export const EngineStatusSchema = z.object({
  engine: EngineSchema,
  available: z.boolean(),
  wasmPath: z.string(),
  version: z.string().optional(),
  initialized: z.boolean().default(false)
});

/**
 * Engine compilation pass result
 */
export const EnginePassResultSchema = z.object({
  passNumber: z.number().int().positive(),
  status: z.number().int(),
  pdfGenerated: z.boolean(),
  log: z.string(),
  duration: z.number().nonnegative().optional()
});

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Parse and validate compile options with defaults
 * @param {unknown} input - Raw input object
 * @returns {z.infer<typeof CompileOptionsSchema>}
 * @throws {z.ZodError} If validation fails
 */
export function parseCompileOptions(input) {
  return CompileOptionsSchema.parse(input);
}

/**
 * Parse and validate CLI build args
 * @param {unknown} input - Raw CLI args
 * @returns {z.infer<typeof CLIBuildArgsSchema>}
 * @throws {z.ZodError} If validation fails
 */
export function parseCLIBuildArgs(input) {
  return CLIBuildArgsSchema.parse(input);
}

/**
 * Validate lockfile structure
 * @param {unknown} input - Raw lockfile object
 * @returns {z.infer<typeof LockfileSchema>}
 * @throws {z.ZodError} If validation fails
 */
export function parseLockfile(input) {
  return LockfileSchema.parse(input);
}

/**
 * Create default diagnostics object
 * @returns {z.infer<typeof DiagnosticsSchema>}
 */
export function createEmptyDiagnostics() {
  return {
    errors: [],
    warnings: [],
    missingInputs: [],
    summary: {
      totalErrors: 0,
      totalWarnings: 0,
      totalMissing: 0
    },
    timestamp: new Date().toISOString()
  };
}

// =============================================================================
// Type Exports (for JSDoc)
// =============================================================================

/**
 * @typedef {z.infer<typeof CompileOptionsSchema>} CompileOptions
 * @typedef {z.infer<typeof CompileResultSchema>} CompileResult
 * @typedef {z.infer<typeof LockfileSchema>} Lockfile
 * @typedef {z.infer<typeof DiagnosticsSchema>} Diagnostics
 * @typedef {z.infer<typeof DiagnosticEntrySchema>} DiagnosticEntry
 * @typedef {z.infer<typeof ResolvedDependencySchema>} ResolvedDependency
 * @typedef {z.infer<typeof VFSValidationSchema>} VFSValidation
 * @typedef {z.infer<typeof CLIBuildArgsSchema>} CLIBuildArgs
 * @typedef {z.infer<typeof CLIDiagnoseArgsSchema>} CLIDiagnoseArgs
 * @typedef {z.infer<typeof CTANPackageSchema>} CTANPackage
 * @typedef {z.infer<typeof ResolutionResultSchema>} ResolutionResult
 * @typedef {z.infer<typeof CacheEntrySchema>} CacheEntry
 * @typedef {z.infer<typeof CacheManifestSchema>} CacheManifest
 * @typedef {z.infer<typeof EngineStatusSchema>} EngineStatus
 */

// =============================================================================
// Module Exports
// =============================================================================

export default {
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
};
