/**
 * @fileoverview Zod schemas for KGC Markdown (.kgcmd) format
 * @module schemas/kgc-markdown
 * @version 1.0.0
 * @license MIT
 *
 * Complete validation schemas for:
 * - Frontmatter (YAML header)
 * - Executable blocks (kgc:query, kgc:proof, kgc:extract, kgc:render)
 * - Receipts (cryptographic proofs)
 * - Full KGC documents
 */

import { z } from 'zod';

// ============================================================================
// PRIMITIVES
// ============================================================================

/**
 * SHA-256 hash (64 hexadecimal characters)
 * @type {z.ZodString}
 */
export const Sha256HashSchema = z
  .string()
  .regex(/^[a-f0-9]{64}$/, 'Must be 64 hexadecimal characters (SHA-256 hash)');

/**
 * UUID v4 (RFC 4122)
 * @type {z.ZodString}
 */
export const UUIDv4Schema = z
  .string()
  .uuid('Must be a valid UUID v4')
  .regex(
    /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/,
    'Must be UUID v4 format',
  );

/**
 * Semantic version (semver 2.0.0)
 * @type {z.ZodString}
 */
export const SemverSchema = z
  .string()
  .regex(
    /^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$/,
    'Must be valid semver (e.g., 1.2.3, 1.0.0-alpha.1)',
  );

/**
 * ISO 8601 datetime with timezone
 * @type {z.ZodString}
 */
export const ISO8601DateTimeSchema = z.string().datetime({
  message: 'Must be valid ISO 8601 datetime with timezone (e.g., 2025-12-26T10:30:00Z)',
});

/**
 * Diátaxis view type
 * @type {z.ZodEnum}
 */
export const DiátaxisViewSchema = z.enum(['tutorial', 'how-to', 'reference', 'explanation'], {
  errorMap: () => ({ message: 'Must be one of: tutorial, how-to, reference, explanation' }),
});

/**
 * Determinism level for executable blocks
 * @type {z.ZodEnum}
 */
export const DeterminismLevelSchema = z.enum(['strict', 'lenient', 'best-effort'], {
  errorMap: () => ({ message: 'Must be one of: strict, lenient, best-effort' }),
});

/**
 * Expected output format
 * @type {z.ZodEnum}
 */
export const OutputFormatSchema = z.enum(['json', 'markdown', 'text'], {
  errorMap: () => ({ message: 'Must be one of: json, markdown, text' }),
});

// ============================================================================
// FRONTMATTER
// ============================================================================

/**
 * Resource bounds for executable blocks
 * @typedef {Object} Bounds
 * @property {number} maxQueries - Maximum SPARQL/query operations (1-10000)
 * @property {number} maxRuntime - Maximum milliseconds per block (100-60000)
 * @property {number} maxFileScans - Maximum files to scan (1-1000)
 */
export const BoundsSchema = z.object({
  maxQueries: z.number().int().min(1).max(10000),
  maxRuntime: z.number().int().min(100).max(60000),
  maxFileScans: z.number().int().min(1).max(1000),
});

/**
 * Source code range reference
 * @typedef {Object} SourceReference
 * @property {string} path - Relative file path from repo root
 * @property {number} lineStart - First line (1-indexed)
 * @property {number} lineEnd - Last line (1-indexed, inclusive)
 * @property {string} hash - SHA-256 of extracted range
 */
export const SourceReferenceSchema = z
  .object({
    path: z
      .string()
      .min(1)
      .refine((path) => !path.includes('..'), {
        message: 'Path must not contain ".." (no directory traversal)',
      }),
    lineStart: z.number().int().min(1),
    lineEnd: z.number().int().min(1),
    hash: Sha256HashSchema,
  })
  .refine((source) => source.lineEnd >= source.lineStart, {
    message: 'lineEnd must be >= lineStart',
  });

/**
 * Author information (optional)
 * @typedef {Object} Author
 * @property {string} name - Author name
 * @property {string} [role] - Author role
 */
export const AuthorSchema = z.object({
  name: z.string().min(1).max(100),
  role: z.string().min(1).max(50).optional(),
});

/**
 * Complete frontmatter schema (YAML header)
 * @typedef {Object} KGCMarkdownFrontmatter
 * @property {string} o_hash - Universe snapshot hash (SHA-256)
 * @property {string} policy_id - Policy pack UUID that admitted this doc
 * @property {string[]} receipts - Receipt hashes justifying content
 * @property {Bounds} bounds - Resource bounds for executable blocks
 * @property {string[]} views - Diátaxis views this document supports
 * @property {SourceReference[]} sources - Source code ranges
 * @property {string} version - Document version (semver)
 * @property {string} createdAt - Creation timestamp (ISO 8601)
 * @property {string} lastProved - Last proof verification timestamp
 * @property {string[]} [tags] - Optional tags for categorization
 * @property {Author[]} [authors] - Optional author information
 */
export const KGCMarkdownFrontmatterSchema = z
  .object({
    o_hash: Sha256HashSchema,
    policy_id: UUIDv4Schema,
    receipts: z.array(Sha256HashSchema).min(0).max(1000),
    bounds: BoundsSchema,
    views: z.array(DiátaxisViewSchema).min(1).max(4),
    sources: z.array(SourceReferenceSchema).min(0).max(100),
    version: SemverSchema,
    createdAt: ISO8601DateTimeSchema,
    lastProved: ISO8601DateTimeSchema,
    tags: z.array(z.string().min(1).max(50)).max(20).optional(),
    authors: z.array(AuthorSchema).optional(),
  })
  .refine((frontmatter) => new Date(frontmatter.lastProved) >= new Date(frontmatter.createdAt), {
    message: 'lastProved must be >= createdAt',
  });

// ============================================================================
// EXECUTABLE BLOCKS
// ============================================================================

/**
 * Base metadata for all executable blocks
 * @typedef {Object} BaseBlockMetadata
 * @property {string} receiptId - Receipt hash (SHA-256)
 * @property {string} expectedOutputFormat - Expected output format
 * @property {string} determinismLevel - Determinism level
 * @property {Object} metadata - Block-specific metadata
 */
const BaseBlockMetadataSchema = z.object({
  receiptId: Sha256HashSchema,
  expectedOutputFormat: OutputFormatSchema,
  determinismLevel: DeterminismLevelSchema,
  metadata: z.object({
    description: z.string().optional(),
  }),
});

/**
 * Result bounds for queries
 * @typedef {Object} ResultBounds
 * @property {number} minResults - Minimum expected results
 * @property {number} maxResults - Maximum allowed results
 */
const ResultBoundsSchema = z.object({
  minResults: z.number().int().min(0),
  maxResults: z.number().int().min(1),
});

/**
 * kgc:query block metadata
 * @typedef {Object} QueryBlockMetadata
 * @property {string} queryType - Query type (sparql, n3, shacl)
 * @property {ResultBounds} resultBounds - Result cardinality bounds
 * @property {number} timeout - Execution timeout in milliseconds
 * @property {string} [description] - Human-readable description
 */
export const QueryBlockMetadataSchema = BaseBlockMetadataSchema.extend({
  metadata: z.object({
    queryType: z.enum(['sparql', 'n3', 'shacl']),
    resultBounds: ResultBoundsSchema,
    timeout: z.number().int().min(100).max(60000),
    description: z.string().optional(),
  }),
});

/**
 * kgc:query block (complete)
 * @typedef {Object} QueryBlock
 * @property {string} type - Block type (always "kgc:query")
 * @property {QueryBlockMetadata} metadata - Block metadata
 * @property {string} body - SPARQL/N3/SHACL query text
 */
export const QueryBlockSchema = z.object({
  type: z.literal('kgc:query'),
  metadata: QueryBlockMetadataSchema,
  body: z.string().min(1),
});

/**
 * kgc:proof block metadata
 * @typedef {Object} ProofBlockMetadata
 * @property {string} proofType - Proof type (merkle, sequential, batch)
 * @property {boolean} verifyChain - Check full receipt chain
 * @property {boolean} validateSignatures - Verify cryptographic signatures
 * @property {string} [description] - Human-readable description
 */
export const ProofBlockMetadataSchema = BaseBlockMetadataSchema.extend({
  metadata: z.object({
    proofType: z.enum(['merkle', 'sequential', 'batch']),
    verifyChain: z.boolean(),
    validateSignatures: z.boolean(),
    description: z.string().optional(),
  }),
});

/**
 * kgc:proof block body
 * @typedef {Object} ProofBlockBody
 * @property {string[]} receiptIds - Receipt hashes to verify
 * @property {string} expectedRoot - Expected Merkle root hash
 */
const ProofBlockBodySchema = z.object({
  receiptIds: z.array(Sha256HashSchema).min(1),
  expectedRoot: Sha256HashSchema,
});

/**
 * kgc:proof block (complete)
 * @typedef {Object} ProofBlock
 * @property {string} type - Block type (always "kgc:proof")
 * @property {ProofBlockMetadata} metadata - Block metadata
 * @property {ProofBlockBody} body - Proof configuration
 */
export const ProofBlockSchema = z.object({
  type: z.literal('kgc:proof'),
  metadata: ProofBlockMetadataSchema,
  body: ProofBlockBodySchema,
});

/**
 * kgc:extract block metadata
 * @typedef {Object} ExtractBlockMetadata
 * @property {string} extractionType - What to extract (exports, types, functions, classes)
 * @property {string[]} fileGlobs - Glob patterns for files to scan
 * @property {boolean} includePrivate - Include private/internal APIs
 * @property {boolean} includeDocstrings - Extract JSDoc comments
 * @property {string} [description] - Human-readable description
 */
export const ExtractBlockMetadataSchema = BaseBlockMetadataSchema.extend({
  metadata: z.object({
    extractionType: z.enum(['exports', 'types', 'functions', 'classes']),
    fileGlobs: z.array(z.string().min(1)).min(1),
    includePrivate: z.boolean(),
    includeDocstrings: z.boolean(),
    description: z.string().optional(),
  }),
});

/**
 * kgc:extract block body
 * @typedef {Object} ExtractBlockBody
 * @property {string[]} [targetFiles] - Specific files (overrides globs)
 * @property {Object} [filters] - Additional filtering criteria
 */
const ExtractBlockBodySchema = z.object({
  targetFiles: z.array(z.string().min(1)).optional(),
  filters: z
    .object({
      visibility: z.enum(['public', 'private', 'all']).optional(),
      documented: z.boolean().optional(),
    })
    .optional(),
});

/**
 * kgc:extract block (complete)
 * @typedef {Object} ExtractBlock
 * @property {string} type - Block type (always "kgc:extract")
 * @property {ExtractBlockMetadata} metadata - Block metadata
 * @property {ExtractBlockBody} body - Extraction configuration
 */
export const ExtractBlockSchema = z.object({
  type: z.literal('kgc:extract'),
  metadata: ExtractBlockMetadataSchema,
  body: ExtractBlockBodySchema,
});

/**
 * kgc:render block metadata
 * @typedef {Object} RenderBlockMetadata
 * @property {string} templateName - Rendering template identifier
 * @property {string} sectionTitle - Heading for generated section
 * @property {boolean} includeTableOfContents - Generate TOC
 * @property {string} [description] - Human-readable description
 */
export const RenderBlockMetadataSchema = BaseBlockMetadataSchema.extend({
  metadata: z.object({
    templateName: z.string().min(1),
    sectionTitle: z.string().min(1),
    includeTableOfContents: z.boolean(),
    description: z.string().optional(),
  }),
});

/**
 * kgc:render block body (template-specific JSON)
 * @typedef {Object} RenderBlockBody
 */
const RenderBlockBodySchema = z.record(z.unknown()); // Template-specific, validated at runtime

/**
 * kgc:render block (complete)
 * @typedef {Object} RenderBlock
 * @property {string} type - Block type (always "kgc:render")
 * @property {RenderBlockMetadata} metadata - Block metadata
 * @property {RenderBlockBody} body - Rendering data
 */
export const RenderBlockSchema = z.object({
  type: z.literal('kgc:render'),
  metadata: RenderBlockMetadataSchema,
  body: RenderBlockBodySchema,
});

/**
 * Union of all executable block types
 * @typedef {QueryBlock | ProofBlock | ExtractBlock | RenderBlock} ExecutableBlock
 */
export const ExecutableBlockSchema = z.discriminatedUnion('type', [
  QueryBlockSchema,
  ProofBlockSchema,
  ExtractBlockSchema,
  RenderBlockSchema,
]);

// ============================================================================
// RECEIPTS
// ============================================================================

/**
 * Merkle proof structure
 * @typedef {Object} MerkleProof
 * @property {string[]} siblings - Sibling hashes at each level
 * @property {string} root - Merkle root hash
 * @property {number} index - Leaf index in tree
 * @property {number} totalLeaves - Total receipts in batch
 */
export const MerkleProofSchema = z.object({
  siblings: z.array(Sha256HashSchema),
  root: Sha256HashSchema,
  index: z.number().int().min(0),
  totalLeaves: z.number().int().min(1),
});

/**
 * Receipt decision
 * @type {z.ZodEnum}
 */
export const ReceiptDecisionSchema = z.enum(['ADMIT', 'REJECT', 'PARTIAL'], {
  errorMap: () => ({ message: 'Must be one of: ADMIT, REJECT, PARTIAL' }),
});

/**
 * Cryptographic receipt (proof of execution)
 * @typedef {Object} Receipt
 * @property {string} id - Receipt hash (SHA-256)
 * @property {string} timestamp - Execution timestamp (ISO 8601)
 * @property {string} o_hash - Universe snapshot hash
 * @property {string} block_type - Executable block type
 * @property {string} input_hash - Hash of input (query/config)
 * @property {string} output_hash - Hash of output
 * @property {string} decision - Admission decision
 * @property {Object} [metadata] - Block-specific metadata
 * @property {string[]} [dependencies] - Dependent receipt IDs
 * @property {MerkleProof} [merkle_proof] - Merkle tree proof
 */
export const ReceiptSchema = z.object({
  id: Sha256HashSchema,
  timestamp: ISO8601DateTimeSchema,
  o_hash: Sha256HashSchema,
  block_type: z.enum(['kgc:query', 'kgc:proof', 'kgc:extract', 'kgc:render']),
  input_hash: Sha256HashSchema,
  output_hash: Sha256HashSchema,
  decision: ReceiptDecisionSchema,
  metadata: z.record(z.unknown()).optional(),
  dependencies: z.array(Sha256HashSchema).optional(),
  merkle_proof: MerkleProofSchema.optional(),
});

/**
 * Receipt chain (DAG of receipts)
 * @typedef {Object} ReceiptChain
 * @property {Receipt[]} receipts - All receipts in chain
 * @property {string} rootHash - Root hash of chain
 */
export const ReceiptChainSchema = z.object({
  receipts: z.array(ReceiptSchema).min(1),
  rootHash: Sha256HashSchema,
});

// ============================================================================
// CONTENT SECTIONS
// ============================================================================

/**
 * Static markdown section
 * @typedef {Object} StaticSection
 * @property {string} type - Section type (always "static")
 * @property {string} content - Markdown content
 * @property {string} hash - SHA-256 of normalized markdown
 */
export const StaticSectionSchema = z.object({
  type: z.literal('static'),
  content: z.string(),
  hash: Sha256HashSchema,
});

/**
 * Dynamic markdown section (generated from executable block)
 * @typedef {Object} DynamicSection
 * @property {string} type - Section type (always "dynamic")
 * @property {string} sectionId - Section identifier
 * @property {string} receiptId - Receipt that generated this section
 * @property {string} content - Generated markdown content
 * @property {string} hash - SHA-256 of content (must match receipt output_hash)
 */
export const DynamicSectionSchema = z.object({
  type: z.literal('dynamic'),
  sectionId: z.string().min(1),
  receiptId: Sha256HashSchema,
  content: z.string(),
  hash: Sha256HashSchema,
});

/**
 * Hybrid section (mix of static prose and dynamic content)
 * @typedef {Object} HybridSection
 * @property {string} type - Section type (always "hybrid")
 * @property {(StaticSection | DynamicSection)[]} parts - Section parts
 * @property {string} hash - Merkle hash of all parts
 */
export const HybridSectionSchema = z.object({
  type: z.literal('hybrid'),
  parts: z.array(z.union([StaticSectionSchema, DynamicSectionSchema])).min(1),
  hash: Sha256HashSchema,
});

/**
 * Union of all section types
 * @typedef {StaticSection | DynamicSection | HybridSection} ContentSection
 */
export const ContentSectionSchema = z.discriminatedUnion('type', [
  StaticSectionSchema,
  DynamicSectionSchema,
  HybridSectionSchema,
]);

// ============================================================================
// COMPLETE DOCUMENT
// ============================================================================

/**
 * Complete KGC Markdown document
 * @typedef {Object} KGCDocument
 * @property {KGCMarkdownFrontmatter} frontmatter - YAML frontmatter
 * @property {string} title - Document title (H1 heading)
 * @property {ExecutableBlock[]} blocks - Executable blocks
 * @property {ContentSection[]} sections - Content sections
 * @property {Receipt[]} receipts - All receipts referenced in document
 */
export const KGCDocumentSchema = z.object({
  frontmatter: KGCMarkdownFrontmatterSchema,
  title: z.string().min(1),
  blocks: z.array(ExecutableBlockSchema),
  sections: z.array(ContentSectionSchema),
  receipts: z.array(ReceiptSchema),
});

// ============================================================================
// VALIDATION FUNCTIONS
// ============================================================================

/**
 * Validate frontmatter
 * @param {unknown} data - Raw frontmatter data
 * @returns {{success: true, data: KGCMarkdownFrontmatter} | {success: false, error: z.ZodError}} Validation result
 */
export function validateFrontmatter(data) {
  return KGCMarkdownFrontmatterSchema.safeParse(data);
}

/**
 * Validate executable block
 * @param {unknown} data - Raw block data
 * @returns {{success: true, data: ExecutableBlock} | {success: false, error: z.ZodError}} Validation result
 */
export function validateExecutableBlock(data) {
  return ExecutableBlockSchema.safeParse(data);
}

/**
 * Validate receipt
 * @param {unknown} data - Raw receipt data
 * @returns {{success: true, data: Receipt} | {success: false, error: z.ZodError}} Validation result
 */
export function validateReceipt(data) {
  return ReceiptSchema.safeParse(data);
}

/**
 * Validate complete KGC document
 * @param {unknown} data - Raw document data
 * @returns {{success: true, data: KGCDocument} | {success: false, error: z.ZodError}} Validation result
 */
export function validateKGCDocument(data) {
  return KGCDocumentSchema.safeParse(data);
}

/**
 * Validate receipt chain (no cycles, all dependencies exist)
 * @param {Receipt[]} receipts - Array of receipts
 * @returns {{valid: boolean, errors: string[]}} Validation result
 */
export function validateReceiptChain(receipts) {
  const errors = [];
  const receiptMap = new Map(receipts.map((r) => [r.id, r]));

  // Check all dependencies exist
  for (const receipt of receipts) {
    if (receipt.dependencies) {
      for (const depId of receipt.dependencies) {
        if (!receiptMap.has(depId)) {
          errors.push(`Receipt ${receipt.id} depends on missing receipt ${depId}`);
        }
      }
    }
  }

  // Check for cycles using DFS
  const visited = new Set();
  const recStack = new Set();

  /**
   * Detect cycle using DFS
   * @param {string} receiptId - Receipt ID to check
   * @returns {boolean} True if cycle detected
   */
  function hasCycle(receiptId) {
    if (recStack.has(receiptId)) return true;
    if (visited.has(receiptId)) return false;

    visited.add(receiptId);
    recStack.add(receiptId);

    const receipt = receiptMap.get(receiptId);
    if (receipt?.dependencies) {
      for (const depId of receipt.dependencies) {
        if (hasCycle(depId)) {
          return true;
        }
      }
    }

    recStack.delete(receiptId);
    return false;
  }

  for (const receipt of receipts) {
    if (hasCycle(receipt.id)) {
      errors.push(`Cyclic dependency detected involving receipt ${receipt.id}`);
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}

// ============================================================================
// ERROR FORMATTERS
// ============================================================================

/**
 * Format zod validation error as human-readable message
 * @param {z.ZodError} error - Zod validation error
 * @returns {string} Formatted error message
 */
export function formatValidationError(error) {
  return error.errors
    .map((err) => {
      const path = err.path.join('.');
      return `${path ? `[${path}]` : '[root]'} ${err.message}`;
    })
    .join('\n');
}

/**
 * Create InvalidFrontmatter error
 * @param {string} field - Field name
 * @param {string} issue - Issue description
 * @returns {Error} Error object
 */
export function createInvalidFrontmatterError(field, issue) {
  const error = new Error(`InvalidFrontmatter: ${field} ${issue}`);
  error.name = 'InvalidFrontmatter';
  error.field = field;
  error.severity = 'error';
  return error;
}

/**
 * Create MissingReceipt error
 * @param {string} receiptId - Receipt ID
 * @param {string} location - Where it was referenced
 * @returns {Error} Error object
 */
export function createMissingReceiptError(receiptId, location) {
  const error = new Error(`MissingReceipt: ${receiptId} referenced in ${location} but not found`);
  error.name = 'MissingReceipt';
  error.receiptId = receiptId;
  error.location = location;
  error.severity = 'error';
  return error;
}

/**
 * Create MismatchedHash error
 * @param {string} entity - Entity type
 * @param {string} expected - Expected hash
 * @param {string} actual - Actual hash
 * @returns {Error} Error object
 */
export function createMismatchedHashError(entity, expected, actual) {
  const error = new Error(
    `MismatchedHash: ${entity} hash mismatch\nExpected: ${expected}\nGot: ${actual}`,
  );
  error.name = 'MismatchedHash';
  error.entity = entity;
  error.expected = expected;
  error.actual = actual;
  error.severity = 'error';
  return error;
}

/**
 * Create BoundsExceeded error
 * @param {string} boundType - Bound type (maxQueries, maxRuntime, etc.)
 * @param {number} limit - Limit value
 * @param {number} actual - Actual value
 * @returns {Error} Error object
 */
export function createBoundsExceededError(boundType, limit, actual) {
  const error = new Error(`BoundsExceeded: ${boundType} exceeded\nLimit: ${limit}\nActual: ${actual}`);
  error.name = 'BoundsExceeded';
  error.boundType = boundType;
  error.limit = limit;
  error.actual = actual;
  error.severity = 'error';
  return error;
}

/**
 * Create NonDeterministic error/warning
 * @param {string} issue - Issue description
 * @param {string} blockType - Block type
 * @param {string} determinismLevel - Determinism level
 * @returns {Error} Error object
 */
export function createNonDeterministicError(issue, blockType, determinismLevel) {
  const error = new Error(
    `NonDeterministic: ${issue} in ${blockType} block\nDeterminism Level: ${determinismLevel}`,
  );
  error.name = 'NonDeterministic';
  error.issue = issue;
  error.blockType = blockType;
  error.determinismLevel = determinismLevel;
  error.severity = determinismLevel === 'strict' ? 'error' : 'warning';
  return error;
}

// ============================================================================
// EXPORTS
// ============================================================================

export default {
  // Schemas
  Sha256HashSchema,
  UUIDv4Schema,
  SemverSchema,
  ISO8601DateTimeSchema,
  DiátaxisViewSchema,
  DeterminismLevelSchema,
  OutputFormatSchema,
  BoundsSchema,
  SourceReferenceSchema,
  AuthorSchema,
  KGCMarkdownFrontmatterSchema,
  QueryBlockMetadataSchema,
  QueryBlockSchema,
  ProofBlockMetadataSchema,
  ProofBlockSchema,
  ExtractBlockMetadataSchema,
  ExtractBlockSchema,
  RenderBlockMetadataSchema,
  RenderBlockSchema,
  ExecutableBlockSchema,
  MerkleProofSchema,
  ReceiptDecisionSchema,
  ReceiptSchema,
  ReceiptChainSchema,
  StaticSectionSchema,
  DynamicSectionSchema,
  HybridSectionSchema,
  ContentSectionSchema,
  KGCDocumentSchema,

  // Validation functions
  validateFrontmatter,
  validateExecutableBlock,
  validateReceipt,
  validateKGCDocument,
  validateReceiptChain,

  // Error formatters
  formatValidationError,
  createInvalidFrontmatterError,
  createMissingReceiptError,
  createMismatchedHashError,
  createBoundsExceededError,
  createNonDeterministicError,
};
