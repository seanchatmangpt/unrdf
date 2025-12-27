/**
 * Shared Hooks Utilities for Dynamic KGC Markdown
 *
 * Common functions for frontmatter parsing, receipt validation,
 * denial receipt generation, and file system operations.
 *
 * @module hooks-shared
 */

import { readFile, writeFile, access, mkdir } from 'node:fs/promises';
import { resolve, dirname, join } from 'node:path';
import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { execSync } from 'node:child_process';
import { randomUUID } from 'node:crypto';

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Generate a UUID v4
 * @returns {string} UUID string
 */
export function generateUUID() {
  return randomUUID();
}

/**
 * Serialize object deterministically for hashing
 * Keys are sorted alphabetically at all levels
 *
 * @param {Object} obj - Object to serialize
 * @returns {string} Deterministic JSON string
 */
export function deterministicSerialize(obj) {
  if (obj === null || obj === undefined) {
    return JSON.stringify(null);
  }

  if (typeof obj === 'bigint') {
    return obj.toString();
  }

  if (typeof obj !== 'object') {
    return JSON.stringify(obj);
  }

  if (Array.isArray(obj)) {
    const items = obj.map((item) => deterministicSerialize(item));
    return `[${items.join(',')}]`;
  }

  // Sort keys alphabetically for deterministic ordering
  const sortedKeys = Object.keys(obj).sort();
  const pairs = sortedKeys.map((key) => {
    const value = obj[key];
    const serializedValue = deterministicSerialize(value);
    return `${JSON.stringify(key)}:${serializedValue}`;
  });

  return `{${pairs.join(',')}}`;
}

// =============================================================================
// Constants
// =============================================================================

export const RECEIPTS_DIR = '/home/user/unrdf/receipts';
export const DENIALS_DIR = '/home/user/unrdf/receipts/denials';
export const DOCS_DIR = '/home/user/unrdf/docs';

/**
 * Denial reason codes for structured error handling
 */
export const DENIAL_REASONS = {
  MISSING_FRONTMATTER: 'MISSING_FRONTMATTER',
  INVALID_FRONTMATTER: 'INVALID_FRONTMATTER',
  MISSING_O_HASH: 'MISSING_O_HASH',
  MISSING_POLICY_ID: 'MISSING_POLICY_ID',
  MISSING_RECEIPTS: 'MISSING_RECEIPTS',
  RECEIPT_NOT_FOUND: 'RECEIPT_NOT_FOUND',
  INVALID_RECEIPT: 'INVALID_RECEIPT',
  O_HASH_UNCHANGED: 'O_HASH_UNCHANGED',
  CONTENT_CHANGED_WITHOUT_HASH: 'CONTENT_CHANGED_WITHOUT_HASH',
  BOUNDS_EXCEEDED: 'BOUNDS_EXCEEDED',
  MAX_QUERIES_EXCEEDED: 'MAX_QUERIES_EXCEEDED',
  MAX_RUNTIME_EXCEEDED: 'MAX_RUNTIME_EXCEEDED',
  MAX_FILE_SCANS_EXCEEDED: 'MAX_FILE_SCANS_EXCEEDED',
  NON_DETERMINISTIC_OUTPUT: 'NON_DETERMINISTIC_OUTPUT',
  HASH_MISMATCH: 'HASH_MISMATCH',
};

// =============================================================================
// Zod Schemas
// =============================================================================

/**
 * KGC Frontmatter schema
 */
export const FrontmatterSchema = z.object({
  o_hash: z.string().min(1),
  policy_id: z.string().min(1),
  receipts: z.array(z.string()).min(0),
  maxQueries: z.number().optional(),
  maxRuntime: z.number().optional(), // milliseconds
  maxFileScans: z.number().optional(),
  expectedHash: z.string().optional(), // for determinism checks
  title: z.string().optional(),
  created: z.string().optional(),
  updated: z.string().optional(),
}).passthrough();

/**
 * Denial receipt schema
 */
export const DenialReceiptSchema = z.object({
  id: z.string().uuid(),
  timestamp: z.string(),
  operation: z.enum(['write', 'edit', 'query', 'proof', 'render']),
  targetPath: z.string(),
  reasonCode: z.enum(Object.values(DENIAL_REASONS)),
  message: z.string(),
  details: z.any().optional(),
  remediation: z.object({
    command: z.string().optional(),
    steps: z.array(z.string()),
    documentation: z.string().optional(),
  }),
  gitContext: z.object({
    branch: z.string().optional(),
    commit: z.string().optional(),
    uncommittedChanges: z.boolean(),
  }).optional(),
});

// =============================================================================
// Frontmatter Parsing
// =============================================================================

/**
 * Extract frontmatter from markdown content
 *
 * @param {string} content - Full markdown content
 * @returns {{ frontmatter: Object | null, content: string, raw: string }}
 */
export function extractFrontmatter(content) {
  const frontmatterRegex = /^---\n([\s\S]*?)\n---\n/;
  const match = content.match(frontmatterRegex);

  if (!match) {
    return { frontmatter: null, content, raw: '' };
  }

  const raw = match[1];
  const bodyContent = content.slice(match[0].length);

  try {
    // Parse YAML-like frontmatter
    const frontmatter = {};
    const lines = raw.split('\n');

    for (const line of lines) {
      if (!line.trim() || line.trim().startsWith('#')) continue;

      const colonIndex = line.indexOf(':');
      if (colonIndex === -1) continue;

      const key = line.slice(0, colonIndex).trim();
      const valueStr = line.slice(colonIndex + 1).trim();

      // Parse arrays
      if (valueStr.startsWith('[') && valueStr.endsWith(']')) {
        const arrayContent = valueStr.slice(1, -1);
        frontmatter[key] = arrayContent
          .split(',')
          .map(item => item.trim().replace(/['"]/g, ''))
          .filter(Boolean);
      }
      // Parse numbers
      else if (/^\d+(\.\d+)?$/.test(valueStr)) {
        frontmatter[key] = Number(valueStr);
      }
      // Parse strings
      else {
        frontmatter[key] = valueStr.replace(/['"]/g, '');
      }
    }

    return { frontmatter, content: bodyContent, raw };
  } catch (error) {
    return { frontmatter: null, content, raw };
  }
}

/**
 * Validate frontmatter against schema
 *
 * @param {Object} frontmatter - Parsed frontmatter
 * @returns {{ valid: boolean, error?: string, data?: Object }}
 */
export function validateFrontmatter(frontmatter) {
  try {
    const data = FrontmatterSchema.parse(frontmatter);
    return { valid: true, data };
  } catch (error) {
    return { valid: false, error: error.message };
  }
}

/**
 * Update frontmatter in markdown content
 *
 * @param {string} content - Original content
 * @param {Object} updates - Frontmatter updates
 * @returns {string} Updated content
 */
export function updateFrontmatter(content, updates) {
  const { frontmatter, content: bodyContent } = extractFrontmatter(content);

  const merged = { ...frontmatter, ...updates };

  // Serialize frontmatter
  const lines = ['---'];
  for (const [key, value] of Object.entries(merged)) {
    if (Array.isArray(value)) {
      lines.push(`${key}: [${value.map(v => `"${v}"`).join(', ')}]`);
    } else {
      lines.push(`${key}: ${value}`);
    }
  }
  lines.push('---', '');

  return lines.join('\n') + bodyContent;
}

// =============================================================================
// Receipt Validation
// =============================================================================

/**
 * Check if receipt file exists
 *
 * @param {string} receiptId - Receipt ID
 * @returns {Promise<{ exists: boolean, path?: string }>}
 */
export async function checkReceiptExists(receiptId) {
  const possiblePaths = [
    join(RECEIPTS_DIR, `${receiptId}.json`),
    join(RECEIPTS_DIR, 'proofs', `${receiptId}.json`),
    join(RECEIPTS_DIR, 'validations', `${receiptId}.json`),
  ];

  for (const path of possiblePaths) {
    try {
      await access(path);
      return { exists: true, path };
    } catch {
      continue;
    }
  }

  return { exists: false };
}

/**
 * Validate all receipts referenced in frontmatter
 *
 * @param {Array<string>} receiptIds - Receipt IDs from frontmatter
 * @returns {Promise<{ valid: boolean, missing: Array<string>, invalid: Array<string> }>}
 */
export async function validateReceipts(receiptIds) {
  const missing = [];
  const invalid = [];

  for (const receiptId of receiptIds) {
    const { exists, path } = await checkReceiptExists(receiptId);

    if (!exists) {
      missing.push(receiptId);
      continue;
    }

    try {
      const content = await readFile(path, 'utf-8');
      const receipt = JSON.parse(content);

      // Basic validation - check required fields
      if (!receipt.id || !receipt.receiptHash || !receipt.payloadHash) {
        invalid.push(receiptId);
      }
    } catch (error) {
      invalid.push(receiptId);
    }
  }

  return {
    valid: missing.length === 0 && invalid.length === 0,
    missing,
    invalid,
  };
}

// =============================================================================
// Git Context
// =============================================================================

/**
 * Get current git context
 *
 * @param {string} [cwd] - Working directory
 * @returns {Object} Git context
 */
export function getGitContext(cwd = '/home/user/unrdf') {
  try {
    const branch = execSync('git rev-parse --abbrev-ref HEAD', {
      cwd,
      encoding: 'utf-8'
    }).trim();

    const commit = execSync('git rev-parse HEAD', {
      cwd,
      encoding: 'utf-8'
    }).trim();

    const status = execSync('git status --porcelain', {
      cwd,
      encoding: 'utf-8'
    }).trim();

    return {
      branch,
      commit: commit.slice(0, 8),
      uncommittedChanges: status.length > 0,
    };
  } catch {
    return {
      branch: 'unknown',
      commit: 'unknown',
      uncommittedChanges: false,
    };
  }
}

// =============================================================================
// Denial Receipt Generation
// =============================================================================

/**
 * Generate a denial receipt
 *
 * @param {Object} params - Denial parameters
 * @param {string} params.operation - Operation type
 * @param {string} params.targetPath - Target file path
 * @param {string} params.reasonCode - Denial reason code
 * @param {string} params.message - Human-readable message
 * @param {Object} [params.details] - Additional details
 * @param {Object} params.remediation - Remediation steps
 * @returns {Promise<Object>} Denial receipt
 */
export async function generateDenialReceipt(params) {
  const {
    operation,
    targetPath,
    reasonCode,
    message,
    details,
    remediation,
  } = params;

  const receipt = {
    id: generateUUID(),
    timestamp: new Date().toISOString(),
    operation,
    targetPath,
    reasonCode,
    message,
    details,
    remediation,
    gitContext: getGitContext(),
  };

  // Validate against schema
  const validated = DenialReceiptSchema.parse(receipt);

  // Write to denials directory
  await mkdir(DENIALS_DIR, { recursive: true });
  const filename = `${validated.id}-${reasonCode}.json`;
  const path = join(DENIALS_DIR, filename);

  await writeFile(path, JSON.stringify(validated, null, 2));

  return validated;
}

// =============================================================================
// Content Hashing
// =============================================================================

/**
 * Compute BLAKE3 hash of content (deterministic)
 *
 * @param {string|Object} data - Data to hash
 * @returns {Promise<string>} 64-character hex hash
 */
export async function computeContentHash(data) {
  const serialized = typeof data === 'string' ? data : deterministicSerialize(data);
  return blake3(serialized);
}

/**
 * Extract content without frontmatter for hashing
 *
 * @param {string} markdown - Full markdown content
 * @returns {string} Content without frontmatter
 */
export function extractBodyContent(markdown) {
  const { content } = extractFrontmatter(markdown);
  return content;
}

// =============================================================================
// File Operations
// =============================================================================

/**
 * Check if path is in docs directory
 *
 * @param {string} filePath - File path to check
 * @returns {boolean}
 */
export function isDocsPath(filePath) {
  const normalized = resolve(filePath);
  // Check if path is in DOCS_DIR or contains /docs/
  return normalized.startsWith(DOCS_DIR) || normalized.includes('/docs/');
}

/**
 * Read file with error handling
 *
 * @param {string} filePath - File to read
 * @returns {Promise<{ success: boolean, content?: string, error?: string }>}
 */
export async function safeReadFile(filePath) {
  try {
    const content = await readFile(filePath, 'utf-8');
    return { success: true, content };
  } catch (error) {
    return { success: false, error: error.message };
  }
}

// =============================================================================
// Exports
// =============================================================================

export default {
  // Constants
  RECEIPTS_DIR,
  DENIALS_DIR,
  DOCS_DIR,
  DENIAL_REASONS,

  // Schemas
  FrontmatterSchema,
  DenialReceiptSchema,

  // Frontmatter
  extractFrontmatter,
  validateFrontmatter,
  updateFrontmatter,

  // Receipts
  checkReceiptExists,
  validateReceipts,

  // Git
  getGitContext,

  // Denial generation
  generateDenialReceipt,

  // Hashing
  computeContentHash,
  extractBodyContent,

  // Files
  isDocsPath,
  safeReadFile,
};
