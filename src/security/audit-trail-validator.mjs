/**
 * @fileoverview Audit Trail Validator - Ensure all operations are properly logged
 *
 * Implements Q_audit_trail invariant:
 * - Validates receipt structure and completeness
 * - Verifies operation logging coverage
 * - Checks temporal consistency
 * - Detects gaps in audit trail
 *
 * @module security/audit-trail-validator
 */

import { z } from 'zod';
import { readFileSync, existsSync, readdirSync, statSync } from 'node:fs';
import { join, relative } from 'node:path';
import { createHash } from 'node:crypto';

/**
 * Receipt schema for validation
 */
export const ReceiptSchema = z.object({
  id: z.string().min(1),
  timestamp: z.string().datetime({ offset: true }).or(z.string().regex(/^\d{4}-\d{2}-\d{2}/)),
  operation: z.string().min(1),
  agent: z.string().min(1).optional(),
  actor: z.string().min(1).optional(),
  status: z.enum(['success', 'failure', 'pending', 'completed', 'error']).optional(),
  result: z.any().optional(),
  metadata: z.record(z.any()).optional(),
  hash: z.string().optional(),
  parentId: z.string().optional(),
  sequence: z.number().optional()
});

/**
 * @typedef {z.infer<typeof ReceiptSchema>} Receipt
 */

/**
 * Audit trail finding schema
 */
export const AuditFindingSchema = z.object({
  type: z.enum([
    'missing_field',
    'invalid_structure',
    'temporal_gap',
    'sequence_gap',
    'hash_mismatch',
    'orphaned_receipt',
    'duplicate_id',
    'missing_receipt',
    'invalid_timestamp'
  ]),
  severity: z.enum(['critical', 'high', 'medium', 'low']),
  receiptId: z.string().optional(),
  description: z.string(),
  details: z.any().optional()
});

/**
 * @typedef {z.infer<typeof AuditFindingSchema>} AuditFinding
 */

/**
 * Required fields for different operation types
 */
const REQUIRED_FIELDS_BY_OPERATION = {
  default: ['id', 'timestamp', 'operation'],
  write: ['id', 'timestamp', 'operation', 'actor', 'status'],
  delete: ['id', 'timestamp', 'operation', 'actor', 'status'],
  update: ['id', 'timestamp', 'operation', 'actor', 'status'],
  admission: ['id', 'timestamp', 'operation', 'agent', 'status', 'result'],
  consensus: ['id', 'timestamp', 'operation', 'status', 'result']
};

/**
 * Maximum allowed gap between consecutive receipts (5 minutes)
 */
const MAX_TEMPORAL_GAP_MS = 5 * 60 * 1000;

/**
 * Validate a single receipt
 *
 * @param {Object} receipt - Receipt to validate
 * @param {Object} [options] - Validation options
 * @returns {Array<AuditFinding>} Validation findings
 */
export function validateReceipt(receipt, options = {}) {
  const findings = [];

  // Basic structure validation
  const parseResult = ReceiptSchema.safeParse(receipt);
  if (!parseResult.success) {
    findings.push({
      type: 'invalid_structure',
      severity: 'high',
      receiptId: receipt?.id || 'unknown',
      description: 'Receipt does not conform to required schema',
      details: parseResult.error.issues
    });
    return findings;
  }

  const validReceipt = parseResult.data;

  // Check required fields based on operation type
  const operation = validReceipt.operation?.toLowerCase() || 'default';
  const requiredFields = REQUIRED_FIELDS_BY_OPERATION[operation] ||
                        REQUIRED_FIELDS_BY_OPERATION.default;

  for (const field of requiredFields) {
    if (validReceipt[field] === undefined || validReceipt[field] === null) {
      findings.push({
        type: 'missing_field',
        severity: field === 'id' || field === 'timestamp' ? 'critical' : 'medium',
        receiptId: validReceipt.id,
        description: `Required field "${field}" is missing for ${operation} operation`
      });
    }
  }

  // Validate timestamp
  try {
    const timestamp = new Date(validReceipt.timestamp);
    if (isNaN(timestamp.getTime())) {
      findings.push({
        type: 'invalid_timestamp',
        severity: 'high',
        receiptId: validReceipt.id,
        description: 'Invalid timestamp format'
      });
    } else if (timestamp > Date.now() + 60000) { // Allow 1 min clock skew
      findings.push({
        type: 'invalid_timestamp',
        severity: 'medium',
        receiptId: validReceipt.id,
        description: 'Timestamp is in the future'
      });
    }
  } catch {
    findings.push({
      type: 'invalid_timestamp',
      severity: 'high',
      receiptId: validReceipt.id,
      description: 'Could not parse timestamp'
    });
  }

  // Verify hash if present
  if (validReceipt.hash && options.verifyHash !== false) {
    const computedHash = computeReceiptHash(validReceipt);
    if (computedHash !== validReceipt.hash) {
      findings.push({
        type: 'hash_mismatch',
        severity: 'critical',
        receiptId: validReceipt.id,
        description: 'Receipt hash does not match computed hash',
        details: { expected: validReceipt.hash, computed: computedHash }
      });
    }
  }

  return findings;
}

/**
 * Compute hash for a receipt (excluding the hash field itself)
 *
 * @param {Object} receipt - Receipt to hash
 * @returns {string} SHA-256 hash
 */
export function computeReceiptHash(receipt) {
  const { hash, ...rest } = receipt;
  const canonical = JSON.stringify(rest, Object.keys(rest).sort());
  return createHash('sha256').update(canonical).digest('hex');
}

/**
 * Validate receipt chain (sequence of related receipts)
 *
 * @param {Array<Object>} receipts - Receipts in chronological order
 * @param {Object} [options] - Validation options
 * @returns {Array<AuditFinding>} Chain validation findings
 */
export function validateReceiptChain(receipts, options = {}) {
  const findings = [];

  if (!Array.isArray(receipts) || receipts.length === 0) {
    return findings;
  }

  // Sort by timestamp
  const sorted = [...receipts].sort((a, b) =>
    new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime()
  );

  // Check for duplicate IDs
  const seenIds = new Set();
  for (const receipt of sorted) {
    if (seenIds.has(receipt.id)) {
      findings.push({
        type: 'duplicate_id',
        severity: 'critical',
        receiptId: receipt.id,
        description: 'Duplicate receipt ID detected'
      });
    }
    seenIds.add(receipt.id);
  }

  // Check temporal gaps
  for (let i = 1; i < sorted.length; i++) {
    const prev = new Date(sorted[i - 1].timestamp).getTime();
    const curr = new Date(sorted[i].timestamp).getTime();
    const gap = curr - prev;

    if (gap > MAX_TEMPORAL_GAP_MS) {
      findings.push({
        type: 'temporal_gap',
        severity: 'medium',
        receiptId: sorted[i].id,
        description: `Large temporal gap detected (${Math.round(gap / 1000)}s)`,
        details: {
          previousReceipt: sorted[i - 1].id,
          gap: gap
        }
      });
    }

    // Check for timestamp going backwards
    if (gap < -1000) { // Allow 1 second tolerance
      findings.push({
        type: 'invalid_timestamp',
        severity: 'high',
        receiptId: sorted[i].id,
        description: 'Timestamp is earlier than previous receipt',
        details: {
          previousReceipt: sorted[i - 1].id,
          previousTimestamp: sorted[i - 1].timestamp,
          currentTimestamp: sorted[i].timestamp
        }
      });
    }
  }

  // Check sequence continuity if sequence numbers present
  const withSequence = sorted.filter(r => typeof r.sequence === 'number');
  if (withSequence.length > 0) {
    const sortedBySeq = [...withSequence].sort((a, b) => a.sequence - b.sequence);

    for (let i = 1; i < sortedBySeq.length; i++) {
      const expectedSeq = sortedBySeq[i - 1].sequence + 1;
      if (sortedBySeq[i].sequence !== expectedSeq) {
        findings.push({
          type: 'sequence_gap',
          severity: 'high',
          receiptId: sortedBySeq[i].id,
          description: `Sequence gap: expected ${expectedSeq}, got ${sortedBySeq[i].sequence}`,
          details: {
            previousSequence: sortedBySeq[i - 1].sequence,
            currentSequence: sortedBySeq[i].sequence
          }
        });
      }
    }
  }

  // Check parent references
  const idSet = new Set(sorted.map(r => r.id));
  for (const receipt of sorted) {
    if (receipt.parentId && !idSet.has(receipt.parentId)) {
      findings.push({
        type: 'orphaned_receipt',
        severity: 'medium',
        receiptId: receipt.id,
        description: `Parent receipt "${receipt.parentId}" not found`
      });
    }
  }

  return findings;
}

/**
 * Load receipts from a directory
 *
 * @param {string} directory - Directory containing receipt files
 * @returns {Array<Object>} Loaded receipts
 */
export function loadReceipts(directory) {
  const receipts = [];

  if (!existsSync(directory)) {
    return receipts;
  }

  const files = readdirSync(directory);

  for (const file of files) {
    if (!file.endsWith('.json')) continue;

    const filePath = join(directory, file);
    try {
      const content = readFileSync(filePath, 'utf-8');
      const receipt = JSON.parse(content);

      // Handle both single receipts and arrays
      if (Array.isArray(receipt)) {
        receipts.push(...receipt);
      } else {
        receipts.push(receipt);
      }
    } catch {
      // Skip invalid files
    }
  }

  return receipts;
}

/**
 * Validate all receipts in a directory
 *
 * @param {string} directory - Directory to validate
 * @param {Object} [options] - Validation options
 * @returns {Object} Validation results
 */
export function validateDirectory(directory, options = {}) {
  const startTime = Date.now();
  const findings = [];
  const receipts = loadReceipts(directory);

  // Validate individual receipts
  for (const receipt of receipts) {
    const receiptFindings = validateReceipt(receipt, options);
    findings.push(...receiptFindings);
  }

  // Validate chain
  const chainFindings = validateReceiptChain(receipts, options);
  findings.push(...chainFindings);

  // Group by severity
  const bySeverity = {
    critical: findings.filter(f => f.severity === 'critical'),
    high: findings.filter(f => f.severity === 'high'),
    medium: findings.filter(f => f.severity === 'medium'),
    low: findings.filter(f => f.severity === 'low')
  };

  // Group by type
  const byType = {};
  for (const finding of findings) {
    byType[finding.type] = (byType[finding.type] || 0) + 1;
  }

  // Calculate integrity score
  const maxScore = receipts.length * 10; // 10 points per receipt
  const deductions = {
    critical: 10,
    high: 5,
    medium: 2,
    low: 1
  };

  let deduction = 0;
  for (const finding of findings) {
    deduction += deductions[finding.severity] || 1;
  }

  const integrityScore = maxScore > 0
    ? Math.max(0, Math.round(((maxScore - deduction) / maxScore) * 100))
    : 100;

  return {
    passed: bySeverity.critical.length === 0 && bySeverity.high.length === 0,
    findings,
    receipts: receipts.length,
    summary: {
      total: findings.length,
      bySeverity: {
        critical: bySeverity.critical.length,
        high: bySeverity.high.length,
        medium: bySeverity.medium.length,
        low: bySeverity.low.length
      },
      byType,
      integrityScore,
      receiptsValidated: receipts.length,
      scanDuration: Date.now() - startTime
    },
    metadata: {
      directory,
      timestamp: new Date().toISOString()
    }
  };
}

/**
 * Validate that required operations have receipts
 *
 * @param {Array<string>} expectedOperations - Operations that should have receipts
 * @param {Array<Object>} receipts - Available receipts
 * @returns {Array<AuditFinding>} Missing receipt findings
 */
export function validateRequiredOperations(expectedOperations, receipts) {
  const findings = [];
  const operationSet = new Set(receipts.map(r => r.operation));

  for (const operation of expectedOperations) {
    if (!operationSet.has(operation)) {
      findings.push({
        type: 'missing_receipt',
        severity: 'high',
        description: `No receipt found for required operation: ${operation}`
      });
    }
  }

  return findings;
}

/**
 * Create a new receipt with proper structure
 *
 * @param {Object} params - Receipt parameters
 * @returns {Object} Complete receipt with hash
 */
export function createReceipt(params) {
  const receipt = {
    id: params.id || crypto.randomUUID?.() || `receipt-${Date.now()}`,
    timestamp: params.timestamp || new Date().toISOString(),
    operation: params.operation,
    actor: params.actor || params.agent,
    agent: params.agent,
    status: params.status || 'success',
    result: params.result,
    metadata: params.metadata || {},
    parentId: params.parentId,
    sequence: params.sequence
  };

  // Remove undefined values
  for (const key of Object.keys(receipt)) {
    if (receipt[key] === undefined) {
      delete receipt[key];
    }
  }

  // Add hash
  receipt.hash = computeReceiptHash(receipt);

  return receipt;
}

/**
 * Format validation results as report
 *
 * @param {Object} result - Validation result
 * @returns {string} Formatted report
 */
export function formatReport(result) {
  const lines = [];

  lines.push('='.repeat(60));
  lines.push('AUDIT TRAIL VALIDATION REPORT');
  lines.push('='.repeat(60));
  lines.push('');
  lines.push(`Validation Time: ${result.metadata.timestamp}`);
  lines.push(`Directory: ${result.metadata.directory}`);
  lines.push(`Scan Duration: ${result.summary.scanDuration}ms`);
  lines.push('');
  lines.push('-'.repeat(60));
  lines.push('SUMMARY');
  lines.push('-'.repeat(60));
  lines.push(`Receipts Validated: ${result.summary.receiptsValidated}`);
  lines.push(`Total Findings: ${result.summary.total}`);
  lines.push(`Integrity Score: ${result.summary.integrityScore}%`);
  lines.push(`Status: ${result.passed ? 'PASSED' : 'FAILED'}`);
  lines.push('');
  lines.push('By Severity:');
  lines.push(`  Critical: ${result.summary.bySeverity.critical}`);
  lines.push(`  High: ${result.summary.bySeverity.high}`);
  lines.push(`  Medium: ${result.summary.bySeverity.medium}`);
  lines.push(`  Low: ${result.summary.bySeverity.low}`);

  if (Object.keys(result.summary.byType).length > 0) {
    lines.push('');
    lines.push('By Type:');
    for (const [type, count] of Object.entries(result.summary.byType)) {
      lines.push(`  ${type}: ${count}`);
    }
  }

  if (result.findings.length > 0) {
    lines.push('');
    lines.push('-'.repeat(60));
    lines.push('FINDINGS');
    lines.push('-'.repeat(60));

    for (const finding of result.findings.slice(0, 20)) {
      lines.push('');
      lines.push(`[${finding.severity.toUpperCase()}] ${finding.type}`);
      if (finding.receiptId) {
        lines.push(`  Receipt: ${finding.receiptId}`);
      }
      lines.push(`  Description: ${finding.description}`);
    }

    if (result.findings.length > 20) {
      lines.push('');
      lines.push(`... and ${result.findings.length - 20} more findings`);
    }
  }

  lines.push('');
  lines.push('='.repeat(60));

  return lines.join('\n');
}

/**
 * Check audit trail coverage for operations
 *
 * @param {Array<Object>} receipts - Available receipts
 * @param {Object} [options] - Coverage options
 * @returns {Object} Coverage report
 */
export function calculateCoverage(receipts, options = {}) {
  const operations = new Map();

  for (const receipt of receipts) {
    const op = receipt.operation || 'unknown';
    if (!operations.has(op)) {
      operations.set(op, { total: 0, success: 0, failure: 0 });
    }

    const stats = operations.get(op);
    stats.total++;

    if (receipt.status === 'success' || receipt.status === 'completed') {
      stats.success++;
    } else if (receipt.status === 'failure' || receipt.status === 'error') {
      stats.failure++;
    }
  }

  // Calculate overall coverage
  const totalOps = Array.from(operations.values()).reduce((sum, s) => sum + s.total, 0);
  const expectedOps = options.expectedOperations || [];
  const coveredOps = expectedOps.filter(op => operations.has(op));

  return {
    operations: Object.fromEntries(operations),
    totalReceipts: receipts.length,
    uniqueOperations: operations.size,
    coverage: expectedOps.length > 0
      ? Math.round((coveredOps.length / expectedOps.length) * 100)
      : 100,
    missing: expectedOps.filter(op => !operations.has(op))
  };
}

export default {
  validateReceipt,
  validateReceiptChain,
  validateDirectory,
  validateRequiredOperations,
  loadReceipts,
  createReceipt,
  computeReceiptHash,
  formatReport,
  calculateCoverage,
  ReceiptSchema
};
