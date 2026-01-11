/**
 * @file Compliance Receipt Framework - Regulatory Audit Trails
 * @module research/prototypes/compliance-receipt-framework
 *
 * @description
 * Prototype demonstrating receipt-based compliance for regulatory audits.
 *
 * Features:
 * - Immutable audit trail with BLAKE3 receipt chains
 * - Compliance rule engine (GDPR, SOX, HIPAA)
 * - Automated regulatory reports
 * - Merkle tree anchoring for tamper-evidence
 * - Retention policy enforcement
 *
 * Compliance Standards:
 * - GDPR: Data processing records, right to erasure, consent management
 * - SOX: Audit trails, access controls, change management
 * - HIPAA: Access logs, encryption, minimum necessary
 */

import { z } from 'zod';
import { randomUUID } from 'crypto';
import { blake3 } from 'hash-wasm';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Compliance event schema
 */
export const ComplianceEventSchema = z.object({
  eventId: z.string().uuid(),
  timestamp: z.bigint(),
  eventType: z.enum([
    'data-access',
    'data-modification',
    'data-deletion',
    'consent-granted',
    'consent-revoked',
    'policy-applied',
    'security-incident',
  ]),
  actor: z.string(), // user:alice, system:backup, etc.
  resource: z.string(), // ontology:finance, data:patient-123
  operation: z.string(), // read, write, delete, export
  justification: z.string(),
  metadata: z.record(z.string(), z.unknown()).optional(),
});

/**
 * Compliance rule schema
 */
export const ComplianceRuleSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1),
  standard: z.enum(['GDPR', 'SOX', 'HIPAA', 'CUSTOM']),
  description: z.string(),
  condition: z.function(), // (event) => boolean
  severity: z.enum(['critical', 'high', 'medium', 'low']),
  requiresReceipt: z.boolean().default(true),
  retentionYears: z.number().int().positive().optional(),
});

/**
 * Compliance receipt schema
 */
export const ComplianceReceiptSchema = z.object({
  receiptId: z.string().uuid(),
  timestamp: z.bigint(),
  event: ComplianceEventSchema,
  rulesEvaluated: z.array(z.string()), // Rule IDs
  compliance: z.object({
    compliant: z.boolean(),
    violations: z.array(z.object({
      ruleId: z.string(),
      severity: z.string(),
      reason: z.string(),
    })),
  }),
  receiptHash: z.string().length(64), // BLAKE3
  previousHash: z.string().length(64).nullable(),
  merkleRoot: z.string().length(64).optional(), // Batch merkle root
});

/**
 * Compliance report schema
 */
export const ComplianceReportSchema = z.object({
  reportId: z.string().uuid(),
  standard: z.enum(['GDPR', 'SOX', 'HIPAA', 'ALL']),
  generatedAt: z.bigint(),
  period: z.object({
    from: z.bigint(),
    until: z.bigint(),
  }),
  summary: z.object({
    totalEvents: z.number().int().nonnegative(),
    compliantEvents: z.number().int().nonnegative(),
    violations: z.number().int().nonnegative(),
    byEventType: z.record(z.string(), z.number()),
    bySeverity: z.record(z.string(), z.number()),
  }),
  receipts: z.array(ComplianceReceiptSchema),
  violations: z.array(z.object({
    receiptId: z.string(),
    ruleId: z.string(),
    severity: z.string(),
    reason: z.string(),
    timestamp: z.bigint(),
  })),
});

// =============================================================================
// Compliance Rule Engine
// =============================================================================

/**
 * Compliance Rule Engine - Evaluates events against compliance rules
 *
 * @class ComplianceRuleEngine
 */
export class ComplianceRuleEngine {
  constructor() {
    this.rules = new Map();
    this._registerDefaultRules();
  }

  /**
   * Register a compliance rule
   *
   * @param {Object} ruleDef - Rule definition
   * @returns {Object} Registered rule
   */
  registerRule(ruleDef) {
    const rule = ComplianceRuleSchema.parse({
      ...ruleDef,
      id: ruleDef.id || randomUUID(),
    });

    this.rules.set(rule.id, rule);
    return rule;
  }

  /**
   * Evaluate an event against all applicable rules
   *
   * @param {Object} event - Compliance event
   * @returns {Object} Evaluation result
   */
  evaluate(event) {
    const validatedEvent = ComplianceEventSchema.parse(event);
    const applicableRules = this._getApplicableRules(validatedEvent);
    const violations = [];

    for (const rule of applicableRules) {
      try {
        const passed = rule.condition(validatedEvent);
        if (!passed) {
          violations.push({
            ruleId: rule.id,
            severity: rule.severity,
            reason: `Rule ${rule.name} violated`,
          });
        }
      } catch (error) {
        violations.push({
          ruleId: rule.id,
          severity: 'critical',
          reason: `Rule evaluation error: ${error.message}`,
        });
      }
    }

    return {
      compliant: violations.length === 0,
      rulesEvaluated: applicableRules.map(r => r.id),
      violations,
    };
  }

  /**
   * Get applicable rules for an event
   *
   * @private
   */
  _getApplicableRules(event) {
    // For now, return all rules
    // In production, filter by event type, standard, etc.
    return Array.from(this.rules.values());
  }

  /**
   * Register default compliance rules
   *
   * @private
   */
  _registerDefaultRules() {
    // GDPR: Right to Erasure
    this.registerRule({
      name: 'GDPR Right to Erasure',
      standard: 'GDPR',
      description: 'Data deletion must be logged',
      condition: (event) => {
        if (event.eventType === 'data-deletion') {
          return event.justification && event.justification.length > 0;
        }
        return true;
      },
      severity: 'high',
      retentionYears: 7,
    });

    // GDPR: Consent Management
    this.registerRule({
      name: 'GDPR Consent Requirement',
      standard: 'GDPR',
      description: 'Data access requires valid consent',
      condition: (event) => {
        if (event.eventType === 'data-access') {
          // Check metadata for consent
          return event.metadata?.consentGranted === true;
        }
        return true;
      },
      severity: 'critical',
      retentionYears: 7,
    });

    // SOX: Audit Trail
    this.registerRule({
      name: 'SOX Audit Trail',
      standard: 'SOX',
      description: 'All financial data modifications must be logged',
      condition: (event) => {
        if (event.eventType === 'data-modification') {
          const isFinancial = event.resource.includes('finance');
          if (isFinancial) {
            return event.actor && event.justification;
          }
        }
        return true;
      },
      severity: 'critical',
      retentionYears: 7,
    });

    // HIPAA: Access Logging
    this.registerRule({
      name: 'HIPAA Access Logging',
      standard: 'HIPAA',
      description: 'PHI access must be logged with justification',
      condition: (event) => {
        if (event.eventType === 'data-access') {
          const isPHI = event.resource.includes('patient') || event.resource.includes('medical');
          if (isPHI) {
            return event.justification && event.justification.length > 10;
          }
        }
        return true;
      },
      severity: 'critical',
      retentionYears: 6,
    });

    // HIPAA: Minimum Necessary
    this.registerRule({
      name: 'HIPAA Minimum Necessary',
      standard: 'HIPAA',
      description: 'PHI access must be minimum necessary',
      condition: (event) => {
        if (event.eventType === 'data-access') {
          const isPHI = event.resource.includes('patient') || event.resource.includes('medical');
          if (isPHI) {
            // Check if access scope is justified
            return event.metadata?.minimumNecessary === true;
          }
        }
        return true;
      },
      severity: 'high',
      retentionYears: 6,
    });
  }
}

// =============================================================================
// Compliance Receipt Framework
// =============================================================================

/**
 * Compliance Receipt Framework - Manages compliance receipts and audits
 *
 * @class ComplianceReceiptFramework
 */
export class ComplianceReceiptFramework {
  /**
   * Create compliance receipt framework
   *
   * @param {Object} options - Configuration options
   */
  constructor(options = {}) {
    this.ruleEngine = new ComplianceRuleEngine();
    this.receipts = [];
    this.previousReceiptHash = null;
    this.merkleRoots = [];
    this.batchSize = options.batchSize || 100;
    this.retentionPolicy = options.retentionPolicy || { defaultYears: 7 };
  }

  /**
   * Record a compliance event and generate receipt
   *
   * @param {Object} eventDef - Event definition
   * @returns {Promise<Object>} Compliance receipt
   */
  async recordEvent(eventDef) {
    const event = ComplianceEventSchema.parse({
      ...eventDef,
      eventId: eventDef.eventId || randomUUID(),
      timestamp: eventDef.timestamp || BigInt(Date.now()) * 1_000_000n,
    });

    // Evaluate compliance rules
    const evaluation = this.ruleEngine.evaluate(event);

    // Create receipt
    const receipt = await this._createReceipt(event, evaluation);

    // Store receipt
    this.receipts.push(receipt);

    // Check if batch is full for Merkle anchoring
    if (this.receipts.length % this.batchSize === 0) {
      await this._anchorBatch();
    }

    return receipt;
  }

  /**
   * Generate compliance report
   *
   * @param {Object} options - Report options
   * @returns {Object} Compliance report
   */
  generateReport(options = {}) {
    const standard = options.standard || 'ALL';
    const from = options.from || 0n;
    const until = options.until || BigInt(Date.now()) * 1_000_000n;

    // Filter receipts by time range
    let filteredReceipts = this.receipts.filter(
      r => r.timestamp >= from && r.timestamp <= until
    );

    // Filter by standard if specified
    if (standard !== 'ALL') {
      const ruleIds = Array.from(this.ruleEngine.rules.values())
        .filter(r => r.standard === standard)
        .map(r => r.id);

      filteredReceipts = filteredReceipts.filter(r =>
        r.rulesEvaluated.some(id => ruleIds.includes(id))
      );
    }

    // Calculate summary
    const totalEvents = filteredReceipts.length;
    const compliantEvents = filteredReceipts.filter(r => r.compliance.compliant).length;
    const violations = filteredReceipts.flatMap(r => r.compliance.violations);

    const byEventType = {};
    const bySeverity = { critical: 0, high: 0, medium: 0, low: 0 };

    for (const receipt of filteredReceipts) {
      const eventType = receipt.event.eventType;
      byEventType[eventType] = (byEventType[eventType] || 0) + 1;

      for (const violation of receipt.compliance.violations) {
        bySeverity[violation.severity]++;
      }
    }

    const report = ComplianceReportSchema.parse({
      reportId: randomUUID(),
      standard,
      generatedAt: BigInt(Date.now()) * 1_000_000n,
      period: { from, until },
      summary: {
        totalEvents,
        compliantEvents,
        violations: violations.length,
        byEventType,
        bySeverity,
      },
      receipts: filteredReceipts,
      violations: violations.map(v => ({
        receiptId: filteredReceipts.find(r =>
          r.compliance.violations.some(vv => vv.ruleId === v.ruleId)
        )?.receiptId || 'unknown',
        ruleId: v.ruleId,
        severity: v.severity,
        reason: v.reason,
        timestamp: filteredReceipts.find(r =>
          r.compliance.violations.some(vv => vv.ruleId === v.ruleId)
        )?.timestamp || 0n,
      })),
    });

    return report;
  }

  /**
   * Verify receipt chain integrity
   *
   * @returns {Promise<Object>} Verification result
   */
  async verifyChain() {
    if (this.receipts.length === 0) {
      return { valid: true, totalReceipts: 0 };
    }

    const tamperedReceipts = [];

    // Verify first receipt (genesis)
    const first = this.receipts[0];
    if (first.previousHash !== null) {
      tamperedReceipts.push({
        receiptId: first.receiptId,
        reason: 'Genesis receipt must have previousHash = null',
      });
    }

    // Verify chain
    for (let i = 1; i < this.receipts.length; i++) {
      const current = this.receipts[i];
      const previous = this.receipts[i - 1];

      if (current.previousHash !== previous.receiptHash) {
        tamperedReceipts.push({
          receiptId: current.receiptId,
          reason: `Chain broken: previousHash mismatch`,
        });
      }

      // Verify timestamp ordering
      if (current.timestamp <= previous.timestamp) {
        tamperedReceipts.push({
          receiptId: current.receiptId,
          reason: 'Temporal violation: timestamp not monotonic',
        });
      }
    }

    return {
      valid: tamperedReceipts.length === 0,
      totalReceipts: this.receipts.length,
      tamperedReceipts,
    };
  }

  /**
   * Get retention requirements
   *
   * @param {string} receiptId - Receipt identifier
   * @returns {Object} Retention info
   */
  getRetentionRequirements(receiptId) {
    const receipt = this.receipts.find(r => r.receiptId === receiptId);
    if (!receipt) {
      return null;
    }

    const maxRetention = Math.max(
      ...receipt.rulesEvaluated.map(ruleId => {
        const rule = this.ruleEngine.rules.get(ruleId);
        return rule?.retentionYears || this.retentionPolicy.defaultYears;
      })
    );

    const deleteAfter = receipt.timestamp + BigInt(maxRetention * 365 * 24 * 60 * 60) * 1_000_000_000n;

    return {
      receiptId,
      retentionYears: maxRetention,
      createdAt: receipt.timestamp,
      deleteAfter,
      canDelete: BigInt(Date.now()) * 1_000_000n >= deleteAfter,
    };
  }

  /**
   * Get framework statistics
   *
   * @returns {Object} Statistics
   */
  getStats() {
    const totalReceipts = this.receipts.length;
    const compliantReceipts = this.receipts.filter(r => r.compliance.compliant).length;
    const violations = this.receipts.flatMap(r => r.compliance.violations);

    return {
      totalReceipts,
      compliantReceipts,
      violationCount: violations.length,
      complianceRate: totalReceipts > 0 ? (compliantReceipts / totalReceipts) * 100 : 100,
      merkleRoots: this.merkleRoots.length,
      rulesRegistered: this.ruleEngine.rules.size,
    };
  }

  /**
   * Create compliance receipt
   *
   * @private
   */
  async _createReceipt(event, evaluation) {
    // Compute payload hash
    const payload = { event, compliance: evaluation };
    const payloadStr = JSON.stringify(payload);
    const payloadHash = await blake3(payloadStr);

    // Compute chain hash
    const chainInput = `${this.previousReceiptHash || 'GENESIS'}:${payloadHash}`;
    const receiptHash = await blake3(chainInput);

    const receipt = ComplianceReceiptSchema.parse({
      receiptId: randomUUID(),
      timestamp: event.timestamp,
      event,
      rulesEvaluated: evaluation.rulesEvaluated,
      compliance: {
        compliant: evaluation.compliant,
        violations: evaluation.violations,
      },
      receiptHash,
      previousHash: this.previousReceiptHash,
    });

    // Update chain
    this.previousReceiptHash = receiptHash;

    return receipt;
  }

  /**
   * Anchor batch of receipts in Merkle tree
   *
   * @private
   */
  async _anchorBatch() {
    const batchStart = this.receipts.length - this.batchSize;
    const batch = this.receipts.slice(batchStart);

    // Compute Merkle root
    const hashes = batch.map(r => r.receiptHash);
    const merkleRoot = await this._computeMerkleRoot(hashes);

    this.merkleRoots.push({
      root: merkleRoot,
      batchStart,
      batchEnd: this.receipts.length - 1,
      timestamp: BigInt(Date.now()) * 1_000_000n,
    });

    // Update receipts with Merkle root
    for (const receipt of batch) {
      receipt.merkleRoot = merkleRoot;
    }
  }

  /**
   * Compute Merkle root from hashes
   *
   * @private
   */
  async _computeMerkleRoot(hashes) {
    if (hashes.length === 0) {
      return await blake3('EMPTY');
    }
    if (hashes.length === 1) {
      return hashes[0];
    }

    // Simple binary tree (in production, use proper Merkle tree)
    const combined = hashes.join(':');
    return blake3(combined);
  }
}

// =============================================================================
// Example Usage
// =============================================================================

/**
 * Example: GDPR compliance workflow
 */
export async function example() {
  const framework = new ComplianceReceiptFramework({
    batchSize: 10,
    retentionPolicy: { defaultYears: 7 },
  });

  console.log('🔒 Compliance Receipt Framework Example\n');

  // Event 1: User consent granted
  const event1 = await framework.recordEvent({
    eventType: 'consent-granted',
    actor: 'user:alice',
    resource: 'data:alice-profile',
    operation: 'consent',
    justification: 'User granted consent for marketing emails',
    metadata: { consentGranted: true, consentType: 'marketing' },
  });

  console.log('Event 1 (Consent):', {
    compliant: event1.compliance.compliant,
    receiptHash: event1.receiptHash.slice(0, 16) + '...',
  });

  // Event 2: Data access (compliant)
  const event2 = await framework.recordEvent({
    eventType: 'data-access',
    actor: 'system:analytics',
    resource: 'data:alice-profile',
    operation: 'read',
    justification: 'Analyze user behavior for recommendations',
    metadata: { consentGranted: true },
  });

  console.log('Event 2 (Access - Compliant):', {
    compliant: event2.compliance.compliant,
  });

  // Event 3: Data access (violation - no consent)
  const event3 = await framework.recordEvent({
    eventType: 'data-access',
    actor: 'system:marketing',
    resource: 'data:bob-profile',
    operation: 'read',
    justification: 'Marketing campaign',
    metadata: { consentGranted: false }, // VIOLATION!
  });

  console.log('Event 3 (Access - Violation):', {
    compliant: event3.compliance.compliant,
    violations: event3.compliance.violations.length,
  });

  // Event 4: HIPAA compliance (PHI access)
  const event4 = await framework.recordEvent({
    eventType: 'data-access',
    actor: 'doctor:smith',
    resource: 'data:patient-123-medical-records',
    operation: 'read',
    justification: 'Reviewing patient history for diagnosis appointment',
    metadata: { minimumNecessary: true },
  });

  console.log('Event 4 (HIPAA PHI Access):', {
    compliant: event4.compliance.compliant,
  });

  // Event 5: SOX compliance (financial data modification)
  const event5 = await framework.recordEvent({
    eventType: 'data-modification',
    actor: 'accountant:jones',
    resource: 'ontology:finance/ledger',
    operation: 'write',
    justification: 'Correcting Q4 revenue entry per audit finding AF-2024-123',
  });

  console.log('Event 5 (SOX Financial Modification):', {
    compliant: event5.compliance.compliant,
  });

  console.log('\n📊 Generating Compliance Reports\n');

  // Generate GDPR report
  const gdprReport = framework.generateReport({ standard: 'GDPR' });
  console.log('GDPR Report:', {
    totalEvents: gdprReport.summary.totalEvents,
    compliantEvents: gdprReport.summary.compliantEvents,
    violations: gdprReport.summary.violations,
  });

  // Generate HIPAA report
  const hipaaReport = framework.generateReport({ standard: 'HIPAA' });
  console.log('HIPAA Report:', {
    totalEvents: hipaaReport.summary.totalEvents,
    compliantEvents: hipaaReport.summary.compliantEvents,
  });

  // Generate SOX report
  const soxReport = framework.generateReport({ standard: 'SOX' });
  console.log('SOX Report:', {
    totalEvents: soxReport.summary.totalEvents,
    compliantEvents: soxReport.summary.compliantEvents,
  });

  // Verify chain integrity
  const chainVerification = await framework.verifyChain();
  console.log('\n🔗 Chain Verification:', chainVerification);

  // Framework stats
  const stats = framework.getStats();
  console.log('\n📈 Framework Statistics:', stats);

  // Retention requirements
  const retention = framework.getRetentionRequirements(event1.receiptId);
  console.log('\n⏰ Retention Requirements (Event 1):', {
    retentionYears: retention?.retentionYears,
    canDelete: retention?.canDelete,
  });

  return {
    events: [event1, event2, event3, event4, event5],
    reports: { gdprReport, hipaaReport, soxReport },
    chainVerification,
    stats,
  };
}

// Run example if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  example()
    .then(result => {
      console.log('\n✅ Compliance Receipt Framework Example Complete');
      console.log('Compliance Rate:', result.stats.complianceRate.toFixed(2) + '%');
      console.log('Chain Valid:', result.chainVerification.valid);
    })
    .catch(error => {
      console.error('❌ Example failed:', error);
      process.exit(1);
    });
}

// =============================================================================
// Exports
// =============================================================================

export default {
  ComplianceReceiptFramework,
  ComplianceRuleEngine,
  ComplianceEventSchema,
  ComplianceRuleSchema,
  ComplianceReceiptSchema,
  ComplianceReportSchema,
  example,
};
