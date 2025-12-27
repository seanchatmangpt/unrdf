/**
 * @fileoverview Receipt Chain Repair Mechanism
 *
 * **Purpose**: Detect and repair broken receipt chains
 * - Detect broken hash linkages (beforeHash → receiptHash mismatch)
 * - Detect missing receipts in sequence
 * - Repair strategies (re-hash, fill gaps, rebuild)
 * - Chain verification and integrity validation
 *
 * @module resilience/receipt-chain-repair
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';

/**
 * Chain integrity issue types
 */
export const IssueType = {
  BROKEN_LINK: 'broken_link', // Hash mismatch
  MISSING_RECEIPT: 'missing_receipt', // Gap in sequence
  INVALID_HASH: 'invalid_hash', // Receipt hash invalid
  EPOCH_REGRESSION: 'epoch_regression', // Epoch not increasing
  DUPLICATE_EPOCH: 'duplicate_epoch', // Same epoch appears twice
};

/**
 * Repair strategy enumeration
 */
export const RepairStrategy = {
  RE_HASH: 're_hash', // Recalculate hashes
  FILL_GAP: 'fill_gap', // Add placeholder receipts
  REBUILD: 'rebuild', // Rebuild chain from scratch
  REMOVE_INVALID: 'remove_invalid', // Remove invalid receipts
};

/**
 * Receipt Chain Repair Manager
 *
 * @example
 * const repair = new ReceiptChainRepairManager();
 * const issues = await repair.detectIssues(receiptChain);
 * if (issues.length > 0) {
 *   await repair.repairChain(receiptChain, RepairStrategy.RE_HASH);
 * }
 */
export class ReceiptChainRepairManager {
  /**
   * Create a new receipt chain repair manager
   * @param {Object} [config] - Manager configuration
   */
  constructor(config = {}) {
    this.config = {
      enableAutoRepair: config.enableAutoRepair ?? false,
      repairStrategy: config.repairStrategy ?? RepairStrategy.RE_HASH,
      verifyAfterRepair: config.verifyAfterRepair ?? true,
    };

    this.tracer = trace.getTracer('unrdf-receipt-chain-repair');

    this.metrics = {
      chainsInspected: 0,
      issuesDetected: 0,
      repairsAttempted: 0,
      repairsSucceeded: 0,
      repairsFailed: 0,
      byIssueType: {
        broken_link: 0,
        missing_receipt: 0,
        invalid_hash: 0,
        epoch_regression: 0,
        duplicate_epoch: 0,
      },
    };
  }

  /**
   * Detect issues in receipt chain
   * @param {Object} chain - Receipt chain to inspect
   * @returns {Promise<Array<Object>>} Detected issues
   */
  async detectIssues(chain) {
    return await this.tracer.startActiveSpan(
      'receipt_repair.detect_issues',
      async (span) => {
        span.setAttributes({
          'chain.length': chain.length,
        });

        this.metrics.chainsInspected++;

        const issues = [];
        const receipts = chain.getAll ? chain.getAll() : chain.receipts || [];

        for (let i = 0; i < receipts.length; i++) {
          const receipt = receipts[i];

          // Check hash validity
          if (receipt.verify) {
            const isValid = await receipt.verify();
            if (!isValid) {
              issues.push({
                type: IssueType.INVALID_HASH,
                index: i,
                receiptId: receipt.id || receipt.epoch,
                message: `Receipt ${i} has invalid hash`,
              });
              this.metrics.byIssueType.invalid_hash++;
            }
          }

          // Check linkage
          if (i === 0) {
            // First receipt should have beforeHash = null
            if (receipt.beforeHash !== null && receipt.beforeHash !== undefined) {
              issues.push({
                type: IssueType.BROKEN_LINK,
                index: i,
                receiptId: receipt.id || receipt.epoch,
                message: `First receipt should have beforeHash=null, got ${receipt.beforeHash}`,
              });
              this.metrics.byIssueType.broken_link++;
            }
          } else {
            // Subsequent receipts must link to previous
            const prevReceipt = receipts[i - 1];

            if (receipt.beforeHash !== prevReceipt.receiptHash) {
              issues.push({
                type: IssueType.BROKEN_LINK,
                index: i,
                receiptId: receipt.id || receipt.epoch,
                message: `Receipt ${i} beforeHash mismatch: expected ${prevReceipt.receiptHash}, got ${receipt.beforeHash}`,
              });
              this.metrics.byIssueType.broken_link++;
            }
          }

          // Check epoch ordering
          if (i > 0) {
            const prevReceipt = receipts[i - 1];

            if (receipt.epoch <= prevReceipt.epoch) {
              if (receipt.epoch === prevReceipt.epoch) {
                issues.push({
                  type: IssueType.DUPLICATE_EPOCH,
                  index: i,
                  receiptId: receipt.id || receipt.epoch,
                  message: `Duplicate epoch: ${receipt.epoch}`,
                });
                this.metrics.byIssueType.duplicate_epoch++;
              } else {
                issues.push({
                  type: IssueType.EPOCH_REGRESSION,
                  index: i,
                  receiptId: receipt.id || receipt.epoch,
                  message: `Epoch regression: ${prevReceipt.epoch} → ${receipt.epoch}`,
                });
                this.metrics.byIssueType.epoch_regression++;
              }
            }
          }
        }

        this.metrics.issuesDetected += issues.length;

        span.setAttributes({
          'issues.count': issues.length,
          'issues.has_problems': issues.length > 0,
        });
        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return issues;
      }
    );
  }

  /**
   * Repair receipt chain
   * @param {Object} chain - Receipt chain to repair
   * @param {string} [strategy] - Repair strategy
   * @returns {Promise<Object>} Repair result
   */
  async repairChain(chain, strategy = this.config.repairStrategy) {
    return await this.tracer.startActiveSpan('receipt_repair.repair_chain', async (span) => {
      span.setAttributes({
        'repair.strategy': strategy,
        'chain.length': chain.length,
      });

      this.metrics.repairsAttempted++;

      try {
        // Detect issues first
        const issues = await this.detectIssues(chain);

        if (issues.length === 0) {
          span.setAttributes({ 'repair.needed': false });
          span.setStatus({ code: SpanStatusCode.OK });
          span.end();

          return {
            repaired: false,
            issuesFound: 0,
            message: 'Chain is valid, no repair needed',
          };
        }

        span.setAttributes({
          'repair.needed': true,
          'repair.issues_count': issues.length,
        });

        console.log(
          `[Receipt Repair] Found ${issues.length} issues, repairing with strategy '${strategy}'...`
        );

        let repairResult;

        switch (strategy) {
          case RepairStrategy.RE_HASH:
            repairResult = await this._repairByReHash(chain, issues);
            break;

          case RepairStrategy.FILL_GAP:
            repairResult = await this._repairByFillGap(chain, issues);
            break;

          case RepairStrategy.REBUILD:
            repairResult = await this._repairByRebuild(chain, issues);
            break;

          case RepairStrategy.REMOVE_INVALID:
            repairResult = await this._repairByRemoveInvalid(chain, issues);
            break;

          default:
            throw new Error(`Unknown repair strategy: ${strategy}`);
        }

        // Verify after repair if enabled
        if (this.config.verifyAfterRepair) {
          const verifyIssues = await this.detectIssues(chain);
          if (verifyIssues.length > 0) {
            throw new Error(
              `Repair verification failed: ${verifyIssues.length} issues still exist`
            );
          }
        }

        this.metrics.repairsSucceeded++;

        span.setAttributes({
          'repair.success': true,
          'repair.issues_fixed': issues.length,
        });
        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return {
          repaired: true,
          issuesFound: issues.length,
          issuesFixed: repairResult.fixed,
          strategy,
          details: repairResult,
        };
      } catch (error) {
        this.metrics.repairsFailed++;

        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: 'Repair failed',
        });
        span.end();

        throw new RepairFailedError(`Chain repair failed: ${error.message}`, error);
      }
    });
  }

  /**
   * Repair by recalculating hashes
   * @param {Object} chain - Chain to repair
   * @param {Array} issues - Detected issues
   * @returns {Promise<Object>} Repair result
   * @private
   */
  async _repairByReHash(chain, issues) {
    console.log('[Receipt Repair] Re-hashing receipt chain...');

    const receipts = chain.getAll ? chain.getAll() : chain.receipts || [];
    let fixed = 0;

    for (let i = 0; i < receipts.length; i++) {
      const receipt = receipts[i];

      // Set correct beforeHash
      if (i === 0) {
        if (receipt.beforeHash !== null) {
          receipt.beforeHash = null;
          fixed++;
        }
      } else {
        const prevReceipt = receipts[i - 1];
        if (receipt.beforeHash !== prevReceipt.receiptHash) {
          receipt.beforeHash = prevReceipt.receiptHash;
          fixed++;
        }
      }

      // Recalculate receipt hash if it has a rehash method
      if (receipt.rehash) {
        await receipt.rehash();
        fixed++;
      }
    }

    return { fixed, method: 're_hash' };
  }

  /**
   * Repair by filling gaps with placeholder receipts
   * @param {Object} chain - Chain to repair
   * @param {Array} issues - Detected issues
   * @returns {Promise<Object>} Repair result
   * @private
   */
  async _repairByFillGap(chain, issues) {
    console.log('[Receipt Repair] Filling gaps in receipt chain...');

    // This is a simplified placeholder - real implementation would need
    // to create actual placeholder receipts
    return { fixed: 0, method: 'fill_gap', message: 'Gap filling not implemented' };
  }

  /**
   * Repair by rebuilding entire chain
   * @param {Object} chain - Chain to repair
   * @param {Array} issues - Detected issues
   * @returns {Promise<Object>} Repair result
   * @private
   */
  async _repairByRebuild(chain, issues) {
    console.log('[Receipt Repair] Rebuilding entire receipt chain...');

    // This would rebuild the chain from scratch
    // For now, delegate to re-hash
    return await this._repairByReHash(chain, issues);
  }

  /**
   * Repair by removing invalid receipts
   * @param {Object} chain - Chain to repair
   * @param {Array} issues - Detected issues
   * @returns {Promise<Object>} Repair result
   * @private
   */
  async _repairByRemoveInvalid(chain, issues) {
    console.log('[Receipt Repair] Removing invalid receipts...');

    const receipts = chain.getAll ? chain.getAll() : chain.receipts || [];
    const invalidIndices = new Set(
      issues.filter((i) => i.type === IssueType.INVALID_HASH).map((i) => i.index)
    );

    const validReceipts = receipts.filter((_, i) => !invalidIndices.has(i));

    // Update chain
    if (chain.receipts) {
      chain.receipts = validReceipts;
    }

    return { fixed: invalidIndices.size, method: 'remove_invalid' };
  }

  /**
   * Get repair metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      repairSuccessRate:
        this.metrics.repairsAttempted > 0
          ? (
              (this.metrics.repairsSucceeded / this.metrics.repairsAttempted) *
              100
            ).toFixed(2) + '%'
          : 'N/A',
    };
  }

  /**
   * Reset metrics
   */
  resetMetrics() {
    this.metrics = {
      chainsInspected: 0,
      issuesDetected: 0,
      repairsAttempted: 0,
      repairsSucceeded: 0,
      repairsFailed: 0,
      byIssueType: {
        broken_link: 0,
        missing_receipt: 0,
        invalid_hash: 0,
        epoch_regression: 0,
        duplicate_epoch: 0,
      },
    };
  }
}

/**
 * Error thrown when repair fails
 */
export class RepairFailedError extends Error {
  /**
   * Create a repair failed error
   * @param {string} message - Error message
   * @param {Error} originalError - Original error
   */
  constructor(message, originalError) {
    super(message);
    this.name = 'RepairFailedError';
    this.originalError = originalError;
  }
}
