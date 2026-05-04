/**
 * @fileoverview Federation Quorum Voting for PICTL Results
 * @module @unrdf/pictl-semantics/quorum
 *
 * Implements M-of-N quorum consensus for validating PICTL process mining results.
 * Uses BLAKE3 receipt chaining for cryptographic proof of votes.
 *
 * Pattern: Multiple PICTL instances validate a result against local SHACL shapes.
 * Consensus: ≥threshold votes approve → result accepted; <threshold → rejected.
 */

import { z } from 'zod';
import { randomBytes, createHash } from 'crypto';
import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/pictl-semantics', '26.4.23');

/**
 * Quorum Configuration Schema
 */
export const QuorumConfigSchema = z.object({
  nodeId: z.string().min(1),
  quorumThreshold: z.number().int().positive().default(2),
  ontologyPath: z.string().optional(),
  shapesPath: z.string().optional(),
});

/**
 * Vote Schema
 */
const VoteSchema = z.object({
  resultId: z.string().min(1),
  nodeId: z.string().min(1),
  vote: z.enum(['approve', 'reject']),
  reason: z.string().optional(),
  timestamp: z.number().int().positive(),
});

/**
 * BLAKE3 Receipt Schema
 */
const ReceiptSchema = z.object({
  hash: z.string().min(64), // BLAKE3 hash (hex)
  previousHash: z.string().min(64).optional(),
  timestamp: z.number().int().positive(),
  nodeId: z.string().min(1),
  resultId: z.string().min(1),
  voteCount: z.number().int().nonnegative(),
  approvalCount: z.number().int().nonnegative(),
});

/**
 * BLAKE3 Hash computation (using Node crypto)
 * Note: Using SHA-256 as BLAKE3 polyfill (same cryptographic strength)
 *
 * @param {string} data - Data to hash
 * @returns {string} Hex hash
 */
function blake3Hash(data) {
  return createHash('sha256').update(data).digest('hex');
}

/**
 * Generate unique result ID
 *
 * @param {Object} result - Result to identify
 * @returns {string} Deterministic result ID
 */
function generateResultId(result) {
  const hash = createHash('sha256');
  hash.update(JSON.stringify(result, null, 2));
  return hash.digest('hex').slice(0, 16);
}

/**
 * Propose a result for federation quorum voting
 *
 * One PICTL instance proposes a result (e.g., fitness=0.92) to the quorum.
 * Result is stored pending votes from other nodes.
 *
 * @param {Object} result - PICTL result (fitness, precision, model, etc.)
 * @param {string} nodeId - Proposing node ID
 * @param {Object} quorumState - Shared quorum state
 * @returns {Object} Proposed result with metadata
 *
 * @example
 * const proposed = proposeResult(
 *   { fitness: 0.92, precision: 0.88, model: 'petri-net' },
 *   'pictl-node-1',
 *   quorumState
 * );
 */
export function proposeResult(result, nodeId, quorumState) {
  const span = tracer.startSpan('quorum.propose_result');
  try {
    const resultId = generateResultId(result);
    const timestamp = Date.now();

    const proposed = {
      resultId,
      result,
      proposerId: nodeId,
      timestamp,
      votes: [],
      status: 'pending', // pending, approved, rejected
    };

    quorumState.results.set(resultId, proposed);

    span.addEvent('result_proposed', {
      'quorum.result_id': resultId,
      'quorum.proposer': nodeId,
    });

    span.setStatus({ code: SpanStatusCode.OK });
    return proposed;
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
    throw error;
  } finally {
    span.end();
  }
}

/**
 * Cast a vote on a proposed result
 *
 * Each node validates the result against local SHACL shapes and votes.
 * Quorum records vote with cryptographic timestamp.
 * When vote count reaches threshold, result status updates to approved/rejected.
 *
 * @param {string} resultId - Result ID to vote on
 * @param {string} vote - 'approve' or 'reject'
 * @param {string} reason - Optional reason for vote
 * @param {Object} quorumState - Shared quorum state
 * @returns {Object} Vote record with receipt info
 *
 * @example
 * const vote = votePictlResult(
 *   'abc123def456',
 *   'approve',
 *   'SHACL validation passed',
 *   quorumState
 * );
 */
export function votePictlResult(resultId, vote, reason, quorumState) {
  const span = tracer.startSpan('quorum.vote_result');
  try {
    // Validate vote
    const voteData = VoteSchema.parse({
      resultId,
      nodeId: quorumState.nodeId,
      vote: vote === 'approve' || vote === 'reject' ? vote : 'reject',
      reason: reason || '',
      timestamp: Date.now(),
    });

    // Get the proposed result
    const proposed = quorumState.results.get(resultId);
    if (!proposed) {
      throw new Error(`Result not found: ${resultId}`);
    }

    // Record vote
    proposed.votes.push(voteData);
    quorumState.votes.set(`${resultId}:${quorumState.nodeId}`, voteData);

    // Check if quorum reached
    const approvalCount = proposed.votes.filter(v => v.vote === 'approve').length;
    const rejectionCount = proposed.votes.filter(v => v.vote === 'reject').length;
    const totalVotes = proposed.votes.length;

    let newStatus = proposed.status;
    let receipt = null;

    // Determine consensus (≥2/3 threshold) only if status hasn't been set and we have minimum quorum
    if (proposed.status === 'pending' && totalVotes >= quorumState.quorumThreshold) {
      const threshold = Math.ceil(totalVotes * (2 / 3));
      // Need ceiling(2/3 of n) approvals. With 2 votes, need 2 (100%). With 3 votes, need 2 (66.7%)
      const needsApproval = Math.ceil(totalVotes * (2 / 3));

      if (approvalCount >= needsApproval) {
        newStatus = 'approved';
        receipt = _createReceipt(
          resultId,
          quorumState.nodeId,
          totalVotes,
          approvalCount,
          quorumState
        );
        proposed.status = newStatus;
      } else if (rejectionCount > needsApproval - 1) {
        // Can't reach threshold even with remaining votes
        newStatus = 'rejected';
        receipt = _createReceipt(
          resultId,
          quorumState.nodeId,
          totalVotes,
          approvalCount,
          quorumState
        );
        proposed.status = newStatus;
      }
    }

    span.addEvent('vote_recorded', {
      'quorum.result_id': resultId,
      'quorum.voter': quorumState.nodeId,
      'quorum.vote': vote,
      'quorum.vote_count': totalVotes,
      'quorum.approval_count': approvalCount,
      'quorum.status': newStatus,
    });

    span.setStatus({ code: SpanStatusCode.OK });
    return {
      voteId: `${resultId}:${quorumState.nodeId}:${voteData.timestamp}`,
      ...voteData,
      totalVotes,
      approvalCount,
      consensusStatus: newStatus,
      receipt: receipt ? { hash: receipt.hash } : null,
    };
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
    throw error;
  } finally {
    span.end();
  }
}

/**
 * Internal: Create a BLAKE3 receipt for finalized vote
 *
 * @private
 * @param {string} resultId - Result ID
 * @param {string} nodeId - Voting node ID
 * @param {number} voteCount - Total votes cast
 * @param {number} approvalCount - Approval votes
 * @param {Object} quorumState - Quorum state
 * @returns {Object} Receipt with BLAKE3 hash chain
 */
function _createReceipt(resultId, nodeId, voteCount, approvalCount, quorumState) {
  const timestamp = Date.now();

  // Get previous receipt hash (or use zero hash)
  const previousHash =
    quorumState.receipts.length > 0
      ? quorumState.receipts[quorumState.receipts.length - 1].hash
      : '0'.repeat(64);

  // Create receipt data (deterministic ordering for hashing)
  const receiptData = JSON.stringify({
    resultId,
    nodeId,
    timestamp,
    voteCount,
    approvalCount,
    previousHash,
  });

  // BLAKE3 hash of receipt
  const hash = blake3Hash(receiptData);

  const receipt = {
    hash,
    previousHash,
    timestamp,
    nodeId,
    resultId,
    voteCount,
    approvalCount,
  };

  ReceiptSchema.parse(receipt); // Validate schema
  quorumState.receipts.push(receipt);

  return receipt;
}

/**
 * Validate receipt chain integrity
 *
 * Verifies that all receipts form a valid BLAKE3 chain
 * (each receipt's previousHash matches the previous receipt's hash)
 *
 * @param {Object} quorumState - Quorum state with receipts
 * @returns {Object} Chain validation result
 *
 * @example
 * const validation = validateVoteTally(quorumState);
 * if (!validation.valid) {
 *   console.error('Receipt chain broken:', validation.error);
 * }
 */
export function validateVoteTally(quorumState) {
  const span = tracer.startSpan('quorum.validate_tally');
  try {
    const receipts = quorumState.receipts || [];

    if (receipts.length === 0) {
      return {
        valid: true,
        receiptCount: 0,
        message: 'No receipts to validate',
      };
    }

    // Check chain continuity
    for (let i = 1; i < receipts.length; i++) {
      const current = receipts[i];
      const previous = receipts[i - 1];

      if (current.previousHash !== previous.hash) {
        span.addEvent('chain_break_detected', {
          'quorum.break_index': i,
          'quorum.expected_hash': previous.hash,
          'quorum.received_hash': current.previousHash,
        });

        return {
          valid: false,
          receiptCount: receipts.length,
          breakIndex: i,
          error: 'Receipt chain integrity violation at index ' + i,
        };
      }
    }

    span.addEvent('chain_validated', {
      'quorum.receipt_count': receipts.length,
    });

    span.setStatus({ code: SpanStatusCode.OK });
    return {
      valid: true,
      receiptCount: receipts.length,
      chainHash: receipts[receipts.length - 1].hash,
      message: 'All receipts form valid chain',
    };
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
    return {
      valid: false,
      error: error.message,
    };
  } finally {
    span.end();
  }
}

/**
 * Get current quorum status and vote tallies
 *
 * Returns summary of all proposed results, vote counts, and consensus status.
 * Includes receipt chain statistics.
 *
 * @param {Object} quorumState - Quorum state
 * @returns {Object} Quorum status snapshot
 *
 * @example
 * const status = getQuorumStatus(quorumState);
 * console.log(`Approved results: ${status.approvedResults.length}`);
 * console.log(`Receipt chain: ${status.receipts.length} entries`);
 */
export function getQuorumStatus(quorumState) {
  const span = tracer.startSpan('quorum.get_status');
  try {
    const results = Array.from(quorumState.results.values());

    const approved = results.filter(r => r.status === 'approved');
    const rejected = results.filter(r => r.status === 'rejected');
    const pending = results.filter(r => r.status === 'pending');

    // Chain validation
    const chainValidation = validateVoteTally(quorumState);

    const status = {
      nodeId: quorumState.nodeId,
      quorumThreshold: quorumState.quorumThreshold,
      timestamp: Date.now(),
      results: {
        total: results.length,
        approved: approved.length,
        rejected: rejected.length,
        pending: pending.length,
      },
      approvedResults: approved.map(r => ({
        resultId: r.resultId,
        result: r.result,
        voteCount: r.votes.length,
        approvalCount: r.votes.filter(v => v.vote === 'approve').length,
      })),
      rejectedResults: rejected.map(r => ({
        resultId: r.resultId,
        result: r.result,
        voteCount: r.votes.length,
        approvalCount: r.votes.filter(v => v.vote === 'approve').length,
      })),
      receipts: {
        count: quorumState.receipts.length,
        chainValid: chainValidation.valid,
        latestHash: quorumState.receipts.length > 0 ? quorumState.receipts[quorumState.receipts.length - 1].hash : null,
      },
    };

    span.addEvent('status_retrieved', {
      'quorum.total_results': results.length,
      'quorum.approved_count': approved.length,
      'quorum.receipt_count': quorumState.receipts.length,
    });

    span.setStatus({ code: SpanStatusCode.OK });
    return status;
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
    throw error;
  } finally {
    span.end();
  }
}

// Re-export utilities
export { blake3Hash, generateResultId };
