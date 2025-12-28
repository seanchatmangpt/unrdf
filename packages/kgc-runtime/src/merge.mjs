/**
 * @fileoverview Multi-agent capsule merge and conflict resolution system
 * Implements deterministic merge strategies for concurrent agent operations
 */

import { z } from 'zod';

// ============================================================================
// Schemas
// ============================================================================

const CapsuleSchema = z.object({
  id: z.string(),
  o_hash: z.string(),
  file_edits: z.array(
    z.object({
      file_path: z.string(),
      line_start: z.number(),
      line_end: z.number(),
      content: z.string(),
      operation: z.enum(['insert', 'replace', 'delete']).default('replace'),
    })
  ),
  metadata: z.object({}).passthrough().optional(),
});

const ConflictRuleSchema = z.object({
  strategy: z.enum(['earlier_wins', 'later_wins', 'lexicographic', 'merge_all']),
  priority_field: z.string().optional(),
});

const TotalOrderSchema = z.object({
  rules: z.array(ConflictRuleSchema),
  default_rule: ConflictRuleSchema,
});

const ConflictReceiptSchema = z.object({
  conflict_id: z.string(),
  file_path: z.string(),
  capsules_involved: z.array(z.string()),
  line_ranges: z.array(
    z.object({
      capsule_id: z.string(),
      line_start: z.number(),
      line_end: z.number(),
    })
  ),
  resolution_rule: z.string(),
  winner: z.string().optional(),
  denied: z.array(z.string()).optional(),
  timestamp: z.string(),
});

const MergeResultSchema = z.object({
  admitted: z.array(z.string()),
  denied: z.array(z.string()),
  conflict_receipts: z.array(ConflictReceiptSchema),
  merged_state: z.record(z.string(), z.any()).optional(),
});

// ============================================================================
// Type definitions (JSDoc)
// ============================================================================

/**
 * @typedef {Object} FileEdit
 * @property {string} file_path - Path to the file
 * @property {number} line_start - Starting line number
 * @property {number} line_end - Ending line number
 * @property {string} content - Content to apply
 * @property {'insert'|'replace'|'delete'} operation - Type of edit operation
 */

/**
 * @typedef {Object} Capsule
 * @property {string} id - Unique capsule identifier
 * @property {string} o_hash - Ordering hash for deterministic resolution
 * @property {FileEdit[]} file_edits - Array of file edits
 * @property {Object} [metadata] - Optional metadata
 */

/**
 * @typedef {Object} ConflictRule
 * @property {'earlier_wins'|'later_wins'|'lexicographic'|'merge_all'} strategy - Resolution strategy
 * @property {string} [priority_field] - Field to use for priority
 */

/**
 * @typedef {Object} TotalOrder
 * @property {ConflictRule[]} rules - Array of resolution rules
 * @property {ConflictRule} default_rule - Default resolution rule
 */

/**
 * @typedef {Object} ConflictRegion
 * @property {string} file_path - Path to the file
 * @property {number} line_start - Starting line of conflict
 * @property {number} line_end - Ending line of conflict
 * @property {Array<{capsule_id: string, edit: FileEdit}>} overlapping_edits - Conflicting edits
 */

/**
 * @typedef {Object} ConflictReceipt
 * @property {string} conflict_id - Unique conflict identifier
 * @property {string} file_path - Path to the conflicting file
 * @property {string[]} capsules_involved - IDs of capsules in conflict
 * @property {Array<{capsule_id: string, line_start: number, line_end: number}>} line_ranges - Line ranges for each capsule
 * @property {string} resolution_rule - Rule applied to resolve
 * @property {string} [winner] - ID of winning capsule
 * @property {string[]} [denied] - IDs of denied capsules
 * @property {string} timestamp - ISO timestamp of resolution
 */

/**
 * @typedef {Object} MergeResult
 * @property {string[]} admitted - IDs of admitted capsules
 * @property {string[]} denied - IDs of denied capsules
 * @property {ConflictReceipt[]} conflict_receipts - Array of conflict receipts
 * @property {Object} [merged_state] - Optional merged state object
 */

// ============================================================================
// ConflictDetector
// ============================================================================

/**
 * Detects conflicts between capsule file edits
 */
export class ConflictDetector {
  /**
   * Check if two line ranges overlap
   * @param {number} start1 - Start of first range
   * @param {number} end1 - End of first range
   * @param {number} start2 - Start of second range
   * @param {number} end2 - End of second range
   * @returns {boolean} True if ranges overlap
   */
  static rangesOverlap(start1, end1, start2, end2) {
    return start1 <= end2 && start2 <= end1;
  }

  /**
   * Detect conflicts in an array of capsules
   * @param {Capsule[]} capsules - Array of capsules to check
   * @returns {ConflictRegion[]} Array of detected conflicts
   */
  static detectConflicts(capsules) {
    const conflicts = [];
    const fileEditMap = new Map();

    // Group edits by file path
    for (const capsule of capsules) {
      for (const edit of capsule.file_edits) {
        if (!fileEditMap.has(edit.file_path)) {
          fileEditMap.set(edit.file_path, []);
        }
        fileEditMap.get(edit.file_path).push({
          capsule_id: capsule.id,
          edit,
        });
      }
    }

    // Check for overlaps within each file
    for (const [file_path, edits] of fileEditMap.entries()) {
      if (edits.length < 2) continue;

      for (let i = 0; i < edits.length; i++) {
        const overlapping = [edits[i]];

        for (let j = i + 1; j < edits.length; j++) {
          if (
            this.rangesOverlap(
              edits[i].edit.line_start,
              edits[i].edit.line_end,
              edits[j].edit.line_start,
              edits[j].edit.line_end
            )
          ) {
            overlapping.push(edits[j]);
          }
        }

        if (overlapping.length > 1) {
          // Create conflict region
          const line_start = Math.min(...overlapping.map((e) => e.edit.line_start));
          const line_end = Math.max(...overlapping.map((e) => e.edit.line_end));

          conflicts.push({
            file_path,
            line_start,
            line_end,
            overlapping_edits: overlapping,
          });

          // Skip already processed edits
          i += overlapping.length - 1;
        }
      }
    }

    return conflicts;
  }

  /**
   * Create a conflict receipt for a detected conflict
   * @param {ConflictRegion} conflict - Detected conflict region
   * @param {string} resolution_rule - Rule applied to resolve
   * @param {string} [winner] - ID of winning capsule
   * @param {string[]} [denied] - IDs of denied capsules
   * @returns {ConflictReceipt} Conflict receipt
   */
  static createReceipt(conflict, resolution_rule, winner = null, denied = []) {
    const capsules_involved = conflict.overlapping_edits.map((e) => e.capsule_id);

    return {
      conflict_id: `conflict_${conflict.file_path}_${conflict.line_start}_${Date.now()}`,
      file_path: conflict.file_path,
      capsules_involved,
      line_ranges: conflict.overlapping_edits.map((e) => ({
        capsule_id: e.capsule_id,
        line_start: e.edit.line_start,
        line_end: e.edit.line_end,
      })),
      resolution_rule,
      winner,
      denied,
      timestamp: new Date().toISOString(),
    };
  }
}

// ============================================================================
// Resolution Strategies
// ============================================================================

/**
 * Resolves conflicts using deterministic rules
 */
export class ConflictResolver {
  /**
   * Apply earlier_wins strategy - earlier o_hash wins
   * @param {ConflictRegion} conflict - Conflict to resolve
   * @param {Capsule[]} capsules - All capsules
   * @returns {{winner: string, denied: string[]}} Resolution result
   */
  static earlierWins(conflict, capsules) {
    const capsuleMap = new Map(capsules.map((c) => [c.id, c]));
    const involved = conflict.overlapping_edits
      .map((e) => ({
        id: e.capsule_id,
        hash: capsuleMap.get(e.capsule_id)?.o_hash || '',
      }))
      .sort((a, b) => a.hash.localeCompare(b.hash));

    return {
      winner: involved[0].id,
      denied: involved.slice(1).map((c) => c.id),
    };
  }

  /**
   * Apply lexicographic strategy - lexicographically first capsule_id wins
   * @param {ConflictRegion} conflict - Conflict to resolve
   * @returns {{winner: string, denied: string[]}} Resolution result
   */
  static lexicographic(conflict) {
    const involved = conflict.overlapping_edits
      .map((e) => e.capsule_id)
      .sort((a, b) => a.localeCompare(b));

    return {
      winner: involved[0],
      denied: involved.slice(1),
    };
  }

  /**
   * Apply later_wins strategy - later o_hash wins
   * @param {ConflictRegion} conflict - Conflict to resolve
   * @param {Capsule[]} capsules - All capsules
   * @returns {{winner: string, denied: string[]}} Resolution result
   */
  static laterWins(conflict, capsules) {
    const capsuleMap = new Map(capsules.map((c) => [c.id, c]));
    const involved = conflict.overlapping_edits
      .map((e) => ({
        id: e.capsule_id,
        hash: capsuleMap.get(e.capsule_id)?.o_hash || '',
      }))
      .sort((a, b) => b.hash.localeCompare(a.hash));

    return {
      winner: involved[0].id,
      denied: involved.slice(1).map((c) => c.id),
    };
  }

  /**
   * Resolve conflict using specified rule
   * @param {ConflictRegion} conflict - Conflict to resolve
   * @param {ConflictRule} rule - Resolution rule
   * @param {Capsule[]} capsules - All capsules
   * @returns {{winner: string, denied: string[], rule: string}} Resolution result
   */
  static resolveConflict(conflict, rule, capsules) {
    let result;

    switch (rule.strategy) {
      case 'earlier_wins':
        result = this.earlierWins(conflict, capsules);
        break;
      case 'later_wins':
        result = this.laterWins(conflict, capsules);
        break;
      case 'lexicographic':
        result = this.lexicographic(conflict);
        break;
      case 'merge_all':
        // Admit all capsules, no winner
        return {
          winner: null,
          denied: [],
          rule: rule.strategy,
        };
      default:
        // Default to earlier_wins
        result = this.earlierWins(conflict, capsules);
    }

    return {
      ...result,
      rule: rule.strategy,
    };
  }
}

// ============================================================================
// ShardMerge
// ============================================================================

/**
 * Performs shard merge with deterministic conflict resolution
 * @param {Capsule[]} capsules - Array of capsules to merge
 * @param {TotalOrder} totalOrder - Ordering rules for resolution
 * @returns {{merged_state: Object|null, conflict_receipts: ConflictReceipt[]}} Merge result
 */
export function shardMerge(capsules, totalOrder) {
  // Validate inputs
  const validatedCapsules = z.array(CapsuleSchema).parse(capsules);
  const validatedOrder = TotalOrderSchema.parse(totalOrder);

  // Detect conflicts
  const conflicts = ConflictDetector.detectConflicts(validatedCapsules);

  if (conflicts.length === 0) {
    // No conflicts - simple merge
    const merged_state = {};
    for (const capsule of validatedCapsules) {
      merged_state[capsule.id] = {
        file_edits: capsule.file_edits,
        metadata: capsule.metadata,
      };
    }
    return { merged_state, conflict_receipts: [] };
  }

  // Resolve conflicts
  const conflict_receipts = [];
  const denied_capsules = new Set();

  for (const conflict of conflicts) {
    const rule = validatedOrder.default_rule;
    const resolution = ConflictResolver.resolveConflict(
      conflict,
      rule,
      validatedCapsules
    );

    // For merge_all strategy, don't deny any capsules - just emit receipts
    if (rule.strategy !== 'merge_all' && resolution.denied) {
      resolution.denied.forEach((id) => denied_capsules.add(id));
    }

    // Create receipt
    const receipt = ConflictDetector.createReceipt(
      conflict,
      resolution.rule,
      resolution.winner,
      resolution.denied
    );
    conflict_receipts.push(receipt);
  }

  // Build merged state
  const merged_state = {};
  for (const capsule of validatedCapsules) {
    // For merge_all, include all capsules; otherwise only admitted
    if (validatedOrder.default_rule.strategy === 'merge_all' || !denied_capsules.has(capsule.id)) {
      merged_state[capsule.id] = {
        file_edits: capsule.file_edits,
        metadata: capsule.metadata,
      };
    }
  }

  return { merged_state, conflict_receipts };
}

// ============================================================================
// mergeCapsules - Main API
// ============================================================================

/**
 * Merge multiple capsules with conflict resolution
 * @param {Capsule[]} capsules - Array of capsules to merge
 * @param {TotalOrder} totalOrder - Ordering rules for resolution
 * @returns {MergeResult} Merge result with admitted/denied capsules and receipts
 */
export function mergeCapsules(capsules, totalOrder) {
  try {
    // Validate inputs
    const validatedCapsules = z.array(CapsuleSchema).parse(capsules);
    const validatedOrder = TotalOrderSchema.parse(totalOrder);

    // Perform shard merge
    const { merged_state, conflict_receipts } = shardMerge(
      validatedCapsules,
      validatedOrder
    );

    // Determine admitted and denied
    const admitted = [];
    const denied = [];

    const deniedSet = new Set();
    for (const receipt of conflict_receipts) {
      if (receipt.denied) {
        receipt.denied.forEach((id) => deniedSet.add(id));
      }
    }

    for (const capsule of validatedCapsules) {
      if (deniedSet.has(capsule.id)) {
        denied.push(capsule.id);
      } else {
        admitted.push(capsule.id);
      }
    }

    return {
      admitted,
      denied,
      conflict_receipts,
      merged_state,
    };
  } catch (error) {
    // Handle validation errors
    throw new Error(`Merge failed: ${error.message}`);
  }
}

// ============================================================================
// Exports
// ============================================================================

export default {
  shardMerge,
  mergeCapsules,
  ConflictDetector,
  ConflictResolver,
};
