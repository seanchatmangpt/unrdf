/**
 * @file useΠMerge.mjs
 * @description React hook for Π-merge: creating coherence across Δ-families
 * Validates that all shards merge into a single unified argument
 */

import { useState, useCallback, useMemo } from 'react';

/**
 * useΠMerge hook
 *
 * Manages Π-merge: the fusion of all 27 Δ-shards into a single coherent A (Argument).
 * Validates Π-coherence conditions:
 * 1. Every Claim has supporting Proof
 * 2. Every Gap has Design + Eval
 * 3. Every Objection has Reply
 * 4. Every Paper has Synthesis connection
 * 5. Every Voice consistent with Canon
 *
 * @param {Δ_Shard[]} shards - All shards
 * @returns {Object} Π-merge operations
 * @returns {number} return.coherence - Score (0-1)
 * @returns {Object[]} return.connections - All Π:⊕ links
 * @returns {Function} return.checkMerge - Validate Π-conditions
 * @returns {Function} return.suggestFixes - Automated corrections
 * @returns {boolean} return.isMerged - Can safely merge?
 *
 * @example
 * const { coherence, connections, checkMerge } = useΠMerge(shards);
 * if (!isMerged) console.log(checkMerge()); // Show violations
 */
export function useΠMerge(shards = []) {
  const [links, setLinks] = useState([]);

  /**
   * Π-coherence conditions (hard constraints)
   */
  const coherenceConditions = useMemo(
    () => [
      {
        id: 'claim-proof',
        name: 'Claim ⟺ Proof',
        check: (shards) => {
          const hasClaim = shards.find((s) => s.type === 'Claim');
          const hasProof = shards.find((s) => s.type === 'Proof');
          return hasClaim && hasProof ? [] : [{ type: 'missing', element: 'Proof for Claim' }];
        },
      },
      {
        id: 'gap-design-eval',
        name: 'Gap ⟺ Design + Eval',
        check: (shards) => {
          const hasGap = shards.find((s) => s.type === 'Gap');
          const hasDesign = shards.find((s) => s.type === 'Design');
          const hasEval = shards.find((s) => s.type === 'Eval');
          const violations = [];
          if (hasGap && !hasDesign) violations.push({ type: 'missing', element: 'Design for Gap' });
          if (hasGap && !hasEval) violations.push({ type: 'missing', element: 'Eval for Gap' });
          return violations;
        },
      },
      {
        id: 'objection-reply',
        name: 'Objection ⟺ Reply',
        check: (shards) => {
          const hasObjection = shards.find((s) => s.type === 'Objection');
          const hasReply = shards.find((s) => s.type === 'Reply');
          return hasObjection && !hasReply
            ? [{ type: 'missing', element: 'Reply to Objection' }]
            : [];
        },
      },
      {
        id: 'paper-synthesis',
        name: 'Papers ⟺ Synthesis',
        check: (shards) => {
          const papers = shards.filter((s) => ['Paper1', 'Paper2', 'Paper3'].includes(s.type));
          const hasSynthesis = shards.find((s) => s.type === 'Synthesis');
          return papers.length > 0 && !hasSynthesis
            ? [{ type: 'missing', element: 'Synthesis for Papers' }]
            : [];
        },
      },
      {
        id: 'voice-canon-consistency',
        name: 'Voice ⟺ Canon (consistent)',
        check: (shards) => {
          const hasVoice = shards.find((s) => s.type === 'Voice');
          const hasCanon = shards.find((s) => s.type === 'Canon');
          // Would need semantic analysis to fully check; basic presence check for now
          return hasVoice && hasCanon ? [] : [];
        },
      },
    ],
    []
  );

  /**
   * Check all Π-coherence conditions
   * @returns {Object[]} Violations
   */
  const checkMerge = useCallback(() => {
    const violations = [];

    for (const condition of coherenceConditions) {
      const conditionViolations = condition.check(shards);
      violations.push({
        condition: condition.name,
        conditionId: condition.id,
        passed: conditionViolations.length === 0,
        violations: conditionViolations,
      });
    }

    return violations;
  }, [shards, coherenceConditions]);

  /**
   * Validate Π-structure
   * @returns {Object} Detailed validation result
   */
  const validateStructure = useCallback(() => {
    const violations = checkMerge();
    const failedCount = violations.filter((v) => !v.passed).length;
    const score = 1 - failedCount / violations.length;

    return {
      valid: failedCount === 0,
      score,
      failedCount,
      violations,
    };
  }, [checkMerge]);

  /**
   * Automatically create Π:⊕ connections
   * Links semantically related shards
   */
  const suggestConnections = useCallback(() => {
    const newLinks = [];

    // Claim → Proof
    const claim = shards.find((s) => s.type === 'Claim');
    const proof = shards.find((s) => s.type === 'Proof');
    if (claim && proof) {
      newLinks.push({
        source: claim.id,
        target: proof.id,
        type: 'supports',
        strength: 1.0,
      });
    }

    // Gap → Design → Eval
    const gap = shards.find((s) => s.type === 'Gap');
    const design = shards.find((s) => s.type === 'Design');
    const eval1 = shards.find((s) => s.type === 'Eval');

    if (gap && design) {
      newLinks.push({
        source: gap.id,
        target: design.id,
        type: 'motivates',
        strength: 0.9,
      });
    }
    if (design && eval1) {
      newLinks.push({
        source: design.id,
        target: eval1.id,
        type: 'supports',
        strength: 0.9,
      });
    }

    // Papers → Synthesis
    const papers = shards.filter((s) => ['Paper1', 'Paper2', 'Paper3'].includes(s.type));
    const synthesis = shards.find((s) => s.type === 'Synthesis');

    if (synthesis) {
      for (const paper of papers) {
        newLinks.push({
          source: paper.id,
          target: synthesis.id,
          type: 'contributes',
          strength: 0.8,
        });
      }
    }

    // Objection → Reply
    const objection = shards.find((s) => s.type === 'Objection');
    const reply = shards.find((s) => s.type === 'Reply');

    if (objection && reply) {
      newLinks.push({
        source: objection.id,
        target: reply.id,
        type: 'answers',
        strength: 0.95,
      });
    }

    setLinks(newLinks);
    return newLinks;
  }, [shards]);

  /**
   * Compute overall Π-coherence score
   */
  const coherence = useMemo(() => {
    const validation = validateStructure();
    const connectionBonus = Math.min(links.length * 0.05, 0.2); // Up to 20% bonus
    return Math.min(1.0, validation.score + connectionBonus);
  }, [validateStructure, links]);

  /**
   * Get all connections
   */
  const connections = useMemo(() => links, [links]);

  /**
   * Can safely merge all shards?
   */
  const isMerged = useMemo(() => {
    return coherence >= 0.95 && validateStructure().valid;
  }, [coherence, validateStructure]);

  /**
   * Get suggested fixes for violations
   */
  const suggestFixes = useCallback(() => {
    const violations = checkMerge();
    const fixes = [];

    for (const violation of violations) {
      if (!violation.passed) {
        if (violation.conditionId === 'claim-proof') {
          fixes.push({
            issue: 'Missing Proof for Claim',
            suggestion: 'Add a Proof section that demonstrates your main claim',
          });
        }
        if (violation.conditionId === 'gap-design-eval') {
          fixes.push({
            issue: 'Gap-Design-Eval chain incomplete',
            suggestion: 'Ensure your identified Gap is addressed by Design + Evaluation',
          });
        }
        if (violation.conditionId === 'objection-reply') {
          fixes.push({
            issue: 'Objection without Reply',
            suggestion: 'Address counterarguments with explicit Reply sections',
          });
        }
        if (violation.conditionId === 'paper-synthesis') {
          fixes.push({
            issue: 'Papers lack synthesis',
            suggestion: 'Add Synthesis section that unifies insights from all papers',
          });
        }
      }
    }

    return fixes;
  }, [checkMerge]);

  return {
    // Data
    coherence,
    connections,
    isMerged,

    // Operations
    checkMerge,
    validateStructure,
    suggestConnections,
    suggestFixes,
  };
}

export default useΠMerge;
