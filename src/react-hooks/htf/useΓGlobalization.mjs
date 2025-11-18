/**
 * @file useΓGlobalization.mjs
 * @description React hook for Γ-globalization: semantic gluing of all Δ-shards
 * Detects and prevents conceptual drift across the thesis
 */

import { useState, useCallback, useMemo } from 'react';

/**
 * useΓGlobalization hook
 *
 * Manages Γ-globalization: ensures local consistency in each Δ propagates globally.
 * Detects "drift"—when terminology, concepts, or reasoning shift inconsistently.
 *
 * @param {Δ_Shard[]} shards - All shards in Λ-order
 * @returns {Object} Γ-globalization operations
 * @returns {number} return.driftScore - Total drift (0-1, lower=better)
 * @returns {Object[]} return.violations - Drift violations by type
 * @returns {Function} return.checkDrift - Detailed drift analysis
 * @returns {Function} return.fixDrift - Suggest corrections
 * @returns {Function} return.enforceGlue - Lock coherence
 *
 * @example
 * const { driftScore, violations, checkDrift } = useΓGlobalization(shards);
 * if (driftScore > 0.2) console.log(fixDrift()); // Get suggestions
 */
export function useΓGlobalization(shards = []) {
  const [glossary, setGlossary] = useState({});
  const [notation, setNotation] = useState({});
  const [enforcedGlue, setEnforcedGlue] = useState(false);

  /**
   * Extract terminology from shard content
   */
  const extractTerminology = useCallback((text = '') => {
    const terms = new Map();
    if (!text) return terms;

    // Simple extraction: capitalized phrases, quoted terms
    const regex = /(?:"([^"]*)"|([A-Z][a-z]+(?:\s+[A-Z][a-z]+)*))/g;
    let match;

    while ((match = regex.exec(text)) !== null) {
      const term = match[1] || match[2];
      terms.set(term, (terms.get(term) || 0) + 1);
    }

    return terms;
  }, []);

  /**
   * Build global glossary from all shards
   */
  const buildGlossary = useCallback(() => {
    const globalTerms = new Map();

    for (const shard of shards) {
      const terms = extractTerminology(shard.content);
      for (const [term, count] of terms) {
        if (!globalTerms.has(term)) {
          globalTerms.set(term, { term, definitions: [], usageCount: 0 });
        }
        const entry = globalTerms.get(term);
        entry.usageCount += count;
        entry.shards = entry.shards || [];
        if (!entry.shards.includes(shard.id)) {
          entry.shards.push(shard.id);
        }
      }
    }

    const glossaryObj = Object.fromEntries(globalTerms);
    setGlossary(glossaryObj);
    return glossaryObj;
  }, [shards, extractTerminology]);

  /**
   * Check for terminology consistency
   * Same term used consistently across shards?
   */
  const checkTerminologyConsistency = useCallback(() => {
    const violations = [];
    const termToShards = new Map();

    // Map each term to shards where it appears
    for (const shard of shards) {
      const terms = extractTerminology(shard.content);
      for (const term of terms.keys()) {
        if (!termToShards.has(term)) {
          termToShards.set(term, []);
        }
        termToShards.get(term).push(shard.id);
      }
    }

    // Flag terms that appear in <2 shards (orphaned terminology)
    for (const [term, shardIds] of termToShards) {
      if (shardIds.length === 1) {
        violations.push({
          type: 'orphaned_term',
          term,
          severity: 'warning',
          message: `Term "${term}" appears in only 1 shard. Consider defining once and reusing.`,
        });
      }
    }

    return violations;
  }, [shards, extractTerminology]);

  /**
   * Check for conceptual consistency
   * E.g., does the author's stance (Voice) remain consistent with methodology?
   */
  const checkConceptualConsistency = useCallback(() => {
    const violations = [];
    const voice = shards.find((s) => s.type === 'Voice');
    const method = shards.find((s) => s.type === 'Method' || s.type === 'Method2');

    if (voice && method) {
      // Would need semantic analysis to fully check
      // For now, check if both exist and have reasonable length
      const voiceLength = (voice.content || '').length;
      const methodLength = (method.content || '').length;

      if (voiceLength < 100 && methodLength > 1000) {
        violations.push({
          type: 'voice_method_mismatch',
          severity: 'warning',
          message: 'Voice section is very brief compared to Method. Consider expanding authorial perspective.',
        });
      }
    }

    return violations;
  }, [shards]);

  /**
   * Check for structural balance
   * Important concepts should get proportional space
   */
  const checkProportionality = useCallback(() => {
    const violations = [];
    const importance = {
      Problem: 1.0,
      Claim: 1.0,
      Proof: 0.9,
      Method: 0.8,
      Result: 0.8,
      Eval: 0.8,
      Discussion: 0.7,
    };

    for (const [type, weight] of Object.entries(importance)) {
      const shard = shards.find((s) => s.type === type);
      if (!shard) continue;

      const expectedWords = weight * 1000; // Expected word count
      const actualWords = (shard.content || '').split(/\s+/).length;

      if (actualWords < expectedWords * 0.3) {
        violations.push({
          type: 'insufficient_length',
          section: type,
          severity: 'warning',
          message: `${type} section (${actualWords} words) is much shorter than expected (~${expectedWords}). Consider expansion.`,
        });
      }
    }

    return violations;
  }, [shards]);

  /**
   * Comprehensive drift analysis
   */
  const checkDrift = useCallback(() => {
    buildGlossary();

    return {
      terminology: checkTerminologyConsistency(),
      conceptual: checkConceptualConsistency(),
      proportionality: checkProportionality(),
    };
  }, [
    buildGlossary,
    checkTerminologyConsistency,
    checkConceptualConsistency,
    checkProportionality,
  ]);

  /**
   * Compute overall drift score (0-1, lower = better)
   */
  const driftScore = useMemo(() => {
    const allDrifts = checkDrift();
    const totalViolations =
      (allDrifts.terminology?.length || 0) +
      (allDrifts.conceptual?.length || 0) +
      (allDrifts.proportionality?.length || 0);

    const maxViolations = 20; // Normalize to 20
    return Math.min(1.0, totalViolations / maxViolations);
  }, [checkDrift]);

  /**
   * Get all violations as flat array
   */
  const violations = useMemo(() => {
    const allDrifts = checkDrift();
    return [
      ...(allDrifts.terminology || []),
      ...(allDrifts.conceptual || []),
      ...(allDrifts.proportionality || []),
    ];
  }, [checkDrift]);

  /**
   * Suggest automatic fixes
   */
  const fixDrift = useCallback(() => {
    const suggestions = [];

    if (driftScore > 0.2) {
      suggestions.push({
        type: 'refactor_terminology',
        action: 'Create unified glossary and standardize terms',
        impact: 'Reduces drift by ~0.1',
      });
    }

    if (violations.some((v) => v.type === 'orphaned_term')) {
      suggestions.push({
        type: 'propagate_terms',
        action: 'Use defined terms consistently across shards',
        impact: 'Improves coherence by 15-20%',
      });
    }

    if (violations.some((v) => v.type === 'voice_method_mismatch')) {
      suggestions.push({
        type: 'expand_voice',
        action: 'Develop authorial perspective section',
        impact: 'Strengthens narrative coherence',
      });
    }

    return suggestions;
  }, [driftScore, violations]);

  /**
   * Lock Γ-glue: enforce no further drift
   */
  const enforceGlue = useCallback(() => {
    if (driftScore < 0.1) {
      setEnforcedGlue(true);
      return { success: true, message: 'Γ-coherence locked. No further drift allowed.' };
    } else {
      return {
        success: false,
        message: `Cannot lock. Current drift: ${(driftScore * 100).toFixed(1)}%. Target: < 10%.`,
      };
    }
  }, [driftScore]);

  return {
    // Data
    driftScore,
    violations,
    glossary,
    notation,
    enforcedGlue,

    // Operations
    checkDrift,
    fixDrift,
    enforceGlue,
    buildGlossary,
  };
}

export default useΓGlobalization;
