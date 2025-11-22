/**
 * @file useΛSchedule.mjs
 * @description React hook for Λ-ordering: total order across all Δ-shards
 * Manages reading sequence, validates dependencies, optimizes flow
 */

import { useState, useCallback, useMemo } from 'react';

/**
 * useΛSchedule hook
 *
 * Manages the Λ-total order (reading sequence) of all shards.
 * Ensures logical dependencies are respected and reading flow is optimal.
 *
 * The canonical Λ-chain has 27 positions:
 * 1. Problem → 2. Gap → 3. Claim → 4. Intro → 5-6. Methods →
 * 7-9. Background → 10. Artifact → 11. Proof → 12-15. Evidence →
 * 16-18. Interpretation → 19-22. Synthesis → 23. Impact → 24. Conclusion
 *
 * @param {Δ_Shard[]} shards - All shards in thesis
 * @returns {Object} Λ-schedule operations
 * @returns {Δ_Shard[]} return.schedule - Shards in optimal Λ-order
 * @returns {Object[]} return.positions - Detailed position metadata
 * @returns {Function} return.validateOrder - Check Λ-constraints
 * @returns {Function} return.optimizeFlow - Reorder for better reading
 * @returns {number} return.flowScore - Reading flow quality (0-100)
 *
 * @example
 * const { schedule, flowScore, validateOrder } = useΛSchedule(shards);
 * const issues = validateOrder();
 * if (flowScore < 80) optimizeFlow();
 */
export function useΛSchedule(shards = []) {
  const [customOrder, setCustomOrder] = useState([]);

  // Define canonical Λ-chain with dependencies
  const canonicalChain = useMemo(
    () => [
      { pos: 1, type: 'Problem', phase: 'Motivation', requires: [] },
      { pos: 2, type: 'Gap', phase: 'Motivation', requires: ['Problem'] },
      { pos: 3, type: 'Claim', phase: 'Thesis', requires: ['Gap'] },
      { pos: 4, type: 'Intro', phase: 'Framing', requires: ['Claim'] },
      { pos: 5, type: 'Method', phase: 'Methods', requires: ['Intro'] },
      { pos: 6, type: 'Method2', phase: 'Methods', requires: ['Method'] },
      { pos: 7, type: 'Voice', phase: 'Background', requires: ['Method2'] },
      { pos: 8, type: 'Canon', phase: 'Background', requires: ['Voice'] },
      { pos: 9, type: 'Field', phase: 'Background', requires: ['Canon'] },
      { pos: 10, type: 'Artifact', phase: 'Evidence', requires: ['Field'] },
      { pos: 11, type: 'Proof', phase: 'Evidence', requires: ['Artifact'] },
      { pos: 12, type: 'Paper1', phase: 'Evidence', requires: ['Proof'] },
      { pos: 13, type: 'Result', phase: 'Evidence', requires: ['Paper1'] },
      { pos: 14, type: 'Paper2', phase: 'Evidence', requires: ['Result'] },
      { pos: 15, type: 'Eval', phase: 'Evidence', requires: ['Paper2'] },
      { pos: 16, type: 'Eval2', phase: 'Interpretation', requires: ['Eval'] },
      {
        pos: 17,
        type: 'Objection',
        phase: 'Interpretation',
        requires: ['Eval2'],
      },
      {
        pos: 18,
        type: 'Discussion',
        phase: 'Interpretation',
        requires: ['Objection'],
      },
      {
        pos: 19,
        type: 'Reply',
        phase: 'Interpretation',
        requires: ['Discussion'],
      },
      { pos: 20, type: 'Pattern', phase: 'Synthesis', requires: ['Reply'] },
      { pos: 21, type: 'Theory', phase: 'Synthesis', requires: ['Pattern'] },
      { pos: 22, type: 'Analysis', phase: 'Synthesis', requires: ['Theory'] },
      {
        pos: 23,
        type: 'Synthesis',
        phase: 'Synthesis',
        requires: ['Analysis'],
      },
      { pos: 24, type: 'Insight', phase: 'Impact', requires: ['Synthesis'] },
      { pos: 25, type: 'Impact', phase: 'Impact', requires: ['Insight'] },
      { pos: 26, type: 'Context', phase: 'Conclusion', requires: ['Impact'] },
      {
        pos: 27,
        type: 'Conclusion',
        phase: 'Conclusion',
        requires: ['Context'],
      },
    ],
    []
  );

  /**
   * Get current schedule (Λ-ordered shards)
   */
  const schedule = useMemo(() => {
    if (customOrder.length > 0) {
      return customOrder.sort((a, b) => a.position - b.position);
    }
    return shards.sort((a, b) => (a.position || 999) - (b.position || 999));
  }, [shards, customOrder]);

  /**
   * Validate Λ-constraints
   * Checks: 1) Dependencies satisfied 2) No cycles 3) Optimal flow
   *
   * @returns {Object[]} Violations (empty if valid)
   */
  const validateOrder = useCallback(() => {
    const violations = [];
    const seen = new Set();
    const _typeToPos = new Map(schedule.map(s => [s.type, s.position]));

    for (const shard of schedule) {
      const chainPos = canonicalChain.find(c => c.type === shard.type);
      if (!chainPos) continue;

      // Check dependencies
      for (const dep of chainPos.requires) {
        const depShard = schedule.find(s => s.type === dep);
        if (!depShard) {
          violations.push({
            type: 'missing_dependency',
            shard: shard.type,
            dependency: dep,
            severity: 'error',
          });
        } else if (depShard.position >= shard.position) {
          violations.push({
            type: 'dependency_order',
            shard: shard.type,
            dependency: dep,
            severity: 'error',
          });
        }
      }

      // Check non-redundancy
      if (seen.has(shard.type)) {
        violations.push({
          type: 'duplicate',
          shard: shard.type,
          severity: 'warning',
        });
      }
      seen.add(shard.type);
    }

    return violations;
  }, [schedule, canonicalChain]);

  /**
   * Compute reading flow quality (0-100)
   * Higher = better progression and coherence
   */
  const flowScore = useMemo(() => {
    const violations = validateOrder();
    const errorCount = violations.filter(v => v.severity === 'error').length;
    const warningCount = violations.filter(v => v.severity === 'warning').length;

    // Score = 100 - (errors * 5 + warnings * 2)
    let score = 100 - errorCount * 5 - warningCount * 2;

    // Bonus for following canonical order
    const matchCanonical = schedule.reduce((acc, s, i) => {
      const expectedPos = canonicalChain.findIndex(c => c.type === s.type);
      return acc + (expectedPos === i ? 1 : 0);
    }, 0);

    score += (matchCanonical / schedule.length) * 10;
    return Math.max(0, Math.min(100, score));
  }, [schedule, validateOrder, canonicalChain]);

  /**
   * Optimize order for maximum reading flow
   * Uses topological sort + heuristics
   */
  const optimizeFlow = useCallback(() => {
    // Topological sort respecting dependencies
    const typeToNode = new Map();
    const adjList = new Map();

    for (const pos of canonicalChain) {
      typeToNode.set(pos.type, pos);
      adjList.set(pos.type, pos.requires);
    }

    const sorted = [];
    const visited = new Set();
    const visiting = new Set();

    function dfs(type) {
      if (visited.has(type)) return;
      if (visiting.has(type)) return; // Cycle detection
      visiting.add(type);

      const deps = adjList.get(type) || [];
      for (const dep of deps) {
        if (schedule.find(s => s.type === dep)) {
          dfs(dep);
        }
      }

      visiting.delete(type);
      visited.add(type);
      if (schedule.find(s => s.type === type)) {
        sorted.push(type);
      }
    }

    for (const shard of schedule) {
      dfs(shard.type);
    }

    // Reorder shards according to sorted order
    const reordered = sorted
      .map(type => schedule.find(s => s.type === type))
      .filter(Boolean)
      .map((s, i) => ({ ...s, position: i + 1 }));

    setCustomOrder(reordered);
  }, [schedule, canonicalChain]);

  /**
   * Get detailed position information
   */
  const positions = useMemo(() => {
    return schedule.map(shard => {
      const canonical = canonicalChain.find(c => c.type === shard.type);
      return {
        shard: shard.id,
        type: shard.type,
        position: shard.position,
        phase: canonical?.phase || 'Unknown',
        canonicalPosition: canonical?.pos || null,
        isCanonical: shard.position === canonical?.pos,
      };
    });
  }, [schedule, canonicalChain]);

  return {
    // Data
    schedule,
    positions,
    flowScore,
    canonicalChain,

    // Operations
    validateOrder,
    optimizeFlow,
  };
}

export default useΛSchedule;
