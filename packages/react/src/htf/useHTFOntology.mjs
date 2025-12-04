/**
 * @file useHTFOntology.mjs
 * @description React hook for managing HTF ontology and Δ-shards
 * Loads the formal Hyper-Thesis Framework ontology and provides CRUD operations
 */

import { useState, useCallback, useMemo } from 'react';

/**
 * @typedef {Object} Δ_Shard
 * @property {string} id - Unique shard identifier
 * @property {string} type - Shard class (Intro, Method, Result, etc.)
 * @property {string} family - Family: IMRaD|Papers|Argument|Contribution|Monograph|DSR|Narrative
 * @property {number} position - Position in Λ-schedule (1-27)
 * @property {string} content - Section content
 * @property {number} wordCount - Word count
 * @property {Date} lastModified - Last edit timestamp
 */

/**
 * useHTFOntology hook
 *
 * Manages the complete HTF ontology structure with 27 canonical shards
 * organized into 7 families.
 *
 * @returns {Object} HTF ontology operations
 * @returns {Δ_Shard[]} return.shards - All shards in current thesis
 * @returns {string[]} return.families - Available families
 * @returns {Function} return.addShard - Create new shard
 * @returns {Function} return.removeShard - Delete shard
 * @returns {Function} return.updateShard - Edit shard content
 * @returns {Function} return.queryShards - Find shards by criteria
 * @returns {Function} return.moveToPosition - Change Λ-position
 *
 * @example
 * const { shards, addShard, queryShards } = useHTFOntology();
 * const claims = queryShards({ type: 'Claim', family: 'Argument' });
 * addShard({ type: 'Intro', family: 'IMRaD', content: '...' });
 */
export function useHTFOntology() {
  const [shards, setShards] = useState([]);
  const [nextId, setNextId] = useState(1);

  // Define the 7 families and their canonical shards
  const families = useMemo(
    () => ({
      IMRaD: [
        { type: 'Intro', description: 'Context + Research Question + Thesis' },
        { type: 'Method', description: 'Design + Protocol + Implementation' },
        { type: 'Result', description: 'Data + Analysis + Tables/Figures' },
        {
          type: 'Discussion',
          description: 'Interpretation + Comparison + Limits',
        },
      ],
      Papers: [
        { type: 'Paper1', description: 'Core contribution A' },
        { type: 'Paper2', description: 'Core contribution B' },
        { type: 'Paper3', description: 'Core contribution C' },
        {
          type: 'Synthesis',
          description: 'Cross-paper synthesis + unified theory',
        },
      ],
      Argument: [
        { type: 'Claim', description: 'Main thesis statement' },
        { type: 'Ground', description: 'Empirical / theoretical grounds' },
        { type: 'Proof', description: 'Formal proof or construction' },
        { type: 'Objection', description: 'Counterargument or limitation' },
        { type: 'Reply', description: 'Response to objection' },
      ],
      Contribution: [
        { type: 'Gap', description: 'Identified gap in literature' },
        { type: 'Design', description: 'Artifact design + rationale' },
        { type: 'Eval', description: 'Evaluation of artifact' },
        { type: 'Impact', description: 'Practical/theoretical impact' },
      ],
      Monograph: [
        { type: 'Context', description: 'Historical + disciplinary context' },
        { type: 'Canon', description: 'Core canonical works + synthesis' },
        { type: 'Method2', description: 'Epistemological grounding' },
        { type: 'Analysis', description: 'Detailed scholarly analysis' },
        { type: 'Conclusion', description: 'Integration + future directions' },
      ],
      DSR: [
        { type: 'Problem', description: 'Problem formulation + motivation' },
        {
          type: 'Artifact',
          description: 'Artifact construction + instantiation',
        },
        { type: 'Eval2', description: 'Rigorous evaluation + metrics' },
        { type: 'Theory', description: 'Theory building from artifact' },
      ],
      Narrative: [
        { type: 'Field', description: 'Field observation + immersion' },
        { type: 'Voice', description: 'Authorial stance + perspective' },
        { type: 'Pattern', description: 'Emergent patterns + themes' },
        { type: 'Insight', description: 'Deep insight or understanding' },
      ],
    }),
    []
  );

  const familyNames = useMemo(() => Object.keys(families), [families]);

  /**
   * Create a new Δ-shard
   * @param {Object} shardData - Shard properties
   * @returns {Δ_Shard} Created shard
   */
  const addShard = useCallback(
    (shardData) => {
      const newShard = {
        id: `shard-${nextId}`,
        position: shards.length + 1,
        wordCount: 0,
        lastModified: new Date(),
        ...shardData,
      };
      setShards((prev) => [...prev, newShard]);
      setNextId((prev) => prev + 1);
      return newShard;
    },
    [shards.length, nextId]
  );

  /**
   * Delete a shard by ID
   * @param {string} shardId - ID to remove
   */
  const removeShard = useCallback((shardId) => {
    setShards((prev) => prev.filter((s) => s.id !== shardId));
  }, []);

  /**
   * Update shard content and metadata
   * @param {string} shardId - ID to update
   * @param {Object} updates - Properties to change
   */
  const updateShard = useCallback((shardId, updates) => {
    setShards((prev) =>
      prev.map((s) =>
        s.id === shardId
          ? {
              ...s,
              ...updates,
              lastModified: new Date(),
              wordCount: updates.content ? updates.content.split(/\s+/).length : s.wordCount,
            }
          : s
      )
    );
  }, []);

  /**
   * Move shard to new Λ-position
   * @param {string} shardId - ID to move
   * @param {number} newPosition - Target position (1-27)
   */
  const moveToPosition = useCallback((shardId, newPosition) => {
    setShards((prev) => {
      const newPosition2 = Math.max(1, Math.min(newPosition, 27));
      const shard = prev.find((s) => s.id === shardId);
      if (!shard) return prev;

      const oldPos = shard.position;
      return prev
        .filter((s) => s.id !== shardId)
        .map((s) => {
          if (oldPos < newPosition2) {
            if (s.position > oldPos && s.position <= newPosition2) {
              return { ...s, position: s.position - 1 };
            }
          } else if (oldPos > newPosition2) {
            if (s.position >= newPosition2 && s.position < oldPos) {
              return { ...s, position: s.position + 1 };
            }
          }
          return s;
        })
        .concat({ ...shard, position: newPosition2 })
        .sort((a, b) => a.position - b.position);
    });
  }, []);

  /**
   * Query shards by multiple criteria
   * @param {Object} criteria - Filter conditions { type?, family?, position? }
   * @returns {Δ_Shard[]} Matching shards
   */
  const queryShards = useCallback(
    (criteria = {}) => {
      return shards.filter((shard) => {
        if (criteria.type && shard.type !== criteria.type) return false;
        if (criteria.family && shard.family !== criteria.family) return false;
        if (criteria.position && shard.position !== criteria.position) return false;
        return true;
      });
    },
    [shards]
  );

  /**
   * Get shards in Λ-order (reading sequence)
   * @returns {Δ_Shard[]} Shards sorted by position
   */
  const getOrderedShards = useCallback(() => {
    return [...shards].sort((a, b) => a.position - b.position);
  }, [shards]);

  return {
    // Data
    shards,
    families,
    familyNames,

    // Operations
    addShard,
    removeShard,
    updateShard,
    moveToPosition,
    queryShards,
    getOrderedShards,
  };
}

export default useHTFOntology;
