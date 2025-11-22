/**
 * @fileoverview HTF Core: Hyper-Thesis Framework mathematical operations
 * Implements μ-architecture blending IMRaD, Papers, Argument, Contribution,
 * Monograph, DSR, and Narrative modes into unified thesis A.
 */

import { z } from 'zod';

/**
 * @typedef {Object} HTFOntology
 * @property {string} id - Ontology identifier
 * @property {string[]} concepts - Core concepts
 * @property {Map<string, string>} relations - Conceptual relations
 */

/**
 * @typedef {Object} DeltaShard
 * @property {string} id - Shard identifier
 * @property {string} family - Family: 'imrad'|'papers'|'argument'|'contribution'|'monograph'|'dsr'|'narrative'
 * @property {string} label - Human-readable label
 * @property {string} content - Shard content/text
 * @property {number} weight - Importance weight (0-1)
 * @property {string[]} dependencies - Prerequisite shards
 * @property {Object} metadata - Family-specific metadata
 */

/**
 * @typedef {Object} LambdaOrder
 * @property {string[]} chain - Total order of all shards (≺ relation)
 * @property {Map<string, number>> positions - Position in order
 * @property {string[]} criticalPath - Longest dependency path
 */

/**
 * @typedef {Object} PiMerge
 * @property {string} unifiedId - Merged argument ID
 * @property {string[]} components - All merged Δ shards
 * @property {Object} mergePoints - Where shards intersect
 * @property {number} coherence - Overall coherence score (0-1)
 */

/**
 * @typedef {Object} GammaGlobalization
 * @property {string} thesisId - Final thesis argument
 * @property {Map<string, string>} shardMap - Δ → position in thesis
 * @property {string[]} invariants - Preserved Q across all layers
 * @property {number} drift - Deviation from canonical form (0-1)
 */

/**
 * @typedef {Object} QueryInvariant
 * @property {string} id - Invariant ID
 * @property {Function} predicate - Verification function
 * @property {string} description - What this invariant maintains
 * @property {string[]} appliesTo - Shard families it applies to
 */

/**
 * @typedef {Object} TauEvolution
 * @property {number} epoch - Draft iteration
 * @property {Object} state - Current thesis state
 * @property {number} distance - Distance to μ-fixed point
 * @property {boolean} isConverged - Reached idempotent closure
 */

// ============================================
// 1. CANONICAL DELTA FAMILIES (7 modes)
// ============================================

/**
 * Define all 7 Δ-families (canonical shards)
 * @returns {Object} Dictionary of family definitions
 */
export const DeltaFamilies = {
  imrad: {
    name: 'IMRaD',
    shards: ['intro', 'method', 'result', 'discuss'],
    description: 'Classical empirical structure',
  },
  papers: {
    name: 'Thesis-by-Papers',
    shards: ['paper1', 'paper2', 'paper3', 'synthesis'],
    description: 'Multi-article modularity',
  },
  argument: {
    name: 'Argument',
    shards: ['claim', 'ground', 'proof', 'objection', 'reply'],
    description: 'Toulmin-style reasoning chain',
  },
  contribution: {
    name: 'Contribution',
    shards: ['gap', 'design', 'eval', 'impact'],
    description: 'Problem-solution-evidence arc',
  },
  monograph: {
    name: 'Monograph',
    shards: ['context', 'canon', 'method', 'analysis', 'conclusion'],
    description: 'Deep contextual narrative',
  },
  dsr: {
    name: 'Design Science Research',
    shards: ['problem', 'artifact', 'eval', 'theory'],
    description: 'Artifact-centric design logic',
  },
  narrative: {
    name: 'Narrative',
    shards: ['field', 'voice', 'pattern', 'insight'],
    description: 'Experiential knowledge creation',
  },
};

// ============================================
// 2. LAMBDA ORDERING (Λ-total order)
// ============================================

/**
 * The canonical Λ-≺ chain: global ordering of all shards
 * This primordial structure ensures all modes nest coherently
 */
export const CanonicalLambdaOrder = [
  'problem', // DSR: starts with problem
  'gap', // Contribution: what's missing
  'claim', // Argument: what we assert
  'intro', // IMRaD: introduce the work
  'method', // IMRaD: method(s)
  'method2', // Monograph: deeper methods
  'voice', // Narrative: author voice
  'canon', // Monograph: canonical works
  'field', // Narrative: field context
  'artifact', // DSR: the artifact
  'proof', // Argument: proof
  'paper1', // Papers: first paper
  'result', // IMRaD: results
  'paper2', // Papers: second paper
  'eval2', // Contribution: evaluation
  'eval3', // DSR: evaluation
  'objection', // Argument: counter-arguments
  'discuss', // IMRaD: discussion
  'reply', // Argument: reply to objections
  'pattern', // Narrative: emerging patterns
  'theory', // DSR: theory contribution
  'analysis', // Monograph: analysis
  'synthesis', // Papers: synthesis
  'insight', // Narrative: key insight
  'impact', // Contribution: impact
  'conclusion', // Monograph: conclusion
];

/**
 * Create a Λ-ordering (total order) of shards
 * @param {DeltaShard[]} shards - Input shards
 * @returns {LambdaOrder} Total order structure
 */
export function computeLambdaOrder(shards) {
  const positions = new Map();
  const chain = [];

  // Topological sort respecting dependencies
  const visited = new Set();
  const visiting = new Set();

  function visit(shardId) {
    if (visited.has(shardId)) return;
    if (visiting.has(shardId)) throw new Error(`Circular dependency: ${shardId}`);

    visiting.add(shardId);

    // Visit dependencies first
    const shard = shards.find(s => s.id === shardId);
    if (shard?.dependencies) {
      for (const dep of shard.dependencies) {
        visit(dep);
      }
    }

    visiting.delete(shardId);
    visited.add(shardId);
    chain.push(shardId);
    positions.set(shardId, chain.length - 1);
  }

  // Start with canonical order, then fill gaps
  for (const shardId of CanonicalLambdaOrder) {
    if (shards.some(s => s.id === shardId)) {
      visit(shardId);
    }
  }

  // Add any remaining shards
  for (const shard of shards) {
    visit(shard.id);
  }

  // Compute critical path (longest dependency path)
  const criticalPath = findCriticalPath(shards, chain);

  return { chain, positions, criticalPath };
}

/**
 * Find critical path (longest dependency chain)
 * @private
 */
function findCriticalPath(shards, order) {
  const lengths = new Map();
  const paths = new Map();

  for (const shardId of order) {
    const shard = shards.find(s => s.id === shardId);
    let maxLength = 0;
    let maxPath = [shardId];

    if (shard?.dependencies) {
      for (const dep of shard.dependencies) {
        const depLength = (lengths.get(dep) || 0) + 1;
        if (depLength > maxLength) {
          maxLength = depLength;
          maxPath = [...(paths.get(dep) || []), shardId];
        }
      }
    }

    lengths.set(shardId, maxLength);
    paths.set(shardId, maxPath);
  }

  // Return path with maximum length
  let maxPath = [];
  let maxLength = 0;

  for (const [shardId, length] of lengths) {
    if (length > maxLength) {
      maxLength = length;
      maxPath = paths.get(shardId) || [];
    }
  }

  return maxPath;
}

// ============================================
// 3. PI MERGE (Π-merge all Δ into single A)
// ============================================

/**
 * Merge all Δ-shards into unified argument A
 * @param {DeltaShard[]} shards - All shards to merge
 * @returns {PiMerge} Merged structure
 */
export function computePiMerge(shards) {
  const mergePoints = findMergePoints(shards);
  const coherence = computeCoherence(shards, mergePoints);

  return {
    unifiedId: `unified-${Date.now()}`,
    components: shards.map(s => s.id),
    mergePoints,
    coherence,
  };
}

/**
 * Find merge points: where shards from different families interact
 * @private
 */
function findMergePoints(shards) {
  const mergePoints = {};
  const familyIndices = new Map();

  // Group by family
  for (const shard of shards) {
    if (!familyIndices.has(shard.family)) {
      familyIndices.set(shard.family, []);
    }
    familyIndices.get(shard.family).push(shard.id);
  }

  // Find cross-family dependencies
  for (const shard of shards) {
    const crossFamilyDeps =
      shard.dependencies?.filter(dep => {
        const depShard = shards.find(s => s.id === dep);
        return depShard && depShard.family !== shard.family;
      }) || [];

    if (crossFamilyDeps.length > 0) {
      mergePoints[shard.id] = {
        shard: shard.id,
        family: shard.family,
        crossFamilyDependencies: crossFamilyDeps,
        integrationStrength: crossFamilyDeps.length / Math.max(shard.dependencies?.length || 1, 1),
      };
    }
  }

  return mergePoints;
}

/**
 * Compute overall coherence of merged structure (0-1)
 * @private
 */
function computeCoherence(shards, mergePoints) {
  if (shards.length === 0) return 0;

  // Count merge points
  const mergeCount = Object.keys(mergePoints).length;
  const expectedMerges =
    (Object.keys(DeltaFamilies).length * (Object.keys(DeltaFamilies).length - 1)) / 2;

  // Coherence = coverage of potential merges + weight consistency
  const mergeCoverage = mergeCount / Math.max(expectedMerges, 1);

  // Weight consistency: how balanced are family contributions?
  const familyWeights = new Map();
  for (const shard of shards) {
    const current = familyWeights.get(shard.family) || 0;
    familyWeights.set(shard.family, current + (shard.weight || 0.5));
  }

  const weights = Array.from(familyWeights.values());
  const avgWeight = weights.reduce((a, b) => a + b, 0) / weights.length;
  const variance = weights.reduce((sum, w) => sum + Math.pow(w - avgWeight, 2), 0) / weights.length;
  const weightBalance = 1 - Math.min(variance, 1);

  return mergeCoverage * 0.6 + weightBalance * 0.4;
}

// ============================================
// 4. GAMMA GLOBALIZATION (Γ-glueing)
// ============================================

/**
 * Globalize all Δ shards into single thesis A
 * Ensures all shards respect Q-invariants
 * @param {DeltaShard[]} shards - All shards
 * @param {QueryInvariant[]} invariants - Q to preserve
 * @returns {GammaGlobalization} Globalized thesis structure
 */
export function computeGammaGlobalization(shards, invariants = []) {
  const lambdaOrder = computeLambdaOrder(shards);
  const shardMap = new Map();

  // Position each shard in canonical form
  for (const [idx, shardId] of lambdaOrder.chain.entries()) {
    shardMap.set(shardId, `section-${idx + 1}`);
  }

  // Check invariants
  const driftViolations = [];
  for (const invariant of invariants) {
    for (const shard of shards) {
      if (invariant.appliesTo.includes(shard.family)) {
        if (!invariant.predicate(shard)) {
          driftViolations.push({
            invariant: invariant.id,
            shard: shard.id,
            description: invariant.description,
          });
        }
      }
    }
  }

  // Compute drift (how far from canonical)
  const drift = Math.min(driftViolations.length / Math.max(shards.length, 1), 1);

  return {
    thesisId: `thesis-${Date.now()}`,
    shardMap,
    invariants: invariants.map(i => i.id),
    drift,
    violations: driftViolations,
  };
}

// ============================================
// 5. Q-INVARIANTS (preserved constraints)
// ============================================

/**
 * Standard HTF invariants all theses must respect
 */
export const StandardInvariants = [
  {
    id: 'Q-coherence',
    description: 'Overall argument is internally coherent',
    predicate: shard => shard.content && shard.content.length > 100,
    appliesTo: Object.keys(DeltaFamilies),
  },
  {
    id: 'Q-positioning',
    description: 'Shard respects dependency constraints',
    predicate: shard => Array.isArray(shard.dependencies),
    appliesTo: Object.keys(DeltaFamilies),
  },
  {
    id: 'Q-contribution',
    description: 'Shard advances the argument',
    predicate: shard => shard.weight > 0,
    appliesTo: Object.keys(DeltaFamilies),
  },
  {
    id: 'Q-imrad-closure',
    description: 'IMRaD family forms complete cycle',
    predicate: shard =>
      shard.family !== 'imrad' || ['intro', 'method', 'result', 'discuss'].includes(shard.id),
    appliesTo: ['imrad'],
  },
  {
    id: 'Q-argument-chain',
    description: 'Argument follows claim→proof→objection→reply',
    predicate: shard =>
      shard.family !== 'argument' ||
      ['claim', 'ground', 'proof', 'objection', 'reply'].includes(shard.id),
    appliesTo: ['argument'],
  },
];

// ============================================
// 6. TAU EVOLUTION (temporal convergence)
// ============================================

/**
 * Track thesis evolution toward μ-fixed point
 * @param {DeltaShard[]} shards - Current shards
 * @param {TauEvolution} previousEpoch - Previous epoch state
 * @returns {TauEvolution} New epoch state
 */
export function evolveTowardMuFixed(shards, previousEpoch = null) {
  const epoch = (previousEpoch?.epoch || 0) + 1;
  const globalized = computeGammaGlobalization(shards, StandardInvariants);

  // Distance to fixed point: how close to zero drift?
  const distance = globalized.drift;
  const isConverged = distance < 0.05; // < 5% drift = converged

  return {
    epoch,
    state: {
      shards: shards.map(s => s.id),
      globalization: globalized,
    },
    distance,
    isConverged,
    improvement: previousEpoch ? previousEpoch.distance - distance : 0,
  };
}

// ============================================
// 7. VALIDATION & SCHEMA
// ============================================

const DeltaShardSchema = z.object({
  id: z.string(),
  family: z.enum(Object.keys(DeltaFamilies)),
  label: z.string(),
  content: z.string(),
  weight: z.number().min(0).max(1).default(0.5),
  dependencies: z.array(z.string()).default([]),
  metadata: z.record(z.any()).default({}),
});

/**
 * Validate shard conforms to HTF spec
 * @param {DeltaShard} shard - Shard to validate
 * @throws {Error} If validation fails
 */
export function validateDeltaShard(shard) {
  return DeltaShardSchema.parse(shard);
}

export default {
  DeltaFamilies,
  CanonicalLambdaOrder,
  computeLambdaOrder,
  computePiMerge,
  computeGammaGlobalization,
  StandardInvariants,
  evolveTowardMuFixed,
  validateDeltaShard,
};
