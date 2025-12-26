/**
 * Agent 6 - Diff-as-Program (Impact + Conflict Detection)
 *
 * Complete system for analyzing, detecting conflicts, and resolving
 * delta operations in capsule-based systems.
 *
 * @module agent-6
 */

// Impact Set Computation
export {
  computeImpactSet,
  hasOverlap,
  mergeImpactSets,
  serializeImpactSet,
  deserializeImpactSet
} from './impact-set.mjs';

// Conflict Detection
export {
  detectConflict,
  canReorder,
  detectBatchConflicts,
  isIndependent,
  findConflictingDeltas,
  partitionConflictFree,
  computeDependencyGraph,
  isSerializable
} from './conflict-detector.mjs';

// Conflict Certificates
export {
  generateCertificate,
  verifyCertificate,
  generateBatchCertificates,
  verifyBatchCertificates,
  serializeCertificate,
  deserializeCertificate,
  createCertificateChain,
  verifyCertificateChain
} from './conflict-certificate.mjs';

// Commutativity Analysis
export {
  checkCommutativity,
  generateWitness,
  checkBatchCommutativity,
  findCommutativeSubsets,
  verifyWitness,
  generateCommutativityChain,
  analyzeParallelizability
} from './commutativity.mjs';

// Merge Conflict Resolution
export {
  resolveConflict,
  resolveBatchConflicts,
  mergeSequence,
  threeWayMerge,
  suggestStrategy,
  validateResolution
} from './merge-resolver.mjs';

/**
 * Full diff-as-program analysis pipeline
 *
 * Combines all analysis steps:
 * 1. Impact analysis
 * 2. Conflict detection
 * 3. Commutativity checking
 * 4. Certificate generation
 *
 * @param {Object} deltaA - First delta
 * @param {Object} deltaB - Second delta
 * @returns {Object} Complete analysis report
 */
export function analyzeDeltaPair(deltaA, deltaB) {
  // Import local functions
  const { computeImpactSet, serializeImpactSet } = await import('./impact-set.mjs');
  const { detectConflict } = await import('./conflict-detector.mjs');
  const { checkCommutativity } = await import('./commutativity.mjs');
  const { generateCertificate } = await import('./conflict-certificate.mjs');

  // Compute impact sets
  const impactA = computeImpactSet(deltaA);
  const impactB = computeImpactSet(deltaB);

  // Detect conflicts
  const conflict = detectConflict(deltaA, deltaB);

  // Check commutativity
  const commutativity = checkCommutativity(deltaA, deltaB);

  // Generate certificate
  const certificate = generateCertificate(deltaA, deltaB, conflict);

  return {
    impacts: {
      deltaA: serializeImpactSet(impactA),
      deltaB: serializeImpactSet(impactB)
    },
    conflict,
    commutativity,
    certificate,
    summary: {
      hasConflict: conflict.hasConflict,
      conflictType: conflict.type,
      commutes: commutativity.commutes,
      canParallelize: !conflict.hasConflict || commutativity.commutes
    }
  };
}

/**
 * Batch analysis for multiple delta pairs
 *
 * @param {Array<[Object, Object]>} deltaPairs - Array of delta pairs
 * @returns {Object} Batch analysis report
 */
export function analyzeBatch(deltaPairs) {
  if (!Array.isArray(deltaPairs)) {
    throw new Error('deltaPairs must be an array');
  }

  const results = deltaPairs.map(([deltaA, deltaB], index) => {
    try {
      return {
        index,
        success: true,
        analysis: analyzeDeltaPair(deltaA, deltaB)
      };
    } catch (error) {
      return {
        index,
        success: false,
        error: error.message
      };
    }
  });

  const successful = results.filter(r => r.success);
  const failed = results.filter(r => !r.success);

  const summary = {
    totalPairs: deltaPairs.length,
    successCount: successful.length,
    failureCount: failed.length,
    conflictCount: successful.filter(r => r.analysis.conflict.hasConflict).length,
    commutativeCount: successful.filter(r => r.analysis.commutativity.commutes).length,
    parallelizableCount: successful.filter(r => r.analysis.summary.canParallelize).length
  };

  return {
    summary,
    results
  };
}

/**
 * Recommend optimal execution strategy for delta sequence
 *
 * @param {Object[]} deltas - Sequence of deltas
 * @returns {Object} Execution strategy recommendation
 */
export async function recommendStrategy(deltas) {
  if (!Array.isArray(deltas) || deltas.length === 0) {
    throw new Error('deltas must be a non-empty array');
  }

  const { detectBatchConflicts, partitionConflictFree } = await import('./conflict-detector.mjs');
  const { analyzeParallelizability } = await import('./commutativity.mjs');

  // Analyze conflicts
  const batchConflicts = detectBatchConflicts(deltas);

  // Analyze parallelizability
  const parallelAnalysis = analyzeParallelizability(deltas);

  // Partition into conflict-free groups
  const groups = partitionConflictFree(deltas);

  // Generate recommendation
  let strategy = 'SEQUENTIAL';
  let reasoning = '';

  if (parallelAnalysis.fullyParallelizable) {
    strategy = 'FULLY_PARALLEL';
    reasoning = 'All deltas commute - can execute in any order or fully parallel';
  } else if (groups.length < deltas.length) {
    strategy = 'PARTIAL_PARALLEL';
    reasoning = `Can partition into ${groups.length} independent groups for partial parallelization`;
  } else {
    strategy = 'SEQUENTIAL';
    reasoning = 'All deltas have dependencies - must execute sequentially';
  }

  return {
    strategy,
    reasoning,
    metrics: {
      totalDeltas: deltas.length,
      conflictCount: batchConflicts.conflictCount,
      parallelizableRatio: parallelAnalysis.parallelizableRatio,
      independentGroups: groups.length
    },
    groups,
    recommendations: parallelAnalysis.recommendations
  };
}

/**
 * Generate comprehensive delta system report
 *
 * @param {Object[]} deltas - Deltas to analyze
 * @returns {Object} Complete system report
 */
export async function generateSystemReport(deltas) {
  if (!Array.isArray(deltas)) {
    throw new Error('deltas must be an array');
  }

  const { detectBatchConflicts } = await import('./conflict-detector.mjs');
  const { checkBatchCommutativity, generateCommutativityChain } = await import('./commutativity.mjs');
  const { createCertificateChain } = await import('./conflict-certificate.mjs');

  // Batch conflict detection
  const conflicts = detectBatchConflicts(deltas);

  // Batch commutativity analysis
  const commutativity = checkBatchCommutativity(deltas);

  // Generate certificate chain
  const certificateChain = createCertificateChain(deltas);

  // Generate commutativity chain
  const commutativityChain = generateCommutativityChain(deltas);

  // Execution strategy
  const strategy = await recommendStrategy(deltas);

  return {
    version: '1.0.0',
    timestamp: new Date().toISOString(),
    deltaCount: deltas.length,
    conflicts: {
      totalPairs: conflicts.totalPairs,
      conflictCount: conflicts.conflictCount,
      hasAnyConflict: conflicts.hasAnyConflict,
      details: conflicts.conflicts
    },
    commutativity: {
      totalPairs: commutativity.totalPairs,
      commutativeCount: commutativity.commutativeCount,
      allCommute: commutativity.allCommute,
      matrix: commutativity.commutativityMatrix
    },
    certificates: {
      certificateCount: certificateChain.certificateCount,
      chainHash: certificateChain.chainHash
    },
    commutativityProofs: {
      witnessCount: commutativityChain.witnessCount,
      summary: commutativityChain.summary
    },
    executionStrategy: strategy,
    summary: {
      hasConflicts: conflicts.hasAnyConflict,
      fullyCommutative: commutativity.allCommute,
      recommendedStrategy: strategy.strategy,
      parallelizationPotential: strategy.metrics.parallelizableRatio
    }
  };
}

// Export version
export const VERSION = '1.0.0';
