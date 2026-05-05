/**
 * @file mu8-metrics.mjs - Compute utility and synergy metrics (μ8 agent)
 * @description U(α) baseline + U(C_k) composition + ΔU synergy
 * @equation ΔU(C_k) := U(C_k) − Σ_i U(α_i)
 */

/**
 * Compute baseline utility for atom
 * @param {object} atom - Atom
 * @returns {number} - Utility score (0.0-1.0)
 */
export function computeAtomUtility(atom) {
  // Base utility from atom definition
  let utility = atom.utility || 0.5

  // Feature count bonus
  const featureCount = (atom.capabilities || []).length
  const featureBonus = Math.min(0.1, featureCount * 0.02)

  // Constraint support bonus
  const hasProof = atom.proof && atom.proof.files && atom.proof.files.length > 0
  const proofBonus = hasProof ? 0.05 : 0

  // Complexity penalty
  const complexityPenalty = computeComplexityPenalty(atom.complexity)

  return Math.max(0, Math.min(1.0, utility + featureBonus + proofBonus - complexityPenalty))
}

/**
 * Compute composition utility
 * @param {array} atomIds - IDs of atoms in composition
 * @param {array} atoms - All atoms
 * @returns {number} - Composition utility
 */
export function computeCompositionUtility(atomIds, atoms) {
  const atomMap = new Map(atoms.map(a => [a.id, a]))

  // Sum of individual utilities
  let sumUtility = 0
  let count = 0

  for (const id of atomIds) {
    const atom = atomMap.get(id)
    if (atom) {
      sumUtility += computeAtomUtility(atom)
      count++
    }
  }

  const averageUtility = count > 0 ? sumUtility / count : 0

  // Interaction bonus: utility from composition exceeds sum
  const interactionBonus = computeInteractionBonus(atomIds, atoms)

  return averageUtility + interactionBonus
}

/**
 * Compute synergy (ΔU)
 * @param {array} atomIds - Composition atoms
 * @param {array} atoms - All atoms
 * @returns {number} - ΔU synergy value
 */
export function computeSynergy(atomIds, atoms) {
  const atomMap = new Map(atoms.map(a => [a.id, a]))

  // Sum of individual utilities
  let sumIndividual = 0
  for (const id of atomIds) {
    const atom = atomMap.get(id)
    if (atom) {
      sumIndividual += computeAtomUtility(atom)
    }
  }

  // Composition utility
  const compositionUtility = computeCompositionUtility(atomIds, atoms)

  // ΔU = U(C) - Σ U(α)
  return compositionUtility - sumIndividual
}

/**
 * Compute interaction bonus
 * @private
 */
function computeInteractionBonus(atomIds, atoms) {
  const atomMap = new Map(atoms.map(a => [a.id, a]))

  // Count compatible interfaces
  let compatibilityScore = 0

  for (let i = 0; i < atomIds.length; i++) {
    for (let j = i + 1; j < atomIds.length; j++) {
      const a1 = atomMap.get(atomIds[i])
      const a2 = atomMap.get(atomIds[j])

      if (a1 && a2 && canInteract(a1, a2)) {
        compatibilityScore += 0.1
      }
    }
  }

  // Bonus scales with composition size (diminishing returns)
  const sizeBonus = Math.log(1 + atomIds.length) * 0.05

  return Math.min(0.3, compatibilityScore + sizeBonus)
}

/**
 * Check if two atoms can interact
 * @private
 */
function canInteract(atom1, atom2) {
  // RDF Store + any query processor
  if ((atom1.id === 'atom-rdf-store' && isQueryProcessor(atom2)) ||
      (atom2.id === 'atom-rdf-store' && isQueryProcessor(atom1))) {
    return true
  }

  // Hook System + any data processor
  if ((atom1.id === 'atom-hook-system' && isDataProcessor(atom2)) ||
      (atom2.id === 'atom-hook-system' && isDataProcessor(atom1))) {
    return true
  }

  // Event Logger + Workflow
  if ((atom1.id === 'atom-event-logger' && atom2.id === 'atom-workflow-engine') ||
      (atom2.id === 'atom-event-logger' && atom1.id === 'atom-workflow-engine')) {
    return true
  }

  return false
}

/**
 * Check if atom is query processor
 * @private
 */
function isQueryProcessor(atom) {
  return ['atom-sparql-executor', 'atom-pattern-matcher', 'atom-graph-analytics'].includes(atom.id)
}

/**
 * Check if atom is data processor
 * @private
 */
function isDataProcessor(atom) {
  return ['atom-rdf-store', 'atom-sparql-executor', 'atom-pattern-matcher',
    'atom-change-feed', 'atom-workflow-engine'].includes(atom.id)
}

/**
 * Compute complexity penalty
 * @private
 */
function computeComplexityPenalty(complexity) {
  const penalties = {
    'low': 0,
    'moderate': 0.05,
    'high': 0.1,
    'very-high': 0.15
  }
  return penalties[complexity] || 0.05
}

/**
 * Compute baseline metrics for atoms
 * @param {array} atoms - All atoms
 * @returns {object} - Baseline metrics
 */
export function computeBaselines(atoms) {
  const utilities = atoms.map(a => computeAtomUtility(a))

  return {
    atoms: atoms.map((a, i) => ({
      id: a.id,
      name: a.name,
      baselineUtility: utilities[i],
      complexity: a.complexity,
      proof: a.proof
    })),
    statistics: {
      averageUtility: utilities.reduce((a, b) => a + b, 0) / utilities.length,
      maxUtility: Math.max(...utilities),
      minUtility: Math.min(...utilities),
      medianUtility: median(utilities)
    }
  }
}

/**
 * Compute metrics for all candidates
 * @param {array} candidates - Candidates from μ5
 * @param {array} atoms - All atoms
 * @returns {object} - Metrics for candidates
 */
export function computeMetrics(candidates, atoms) {
  if (!candidates || !Array.isArray(candidates) || candidates.length === 0) {
    return []
  }

  const metrics = candidates.map(candidate => {
    const synergy = computeSynergy(candidate.atoms || [], atoms)
    const utility = computeCompositionUtility(candidate.atoms || [], atoms)

    return {
      id: candidate.id,
      atoms: candidate.atoms || [],
      utility,
      synergy,
      synergy_delta: synergy,
      score: (candidate.score || 0) + synergy * 0.2,
      rank: 0
    }
  })

  // Rank by synergy
  metrics.sort((a, b) => b.synergy - a.synergy)
  metrics.forEach((m, i) => { m.rank = i + 1 })

  return metrics
}

/**
 * Helper: compute median
 * @private
 */
function median(arr) {
  const sorted = [...arr].sort((a, b) => a - b)
  const mid = Math.floor(sorted.length / 2)
  return sorted.length % 2 !== 0 ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2
}

export default {
  computeAtomUtility,
  computeCompositionUtility,
  computeSynergy,
  computeBaselines,
  computeMetrics
}
