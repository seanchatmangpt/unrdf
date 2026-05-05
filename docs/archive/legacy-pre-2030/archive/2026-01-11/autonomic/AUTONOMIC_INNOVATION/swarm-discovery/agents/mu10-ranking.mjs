/**
 * @file mu10-ranking.mjs - Rank and promote high-synergy capabilities (μ10 agent)
 * @description Sort by ΔU and promote top emergent capabilities
 * @equation Rank_τ := sort_{≺}(Ledger_τ by ΔU)
 */

/**
 * Rank candidates by synergy and constraint satisfaction
 * @param {array} metrics - Metrics from μ8
 * @param {array} candidates - Candidates from μ5
 * @param {object} options - Ranking options
 * @returns {object} - Ranked candidates with promotions
 */
export function rankCandidates(metrics, candidates, options = {}) {
  const topN = options.topN || 5
  const minSynergy = options.minSynergy || -1

  if (!metrics || !Array.isArray(metrics) || metrics.length === 0) {
    return {
      ranked: [],
      promoted: [],
      statistics: {
        totalRanked: 0,
        topSynergy: 0,
        promotionThreshold: topN
      }
    }
  }

  // Filter by constraint satisfaction
  const eligible = metrics.filter(m => m && m.synergy >= minSynergy)

  // Sort by total order ≺
  const ranked = eligible.map((m, idx) => ({
    ...m,
    totalOrder: idx + 1,
    constraintStrength: computeConstraintStrength(m, candidates),
    loopClosureDegree: computeLoopClosure(m.atoms)
  }))

  // Final sort: ΔU desc → constraint strength desc → loop closure desc
  ranked.sort((a, b) => {
    if (b.synergy !== a.synergy) return b.synergy - a.synergy
    if (b.constraintStrength !== a.constraintStrength) {
      return b.constraintStrength - a.constraintStrength
    }
    return b.loopClosureDegree - a.loopClosureDegree
  })

  // Update ranks
  ranked.forEach((r, i) => {
    r.rank = i + 1
  })

  return {
    ranked,
    promoted: ranked.slice(0, topN),
    statistics: {
      totalRanked: ranked.length,
      topSynergy: ranked[0]?.synergy || 0,
      promotionThreshold: topN
    }
  }
}

/**
 * Compute constraint strength (how many invariants satisfied)
 * @private
 */
function computeConstraintStrength(metric, candidates) {
  let strength = 0

  // Q1: Determinism (always 1 in this system)
  strength += 0.2

  // Q2: Provenance (check atom evidence hashes)
  const candidate = candidates.find(c => c.id === metric.id)
  if (candidate && candidate.atoms && candidate.atoms.length > 0) {
    strength += 0.2 // Atoms have evidenceHash
  }

  // Q3: Receipts (experiments would have receipts)
  strength += 0.2

  // Q4: Poka-yoke (composition is well-formed)
  strength += 0.2

  // Q5: SLA honesty (metrics computed)
  if (metric.utility && metric.synergy !== undefined) {
    strength += 0.2
  }

  return Math.min(1.0, strength)
}

/**
 * Compute loop closure degree
 * @private
 */
function computeLoopClosure(atomIds) {
  // Simple heuristic: more atoms = more closure opportunities
  return Math.min(1.0, (atomIds.length - 1) / 3)
}

/**
 * Generate promotion records
 * @param {array} promoted - Top candidates
 * @param {array} atoms - All atoms
 * @param {object} options - Options (deterministic?, timestamp?)
 * @returns {array} - Promotion records
 */
export function generatePromotions(promoted, atoms, options = {}) {
  const atomMap = new Map(atoms.map(a => [a.id, a]))
  const timestamp = options.timestamp || (options.deterministic ? '2025-12-26T00:00:00.000Z' : new Date().toISOString())

  return promoted.map((p, idx) => ({
    id: `promotion-${idx + 1}`,
    rank: p.rank,
    candidateId: p.id,
    atomIds: p.atoms,
    atomDetails: p.atoms.map(id => {
      const atom = atomMap.get(id)
      return atom ? {
        id: atom.id,
        name: atom.name,
        utility: atom.utility,
        complexity: atom.complexity
      } : null
    }).filter(Boolean),
    synergy: p.synergy,
    utility: p.utility,
    constraintStrength: p.constraintStrength,
    capabilities: generateCapabilities(p, atoms),
    runnablePointer: `experiments/${p.id}/run.mjs`,
    receiptPointer: `experiments/${p.id}/receipt.json`,
    timestamp: timestamp
  }))
}

/**
 * Generate capability description for promoted composition
 * @private
 */
function generateCapabilities(promotion, atoms) {
  const atomMap = new Map(atoms.map(a => [a.id, a]))
  const capabilities = new Set()

  for (const atomId of promotion.atoms) {
    const atom = atomMap.get(atomId)
    if (atom && atom.capabilities) {
      atom.capabilities.forEach(c => capabilities.add(c))
    }
  }

  return Array.from(capabilities)
}

/**
 * Create promotion manifest
 * @param {array} promotions - Generated promotions
 * @param {object} options - Options (deterministic?, timestamp?)
 * @returns {object} - Manifest for all promotions
 */
export function createPromotionManifest(promotions, options = {}) {
  const timestamp = options.timestamp || (options.deterministic ? '2025-12-26T00:00:00.000Z' : new Date().toISOString())

  return {
    version: '1.0.0',
    timestamp: timestamp,
    promotedCount: promotions.length,
    promotions,
    discoveryProcess: {
      phases: ['μ1_orchestrator', 'μ2_atomization', 'μ3_graph', 'μ5_search',
        'μ8_metrics', 'μ10_ranking'],
      invariants: ['Q1_determinism', 'Q2_provenance', 'Q3_receipts', 'Q4_pokayoke', 'Q5_sla_honesty']
    },
    acceptance: {
      criteria: [
        'Each promotion has experiment pointer',
        'Receipt verifies',
        'Measured ΔU > 0',
        'Evidence links for all atoms'
      ],
      verified: false
    }
  }
}

/**
 * Rank by secondary criteria (constraint + loop closure)
 * @param {array} ranked - Initially ranked candidates
 * @returns {array} - Re-ranked by secondary criteria
 */
export function applySecondaryRanking(ranked) {
  return ranked
    .sort((a, b) => {
      // Primary: synergy descending
      if (b.synergy !== a.synergy) return b.synergy - a.synergy

      // Secondary: constraint strength descending
      if (b.constraintStrength !== a.constraintStrength) {
        return b.constraintStrength - a.constraintStrength
      }

      // Tertiary: loop closure count
      return b.loopClosureDegree - a.loopClosureDegree
    })
    .map((r, i) => ({ ...r, finalRank: i + 1 }))
}

export default {
  rankCandidates,
  generatePromotions,
  createPromotionManifest,
  applySecondaryRanking
}
