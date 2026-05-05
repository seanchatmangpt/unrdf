/**
 * @file mu5-search.mjs - Beam search for high-synergy compositions (μ5 agent)
 * @description Find C* = argmax ΔU(C_k) subject to Adm(C_k)=true
 * @equation Candidates_τ := Search(G_τ, constraints=Adm, beamWidth=N, depth=D)
 */

/**
 * Beam search for high-synergy compositions
 * @param {object} graph - Dependency graph from μ3
 * @param {array} atoms - Atoms from μ2
 * @param {object} options - Search options
 * @returns {object} - Candidates with synergy predictions
 */
export async function searchCompositions(graph, atoms, options = {}) {
  const beamWidth = options.beamWidth || 10
  const maxDepth = options.maxDepth || 5

  // Seeds: high-leverage atoms
  const seeds = selectSeeds(atoms, beamWidth)

  // Expand seeds via edges
  const candidates = []
  const queue = seeds.map(seed => ({
    atoms: [seed.id],
    depth: 0,
    heuristic: seed.utility
  }))

  const visited = new Set()

  while (queue.length > 0) {
    // Sort by heuristic (best first)
    queue.sort((a, b) => b.heuristic - a.heuristic)

    // Keep beam width
    const beam = queue.splice(0, beamWidth)

    for (const candidate of beam) {
      const key = candidate.atoms.sort().join(',')
      if (visited.has(key)) continue
      visited.add(key)

      candidates.push({
        id: `candidate-${candidates.length + 1}`,
        atoms: candidate.atoms,
        depth: candidate.depth,
        heuristic: candidate.heuristic,
        closed: candidate.atoms.length >= 3 ? isLoopClosed(candidate.atoms, graph) : false
      })

      // Expand if depth < maxDepth
      if (candidate.depth < maxDepth) {
        const neighbors = findNeighbors(candidate.atoms, graph)
        for (const neighbor of neighbors) {
          const newAtoms = [...new Set([...candidate.atoms, neighbor.id])]
          const newHeuristic = estimateSynergy(newAtoms, atoms)

          queue.push({
            atoms: newAtoms,
            depth: candidate.depth + 1,
            heuristic: newHeuristic
          })
        }
      }
    }
  }

  // Score candidates
  const scored = candidates.map(c => ({
    ...c,
    synergy: estimateSynergy(c.atoms, atoms),
    score: estimateScore(c, atoms)
  }))

  // Sort by score
  scored.sort((a, b) => b.score - a.score)

  return {
    candidates: scored.slice(0, beamWidth * 2),
    statistics: {
      totalExplored: candidates.length,
      topCandidateScore: scored[0]?.score || 0,
      closedLoops: scored.filter(c => c.closed).length
    }
  }
}

/**
 * Select seed atoms with high leverage
 * @private
 */
function selectSeeds(atoms, count) {
  return atoms
    .filter(a => a.utility >= 0.8) // High utility
    .sort((a, b) => (b.utility * complexityScore(b.complexity)) - (a.utility * complexityScore(a.complexity)))
    .slice(0, count)
}

/**
 * Find neighboring atoms via edges
 * @private
 */
function findNeighbors(atomIds, graph) {
  const neighbors = new Set()

  for (const edge of graph.edges) {
    if (atomIds.includes(edge.from) && !atomIds.includes(edge.to)) {
      neighbors.add(edge.to)
    }
  }

  return Array.from(neighbors).map(id => ({ id }))
}

/**
 * Check if composition forms a loop (cycle detection)
 * @private
 */
function isLoopClosed(atomIds, graph) {
  if (atomIds.length < 2) return false

  const adjList = new Map()
  for (const id of atomIds) {
    adjList.set(id, [])
  }

  for (const edge of graph.edges) {
    if (atomIds.includes(edge.from) && atomIds.includes(edge.to)) {
      adjList.get(edge.from).push(edge.to)
    }
  }

  // Check for cycle via DFS
  const visited = new Set()
  const recStack = new Set()

  function hasCycle(v) {
    visited.add(v)
    recStack.add(v)

    for (const w of adjList.get(v) || []) {
      if (!visited.has(w)) {
        if (hasCycle(w)) return true
      } else if (recStack.has(w)) {
        return true
      }
    }

    recStack.delete(v)
    return false
  }

  for (const v of adjList.keys()) {
    if (!visited.has(v) && hasCycle(v)) {
      return true
    }
  }

  return false
}

/**
 * Estimate synergy of composition
 * @private
 */
function estimateSynergy(atomIds, atoms) {
  const atomMap = new Map(atoms.map(a => [a.id, a]))

  let synergy = 0
  let count = 0

  for (const id of atomIds) {
    const atom = atomMap.get(id)
    if (atom) {
      synergy += atom.utility * complexityScore(atom.complexity)
      count++
    }
  }

  // Bonus for loop closure
  const bonus = 1.0

  return count > 0 ? (synergy / count) * (1 + Math.min(0.5, (atomIds.length - 1) * 0.1)) : 0
}

/**
 * Estimate score for candidate (synergy + closure + edges)
 * @private
 */
function estimateScore(candidate, atoms) {
  const synergy = candidate.synergy
  const closureBonus = candidate.closed ? 0.3 : 0
  const sizeBonus = Math.min(0.2, candidate.atoms.length * 0.05)

  return synergy + closureBonus + sizeBonus
}

/**
 * Convert complexity to numeric score
 * @private
 */
function complexityScore(complexity) {
  const scores = {
    'low': 0.6,
    'moderate': 0.8,
    'high': 1.0,
    'very-high': 1.2
  }
  return scores[complexity] || 0.8
}

export default {
  searchCompositions,
  findNeighbors: (atomIds, graph) => findNeighbors(atomIds, graph)
}
