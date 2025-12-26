/**
 * @file mu3-graph-inference.mjs - Infer dependency graph from atoms (μ3 agent)
 * @description Build G_τ := ⟨V=Atoms, E=Edges⟩ with edge kinds: feed, govern, host
 * @equation G_τ := μ_graph(Atoms_τ)
 */

/**
 * Infer edges between atoms
 * @param {array} atoms - Extracted atoms
 * @returns {object} - Graph with vertices and edges
 */
export async function inferGraph(atoms) {
  const edges = []
  const atomMap = new Map(atoms.map(a => [a.id, a]))

  // Feed edges: output type ⊆ input type
  for (let i = 0; i < atoms.length; i++) {
    for (let j = 0; j < atoms.length; j++) {
      if (i === j) continue

      const source = atoms[i]
      const target = atoms[j]

      if (canFeed(source, target)) {
        edges.push({
          from: source.id,
          to: target.id,
          kind: 'feed',
          reason: `${source.name} output → ${target.name} input`,
          strength: calculateStrength(source, target)
        })
      }
    }
  }

  // Govern edges: source has policy/guard applicable to target
  for (let i = 0; i < atoms.length; i++) {
    for (let j = 0; j < atoms.length; j++) {
      if (i === j) continue

      const governor = atoms[i]
      const governed = atoms[j]

      if (canGovern(governor, governed)) {
        edges.push({
          from: governor.id,
          to: governed.id,
          kind: 'govern',
          reason: `${governor.name} governs ${governed.name}`,
          strength: 0.8
        })
      }
    }
  }

  // Host edges: source runtime can execute target module
  for (let i = 0; i < atoms.length; i++) {
    for (let j = 0; j < atoms.length; j++) {
      if (i === j) continue

      const host = atoms[i]
      const module = atoms[j]

      if (canHost(host, module)) {
        edges.push({
          from: host.id,
          to: module.id,
          kind: 'host',
          reason: `${host.name} can host ${module.name}`,
          strength: 0.85
        })
      }
    }
  }

  // Dependency edges: explicit dependencies
  for (const atom of atoms) {
    if (atom.dependencies && Array.isArray(atom.dependencies)) {
      for (const dep of atom.dependencies) {
        const depAtom = atoms.find(a => a.implementation && a.implementation.includes(dep))
        if (depAtom) {
          edges.push({
            from: atom.id,
            to: depAtom.id,
            kind: 'depends',
            reason: `${atom.name} depends on ${dep}`,
            strength: 1.0
          })
        }
      }
    }
  }

  return {
    vertices: atoms.map(a => ({
      id: a.id,
      name: a.name,
      complexity: a.complexity,
      utility: a.utility,
      inDegree: 0,
      outDegree: 0
    })),
    edges: edges,
    statistics: {
      vertexCount: atoms.length,
      edgeCount: edges.length,
      averageDegree: edges.length * 2 / atoms.length,
      edgesByKind: countEdgesByKind(edges)
    }
  }
}

/**
 * Check if source can feed into target
 * @private
 */
function canFeed(source, target) {
  // RDF Store feeds into SPARQL Executor, Hooks, Pattern Matcher, etc.
  if (source.id === 'atom-rdf-store') {
    return ['atom-sparql-executor', 'atom-hook-system', 'atom-pattern-matcher',
      'atom-change-feed', 'atom-federation-coordinator', 'atom-graph-analytics'].includes(target.id)
  }

  // SPARQL Executor feeds into Knowledge Engine, Hooks, etc.
  if (source.id === 'atom-sparql-executor') {
    return ['atom-pattern-matcher', 'atom-hook-system', 'atom-graph-analytics'].includes(target.id)
  }

  // Change Feed feeds into Workflow, Federation
  if (source.id === 'atom-change-feed') {
    return ['atom-workflow-engine', 'atom-federation-coordinator'].includes(target.id)
  }

  // Event Logger feeds into Workflow
  if (source.id === 'atom-event-logger') {
    return target.id === 'atom-workflow-engine'
  }

  return false
}

/**
 * Check if source can govern target
 * @private
 */
function canGovern(source, target) {
  // Hook System governs RDF Store, SPARQL, Pattern Matcher
  if (source.id === 'atom-hook-system') {
    return ['atom-rdf-store', 'atom-sparql-executor', 'atom-pattern-matcher'].includes(target.id)
  }

  // Event Logger governs Workflow
  if (source.id === 'atom-event-logger') {
    return target.id === 'atom-workflow-engine'
  }

  return false
}

/**
 * Check if source can host target
 * @private
 */
function canHost(source, target) {
  // RDF Store can host Hooks, Pattern Matcher, Streaming
  if (source.id === 'atom-rdf-store') {
    return ['atom-hook-system', 'atom-pattern-matcher', 'atom-change-feed'].includes(target.id)
  }

  // Workflow Engine can host ML Inference
  if (source.id === 'atom-workflow-engine') {
    return target.id === 'atom-ml-inference'
  }

  return false
}

/**
 * Calculate edge strength (0.0-1.0)
 * @private
 */
function calculateStrength(source, target) {
  // Strong edges: low complexity source to high utility target
  const base = 0.5
  const complexityBonus = 1.0 - (complexityScore(source.complexity) / 10)
  const utilityBonus = target.utility * 0.3

  return Math.min(1.0, base + complexityBonus + utilityBonus)
}

/**
 * Convert complexity string to numeric score
 * @private
 */
function complexityScore(complexity) {
  const scores = {
    'low': 2,
    'moderate': 5,
    'high': 7,
    'very-high': 9
  }
  return scores[complexity] || 5
}

/**
 * Count edges by kind
 * @private
 */
function countEdgesByKind(edges) {
  const counts = {
    feed: 0,
    govern: 0,
    host: 0,
    depends: 0
  }

  for (const edge of edges) {
    counts[edge.kind] = (counts[edge.kind] || 0) + 1
  }

  return counts
}

/**
 * Find strongly connected components (composition loops)
 * @param {object} graph - Graph from inferGraph
 * @returns {array} - Array of SCCs
 */
export function findCompositionLoops(graph) {
  const adjList = new Map()

  // Build adjacency list
  for (const vertex of graph.vertices) {
    adjList.set(vertex.id, [])
  }

  for (const edge of graph.edges) {
    if (adjList.has(edge.from)) {
      adjList.get(edge.from).push(edge.to)
    }
  }

  // Tarjan's SCC algorithm (simplified)
  const sccs = []
  const visited = new Set()
  const stack = []

  function visit(v) {
    visited.add(v)
    for (const w of adjList.get(v) || []) {
      if (!visited.has(w)) {
        visit(w)
      }
    }
    stack.push(v)
  }

  for (const v of adjList.keys()) {
    if (!visited.has(v)) {
      visit(v)
    }
  }

  return sccs
}

export default {
  inferGraph,
  findCompositionLoops
}
