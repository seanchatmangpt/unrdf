/**
 * @file Dependency graph tests - Chicago School TDD with real RDF stores
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest'
import { Store, DataFactory } from 'n3'
import {
  buildDependencyGraph,
  detectCircularDependencies,
  topologicalSort,
  analyzeDependencyPath,
  getTransitiveDependencies,
  getTransitiveDependents,
  calculateImpactScore,
} from '../../src/project-engine/dependency-graph.mjs'

const { namedNode, literal } = DataFactory

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  dom: 'http://example.org/unrdf/domain#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
}

/* ========================================================================= */
/* Test helpers                                                              */
/* ========================================================================= */

/**
 * Create domain store with entities and relationships
 * @param {Array<{name: string, relatesTo?: string[]}>} entities
 * @returns {Store}
 */
function createDomainStore(entities) {
  const store = new Store()

  for (const { name, relatesTo = [] } of entities) {
    const iri = namedNode(`${NS.dom}${name}`)

    // Add entity type
    store.addQuad(iri, namedNode(`${NS.rdf}type`), namedNode(`${NS.dom}Entity`))
    store.addQuad(iri, namedNode(`${NS.rdfs}label`), literal(name))

    // Add relationships
    for (const related of relatesTo) {
      store.addQuad(iri, namedNode(`${NS.dom}relatesTo`), namedNode(`${NS.dom}${related}`))
    }
  }

  return store
}

/**
 * Create project store with features and files
 * @param {Object} config
 * @returns {Store}
 */
function createProjectStore(config = {}) {
  const store = new Store()
  const {
    features = {},
    baseIri = 'http://example.org/unrdf/project#',
  } = config

  // Add project
  const projectIri = namedNode(`${baseIri}project`)
  store.addQuad(projectIri, namedNode(`${NS.rdf}type`), namedNode(`${NS.proj}Project`))

  // Add features
  for (const [featureName, featureConfig] of Object.entries(features)) {
    const featureIri = namedNode(`${baseIri}feature/${encodeURIComponent(featureName)}`)

    // Feature type and label
    store.addQuad(featureIri, namedNode(`${NS.rdf}type`), namedNode(`${NS.proj}Feature`))
    store.addQuad(featureIri, namedNode(`${NS.rdfs}label`), literal(featureName))
    store.addQuad(projectIri, namedNode(`${NS.proj}hasFeature`), featureIri)

    // Add files
    const files = featureConfig.files || []
    for (const filePath of files) {
      const fileIri = namedNode(`http://example.org/unrdf/fs#${encodeURIComponent(filePath)}`)
      store.addQuad(fileIri, namedNode(`${NS.fs}relativePath`), literal(filePath))
      store.addQuad(fileIri, namedNode(`${NS.proj}belongsToFeature`), featureIri)
    }

    // Add dependencies
    const dependencies = featureConfig.dependencies || []
    for (const dep of dependencies) {
      const depIri = namedNode(`${baseIri}feature/${encodeURIComponent(dep)}`)
      store.addQuad(featureIri, namedNode(`${NS.proj}dependsOn`), depIri)
    }
  }

  return store
}

/* ========================================================================= */
/* buildDependencyGraph tests                                                */
/* ========================================================================= */

describe('dependency-graph', () => {
  describe('buildDependencyGraph', () => {
    it('creates graph from domain entities', () => {
      const domainStore = createDomainStore([
        { name: 'User', relatesTo: ['Order'] },
        { name: 'Order', relatesTo: ['Product'] },
        { name: 'Product' },
      ])
      const projectStore = new Store()

      const graph = buildDependencyGraph({ domainStore, projectStore })

      expect(graph.nodes.size).toBe(3)
      expect(graph.nodes.has('User')).toBe(true)
      expect(graph.nodes.has('Order')).toBe(true)
      expect(graph.nodes.has('Product')).toBe(true)
    })

    it('creates edges from domain relationships', () => {
      const domainStore = createDomainStore([
        { name: 'User', relatesTo: ['Order'] },
        { name: 'Order', relatesTo: ['Product'] },
        { name: 'Product' },
      ])
      const projectStore = new Store()

      const graph = buildDependencyGraph({ domainStore, projectStore })

      expect(graph.edges.get('User').has('Order')).toBe(true)
      expect(graph.edges.get('Order').has('Product')).toBe(true)
      expect(graph.edges.get('Product').size).toBe(0)
    })

    it('includes project features as nodes', () => {
      const domainStore = createDomainStore([{ name: 'User' }])
      const projectStore = createProjectStore({
        features: {
          checkout: { files: ['src/checkout/cart.mjs'] },
          catalog: { files: ['src/catalog/products.mjs'] },
        },
      })

      const graph = buildDependencyGraph({ domainStore, projectStore })

      expect(graph.nodes.has('checkout')).toBe(true)
      expect(graph.nodes.has('catalog')).toBe(true)
      expect(graph.nodes.get('checkout').type).toBe('feature')
    })

    it('includes explicit feature dependencies', () => {
      const domainStore = new Store()
      const projectStore = createProjectStore({
        features: {
          checkout: { files: ['src/checkout/cart.mjs'], dependencies: ['payment'] },
          payment: { files: ['src/payment/process.mjs'] },
        },
      })

      const graph = buildDependencyGraph({ domainStore, projectStore })

      expect(graph.edges.get('checkout').has('payment')).toBe(true)
    })

    it('creates reverse edges for dependents', () => {
      const domainStore = createDomainStore([
        { name: 'User', relatesTo: ['Order'] },
        { name: 'Order' },
      ])
      const projectStore = new Store()

      const graph = buildDependencyGraph({ domainStore, projectStore })

      expect(graph.reverseEdges.get('Order').has('User')).toBe(true)
    })

    it('reports correct summary statistics', () => {
      const domainStore = createDomainStore([
        { name: 'User', relatesTo: ['Order', 'Profile'] },
        { name: 'Order', relatesTo: ['Product'] },
        { name: 'Product' },
        { name: 'Profile' },
      ])
      const projectStore = new Store()

      const graph = buildDependencyGraph({ domainStore, projectStore })

      expect(graph.summary.nodeCount).toBe(4)
      expect(graph.summary.edgeCount).toBe(3) // User->Order, User->Profile, Order->Product
    })

    it('handles empty stores', () => {
      const domainStore = new Store()
      const projectStore = new Store()

      const graph = buildDependencyGraph({ domainStore, projectStore })

      expect(graph.nodes.size).toBe(0)
      expect(graph.edges.size).toBe(0)
      expect(graph.summary.nodeCount).toBe(0)
      expect(graph.summary.edgeCount).toBe(0)
    })
  })

  /* ========================================================================= */
  /* detectCircularDependencies tests                                          */
  /* ========================================================================= */

  describe('detectCircularDependencies', () => {
    it('returns no cycles for acyclic graph', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B', relatesTo: ['C'] },
        { name: 'C' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const result = detectCircularDependencies(graph)

      expect(result.cycles).toHaveLength(0)
      expect(result.severity).toBe('none')
    })

    it('detects simple two-node cycle', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B', relatesTo: ['A'] },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const result = detectCircularDependencies(graph)

      expect(result.cycles.length).toBeGreaterThan(0)
      expect(result.severity).toBe('medium')
    })

    it('detects three-node cycle', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B', relatesTo: ['C'] },
        { name: 'C', relatesTo: ['A'] },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const result = detectCircularDependencies(graph)

      expect(result.cycles.length).toBeGreaterThan(0)
      // Cycle should contain all three nodes
      const cycle = result.cycles[0]
      expect(cycle).toContain('A')
      expect(cycle).toContain('B')
      expect(cycle).toContain('C')
    })

    it('detects multiple cycles', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B', relatesTo: ['A'] },  // Cycle 1: A <-> B
        { name: 'C', relatesTo: ['D'] },
        { name: 'D', relatesTo: ['C'] },  // Cycle 2: C <-> D
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const result = detectCircularDependencies(graph)

      expect(result.cycles.length).toBeGreaterThanOrEqual(2)
    })

    it('sets high severity for many cycles', () => {
      // Create graph with 4+ cycles
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B', relatesTo: ['A', 'C'] },
        { name: 'C', relatesTo: ['D'] },
        { name: 'D', relatesTo: ['C', 'E'] },
        { name: 'E', relatesTo: ['F'] },
        { name: 'F', relatesTo: ['E', 'G'] },
        { name: 'G', relatesTo: ['H'] },
        { name: 'H', relatesTo: ['G'] },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const result = detectCircularDependencies(graph)

      expect(result.cycles.length).toBeGreaterThanOrEqual(4)
      expect(result.severity).toBe('high')
    })

    it('sets high severity for long cycles', () => {
      // Create one long cycle (4+ nodes)
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B', relatesTo: ['C'] },
        { name: 'C', relatesTo: ['D'] },
        { name: 'D', relatesTo: ['E'] },
        { name: 'E', relatesTo: ['A'] },  // Long cycle: A -> B -> C -> D -> E -> A
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const result = detectCircularDependencies(graph)

      expect(result.cycles.length).toBeGreaterThan(0)
      expect(result.severity).toBe('high')
    })
  })

  /* ========================================================================= */
  /* topologicalSort tests                                                     */
  /* ========================================================================= */

  describe('topologicalSort', () => {
    it('sorts simple linear dependencies', () => {
      // A depends on B, B depends on C
      // Build order: C first (no deps), then B, then A
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B', relatesTo: ['C'] },
        { name: 'C' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const { sorted, hasCycle } = topologicalSort(graph)

      expect(hasCycle).toBe(false)
      expect(sorted).toHaveLength(3)
      // Build order: C before B, B before A (dependencies first)
      expect(sorted.indexOf('C')).toBeLessThan(sorted.indexOf('B'))
      expect(sorted.indexOf('B')).toBeLessThan(sorted.indexOf('A'))
    })

    it('handles independent nodes', () => {
      const domainStore = createDomainStore([
        { name: 'A' },
        { name: 'B' },
        { name: 'C' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const { sorted, hasCycle } = topologicalSort(graph)

      expect(hasCycle).toBe(false)
      expect(sorted).toHaveLength(3)
      expect(sorted).toContain('A')
      expect(sorted).toContain('B')
      expect(sorted).toContain('C')
    })

    it('handles diamond dependencies', () => {
      // Diamond: A depends on B and C, B depends on D, C depends on D
      // Build order: D first (no deps), then B and C, then A
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B', 'C'] },
        { name: 'B', relatesTo: ['D'] },
        { name: 'C', relatesTo: ['D'] },
        { name: 'D' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const { sorted, hasCycle } = topologicalSort(graph)

      expect(hasCycle).toBe(false)
      // D has no deps, comes first
      // B and C depend on D, come after D
      // A depends on B and C, comes last
      expect(sorted.indexOf('D')).toBeLessThan(sorted.indexOf('B'))
      expect(sorted.indexOf('D')).toBeLessThan(sorted.indexOf('C'))
      expect(sorted.indexOf('B')).toBeLessThan(sorted.indexOf('A'))
      expect(sorted.indexOf('C')).toBeLessThan(sorted.indexOf('A'))
    })

    it('indicates cycle when present', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B', relatesTo: ['A'] },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const { hasCycle } = topologicalSort(graph)

      expect(hasCycle).toBe(true)
    })

    it('returns deterministic order for same graph', () => {
      const domainStore = createDomainStore([
        { name: 'X', relatesTo: ['Y'] },
        { name: 'Y', relatesTo: ['Z'] },
        { name: 'Z' },
        { name: 'A' },  // Independent
        { name: 'B' },  // Independent
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const { sorted: sorted1 } = topologicalSort(graph)
      const { sorted: sorted2 } = topologicalSort(graph)

      expect(sorted1).toEqual(sorted2)
    })
  })

  /* ========================================================================= */
  /* analyzeDependencyPath tests                                               */
  /* ========================================================================= */

  describe('analyzeDependencyPath', () => {
    it('finds direct dependency path', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const result = analyzeDependencyPath(graph, 'A', 'B')

      expect(result).not.toBeNull()
      expect(result.path).toEqual(['A', 'B'])
      expect(result.depth).toBe(1)
    })

    it('finds multi-hop dependency path', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B', relatesTo: ['C'] },
        { name: 'C', relatesTo: ['D'] },
        { name: 'D' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const result = analyzeDependencyPath(graph, 'A', 'D')

      expect(result).not.toBeNull()
      expect(result.path).toEqual(['A', 'B', 'C', 'D'])
      expect(result.depth).toBe(3)
    })

    it('returns null when no path exists', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B' },
        { name: 'C' },  // Disconnected
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const result = analyzeDependencyPath(graph, 'A', 'C')

      expect(result).toBeNull()
    })

    it('returns null for non-existent nodes', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      expect(analyzeDependencyPath(graph, 'A', 'NonExistent')).toBeNull()
      expect(analyzeDependencyPath(graph, 'NonExistent', 'B')).toBeNull()
    })

    it('identifies break points in long paths', () => {
      // Create a path where middle node has high connectivity
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['Hub'] },
        { name: 'X', relatesTo: ['Hub'] },
        { name: 'Y', relatesTo: ['Hub'] },
        { name: 'Hub', relatesTo: ['B', 'C', 'D'] },
        { name: 'B', relatesTo: ['End'] },
        { name: 'C' },
        { name: 'D' },
        { name: 'End' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const result = analyzeDependencyPath(graph, 'A', 'End')

      expect(result).not.toBeNull()
      // Hub should be identified as break point (high connectivity)
      expect(result.breakPoints).toContain('Hub')
    })

    it('finds shortest path when multiple exist', () => {
      // A -> B -> C (length 2)
      // A -> D -> E -> C (length 3)
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B', 'D'] },
        { name: 'B', relatesTo: ['C'] },
        { name: 'D', relatesTo: ['E'] },
        { name: 'E', relatesTo: ['C'] },
        { name: 'C' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const result = analyzeDependencyPath(graph, 'A', 'C')

      expect(result).not.toBeNull()
      expect(result.depth).toBe(2)  // Shortest path
    })
  })

  /* ========================================================================= */
  /* getTransitiveDependencies tests                                           */
  /* ========================================================================= */

  describe('getTransitiveDependencies', () => {
    it('returns all transitive dependencies', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B', relatesTo: ['C', 'D'] },
        { name: 'C', relatesTo: ['E'] },
        { name: 'D' },
        { name: 'E' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const deps = getTransitiveDependencies(graph, 'A')

      expect(deps.has('B')).toBe(true)
      expect(deps.has('C')).toBe(true)
      expect(deps.has('D')).toBe(true)
      expect(deps.has('E')).toBe(true)
      expect(deps.size).toBe(4)
    })

    it('returns empty set for leaf nodes', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const deps = getTransitiveDependencies(graph, 'B')

      expect(deps.size).toBe(0)
    })
  })

  /* ========================================================================= */
  /* getTransitiveDependents tests                                             */
  /* ========================================================================= */

  describe('getTransitiveDependents', () => {
    it('returns all transitive dependents', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B', relatesTo: ['C'] },
        { name: 'X', relatesTo: ['C'] },
        { name: 'C' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const dependents = getTransitiveDependents(graph, 'C')

      expect(dependents.has('A')).toBe(true)
      expect(dependents.has('B')).toBe(true)
      expect(dependents.has('X')).toBe(true)
      expect(dependents.size).toBe(3)
    })

    it('returns empty set for root nodes', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const dependents = getTransitiveDependents(graph, 'A')

      expect(dependents.size).toBe(0)
    })
  })

  /* ========================================================================= */
  /* calculateImpactScore tests                                                */
  /* ========================================================================= */

  describe('calculateImpactScore', () => {
    it('calculates impact for highly depended node', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['Core'] },
        { name: 'B', relatesTo: ['Core'] },
        { name: 'C', relatesTo: ['Core'] },
        { name: 'Core', relatesTo: ['D'] },
        { name: 'D' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const impact = calculateImpactScore(graph, 'Core')

      expect(impact.direct).toBe(3)  // A, B, C depend directly
      expect(impact.transitive).toBe(3)  // Same as direct in this case
      expect(impact.score).toBeGreaterThan(0)
    })

    it('returns zero impact for leaf nodes', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['B'] },
        { name: 'B' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const impact = calculateImpactScore(graph, 'A')

      expect(impact.direct).toBe(0)
      expect(impact.transitive).toBe(0)
      expect(impact.score).toBe(0)
    })

    it('weights direct dependencies higher', () => {
      const domainStore = createDomainStore([
        { name: 'A', relatesTo: ['Hub'] },
        { name: 'B', relatesTo: ['A'] },  // Indirect via A
        { name: 'Hub' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const impact = calculateImpactScore(graph, 'Hub')

      // Direct: A (1), Transitive: A, B (2)
      expect(impact.direct).toBe(1)
      expect(impact.transitive).toBe(2)
      // Score = (1 * 2) + 2 = 4
      expect(impact.score).toBe(4)
    })
  })

  /* ========================================================================= */
  /* Integration tests with real project patterns                              */
  /* ========================================================================= */

  describe('integration: e-commerce domain', () => {
    it('models typical e-commerce dependencies', () => {
      const domainStore = createDomainStore([
        { name: 'User', relatesTo: ['Order', 'Cart', 'Address'] },
        { name: 'Order', relatesTo: ['Product', 'Payment', 'Shipping'] },
        { name: 'Cart', relatesTo: ['Product'] },
        { name: 'Product', relatesTo: ['Category', 'Inventory'] },
        { name: 'Payment' },
        { name: 'Shipping', relatesTo: ['Address'] },
        { name: 'Address' },
        { name: 'Category' },
        { name: 'Inventory' },
      ])
      const projectStore = new Store()

      const graph = buildDependencyGraph({ domainStore, projectStore })

      expect(graph.nodes.size).toBe(9)
      expect(graph.summary.edgeCount).toBeGreaterThan(8)

      // User has most direct dependencies
      expect(graph.edges.get('User').size).toBe(3)

      // Product is most depended upon
      const productDeps = getTransitiveDependents(graph, 'Product')
      expect(productDeps.size).toBeGreaterThanOrEqual(3)
    })

    it('sorts e-commerce entities in buildable order', () => {
      const domainStore = createDomainStore([
        { name: 'Order', relatesTo: ['Product', 'User'] },
        { name: 'Product', relatesTo: ['Category'] },
        { name: 'User' },
        { name: 'Category' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const { sorted, hasCycle } = topologicalSort(graph)

      expect(hasCycle).toBe(false)
      // Dependencies must come before dependents
      // Order depends on Product and User - both must come before Order
      expect(sorted.indexOf('Product')).toBeLessThan(sorted.indexOf('Order'))
      expect(sorted.indexOf('User')).toBeLessThan(sorted.indexOf('Order'))
      // Product depends on Category - Category must come before Product
      expect(sorted.indexOf('Category')).toBeLessThan(sorted.indexOf('Product'))
    })
  })

  describe('integration: microservices pattern', () => {
    it('detects service dependency cycles', () => {
      // Common anti-pattern: circular service dependencies
      const domainStore = createDomainStore([
        { name: 'UserService', relatesTo: ['OrderService'] },
        { name: 'OrderService', relatesTo: ['NotificationService'] },
        { name: 'NotificationService', relatesTo: ['UserService'] },  // Creates cycle!
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const { cycles, severity } = detectCircularDependencies(graph)

      expect(cycles.length).toBeGreaterThan(0)
      expect(severity).not.toBe('none')
    })

    it('identifies high-impact shared services', () => {
      const domainStore = createDomainStore([
        { name: 'UserService', relatesTo: ['AuthService', 'DBService'] },
        { name: 'OrderService', relatesTo: ['AuthService', 'DBService'] },
        { name: 'PaymentService', relatesTo: ['AuthService', 'DBService'] },
        { name: 'AuthService', relatesTo: ['DBService'] },
        { name: 'DBService' },
      ])
      const projectStore = new Store()
      const graph = buildDependencyGraph({ domainStore, projectStore })

      const dbImpact = calculateImpactScore(graph, 'DBService')
      const authImpact = calculateImpactScore(graph, 'AuthService')
      const userImpact = calculateImpactScore(graph, 'UserService')

      // DBService has highest impact
      expect(dbImpact.score).toBeGreaterThan(authImpact.score)
      expect(dbImpact.score).toBeGreaterThan(userImpact.score)
    })
  })
})
