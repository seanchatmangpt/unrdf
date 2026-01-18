/**
 * @file test.mjs
 * @description Comprehensive test suite for Agent 4 impact set computation.
 */

import assert from 'node:assert/strict'
import { computeImpactSet, summarizeImpactSet, hashImpactSet } from './impact-set.mjs'
import { groupByPredicate, groupBySubject, analyzeGraphStructure } from './cardinality.mjs'

/**
 * Helper to create a simple quad for testing.
 * @param {string} subject - Subject IRI
 * @param {string} predicate - Predicate IRI
 * @param {string} object - Object IRI or literal
 * @param {string} [graph] - Graph IRI (optional)
 * @returns {Object} Quad object
 */
function createQuad(subject, predicate, object, graph = null) {
  return {
    subject: { value: subject, termType: 'NamedNode' },
    predicate: { value: predicate, termType: 'NamedNode' },
    object: { value: object, termType: 'Literal' },
    graph: graph ? { value: graph, termType: 'NamedNode' } : null
  }
}

/**
 * Test 1: Impact set computation with 10 quads.
 */
function test1_impactSetComputation() {
  console.log('Test 1: Impact set computation (10 quads)')

  const capsule = {
    delta: [
      createQuad('http://example.org/s1', 'http://example.org/p1', 'value1'),
      createQuad('http://example.org/s1', 'http://example.org/p2', 'value2'),
      createQuad('http://example.org/s2', 'http://example.org/p1', 'value3'),
      createQuad('http://example.org/s2', 'http://example.org/p2', 'value4'),
      createQuad('http://example.org/s3', 'http://example.org/p3', 'value5'),
      createQuad('http://example.org/s3', 'http://example.org/p1', 'value6'),
      createQuad('http://example.org/s4', 'http://example.org/p2', 'value7'),
      createQuad('http://example.org/s5', 'http://example.org/p3', 'value8'),
      createQuad('http://example.org/s5', 'http://example.org/p1', 'value9'),
      createQuad('http://example.org/s5', 'http://example.org/p2', 'value10')
    ]
  }

  const impactSet = computeImpactSet(capsule)

  assert.equal(impactSet.cardinality.subjects, 5, 'Should have 5 unique subjects')
  assert.equal(impactSet.cardinality.predicates, 3, 'Should have 3 unique predicates')
  assert.equal(impactSet.cardinality.graphs, 1, 'Should have 1 graph (default)')
  assert.equal(impactSet.cardinality.totalQuads, 10, 'Should have 10 total quads')

  assert(impactSet.subjects.has('http://example.org/s1'), 'Should include s1')
  assert(impactSet.subjects.has('http://example.org/s5'), 'Should include s5')
  assert(impactSet.predicates.has('http://example.org/p1'), 'Should include p1')
  assert(impactSet.predicates.has('http://example.org/p3'), 'Should include p3')

  console.log('✅ Test 1 passed')
}

/**
 * Test 2: Deterministic hashing (run 100 times).
 */
function test2_deterministicHashing() {
  console.log('Test 2: Deterministic hashing (100 runs)')

  const capsule = {
    delta: [
      createQuad('http://example.org/s1', 'http://example.org/p1', 'value1'),
      createQuad('http://example.org/s2', 'http://example.org/p2', 'value2'),
      createQuad('http://example.org/s3', 'http://example.org/p3', 'value3')
    ]
  }

  const hashes = new Set()

  for (let i = 0; i < 100; i++) {
    const impactSet = computeImpactSet(capsule)
    const hash = hashImpactSet(impactSet)
    hashes.add(hash)
  }

  assert.equal(hashes.size, 1, 'Should produce same hash 100 times')

  console.log(`✅ Test 2 passed (hash: ${Array.from(hashes)[0].slice(0, 16)}...)`)
}

/**
 * Test 3: Predicate grouping (3 predicates).
 */
function test3_predicateGrouping() {
  console.log('Test 3: Predicate grouping')

  const capsule = {
    delta: [
      createQuad('http://example.org/s1', 'http://example.org/p1', 'v1'),
      createQuad('http://example.org/s2', 'http://example.org/p1', 'v2'),
      createQuad('http://example.org/s3', 'http://example.org/p1', 'v3'),
      createQuad('http://example.org/s4', 'http://example.org/p2', 'v4'),
      createQuad('http://example.org/s5', 'http://example.org/p2', 'v5'),
      createQuad('http://example.org/s6', 'http://example.org/p3', 'v6')
    ]
  }

  const grouped = groupByPredicate(capsule)

  assert.equal(grouped.size, 3, 'Should have 3 predicates')
  assert.equal(grouped.get('http://example.org/p1'), 3, 'p1 should have 3 quads')
  assert.equal(grouped.get('http://example.org/p2'), 2, 'p2 should have 2 quads')
  assert.equal(grouped.get('http://example.org/p3'), 1, 'p3 should have 1 quad')

  console.log('✅ Test 3 passed')
}

/**
 * Test 4: Empty capsule (0 quads).
 */
function test4_emptyCapsule() {
  console.log('Test 4: Empty capsule')

  const capsule = { delta: [] }

  const impactSet = computeImpactSet(capsule)

  assert.equal(impactSet.cardinality.subjects, 0, 'Should have 0 subjects')
  assert.equal(impactSet.cardinality.predicates, 0, 'Should have 0 predicates')
  assert.equal(impactSet.cardinality.graphs, 0, 'Should have 0 graphs')
  assert.equal(impactSet.cardinality.totalQuads, 0, 'Should have 0 quads')

  assert.equal(impactSet.summary, 'Modified 0 subjects, 0 predicates, 0 graphs. Total 0 quads.')

  console.log('✅ Test 4 passed')
}

/**
 * Test 5: Large capsule (1000 quads, performance <100ms).
 */
function test5_largeCapsule() {
  console.log('Test 5: Large capsule (1000 quads, performance test)')

  // Generate 1000 quads
  const delta = []
  for (let i = 0; i < 1000; i++) {
    const subjectNum = i % 100 // 100 unique subjects
    const predicateNum = i % 10 // 10 unique predicates
    delta.push(
      createQuad(
        `http://example.org/s${subjectNum}`,
        `http://example.org/p${predicateNum}`,
        `value${i}`
      )
    )
  }

  const capsule = { delta }

  const startTime = performance.now()
  const impactSet = computeImpactSet(capsule)
  const elapsed = performance.now() - startTime

  assert.equal(impactSet.cardinality.totalQuads, 1000, 'Should have 1000 quads')
  assert.equal(impactSet.cardinality.subjects, 100, 'Should have 100 subjects')
  assert.equal(impactSet.cardinality.predicates, 10, 'Should have 10 predicates')

  assert(elapsed < 100, `Should complete in <100ms (took ${elapsed.toFixed(2)}ms)`)

  console.log(`✅ Test 5 passed (${elapsed.toFixed(2)}ms)`)
}

/**
 * Test 6: Subject grouping.
 */
function test6_subjectGrouping() {
  console.log('Test 6: Subject grouping')

  const capsule = {
    delta: [
      createQuad('http://example.org/s1', 'http://example.org/p1', 'v1'),
      createQuad('http://example.org/s1', 'http://example.org/p2', 'v2'),
      createQuad('http://example.org/s1', 'http://example.org/p3', 'v3'),
      createQuad('http://example.org/s2', 'http://example.org/p1', 'v4'),
      createQuad('http://example.org/s2', 'http://example.org/p2', 'v5')
    ]
  }

  const grouped = groupBySubject(capsule)

  assert.equal(grouped.size, 2, 'Should have 2 subjects')
  assert.equal(grouped.get('http://example.org/s1'), 3, 's1 should have 3 quads')
  assert.equal(grouped.get('http://example.org/s2'), 2, 's2 should have 2 quads')

  console.log('✅ Test 6 passed')
}

/**
 * Test 7: Graph structure analysis.
 */
function test7_graphStructureAnalysis() {
  console.log('Test 7: Graph structure analysis')

  const capsule = {
    delta: [
      createQuad('http://example.org/s1', 'http://example.org/p1', 'v1'),
      createQuad('http://example.org/s1', 'http://example.org/p2', 'v2'),
      createQuad('http://example.org/s2', 'http://example.org/p1', 'v3'),
      createQuad('http://example.org/s3', 'http://example.org/p1', 'v4')
    ]
  }

  const analysis = analyzeGraphStructure(capsule)

  assert.equal(analysis.spanCount, 1, 'Should span 1 graph')
  assert.equal(analysis.density, 4 / 3, 'Density should be 4/3 quads per subject')

  console.log(`✅ Test 7 passed (density: ${analysis.density.toFixed(2)})`)
}

/**
 * Test 8: Multiple graphs.
 */
function test8_multipleGraphs() {
  console.log('Test 8: Multiple graphs')

  const capsule = {
    delta: [
      createQuad('http://example.org/s1', 'http://example.org/p1', 'v1', 'http://example.org/g1'),
      createQuad('http://example.org/s2', 'http://example.org/p1', 'v2', 'http://example.org/g2'),
      createQuad('http://example.org/s3', 'http://example.org/p1', 'v3', 'http://example.org/g3')
    ]
  }

  const impactSet = computeImpactSet(capsule)

  assert.equal(impactSet.cardinality.graphs, 3, 'Should have 3 graphs')
  assert(impactSet.graphs.has('http://example.org/g1'), 'Should include g1')
  assert(impactSet.graphs.has('http://example.org/g3'), 'Should include g3')

  console.log('✅ Test 8 passed')
}

// Run all tests
console.log('=== Agent 4: Impact Set Tests ===\n')

try {
  test1_impactSetComputation()
  test2_deterministicHashing()
  test3_predicateGrouping()
  test4_emptyCapsule()
  test5_largeCapsule()
  test6_subjectGrouping()
  test7_graphStructureAnalysis()
  test8_multipleGraphs()

  console.log('\n=== All tests passed ✅ ===')
} catch (error) {
  console.error('\n❌ Test failed:', error.message)
  console.error(error.stack)
  process.exit(1)
}
