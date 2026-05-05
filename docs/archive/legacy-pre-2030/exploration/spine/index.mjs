/**
 * UNRDF Substrate Exploration Spine
 * Shared core module for all agent explorations
 *
 * Provides:
 * - RDF store initialization patterns
 * - Common data factory utilities
 * - Logging/reporting structure
 * - Test data generators
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { namedNode, literal } from 'n3-justified-only';

/**
 * Initialize a fresh RDF store
 * @returns {Object} Store instance with factory
 */
export function initializeStore() {
  const store = createStore();
  return {
    store,
    factory: dataFactory,
    quadCount: () => store.query('SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }')
  };
}

/**
 * Create basic test dataset (Turtles and basic triples)
 * @returns {Array} Array of quads
 */
export function createTestDataset() {
  const ex = (local) => namedNode(`http://example.org/${local}`);
  const foaf = (local) => namedNode(`http://xmlns.com/foaf/0.1/${local}`);
  const rdf = (local) => namedNode(`http://www.w3.org/1999/02/22-rdf-syntax-ns#${local}`);
  const rdfs = (local) => namedNode(`http://www.w3.org/2000/01/rdf-schema#${local}`);

  return [
    {
      subject: ex('alice'),
      predicate: rdf('type'),
      object: foaf('Person'),
      graph: namedNode('http://example.org/graph1')
    },
    {
      subject: ex('alice'),
      predicate: foaf('name'),
      object: literal('Alice'),
      graph: namedNode('http://example.org/graph1')
    },
    {
      subject: ex('alice'),
      predicate: foaf('knows'),
      object: ex('bob'),
      graph: namedNode('http://example.org/graph1')
    },
    {
      subject: ex('bob'),
      predicate: rdf('type'),
      object: foaf('Person'),
      graph: namedNode('http://example.org/graph1')
    },
    {
      subject: ex('bob'),
      predicate: foaf('name'),
      object: literal('Bob'),
      graph: namedNode('http://example.org/graph1')
    }
  ];
}

/**
 * Load quads into store
 * @param {Object} store - RDF store
 * @param {Array} quads - Array of quads to insert
 */
export function loadQuads(store, quads) {
  for (const quad of quads) {
    store.insert(quad);
  }
}

/**
 * Generate a simple report structure
 * @param {string} agentName - Agent identifier
 * @param {string} hypothesis - What was being tested
 * @param {boolean} passed - Success flag
 * @param {any} evidence - Proof data
 * @param {string} notes - Observations/surprises
 */
export function createReport(agentName, hypothesis, passed, evidence, notes) {
  return {
    agent: agentName,
    timestamp: new Date().toISOString(),
    hypothesis,
    passed,
    evidence,
    notes,
    context: {
      node_version: process.version,
      platform: process.platform
    }
  };
}

/**
 * Pretty-print a report to stdout
 */
export function printReport(report) {
  console.log('\n' + '='.repeat(70));
  console.log(`AGENT REPORT: ${report.agent}`);
  console.log('='.repeat(70));
  console.log(`Timestamp: ${report.timestamp}`);
  console.log(`Hypothesis: ${report.hypothesis}`);
  console.log(`Status: ${report.passed ? '✅ PASS' : '❌ FAIL'}`);
  console.log('\nEvidence:');
  console.log(JSON.stringify(report.evidence, null, 2));
  console.log('\nNotes:');
  console.log(report.notes);
  console.log('='.repeat(70) + '\n');
}

/**
 * Get packages available in UNRDF (scanner helper)
 * @returns {Promise<Array>} Array of discovered packages
 */
export async function discoverPackages() {
  // Placeholder - will be implemented by Agent 1
  // For now, return hardcoded list of known critical packages
  return [
    {
      name: '@unrdf/oxigraph',
      path: 'packages/oxigraph',
      role: 'store'
    },
    {
      name: '@unrdf/hooks',
      path: 'packages/hooks',
      role: 'enforce'
    },
    {
      name: '@unrdf/validation',
      path: 'packages/validation',
      role: 'enforce'
    },
    {
      name: '@unrdf/kgc-4d',
      path: 'packages/kgc-4d',
      role: 'derive'
    },
    {
      name: '@unrdf/streaming',
      path: 'packages/streaming',
      role: 'io'
    }
  ];
}

export default {
  initializeStore,
  createTestDataset,
  loadQuads,
  createReport,
  printReport,
  discoverPackages
};
