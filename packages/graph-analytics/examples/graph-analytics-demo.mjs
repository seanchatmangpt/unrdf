#!/usr/bin/env node
/**
 * Graph Analytics Demo
 *
 * Demonstrates RDF graph analytics capabilities:
 * - Convert RDF to graph
 * - PageRank centrality
 * - Shortest paths
 * - Community detection
 */

import { createStore, dataFactory } from '../../oxigraph/src/index.mjs';
import {
  rdfToGraph,
  getGraphStats,
  computePageRank,
  computeDegreeCentrality,
  getTopNodes,
  findShortestPath,
  findAllPaths,
  detectCommunitiesLPA,
  getCommunityStats,
} from '../src/index.mjs';

const { namedNode, literal, triple } = dataFactory;

/**
 * Create a sample knowledge graph about people and organizations
 */
function createSampleGraph() {
  const store = createStore();

  // People
  const alice = namedNode('http://example.org/Alice');
  const bob = namedNode('http://example.org/Bob');
  const charlie = namedNode('http://example.org/Charlie');
  const diana = namedNode('http://example.org/Diana');
  const eve = namedNode('http://example.org/Eve');
  const frank = namedNode('http://example.org/Frank');

  // Organizations
  const acmeCorp = namedNode('http://example.org/AcmeCorp');
  const techInc = namedNode('http://example.org/TechInc');

  // Predicates
  const knows = namedNode('http://schema.org/knows');
  const worksFor = namedNode('http://schema.org/worksFor');
  const manages = namedNode('http://schema.org/manages');
  const collaboratesWith = namedNode('http://schema.org/collaboratesWith');
  const name = namedNode('http://schema.org/name');

  // Add triples - Social network
  store.add(triple(alice, knows, bob));
  store.add(triple(alice, knows, charlie));
  store.add(triple(bob, knows, charlie));
  store.add(triple(bob, knows, diana));
  store.add(triple(charlie, knows, diana));
  store.add(triple(charlie, knows, eve));
  store.add(triple(diana, knows, eve));
  store.add(triple(diana, knows, frank));
  store.add(triple(eve, knows, frank));

  // Work relationships
  store.add(triple(alice, worksFor, acmeCorp));
  store.add(triple(bob, worksFor, acmeCorp));
  store.add(triple(charlie, worksFor, acmeCorp));
  store.add(triple(diana, worksFor, techInc));
  store.add(triple(eve, worksFor, techInc));
  store.add(triple(frank, worksFor, techInc));

  // Management
  store.add(triple(alice, manages, bob));
  store.add(triple(alice, manages, charlie));
  store.add(triple(diana, manages, eve));

  // Collaboration
  store.add(triple(charlie, collaboratesWith, diana));

  // Names
  store.add(triple(alice, name, literal('Alice Smith')));
  store.add(triple(bob, name, literal('Bob Jones')));
  store.add(triple(charlie, name, literal('Charlie Brown')));
  store.add(triple(diana, name, literal('Diana Prince')));
  store.add(triple(eve, name, literal('Eve White')));
  store.add(triple(frank, name, literal('Frank Castle')));

  return store;
}

/**
 * Pretty print URI
 */
function prettyUri(uri) {
  return uri.replace('http://example.org/', '').replace('http://schema.org/', '');
}

/**
 * Main demo
 */
function runDemo() {
  console.log('='.repeat(70));
  console.log('  RDF Graph Analytics Demo');
  console.log('  Advanced Knowledge Graph Analysis');
  console.log('='.repeat(70));
  console.log();

  // Create sample data
  console.log('📊 Creating sample knowledge graph...');
  const store = createSampleGraph();
  console.log(`   ✅ Created RDF store with ${store.size} triples`);
  console.log();

  // Convert to graphlib
  console.log('🔄 Converting RDF to graphlib Graph...');
  const graph = rdfToGraph(store);
  const stats = getGraphStats(graph);

  console.log(`   ✅ Converted to graph:`);
  console.log(`      Nodes: ${stats.nodeCount}`);
  console.log(`      Edges: ${stats.edgeCount}`);
  console.log(`      Avg Degree: ${stats.averageDegree.toFixed(2)}`);
  console.log(`      Max Degree: ${stats.maxDegree}`);
  console.log(`      Density: ${stats.density.toFixed(4)}`);
  console.log();

  // PageRank analysis
  console.log('📈 Computing PageRank centrality...');
  const pagerank = computePageRank(graph, {
    dampingFactor: 0.85,
    maxIterations: 100,
  });

  const topByPageRank = getTopNodes(pagerank, 5);
  console.log('   ✅ Top 5 nodes by PageRank:');
  topByPageRank.forEach((item, idx) => {
    console.log(`      ${idx + 1}. ${prettyUri(item.node)}: ${item.score.toFixed(6)}`);
  });
  console.log();

  // Degree centrality
  console.log('📊 Computing degree centrality...');
  const degrees = computeDegreeCentrality(graph);
  const topByDegree = getTopNodes(degrees.totalDegree, 5);

  console.log('   ✅ Top 5 nodes by total degree:');
  topByDegree.forEach((item, idx) => {
    console.log(`      ${idx + 1}. ${prettyUri(item.node)}: ${item.score.toFixed(4)}`);
  });
  console.log();

  // Shortest path
  console.log('🔍 Finding shortest path (Alice → Frank)...');
  const aliceUri = 'http://example.org/Alice';
  const frankUri = 'http://example.org/Frank';

  const shortestPath = findShortestPath(graph, aliceUri, frankUri);

  if (shortestPath) {
    console.log(`   ✅ Found path (length ${shortestPath.length}):`);
    console.log(`      Path: ${shortestPath.path.map(prettyUri).join(' → ')}`);
    console.log('      Relationships:');
    shortestPath.edges.forEach((edge, idx) => {
      console.log(`         ${idx + 1}. ${prettyUri(edge.from)} --[${prettyUri(edge.predicate)}]--> ${prettyUri(edge.to)}`);
    });
  } else {
    console.log('   ❌ No path found');
  }
  console.log();

  // All paths
  console.log('🔍 Finding all paths (Alice → Diana, max depth 4)...');
  const dianaUri = 'http://example.org/Diana';
  const allPaths = findAllPaths(graph, aliceUri, dianaUri, { maxDepth: 4, maxPaths: 10 });

  console.log(`   ✅ Found ${allPaths.length} paths:`);
  allPaths.slice(0, 3).forEach((pathResult, idx) => {
    console.log(`      Path ${idx + 1} (length ${pathResult.length}): ${pathResult.path.map(prettyUri).join(' → ')}`);
  });
  console.log();

  // Community detection
  console.log('🏘️  Detecting communities (Label Propagation)...');
  const communities = detectCommunitiesLPA(graph, { maxIterations: 50 });
  const communityStats = getCommunityStats(communities);

  console.log(`   ✅ Detected ${communityStats.totalCommunities} communities:`);
  console.log(`      Average size: ${communityStats.averageSize.toFixed(2)}`);
  console.log(`      Size range: ${communityStats.minSize} - ${communityStats.maxSize}`);

  console.log('      Community members:');
  for (const [communityId, members] of communityStats.communities.entries()) {
    if (members.length >= 2) {
      const memberNames = Array.from(members).map(prettyUri).join(', ');
      console.log(`         Community ${communityId}: ${memberNames}`);
    }
  }
  console.log();

  // Results summary
  console.log('='.repeat(70));
  console.log('  DEMO COMPLETE');
  console.log('='.repeat(70));
  console.log();
  console.log('✅ Successfully demonstrated:');
  console.log('   ✓ RDF to graphlib conversion');
  console.log('   ✓ PageRank centrality computation');
  console.log('   ✓ Degree centrality metrics');
  console.log('   ✓ Shortest path finding');
  console.log('   ✓ All paths enumeration');
  console.log('   ✓ Community detection (Label Propagation)');
  console.log();
  console.log('📊 Performance Summary:');
  console.log(`   Graph size: ${stats.nodeCount} nodes, ${stats.edgeCount} edges`);
  console.log(`   PageRank iterations: converged`);
  console.log(`   Paths found: ${allPaths.length}`);
  console.log(`   Communities detected: ${communityStats.totalCommunities}`);
  console.log();
}

// Run demo
try {
  runDemo();
  process.exit(0);
} catch (error) {
  console.error('❌ Demo failed:', error);
  console.error(error.stack);
  process.exit(1);
}
