/**
 * Composition C20: RDF to Graph + PageRank + Community Detector
 * Atoms: A25 + A26 + A28
 *
 * Proof: Full graph analytics pipeline
 */

import { createStore, namedNode, literal, quad } from '@unrdf/core';
import { rdfToGraph, computePageRank, detectCommunitiesLPA, getTopNodes } from '@unrdf/graph-analytics';

console.log('=== C20: Graph Analytics Pipeline Proof ===\n');

async function prove() {
  try {
    // Create RDF store with social network
    const store = createStore();

    const alice = namedNode('http://example.org/alice');
    const bob = namedNode('http://example.org/bob');
    const carol = namedNode('http://example.org/carol');
    const dave = namedNode('http://example.org/dave');
    const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    // Names
    store.add(quad(alice, name, literal('Alice')));
    store.add(quad(bob, name, literal('Bob')));
    store.add(quad(carol, name, literal('Carol')));
    store.add(quad(dave, name, literal('Dave')));

    // Social connections
    store.add(quad(alice, knows, bob));
    store.add(quad(alice, knows, carol));
    store.add(quad(bob, knows, carol));
    store.add(quad(carol, knows, dave));

    console.log('✅ Created RDF social network (4 people, 4 connections)');

    // A25: Convert RDF to graph
    const quads = Array.from(store.match());
    const graph = rdfToGraph(quads, {
      edgePredicate: 'http://xmlns.com/foaf/0.1/knows'
    });

    console.log('✅ A25: Converted RDF to graph structure');
    console.log(`   Nodes: ${graph.nodeCount()}`);
    console.log(`   Edges: ${graph.edgeCount()}`);

    // A26: Compute PageRank
    const pagerank = computePageRank(graph, {
      iterations: 20,
      dampingFactor: 0.85
    });

    console.log('\n✅ A26: PageRank computed');
    const topNodes = getTopNodes(pagerank, 3);
    console.log('   Top 3 influential nodes:');
    for (const [node, score] of topNodes) {
      console.log(`   - ${node}: ${score.toFixed(4)}`);
    }

    // A28: Detect communities
    const communities = detectCommunitiesLPA(graph, {
      maxIterations: 10
    });

    console.log('\n✅ A28: Communities detected');
    console.log(`   Number of communities: ${communities.size}`);

    const communityMap = new Map();
    for (const [node, communityId] of communities.entries()) {
      if (!communityMap.has(communityId)) {
        communityMap.set(communityId, []);
      }
      communityMap.get(communityId).push(node);
    }

    let i = 1;
    for (const [communityId, members] of communityMap.entries()) {
      console.log(`   Community ${i}: ${members.join(', ')}`);
      i++;
    }

    console.log('\n✅ COMPOSITION VERIFIED');
    console.log('   Value: Full graph analytics on RDF data');
    console.log(`   PageRank computed: ${pagerank.size} nodes ranked`);
    console.log(`   Communities found: ${communityMap.size}`);
    console.log('   Use case: Social network analysis, influence detection');

    process.exit(pagerank.size > 0 && communities.size > 0 ? 0 : 1);
  } catch (error) {
    console.error('❌ COMPOSITION FAILED:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

prove();
