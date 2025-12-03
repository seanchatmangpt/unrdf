/**
 * @unrdf/federation - Basic Example
 *
 * Demonstrates basic usage of the federation package.
 */

import { createCoordinator } from '../src/index.mjs';

async function main() {
  console.log('UNRDF Federation - Basic Example\n');

  // Create a federation coordinator with initial peers
  const coordinator = createCoordinator({
    peers: [
      {
        id: 'dbpedia',
        endpoint: 'https://dbpedia.org/sparql',
        metadata: { description: 'DBpedia SPARQL endpoint' },
      },
      {
        id: 'wikidata',
        endpoint: 'https://query.wikidata.org/sparql',
        metadata: { description: 'Wikidata Query Service' },
      },
    ],
    strategy: 'broadcast', // Query all peers
    timeout: 10000, // 10 second timeout
  });

  // List registered peers
  console.log('Registered Peers:');
  const peers = coordinator.listPeers();
  peers.forEach(peer => {
    console.log(`  - ${peer.id}: ${peer.endpoint} (${peer.status})`);
  });
  console.log();

  // Add a new peer dynamically
  console.log('Adding new peer...');
  await coordinator.addPeer(
    'local',
    'http://localhost:3030/dataset/sparql',
    { description: 'Local Fuseki instance' }
  );
  console.log('Peer added.\n');

  // Execute a federated query
  // Note: This is a demo query - actual endpoints may require authentication
  const sparqlQuery = `
    SELECT DISTINCT ?type WHERE {
      ?s a ?type .
    } LIMIT 10
  `;

  console.log('Executing federated query...');
  console.log(`Query: ${sparqlQuery.trim()}\n`);

  try {
    const result = await coordinator.query(sparqlQuery, {
      strategy: 'selective', // Override to selective routing
      timeout: 5000,
    });

    console.log('Query Results:');
    console.log(`  Success: ${result.success}`);
    console.log(`  Successful peers: ${result.successCount}`);
    console.log(`  Failed peers: ${result.failureCount}`);
    console.log(`  Total duration: ${result.totalDuration}ms`);
    console.log(`  Results count: ${result.results.length}`);

    if (result.results.length > 0) {
      console.log('\n  Sample results:');
      result.results.slice(0, 5).forEach((binding, i) => {
        console.log(`    ${i + 1}. ${JSON.stringify(binding)}`);
      });
    }

    console.log('\n  Per-peer results:');
    result.peerResults.forEach(pr => {
      console.log(`    - ${pr.peerId}: ${pr.success ? 'Success' : 'Failed'} (${pr.duration}ms)`);
      if (pr.error) {
        console.log(`      Error: ${pr.error}`);
      }
    });
  } catch (error) {
    console.error('Query failed:', error.message);
  }

  console.log();

  // Query a specific peer
  console.log('Querying specific peer (dbpedia)...');
  try {
    const peerResult = await coordinator.queryPeer('dbpedia', sparqlQuery);
    console.log(`  Success: ${peerResult.success}`);
    console.log(`  Duration: ${peerResult.duration}ms`);
    if (peerResult.error) {
      console.log(`  Error: ${peerResult.error}`);
    }
  } catch (error) {
    console.error('Peer query failed:', error.message);
  }

  console.log();

  // Run health checks
  console.log('Running health checks...');
  const health = await coordinator.healthCheck();
  console.log(`  Total peers: ${health.totalPeers}`);
  console.log(`  Healthy: ${health.healthyPeers}`);
  console.log(`  Degraded: ${health.degradedPeers}`);
  console.log(`  Unreachable: ${health.unreachablePeers}`);

  console.log();

  // Get federation statistics
  const stats = coordinator.getStats();
  console.log('Federation Statistics:');
  console.log(`  Total queries: ${stats.totalQueries}`);
  console.log(`  Total errors: ${stats.totalErrors}`);
  console.log(`  Error rate: ${(stats.errorRate * 100).toFixed(2)}%`);

  console.log();

  // Remove a peer
  console.log('Removing peer (local)...');
  const removed = coordinator.removePeer('local');
  console.log(`  Removed: ${removed}`);

  console.log('\nExample complete.');
}

// Run the example
main().catch(console.error);
