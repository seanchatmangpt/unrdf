/**
 * @file Distributed Queries Example
 * @description Demonstrates federated query execution, result aggregation, and fault handling
 */

import { createCoordinator } from '@unrdf/federation';

/**
 * Main example: Distributed Queries
 */
async function main() {
  console.log('=== UNRDF Federation: Distributed Queries Example ===\n');

  // Create federation coordinator with initial peers
  console.log('1. Creating Federation Coordinator...');
  const coordinator = createCoordinator({
    peers: [
      {
        id: 'movies-db',
        endpoint: 'http://localhost:8001/sparql',
        metadata: { dataset: 'movies', region: 'us-west' },
      },
      {
        id: 'books-db',
        endpoint: 'http://localhost:8002/sparql',
        metadata: { dataset: 'books', region: 'us-east' },
      },
      {
        id: 'music-db',
        endpoint: 'http://localhost:8003/sparql',
        metadata: { dataset: 'music', region: 'eu-west' },
      },
    ],
    strategy: 'broadcast', // Query all peers by default
    timeout: 30000, // 30 second timeout
  });
  console.log('✓ Coordinator created with 3 peers\n');

  // List available peers
  console.log('2. Federation Status...');
  const peers = coordinator.listPeers();
  console.log(`  Total peers: ${peers.length}`);
  for (const peer of peers) {
    console.log(`  - ${peer.id}: ${peer.status} (${peer.metadata?.dataset})`);
  }
  console.log();

  // Execute distributed query across all peers
  console.log('3. Executing Distributed Query (Broadcast)...');
  const query1 = `
    SELECT ?subject ?predicate ?object
    WHERE {
      ?subject ?predicate ?object .
    }
    LIMIT 10
  `;

  const result1 = await coordinator.query(query1);
  console.log(`  Success: ${result1.success}`);
  console.log(`  Total duration: ${result1.totalDuration}ms`);
  console.log(`  Successful peers: ${result1.successCount}/${result1.successCount + result1.failureCount}`);
  console.log(`  Results aggregated: ${result1.results.length} bindings`);

  // Show per-peer results
  console.log('\n  Per-Peer Results:');
  for (const peerResult of result1.peerResults) {
    console.log(`    ${peerResult.peerId}:`);
    console.log(`      Success: ${peerResult.success}`);
    console.log(`      Duration: ${peerResult.duration}ms`);
    if (peerResult.error) {
      console.log(`      Error: ${peerResult.error}`);
    }
  }
  console.log();

  // Query specific peer
  console.log('4. Querying Specific Peer...');
  const query2 = `
    SELECT ?title ?director
    WHERE {
      ?movie a :Movie ;
             :title ?title ;
             :director ?director .
    }
    LIMIT 5
  `;

  const result2 = await coordinator.queryPeer('movies-db', query2);
  console.log(`  Peer: ${result2.peerId}`);
  console.log(`  Success: ${result2.success}`);
  console.log(`  Duration: ${result2.duration}ms`);
  if (result2.error) {
    console.log(`  Error: ${result2.error}`);
  }
  console.log();

  // Handle peer failures gracefully
  console.log('5. Handling Peer Failures...');
  console.log('  Adding unreachable peer...');
  await coordinator.addPeer('unreachable-db', 'http://localhost:9999/sparql', {
    dataset: 'unavailable',
  });

  const query3 = `
    SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 1
  `;

  const result3 = await coordinator.query(query3);
  console.log(`  Query completed despite failures`);
  console.log(`  Success: ${result3.success}`);
  console.log(`  Successful peers: ${result3.successCount}`);
  console.log(`  Failed peers: ${result3.failureCount}`);
  console.log('  ✓ Federation continues to work with partial failures\n');

  // Aggregating results from multiple peers
  console.log('6. Result Aggregation...');
  const query4 = `
    SELECT ?type (COUNT(?item) as ?count)
    WHERE {
      ?item a ?type .
    }
    GROUP BY ?type
  `;

  const result4 = await coordinator.query(query4, {
    strategy: 'broadcast', // Query all healthy peers
  });

  console.log(`  Results from ${result4.successCount} peers aggregated`);
  console.log(`  Total unique bindings: ${result4.results.length}`);
  console.log('  ✓ Duplicates removed during aggregation\n');

  // Selective routing strategy
  console.log('7. Selective Query Routing...');
  const result5 = await coordinator.query(query1, {
    strategy: 'first-available', // Only query first healthy peer
  });

  console.log(`  Strategy: first-available`);
  console.log(`  Peers queried: ${result5.peerResults.length}`);
  console.log(`  Duration: ${result5.totalDuration}ms (faster than broadcast)`);
  console.log();

  // Health checks
  console.log('8. Federation Health Check...');
  const healthCheck = await coordinator.healthCheck();
  console.log(`  Total peers: ${healthCheck.totalPeers}`);
  console.log(`  Healthy: ${healthCheck.healthyPeers}`);
  console.log(`  Degraded: ${healthCheck.degradedPeers}`);
  console.log(`  Unreachable: ${healthCheck.unreachablePeers}`);
  console.log();

  // Get federation statistics
  console.log('9. Federation Statistics...');
  const stats = coordinator.getStats();
  console.log(`  Total peers: ${stats.totalPeers}`);
  console.log(`  Healthy peers: ${stats.healthyPeers}`);
  console.log(`  Total queries executed: ${stats.totalQueries}`);
  console.log(`  Total errors: ${stats.totalErrors}`);
  console.log(`  Error rate: ${(stats.errorRate * 100).toFixed(2)}%`);
  console.log();

  // Synchronize graph updates (conceptual)
  console.log('10. Graph Synchronization Protocol...');
  console.log('  Conceptual workflow for updating federated graphs:');
  console.log('  1. Broadcast INSERT/DELETE query to all peers');
  console.log('  2. Wait for acknowledgment from majority (quorum)');
  console.log('  3. Commit changes or rollback on failure');
  console.log('  4. Update peer status based on sync success');
  console.log('  ✓ Ensures consistency across federation\n');

  console.log('=== Example Complete ===');
}

// Run example
main().catch(console.error);

export { main };
