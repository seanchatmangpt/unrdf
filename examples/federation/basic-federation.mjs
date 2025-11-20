/**
 * @fileoverview Basic Federation Example
 *
 * Demonstrates how to set up a basic federated RDF system with
 * multiple stores, execute queries, and replicate data.
 */

import { createFederatedSystem } from '../../src/knowledge-engine/federation/index.mjs';

async function main() {
  console.log('🚀 UNRDF Federation - Basic Example\n');

  // Create a federated system
  console.log('Step 1: Creating federated system...');
  const federation = await createFederatedSystem({
    federationId: 'example-federation',
    enableConsensus: true,
    replicationTopology: 'full-mesh',
    conflictResolution: 'last-write-wins',
    loadBalancingStrategy: 'weighted'
  });
  console.log('✅ Federation created\n');

  // Register stores
  console.log('Step 2: Registering RDF stores...');
  await federation.registerStore({
    storeId: 'store-1',
    endpoint: 'http://localhost:3001',
    name: 'Primary Store',
    capabilities: ['sparql-1.1', 'update'],
    weight: 1.0
  });

  await federation.registerStore({
    storeId: 'store-2',
    endpoint: 'http://localhost:3002',
    name: 'Secondary Store',
    capabilities: ['sparql-1.1'],
    weight: 0.8
  });

  await federation.registerStore({
    storeId: 'store-3',
    endpoint: 'http://localhost:3003',
    name: 'Tertiary Store',
    capabilities: ['sparql-1.1'],
    weight: 0.6
  });
  console.log('✅ Registered 3 stores\n');

  // Wait for health checks
  await new Promise(resolve => setTimeout(resolve, 1000));

  // Execute a federated query
  console.log('Step 3: Executing federated SPARQL query...');
  try {
    const results = await federation.query(`
      SELECT ?person ?name ?age WHERE {
        ?person <http://xmlns.com/foaf/0.1/name> ?name .
        ?person <http://example.org/age> ?age .
        FILTER(?age > 18)
      }
      LIMIT 10
    `);

    console.log(`✅ Query returned ${results.length} results:`);
    results.slice(0, 5).forEach((result, i) => {
      console.log(`  ${i + 1}. ${JSON.stringify(result)}`);
    });
    console.log();
  } catch (error) {
    console.error('❌ Query failed:', error.message);
  }

  // Replicate data across federation
  console.log('Step 4: Replicating data across stores...');
  await federation.replicate({
    storeId: 'store-1',
    operation: 'INSERT',
    quad: {
      subject: 'http://example.org/alice',
      predicate: 'http://xmlns.com/foaf/0.1/name',
      object: '"Alice"'
    }
  });

  await federation.replicate({
    storeId: 'store-1',
    operation: 'INSERT',
    quad: {
      subject: 'http://example.org/alice',
      predicate: 'http://example.org/age',
      object: '"25"^^<http://www.w3.org/2001/XMLSchema#integer>'
    }
  });
  console.log('✅ Data replicated\n');

  // Get federation statistics
  console.log('Step 5: Getting federation statistics...');
  const stats = federation.getStats();
  console.log('Federation Stats:');
  console.log(`  Total Stores: ${stats.coordinator.totalStores}`);
  console.log(`  Load Balancing: ${stats.coordinator.loadBalancingStrategy}`);
  console.log(`  Replication Queue: ${stats.replication.queueSize}`);
  console.log(`  Change Log Size: ${stats.replication.changeLogSize}`);
  console.log();

  // Cleanup
  console.log('Step 6: Shutting down federation...');
  await federation.shutdown();
  console.log('✅ Federation shutdown complete\n');

  console.log('🎉 Example completed successfully!');
}

// Run the example
main().catch(console.error);
