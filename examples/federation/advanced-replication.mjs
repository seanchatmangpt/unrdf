/**
 * @fileoverview Advanced Replication Example
 *
 * Demonstrates advanced replication features including:
 * - Different replication topologies
 * - Conflict resolution strategies
 * - Version vectors
 * - Batch replication
 */

import {
  createFederatedSystem,
  ReplicationTopology,
  ConflictResolution
} from '../../src/knowledge-engine/federation/index.mjs';

async function demonstrateTopologies() {
  console.log('📊 Replication Topologies Demo\n');

  // Full Mesh Topology
  console.log('1. Full Mesh Topology:');
  console.log('   - Every store replicates to every other store');
  console.log('   - Highest consistency, highest network overhead');
  const meshFederation = await createFederatedSystem({
    federationId: 'mesh-federation',
    replicationTopology: ReplicationTopology.FULL_MESH
  });

  await meshFederation.registerStore({
    storeId: 'mesh-1',
    endpoint: 'http://localhost:4001'
  });
  await meshFederation.registerStore({
    storeId: 'mesh-2',
    endpoint: 'http://localhost:4002'
  });
  await meshFederation.registerStore({
    storeId: 'mesh-3',
    endpoint: 'http://localhost:4003'
  });

  await meshFederation.replicate({
    storeId: 'mesh-1',
    operation: 'INSERT',
    quad: {
      subject: 'http://example.org/entity1',
      predicate: 'http://example.org/property',
      object: '"value1"'
    }
  });

  console.log('   ✅ Data replicated to all stores in mesh\n');
  await meshFederation.shutdown();

  // Star Topology
  console.log('2. Star Topology:');
  console.log('   - Hub-and-spoke model');
  console.log('   - All replication goes through central hub');
  const starFederation = await createFederatedSystem({
    federationId: 'star-federation',
    replicationTopology: ReplicationTopology.STAR
  });

  await starFederation.registerStore({
    storeId: 'hub',
    endpoint: 'http://localhost:5001'
  });
  await starFederation.registerStore({
    storeId: 'spoke-1',
    endpoint: 'http://localhost:5002'
  });
  await starFederation.registerStore({
    storeId: 'spoke-2',
    endpoint: 'http://localhost:5003'
  });

  await starFederation.replicate({
    storeId: 'spoke-1',
    operation: 'INSERT',
    quad: {
      subject: 'http://example.org/entity2',
      predicate: 'http://example.org/property',
      object: '"value2"'
    }
  });

  console.log('   ✅ Data replicated through hub\n');
  await starFederation.shutdown();

  // Ring Topology
  console.log('3. Ring Topology:');
  console.log('   - Each store replicates to next store in ring');
  console.log('   - Eventual consistency with lower overhead');
  const ringFederation = await createFederatedSystem({
    federationId: 'ring-federation',
    replicationTopology: ReplicationTopology.RING
  });

  await ringFederation.registerStore({
    storeId: 'ring-1',
    endpoint: 'http://localhost:6001'
  });
  await ringFederation.registerStore({
    storeId: 'ring-2',
    endpoint: 'http://localhost:6002'
  });
  await ringFederation.registerStore({
    storeId: 'ring-3',
    endpoint: 'http://localhost:6003'
  });

  await ringFederation.replicate({
    storeId: 'ring-1',
    operation: 'INSERT',
    quad: {
      subject: 'http://example.org/entity3',
      predicate: 'http://example.org/property',
      object: '"value3"'
    }
  });

  console.log('   ✅ Data propagating through ring\n');
  await ringFederation.shutdown();
}

async function demonstrateConflictResolution() {
  console.log('⚔️  Conflict Resolution Strategies Demo\n');

  // Last Write Wins
  console.log('1. Last Write Wins:');
  const lwwFederation = await createFederatedSystem({
    federationId: 'lww-federation',
    conflictResolution: ConflictResolution.LAST_WRITE_WINS
  });

  await lwwFederation.registerStore({
    storeId: 'store-a',
    endpoint: 'http://localhost:7001'
  });
  await lwwFederation.registerStore({
    storeId: 'store-b',
    endpoint: 'http://localhost:7002'
  });

  // Simulate concurrent writes
  await Promise.all([
    lwwFederation.replicate({
      storeId: 'store-a',
      operation: 'UPDATE',
      quad: {
        subject: 'http://example.org/doc1',
        predicate: 'http://example.org/value',
        object: '"version-a"'
      }
    }),
    lwwFederation.replicate({
      storeId: 'store-b',
      operation: 'UPDATE',
      quad: {
        subject: 'http://example.org/doc1',
        predicate: 'http://example.org/value',
        object: '"version-b"'
      }
    })
  ]);

  console.log('   ✅ Conflict resolved using last write wins\n');
  await lwwFederation.shutdown();

  // Manual Resolution
  console.log('2. Manual Conflict Resolution:');
  const manualFederation = await createFederatedSystem({
    federationId: 'manual-federation',
    conflictResolution: ConflictResolution.MANUAL
  });

  await manualFederation.registerStore({
    storeId: 'store-c',
    endpoint: 'http://localhost:8001'
  });
  await manualFederation.registerStore({
    storeId: 'store-d',
    endpoint: 'http://localhost:8002'
  });

  // Listen for conflicts
  manualFederation.replication.on('conflict', (conflict) => {
    console.log('   📢 Conflict detected:', conflict);
    console.log('   👤 Manual intervention required');

    // Resolve manually (in production, this would involve user/admin decision)
    manualFederation.replication.emit(
      `conflict-resolved-${conflict.operation.changeId}`,
      true
    );
  });

  console.log('   ✅ Manual conflict resolution configured\n');
  await manualFederation.shutdown();
}

async function demonstrateBatchReplication() {
  console.log('📦 Batch Replication Demo\n');

  const federation = await createFederatedSystem({
    federationId: 'batch-federation',
    replicationTopology: ReplicationTopology.FULL_MESH
  });

  await federation.registerStore({
    storeId: 'batch-1',
    endpoint: 'http://localhost:9001'
  });
  await federation.registerStore({
    storeId: 'batch-2',
    endpoint: 'http://localhost:9002'
  });

  console.log('Replicating 100 changes in batch...');
  const startTime = Date.now();

  const promises = [];
  for (let i = 0; i < 100; i++) {
    promises.push(
      federation.replicate({
        storeId: 'batch-1',
        operation: 'INSERT',
        quad: {
          subject: `http://example.org/entity-${i}`,
          predicate: 'http://example.org/value',
          object: `"value-${i}"`
        }
      })
    );
  }

  await Promise.all(promises);

  const duration = Date.now() - startTime;
  console.log(`✅ Batch replication completed in ${duration}ms`);
  console.log(`   Average: ${(duration / 100).toFixed(2)}ms per change\n`);

  // Get replication stats
  const stats = federation.getStats();
  console.log('Replication Statistics:');
  console.log(`  Change Log Size: ${stats.replication.changeLogSize}`);
  console.log(`  Queue Size: ${stats.replication.queueSize}`);
  console.log(`  Conflicts: ${stats.replication.conflictCount}\n`);

  await federation.shutdown();
}

async function main() {
  console.log('🔄 UNRDF Federation - Advanced Replication Examples\n');

  await demonstrateTopologies();
  await demonstrateConflictResolution();
  await demonstrateBatchReplication();

  console.log('🎉 All advanced replication examples completed!');
}

// Run the example
main().catch(console.error);
