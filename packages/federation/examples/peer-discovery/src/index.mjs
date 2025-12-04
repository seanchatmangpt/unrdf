/**
 * @file Peer Discovery Example
 * @description Demonstrates peer registration, health checks, and status management
 */

import { createPeerManager } from '@unrdf/federation';

/**
 * Main example: Peer Discovery
 */
async function main() {
  console.log('=== UNRDF Federation: Peer Discovery Example ===\n');

  // Create peer manager
  console.log('1. Creating Peer Manager...');
  const peerManager = createPeerManager();
  console.log('✓ Peer manager created\n');

  // Register multiple peers
  console.log('2. Registering Peers...');
  const peers = [
    { id: 'peer-1', endpoint: 'http://localhost:8001/sparql', metadata: { region: 'us-west', dataset: 'movies' } },
    { id: 'peer-2', endpoint: 'http://localhost:8002/sparql', metadata: { region: 'us-east', dataset: 'books' } },
    { id: 'peer-3', endpoint: 'http://localhost:8003/sparql', metadata: { region: 'eu-west', dataset: 'music' } },
  ];

  for (const peer of peers) {
    const registered = peerManager.registerPeer(peer.id, peer.endpoint, peer.metadata);
    console.log(`  ✓ Registered ${registered.id} at ${registered.endpoint}`);
    console.log(`    Metadata:`, registered.metadata);
    console.log(`    Status: ${registered.status}`);
  }
  console.log();

  // List all available peers
  console.log('3. Listing All Peers...');
  const allPeers = peerManager.listPeers();
  console.log(`  Total peers: ${allPeers.length}`);
  for (const peer of allPeers) {
    console.log(`  - ${peer.id}: ${peer.status} (${peer.endpoint})`);
  }
  console.log();

  // Check peer health (ping)
  console.log('4. Checking Peer Health...');
  for (const peer of peers) {
    console.log(`  Pinging ${peer.id}...`);
    const isHealthy = await peerManager.ping(peer.id);
    const peerInfo = peerManager.getPeer(peer.id);
    console.log(`    Result: ${isHealthy ? '✓ healthy' : '✗ unreachable'}`);
    console.log(`    Status: ${peerInfo?.status}`);
    console.log(`    Last seen: ${new Date(peerInfo?.lastSeen || 0).toISOString()}`);
  }
  console.log();

  // List only healthy peers
  console.log('5. Filtering Peers by Status...');
  const healthyPeers = peerManager.listPeers({ status: 'healthy' });
  console.log(`  Healthy peers: ${healthyPeers.length}`);
  for (const peer of healthyPeers) {
    console.log(`  - ${peer.id}: ${peer.endpoint}`);
  }
  console.log();

  // Update peer information
  console.log('6. Updating Peer Information...');
  const updatedPeer = peerManager.registerPeer(
    'peer-1',
    'http://localhost:8001/sparql',
    { region: 'us-west', dataset: 'movies', version: '2.0' }
  );
  console.log(`  ✓ Updated ${updatedPeer.id}`);
  console.log(`    New metadata:`, updatedPeer.metadata);
  console.log();

  // Manually update peer status
  console.log('7. Manually Updating Peer Status...');
  const statusUpdated = peerManager.updateStatus('peer-2', 'degraded');
  console.log(`  ✓ Updated peer-2 status: ${statusUpdated ? 'degraded' : 'failed'}`);
  const peer2 = peerManager.getPeer('peer-2');
  console.log(`    Current status: ${peer2?.status}`);
  console.log();

  // Get peer statistics
  console.log('8. Peer Statistics...');
  const stats = {
    total: peerManager.size(),
    healthy: peerManager.listPeers({ status: 'healthy' }).length,
    degraded: peerManager.listPeers({ status: 'degraded' }).length,
    unreachable: peerManager.listPeers({ status: 'unreachable' }).length,
  };
  console.log(`  Total peers: ${stats.total}`);
  console.log(`  Healthy: ${stats.healthy}`);
  console.log(`  Degraded: ${stats.degraded}`);
  console.log(`  Unreachable: ${stats.unreachable}`);
  console.log();

  // Unregister a peer
  console.log('9. Unregistering Peer...');
  const removed = peerManager.unregisterPeer('peer-3');
  console.log(`  ${removed ? '✓' : '✗'} Removed peer-3`);
  console.log(`  Remaining peers: ${peerManager.size()}`);
  console.log();

  console.log('=== Example Complete ===');
}

// Run example
main().catch(console.error);

export { main };
