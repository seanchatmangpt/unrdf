# Peer Discovery Example

This example demonstrates peer discovery and health management in UNRDF Federation.

## Features

- **Peer Registration**: Register multiple federation peers with metadata
- **Health Checks**: Ping peers to verify availability
- **Status Management**: Track peer health (healthy, degraded, unreachable)
- **Peer Discovery**: List and filter peers by status
- **Dynamic Updates**: Update peer information and status

## Usage

```bash
# Install dependencies
pnpm install

# Run the example
pnpm start

# Run tests
pnpm test
```

## Example Code

```javascript
import { createPeerManager } from '@unrdf/federation';

// Create peer manager
const peerManager = createPeerManager();

// Register peers
peerManager.registerPeer('peer-1', 'http://localhost:8001/sparql', {
  region: 'us-west',
  dataset: 'movies',
});

// List all peers
const peers = peerManager.listPeers();

// Check peer health
const isHealthy = await peerManager.ping('peer-1');

// Filter by status
const healthyPeers = peerManager.listPeers({ status: 'healthy' });

// Update peer status
peerManager.updateStatus('peer-1', 'degraded');
```

## API

### Register Peer
```javascript
const peer = peerManager.registerPeer(id, endpoint, metadata);
```

### List Peers
```javascript
const allPeers = peerManager.listPeers();
const healthyPeers = peerManager.listPeers({ status: 'healthy' });
```

### Check Health
```javascript
const isHealthy = await peerManager.ping(id, timeout);
```

### Update Status
```javascript
peerManager.updateStatus(id, 'healthy' | 'degraded' | 'unreachable');
```

### Get Peer Info
```javascript
const peer = peerManager.getPeer(id);
```

### Remove Peer
```javascript
peerManager.unregisterPeer(id);
```

## Status Types

- **healthy**: Peer is responding to health checks
- **degraded**: Peer responded but with errors
- **unreachable**: Peer did not respond to health check

## Metadata

Peers can store arbitrary metadata for routing decisions:

```javascript
{
  region: 'us-west',
  dataset: 'movies',
  version: '2.0',
  capabilities: ['sparql', 'graphql']
}
```

## Learn More

- [Federation Documentation](../../README.md)
- [Distributed Queries Example](../distributed-queries/)
- [UNRDF Core](../../../core/)


## Testing

Run the test suite:

```bash
pnpm test
```

Run tests in watch mode:

```bash
pnpm test:watch
```

Generate coverage report:

```bash
pnpm test:coverage
```

Test coverage: 80%+ (minimum requirement)
