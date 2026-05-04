# @unrdf/spatial-kg

> WebXR-enabled 3D visualization and navigation of RDF knowledge graphs

## Overview

`@unrdf/spatial-kg` provides spatial knowledge graph capabilities with WebXR support for VR/AR experiences. Navigate knowledge graphs in 3D space using VR headsets, AR devices, or standard screens.

## Features

- **3D Force-Directed Layout**: Fruchterman-Reingold algorithm optimized for 3D
- **WebXR Support**: VR (Meta Quest, Vive, etc.) and AR (smartphone AR)
- **Spatial Queries**: Proximity, ray casting, k-NN, bounding box
- **Gesture Controls**: Point, grab, teleport, pinch gestures
- **Multi-User Collaboration**: Real-time state synchronization
- **LOD Optimization**: Distance-based level-of-detail for 60 FPS
- **OTEL Instrumentation**: Full observability

## Installation

```bash
pnpm add @unrdf/spatial-kg
```

## Quick Start

```javascript
import { SpatialKGEngine } from '@unrdf/spatial-kg';

// Create engine
const engine = new SpatialKGEngine({
  layout: {
    iterations: 100,
    repulsionStrength: 100,
    attractionStrength: 0.1,
  },
  rendering: {
    targetFPS: 60,
    enableVR: true,
  },
  lod: {
    enabled: true,
  },
});

// Initialize
await engine.initialize();

// Load graph
await engine.loadGraph({
  nodes: [
    { id: 'alice', label: 'Alice', position: { x: 0, y: 0, z: 0 } },
    { id: 'bob', label: 'Bob', position: { x: 10, y: 0, z: 0 } },
  ],
  edges: [
    { id: 'e1', source: 'alice', target: 'bob', predicate: 'knows' },
  ],
});

// Start rendering
engine.start();

// Spatial query
const nearby = engine.query({
  type: 'proximity',
  origin: { x: 0, y: 0, z: 0 },
  radius: 15,
});

console.log('Nearby nodes:', nearby);
```

## WebXR Setup

### VR Mode

```javascript
// Start VR session
await engine.startXR('immersive-vr');

// Register gesture handlers
engine.onGesture('select', (gesture) => {
  console.log('Node selected:', gesture.target);
});

engine.onGesture('teleport', (gesture) => {
  console.log('Teleport to:', gesture.position);
});
```

### AR Mode

```javascript
// Start AR session
await engine.startXR('immersive-ar');

// AR-specific gestures
engine.onGesture('pinch', (gesture) => {
  console.log('Pinch detected:', gesture.intensity);
});
```

## Spatial Queries

### Proximity Query

```javascript
const results = engine.query({
  type: 'proximity',
  origin: { x: 0, y: 0, z: 0 },
  radius: 20,
});
```

### Ray Casting

```javascript
const results = engine.query({
  type: 'ray',
  origin: { x: 0, y: 1.6, z: 0 },
  direction: { x: 1, y: 0, z: 0 },
  radius: 0.5,
});
```

### K-Nearest Neighbors

```javascript
const results = engine.query({
  type: 'knn',
  origin: { x: 5, y: 5, z: 5 },
  k: 10,
});
```

### Bounding Box

```javascript
const results = engine.query({
  type: 'box',
  bounds: {
    min: { x: -10, y: -10, z: -10 },
    max: { x: 10, y: 10, z: 10 },
  },
});
```

## Multi-User Collaboration

```javascript
const engine = new SpatialKGEngine({
  collaboration: {
    enabled: true,
    username: 'alice',
    serverUrl: 'wss://collab.example.com',
  },
});

await engine.initialize();

// Listen for remote users
engine.collaboration.on('user-state-updated', (user) => {
  console.log('User updated:', user.userId, user.position);
});

engine.collaboration.on('user-left', (event) => {
  console.log('User left:', event.userId);
});
```

## Performance Targets

| Operation | Target | Typical |
|-----------|--------|---------|
| Layout (1000 nodes) | <1s | ~0.5s |
| Render frame | <16ms (60 FPS) | ~10ms |
| Spatial query | <5ms | ~2ms |
| Gesture recognition | <10ms | ~3ms |
| LOD switch | <1ms | ~0.5ms |

## Performance Monitoring

```javascript
const metrics = engine.getMetrics();

console.log('FPS:', metrics.rendering.fps);
console.log('Frame time:', metrics.rendering.frameTime);
console.log('LOD stats:', metrics.lod);
console.log('Layout iteration:', metrics.layout.iteration);
```

## VR/AR Hardware Support

### VR Headsets
- Meta Quest 2/3/Pro
- HTC Vive/Vive Pro
- Valve Index
- PlayStation VR2
- Windows Mixed Reality

### AR Devices
- Smartphone AR (iOS/Android)
- HoloLens 2
- Magic Leap

## API Reference

### SpatialKGEngine

Main orchestration engine.

- `constructor(config)` - Create engine
- `initialize()` - Initialize renderer and components
- `loadGraph(data)` - Load graph data
- `start()` - Start rendering loop
- `stop()` - Stop rendering
- `query(options)` - Spatial query
- `startXR(mode)` - Start WebXR session
- `onGesture(type, callback)` - Register gesture handler
- `getMetrics()` - Get performance metrics
- `dispose()` - Cleanup resources

### Layout3D

3D force-directed layout.

- `addNode(node)` - Add node to layout
- `addEdge(edge)` - Add edge to layout
- `step()` - Execute one layout iteration
- `run(iterations)` - Run layout until convergence
- `getPositions()` - Get all node positions
- `reset()` - Reset layout

### SpatialQueryEngine

Spatial queries with octree indexing.

- `proximity(options)` - Find nodes within radius
- `rayCast(options)` - Ray casting query
- `kNearestNeighbors(options)` - K-NN query
- `box(options)` - Bounding box query
- `rebuild()` - Rebuild spatial index

### GestureController

VR/AR gesture recognition.

- `processInput(controllerId, state)` - Process controller input
- `on(gestureType, callback)` - Register listener
- `getLastGesture(controller, type)` - Get last gesture
- `simulateGesture(gesture)` - Simulate gesture (testing)

### LODManager

Level-of-detail optimization.

- `updateCamera(position)` - Update camera position
- `calculateLevel(node)` - Calculate LOD level
- `updateLevels(nodes)` - Update all node levels
- `getStats()` - Get LOD statistics
- `shouldRender(nodeId)` - Check render eligibility

### CollaborationManager

Multi-user synchronization.

- `connect()` - Connect to server
- `disconnect()` - Disconnect
- `updateLocalState(state)` - Update local user
- `receiveState(state)` - Receive remote state
- `getUserState(userId)` - Get user state
- `getAllUsers()` - Get all remote users
- `on(event, callback)` - Register event listener

## Testing

```bash
# Run tests
pnpm test

# Run with coverage
pnpm test:coverage

# Watch mode
pnpm test:watch
```

All tests must pass with 100% pass rate. Coverage target: 80%+.

## License

MIT
