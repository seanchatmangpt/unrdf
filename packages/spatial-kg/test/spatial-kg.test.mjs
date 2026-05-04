/**
 * @file Spatial Knowledge Graph Tests
 * @description Comprehensive test suite for spatial-kg package
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { SpatialKGEngine } from '../src/spatial-kg-engine.mjs';
import { Layout3D } from '../src/layout-3d.mjs';
import { SpatialQueryEngine } from '../src/spatial-query.mjs';
import { GestureController } from '../src/gesture-controller.mjs';
import { LODManager } from '../src/lod-manager.mjs';
import { CollaborationManager } from '../src/collaboration.mjs';
import { WebXRRenderer } from '../src/webxr-renderer.mjs';

describe('Layout3D', () => {
  let layout;

  beforeEach(() => {
    layout = new Layout3D({
      iterations: 50,
      cooldown: 0.95,
      repulsionStrength: 100,
      attractionStrength: 0.1,
    });
  });

  it('should initialize with default config', () => {
    expect(layout).toBeDefined();
    expect(layout.nodes.size).toBe(0);
    expect(layout.edges.length).toBe(0);
  });

  it('should add nodes', () => {
    layout.addNode({
      id: 'node1',
      position: { x: 0, y: 0, z: 0 },
    });

    expect(layout.nodes.size).toBe(1);
    expect(layout.nodes.has('node1')).toBe(true);
  });

  it('should add edges', () => {
    layout.addNode({ id: 'n1', position: { x: 0, y: 0, z: 0 } });
    layout.addNode({ id: 'n2', position: { x: 10, y: 0, z: 0 } });

    layout.addEdge({
      id: 'e1',
      source: 'n1',
      target: 'n2',
    });

    expect(layout.edges.length).toBe(1);
  });

  it('should execute layout step', () => {
    layout.addNode({ id: 'n1', position: { x: 0, y: 0, z: 0 } });
    layout.addNode({ id: 'n2', position: { x: 1, y: 0, z: 0 } });

    const displacement = layout.step();
    expect(displacement).toBeGreaterThan(0);
  });

  it('should converge with iterations', async () => {
    for (let i = 0; i < 10; i++) {
      layout.addNode({
        id: `n${i}`,
        position: { x: Math.random() * 10, y: Math.random() * 10, z: Math.random() * 10 },
      });
    }

    for (let i = 0; i < 9; i++) {
      layout.addEdge({
        id: `e${i}`,
        source: `n${i}`,
        target: `n${i + 1}`,
      });
    }

    const displacement = await layout.run(100);
    expect(displacement).toBeLessThan(10);
    expect(layout.iteration).toBeGreaterThan(0);
  });

  it('should respect fixed nodes', () => {
    layout.addNode({
      id: 'fixed',
      position: { x: 0, y: 0, z: 0 },
      fixed: true,
    });

    const initialPos = { ...layout.nodes.get('fixed').position };

    layout.step();

    const finalPos = layout.nodes.get('fixed').position;
    expect(finalPos.x).toBe(initialPos.x);
    expect(finalPos.y).toBe(initialPos.y);
    expect(finalPos.z).toBe(initialPos.z);
  });

  it('should get node positions', () => {
    layout.addNode({ id: 'n1', position: { x: 5, y: 10, z: 15 } });

    const positions = layout.getPositions();
    expect(positions.size).toBe(1);
    expect(positions.get('n1')).toEqual({ x: 5, y: 10, z: 15 });
  });

  it('should reset layout', () => {
    layout.addNode({ id: 'n1', position: { x: 0, y: 0, z: 0 } });
    layout.step();

    layout.reset();

    expect(layout.iteration).toBe(0);
    expect(layout.temperature).toBe(1.0);
  });
});

describe('SpatialQueryEngine', () => {
  let engine;
  let nodes;

  beforeEach(() => {
    nodes = new Map([
      ['n1', { id: 'n1', position: { x: 0, y: 0, z: 0 } }],
      ['n2', { id: 'n2', position: { x: 5, y: 0, z: 0 } }],
      ['n3', { id: 'n3', position: { x: 10, y: 0, z: 0 } }],
      ['n4', { id: 'n4', position: { x: 0, y: 10, z: 0 } }],
      ['n5', { id: 'n5', position: { x: 0, y: 0, z: 10 } }],
    ]);

    engine = new SpatialQueryEngine(nodes);
  });

  it('should find nodes by proximity', () => {
    const results = engine.proximity({
      origin: { x: 0, y: 0, z: 0 },
      radius: 6,
    });

    expect(results.length).toBe(2); // n1 and n2
    expect(results.find(n => n.id === 'n1')).toBeDefined();
    expect(results.find(n => n.id === 'n2')).toBeDefined();
  });

  it('should perform ray casting', () => {
    const results = engine.rayCast({
      origin: { x: 0, y: 0, z: 0 },
      direction: { x: 1, y: 0, z: 0 },
      radius: 1,
    });

    expect(results.length).toBeGreaterThan(0);
    expect(results.find(n => n.id === 'n2' || n.id === 'n3')).toBeDefined();
  });

  it('should find k-nearest neighbors', () => {
    const results = engine.kNearestNeighbors({
      origin: { x: 0, y: 0, z: 0 },
      k: 3,
    });

    expect(results.length).toBe(3);
    expect(results[0].id).toBe('n1'); // Closest
  });

  it('should query box region', () => {
    const results = engine.box({
      bounds: {
        min: { x: -1, y: -1, z: -1 },
        max: { x: 6, y: 1, z: 1 },
      },
    });

    expect(results.length).toBe(2); // n1 and n2
  });

  it('should rebuild spatial index', () => {
    engine.rebuild();
    expect(engine.octree).toBeDefined();
  });
});

describe('GestureController', () => {
  let controller;

  beforeEach(() => {
    controller = new GestureController();
  });

  it('should detect select gesture', () => {
    const gestures = controller.processInput('right', {
      position: { x: 0, y: 1, z: 0 },
      buttons: { trigger: 1.0 },
    });

    expect(gestures.length).toBe(1);
    expect(gestures[0].type).toBe('select');
  });

  it('should detect grab gesture', () => {
    const gestures = controller.processInput('left', {
      position: { x: 1, y: 1, z: 1 },
      buttons: { grip: 0.95 },
    });

    expect(gestures.length).toBe(1);
    expect(gestures[0].type).toBe('grab');
    expect(gestures[0].intensity).toBe(0.95);
  });

  it('should detect teleport gesture', () => {
    const gestures = controller.processInput('right', {
      position: { x: 0, y: 0, z: 0 },
      buttons: { thumbstick: 1 },
    });

    expect(gestures.length).toBe(1);
    expect(gestures[0].type).toBe('teleport');
  });

  it('should detect pinch from hand tracking', () => {
    const gestures = controller.processInput('hand-right', {
      position: { x: 0, y: 1, z: 0 },
      handPose: {
        joints: {
          'thumb-tip': { position: { x: 0, y: 1, z: 0 } },
          'index-finger-tip': { position: { x: 0.01, y: 1, z: 0 } },
        },
      },
    });

    expect(gestures.length).toBe(1);
    expect(gestures[0].type).toBe('pinch');
  });

  it('should register event listeners', () => {
    const callback = vi.fn();
    const unsubscribe = controller.on('select', callback);

    controller.simulateGesture({
      type: 'select',
      controller: 'right',
      position: { x: 0, y: 0, z: 0 },
      timestamp: Date.now(),
    });

    expect(callback).toHaveBeenCalled();

    unsubscribe();
    controller.simulateGesture({
      type: 'select',
      controller: 'right',
      position: { x: 0, y: 0, z: 0 },
      timestamp: Date.now(),
    });

    expect(callback).toHaveBeenCalledTimes(1);
  });

  it('should get last gesture', () => {
    controller.simulateGesture({
      type: 'grab',
      controller: 'left',
      position: { x: 1, y: 1, z: 1 },
      timestamp: Date.now(),
    });

    const last = controller.getLastGesture('left', 'grab');
    expect(last).toBeDefined();
    expect(last.type).toBe('grab');
  });
});

describe('LODManager', () => {
  let lod;

  beforeEach(() => {
    lod = new LODManager({
      enabled: true,
      levels: [
        { distance: 10, complexity: 'high' },
        { distance: 50, complexity: 'medium' },
        { distance: 100, complexity: 'low' },
      ],
    });
  });

  it('should calculate LOD level by distance', () => {
    lod.updateCamera({ x: 0, y: 0, z: 0 });

    const nearNode = { id: 'near', position: { x: 5, y: 0, z: 0 } };
    const midNode = { id: 'mid', position: { x: 30, y: 0, z: 0 } };
    const farNode = { id: 'far', position: { x: 80, y: 0, z: 0 } };

    expect(lod.calculateLevel(nearNode)).toBe(0); // high
    expect(lod.calculateLevel(midNode)).toBe(1); // medium
    expect(lod.calculateLevel(farNode)).toBe(2); // low
  });

  it('should update all node levels', () => {
    lod.updateCamera({ x: 0, y: 0, z: 0 });

    const nodes = new Map([
      ['n1', { id: 'n1', position: { x: 5, y: 0, z: 0 } }],
      ['n2', { id: 'n2', position: { x: 30, y: 0, z: 0 } }],
    ]);

    const changes = lod.updateLevels(nodes);
    expect(changes.length).toBe(2);
  });

  it('should get nodes at level', () => {
    lod.updateCamera({ x: 0, y: 0, z: 0 });

    const nodes = new Map([
      ['n1', { id: 'n1', position: { x: 5, y: 0, z: 0 } }],
      ['n2', { id: 'n2', position: { x: 30, y: 0, z: 0 } }],
    ]);

    lod.updateLevels(nodes);

    const highDetail = lod.getNodesAtLevel(0);
    const mediumDetail = lod.getNodesAtLevel(1);

    expect(highDetail.length).toBe(1);
    expect(mediumDetail.length).toBe(1);
  });

  it('should get LOD statistics', () => {
    lod.updateCamera({ x: 0, y: 0, z: 0 });

    const nodes = new Map([
      ['n1', { id: 'n1', position: { x: 5, y: 0, z: 0 } }],
      ['n2', { id: 'n2', position: { x: 30, y: 0, z: 0 } }],
      ['n3', { id: 'n3', position: { x: 80, y: 0, z: 0 } }],
    ]);

    lod.updateLevels(nodes);

    const stats = lod.getStats();
    expect(stats.enabled).toBe(true);
    expect(stats.total).toBe(3);
    expect(stats.levels[0].count).toBe(1);
  });

  it('should determine render eligibility', () => {
    lod.updateCamera({ x: 0, y: 0, z: 0 });

    const nearNode = { id: 'near', position: { x: 5, y: 0, z: 0 } };
    lod.updateLevels(new Map([['near', nearNode]]));

    expect(lod.shouldRender('near')).toBe(true);
  });

  it('should reset LOD state', () => {
    lod.updateCamera({ x: 10, y: 10, z: 10 });
    lod.reset();

    expect(lod.cameraPosition).toEqual({ x: 0, y: 0, z: 0 });
    expect(lod.nodeLevels.size).toBe(0);
  });
});

describe('CollaborationManager', () => {
  let collab;

  beforeEach(() => {
    collab = new CollaborationManager({
      userId: 'user1',
    });
  });

  it('should initialize with user ID', () => {
    expect(collab.userId).toBe('user1');
    expect(collab.isConnected()).toBe(false);
  });

  it('should connect to collaboration', async () => {
    await collab.connect();
    expect(collab.isConnected()).toBe(true);
  });

  it('should update local state', async () => {
    await collab.connect();

    collab.updateLocalState({
      position: { x: 1, y: 2, z: 3 },
      rotation: { x: 0, y: 0, z: 0, w: 1 },
    });

    expect(collab.localState).toBeDefined();
    expect(collab.localState.position).toEqual({ x: 1, y: 2, z: 3 });
  });

  it('should receive remote user state', async () => {
    await collab.connect();

    collab.receiveState({
      userId: 'user2',
      position: { x: 5, y: 5, z: 5 },
      rotation: { x: 0, y: 0, z: 0, w: 1 },
      timestamp: Date.now(),
    });

    const user2 = collab.getUserState('user2');
    expect(user2).toBeDefined();
    expect(user2.userId).toBe('user2');
  });

  it('should get all remote users', async () => {
    await collab.connect();

    collab.receiveState({
      userId: 'user2',
      position: { x: 0, y: 0, z: 0 },
      rotation: { x: 0, y: 0, z: 0, w: 1 },
      timestamp: Date.now(),
    });

    const users = collab.getAllUsers();
    expect(users.length).toBe(1);
  });

  it('should remove user', async () => {
    await collab.connect();

    collab.receiveState({
      userId: 'user2',
      position: { x: 0, y: 0, z: 0 },
      rotation: { x: 0, y: 0, z: 0, w: 1 },
      timestamp: Date.now(),
    });

    collab.removeUser('user2');
    expect(collab.getUserState('user2')).toBeNull();
  });

  it('should emit events', async () => {
    await collab.connect();

    const callback = vi.fn();
    collab.on('user-state-updated', callback);

    collab.receiveState({
      userId: 'user3',
      position: { x: 0, y: 0, z: 0 },
      rotation: { x: 0, y: 0, z: 0, w: 1 },
      timestamp: Date.now(),
    });

    expect(callback).toHaveBeenCalled();
  });

  it('should disconnect', async () => {
    await collab.connect();
    expect(collab.isConnected()).toBe(true);

    collab.disconnect();
    expect(collab.isConnected()).toBe(false);
  });

  it('should get statistics', async () => {
    await collab.connect();

    const stats = collab.getStats();
    expect(stats.connected).toBe(true);
    expect(stats.userId).toBe('user1');
  });
});

describe('WebXRRenderer', () => {
  let renderer;

  beforeEach(() => {
    renderer = new WebXRRenderer({
      targetFPS: 60,
      enableVR: true,
    });
  });

  it('should initialize renderer', async () => {
    await renderer.initialize();
    expect(renderer.camera).toBeDefined();
  });

  it('should start XR session', async () => {
    await renderer.initialize();
    await renderer.startXRSession('immersive-vr');

    expect(renderer.isVR).toBe(true);
    expect(renderer.xrSession).toBeDefined();
  });

  it('should update node positions', async () => {
    await renderer.initialize();

    const nodes = new Map([
      ['n1', { id: 'n1', position: { x: 0, y: 0, z: 0 } }],
      ['n2', { id: 'n2', position: { x: 5, y: 5, z: 5 } }],
    ]);

    renderer.updateNodes(nodes);
    expect(renderer.nodeMeshes.size).toBe(2);
  });

  it('should update edges', async () => {
    await renderer.initialize();

    const nodes = new Map([
      ['n1', { id: 'n1', position: { x: 0, y: 0, z: 0 } }],
      ['n2', { id: 'n2', position: { x: 5, y: 5, z: 5 } }],
    ]);

    const edges = [
      { id: 'e1', source: 'n1', target: 'n2' },
    ];

    renderer.updateEdges(edges, nodes);
    expect(renderer.edgeMeshes.size).toBe(1);
  });

  it('should render frame and track FPS', async () => {
    await renderer.initialize();

    renderer.render();
    expect(renderer.fps).toBeGreaterThan(0);
  });

  it('should get/set camera position', async () => {
    await renderer.initialize();

    renderer.setCameraPosition({ x: 10, y: 20, z: 30 });
    const pos = renderer.getCameraPosition();

    expect(pos).toEqual({ x: 10, y: 20, z: 30 });
  });

  it('should get performance metrics', async () => {
    await renderer.initialize();
    renderer.render();

    const metrics = renderer.getMetrics();
    expect(metrics.fps).toBeDefined();
    expect(metrics.frameTime).toBeDefined();
  });

  it('should dispose resources', async () => {
    await renderer.initialize();
    await renderer.startXRSession('immersive-vr');

    renderer.dispose();
    expect(renderer.xrSession).toBeNull();
    expect(renderer.nodeMeshes.size).toBe(0);
  });
});

describe('SpatialKGEngine', () => {
  let engine;

  beforeEach(() => {
    engine = new SpatialKGEngine({
      layout: { iterations: 50 },
      rendering: { targetFPS: 60 },
      lod: { enabled: true },
    });
  });

  it('should initialize engine', async () => {
    await engine.initialize();
    expect(engine.initialized).toBe(true);
  });

  it('should load graph data', async () => {
    await engine.initialize();

    await engine.loadGraph({
      nodes: [
        { id: 'n1', position: { x: 0, y: 0, z: 0 } },
        { id: 'n2', position: { x: 10, y: 0, z: 0 } },
      ],
      edges: [
        { id: 'e1', source: 'n1', target: 'n2' },
      ],
    });

    expect(engine.layout.nodes.size).toBe(2);
    expect(engine.layout.edges.length).toBe(1);
    expect(engine.queryEngine).toBeDefined();
  });

  it('should perform spatial queries', async () => {
    await engine.initialize();

    await engine.loadGraph({
      nodes: [
        { id: 'n1', position: { x: 0, y: 0, z: 0 } },
        { id: 'n2', position: { x: 5, y: 0, z: 0 } },
        { id: 'n3', position: { x: 20, y: 0, z: 0 } },
      ],
      edges: [],
    });

    const results = engine.query({
      type: 'proximity',
      origin: { x: 0, y: 0, z: 0 },
      radius: 10,
    });

    expect(results.length).toBeGreaterThanOrEqual(1);
    expect(results.length).toBeLessThanOrEqual(2);
  });

  it('should start and stop rendering', async () => {
    // Mock requestAnimationFrame for Node environment
    global.requestAnimationFrame = (cb) => setTimeout(cb, 16);
    global.cancelAnimationFrame = (id) => clearTimeout(id);

    await engine.initialize();

    await engine.loadGraph({
      nodes: [{ id: 'n1', position: { x: 0, y: 0, z: 0 } }],
      edges: [],
    });

    engine.start();
    expect(engine.running).toBe(true);

    engine.stop();
    expect(engine.running).toBe(false);

    // Cleanup
    delete global.requestAnimationFrame;
    delete global.cancelAnimationFrame;
  });

  it('should register gesture listeners', async () => {
    await engine.initialize();

    const callback = vi.fn();
    engine.onGesture('select', callback);

    engine.gestureController.simulateGesture({
      type: 'select',
      controller: 'right',
      position: { x: 0, y: 0, z: 0 },
      timestamp: Date.now(),
    });

    expect(callback).toHaveBeenCalled();
  });

  it('should get performance metrics', async () => {
    await engine.initialize();

    await engine.loadGraph({
      nodes: [{ id: 'n1', position: { x: 0, y: 0, z: 0 } }],
      edges: [],
    });

    const metrics = engine.getMetrics();
    expect(metrics.rendering).toBeDefined();
    expect(metrics.lod).toBeDefined();
    expect(metrics.layout).toBeDefined();
  });

  it('should start WebXR session', async () => {
    await engine.initialize();

    await engine.startXR('immersive-vr');
    expect(engine.renderer.isVR).toBe(true);
  });

  it('should dispose engine', async () => {
    await engine.initialize();

    engine.dispose();
    expect(engine.running).toBe(false);
  });
});
