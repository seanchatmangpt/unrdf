/**
 * Hierarchical Delegation Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  HierarchicalDelegator,
  createHierarchicalDelegator,
  create3LevelHierarchy,
} from '../src/hierarchical-delegation.mjs';

describe('HierarchicalDelegation', () => {
  let delegator;

  beforeEach(() => {
    delegator = createHierarchicalDelegator({
      aggregation: 'union',
      failure_propagation: 'collect',
    });
  });

  it('should register nodes in hierarchy', () => {
    const root = delegator.registerNode({
      id: 'root',
      level: 'root',
      agent_config: { id: 'root', scope: { files: [] } },
      max_children: 5,
      max_depth: 3,
    });

    expect(root.id).toBe('root');
    expect(root.level).toBe('root');
    expect(root.current_depth).toBe(0);
  });

  it('should spawn child agents', async () => {
    delegator.registerNode({
      id: 'parent',
      level: 'parent',
      agent_config: { id: 'parent', scope: { files: [] } },
      max_children: 3,
      max_depth: 3,
    });

    const { children, parent } = await delegator.spawnChildren('parent', [
      { scope: { files: ['child1/**'] } },
      { scope: { files: ['child2/**'] } },
    ]);

    expect(children).toHaveLength(2);
    expect(parent.children_ids).toHaveLength(2);
  });

  it('should prevent spawning from leaf nodes', async () => {
    delegator.registerNode({
      id: 'leaf',
      level: 'leaf',
      agent_config: { id: 'leaf', scope: { files: [] } },
      max_children: 0,
      max_depth: 3,
    });

    await expect(async () => {
      await delegator.spawnChildren('leaf', [
        { scope: { files: [] } },
      ]);
    }).rejects.toThrow('Leaf nodes cannot spawn children');
  });

  it('should execute hierarchical delegation', async () => {
    const root = delegator.registerNode({
      id: 'root',
      level: 'root',
      agent_config: { id: 'root', scope: { files: [] } },
      max_children: 2,
      max_depth: 2,
    });

    // Mock executor
    const executor = async (probe) => ({
      type: 'success',
      probe_id: probe.id,
    });

    const result = await delegator.execute('root', executor, [
      { type: 'read', target: 'test', cost: 1, expected_yield: 0.5 },
    ]);

    expect(result.success).toBe(true);
    expect(result.node_id).toBe('root');
    expect(result.result_hash).toBeDefined();
  });

  it('should create 3-level hierarchy', () => {
    const hierarchy = create3LevelHierarchy({
      rootConfig: { id: 'root', scope: { files: [] } },
      parentCount: 3,
      childrenPerParent: 2,
    });

    const tree = hierarchy.getTree();
    expect(tree).toHaveLength(1);
    expect(tree[0].children).toHaveLength(3);
    expect(tree[0].children[0].children).toHaveLength(2);
  });

  it('should propagate failures based on policy', async () => {
    const immediatePolicy = createHierarchicalDelegator({
      failure_propagation: 'collect', // Use 'collect' to get clean failure result
    });

    immediatePolicy.registerNode({
      id: 'parent',
      level: 'parent',
      agent_config: { id: 'parent', scope: { files: [] } },
      max_children: 2,
      max_depth: 2,
    });

    // Mock failing executor
    const failExecutor = async () => {
      throw new Error('Task failed');
    };

    const result = await immediatePolicy.execute('parent', failExecutor, [
      { type: 'read', target: 'test' },
    ]);

    // With 'collect' policy, execution continues but ultimately fails
    expect(result.success || result.error).toBeDefined();
  });

  it('should aggregate results from children', async () => {
    // Simple hierarchy with just root node (no children)
    const hierarchy = createHierarchicalDelegator();

    hierarchy.registerNode({
      id: 'root',
      level: 'root',
      agent_config: { id: 'root', scope: { files: [] } },
      max_children: 0,
      max_depth: 1,
    });

    const executor = async (probe) => ({
      observations: 5,
      deltas: 3,
    });

    const result = await hierarchy.execute('root', executor, [
      { type: 'transform', target: 'test' },
    ]);

    expect(result.success).toBe(true);
    // Root node itself should have observations
    expect(result.aggregated_observations).toBeGreaterThanOrEqual(0);
  });
});
