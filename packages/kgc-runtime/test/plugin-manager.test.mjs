/**
 * Plugin Manager Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { PluginManager, PLUGIN_STATES } from '../src/plugin-manager.mjs';

describe('PluginManager', () => {
  let manager;

  beforeEach(() => {
    manager = new PluginManager();
  });

  // Test 1: Plugin Registration
  it('should register a valid plugin', async () => {
    const manifest = {
      name: 'test-plugin',
      version: '[VERSION]',
      description: 'Test plugin',
      entryPoint: './test.mjs',
      capabilities: ['receipt:generate'],
      api_version: '[VERSION]',
    };

    const pluginId = await manager.registerPlugin(manifest);

    expect(pluginId).toBe('test-plugin@[VERSION]');
    expect(manager.getPluginState(pluginId)).toBe(PLUGIN_STATES.REGISTERED);
  });

  // Test 2: Plugin Loading
  it('should load a registered plugin', async () => {
    const manifest = {
      name: 'test-plugin',
      version: '[VERSION]',
      entryPoint: './test.mjs',
      capabilities: [],
      api_version: '[VERSION]',
    };

    const pluginId = await manager.registerPlugin(manifest);
    await manager.loadPlugin(pluginId);

    expect(manager.getPluginState(pluginId)).toBe(PLUGIN_STATES.LOADED);
  });

  // Test 3: Plugin Activation
  it('should activate a loaded plugin', async () => {
    const manifest = {
      name: 'test-plugin',
      version: '[VERSION]',
      entryPoint: './test.mjs',
      capabilities: [],
      api_version: '[VERSION]',
    };

    const pluginId = await manager.registerPlugin(manifest);
    await manager.loadPlugin(pluginId);
    await manager.activatePlugin(pluginId);

    expect(manager.getPluginState(pluginId)).toBe(PLUGIN_STATES.EXECUTING);
  });

  // Test 4: Plugin Deactivation
  it('should deactivate an executing plugin', async () => {
    const manifest = {
      name: 'test-plugin',
      version: '[VERSION]',
      entryPoint: './test.mjs',
      capabilities: [],
      api_version: '[VERSION]',
    };

    const pluginId = await manager.registerPlugin(manifest);
    await manager.loadPlugin(pluginId);
    await manager.activatePlugin(pluginId);
    await manager.deactivatePlugin(pluginId);

    expect(manager.getPluginState(pluginId)).toBe(PLUGIN_STATES.LOADED);
  });

  // Test 5: Invalid State Transition
  it('should reject invalid state transitions', async () => {
    const manifest = {
      name: 'test-plugin',
      version: '[VERSION]',
      entryPoint: './test.mjs',
      capabilities: [],
      api_version: '[VERSION]',
    };

    const pluginId = await manager.registerPlugin(manifest);

    // Cannot activate without loading
    await expect(manager.activatePlugin(pluginId)).rejects.toThrow(
      /Cannot activate plugin in state/
    );
  });

  // Test 6: Parallel Loading
  it('should support parallel plugin loading', async () => {
    const manager = new PluginManager({ parallelLoading: true });

    const plugins = ['plugin-1', 'plugin-2', 'plugin-3'];

    for (const name of plugins) {
      await manager.registerPlugin({
        name,
        version: '[VERSION]',
        entryPoint: './test.mjs',
        capabilities: [],
        api_version: '[VERSION]',
      });
    }

    const pluginIds = plugins.map(name => `${name}@[VERSION]`);
    const result = await manager.loadPluginsParallel(pluginIds);

    expect(result.success).toHaveLength(3);
    expect(result.failed).toHaveLength(0);
  });

  // Test 7: Audit Log
  it('should maintain audit log of all operations', async () => {
    const manifest = {
      name: 'test-plugin',
      version: '[VERSION]',
      entryPoint: './test.mjs',
      capabilities: [],
      api_version: '[VERSION]',
    };

    const pluginId = await manager.registerPlugin(manifest);
    await manager.loadPlugin(pluginId);

    const auditLog = manager.getAuditLog({ pluginId });

    expect(auditLog.length).toBeGreaterThan(0);
    expect(auditLog.some(entry => entry.action === 'register')).toBe(true);
    expect(auditLog.some(entry => entry.action === 'load')).toBe(true);
  });

  // Test 8: State Transition Count
  it('should track state transition count', async () => {
    const manifest = {
      name: 'test-plugin',
      version: '[VERSION]',
      entryPoint: './test.mjs',
      capabilities: [],
      api_version: '[VERSION]',
    };

    const initialCount = manager.getTransitionCount();

    const pluginId = await manager.registerPlugin(manifest);
    await manager.loadPlugin(pluginId);
    await manager.activatePlugin(pluginId);

    const finalCount = manager.getTransitionCount();

    expect(finalCount).toBe(initialCount + 3); // register, load, activate
  });

  // Test 9: List Plugins by State
  it('should list plugins by state', async () => {
    await manager.registerPlugin({
      name: 'plugin-1',
      version: '[VERSION]',
      entryPoint: './test.mjs',
      capabilities: [],
      api_version: '[VERSION]',
    });

    await manager.registerPlugin({
      name: 'plugin-2',
      version: '[VERSION]',
      entryPoint: './test.mjs',
      capabilities: [],
      api_version: '[VERSION]',
    });

    await manager.loadPlugin('plugin-1@[VERSION]');

    const registered = manager.listPluginsByState(PLUGIN_STATES.REGISTERED);
    const loaded = manager.listPluginsByState(PLUGIN_STATES.LOADED);

    expect(registered).toHaveLength(1);
    expect(loaded).toHaveLength(1);
  });

  // Test 10: Plugin Uninstall
  it('should uninstall a plugin completely', async () => {
    const manifest = {
      name: 'test-plugin',
      version: '[VERSION]',
      entryPoint: './test.mjs',
      capabilities: [],
      api_version: '[VERSION]',
    };

    const pluginId = await manager.registerPlugin(manifest);
    await manager.uninstallPlugin(pluginId);

    expect(manager.getPlugin(pluginId)).toBeNull();
    expect(manager.getPluginState(pluginId)).toBeNull();
  });
});
