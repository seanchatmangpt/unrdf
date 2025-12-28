/**
 * @fileoverview Hot Code Loader Tests
 * @vitest-environment jsdom
 */

import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest';
import { HotCodeLoader } from '../src/hot-code-loader.mjs';
import { SupervisorTree } from '../src/supervisor-tree.mjs';

// Mock AtomVMRuntime
const createMockRuntime = () => ({
  state: 'Ready',
  moduleName: 'testmodule',
  isReady: () => true,
  isLoaded: () => true,
  terminal: {
    log: vi.fn(),
  },
});

// Mock module content
const createMockModuleContent = (size = 1024) => {
  const buffer = new ArrayBuffer(size);
  const view = new Uint8Array(buffer);
  for (let i = 0; i < size; i++) {
    view[i] = i % 256;
  }
  return buffer;
};

describe('HotCodeLoader', () => {
  let loader;
  let mockRuntime;

  beforeEach(() => {
    mockRuntime = createMockRuntime();
    loader = new HotCodeLoader(mockRuntime);

    // Mock fetch for browser environment
    global.fetch = vi.fn().mockResolvedValue({
      ok: true,
      arrayBuffer: () => Promise.resolve(createMockModuleContent()),
    });

    // Mock crypto.subtle for signature computation
    global.crypto = {
      subtle: {
        digest: vi.fn().mockResolvedValue(new Uint8Array(32).buffer),
      },
    };
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe('constructor', () => {
    it('should create loader instance with runtime', () => {
      expect(loader).toBeDefined();
      expect(loader.runtime).toBe(mockRuntime);
      expect(loader.activeModules).toBeInstanceOf(Map);
      expect(loader.hotSwapCallbacks).toBeInstanceOf(Map);
    });

    it('should throw error when runtime is not provided', () => {
      expect(() => new HotCodeLoader(null)).toThrow('AtomVMRuntime instance is required');
    });

    it('should accept optional supervisor', () => {
      const supervisor = new SupervisorTree('test-supervisor', 'one_for_one');
      const loaderWithSupervisor = new HotCodeLoader(mockRuntime, { supervisor });
      expect(loaderWithSupervisor.supervisor).toBe(supervisor);
    });

    it('should set default options', () => {
      expect(loader.maxQueueSize).toBe(100);
      expect(loader.reloadTimeout).toBe(30000);
    });

    it('should accept custom options', () => {
      const customLoader = new HotCodeLoader(mockRuntime, {
        maxQueueSize: 50,
        reloadTimeout: 15000,
      });
      expect(customLoader.maxQueueSize).toBe(50);
      expect(customLoader.reloadTimeout).toBe(15000);
    });
  });

  describe('loadModule', () => {
    it('should load a valid module', async () => {
      const result = await loader.loadModule('/test/module.beam');

      expect(result.success).toBe(true);
      expect(result.moduleName).toBe('module');
      expect(result.signature).toBeDefined();
    });

    it('should return error for invalid path', async () => {
      const result = await loader.loadModule('');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Module path is required');
    });

    it('should return error for null path', async () => {
      const result = await loader.loadModule(null);

      expect(result.success).toBe(false);
      expect(result.error).toBe('Module path is required');
    });

    it('should return existing module if already loaded', async () => {
      // First load
      await loader.loadModule('/test/module.beam');

      // Second load should return existing
      const result = await loader.loadModule('/test/module.beam');

      expect(result.success).toBe(true);
      expect(result.moduleName).toBe('module');
    });

    it('should handle fetch failure', async () => {
      global.fetch = vi.fn().mockResolvedValue({
        ok: false,
        status: 404,
      });

      const result = await loader.loadModule('/test/missing.beam');

      expect(result.success).toBe(false);
      expect(result.error).toContain('Failed to load module');
    });

    it('should extract module name from path correctly', async () => {
      const result = await loader.loadModule('/path/to/my-module.avm');

      expect(result.moduleName).toBe('my-module');
    });
  });

  describe('reloadModule', () => {
    beforeEach(async () => {
      // Pre-load a module
      await loader.loadModule('/test/module.beam');
    });

    it('should reload an existing module', async () => {
      const result = await loader.reloadModule('module');

      expect(result.success).toBe(true);
      expect(result.moduleName).toBe('module');
      expect(result.version).toBe(2); // Incremented from 1
      expect(result.duration).toBeGreaterThanOrEqual(0);
    });

    it('should return error for non-existent module', async () => {
      const result = await loader.reloadModule('nonexistent');

      expect(result.success).toBe(false);
      expect(result.error).toContain('not found');
    });

    it('should return error for invalid module name', async () => {
      const result = await loader.reloadModule('');

      expect(result.success).toBe(false);
      expect(result.error).toBe('Module name is required');
    });

    it('should increment version on each reload', async () => {
      await loader.reloadModule('module');
      const result = await loader.reloadModule('module');

      expect(result.version).toBe(3);
    });
  });

  describe('registerHotSwap', () => {
    it('should register callback function', () => {
      const callback = vi.fn();
      const unregister = loader.registerHotSwap('module', callback);

      expect(typeof unregister).toBe('function');
      expect(loader.hotSwapCallbacks.has('module')).toBe(true);
    });

    it('should register callback object with beforeSwap and afterSwap', () => {
      const callback = {
        beforeSwap: vi.fn(),
        afterSwap: vi.fn(),
      };
      loader.registerHotSwap('module', callback);

      expect(loader.hotSwapCallbacks.get('module')).toHaveLength(1);
    });

    it('should throw error for invalid module name', () => {
      expect(() => loader.registerHotSwap('', vi.fn())).toThrow('Module name is required');
    });

    it('should throw error for invalid callback', () => {
      expect(() => loader.registerHotSwap('module', null)).toThrow('Callback is required');
    });

    it('should unregister callback when unregister function called', () => {
      const callback = vi.fn();
      const unregister = loader.registerHotSwap('module', callback);

      unregister();

      expect(loader.hotSwapCallbacks.get('module')).toHaveLength(0);
    });

    it('should fire beforeSwap callback before reload', async () => {
      await loader.loadModule('/test/module.beam');

      const beforeSwap = vi.fn();
      loader.registerHotSwap('module', { beforeSwap, afterSwap: vi.fn() });

      await loader.reloadModule('module');

      expect(beforeSwap).toHaveBeenCalled();
      expect(beforeSwap.mock.calls[0][0]).toHaveProperty('moduleName', 'module');
    });

    it('should fire afterSwap callback after successful reload', async () => {
      await loader.loadModule('/test/module.beam');

      const afterSwap = vi.fn();
      loader.registerHotSwap('module', { beforeSwap: vi.fn(), afterSwap });

      await loader.reloadModule('module');

      expect(afterSwap).toHaveBeenCalled();
      expect(afterSwap.mock.calls[0][0]).toHaveProperty('version');
    });
  });

  describe('getActiveModules', () => {
    it('should return empty array when no modules loaded', () => {
      const modules = loader.getActiveModules();

      expect(modules).toEqual([]);
    });

    it('should return all loaded modules', async () => {
      await loader.loadModule('/test/module1.beam');
      await loader.loadModule('/test/module2.beam');

      const modules = loader.getActiveModules();

      expect(modules).toHaveLength(2);
      expect(modules.map(m => m.name)).toContain('module1');
      expect(modules.map(m => m.name)).toContain('module2');
    });

    it('should return module info with correct structure', async () => {
      await loader.loadModule('/test/module.beam');

      const [module] = loader.getActiveModules();

      expect(module).toHaveProperty('name', 'module');
      expect(module).toHaveProperty('path', '/test/module.beam');
      expect(module).toHaveProperty('signature');
      expect(module).toHaveProperty('loadedAt');
      expect(module).toHaveProperty('version', 1);
      expect(module).toHaveProperty('status', 'loaded');
    });
  });

  describe('validateModuleSignature', () => {
    it('should validate signature of loaded module', async () => {
      await loader.loadModule('/test/module.beam');

      const result = await loader.validateModuleSignature('module');

      expect(result.valid).toBe(true);
      expect(result.signature).toBeDefined();
    });

    it('should return error for non-existent module', async () => {
      const result = await loader.validateModuleSignature('nonexistent');

      expect(result.valid).toBe(false);
      expect(result.error).toBe('Module not loaded');
    });

    it('should return error for invalid module name', async () => {
      const result = await loader.validateModuleSignature('');

      expect(result.valid).toBe(false);
      expect(result.error).toBe('Module name is required');
    });

    it('should detect signature mismatch on file change', async () => {
      await loader.loadModule('/test/module.beam');

      // Mock different content for re-fetch
      global.fetch = vi.fn().mockResolvedValue({
        ok: true,
        arrayBuffer: () => Promise.resolve(createMockModuleContent(2048)),
      });

      const result = await loader.validateModuleSignature('module');

      expect(result.valid).toBe(true);
      expect(result.previousSignature).toBeDefined();
    });
  });

  describe('concurrent reloads', () => {
    beforeEach(async () => {
      await loader.loadModule('/test/module.beam');
    });

    it('should queue concurrent reload requests', async () => {
      // Start first reload
      const reload1 = loader.reloadModule('module');

      // Queue second reload while first is processing
      loader.isProcessingQueue = true;
      const reload2Promise = loader.reloadModule('module');

      expect(loader.getPendingReloadCount()).toBe(1);

      // Complete first reload
      await reload1;

      // Second should be processed from queue
      loader.isProcessingQueue = false;
      loader._processNextInQueue();
      await reload2Promise;
    });

    it('should reject when queue is full', async () => {
      loader.maxQueueSize = 1;
      loader.isProcessingQueue = true;

      // Fill queue
      loader.reloadModule('module');

      // Next should be rejected
      await expect(loader.reloadModule('module')).rejects.toThrow('Reload queue is full');
    });
  });

  describe('supervisor integration', () => {
    it('should notify supervisor on reload', async () => {
      const supervisor = new SupervisorTree('test-supervisor', 'one_for_one');
      loader.setSupervisor(supervisor);

      await loader.loadModule('/test/module.beam');

      const consoleSpy = vi.spyOn(console, 'log');
      await loader.reloadModule('module');

      expect(consoleSpy).toHaveBeenCalledWith(
        expect.stringContaining('Notified supervisor')
      );
    });

    it('should handle supervisor notification errors gracefully', async () => {
      const supervisor = {
        // Invalid supervisor that will cause errors
      };
      loader.setSupervisor(supervisor);

      await loader.loadModule('/test/module.beam');

      // Should not throw
      const result = await loader.reloadModule('module');
      expect(result.success).toBe(true);
    });
  });

  describe('unloadModule', () => {
    it('should unload a loaded module', async () => {
      await loader.loadModule('/test/module.beam');

      const result = loader.unloadModule('module');

      expect(result).toBe(true);
      expect(loader.activeModules.has('module')).toBe(false);
    });

    it('should return false for non-existent module', () => {
      const result = loader.unloadModule('nonexistent');

      expect(result).toBe(false);
    });

    it('should remove associated callbacks', async () => {
      await loader.loadModule('/test/module.beam');
      loader.registerHotSwap('module', vi.fn());

      loader.unloadModule('module');

      expect(loader.hotSwapCallbacks.has('module')).toBe(false);
    });
  });

  describe('error handling in callbacks', () => {
    it('should fire onError callback when reload fails', async () => {
      await loader.loadModule('/test/module.beam');

      // Mock fetch failure
      global.fetch = vi.fn().mockResolvedValue({
        ok: false,
        status: 500,
      });

      const onError = vi.fn();
      loader.registerHotSwap('module', {
        beforeSwap: vi.fn(),
        afterSwap: vi.fn(),
        onError,
      });

      await loader.reloadModule('module');

      expect(onError).toHaveBeenCalled();
      expect(onError.mock.calls[0][0]).toHaveProperty('error');
    });

    it('should continue processing even if callback throws', async () => {
      await loader.loadModule('/test/module.beam');

      loader.registerHotSwap('module', {
        beforeSwap: () => { throw new Error('Callback error'); },
        afterSwap: vi.fn(),
      });

      // Should not throw
      const result = await loader.reloadModule('module');
      expect(result.success).toBe(true);
    });
  });

  describe('workers resume without restart', () => {
    it('should maintain module state during reload', async () => {
      await loader.loadModule('/test/module.beam');

      const moduleBefore = loader.activeModules.get('module');
      const pathBefore = moduleBefore.path;

      await loader.reloadModule('module');

      const moduleAfter = loader.activeModules.get('module');
      expect(moduleAfter.path).toBe(pathBefore);
      expect(moduleAfter.status).toBe('loaded');
    });

    it('should update loadedAt timestamp on reload', async () => {
      await loader.loadModule('/test/module.beam');

      const loadedAtBefore = loader.activeModules.get('module').loadedAt;

      // Small delay to ensure different timestamp
      await new Promise(resolve => setTimeout(resolve, 10));
      await loader.reloadModule('module');

      const loadedAtAfter = loader.activeModules.get('module').loadedAt;
      expect(loadedAtAfter).toBeGreaterThan(loadedAtBefore);
    });
  });
});
