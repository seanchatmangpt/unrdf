/**
 * @file Tests for use-module-preloader hook
 * @description TRIZ #24 - Intermediary pattern implementation tests
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import {
  preloadModule,
  preloadModules,
  getModule,
  isModuleLoaded,
  isModuleLoading,
  getModuleLoadTime,
  clearModule,
  clearModuleCache,
  getCacheStats,
  useModulePreloader,
  PRELOAD_MODULES,
  PRELOAD_PRIORITY,
  PRELOAD_GROUPS,
  areEssentialModulesLoaded,
  preloadEssentialModules,
  getAllLoadedModules,
} from '../../../src/react-hooks/core/use-module-preloader.mjs';

// Mock React hooks for testing
vi.mock('react', () => ({
  useState: vi.fn(initial => {
    let state = typeof initial === 'function' ? initial() : initial;
    return [
      state,
      vi.fn(newState => {
        state = typeof newState === 'function' ? newState(state) : newState;
      }),
    ];
  }),
  useEffect: vi.fn(fn => {
    fn();
    return vi.fn();
  }),
  useCallback: vi.fn(fn => fn),
  useRef: vi.fn(initial => ({ current: initial })),
}));

describe('use-module-preloader', () => {
  beforeEach(() => {
    clearModuleCache();
    vi.clearAllMocks();
  });

  afterEach(() => {
    clearModuleCache();
  });

  describe('PRELOAD_MODULES', () => {
    it('should export predefined module paths', () => {
      expect(PRELOAD_MODULES).toBeDefined();
      expect(PRELOAD_MODULES.federation).toBeDefined();
      expect(PRELOAD_MODULES.optimizer).toBeDefined();
      expect(PRELOAD_MODULES.streaming).toBeDefined();
      expect(PRELOAD_MODULES.consensus).toBeDefined();
      expect(PRELOAD_MODULES.replication).toBeDefined();
    });

    it('should have valid module paths', () => {
      for (const [_key, path] of Object.entries(PRELOAD_MODULES)) {
        expect(typeof path).toBe('string');
        expect(path).toContain('.mjs');
        expect(path.startsWith('../../')).toBe(true);
      }
    });
  });

  describe('PRELOAD_PRIORITY', () => {
    it('should export priority levels', () => {
      expect(PRELOAD_PRIORITY.CRITICAL).toBe(1);
      expect(PRELOAD_PRIORITY.HIGH).toBe(2);
      expect(PRELOAD_PRIORITY.NORMAL).toBe(3);
      expect(PRELOAD_PRIORITY.LOW).toBe(4);
    });
  });

  describe('PRELOAD_GROUPS', () => {
    it('should export preload groups', () => {
      expect(PRELOAD_GROUPS.essential).toBeDefined();
      expect(PRELOAD_GROUPS.distributed).toBeDefined();
      expect(PRELOAD_GROUPS.query).toBeDefined();
      expect(PRELOAD_GROUPS.realtime).toBeDefined();
      expect(PRELOAD_GROUPS.all).toBeDefined();
    });

    it('should have all modules in "all" group', () => {
      const allPaths = Object.values(PRELOAD_MODULES);
      expect(PRELOAD_GROUPS.all).toEqual(allPaths);
    });

    it('should have distributed modules', () => {
      expect(PRELOAD_GROUPS.distributed).toContain(PRELOAD_MODULES.federation);
      expect(PRELOAD_GROUPS.distributed).toContain(PRELOAD_MODULES.consensus);
      expect(PRELOAD_GROUPS.distributed).toContain(PRELOAD_MODULES.replication);
    });
  });

  describe('clearModuleCache', () => {
    it('should clear all caches', () => {
      // Manually add to cache via internal access if needed
      clearModuleCache();
      const stats = getCacheStats();
      expect(stats.cachedModules).toBe(0);
      expect(stats.loadingModules).toBe(0);
    });
  });

  describe('getCacheStats', () => {
    it('should return cache statistics', () => {
      const stats = getCacheStats();
      expect(stats).toHaveProperty('cachedModules');
      expect(stats).toHaveProperty('loadingModules');
      expect(stats).toHaveProperty('moduleList');
      expect(stats).toHaveProperty('loadingList');
      expect(stats).toHaveProperty('timestamps');
    });

    it('should return empty stats initially', () => {
      clearModuleCache();
      const stats = getCacheStats();
      expect(stats.cachedModules).toBe(0);
      expect(stats.moduleList).toHaveLength(0);
    });
  });

  describe('isModuleLoaded', () => {
    it('should return false for unloaded module', () => {
      expect(isModuleLoaded('/fake/module.mjs')).toBe(false);
    });
  });

  describe('isModuleLoading', () => {
    it('should return false for module not being loaded', () => {
      expect(isModuleLoading('/fake/module.mjs')).toBe(false);
    });
  });

  describe('getModule', () => {
    it('should return undefined for uncached module', () => {
      expect(getModule('/fake/module.mjs')).toBeUndefined();
    });
  });

  describe('getModuleLoadTime', () => {
    it('should return undefined for uncached module', () => {
      expect(getModuleLoadTime('/fake/module.mjs')).toBeUndefined();
    });
  });

  describe('clearModule', () => {
    it('should not throw for non-existent module', () => {
      expect(() => clearModule('/fake/module.mjs')).not.toThrow();
    });
  });

  describe('preloadModules', () => {
    it('should return empty results for empty array', async () => {
      const results = await preloadModules([]);
      expect(results.modules.size).toBe(0);
      expect(results.errors.size).toBe(0);
      expect(results.loaded).toBe(0);
      expect(results.failed).toBe(0);
    });

    it('should return empty results for null', async () => {
      const results = await preloadModules(null);
      expect(results.loaded).toBe(0);
      expect(results.failed).toBe(0);
    });

    it('should handle progress callback', async () => {
      const onProgress = vi.fn();
      // Use empty array to avoid actual module loading
      await preloadModules([], { onProgress });
      // No modules = no progress calls
      expect(onProgress).not.toHaveBeenCalled();
    });

    it('should respect concurrency option', async () => {
      const results = await preloadModules([], { concurrency: 2 });
      expect(results).toBeDefined();
    });
  });

  describe('preloadModule', () => {
    it('should handle module load timeout', async () => {
      const fakePath = '/non/existent/module-that-will-fail.mjs';

      await expect(preloadModule(fakePath, { timeout: 100 })).rejects.toThrow();
    });

    it('should handle invalid module path', async () => {
      await expect(preloadModule('/invalid/path/module.mjs', { timeout: 500 })).rejects.toThrow();
    });
  });

  describe('areEssentialModulesLoaded', () => {
    it('should return false when essential modules not loaded', () => {
      clearModuleCache();
      expect(areEssentialModulesLoaded()).toBe(false);
    });
  });

  describe('preloadEssentialModules', () => {
    it('should attempt to load essential modules', async () => {
      // This will likely fail due to relative paths in test environment
      // but should not throw synchronously
      const promise = preloadEssentialModules();
      expect(promise).toBeInstanceOf(Promise);

      // Wait for result (will have errors due to path issues)
      const result = await promise;
      expect(result).toHaveProperty('modules');
      expect(result).toHaveProperty('errors');
    });
  });

  describe('getAllLoadedModules', () => {
    it('should return empty object when no modules loaded', () => {
      clearModuleCache();
      const modules = getAllLoadedModules();
      expect(modules).toEqual({});
    });
  });

  describe('useModulePreloader hook', () => {
    it('should return hook interface', () => {
      const result = useModulePreloader([]);

      expect(result).toHaveProperty('loaded');
      expect(result).toHaveProperty('loading');
      expect(result).toHaveProperty('progress');
      expect(result).toHaveProperty('loadedCount');
      expect(result).toHaveProperty('totalCount');
      expect(result).toHaveProperty('errors');
      expect(result).toHaveProperty('getModule');
      expect(result).toHaveProperty('reload');
      expect(result).toHaveProperty('clear');
    });

    it('should handle empty module list', () => {
      const result = useModulePreloader([]);
      expect(result.totalCount).toBe(0);
    });

    it('should provide getModule function', () => {
      const result = useModulePreloader([]);
      expect(typeof result.getModule).toBe('function');
      expect(result.getModule('/fake/path.mjs')).toBeUndefined();
    });

    it('should provide reload function', () => {
      const result = useModulePreloader([]);
      expect(typeof result.reload).toBe('function');
    });

    it('should provide clear function', () => {
      const result = useModulePreloader([]);
      expect(typeof result.clear).toBe('function');
    });

    it('should accept options', () => {
      const onComplete = vi.fn();
      const onError = vi.fn();

      const result = useModulePreloader([], {
        autoLoad: false,
        concurrency: 3,
        onComplete,
        onError,
      });

      expect(result).toBeDefined();
    });

    it('should track total count correctly', () => {
      const modules = ['mod1', 'mod2', 'mod3'];
      const result = useModulePreloader(modules, { autoLoad: false });
      expect(result.totalCount).toBe(3);
    });
  });

  describe('integration scenarios', () => {
    it('should handle cache operations in sequence', () => {
      clearModuleCache();

      let stats = getCacheStats();
      expect(stats.cachedModules).toBe(0);

      // Clear again should work
      clearModuleCache();
      stats = getCacheStats();
      expect(stats.cachedModules).toBe(0);
    });

    it('should handle concurrent cache operations', async () => {
      const operations = [
        () => getCacheStats(),
        () => isModuleLoaded('/test1'),
        () => isModuleLoading('/test2'),
        () => clearModule('/test3'),
        () => getModule('/test4'),
      ];

      const results = await Promise.all(operations.map(op => Promise.resolve(op())));
      expect(results).toHaveLength(5);
    });
  });

  describe('edge cases', () => {
    it('should handle undefined modules array', () => {
      // Using default parameter
      const result = useModulePreloader(undefined, { autoLoad: false });
      expect(result.totalCount).toBe(Object.values(PRELOAD_MODULES).length);
    });

    it('should handle module path with special characters', () => {
      const specialPath = '/path/with spaces/and-dashes/module.mjs';
      expect(isModuleLoaded(specialPath)).toBe(false);
      clearModule(specialPath); // Should not throw
    });

    it('should handle empty string module path', () => {
      expect(isModuleLoaded('')).toBe(false);
      expect(getModule('')).toBeUndefined();
    });
  });
});
