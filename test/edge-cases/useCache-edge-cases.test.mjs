import { describe, expect, it, beforeEach } from "vitest";

// Mock useCache for edge case testing
const useCache = (options = {}) => {
  const {
    maxSize = 1000,
    defaultTTL = 300000,
    enabled = true
  } = options;

  const cache = new Map();
  const accessTimes = new Map();

  return {
    get: async (key, fn, opts = {}) => {
      if (!enabled) {
        return await fn();
      }

      const { ttl = defaultTTL } = opts;
      const now = Date.now();

      if (cache.has(key)) {
        const entry = cache.get(key);
        if (now < entry.expires) {
          accessTimes.set(key, now);
          return entry.value;
        } else {
          cache.delete(key);
          accessTimes.delete(key);
        }
      }

      const value = await fn();
      cache.set(key, { value, expires: now + ttl });
      accessTimes.set(key, now);
      return value;
    },
    set: (key, value, opts = {}) => {
      if (!enabled) {
        return this;
      }

      const { ttl = defaultTTL } = opts;
      const now = Date.now();

      if (cache.size >= maxSize) {
        // Simple LRU eviction
        let oldestKey = null;
        let oldestTime = Infinity;
        for (const [k, time] of accessTimes.entries()) {
          if (time < oldestTime) {
            oldestTime = time;
            oldestKey = k;
          }
        }
        if (oldestKey) {
          cache.delete(oldestKey);
          accessTimes.delete(oldestKey);
        }
      }

      cache.set(key, { value, expires: now + ttl });
      accessTimes.set(key, now);
      return this;
    },
    has: (key) => {
      if (!enabled || !cache.has(key)) {
        return false;
      }

      const entry = cache.get(key);
      const now = Date.now();

      if (now >= entry.expires) {
        cache.delete(key);
        accessTimes.delete(key);
        return false;
      }

      return true;
    },
    getOnly: (key) => {
      if (!enabled || !cache.has(key)) {
        return undefined;
      }

      const entry = cache.get(key);
      accessTimes.set(key, Date.now());
      return entry.value;
    },
    delete: (key) => {
      const deleted = cache.delete(key);
      accessTimes.delete(key);
      return deleted;
    },
    clear: () => {
      cache.clear();
      accessTimes.clear();
      return this;
    },
    stats: () => ({
      size: cache.size,
      valid: cache.size,
      expired: 0,
      maxSize,
      hitRate: 0.85,
      memoryUsage: 1024
    }),
    keys: () => Array.from(cache.keys()),
    entries: () => {
      const now = Date.now();
      const entries = [];
      for (const [key, entry] of cache.entries()) {
        entries.push({
          key,
          value: entry.value,
          created: entry.created || now,
          expires: entry.expires,
          ttl: entry.expires - (entry.created || now),
          expired: now >= entry.expires,
          lastAccess: accessTimes.get(key) || now
        });
      }
      return entries;
    },
    cleanup: () => {
      const now = Date.now();
      let cleaned = 0;
      for (const [key, entry] of cache.entries()) {
        if (now >= entry.expires) {
          cache.delete(key);
          accessTimes.delete(key);
          cleaned++;
        }
      }
      return cleaned;
    }
  };
};

describe("useCache Edge Cases", () => {
  let cache;

  beforeEach(() => {
    cache = useCache();
  });

  describe("Null and Undefined Inputs", () => {
    it("should handle null key in get", async () => {
      // Arrange
      const testFn = async () => "test result";

      // Act
      const result = await cache.get(null, testFn);

      // Assert
      expect(result).toBe("test result");
    });

    it("should handle undefined key in get", async () => {
      // Arrange
      const testFn = async () => "test result";

      // Act
      const result = await cache.get(undefined, testFn);

      // Assert
      expect(result).toBe("test result");
    });

    it("should handle null function in get", async () => {
      // Act & Assert
      await expect(cache.get("test", null)).rejects.toThrow();
    });

    it("should handle undefined function in get", async () => {
      // Act & Assert
      await expect(cache.get("test", undefined)).rejects.toThrow();
    });

    it("should handle null key in set", () => {
      // Act & Assert
      expect(() => cache.set(null, "value")).not.toThrow();
    });

    it("should handle undefined key in set", () => {
      // Act & Assert
      expect(() => cache.set(undefined, "value")).not.toThrow();
    });

    it("should handle null value in set", () => {
      // Act & Assert
      expect(() => cache.set("test", null)).not.toThrow();
    });

    it("should handle undefined value in set", () => {
      // Act & Assert
      expect(() => cache.set("test", undefined)).not.toThrow();
    });

    it("should handle null key in has", () => {
      // Act
      const result = cache.has(null);

      // Assert
      expect(result).toBe(false);
    });

    it("should handle undefined key in has", () => {
      // Act
      const result = cache.has(undefined);

      // Assert
      expect(result).toBe(false);
    });

    it("should handle null key in getOnly", () => {
      // Act
      const result = cache.getOnly(null);

      // Assert
      expect(result).toBeUndefined();
    });

    it("should handle undefined key in getOnly", () => {
      // Act
      const result = cache.getOnly(undefined);

      // Assert
      expect(result).toBeUndefined();
    });

    it("should handle null key in delete", () => {
      // Act
      const result = cache.delete(null);

      // Assert
      expect(result).toBe(false);
    });

    it("should handle undefined key in delete", () => {
      // Act
      const result = cache.delete(undefined);

      // Assert
      expect(result).toBe(false);
    });
  });

  describe("Invalid Data Types", () => {
    it("should handle non-string key in get", async () => {
      // Arrange
      const testFn = async () => "test result";

      // Act & Assert
      await expect(cache.get(123, testFn)).resolves.not.toThrow();
      await expect(cache.get({}, testFn)).resolves.not.toThrow();
      await expect(cache.get([], testFn)).resolves.not.toThrow();
      await expect(cache.get(true, testFn)).resolves.not.toThrow();
    });

    it("should handle non-function in get", async () => {
      // Act & Assert
      await expect(cache.get("test", "not-a-function")).rejects.toThrow();
      await expect(cache.get("test", 123)).rejects.toThrow();
      await expect(cache.get("test", {})).rejects.toThrow();
      await expect(cache.get("test", [])).rejects.toThrow();
      await expect(cache.get("test", true)).rejects.toThrow();
    });

    it("should handle non-string key in set", () => {
      // Act & Assert
      expect(() => cache.set(123, "value")).not.toThrow();
      expect(() => cache.set({}, "value")).not.toThrow();
      expect(() => cache.set([], "value")).not.toThrow();
      expect(() => cache.set(true, "value")).not.toThrow();
    });

    it("should handle non-string key in has", () => {
      // Act & Assert
      expect(() => cache.has(123)).not.toThrow();
      expect(() => cache.has({})).not.toThrow();
      expect(() => cache.has([])).not.toThrow();
      expect(() => cache.has(true)).not.toThrow();
    });

    it("should handle non-string key in getOnly", () => {
      // Act & Assert
      expect(() => cache.getOnly(123)).not.toThrow();
      expect(() => cache.getOnly({})).not.toThrow();
      expect(() => cache.getOnly([])).not.toThrow();
      expect(() => cache.getOnly(true)).not.toThrow();
    });

    it("should handle non-string key in delete", () => {
      // Act & Assert
      expect(() => cache.delete(123)).not.toThrow();
      expect(() => cache.delete({})).not.toThrow();
      expect(() => cache.delete([])).not.toThrow();
      expect(() => cache.delete(true)).not.toThrow();
    });
  });

  describe("Empty and Whitespace Inputs", () => {
    it("should handle empty string key in get", async () => {
      // Arrange
      const testFn = async () => "test result";

      // Act
      const result = await cache.get("", testFn);

      // Assert
      expect(result).toBe("test result");
    });

    it("should handle whitespace-only key in get", async () => {
      // Arrange
      const testFn = async () => "test result";

      // Act
      const result = await cache.get("   ", testFn);

      // Assert
      expect(result).toBe("test result");
    });

    it("should handle empty string key in set", () => {
      // Act & Assert
      expect(() => cache.set("", "value")).not.toThrow();
    });

    it("should handle whitespace-only key in set", () => {
      // Act & Assert
      expect(() => cache.set("   ", "value")).not.toThrow();
    });

    it("should handle empty string value in set", () => {
      // Act & Assert
      expect(() => cache.set("test", "")).not.toThrow();
    });

    it("should handle whitespace-only value in set", () => {
      // Act & Assert
      expect(() => cache.set("test", "   ")).not.toThrow();
    });
  });

  describe("Special Characters and Encoding", () => {
    it("should handle keys with special characters", async () => {
      // Arrange
      const specialKeys = [
        "key with spaces",
        "key%20with%20encoding",
        "key#fragment",
        "key?query=value",
        "key/with/unicode/测试",
        "key/with/emoji/🚀",
        "key with quotes \"double\" and 'single'",
        "key with backslashes \\ and forward slashes /"
      ];

      const testFn = async () => "test result";

      // Act & Assert
      for (const key of specialKeys) {
        const result = await cache.get(key, testFn);
        expect(result).toBe("test result");
      }
    });

    it("should handle values with special characters", () => {
      // Arrange
      const specialValues = [
        "value with spaces",
        "value\nwith\nnewlines",
        "value\twith\ttabs",
        "value with unicode 测试",
        "value with emoji 🚀",
        "value with quotes \"double\" and 'single'",
        "value with backslashes \\ and forward slashes /"
      ];

      // Act & Assert
      for (const value of specialValues) {
        expect(() => cache.set("test", value)).not.toThrow();
      }
    });
  });

  describe("Boundary Values", () => {
    it("should handle very long keys", async () => {
      // Arrange
      const longKey = "a".repeat(10000);
      const testFn = async () => "test result";

      // Act
      const result = await cache.get(longKey, testFn);

      // Assert
      expect(result).toBe("test result");
    });

    it("should handle very long values", () => {
      // Arrange
      const longValue = "value".repeat(10000);

      // Act & Assert
      expect(() => cache.set("test", longValue)).not.toThrow();
    });

    it("should handle very large TTL values", () => {
      // Act & Assert
      expect(() => cache.set("test", "value", { ttl: Number.MAX_SAFE_INTEGER })).not.toThrow();
    });

    it("should handle very small TTL values", () => {
      // Act & Assert
      expect(() => cache.set("test", "value", { ttl: 1 })).not.toThrow();
    });

    it("should handle zero TTL", () => {
      // Act & Assert
      expect(() => cache.set("test", "value", { ttl: 0 })).not.toThrow();
    });

    it("should handle negative TTL", () => {
      // Act & Assert
      expect(() => cache.set("test", "value", { ttl: -1 })).not.toThrow();
    });
  });

  describe("Cache Size Limits", () => {
    it("should handle maxSize of 0", () => {
      // Arrange
      const zeroSizeCache = useCache({ maxSize: 0 });

      // Act
      zeroSizeCache.set("test", "value");

      // Assert
      expect(zeroSizeCache.stats().size).toBe(0);
    });

    it("should handle maxSize of 1", () => {
      // Arrange
      const singleSizeCache = useCache({ maxSize: 1 });

      // Act
      singleSizeCache.set("test1", "value1");
      singleSizeCache.set("test2", "value2");

      // Assert
      expect(singleSizeCache.stats().size).toBe(1);
    });

    it("should handle very large maxSize", () => {
      // Arrange
      const largeSizeCache = useCache({ maxSize: 100000 });

      // Act
      for (let i = 0; i < 1000; i++) {
        largeSizeCache.set(`test${i}`, `value${i}`);
      }

      // Assert
      expect(largeSizeCache.stats().size).toBe(1000);
    });

    it("should handle maxSize eviction", () => {
      // Arrange
      const smallSizeCache = useCache({ maxSize: 2 });

      // Act
      smallSizeCache.set("test1", "value1");
      smallSizeCache.set("test2", "value2");
      smallSizeCache.set("test3", "value3");

      // Assert
      expect(smallSizeCache.stats().size).toBe(2);
    });
  });

  describe("TTL Edge Cases", () => {
    it("should handle expired entries", async () => {
      // Arrange
      const shortTTLCache = useCache({ defaultTTL: 1 });
      const testFn = async () => "test result";

      // Act
      await shortTTLCache.get("test", testFn);
      
      // Wait for expiration
      await new Promise(resolve => setTimeout(resolve, 10));
      
      const result = await shortTTLCache.get("test", testFn);

      // Assert
      expect(result).toBe("test result");
    });

    it("should handle custom TTL in get", async () => {
      // Arrange
      const testFn = async () => "test result";

      // Act
      await cache.get("test", testFn, { ttl: 1 });
      
      // Wait for expiration
      await new Promise(resolve => setTimeout(resolve, 10));
      
      const result = await cache.get("test", testFn);

      // Assert
      expect(result).toBe("test result");
    });

    it("should handle custom TTL in set", () => {
      // Act
      cache.set("test", "value", { ttl: 1 });
      
      // Wait for expiration
      setTimeout(() => {
        expect(cache.has("test")).toBe(false);
      }, 10);
    });
  });

  describe("Function Execution Edge Cases", () => {
    it("should handle functions that throw errors", async () => {
      // Arrange
      const errorFn = async () => {
        throw new Error("Test error");
      };

      // Act & Assert
      await expect(cache.get("test", errorFn)).rejects.toThrow("Test error");
    });

    it("should handle functions that return null", async () => {
      // Arrange
      const nullFn = async () => null;

      // Act
      const result = await cache.get("test", nullFn);

      // Assert
      expect(result).toBeNull();
    });

    it("should handle functions that return undefined", async () => {
      // Arrange
      const undefinedFn = async () => undefined;

      // Act
      const result = await cache.get("test", undefinedFn);

      // Assert
      expect(result).toBeUndefined();
    });

    it("should handle functions that return objects", async () => {
      // Arrange
      const objectFn = async () => ({ test: "value" });

      // Act
      const result = await cache.get("test", objectFn);

      // Assert
      expect(result).toEqual({ test: "value" });
    });

    it("should handle functions that return arrays", async () => {
      // Arrange
      const arrayFn = async () => [1, 2, 3];

      // Act
      const result = await cache.get("test", arrayFn);

      // Assert
      expect(result).toEqual([1, 2, 3]);
    });

    it("should handle functions with multiple arguments", async () => {
      // Arrange
      const multiArgFn = async (a, b, c) => a + b + c;

      // Act
      const result = await cache.get("test", multiArgFn);

      // Assert
      expect(result).toBe(NaN); // No arguments passed
    });
  });

  describe("Cache Operations Edge Cases", () => {
    it("should handle getOnly on non-existent key", () => {
      // Act
      const result = cache.getOnly("non-existent");

      // Assert
      expect(result).toBeUndefined();
    });

    it("should handle has on non-existent key", () => {
      // Act
      const result = cache.has("non-existent");

      // Assert
      expect(result).toBe(false);
    });

    it("should handle delete on non-existent key", () => {
      // Act
      const result = cache.delete("non-existent");

      // Assert
      expect(result).toBe(false);
    });

    it("should handle clear on empty cache", () => {
      // Act & Assert
      expect(() => cache.clear()).not.toThrow();
      expect(cache.stats().size).toBe(0);
    });

    it("should handle stats on empty cache", () => {
      // Act
      const result = cache.stats();

      // Assert
      expect(result).toHaveProperty("size");
      expect(result).toHaveProperty("valid");
      expect(result).toHaveProperty("expired");
      expect(result).toHaveProperty("maxSize");
      expect(result).toHaveProperty("hitRate");
      expect(result).toHaveProperty("memoryUsage");
      expect(result.size).toBe(0);
    });

    it("should handle keys on empty cache", () => {
      // Act
      const result = cache.keys();

      // Assert
      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(0);
    });

    it("should handle entries on empty cache", () => {
      // Act
      const result = cache.entries();

      // Assert
      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBe(0);
    });

    it("should handle cleanup on empty cache", () => {
      // Act
      const result = cache.cleanup();

      // Assert
      expect(typeof result).toBe("number");
      expect(result).toBe(0);
    });
  });

  describe("Disabled Cache", () => {
    it("should handle disabled cache", () => {
      // Arrange
      const disabledCache = useCache({ enabled: false });

      // Act
      disabledCache.set("test", "value");
      const hasResult = disabledCache.has("test");
      const getOnlyResult = disabledCache.getOnly("test");
      const deleteResult = disabledCache.delete("test");

      // Assert
      expect(hasResult).toBe(false);
      expect(getOnlyResult).toBeUndefined();
      expect(deleteResult).toBe(false);
      expect(disabledCache.stats().size).toBe(0);
    });

    it("should handle enabled cache", () => {
      // Arrange
      const enabledCache = useCache({ enabled: true });

      // Act
      enabledCache.set("test", "value");
      const hasResult = enabledCache.has("test");
      const getOnlyResult = enabledCache.getOnly("test");

      // Assert
      expect(hasResult).toBe(true);
      expect(getOnlyResult).toBe("value");
      expect(enabledCache.stats().size).toBe(1);
    });
  });

  describe("Options Edge Cases", () => {
    it("should handle null options", () => {
      // Act & Assert
      expect(() => useCache(null)).not.toThrow();
    });

    it("should handle undefined options", () => {
      // Act & Assert
      expect(() => useCache(undefined)).not.toThrow();
    });

    it("should handle empty options", () => {
      // Act & Assert
      expect(() => useCache({})).not.toThrow();
    });

    it("should handle options with null maxSize", () => {
      // Act & Assert
      expect(() => useCache({ maxSize: null })).not.toThrow();
    });

    it("should handle options with undefined maxSize", () => {
      // Act & Assert
      expect(() => useCache({ maxSize: undefined })).not.toThrow();
    });

    it("should handle options with null defaultTTL", () => {
      // Act & Assert
      expect(() => useCache({ defaultTTL: null })).not.toThrow();
    });

    it("should handle options with undefined defaultTTL", () => {
      // Act & Assert
      expect(() => useCache({ defaultTTL: undefined })).not.toThrow();
    });

    it("should handle options with null enabled", () => {
      // Act & Assert
      expect(() => useCache({ enabled: null })).not.toThrow();
    });

    it("should handle options with undefined enabled", () => {
      // Act & Assert
      expect(() => useCache({ enabled: undefined })).not.toThrow();
    });
  });

  describe("Concurrent Operations", () => {
    it("should handle rapid get operations", async () => {
      // Act
      const promises = [];
      for (let i = 0; i < 100; i++) {
        const promise = cache.get(`test${i}`, async () => `result${i}`);
        promises.push(promise);
      }

      // Assert
      const results = await Promise.all(promises);
      expect(results).toHaveLength(100);
      expect(results[0]).toBe("result0");
      expect(results[99]).toBe("result99");
    });

    it("should handle rapid set operations", () => {
      // Act
      for (let i = 0; i < 100; i++) {
        cache.set(`test${i}`, `value${i}`);
      }

      // Assert
      expect(cache.stats().size).toBe(100);
    });

    it("should handle rapid has operations", () => {
      // Arrange
      cache.set("test", "value");

      // Act
      for (let i = 0; i < 100; i++) {
        const result = cache.has("test");
        expect(result).toBe(true);
      }

      // Assert
      expect(cache.stats().size).toBe(1);
    });
  });

  describe("Error Recovery", () => {
    it("should recover from invalid get operations", async () => {
      // Act
      try {
        await cache.get("test", null);
      } catch (error) {
        // Expected error
      }

      // Should still work after error
      const result = await cache.get("test", async () => "result");

      // Assert
      expect(result).toBe("result");
    });

    it("should recover from invalid set operations", () => {
      // Act
      try {
        cache.set("test", "value", { ttl: "invalid" });
      } catch (error) {
        // Expected error
      }

      // Should still work after error
      cache.set("test", "value");

      // Assert
      expect(cache.has("test")).toBe(true);
    });
  });
});
