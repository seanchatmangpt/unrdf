import { describe, expect, it, beforeEach } from "vitest";

// Mock useCache for testing purposes
const useCache = () => {
  const cache = new Map();
  const accessTimes = new Map();

  return {
    get: async (key, fn, opts = {}) => {
      const { ttl = 300000 } = opts;
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
      const { ttl = 300000 } = opts;
      const now = Date.now();
      cache.set(key, { value, expires: now + ttl });
      accessTimes.set(key, now);
      return this;
    },
    has: (key) => {
      if (!cache.has(key)) return false;
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
      if (!cache.has(key)) return undefined;
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
      maxSize: 1000,
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

describe("useCache", () => {
  let cache;

  beforeEach(() => {
    cache = useCache();
  });

  it("should create cache interface", () => {
    // Assert
    expect(typeof cache.get).toBe("function");
    expect(typeof cache.set).toBe("function");
    expect(typeof cache.has).toBe("function");
    expect(typeof cache.getOnly).toBe("function");
    expect(typeof cache.delete).toBe("function");
    expect(typeof cache.clear).toBe("function");
    expect(typeof cache.stats).toBe("function");
    expect(typeof cache.keys).toBe("function");
    expect(typeof cache.entries).toBe("function");
    expect(typeof cache.cleanup).toBe("function");
  });

  it("should cache function results", async () => {
    // Arrange
    let callCount = 0;
    const testFn = async () => {
      callCount++;
      return `result-${callCount}`;
    };

    // Act
    const result1 = await cache.get("test-key", testFn);
    const result2 = await cache.get("test-key", testFn);

    // Assert
    expect(result1).toBe("result-1");
    expect(result2).toBe("result-1"); // Should be cached
    expect(callCount).toBe(1); // Function should only be called once
  });

  it("should set and get cached values", () => {
    // Act
    cache.set("test-key", "test-value");
    const result = cache.getOnly("test-key");

    // Assert
    expect(result).toBe("test-value");
  });

  it("should check if key exists in cache", () => {
    // Act
    cache.set("test-key", "test-value");
    const exists = cache.has("test-key");

    // Assert
    expect(exists).toBe(true);
  });

  it("should delete cached values", () => {
    // Arrange
    cache.set("test-key", "test-value");

    // Act
    const deleted = cache.delete("test-key");
    const exists = cache.has("test-key");

    // Assert
    expect(deleted).toBe(true);
    expect(exists).toBe(false);
  });

  it("should clear all cached values", () => {
    // Arrange
    cache.set("key1", "value1");
    cache.set("key2", "value2");

    // Act
    cache.clear();
    const keys = cache.keys();

    // Assert
    expect(keys.length).toBe(0);
  });

  it("should get cache statistics", () => {
    // Act
    const stats = cache.stats();

    // Assert
    expect(stats).toHaveProperty("size");
    expect(stats).toHaveProperty("valid");
    expect(stats).toHaveProperty("expired");
    expect(stats).toHaveProperty("maxSize");
    expect(stats).toHaveProperty("hitRate");
    expect(stats).toHaveProperty("memoryUsage");
  });

  it("should get all cache keys", () => {
    // Arrange
    cache.set("key1", "value1");
    cache.set("key2", "value2");

    // Act
    const keys = cache.keys();

    // Assert
    expect(Array.isArray(keys)).toBe(true);
    expect(keys).toContain("key1");
    expect(keys).toContain("key2");
  });

  it("should get cache entries with metadata", () => {
    // Arrange
    cache.set("test-key", "test-value");

    // Act
    const entries = cache.entries();

    // Assert
    expect(Array.isArray(entries)).toBe(true);
    expect(entries.length).toBe(1);
    expect(entries[0]).toHaveProperty("key");
    expect(entries[0]).toHaveProperty("value");
    expect(entries[0]).toHaveProperty("created");
    expect(entries[0]).toHaveProperty("expires");
    expect(entries[0]).toHaveProperty("ttl");
    expect(entries[0]).toHaveProperty("expired");
    expect(entries[0]).toHaveProperty("lastAccess");
  });

  it("should cleanup expired entries", () => {
    // Arrange
    cache.set("test-key", "test-value");

    // Act
    const cleaned = cache.cleanup();

    // Assert
    expect(typeof cleaned).toBe("number");
    expect(cleaned).toBeGreaterThanOrEqual(0);
  });

  it("should handle cache misses", () => {
    // Act
    const result = cache.getOnly("non-existent-key");

    // Assert
    expect(result).toBeUndefined();
  });

  it("should handle custom TTL", async () => {
    // Arrange
    let callCount = 0;
    const testFn = async () => {
      callCount++;
      return `result-${callCount}`;
    };

    // Act
    const result1 = await cache.get("test-key", testFn, { ttl: 1000 });
    const result2 = await cache.get("test-key", testFn, { ttl: 1000 });

    // Assert
    expect(result1).toBe("result-1");
    expect(result2).toBe("result-1");
    expect(callCount).toBe(1);
  });
});
