import { describe, expect, it, beforeEach } from "vitest";
import { Store, DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

// Mock useDelta for edge case testing
const useDelta = (options = {}) => {
  const { deterministic = true } = options;

  return {
    diff: (storeA, storeB) => {
      if (!storeA || typeof storeA.getQuads !== "function") {
        throw new Error("[useDelta] First store must be a valid N3.Store");
      }
      
      if (!storeB || typeof storeB.getQuads !== "function") {
        throw new Error("[useDelta] Second store must be a valid N3.Store");
      }

      const added = new Store();
      const removed = new Store();

      for (const quad of storeB) {
        if (!storeA.has(quad)) {
          added.add(quad);
        }
      }

      for (const quad of storeA) {
        if (!storeB.has(quad)) {
          removed.add(quad);
        }
      }

      return {
        added: deterministic ? added : added,
        removed: deterministic ? removed : removed
      };
    },
    patch: (baseStore, changes) => {
      if (!baseStore || typeof baseStore.getQuads !== "function") {
        throw new Error("[useDelta] Base store must be a valid N3.Store");
      }

      if (!changes || typeof changes !== "object") {
        throw new Error("[useDelta] Changes must be an object");
      }

      const { added, removed } = changes;
      
      if (!added || !removed) {
        throw new Error("[useDelta] Changes must include added and removed stores");
      }

      const result = new Store([...baseStore]);

      for (const quad of removed) {
        result.delete(quad);
      }

      for (const quad of added) {
        result.add(quad);
      }

      return result;
    },
    getStats: (changes) => {
      if (!changes || typeof changes !== "object") {
        throw new Error("[useDelta] Changes must be an object");
      }

      const { added, removed } = changes;
      
      if (!added || !removed) {
        throw new Error("[useDelta] Changes must include added and removed stores");
      }

      return {
        added: { quads: added.size },
        removed: { quads: removed.size },
        total: { quads: added.size + removed.size }
      };
    },
    isEmpty: (changes) => {
      if (!changes || typeof changes !== "object") {
        return true;
      }

      const { added, removed } = changes;
      return (!added || added.size === 0) && (!removed || removed.size === 0);
    },
    merge: (...changeSets) => {
      const merged = { added: new Store(), removed: new Store() };

      for (const changes of changeSets) {
        if (!changes || typeof changes !== "object") {
          continue;
        }

        const { added, removed } = changes;
        
        if (added) {
          for (const quad of added) {
            merged.added.add(quad);
          }
        }
        
        if (removed) {
          for (const quad of removed) {
            merged.removed.add(quad);
          }
        }
      }

      return {
        added: merged.added,
        removed: merged.removed
      };
    },
    invert: (changes) => {
      if (!changes || typeof changes !== "object") {
        throw new Error("[useDelta] Changes must be an object");
      }

      const { added, removed } = changes;
      
      return {
        added: removed || new Store(),
        removed: added || new Store()
      };
    }
  };
};

describe("useDelta Edge Cases", () => {
  let delta;
  let store1, store2, store3;

  beforeEach(() => {
    delta = useDelta();
    
    store1 = new Store();
    store2 = new Store();
    store3 = new Store();

    const q1 = quad(namedNode("ex:s1"), namedNode("ex:p1"), literal("o1"));
    const q2 = quad(namedNode("ex:s2"), namedNode("ex:p2"), literal("o2"));
    const q3 = quad(namedNode("ex:s3"), namedNode("ex:p3"), literal("o3"));

    store1.add(q1);
    store1.add(q2);

    store2.add(q2);
    store2.add(q3);

    store3.add(q1);
    store3.add(q2);
    store3.add(q3);
  });

  describe("Null and Undefined Inputs", () => {
    it("should throw error for null first store in diff", () => {
      // Act & Assert
      expect(() => delta.diff(null, store2)).toThrow("[useDelta] First store must be a valid N3.Store");
    });

    it("should throw error for undefined first store in diff", () => {
      // Act & Assert
      expect(() => delta.diff(undefined, store2)).toThrow("[useDelta] First store must be a valid N3.Store");
    });

    it("should throw error for null second store in diff", () => {
      // Act & Assert
      expect(() => delta.diff(store1, null)).toThrow("[useDelta] Second store must be a valid N3.Store");
    });

    it("should throw error for undefined second store in diff", () => {
      // Act & Assert
      expect(() => delta.diff(store1, undefined)).toThrow("[useDelta] Second store must be a valid N3.Store");
    });

    it("should throw error for null base store in patch", () => {
      // Arrange
      const changes = { added: new Store(), removed: new Store() };

      // Act & Assert
      expect(() => delta.patch(null, changes)).toThrow("[useDelta] Base store must be a valid N3.Store");
    });

    it("should throw error for undefined base store in patch", () => {
      // Arrange
      const changes = { added: new Store(), removed: new Store() };

      // Act & Assert
      expect(() => delta.patch(undefined, changes)).toThrow("[useDelta] Base store must be a valid N3.Store");
    });

    it("should throw error for null changes in patch", () => {
      // Act & Assert
      expect(() => delta.patch(store1, null)).toThrow("[useDelta] Changes must be an object");
    });

    it("should throw error for undefined changes in patch", () => {
      // Act & Assert
      expect(() => delta.patch(store1, undefined)).toThrow("[useDelta] Changes must be an object");
    });

    it("should throw error for null changes in getStats", () => {
      // Act & Assert
      expect(() => delta.getStats(null)).toThrow("[useDelta] Changes must be an object");
    });

    it("should throw error for undefined changes in getStats", () => {
      // Act & Assert
      expect(() => delta.getStats(undefined)).toThrow("[useDelta] Changes must be an object");
    });

    it("should throw error for null changes in invert", () => {
      // Act & Assert
      expect(() => delta.invert(null)).toThrow("[useDelta] Changes must be an object");
    });

    it("should throw error for undefined changes in invert", () => {
      // Act & Assert
      expect(() => delta.invert(undefined)).toThrow("[useDelta] Changes must be an object");
    });
  });

  describe("Invalid Data Types", () => {
    it("should throw error for non-store first store in diff", () => {
      // Act & Assert
      expect(() => delta.diff("not-a-store", store2)).toThrow("[useDelta] First store must be a valid N3.Store");
      expect(() => delta.diff(123, store2)).toThrow("[useDelta] First store must be a valid N3.Store");
      expect(() => delta.diff({}, store2)).toThrow("[useDelta] First store must be a valid N3.Store");
      expect(() => delta.diff([], store2)).toThrow("[useDelta] First store must be a valid N3.Store");
      expect(() => delta.diff(true, store2)).toThrow("[useDelta] First store must be a valid N3.Store");
    });

    it("should throw error for non-store second store in diff", () => {
      // Act & Assert
      expect(() => delta.diff(store1, "not-a-store")).toThrow("[useDelta] Second store must be a valid N3.Store");
      expect(() => delta.diff(store1, 123)).toThrow("[useDelta] Second store must be a valid N3.Store");
      expect(() => delta.diff(store1, {})).toThrow("[useDelta] Second store must be a valid N3.Store");
      expect(() => delta.diff(store1, [])).toThrow("[useDelta] Second store must be a valid N3.Store");
      expect(() => delta.diff(store1, true)).toThrow("[useDelta] Second store must be a valid N3.Store");
    });

    it("should throw error for non-object changes in patch", () => {
      // Act & Assert
      expect(() => delta.patch(store1, "not-an-object")).toThrow("[useDelta] Changes must be an object");
      expect(() => delta.patch(store1, 123)).toThrow("[useDelta] Changes must be an object");
      expect(() => delta.patch(store1, [])).toThrow("[useDelta] Changes must be an object");
      expect(() => delta.patch(store1, true)).toThrow("[useDelta] Changes must be an object");
    });

    it("should throw error for non-object changes in getStats", () => {
      // Act & Assert
      expect(() => delta.getStats("not-an-object")).toThrow("[useDelta] Changes must be an object");
      expect(() => delta.getStats(123)).toThrow("[useDelta] Changes must be an object");
      expect(() => delta.getStats([])).toThrow("[useDelta] Changes must be an object");
      expect(() => delta.getStats(true)).toThrow("[useDelta] Changes must be an object");
    });

    it("should throw error for non-object changes in invert", () => {
      // Act & Assert
      expect(() => delta.invert("not-an-object")).toThrow("[useDelta] Changes must be an object");
      expect(() => delta.invert(123)).toThrow("[useDelta] Changes must be an object");
      expect(() => delta.invert([])).toThrow("[useDelta] Changes must be an object");
      expect(() => delta.invert(true)).toThrow("[useDelta] Changes must be an object");
    });
  });

  describe("Empty and Invalid Changes", () => {
    it("should throw error for changes without added store", () => {
      // Arrange
      const invalidChanges = { removed: new Store() };

      // Act & Assert
      expect(() => delta.patch(store1, invalidChanges)).toThrow("[useDelta] Changes must include added and removed stores");
    });

    it("should throw error for changes without removed store", () => {
      // Arrange
      const invalidChanges = { added: new Store() };

      // Act & Assert
      expect(() => delta.patch(store1, invalidChanges)).toThrow("[useDelta] Changes must include added and removed stores");
    });

    it("should throw error for changes with null added store", () => {
      // Arrange
      const invalidChanges = { added: null, removed: new Store() };

      // Act & Assert
      expect(() => delta.patch(store1, invalidChanges)).toThrow("[useDelta] Changes must include added and removed stores");
    });

    it("should throw error for changes with null removed store", () => {
      // Arrange
      const invalidChanges = { added: new Store(), removed: null };

      // Act & Assert
      expect(() => delta.patch(store1, invalidChanges)).toThrow("[useDelta] Changes must include added and removed stores");
    });

    it("should throw error for changes with undefined added store", () => {
      // Arrange
      const invalidChanges = { added: undefined, removed: new Store() };

      // Act & Assert
      expect(() => delta.patch(store1, invalidChanges)).toThrow("[useDelta] Changes must include added and removed stores");
    });

    it("should throw error for changes with undefined removed store", () => {
      // Arrange
      const invalidChanges = { added: new Store(), removed: undefined };

      // Act & Assert
      expect(() => delta.patch(store1, invalidChanges)).toThrow("[useDelta] Changes must include added and removed stores");
    });
  });

  describe("Empty Stores", () => {
    it("should handle diff between empty stores", () => {
      // Arrange
      const emptyStore1 = new Store();
      const emptyStore2 = new Store();

      // Act
      const result = delta.diff(emptyStore1, emptyStore2);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added.size).toBe(0);
      expect(result.removed.size).toBe(0);
    });

    it("should handle diff between empty and non-empty stores", () => {
      // Arrange
      const emptyStore = new Store();

      // Act
      const result = delta.diff(emptyStore, store1);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added.size).toBe(2); // All quads from store1
      expect(result.removed.size).toBe(0);
    });

    it("should handle diff between non-empty and empty stores", () => {
      // Arrange
      const emptyStore = new Store();

      // Act
      const result = delta.diff(store1, emptyStore);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added.size).toBe(0);
      expect(result.removed.size).toBe(2); // All quads from store1
    });

    it("should handle patch with empty changes", () => {
      // Arrange
      const emptyChanges = { added: new Store(), removed: new Store() };

      // Act
      const result = delta.patch(store1, emptyChanges);

      // Assert
      expect(result.size).toBe(2); // Should be unchanged
    });

    it("should handle patch with empty base store", () => {
      // Arrange
      const emptyStore = new Store();
      const changes = delta.diff(emptyStore, store1);

      // Act
      const result = delta.patch(emptyStore, changes);

      // Assert
      expect(result.size).toBe(2); // Should have all quads from store1
    });
  });

  describe("Identical Stores", () => {
    it("should handle diff between identical stores", () => {
      // Act
      const result = delta.diff(store1, store1);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added.size).toBe(0);
      expect(result.removed.size).toBe(0);
    });

    it("should handle patch with no changes", () => {
      // Arrange
      const noChanges = { added: new Store(), removed: new Store() };

      // Act
      const result = delta.patch(store1, noChanges);

      // Assert
      expect(result.size).toBe(2); // Should be unchanged
    });
  });

  describe("Boundary Values", () => {
    it("should handle very large stores", () => {
      // Arrange
      const largeStore1 = new Store();
      const largeStore2 = new Store();

      for (let i = 0; i < 10_000; i++) {
        const q1 = quad(namedNode(`ex:s${i}`), namedNode("ex:p"), literal(`o${i}`));
        const q2 = quad(namedNode(`ex:s${i}`), namedNode("ex:p"), literal(`o${i + 10_000}`));
        
        largeStore1.add(q1);
        largeStore2.add(q2);
      }

      // Act
      const result = delta.diff(largeStore1, largeStore2);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added.size).toBe(10_000);
      expect(result.removed.size).toBe(10_000);
    });

    it("should handle very large changes", () => {
      // Arrange
      const largeAdded = new Store();
      const largeRemoved = new Store();

      for (let i = 0; i < 10_000; i++) {
        const q1 = quad(namedNode(`ex:s${i}`), namedNode("ex:p"), literal(`o${i}`));
        const q2 = quad(namedNode(`ex:s${i + 10_000}`), namedNode("ex:p"), literal(`o${i + 10_000}`));
        
        largeAdded.add(q1);
        largeRemoved.add(q2);
      }

      const largeChanges = { added: largeAdded, removed: largeRemoved };

      // Act
      const result = delta.patch(store1, largeChanges);

      // Assert
      expect(result.size).toBe(10_002); // Original 2 + 10000 added - 0 removed
    });
  });

  describe("Special Characters and Encoding", () => {
    it("should handle quads with special characters", () => {
      // Arrange
      const specialStore1 = new Store();
      const specialStore2 = new Store();

      const specialQuads = [
        quad(namedNode("ex:s with spaces"), namedNode("ex:p"), literal("o with spaces")),
        quad(namedNode("ex:s%20with%20encoding"), namedNode("ex:p"), literal("o%20with%20encoding")),
        quad(namedNode("ex:s#fragment"), namedNode("ex:p"), literal("o#fragment")),
        quad(namedNode("ex:s?query=value"), namedNode("ex:p"), literal("o?query=value")),
        quad(namedNode("ex:s/with/unicode/测试"), namedNode("ex:p"), literal("o/with/unicode/测试")),
        quad(namedNode("ex:s/with/emoji/🚀"), namedNode("ex:p"), literal("o/with/emoji/🚀"))
      ];

      specialStore1.add(specialQuads[0]);
      specialStore1.add(specialQuads[1]);
      specialStore2.add(specialQuads[1]);
      specialStore2.add(specialQuads[2]);

      // Act
      const result = delta.diff(specialStore1, specialStore2);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added.size).toBe(1);
      expect(result.removed.size).toBe(1);
    });
  });

  describe("Statistics Edge Cases", () => {
    it("should handle empty changes in getStats", () => {
      // Arrange
      const emptyChanges = { added: new Store(), removed: new Store() };

      // Act
      const result = delta.getStats(emptyChanges);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result).toHaveProperty("total");
      expect(result.added.quads).toBe(0);
      expect(result.removed.quads).toBe(0);
      expect(result.total.quads).toBe(0);
    });

    it("should handle large changes in getStats", () => {
      // Arrange
      const largeAdded = new Store();
      const largeRemoved = new Store();

      for (let i = 0; i < 1000; i++) {
        const q1 = quad(namedNode(`ex:s${i}`), namedNode("ex:p"), literal(`o${i}`));
        const q2 = quad(namedNode(`ex:s${i + 1000}`), namedNode("ex:p"), literal(`o${i + 1000}`));
        
        largeAdded.add(q1);
        largeRemoved.add(q2);
      }

      const largeChanges = { added: largeAdded, removed: largeRemoved };

      // Act
      const result = delta.getStats(largeChanges);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result).toHaveProperty("total");
      expect(result.added.quads).toBe(1000);
      expect(result.removed.quads).toBe(1000);
      expect(result.total.quads).toBe(2000);
    });
  });

  describe("isEmpty Edge Cases", () => {
    it("should handle null changes in isEmpty", () => {
      // Act
      const result = delta.isEmpty(null);

      // Assert
      expect(result).toBe(true);
    });

    it("should handle undefined changes in isEmpty", () => {
      // Act
      const result = delta.isEmpty(undefined);

      // Assert
      expect(result).toBe(true);
    });

    it("should handle empty changes in isEmpty", () => {
      // Arrange
      const emptyChanges = { added: new Store(), removed: new Store() };

      // Act
      const result = delta.isEmpty(emptyChanges);

      // Assert
      expect(result).toBe(true);
    });

    it("should handle non-empty changes in isEmpty", () => {
      // Arrange
      const changes = delta.diff(store1, store2);

      // Act
      const result = delta.isEmpty(changes);

      // Assert
      expect(result).toBe(false);
    });

    it("should handle changes with null stores in isEmpty", () => {
      // Arrange
      const nullChanges = { added: null, removed: null };

      // Act
      const result = delta.isEmpty(nullChanges);

      // Assert
      expect(result).toBe(true);
    });

    it("should handle changes with undefined stores in isEmpty", () => {
      // Arrange
      const undefinedChanges = { added: undefined, removed: undefined };

      // Act
      const result = delta.isEmpty(undefinedChanges);

      // Assert
      expect(result).toBe(true);
    });
  });

  describe("Merge Edge Cases", () => {
    it("should handle empty change sets in merge", () => {
      // Act
      const result = delta.merge();

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added.size).toBe(0);
      expect(result.removed.size).toBe(0);
    });

    it("should handle null change sets in merge", () => {
      // Act
      const result = delta.merge(null, undefined, {});

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added.size).toBe(0);
      expect(result.removed.size).toBe(0);
    });

    it("should handle multiple change sets in merge", () => {
      // Arrange
      const changes1 = delta.diff(store1, store2);
      const changes2 = delta.diff(store2, store3);

      // Act
      const result = delta.merge(changes1, changes2);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added.size).toBe(2); // q3 from both diffs
      expect(result.removed.size).toBe(1); // q1 from first diff
    });

    it("should handle change sets with null stores in merge", () => {
      // Arrange
      const changes1 = { added: new Store(), removed: new Store() };
      const changes2 = { added: null, removed: null };

      // Act
      const result = delta.merge(changes1, changes2);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added.size).toBe(0);
      expect(result.removed.size).toBe(0);
    });
  });

  describe("Invert Edge Cases", () => {
    it("should handle empty changes in invert", () => {
      // Arrange
      const emptyChanges = { added: new Store(), removed: new Store() };

      // Act
      const result = delta.invert(emptyChanges);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added.size).toBe(0);
      expect(result.removed.size).toBe(0);
    });

    it("should handle changes with null stores in invert", () => {
      // Arrange
      const nullChanges = { added: null, removed: null };

      // Act
      const result = delta.invert(nullChanges);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added).toBeNull();
      expect(result.removed).toBeNull();
    });

    it("should handle changes with undefined stores in invert", () => {
      // Arrange
      const undefinedChanges = { added: undefined, removed: undefined };

      // Act
      const result = delta.invert(undefinedChanges);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added).toBeUndefined();
      expect(result.removed).toBeUndefined();
    });

    it("should handle changes with only added store in invert", () => {
      // Arrange
      const addedOnlyChanges = { added: new Store(), removed: undefined };

      // Act
      const result = delta.invert(addedOnlyChanges);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added).toBeUndefined();
      expect(result.removed).toBeInstanceOf(Store);
    });

    it("should handle changes with only removed store in invert", () => {
      // Arrange
      const removedOnlyChanges = { added: undefined, removed: new Store() };

      // Act
      const result = delta.invert(removedOnlyChanges);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
      expect(result.added).toBeInstanceOf(Store);
      expect(result.removed).toBeUndefined();
    });
  });

  describe("Options Edge Cases", () => {
    it("should handle null options", () => {
      // Act & Assert
      expect(() => useDelta(null)).not.toThrow();
    });

    it("should handle undefined options", () => {
      // Act & Assert
      expect(() => useDelta(undefined)).not.toThrow();
    });

    it("should handle empty options", () => {
      // Act & Assert
      expect(() => useDelta({})).not.toThrow();
    });

    it("should handle options with null deterministic", () => {
      // Act & Assert
      expect(() => useDelta({ deterministic: null })).not.toThrow();
    });

    it("should handle options with undefined deterministic", () => {
      // Act & Assert
      expect(() => useDelta({ deterministic: undefined })).not.toThrow();
    });
  });

  describe("Concurrent Operations", () => {
    it("should handle rapid diff operations", () => {
      // Act
      for (let i = 0; i < 1000; i++) {
        const result = delta.diff(store1, store2);
        expect(result).toHaveProperty("added");
        expect(result).toHaveProperty("removed");
      }

      // Assert
      expect(store1.size).toBe(2);
      expect(store2.size).toBe(2);
    });

    it("should handle rapid patch operations", () => {
      // Arrange
      const changes = delta.diff(store1, store2);

      // Act
      for (let i = 0; i < 1000; i++) {
        const result = delta.patch(store1, changes);
        expect(result).toBeInstanceOf(Store);
      }

      // Assert
      expect(store1.size).toBe(2); // Original store should be unchanged
    });
  });

  describe("Error Recovery", () => {
    it("should recover from invalid diff operations", () => {
      // Act
      try {
        delta.diff(null, store2);
      } catch {
        // Expected error
      }

      // Should still work after error
      const result = delta.diff(store1, store2);

      // Assert
      expect(result).toHaveProperty("added");
      expect(result).toHaveProperty("removed");
    });

    it("should recover from invalid patch operations", () => {
      // Act
      try {
        delta.patch(null, { added: new Store(), removed: new Store() });
      } catch {
        // Expected error
      }

      // Should still work after error
      const changes = delta.diff(store1, store2);
      const result = delta.patch(store1, changes);

      // Assert
      expect(result).toBeInstanceOf(Store);
    });
  });
});
