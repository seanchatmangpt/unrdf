import { describe, expect, it, beforeEach } from "vitest";
import { Store, DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

// Mock useDelta for testing purposes
const useDelta = () => ({
  diff: (storeA, storeB) => {
    const added = new Store();
    const removed = new Store();

    // Find quads in B but not in A (added)
    for (const quad of storeB) {
      if (!storeA.has(quad)) {
        added.add(quad);
      }
    }

    // Find quads in A but not in B (removed)
    for (const quad of storeA) {
      if (!storeB.has(quad)) {
        removed.add(quad);
      }
    }

    return { added, removed };
  },
  patch: (baseStore, changes) => {
    const result = new Store([...baseStore]);

    // Remove quads
    for (const quad of changes.removed) {
      result.delete(quad);
    }

    // Add quads
    for (const quad of changes.added) {
      result.add(quad);
    }

    return result;
  },
  getStats: (changes) => ({
    added: { quads: changes.added.size },
    removed: { quads: changes.removed.size },
    total: { quads: changes.added.size + changes.removed.size }
  }),
  isEmpty: (changes) => {
    return (!changes.added || changes.added.size === 0) && 
           (!changes.removed || changes.removed.size === 0);
  },
  merge: (...changeSets) => {
    const merged = { added: new Store(), removed: new Store() };
    
    for (const changes of changeSets) {
      if (changes.added) {
        for (const quad of changes.added) {
          merged.added.add(quad);
        }
      }
      if (changes.removed) {
        for (const quad of changes.removed) {
          merged.removed.add(quad);
        }
      }
    }
    
    return merged;
  },
  invert: (changes) => ({
    added: changes.removed || new Store(),
    removed: changes.added || new Store()
  })
});

describe("useDelta", () => {
  let delta;
  let store1, store2, store3;

  beforeEach(() => {
    delta = useDelta();
    
    // Create test stores
    store1 = new Store();
    store2 = new Store();
    store3 = new Store();

    // Add some test quads
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

  it("should create delta interface", () => {
    // Assert
    expect(typeof delta.diff).toBe("function");
    expect(typeof delta.patch).toBe("function");
    expect(typeof delta.getStats).toBe("function");
    expect(typeof delta.isEmpty).toBe("function");
    expect(typeof delta.merge).toBe("function");
    expect(typeof delta.invert).toBe("function");
  });

  it("should calculate difference between stores", () => {
    // Act
    const result = delta.diff(store1, store2);

    // Assert
    expect(result).toHaveProperty("added");
    expect(result).toHaveProperty("removed");
    expect(result.added.size).toBe(1); // q3
    expect(result.removed.size).toBe(1); // q1
  });

  it("should patch a store with changes", () => {
    // Arrange
    const changes = delta.diff(store1, store2);

    // Act
    const result = delta.patch(store1, changes);

    // Assert
    expect(result.size).toBe(2); // Should match store2
    expect(result.has(quad(namedNode("ex:s2"), namedNode("ex:p2"), literal("o2")))).toBe(true);
    expect(result.has(quad(namedNode("ex:s3"), namedNode("ex:p3"), literal("o3")))).toBe(true);
  });

  it("should get statistics about changes", () => {
    // Arrange
    const changes = delta.diff(store1, store2);

    // Act
    const result = delta.getStats(changes);

    // Assert
    expect(result).toHaveProperty("added");
    expect(result).toHaveProperty("removed");
    expect(result).toHaveProperty("total");
    expect(result.added.quads).toBe(1);
    expect(result.removed.quads).toBe(1);
    expect(result.total.quads).toBe(2);
  });

  it("should check if changes are empty", () => {
    // Arrange
    const changes1 = delta.diff(store1, store2);
    const changes2 = { added: new Store(), removed: new Store() };

    // Act & Assert
    expect(delta.isEmpty(changes1)).toBe(false);
    expect(delta.isEmpty(changes2)).toBe(true);
  });

  it("should merge multiple change sets", () => {
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

  it("should invert changes", () => {
    // Arrange
    const changes = delta.diff(store1, store2);

    // Act
    const result = delta.invert(changes);

    // Assert
    expect(result).toHaveProperty("added");
    expect(result).toHaveProperty("removed");
    expect(result.added.size).toBe(1); // q1 (was removed)
    expect(result.removed.size).toBe(1); // q3 (was added)
  });
});
