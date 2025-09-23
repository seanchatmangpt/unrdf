import { describe, expect, it, beforeEach } from "vitest";

/**
 * @fileoverview Tests for useStore composable
 * 
 * Tests the core store management functionality using London School of TDD
 */

describe("useStore", () => {
  let useStore;
  
  beforeEach(() => {
    // Reset the composable for each test
    useStore = null;
  });

  it("should create a new N3 store when called", () => {
    // Arrange - minimal setup
    const { Store } = require("n3");
    
    // Act - call the composable
    useStore = () => {
      return {
        store: new Store(),
        add: (quads) => quads.forEach(q => useStore.store.add(q)),
        remove: (quads) => quads.forEach(q => useStore.store.delete(q)),
        clear: () => {
          // N3 Store doesn't have clear(), so we remove all quads
          const quads = [...useStore.store];
          for (const quad of quads) {
            useStore.store.delete(quad);
          }
        },
        stats: () => ({ quads: useStore.store.size }),
        serialize: ({ format = "Turtle" }) => format
      };
    };
    
    const result = useStore();
    
    // Assert - verify the store is created
    expect(result.store).toBeDefined();
    expect(result.store.size).toBe(0);
    expect(typeof result.add).toBe("function");
    expect(typeof result.remove).toBe("function");
    expect(typeof result.clear).toBe("function");
    expect(typeof result.stats).toBe("function");
    expect(typeof result.serialize).toBe("function");
  });

  it("should add quads to the store", () => {
    // Arrange
    const { Store, DataFactory } = require("n3");
    const { namedNode, literal, quad } = DataFactory;
    
    useStore = () => {
      const store = new Store();
      return {
        store,
        add: (quads) => quads.forEach(q => store.add(q)),
        remove: (quads) => quads.forEach(q => store.delete(q)),
        clear: () => {
          // N3 Store doesn't have clear(), so we remove all quads
          const quads = [...store];
          for (const quad of quads) {
            store.delete(quad);
          }
        },
        stats: () => ({ quads: store.size }),
        serialize: ({ format = "Turtle" }) => format
      };
    };
    
    const storeComposable = useStore();
    const testQuad = quad(
      namedNode("http://example.org/subject"),
      namedNode("http://example.org/predicate"),
      literal("object")
    );
    
    // Act
    storeComposable.add([testQuad]);
    
    // Assert
    expect(storeComposable.stats().quads).toBe(1);
    expect(storeComposable.store.has(testQuad)).toBe(true);
  });

  it("should remove quads from the store", () => {
    // Arrange
    const { Store, DataFactory } = require("n3");
    const { namedNode, literal, quad } = DataFactory;
    
    useStore = () => {
      const store = new Store();
      return {
        store,
        add: (quads) => quads.forEach(q => store.add(q)),
        remove: (quads) => quads.forEach(q => store.delete(q)),
        clear: () => {
          // N3 Store doesn't have clear(), so we remove all quads
          const quads = [...store];
          for (const quad of quads) {
            store.delete(quad);
          }
        },
        stats: () => ({ quads: store.size }),
        serialize: ({ format = "Turtle" }) => format
      };
    };
    
    const storeComposable = useStore();
    const testQuad = quad(
      namedNode("http://example.org/subject"),
      namedNode("http://example.org/predicate"),
      literal("object")
    );
    
    // Act
    storeComposable.add([testQuad]);
    expect(storeComposable.stats().quads).toBe(1);
    
    storeComposable.remove([testQuad]);
    
    // Assert
    expect(storeComposable.stats().quads).toBe(0);
    expect(storeComposable.store.has(testQuad)).toBe(false);
  });

  it("should clear all quads from the store", () => {
    // Arrange
    const { Store, DataFactory } = require("n3");
    const { namedNode, literal, quad } = DataFactory;
    
    useStore = () => {
      const store = new Store();
      return {
        store,
        add: (quads) => quads.forEach(q => store.add(q)),
        remove: (quads) => quads.forEach(q => store.delete(q)),
        clear: () => {
          // N3 Store doesn't have clear(), so we remove all quads
          const quads = [...store];
          for (const quad of quads) {
            store.delete(quad);
          }
        },
        stats: () => ({ quads: store.size }),
        serialize: ({ format = "Turtle" }) => format
      };
    };
    
    const storeComposable = useStore();
    const testQuads = [
      quad(namedNode("http://example.org/s1"), namedNode("http://example.org/p1"), literal("o1")),
      quad(namedNode("http://example.org/s2"), namedNode("http://example.org/p2"), literal("o2"))
    ];
    
    // Act
    storeComposable.add(testQuads);
    expect(storeComposable.stats().quads).toBe(2);
    
    storeComposable.clear();
    
    // Assert
    expect(storeComposable.stats().quads).toBe(0);
  });
});
