import { describe, expect, it, beforeEach } from "vitest";
import { useStore, initStore } from "../../src/index.mjs";

/**
 * @fileoverview Tests for useStore composable
 * 
 * Tests the core store management functionality with context support
 */

describe("useStore", () => {
  it("should create a store from context", async () => {
    const runApp = initStore([], { baseIRI: "http://example.org/" });
    
    await runApp(async () => {
      const store = useStore();
      
      // Assert - verify the store is created
      expect(store.store).toBeDefined();
      expect(store.store.size).toBe(0);
      expect(typeof store.add).toBe("function");
      expect(typeof store.remove).toBe("function");
      expect(typeof store.clear).toBe("function");
      expect(typeof store.stats).toBe("function");
      expect(typeof store.serialize).toBe("function");
    });
  });

  it("should add quads to the store", async () => {
    const runApp = initStore([], { baseIRI: "http://example.org/" });
    
    await runApp(async () => {
      const store = useStore();
      const testQuad = store.quad(
        store.namedNode("http://example.org/subject"),
        store.namedNode("http://example.org/predicate"),
        store.literal("object")
      );
      
      // Act
      store.add(testQuad);
      
      // Assert
      expect(store.stats().quads).toBe(1);
      expect(store.store.has(testQuad)).toBe(true);
    });
  });

  it("should remove quads from the store", async () => {
    const runApp = initStore([], { baseIRI: "http://example.org/" });
    
    await runApp(async () => {
      const store = useStore();
      const testQuad = store.quad(
        store.namedNode("http://example.org/subject"),
        store.namedNode("http://example.org/predicate"),
        store.literal("object")
      );
      
      // Act
      store.add(testQuad);
      expect(store.stats().quads).toBe(1);
      
      store.remove(testQuad);
      
      // Assert
      expect(store.stats().quads).toBe(0);
      expect(store.store.has(testQuad)).toBe(false);
    });
  });

  it("should clear all quads from the store", async () => {
    const runApp = initStore([], { baseIRI: "http://example.org/" });
    
    await runApp(async () => {
      const store = useStore();
      const testQuads = [
        store.quad(store.namedNode("http://example.org/s1"), store.namedNode("http://example.org/p1"), store.literal("o1")),
        store.quad(store.namedNode("http://example.org/s2"), store.namedNode("http://example.org/p2"), store.literal("o2"))
      ];
      
      // Act
      store.add(...testQuads);
      expect(store.stats().quads).toBe(2);
      
      store.clear();
      
      // Assert
      expect(store.stats().quads).toBe(0);
    });
  });
});
