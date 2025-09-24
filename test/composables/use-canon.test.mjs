/**
 * @fileoverview Tests for useCanon composable with context architecture
 * 
 * Tests the canonicalization functionality using the context system
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, expect, it, beforeEach } from "vitest";
import { useCanon } from "../../src/composables/use-canon.mjs";
import { useStore } from "../../src/composables/use-store.mjs";
import { initStore } from "../../src/context/index.mjs";
import { Store, DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

describe("useCanon with Context", () => {
  let runApp;

  beforeEach(() => {
    // Create test data
    const testQuads = [
      quad(
        namedNode("http://example.org/person1"),
        namedNode("http://xmlns.com/foaf/0.1/name"),
        literal("John Doe")
      ),
      quad(
        namedNode("http://example.org/person1"),
        namedNode("http://xmlns.com/foaf/0.1/age"),
        literal("30", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
      )
    ];
    
    runApp = initStore(testQuads, { baseIRI: "http://example.org/" });
  });

  it("should create canon interface with context", async () => {
    await runApp(() => {
      // Act
      const canon = useCanon();
      
      // Assert
      expect(typeof canon.canonicalize).toBe("function");
      expect(typeof canon.isIsomorphic).toBe("function");
      expect(typeof canon.hash).toBe("function");
      expect(canon.engine).toBeDefined();
    });
  });

  it("should canonicalize a store using context engine", async () => {
    await runApp(async () => {
      // Arrange
      const canon = useCanon();
      const store = useStore();
      
      // Act
      const canonical = await canon.canonicalize(store.store);
      
      // Assert
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(0);
      expect(canonical).toContain("http://example.org/person1");
      expect(canonical).toContain("John Doe");
    });
  });

  it("should check isomorphism between stores", async () => {
    await runApp(async () => {
      // Arrange
      const canon = useCanon();
      const store1 = useStore();
      
      // Create identical store
      const store2 = new Store([...store1.store]);
      
      // Act
      const isIsomorphic = await canon.isIsomorphic(store1.store, store2);
      
      // Assert
      expect(isIsomorphic).toBe(true);
    });
  });

  it("should generate canonical hash", async () => {
    await runApp(async () => {
      // Arrange
      const canon = useCanon();
      const store = useStore();
      
      // Act
      const hash = await canon.hash(store.store);
      
      // Assert
      expect(typeof hash).toBe("string");
      expect(hash.length).toBeGreaterThan(0);
    });
  });

  it("should handle empty stores", async () => {
    const runAppEmpty = initStore([], { baseIRI: "http://example.org/" });
    
    await runAppEmpty(async () => {
      // Arrange
      const canon = useCanon();
      const emptyStore = new Store();
      
      // Act
      const canonical = await canon.canonicalize(emptyStore);
      const hash = await canon.hash(emptyStore);
      
      // Assert
      expect(canonical).toBe("");
      expect(typeof hash).toBe("string");
    });
  });

  it("should throw error when context is not initialized", () => {
    // Act & Assert
    expect(() => {
      useCanon();
    }).toThrow();
  });

  it("should work with different base IRIs", async () => {
    const runAppCustom = initStore([], { baseIRI: "http://custom.org/" });
    
    await runAppCustom(() => {
      // Act
      const canon = useCanon();
      
      // Assert
      expect(canon.engine).toBeDefined();
      expect(typeof canon.canonicalize).toBe("function");
    });
  });
});