/**
 * @fileoverview Unit tests for knowledge-engine/canonicalize.mjs
 * Tests canonicalization and isomorphism functionality
 */

import { describe, it, expect, beforeEach } from "vitest";
import { Store, DataFactory } from "n3";
import {
  canonicalize,
  isIsomorphic,
  getCanonicalHash,
  groupByIsomorphism,
  findDuplicates,
  getCanonicalizationStats,
  createCanonicalizationSession
} from "../../src/knowledge-engine/canonicalize.mjs";

const { namedNode, literal, quad, blankNode } = DataFactory;

describe("canonicalize.mjs", () => {
  let testStore;
  let isomorphicStore;
  let differentStore;

  beforeEach(() => {
    testStore = new Store();
    // Add test data
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/name"),
      literal("Alice")
    );
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/age"),
      literal("30", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
    );
    testStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/knows"),
      namedNode("http://example.org/bob")
    );

    // Create isomorphic store (same data, different order)
    isomorphicStore = new Store();
    isomorphicStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/knows"),
      namedNode("http://example.org/bob")
    );
    isomorphicStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/age"),
      literal("30", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
    );
    isomorphicStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/name"),
      literal("Alice")
    );

    // Create different store
    differentStore = new Store();
    differentStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/name"),
      literal("Bob") // Different value
    );
    differentStore.addQuad(
      namedNode("http://example.org/alice"),
      namedNode("http://example.org/age"),
      literal("25", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
    );
  });

  describe("canonicalize", () => {
    it("should canonicalize a store", async () => {
      const canonical = await canonicalize(testStore);
      
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(0);
      expect(canonical).toContain("<http://example.org/alice>");
    });

    it("should produce consistent canonical form", async () => {
      const canonical1 = await canonicalize(testStore);
      const canonical2 = await canonicalize(testStore);
      
      expect(canonical1).toBe(canonical2);
    });

    it("should produce same canonical form for isomorphic stores", async () => {
      const canonical1 = await canonicalize(testStore);
      const canonical2 = await canonicalize(isomorphicStore);
      
      expect(canonical1).toBe(canonical2);
    });

    it("should produce different canonical forms for different stores", async () => {
      const canonical1 = await canonicalize(testStore);
      const canonical2 = await canonicalize(differentStore);
      
      expect(canonical1).not.toBe(canonical2);
    });

    it("should handle canonicalization options", async () => {
      const canonical = await canonicalize(testStore, {
        algorithm: 'URDNA2015',
        produceGeneralizedRdf: false,
        timeoutMs: 10000
      });
      
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(0);
    });

    it("should handle empty store", async () => {
      const emptyStore = new Store();
      const canonical = await canonicalize(emptyStore);
      
      expect(canonical).toBe("");
    });

    it("should throw error for invalid store", async () => {
      await expect(canonicalize(null)).rejects.toThrow("canonicalize: store must be a valid Store instance");
      await expect(canonicalize("invalid")).rejects.toThrow("canonicalize: store must be a valid Store instance");
    });

    it("should handle stores with blank nodes", async () => {
      const blankNodeStore = new Store();
      blankNodeStore.addQuad(
        namedNode("http://example.org/person"),
        namedNode("http://example.org/address"),
        blankNode("addr1")
      );
      blankNodeStore.addQuad(
        blankNode("addr1"),
        namedNode("http://example.org/street"),
        literal("123 Main St")
      );

      const canonical = await canonicalize(blankNodeStore);
      
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(0);
    });

    it("should handle stores with different datatypes", async () => {
      const typedStore = new Store();
      typedStore.addQuad(
        namedNode("http://example.org/item"),
        namedNode("http://example.org/price"),
        literal("10.50", namedNode("http://www.w3.org/2001/XMLSchema#decimal"))
      );
      typedStore.addQuad(
        namedNode("http://example.org/item"),
        namedNode("http://example.org/quantity"),
        literal("5", namedNode("http://www.w3.org/2001/XMLSchema#integer"))
      );
      typedStore.addQuad(
        namedNode("http://example.org/item"),
        namedNode("http://example.org/active"),
        literal("true", namedNode("http://www.w3.org/2001/XMLSchema#boolean"))
      );

      const canonical = await canonicalize(typedStore);
      
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(0);
    });
  });

  describe("isIsomorphic", () => {
    it("should return true for identical stores", async () => {
      const result = await isIsomorphic(testStore, testStore);
      expect(result).toBe(true);
    });

    it("should return true for isomorphic stores", async () => {
      const result = await isIsomorphic(testStore, isomorphicStore);
      expect(result).toBe(true);
    });

    it("should return false for different stores", async () => {
      const result = await isIsomorphic(testStore, differentStore);
      expect(result).toBe(false);
    });

    it("should return false for stores with different sizes", async () => {
      const smallStore = new Store();
      smallStore.addQuad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/name"),
        literal("Alice")
      );

      const result = await isIsomorphic(testStore, smallStore);
      expect(result).toBe(false);
    });

    it("should handle empty stores", async () => {
      const emptyStore1 = new Store();
      const emptyStore2 = new Store();
      
      const result = await isIsomorphic(emptyStore1, emptyStore2);
      expect(result).toBe(true);
    });

    it("should handle isomorphism options", async () => {
      const result = await isIsomorphic(testStore, isomorphicStore, {
        algorithm: 'URDNA2015',
        timeoutMs: 5000
      });
      expect(result).toBe(true);
    });

    it("should throw error for invalid stores", async () => {
      await expect(isIsomorphic(null, testStore)).rejects.toThrow("isIsomorphic: storeA must be a valid Store instance");
      await expect(isIsomorphic(testStore, null)).rejects.toThrow("isIsomorphic: storeB must be a valid Store instance");
    });

    it("should handle stores with blank nodes", async () => {
      const blankNodeStore1 = new Store();
      blankNodeStore1.addQuad(
        namedNode("http://example.org/person"),
        namedNode("http://example.org/address"),
        blankNode("addr1")
      );
      blankNodeStore1.addQuad(
        blankNode("addr1"),
        namedNode("http://example.org/street"),
        literal("123 Main St")
      );

      const blankNodeStore2 = new Store();
      blankNodeStore2.addQuad(
        namedNode("http://example.org/person"),
        namedNode("http://example.org/address"),
        blankNode("addr2") // Different blank node label
      );
      blankNodeStore2.addQuad(
        blankNode("addr2"),
        namedNode("http://example.org/street"),
        literal("123 Main St")
      );

      const result = await isIsomorphic(blankNodeStore1, blankNodeStore2);
      expect(result).toBe(true); // Should be isomorphic despite different blank node labels
    });
  });

  describe("getCanonicalHash", () => {
    it("should return a hash for a store", async () => {
      const hash = await getCanonicalHash(testStore);
      
      expect(typeof hash).toBe("string");
      expect(hash.length).toBeGreaterThan(0);
    });

    it("should return same hash for identical stores", async () => {
      const hash1 = await getCanonicalHash(testStore);
      const hash2 = await getCanonicalHash(testStore);
      
      expect(hash1).toBe(hash2);
    });

    it("should return same hash for isomorphic stores", async () => {
      const hash1 = await getCanonicalHash(testStore);
      const hash2 = await getCanonicalHash(isomorphicStore);
      
      expect(hash1).toBe(hash2);
    });

    it("should return different hashes for different stores", async () => {
      const hash1 = await getCanonicalHash(testStore);
      const hash2 = await getCanonicalHash(differentStore);
      
      expect(hash1).not.toBe(hash2);
    });

    it("should handle hash options", async () => {
      const hash = await getCanonicalHash(testStore, {
        algorithm: 'URDNA2015',
        hashAlgorithm: 'sha256'
      });
      
      expect(typeof hash).toBe("string");
      expect(hash.length).toBeGreaterThan(0);
    });

    it("should throw error for invalid store", async () => {
      await expect(getCanonicalHash(null)).rejects.toThrow("getCanonicalHash: store must be a valid Store instance");
    });
  });

  describe("groupByIsomorphism", () => {
    it("should group isomorphic stores", async () => {
      const stores = [testStore, isomorphicStore, differentStore];
      const groups = await groupByIsomorphism(stores);
      
      expect(Array.isArray(groups)).toBe(true);
      expect(groups.length).toBe(2); // testStore and isomorphicStore should be in same group
      
      // Find the group containing testStore
      const testGroup = groups.find(group => 
        group.stores.some(store => store === testStore)
      );
      expect(testGroup).toBeDefined();
      expect(testGroup.stores).toContain(testStore);
      expect(testGroup.stores).toContain(isomorphicStore);
      expect(testGroup.stores).not.toContain(differentStore);
    });

    it("should handle empty stores array", async () => {
      const groups = await groupByIsomorphism([]);
      
      expect(Array.isArray(groups)).toBe(true);
      expect(groups.length).toBe(0);
    });

    it("should handle single store", async () => {
      const groups = await groupByIsomorphism([testStore]);
      
      expect(Array.isArray(groups)).toBe(true);
      expect(groups.length).toBe(1);
      expect(groups[0].stores).toContain(testStore);
    });

    it("should handle grouping options", async () => {
      const stores = [testStore, isomorphicStore];
      const groups = await groupByIsomorphism(stores, {
        algorithm: 'URDNA2015',
        timeoutMs: 5000
      });
      
      expect(Array.isArray(groups)).toBe(true);
      expect(groups.length).toBe(1);
    });

    it("should throw error for invalid stores", async () => {
      await expect(groupByIsomorphism([null, testStore])).rejects.toThrow();
    });
  });

  describe("findDuplicates", () => {
    it("should find duplicate stores", async () => {
      const stores = [testStore, isomorphicStore, differentStore];
      const duplicates = await findDuplicates(stores);
      
      expect(Array.isArray(duplicates)).toBe(true);
      expect(duplicates.length).toBe(1); // testStore and isomorphicStore are duplicates
      
      const duplicate = duplicates[0];
      expect(duplicate).toHaveProperty("stores");
      expect(duplicate).toHaveProperty("canonicalHash");
      expect(Array.isArray(duplicate.stores)).toBe(true);
      expect(duplicate.stores).toContain(testStore);
      expect(duplicate.stores).toContain(isomorphicStore);
    });

    it("should handle no duplicates", async () => {
      const stores = [testStore, differentStore];
      const duplicates = await findDuplicates(stores);
      
      expect(Array.isArray(duplicates)).toBe(true);
      expect(duplicates.length).toBe(0);
    });

    it("should handle empty stores array", async () => {
      const duplicates = await findDuplicates([]);
      
      expect(Array.isArray(duplicates)).toBe(true);
      expect(duplicates.length).toBe(0);
    });

    it("should handle duplicate options", async () => {
      const stores = [testStore, isomorphicStore];
      const duplicates = await findDuplicates(stores, {
        algorithm: 'URDNA2015',
        timeoutMs: 5000
      });
      
      expect(Array.isArray(duplicates)).toBe(true);
      expect(duplicates.length).toBe(1);
    });
  });

  describe("getCanonicalizationStats", () => {
    it("should return canonicalization statistics", async () => {
      const stats = await getCanonicalizationStats(testStore);
      
      expect(stats).toHaveProperty("quads");
      expect(stats).toHaveProperty("canonicalLength");
      expect(stats).toHaveProperty("canonicalizationTime");
      expect(stats).toHaveProperty("algorithm");
      
      expect(typeof stats.quads).toBe("number");
      expect(typeof stats.canonicalLength).toBe("number");
      expect(typeof stats.canonicalizationTime).toBe("number");
      expect(typeof stats.algorithm).toBe("string");
      
      expect(stats.quads).toBe(testStore.size);
      expect(stats.canonicalLength).toBeGreaterThan(0);
      expect(stats.canonicalizationTime).toBeGreaterThan(0);
    });

    it("should handle stats options", async () => {
      const stats = await getCanonicalizationStats(testStore, {
        algorithm: 'URDNA2015',
        includeTiming: true
      });
      
      expect(stats).toHaveProperty("quads");
      expect(stats).toHaveProperty("canonicalLength");
      expect(stats).toHaveProperty("canonicalizationTime");
    });

    it("should throw error for invalid store", async () => {
      await expect(getCanonicalizationStats(null)).rejects.toThrow("getCanonicalizationStats: store must be a valid Store instance");
    });
  });

  describe("createCanonicalizationSession", () => {
    it("should create a canonicalization session", async () => {
      const session = await createCanonicalizationSession();
      
      expect(session).toHaveProperty("canonicalize");
      expect(session).toHaveProperty("isIsomorphic");
      expect(session).toHaveProperty("getCanonicalHash");
      expect(session).toHaveProperty("getStats");
      expect(typeof session.canonicalize).toBe("function");
      expect(typeof session.isIsomorphic).toBe("function");
      expect(typeof session.getCanonicalHash).toBe("function");
      expect(typeof session.getStats).toBe("function");
    });

    it("should canonicalize stores in session", async () => {
      const session = await createCanonicalizationSession();
      
      const canonical = await session.canonicalize(testStore);
      
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(0);
    });

    it("should check isomorphism in session", async () => {
      const session = await createCanonicalizationSession();
      
      const result = await session.isIsomorphic(testStore, isomorphicStore);
      expect(result).toBe(true);
    });

    it("should get canonical hash in session", async () => {
      const session = await createCanonicalizationSession();
      
      const hash = await session.getCanonicalHash(testStore);
      
      expect(typeof hash).toBe("string");
      expect(hash.length).toBeGreaterThan(0);
    });

    it("should provide session statistics", async () => {
      const session = await createCanonicalizationSession();
      
      await session.canonicalize(testStore);
      await session.isIsomorphic(testStore, isomorphicStore);
      
      const stats = session.getStats();
      
      expect(stats).toHaveProperty("canonicalizations");
      expect(stats).toHaveProperty("isomorphismChecks");
      expect(stats).toHaveProperty("totalTime");
      expect(typeof stats.canonicalizations).toBe("number");
      expect(typeof stats.isomorphismChecks).toBe("number");
      expect(typeof stats.totalTime).toBe("number");
    });

    it("should handle session options", async () => {
      const session = await createCanonicalizationSession({
        algorithm: 'URDNA2015',
        timeoutMs: 10000
      });
      
      expect(session).toHaveProperty("canonicalize");
      expect(session).toHaveProperty("isIsomorphic");
    });
  });

  describe("edge cases", () => {
    it("should handle large stores", async () => {
      const largeStore = new Store();
      for (let i = 0; i < 1000; i++) {
        largeStore.addQuad(
          namedNode(`http://example.org/resource${i}`),
          namedNode("http://example.org/hasValue"),
          literal(`value${i}`)
        );
      }

      const canonical = await canonicalize(largeStore);
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(1000);
    });

    it("should handle stores with many blank nodes", async () => {
      const blankNodeStore = new Store();
      for (let i = 0; i < 100; i++) {
        const blank = blankNode(`b${i}`);
        blankNodeStore.addQuad(
          namedNode(`http://example.org/person${i}`),
          namedNode("http://example.org/address"),
          blank
        );
        blankNodeStore.addQuad(
          blank,
          namedNode("http://example.org/street"),
          literal(`Street ${i}`)
        );
      }

      const canonical = await canonicalize(blankNodeStore);
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(0);
    });

    it("should handle concurrent canonicalization", async () => {
      const promises = [
        canonicalize(testStore),
        canonicalize(isomorphicStore),
        canonicalize(differentStore)
      ];

      const results = await Promise.all(promises);
      
      expect(results).toHaveLength(3);
      expect(results[0]).toBe(results[1]); // testStore and isomorphicStore should have same canonical form
      expect(results[0]).not.toBe(results[2]); // differentStore should have different canonical form
    });

    it("should handle stores with language tags", async () => {
      const langStore = new Store();
      langStore.addQuad(
        namedNode("http://example.org/person"),
        namedNode("http://example.org/name"),
        literal("John", "en")
      );
      langStore.addQuad(
        namedNode("http://example.org/person"),
        namedNode("http://example.org/name"),
        literal("Juan", "es")
      );

      const canonical = await canonicalize(langStore);
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(0);
    });

    it("should handle stores with complex datatypes", async () => {
      const complexStore = new Store();
      complexStore.addQuad(
        namedNode("http://example.org/event"),
        namedNode("http://example.org/startTime"),
        literal("2024-01-01T10:00:00Z", namedNode("http://www.w3.org/2001/XMLSchema#dateTime"))
      );
      complexStore.addQuad(
        namedNode("http://example.org/event"),
        namedNode("http://example.org/duration"),
        literal("PT1H30M", namedNode("http://www.w3.org/2001/XMLSchema#duration"))
      );

      const canonical = await canonicalize(complexStore);
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(0);
    });

    it("should handle timeout scenarios", async () => {
      const largeStore = new Store();
      for (let i = 0; i < 10000; i++) {
        largeStore.addQuad(
          namedNode(`http://example.org/resource${i}`),
          namedNode("http://example.org/hasValue"),
          literal(`value${i}`)
        );
      }

      // This should either complete or timeout gracefully
      try {
        const canonical = await canonicalize(largeStore, { timeoutMs: 100 });
        expect(typeof canonical).toBe("string");
      } catch (error) {
        expect(error.message).toContain("timeout");
      }
    });
  });
});
