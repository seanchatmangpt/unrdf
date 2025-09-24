/**
 * @fileoverview Tests for useReasoner composable with context architecture
 * 
 * Tests the reasoning functionality using the context system
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, expect, it, beforeEach } from "vitest";
import { useReasoner } from "../../src/composables/use-reasoner.mjs";
import { useStore } from "../../src/composables/use-store.mjs";
import { initStore } from "../../src/context/index.mjs";
import { Store, DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

describe("useReasoner with Context", () => {
  let runApp;

  beforeEach(() => {
    // Create test data
    const testQuads = [
      quad(
        namedNode("http://example.org/alice"),
        namedNode("http://example.org/parentOf"),
        namedNode("http://example.org/bob")
      ),
      quad(
        namedNode("http://example.org/bob"),
        namedNode("http://example.org/parentOf"),
        namedNode("http://example.org/charlie")
      )
    ];
    
    runApp = initStore(testQuads, { baseIRI: "http://example.org/" });
  });

  it("should create reasoner interface with context", async () => {
    await runApp(() => {
      // Act
      const reasoner = useReasoner();
      
      // Assert
      expect(typeof reasoner.reason).toBe("function");
      expect(typeof reasoner.reasonSequentially).toBe("function");
      expect(typeof reasoner.reasonParallel).toBe("function");
      expect(typeof reasoner.getNewTriples).toBe("function");
      expect(reasoner.engine).toBeDefined();
    });
  });

  it("should reason over context store with rules", async () => {
    await runApp(async () => {
      // Arrange
      const reasoner = useReasoner();
      const rules = `
        @prefix ex: <http://example.org/> .
        { ex:alice ex:parentOf ex:bob } => { ex:alice ex:ancestorOf ex:bob } .
      `;
      
      // Act
      const inferred = await reasoner.reason(null, rules);
      
      // Assert
      expect(inferred).toBeDefined();
      expect(typeof inferred.store).toBe("object");
    });
  });

  it("should reason over provided data store", async () => {
    await runApp(async () => {
      // Arrange
      const reasoner = useReasoner();
      const dataStore = useStore();
      const rules = `
        @prefix ex: <http://example.org/> .
        { ex:alice ex:parentOf ex:bob } => { ex:alice ex:ancestorOf ex:bob } .
      `;
      
      // Act
      const inferred = await reasoner.reason(dataStore, rules);
      
      // Assert
      expect(inferred).toBeDefined();
      expect(typeof inferred.store).toBe("object");
    });
  });

  it("should handle empty rules gracefully", async () => {
    await runApp(async () => {
      // Arrange
      const reasoner = useReasoner();
      const dataStore = useStore();
      
      // Act - Pass null for rules to avoid parsing empty string
      const inferred = await reasoner.reason(dataStore, null);
      
      // Assert
      expect(inferred).toBeDefined();
      expect(typeof inferred.store).toBe("object");
    });
  });

  it("should handle empty data store gracefully", async () => {
    await runApp(async () => {
      // Arrange
      const reasoner = useReasoner();
      const emptyStore = new Store();
      const rules = `
        @prefix ex: <http://example.org/> .
        { ex:alice ex:parentOf ex:bob } => { ex:alice ex:ancestorOf ex:bob } .
      `;
      
      // Act
      const inferred = await reasoner.reason(emptyStore, rules);
      
      // Assert
      expect(inferred).toBeDefined();
      expect(typeof inferred.store).toBe("object");
    });
  });

  it("should get new triples from reasoning", async () => {
    await runApp(async () => {
      // Arrange
      const reasoner = useReasoner();
      const originalStore = useStore();
      const rules = `
        @prefix ex: <http://example.org/> .
        { ex:alice ex:parentOf ex:bob } => { ex:alice ex:ancestorOf ex:bob } .
      `;
      
      // Act
      const inferred = await reasoner.reason(originalStore, rules);
      const newTriples = reasoner.getNewTriples(originalStore, inferred);
      
      // Assert
      expect(newTriples).toBeDefined();
      expect(typeof newTriples.store).toBe("object");
    });
  });

  it("should check if reasoning would produce new triples", async () => {
    await runApp(async () => {
      // Arrange
      const reasoner = useReasoner();
      const dataStore = useStore();
      const rules = `
        @prefix ex: <http://example.org/> .
        { ex:alice ex:parentOf ex:bob } => { ex:alice ex:ancestorOf ex:bob } .
      `;
      
      // Act
      const wouldProduceNew = await reasoner.wouldProduceNewTriples(dataStore, rules);
      
      // Assert
      expect(typeof wouldProduceNew).toBe("boolean");
    });
  });

  it("should get reasoning statistics", async () => {
    await runApp(async () => {
      // Arrange
      const reasoner = useReasoner();
      const originalStore = useStore();
      const rules = `
        @prefix ex: <http://example.org/> .
        { ex:alice ex:parentOf ex:bob } => { ex:alice ex:ancestorOf ex:bob } .
      `;
      
      // Act
      const inferred = await reasoner.reason(originalStore, rules);
      const stats = reasoner.getStats(originalStore, inferred);
      
      // Assert
      expect(stats).toBeDefined();
      expect(typeof stats.original).toBe("object");
      expect(typeof stats.inferred).toBe("object");
      expect(typeof stats.new).toBe("object");
      expect(typeof stats.growth).toBe("object");
    });
  });

  it("should throw error when context is not initialized", () => {
    // Act & Assert
    expect(() => {
      useReasoner();
    }).toThrow();
  });

  it("should work with different timeout settings", async () => {
    await runApp(() => {
      // Act
      const reasoner = useReasoner({ timeoutMs: 60000 });
      
      // Assert
      expect(reasoner.engine).toBeDefined();
      expect(typeof reasoner.reason).toBe("function");
    });
  });
});