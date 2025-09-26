/**
 * @fileoverview Unit tests for knowledge-engine.mjs
 * Tests main integration and factory functions
 */

import { describe, it, expect, beforeEach } from "vitest";
import { Store, DataFactory } from "n3";
import {
  createKnowledgeEngine,
  createSimpleEngine,
  createStrictEngine,
  // Import individual functions for testing
  parseTurtle,
  toTurtle,
  query,
  validateShacl,
  reason,
  canonicalize,
  isIsomorphic,
  TransactionManager
} from "../../src/knowledge-engine.mjs";

const { namedNode, literal, quad } = DataFactory;

describe("knowledge-engine.mjs", () => {
  let testStore;
  let testTtl;
  let testShapes;
  let testRules;

  beforeEach(() => {
    testStore = new Store();
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

    testTtl = `
      @prefix ex: <http://example.org/> .
      ex:alice ex:name "Alice" ;
               ex:age 30 ;
               ex:knows ex:bob .
    `;

    testShapes = `
      @prefix sh: <http://www.w3.org/ns/shacl#> .
      @prefix ex: <http://example.org/> .
      @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
      
      ex:PersonShape a sh:NodeShape ;
        sh:targetClass ex:Person ;
        sh:property [
          sh:path ex:name ;
          sh:minCount 1 ;
          sh:maxCount 1 ;
          sh:datatype xsd:string
        ] .
    `;

    testRules = `
      @prefix ex: <http://example.org/> .
      
      { ?x ex:knows ?y } => { ?y ex:knows ?x } .
    `;
  });

  describe("createKnowledgeEngine", () => {
    it("should create a knowledge engine with default options", () => {
      const engine = createKnowledgeEngine();
      
      expect(engine).toHaveProperty("baseIRI");
      expect(engine).toHaveProperty("strictMode");
      expect(engine).toHaveProperty("maxHooks");
      expect(engine.baseIRI).toBe("http://example.org/");
      expect(engine.strictMode).toBe(false);
      expect(engine.maxHooks).toBe(100);
    });

    it("should create a knowledge engine with custom options", () => {
      const engine = createKnowledgeEngine({
        baseIRI: "http://custom.org/",
        strictMode: true,
        maxHooks: 50
      });
      
      expect(engine.baseIRI).toBe("http://custom.org/");
      expect(engine.strictMode).toBe(true);
      expect(engine.maxHooks).toBe(50);
    });

    it("should provide all engine methods", () => {
      const engine = createKnowledgeEngine();
      
      // Parsing and serialization
      expect(typeof engine.parseTurtle).toBe("function");
      expect(typeof engine.toTurtle).toBe("function");
      expect(typeof engine.toNQuads).toBe("function");
      expect(typeof engine.parseJsonLd).toBe("function");
      expect(typeof engine.toJsonLd).toBe("function");
      
      // SPARQL querying
      expect(typeof engine.query).toBe("function");
      expect(typeof engine.select).toBe("function");
      expect(typeof engine.ask).toBe("function");
      expect(typeof engine.construct).toBe("function");
      expect(typeof engine.describe).toBe("function");
      expect(typeof engine.update).toBe("function");
      expect(typeof engine.getQueryStats).toBe("function");
      
      // SHACL validation
      expect(typeof engine.validateShacl).toBe("function");
      expect(typeof engine.validateShaclMultiple).toBe("function");
      expect(typeof engine.formatValidationReport).toBe("function");
      expect(typeof engine.hasValidationErrors).toBe("function");
      expect(typeof engine.getValidationErrors).toBe("function");
      expect(typeof engine.getValidationWarnings).toBe("function");
      
      // N3 reasoning
      expect(typeof engine.reason).toBe("function");
      expect(typeof engine.reasonMultiple).toBe("function");
      expect(typeof engine.extractInferred).toBe("function");
      expect(typeof engine.getReasoningStats).toBe("function");
      expect(typeof engine.validateRules).toBe("function");
      expect(typeof engine.createReasoningSession).toBe("function");
      
      // Canonicalization and isomorphism
      expect(typeof engine.canonicalize).toBe("function");
      expect(typeof engine.isIsomorphic).toBe("function");
      expect(typeof engine.getCanonicalHash).toBe("function");
      expect(typeof engine.groupByIsomorphism).toBe("function");
      expect(typeof engine.findDuplicates).toBe("function");
      expect(typeof engine.getCanonicalizationStats).toBe("function");
      expect(typeof engine.createCanonicalizationSession).toBe("function");
      
      // Transaction management
      expect(typeof engine.getTransactionManagerClass).toBe("function");
      expect(typeof engine.createTransactionManager).toBe("function");
    });
  });

  describe("createSimpleEngine", () => {
    it("should create a simple engine with default base IRI", () => {
      const engine = createSimpleEngine();
      
      expect(engine.baseIRI).toBe("http://example.org/");
      expect(engine.strictMode).toBe(false);
      expect(engine.maxHooks).toBe(100);
    });

    it("should create a simple engine with custom base IRI", () => {
      const engine = createSimpleEngine("http://custom.org/");
      
      expect(engine.baseIRI).toBe("http://custom.org/");
      expect(engine.strictMode).toBe(false);
      expect(engine.maxHooks).toBe(100);
    });
  });

  describe("createStrictEngine", () => {
    it("should create a strict engine with default base IRI", () => {
      const engine = createStrictEngine();
      
      expect(engine.baseIRI).toBe("http://example.org/");
      expect(engine.strictMode).toBe(true);
      expect(engine.maxHooks).toBe(50);
    });

    it("should create a strict engine with custom base IRI", () => {
      const engine = createStrictEngine("http://strict.org/");
      
      expect(engine.baseIRI).toBe("http://strict.org/");
      expect(engine.strictMode).toBe(true);
      expect(engine.maxHooks).toBe(50);
    });
  });

  describe("engine integration", () => {
    it("should parse Turtle with engine", async () => {
      const engine = createKnowledgeEngine();
      
      const store = await engine.parseTurtle(testTtl);
      
      expect(store).toBeInstanceOf(Store);
      expect(store.size).toBe(3);
    });

    it("should serialize to Turtle with engine", async () => {
      const engine = createKnowledgeEngine();
      
      const ttl = await engine.toTurtle(testStore);
      
      expect(typeof ttl).toBe("string");
      expect(ttl).toContain("Alice");
    });

    it("should query with engine", async () => {
      const engine = createKnowledgeEngine();
      
      const results = await engine.query(testStore, `
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE {
          ?person ex:name ?name .
        }
      `);
      
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBe(1);
      expect(results[0].name).toBe("Alice");
    });

    it("should validate with engine", async () => {
      const engine = createKnowledgeEngine();
      
      // Add type information for validation
      testStore.addQuad(
        namedNode("http://example.org/alice"),
        namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        namedNode("http://example.org/Person")
      );
      
      const report = await engine.validateShacl(testStore, testShapes);
      
      expect(report).toHaveProperty("conforms");
      expect(report).toHaveProperty("results");
    });

    it("should reason with engine", async () => {
      const engine = createKnowledgeEngine();
      
      const reasonedStore = await engine.reason(testStore, testRules);
      
      expect(reasonedStore).toBeInstanceOf(Store);
      expect(reasonedStore.size).toBeGreaterThanOrEqual(testStore.size);
    });

    it("should canonicalize with engine", async () => {
      const engine = createKnowledgeEngine();
      
      const canonical = await engine.canonicalize(testStore);
      
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(0);
    });

    it("should check isomorphism with engine", async () => {
      const engine = createKnowledgeEngine();
      
      const storeCopy = new Store(testStore.getQuads());
      const isIso = await engine.isIsomorphic(testStore, storeCopy);
      
      expect(isIso).toBe(true);
    });

    it("should create transaction manager with engine", async () => {
      const engine = createKnowledgeEngine();
      
      const tx = await engine.createTransactionManager();
      
      expect(tx).toBeInstanceOf(TransactionManager);
    });
  });

  describe("individual function exports", () => {
    it("should export parseTurtle function", async () => {
      const store = await parseTurtle(testTtl);
      
      expect(store).toBeInstanceOf(Store);
      expect(store.size).toBe(3);
    });

    it("should export toTurtle function", async () => {
      const ttl = await toTurtle(testStore);
      
      expect(typeof ttl).toBe("string");
      expect(ttl).toContain("Alice");
    });

    it("should export query function", async () => {
      const results = await query(testStore, `
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE {
          ?person ex:name ?name .
        }
      `);
      
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBe(1);
    });

    it("should export validateShacl function", () => {
      // Add type information for validation
      testStore.addQuad(
        namedNode("http://example.org/alice"),
        namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        namedNode("http://example.org/Person")
      );
      
      const report = validateShacl(testStore, testShapes);
      
      expect(report).toHaveProperty("conforms");
      expect(report).toHaveProperty("results");
    });

    it("should export reason function", async () => {
      const reasonedStore = await reason(testStore, testRules);
      
      expect(reasonedStore).toBeInstanceOf(Store);
      expect(reasonedStore.size).toBeGreaterThanOrEqual(testStore.size);
    });

    it("should export canonicalize function", async () => {
      const canonical = await canonicalize(testStore);
      
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(0);
    });

    it("should export isIsomorphic function", async () => {
      const storeCopy = new Store(testStore.getQuads());
      const isIso = await isIsomorphic(testStore, storeCopy);
      
      expect(isIso).toBe(true);
    });

    it("should export TransactionManager class", () => {
      const tx = new TransactionManager();
      
      expect(tx).toBeInstanceOf(TransactionManager);
    });
  });

  describe("engine workflow", () => {
    it("should support complete workflow", async () => {
      const engine = createKnowledgeEngine();
      
      // 1. Parse data
      const store = await engine.parseTurtle(testTtl);
      expect(store.size).toBe(3);
      
      // 2. Query data
      const results = await engine.query(store, `
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE {
          ?person ex:name ?name .
        }
      `);
      expect(results.length).toBe(1);
      
      // 3. Add type information
      store.addQuad(
        namedNode("http://example.org/alice"),
        namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        namedNode("http://example.org/Person")
      );
      
      // 4. Validate data
      const report = await engine.validateShacl(store, testShapes);
      expect(report).toHaveProperty("conforms");
      
      // 5. Reason over data
      const reasonedStore = await engine.reason(store, testRules);
      expect(reasonedStore.size).toBeGreaterThanOrEqual(store.size);
      
      // 6. Canonicalize result
      const canonical = await engine.canonicalize(reasonedStore);
      expect(typeof canonical).toBe("string");
      
      // 7. Check isomorphism
      const isIso = await engine.isIsomorphic(store, reasonedStore);
      expect(typeof isIso).toBe("boolean");
      
      // 8. Create transaction manager
      const tx = await engine.createTransactionManager();
      expect(tx).toBeInstanceOf(TransactionManager);
    });

    it("should support workflow with strict engine", async () => {
      const engine = createStrictEngine();
      
      // Parse data
      const store = await engine.parseTurtle(testTtl);
      expect(store.size).toBe(3);
      
      // Query data
      const results = await engine.query(store, `
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE {
          ?person ex:name ?name .
        }
      `);
      expect(results.length).toBe(1);
      
      // Create transaction manager
      const tx = await engine.createTransactionManager();
      expect(tx).toBeInstanceOf(TransactionManager);
    });
  });

  describe("error handling", () => {
    it("should handle invalid Turtle in engine", async () => {
      const engine = createKnowledgeEngine();
      
      await expect(engine.parseTurtle("invalid turtle syntax")).rejects.toThrow();
    });

    it("should handle invalid SPARQL in engine", async () => {
      const engine = createKnowledgeEngine();
      
      await expect(engine.query(testStore, "INVALID SPARQL")).rejects.toThrow();
    });

    it("should handle invalid validation in engine", async () => {
      const engine = createKnowledgeEngine();
      
      await expect(engine.validateShacl(testStore, "invalid shapes")).rejects.toThrow();
    });

    it("should handle invalid reasoning in engine", async () => {
      const engine = createKnowledgeEngine();
      
      await expect(engine.reason(testStore, "invalid rules")).rejects.toThrow();
    });
  });

  describe("edge cases", () => {
    it("should handle empty data", async () => {
      const engine = createKnowledgeEngine();
      
      const emptyStore = new Store();
      const ttl = await engine.toTurtle(emptyStore);
      expect(ttl).toBe("");
      
      const results = await engine.query(emptyStore, "SELECT * WHERE { ?s ?p ?o }");
      expect(results).toEqual([]);
      
      const canonical = await engine.canonicalize(emptyStore);
      expect(canonical).toBe("");
    });

    it("should handle large datasets", async () => {
      const engine = createKnowledgeEngine();
      
      const largeStore = new Store();
      for (let i = 0; i < 1000; i++) {
        largeStore.addQuad(
          namedNode(`http://example.org/resource${i}`),
          namedNode("http://example.org/hasValue"),
          literal(`value${i}`)
        );
      }
      
      const ttl = await engine.toTurtle(largeStore);
      expect(ttl.length).toBeGreaterThan(1000);
      
      const results = await engine.query(largeStore, "SELECT * WHERE { ?s ?p ?o } LIMIT 10");
      expect(results.length).toBe(10);
    });

    it("should handle concurrent operations", async () => {
      const engine = createKnowledgeEngine();
      
      const promises = [
        engine.parseTurtle(testTtl),
        engine.toTurtle(testStore),
        engine.query(testStore, "SELECT * WHERE { ?s ?p ?o }"),
        engine.canonicalize(testStore)
      ];
      
      const results = await Promise.all(promises);
      
      expect(results).toHaveLength(4);
      expect(results[0]).toBeInstanceOf(Store);
      expect(typeof results[1]).toBe("string");
      expect(Array.isArray(results[2])).toBe(true);
      expect(typeof results[3]).toBe("string");
    });

    it("should handle different base IRIs", async () => {
      const engine1 = createKnowledgeEngine({ baseIRI: "http://example1.org/" });
      const engine2 = createKnowledgeEngine({ baseIRI: "http://example2.org/" });
      
      const ttl1 = `
        @prefix ex: <http://example.org/> .
        ex:alice ex:name "Alice" .
      `;
      
      const store1 = await engine1.parseTurtle(ttl1);
      const store2 = await engine2.parseTurtle(ttl1);
      
      expect(store1.size).toBe(1);
      expect(store2.size).toBe(1);
      
      // The stores should be isomorphic but have different base IRIs
      const isIso = await engine1.isIsomorphic(store1, store2);
      expect(isIso).toBe(true);
    });
  });
});
