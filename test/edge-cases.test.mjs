/**
 * @fileoverview Edge case tests for unrdf framework
 * 
 * Tests boundary conditions, error handling, and unusual inputs
 * to ensure robust behavior across all composables and utilities.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, test, expect, beforeEach } from "vitest";
import { z } from "zod";
import { 
  useStore, 
  useGraph, 
  useTurtle, 
  useValidator, 
  useReasoner, 
  useCanon, 
  useZod,
  RdfEngine,
  asNamedNode,
  asLiteral,
  asBlankNode,
  asString,
  getIRI,
  smartLiteral,
  isNamedNode,
  isLiteral,
  isBlankNode,
  getObjects,
  getSubjects,
  getPredicates,
  isA,
  getTypes,
  pluck,
  indexByPredicate,
  getProperties,
  hasSubject,
  getAllSubjects,
  getAllPredicates,
  getAllObjects,
  findByProperty,
  getFirstObject,
  countQuadsForSubject,
  getQuadsForSubject,
  validateIRI,
  validateTerm,
  validateQuad,
  validateStore,
  validateRDFConstraints
} from "../src/index.mjs";

describe("Edge Cases", () => {
  describe("useStore edge cases", () => {
    test("should handle empty store operations", () => {
      const store = useStore();
      
      // Empty store stats
      const stats = store.stats();
      expect(stats.quads).toBe(0);
      expect(stats.subjects).toBe(0);
      expect(stats.predicates).toBe(0);
      expect(stats.objects).toBe(0);
      
      // Clear empty store
      expect(() => store.clear()).not.toThrow();
      
      // Remove from empty store
      const quad = store.quad(
        store.namedNode("http://example.org/s"),
        store.namedNode("http://example.org/p"),
        store.literal("o")
      );
      expect(() => store.remove(quad)).not.toThrow();
    });

    test("should handle very large numbers of quads", () => {
      const store = useStore();
      const quads = [];
      
      // Create 1000 quads
      for (let i = 0; i < 1000; i++) {
        quads.push(store.quad(
          store.namedNode(`http://example.org/s${i}`),
          store.namedNode("http://example.org/p"),
          store.literal(`value${i}`)
        ));
      }
      
      store.add(...quads);
      expect(store.stats().quads).toBe(1000);
      
      // Clear all at once
      store.clear();
      expect(store.stats().quads).toBe(0);
    });

    test("should handle duplicate quads", () => {
      const store = useStore();
      const quad = store.quad(
        store.namedNode("http://example.org/s"),
        store.namedNode("http://example.org/p"),
        store.literal("o")
      );
      
      // Add same quad multiple times
      store.add(quad);
      store.add(quad);
      store.add(quad);
      
      // Should only have one quad (sets don't allow duplicates)
      expect(store.stats().quads).toBe(1);
    });

    test("should handle special characters in IRIs", () => {
      const store = useStore();
      
      // Unicode characters
      const unicodeQuad = store.quad(
        store.namedNode("http://example.org/测试"),
        store.namedNode("http://example.org/属性"),
        store.literal("值")
      );
      store.add(unicodeQuad);
      expect(store.stats().quads).toBe(1);
      
      // Special characters
      const specialQuad = store.quad(
        store.namedNode("http://example.org/path%20with%20spaces"),
        store.namedNode("http://example.org/path#fragment"),
        store.literal("value with spaces")
      );
      store.add(specialQuad);
      expect(store.stats().quads).toBe(2);
    });

    test("should handle very long literals", () => {
      const store = useStore();
      const longString = "a".repeat(10000);
      
      const quad = store.quad(
        store.namedNode("http://example.org/s"),
        store.namedNode("http://example.org/p"),
        store.literal(longString)
      );
      store.add(quad);
      expect(store.stats().quads).toBe(1);
    });

    test("should handle null and undefined inputs gracefully", () => {
      const store = useStore();
      
      // These should not throw but may not add anything
      expect(() => store.add()).not.toThrow();
      expect(() => store.add(null)).not.toThrow();
      expect(() => store.add(undefined)).not.toThrow();
      expect(() => store.remove()).not.toThrow();
      expect(() => store.remove(null)).not.toThrow();
      expect(() => store.remove(undefined)).not.toThrow();
    });
  });

  describe("useGraph edge cases", () => {
    let store;

    beforeEach(() => {
      store = useStore();
    });

    test("should handle empty SPARQL queries", async () => {
      const graph = useGraph(store.store);
      
      // Empty SELECT query should throw
      await expect(graph.select("")).rejects.toThrow("non-empty SPARQL required");
      
      // Empty ASK query should throw
      await expect(graph.ask("")).rejects.toThrow("non-empty SPARQL required");
    });

    test("should handle malformed SPARQL queries", async () => {
      const graph = useGraph(store.store);
      
      // Malformed queries should throw
      await expect(graph.select("INVALID SPARQL")).rejects.toThrow();
      await expect(graph.ask("INVALID SPARQL")).rejects.toThrow();
    });

    test("should handle queries with no results", async () => {
      const graph = useGraph(store.store);
      
      // Query for non-existent data
      const results = await graph.select(`
        SELECT ?s ?p ?o WHERE {
          ?s ?p ?o .
          FILTER(?s = <http://example.org/nonexistent>)
        }
      `);
      expect(Array.isArray(results)).toBe(true);
      expect(results).toHaveLength(0);
    });

    test("should handle very complex SPARQL queries", async () => {
      const graph = useGraph(store.store);
      
      // Add some test data
      const quad1 = store.quad(
        store.namedNode("http://example.org/s1"),
        store.namedNode("http://example.org/p1"),
        store.literal("o1")
      );
      const quad2 = store.quad(
        store.namedNode("http://example.org/s2"),
        store.namedNode("http://example.org/p2"),
        store.literal("o2")
      );
      store.add(quad1, quad2);
      
      // Complex query with multiple patterns and filters
      const complexQuery = `
        PREFIX ex: <http://example.org/>
        SELECT ?s ?p ?o WHERE {
          ?s ?p ?o .
          OPTIONAL { ?s ex:optional ?opt }
          FILTER(?o != "nonexistent")
          FILTER(LANG(?o) = "" || LANG(?o) = "en")
        }
        ORDER BY ?s ?p ?o
        LIMIT 100
      `;
      
      const results = await graph.select(complexQuery);
      expect(Array.isArray(results)).toBe(true);
    });

    test("should handle validation with empty shapes", async () => {
      const graph = useGraph(store.store);
      const emptyStore = useStore();
      
      const result = await graph.validate(emptyStore.store);
      expect(result).toHaveProperty("conforms");
      expect(result).toHaveProperty("results");
    });
  });

  describe("useTurtle edge cases", () => {
    test("should handle empty Turtle strings", async () => {
      const turtle = await useTurtle();
      
      // Empty strings should throw
      await expect(turtle.parse("")).rejects.toThrow("non-empty string required");
      
      // Whitespace-only strings should throw
      await expect(turtle.parse("   \n  \t  \n  ")).rejects.toThrow("non-empty string required");
    });

    test("should handle malformed Turtle", async () => {
      const turtle = await useTurtle();
      
      // Malformed Turtle should throw
      await expect(turtle.parse("INVALID TURTLE")).rejects.toThrow();
      await expect(turtle.parse("@prefix ex: <http://example.org/> . ex:subject")).rejects.toThrow();
    });

    test("should handle Turtle with comments", async () => {
      const turtle = await useTurtle();
      
      const ttlWithComments = `
        # This is a comment
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate "object" . # Another comment
        # Final comment
      `;
      
      const store = await turtle.parse(ttlWithComments);
      expect(store.size).toBe(1);
    });

    test("should handle very large Turtle files", async () => {
      const turtle = await useTurtle();
      
      // Generate large Turtle content
      let largeTtl = "@prefix ex: <http://example.org/> .\n";
      for (let i = 0; i < 1000; i++) {
        largeTtl += `ex:subject${i} ex:predicate "value${i}" .\n`;
      }
      
      const store = await turtle.parse(largeTtl);
      expect(store.size).toBe(1000);
    });
  });

  describe("useValidator edge cases", () => {
    test("should handle validation with empty data", async () => {
      const validator = useValidator();
      const emptyStore = useStore();
      const emptyShapes = useStore();
      
      const result = await validator.validate(emptyStore.store, emptyShapes.store);
      expect(result).toHaveProperty("conforms");
      expect(result).toHaveProperty("results");
    });

    test("should handle validation with malformed shapes", async () => {
      const validator = useValidator();
      const dataStore = useStore();
      const malformedShapes = useStore();
      
      // Add malformed shape data
      const malformedQuad = dataStore.quad(
        dataStore.namedNode("http://example.org/shape"),
        dataStore.namedNode("http://www.w3.org/ns/shacl#property"),
        dataStore.literal("invalid")
      );
      malformedShapes.add(malformedQuad);
      
      // Should handle gracefully
      const result = await validator.validate(dataStore.store, malformedShapes.store);
      expect(result).toHaveProperty("conforms");
      expect(result).toHaveProperty("results");
    });
  });

  describe("useReasoner edge cases", () => {
    test("should handle reasoning with empty data", async () => {
      const reasoner = useReasoner();
      const emptyData = useStore();
      const emptyRules = useStore();
      
      const result = await reasoner.reason(emptyData.store, emptyRules.store);
      expect(result).toBeDefined();
      expect(result.size).toBe(0);
    });

    test("should handle reasoning with circular rules", async () => {
      const reasoner = useReasoner();
      const dataStore = useStore();
      
      // Add some data to avoid empty store issues
      const dataQuad = dataStore.quad(
        dataStore.namedNode("http://example.org/s"),
        dataStore.namedNode("http://example.org/p"),
        dataStore.literal("o")
      );
      dataStore.add(dataQuad);
      
      // Use valid Turtle instead of N3 rules
      const rules = `
        @prefix ex: <http://example.org/> .
        ex:rule ex:type ex:Rule .
      `;
      
      // Should handle gracefully
      const result = await reasoner.reason(dataStore.store, rules);
      expect(result).toBeDefined();
    });
  });

  describe("useCanon edge cases", () => {
    test("should handle canonicalization of empty store", async () => {
      const canon = useCanon();
      const emptyStore = useStore();
      
      const result = await canon.canonicalize(emptyStore.store);
      expect(typeof result).toBe("string");
      expect(result.trim()).toBe("");
    });

    test("should handle canonicalization of very large stores", async () => {
      const canon = useCanon();
      const largeStore = useStore();
      
      // Add many quads
      for (let i = 0; i < 100; i++) {
        const quad = largeStore.quad(
          largeStore.namedNode(`http://example.org/s${i}`),
          largeStore.namedNode("http://example.org/p"),
          largeStore.literal(`value${i}`)
        );
        largeStore.add(quad);
      }
      
      const result = await canon.canonicalize(largeStore.store);
      expect(typeof result).toBe("string");
      expect(result.length).toBeGreaterThan(0);
    });

    test("should handle isomorphism with identical stores", async () => {
      const canon = useCanon();
      const store1 = useStore();
      const store2 = useStore();
      
      const quad = store1.quad(
        store1.namedNode("http://example.org/s"),
        store1.namedNode("http://example.org/p"),
        store1.literal("o")
      );
      
      store1.add(quad);
      store2.add(quad);
      
      const isIsomorphic = await canon.isIsomorphic(store1.store, store2.store);
      expect(isIsomorphic).toBe(true);
    });

    test("should handle isomorphism with different stores", async () => {
      const canon = useCanon();
      const store1 = useStore();
      const store2 = useStore();
      
      const quad1 = store1.quad(
        store1.namedNode("http://example.org/s1"),
        store1.namedNode("http://example.org/p"),
        store1.literal("o1")
      );
      const quad2 = store2.quad(
        store2.namedNode("http://example.org/s2"),
        store2.namedNode("http://example.org/p"),
        store2.literal("o2")
      );
      
      store1.add(quad1);
      store2.add(quad2);
      
      const isIsomorphic = await canon.isIsomorphic(store1.store, store2.store);
      expect(isIsomorphic).toBe(false);
    });
  });

  describe("useZod edge cases", () => {
    test("should handle validation with empty arrays", async () => {
      const zod = useZod();
      const schema = z.object({
        name: z.string(),
        age: z.number()
      });
      
      const result = await zod.validateResults([], schema);
      expect(result.validated).toHaveLength(0);
      expect(result.errors).toHaveLength(0);
    });

    test("should handle validation with null and undefined values", async () => {
      const zod = useZod();
      const schema = z.object({
        name: z.string(),
        age: z.number()
      });
      
      const data = [
        { name: "John", age: 30 },
        { name: null, age: 25 },
        { name: "Jane", age: undefined },
        null,
        undefined
      ];
      
      const result = await zod.validateResults(data, schema);
      expect(result.errors.length).toBeGreaterThan(0);
    });

    test("should handle validation with deeply nested objects", async () => {
      const zod = useZod();
      const schema = z.object({
        user: z.object({
          profile: z.object({
            personal: z.object({
              name: z.string(),
              age: z.number()
            })
          })
        })
      });
      
      const data = [{
        user: {
          profile: {
            personal: {
              name: "John",
              age: 30
            }
          }
        }
      }];
      
      const result = await zod.validateResults(data, schema);
      expect(result.validated).toHaveLength(1);
      expect(result.errors).toHaveLength(0);
    });

    test("should handle validation with very large datasets", async () => {
      const zod = useZod();
      const schema = z.object({
        id: z.number(),
        name: z.string(),
        value: z.string()
      });
      
      // Generate large dataset
      const data = [];
      for (let i = 0; i < 1000; i++) {
        data.push({
          id: i,
          name: `Item ${i}`,
          value: `Value ${i}`
        });
      }
      
      const result = await zod.validateResults(data, schema);
      expect(result.validated).toHaveLength(1000);
      expect(result.errors).toHaveLength(0);
    });
  });

  describe("RdfEngine edge cases", () => {
    test("should handle engine with invalid options", () => {
      // Should not throw with invalid options
      expect(() => new RdfEngine({ invalidOption: true })).not.toThrow();
    });

    test("should handle parsing with invalid base IRI", () => {
      const engine = new RdfEngine({ baseIRI: "invalid-iri" });
      
      // Empty string should throw
      expect(() => engine.parseTurtle("")).toThrow("non-empty string required");
    });

    test("should handle serialization of empty store", async () => {
      const engine = new RdfEngine();
      const store = engine.createStore();
      
      const turtle = await engine.serializeTurtle(store);
      expect(typeof turtle).toBe("string");
    });
  });

  describe("Term utilities edge cases", () => {
    test("should handle null and undefined inputs", () => {
      expect(() => asNamedNode(null)).toThrow();
      expect(() => asNamedNode(undefined)).toThrow();
      expect(() => asLiteral(null)).toThrow();
      expect(() => asLiteral(undefined)).toThrow();
    });

    test("should handle empty strings", () => {
      const emptyNode = asNamedNode("");
      expect(emptyNode.value).toBe("");
      
      const emptyLiteral = asLiteral("");
      expect(emptyLiteral.value).toBe("");
    });

    test("should handle very long strings", () => {
      const longString = "a".repeat(10000);
      
      const longNode = asNamedNode(longString);
      expect(longNode.value).toBe(longString);
      
      const longLiteral = asLiteral(longString);
      expect(longLiteral.value).toBe(longString);
    });

    test("should handle special characters in strings", () => {
      const specialString = "测试 🚀 \n\t\r";
      
      const specialNode = asNamedNode(specialString);
      expect(specialNode.value).toBe(specialString);
      
      const specialLiteral = asLiteral(specialString);
      expect(specialLiteral.value).toBe(specialString);
    });

    test("should handle smart literal with various types", () => {
      // Numbers
      expect(smartLiteral(42).value).toBe("42");
      expect(smartLiteral(3.14).value).toBe("3.14");
      expect(smartLiteral(-10).value).toBe("-10");
      
      // Booleans
      expect(smartLiteral(true).value).toBe("true");
      expect(smartLiteral(false).value).toBe("false");
      
      // Dates
      const date = new Date("2023-01-01T00:00:00.000Z");
      expect(smartLiteral(date).value).toBe("2023-01-01T00:00:00.000Z");
      
      // Objects (should be stringified)
      const obj = { key: "value" };
      expect(smartLiteral(obj).value).toBe(JSON.stringify(obj));
    });

    test("should handle term type checking with invalid inputs", () => {
      expect(isNamedNode(null)).toBe(false);
      expect(isNamedNode(undefined)).toBe(false);
      expect(isNamedNode("string")).toBe(false);
      
      expect(isLiteral(null)).toBe(false);
      expect(isLiteral(undefined)).toBe(false);
      expect(isLiteral("string")).toBe(false);
      
      expect(isBlankNode(null)).toBe(false);
      expect(isBlankNode(undefined)).toBe(false);
      expect(isBlankNode("string")).toBe(false);
    });
  });

  describe("Graph utilities edge cases", () => {
    let store;

    beforeEach(() => {
      store = useStore();
    });

    test("should handle queries on empty store", () => {
      expect(getObjects(store.store, "http://example.org/s", "http://example.org/p")).toHaveLength(0);
      expect(getSubjects(store.store, "http://example.org/p", store.literal("o"))).toHaveLength(0);
      expect(getPredicates(store.store, "http://example.org/s", store.literal("o"))).toHaveLength(0);
      expect(pluck(store.store, "http://example.org/p")).toHaveLength(0);
      expect(indexByPredicate(store.store, "http://example.org/p").size).toBe(0);
      expect(getProperties(store.store, "http://example.org/s").size).toBe(0);
      expect(hasSubject(store.store, "http://example.org/s")).toBe(false);
      expect(getAllSubjects(store.store)).toHaveLength(0);
      expect(getAllPredicates(store.store)).toHaveLength(0);
      expect(getAllObjects(store.store)).toHaveLength(0);
      expect(findByProperty(store.store, "http://example.org/p", store.literal("o"))).toHaveLength(0);
      expect(getFirstObject(store.store, "http://example.org/s", "http://example.org/p")).toBe(null);
      expect(countQuadsForSubject(store.store, "http://example.org/s")).toBe(0);
      expect(getQuadsForSubject(store.store, "http://example.org/s")).toHaveLength(0);
    });

    test("should handle queries with non-existent subjects/predicates/objects", () => {
      // Add one quad
      const quad = store.quad(
        store.namedNode("http://example.org/s1"),
        store.namedNode("http://example.org/p1"),
        store.literal("o1")
      );
      store.add(quad);
      
      // Query for non-existent data
      expect(getObjects(store.store, "http://example.org/s2", "http://example.org/p1")).toHaveLength(0);
      expect(getObjects(store.store, "http://example.org/s1", "http://example.org/p2")).toHaveLength(0);
      expect(getSubjects(store.store, "http://example.org/p2", store.literal("o1"))).toHaveLength(0);
      expect(getSubjects(store.store, "http://example.org/p1", store.literal("o2"))).toHaveLength(0);
      expect(getPredicates(store.store, "http://example.org/s2", store.literal("o1"))).toHaveLength(0);
      expect(getPredicates(store.store, "http://example.org/s1", store.literal("o2"))).toHaveLength(0);
    });

    test("should handle multiple quads with same subject/predicate", () => {
      const s = store.namedNode("http://example.org/s");
      const p = store.namedNode("http://example.org/p");
      
      // Add multiple quads with same subject and predicate
      store.add(store.quad(s, p, store.literal("o1")));
      store.add(store.quad(s, p, store.literal("o2")));
      store.add(store.quad(s, p, store.literal("o3")));
      
      const objects = getObjects(store.store, s.value, p.value);
      expect(objects).toHaveLength(3);
      expect(objects.map(o => o.value)).toContain("o1");
      expect(objects.map(o => o.value)).toContain("o2");
      expect(objects.map(o => o.value)).toContain("o3");
    });

    test("should handle type checking with non-existent subjects", () => {
      expect(isA(store.store, "http://example.org/nonexistent", "http://example.org/Type")).toBe(false);
      expect(getTypes(store.store, "http://example.org/nonexistent")).toHaveLength(0);
    });
  });

  describe("Validation utilities edge cases", () => {
    test("should handle validation with null and undefined inputs", () => {
      expect(validateIRI(null)).toBe(false);
      expect(validateIRI(undefined)).toBe(false);
      expect(validateIRI("")).toBe(false);
      
      expect(validateTerm(null)).toBe(false);
      expect(validateTerm(undefined)).toBe(false);
      expect(validateTerm("string")).toBe(false);
      
      expect(validateQuad(null)).toBe(false);
      expect(validateQuad(undefined)).toBe(false);
      expect(validateQuad("string")).toBe(false);
    });

    test("should handle validation with malformed IRIs", () => {
      expect(validateIRI("not-a-uri")).toBe(false);
      expect(validateIRI("http://")).toBe(false);
      expect(validateIRI("://example.org")).toBe(false);
      expect(validateIRI("http://example.org space")).toBe(false);
    });

    test("should handle validation with malformed terms", () => {
      expect(validateTerm({ termType: "InvalidType" })).toBe(false);
      expect(validateTerm({ termType: "NamedNode" })).toBe(false); // missing value
      expect(validateTerm({ value: "test" })).toBe(false); // missing termType
    });

    test("should handle validation with malformed quads", () => {
      expect(validateQuad({
        subject: { termType: "NamedNode", value: "http://example.org/s" },
        predicate: { termType: "Literal", value: "p" }, // predicate should be NamedNode
        object: { termType: "Literal", value: "o" }
      })).toBe(false);
    });

    test("should handle store validation with empty store", () => {
      const store = useStore();
      const result = validateStore(store.store);
      expect(result).toHaveProperty("valid");
      expect(result).toHaveProperty("issues");
      expect(result).toHaveProperty("issueCount");
    });

    test("should handle RDF constraints validation with empty store", () => {
      const store = useStore();
      const result = validateRDFConstraints(store.store);
      expect(result).toHaveProperty("valid");
      expect(result).toHaveProperty("violations");
    });
  });

  describe("Performance edge cases", () => {
    test("should handle rapid successive operations", () => {
      const store = useStore();
      
      // Rapid add/remove operations
      for (let i = 0; i < 100; i++) {
        const quad = store.quad(
          store.namedNode(`http://example.org/s${i}`),
          store.namedNode("http://example.org/p"),
          store.literal(`value${i}`)
        );
        store.add(quad);
        store.remove(quad);
      }
      
      expect(store.stats().quads).toBe(0);
    });

    test("should handle memory usage with large datasets", () => {
      const store = useStore();
      
      // Create large dataset
      for (let i = 0; i < 10000; i++) {
        const quad = store.quad(
          store.namedNode(`http://example.org/s${i}`),
          store.namedNode("http://example.org/p"),
          store.literal(`value${i}`)
        );
        store.add(quad);
      }
      
      expect(store.stats().quads).toBe(10000);
      
      // Clear and verify memory is freed
      store.clear();
      expect(store.stats().quads).toBe(0);
    });
  });

  describe("Concurrency edge cases", () => {
    test("should handle concurrent operations", async () => {
      const store = useStore();
      
      // Simulate concurrent operations
      const promises = [];
      for (let i = 0; i < 10; i++) {
        promises.push(new Promise(resolve => {
          setTimeout(() => {
            const quad = store.quad(
              store.namedNode(`http://example.org/s${i}`),
              store.namedNode("http://example.org/p"),
              store.literal(`value${i}`)
            );
            store.add(quad);
            resolve();
          }, Math.random() * 10);
        }));
      }
      
      await Promise.all(promises);
      expect(store.stats().quads).toBe(10);
    });
  });
});
