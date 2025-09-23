/**
 * @fileoverview Edge case tests for UNRDF utils only
 * 
 * Tests edge cases, error conditions, and boundary conditions
 * for the utils directory specifically.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, it, expect, beforeEach } from "vitest";
import { DataFactory, Store } from "n3";

// Import utils
import {
  asNamedNode, asLiteral, asBlankNode, smartLiteral,
  quadToJSON, jsonToQuad, 
  getObjects, isA, getFirstObject,
  validateIRI, validateTerm, validateStore,
  generateUUID, createHashIRI, isBlankNodeIRI,
  createNamespaceManager, getVocabularyTerm,
  createSPARQLBuilder, analyzeSPARQLQuery, validateSPARQLQuery,
  storeToJSONLD, transformStore, storeToNTriples,
  mergeStores, intersectStores, areStoresEqual,
  assessDataQuality, generateQualityReport
} from "../src/utils/index.mjs";

const { namedNode, literal, blankNode, quad } = DataFactory;

describe("Utils Edge Cases", () => {
  describe("Term Utils Edge Cases", () => {
    it("should handle null inputs with proper errors", () => {
      expect(() => asNamedNode(null)).toThrow("asNamedNode: IRI cannot be null or undefined");
      expect(() => asLiteral(null)).toThrow("asLiteral: value cannot be null or undefined");
      expect(() => asBlankNode(null)).not.toThrow(); // asBlankNode handles null
    });

    it("should handle empty strings", () => {
      const emptyNamed = asNamedNode("");
      expect(emptyNamed.value).toBe("");
      
      const emptyLiteral = asLiteral("");
      expect(emptyLiteral.value).toBe("");
    });

    it("should handle unicode characters", () => {
      const unicode = "Hello 世界 🌍";
      const literal = asLiteral(unicode);
      expect(literal.value).toBe(unicode);
    });

    it("should handle smart literal edge cases", () => {
      expect(smartLiteral("").value).toBe("");
      expect(smartLiteral("   ").value).toBe("   ");
      expect(smartLiteral("not-a-date").value).toBe("not-a-date");
    });
  });

  describe("Quad Utils Edge Cases", () => {
    it("should handle malformed JSON input", () => {
      const malformed = { subject: "s", predicate: "p" }; // missing object
      expect(() => jsonToQuad(malformed)).toThrow();
    });

    it("should handle blank nodes in quads", () => {
      const blankQuad = quad(
        blankNode("b1"),
        namedNode("http://example.org/p"),
        blankNode("b2")
      );
      
      const json = quadToJSON(blankQuad);
      expect(json.subject).toBe("b1"); // N3 blank nodes don't include _: prefix
      expect(json.object).toBe("b2");
    });
  });

  describe("Graph Utils Edge Cases", () => {
    it("should handle empty stores", () => {
      const emptyStore = new Store();
      
      expect(getObjects(emptyStore, "http://example.org/s", "http://example.org/p")).toEqual([]);
      expect(isA(emptyStore, "http://example.org/s", "http://example.org/Type")).toBe(false);
      expect(getFirstObject(emptyStore, "http://example.org/s", "http://example.org/p")).toBeNull();
    });

    it("should handle null inputs with errors", () => {
      const store = new Store();
      // getObjects calls asNamedNode on both params, so throws for null
      expect(() => getObjects(store, null, "http://example.org/p")).toThrow();
      // isA only calls asNamedNode on the type param, not subject, so doesn't throw for null subject
      expect(() => isA(store, null, "http://example.org/Type")).not.toThrow();
    });
  });

  describe("Validation Utils Edge Cases", () => {
    it("should handle invalid IRIs", () => {
      expect(validateIRI("")).toBe(false);
      expect(validateIRI("not-a-url")).toBe(false);
      expect(validateIRI("ftp://example.org")).toBe(true);
    });

    it("should handle invalid terms", () => {
      expect(validateTerm(null)).toBe(false);
      expect(validateTerm({})).toBe(false);
      expect(validateTerm({ termType: "Invalid" })).toBe(false);
    });

    it("should handle empty store validation", () => {
      const emptyStore = new Store();
      const result = validateStore(emptyStore);
      expect(result.valid).toBe(false);
      expect(result.issues[0].message).toBe("Store is empty");
    });
  });

  describe("ID Utils Edge Cases", () => {
    it("should handle empty inputs", () => {
      expect(() => generateUUID()).not.toThrow();
      expect(() => createHashIRI("")).not.toThrow();
    });

    it("should handle special characters", () => {
      const special = "!@#$%^&*()";
      expect(() => createHashIRI(special)).not.toThrow();
    });

    it("should handle blank node IRI detection", () => {
      expect(isBlankNodeIRI("")).toBe(false);
      expect(isBlankNodeIRI("not-a-blank")).toBe(false);
    });
  });

  describe("Namespace Utils Edge Cases", () => {
    it("should handle unknown prefixes", () => {
      const nsManager = createNamespaceManager();
      expect(() => nsManager.createNamedNode("unknown", "test")).toThrow();
    });

    it("should handle empty inputs", () => {
      const nsManager = createNamespaceManager();
      expect(nsManager.expandIRI("")).toBe("");
      expect(() => getVocabularyTerm("", "")).toThrow(); // Throws for unknown vocabulary
    });
  });

  describe("SPARQL Utils Edge Cases", () => {
    it("should handle empty queries", () => {
      const analysis = analyzeSPARQLQuery("");
      expect(analysis.type).toBe("UNKNOWN");
      
      const validation = validateSPARQLQuery("");
      expect(validation.valid).toBe(false);
    });

    it("should handle builder edge cases", () => {
      const builder = createSPARQLBuilder();
      builder.select();
      expect(builder.build()).toContain("SELECT *");
    });
  });

  describe("Transform Utils Edge Cases", () => {
    it("should handle empty stores", () => {
      const empty = new Store();
      
      expect(storeToJSONLD(empty)).toEqual({
        "@context": {},
        "@graph": []
      });
      
      expect(storeToNTriples(empty)).toBe("");
    });

    it("should handle null transformer", () => {
      const store = new Store();
      store.add(quad(
        namedNode("http://example.org/s"),
        namedNode("http://example.org/p"),
        literal("o")
      ));
      
      const transformed = transformStore(store, () => null);
      expect(transformed.size).toBe(0);
    });
  });

  describe("Merge Utils Edge Cases", () => {
    it("should handle empty stores", () => {
      const empty1 = new Store();
      const empty2 = new Store();
      
      expect(mergeStores(empty1, empty2).size).toBe(0);
      expect(intersectStores(empty1, empty2).size).toBe(0);
    });

    it("should handle identical stores", () => {
      const store1 = new Store();
      const store2 = new Store();
      const testQuad = quad(
        namedNode("http://example.org/s"),
        namedNode("http://example.org/p"),
        literal("o")
      );
      
      store1.add(testQuad);
      store2.add(testQuad);
      
      expect(areStoresEqual(store1, store2)).toBe(true);
    });
  });

  describe("Quality Utils Edge Cases", () => {
    it("should handle empty stores", () => {
      const empty = new Store();
      const assessment = assessDataQuality(empty);
      
      expect(assessment.overallScore).toBeGreaterThanOrEqual(0);
      expect(assessment.dimensions.completeness.issues).toContainEqual(
        expect.objectContaining({ message: "Store is empty" })
      );
    });

    it("should handle invalid RDF", () => {
      const invalid = new Store();
      invalid.add(quad(
        literal("invalid-subject"),
        namedNode("http://example.org/p"),
        literal("o")
      ));
      
      const assessment = assessDataQuality(invalid);
      expect(assessment.dimensions.validity.issues.length).toBeGreaterThan(0);
    });

    it("should generate reports", () => {
      const store = new Store();
      store.add(quad(
        namedNode("http://example.org/s"),
        namedNode("http://example.org/p"),
        literal("o")
      ));
      
      const assessment = assessDataQuality(store);
      const report = generateQualityReport(assessment);
      
      expect(report).toContain("Data Quality Assessment Report");
    });
  });

  describe("Integration Edge Cases", () => {
    it("should handle complete workflow with edge cases", () => {
      // Create store with edge cases
      const store = new Store();
      store.add(quad(
        namedNode("http://example.org/s"),
        namedNode("http://example.org/p"),
        literal("")
      ));
      
      // Test namespace operations
      const nsManager = createNamespaceManager();
      const expanded = nsManager.expandIRI("rdf:type");
      expect(expanded).toBe("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
      
      // Test SPARQL
      const query = createSPARQLBuilder()
        .select("?s", "?p", "?o")
        .where("?s", "?p", "?o")
        .build();
      expect(query).toContain("SELECT ?s ?p ?o");
      
      // Test transformations
      const jsonld = storeToJSONLD(store);
      expect(jsonld).toHaveProperty("@graph");
      
      // Test quality
      const quality = assessDataQuality(store);
      expect(quality.overallScore).toBeGreaterThanOrEqual(0);
    });

    it("should handle error recovery", () => {
      expect(() => asNamedNode(null)).toThrow(); // Actually throws for null
      expect(() => validateIRI("invalid")).not.toThrow(); // Returns false, doesn't throw
      expect(() => createNamespaceManager()).not.toThrow();
    });

    it("should handle large datasets", () => {
      const large = new Store();
      for (let i = 0; i < 1000; i++) {
        large.add(quad(
          namedNode(`http://example.org/s${i}`),
          namedNode("http://example.org/p"),
          literal(`value${i}`)
        ));
      }
      
      expect(() => assessDataQuality(large)).not.toThrow();
      expect(() => storeToJSONLD(large)).not.toThrow();
    });
  });
});
