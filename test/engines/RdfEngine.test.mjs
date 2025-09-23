/**
 * @fileoverview Comprehensive test suite for RdfEngine
 * 
 * Tests all functionality of the production-grade RDF engine including:
 * - Constructor and configuration
 * - Term creation methods
 * - Parsing and serialization
 * - Canonicalization and isomorphism
 * - SHACL validation
 * - SPARQL queries and updates
 * - Reasoning and JSON-LD
 * - Set operations and utilities
 * - Edge cases and error handling
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, test, expect, beforeEach, vi } from "vitest";
import { RdfEngine } from "../../src/engines/rdf-engine.mjs";
import { initStore, setStoreContext } from "../../src/context/index.mjs";

describe("RdfEngine", () => {
  let engine;
  let runApp;

  beforeEach(() => {
    // Initialize a fresh store context for each test
    runApp = initStore([], { 
      baseIRI: "http://example.org/",
      deterministic: true,
      timeoutMs: 5000
    });
  });

  describe("Constructor and Configuration", () => {
    test("should create engine with default options", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        expect(engine).toBeDefined();
        expect(engine.baseIRI).toBe("http://example.org/");
        expect(engine.deterministic).toBe(true);
        expect(engine.timeoutMs).toBe(30_000);
        expect(engine.onMetric).toBeNull();
        expect(engine.log).toBe(console);
        expect(engine.engine).toBeDefined();
        expect(engine.$rdf).toBeDefined();
      });
    });

    test("should create engine with custom options", async () => {
      const customLogger = { log: vi.fn(), error: vi.fn(), warn: vi.fn() };
      const customMetric = vi.fn();
      
      await runApp(async () => {
        engine = new RdfEngine({
          baseIRI: "http://custom.org/",
          deterministic: false,
          timeoutMs: 10000,
          onMetric: customMetric,
          logger: customLogger
        });
        
        expect(engine.baseIRI).toBe("http://custom.org/");
        expect(engine.deterministic).toBe(false);
        expect(engine.timeoutMs).toBe(10000);
        expect(engine.onMetric).toBe(customMetric);
        expect(engine.log).toBe(customLogger);
      });
    });

    test("should handle invalid timeout values", async () => {
      await runApp(async () => {
        engine = new RdfEngine({
          timeoutMs: "invalid"
        });
        
        expect(engine.timeoutMs).toBe(30_000);
      });
    });
  });

  describe("Store Access", () => {
    test("should get store from context", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const store = engine.getStore();
        
        expect(store).toBeDefined();
        expect(typeof store.add).toBe("function");
        expect(typeof store.size).toBe("number");
      });
    });

    test("should get store context", async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        const context = engine.getStoreContext();
        
        expect(context).toBeDefined();
        expect(context.store).toBeDefined();
        expect(context.engine).toBeDefined();
      });
    });

    test("should throw error when store context not initialized", () => {
      // Test without context
      expect(() => {
        engine = new RdfEngine();
        engine.getStore();
      }).toThrow();
    });
  });

  describe("Term Creation Methods", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine();
      });
    });

    test("should create named nodes", () => {
      const node = engine.namedNode("http://example.org/test");
      
      expect(node.termType).toBe("NamedNode");
      expect(node.value).toBe("http://example.org/test");
    });

    test("should create literals", () => {
      const literal1 = engine.literal("hello");
      const literal2 = engine.literal("world", "en");
      const literal3 = engine.literal("42", engine.namedNode("http://www.w3.org/2001/XMLSchema#integer"));
      
      expect(literal1.termType).toBe("Literal");
      expect(literal1.value).toBe("hello");
      
      expect(literal2.termType).toBe("Literal");
      expect(literal2.value).toBe("world");
      expect(literal2.language).toBe("en");
      
      expect(literal3.termType).toBe("Literal");
      expect(literal3.value).toBe("42");
      expect(literal3.datatype.value).toBe("http://www.w3.org/2001/XMLSchema#integer");
    });

    test("should create blank nodes", () => {
      const bnode1 = engine.blankNode();
      const bnode2 = engine.blankNode("test123");
      
      expect(bnode1.termType).toBe("BlankNode");
      expect(bnode1.value).toBeDefined();
      
      expect(bnode2.termType).toBe("BlankNode");
      expect(bnode2.value).toBe("test123");
    });

    test("should create quads", () => {
      const s = engine.namedNode("http://example.org/s");
      const p = engine.namedNode("http://example.org/p");
      const o = engine.literal("o");
      const g = engine.namedNode("http://example.org/g");
      
      const quad1 = engine.quad(s, p, o);
      const quad2 = engine.quad(s, p, o, g);
      
      expect(quad1.subject).toBe(s);
      expect(quad1.predicate).toBe(p);
      expect(quad1.object).toBe(o);
      expect(quad1.graph.termType).toBe("DefaultGraph");
      
      expect(quad2.subject).toBe(s);
      expect(quad2.predicate).toBe(p);
      expect(quad2.object).toBe(o);
      expect(quad2.graph).toBe(g);
    });
  });

  describe("Parsing Methods", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine();
      });
    });

    test("should parse Turtle into global store", () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
        ex:John ex:name "John Doe" .
      `;
      
      const store = engine.parseTurtle(ttl);
      
      expect(store.size).toBeGreaterThan(0);
      expect(store).toBe(engine.getStore());
    });

    test("should parse Turtle with custom base IRI", () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
        ex:John ex:name "John Doe" .
      `;
      
      const store = engine.parseTurtle(ttl, { 
        baseIRI: "http://custom.org/" 
      });
      
      expect(store.size).toBeGreaterThan(0);
    });

    test("should throw error for invalid Turtle input", () => {
      expect(() => {
        engine.parseTurtle("");
      }).toThrow("parseTurtle: non-empty string required");
      
      expect(() => {
        engine.parseTurtle(null);
      }).toThrow("parseTurtle: non-empty string required");
    });

    test("should parse N-Quads into global store", () => {
      const nq = `
        <http://example.org/s> <http://example.org/p> "o" .
        <http://example.org/s> <http://example.org/p2> "o2" .
      `;
      
      const store = engine.parseNQuads(nq);
      
      expect(store.size).toBeGreaterThan(0);
      expect(store).toBe(engine.getStore());
    });

    test("should throw error for invalid N-Quads input", () => {
      expect(() => {
        engine.parseNQuads("");
      }).toThrow("parseNQuads: non-empty string required");
      
      expect(() => {
        engine.parseNQuads(null);
      }).toThrow("parseNQuads: non-empty string required");
    });
  });

  describe("Serialization Methods", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        // Add some test data
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
        `;
        engine.parseTurtle(ttl);
      });
    });

    test("should serialize global store to Turtle", async () => {
      const turtle = await engine.serializeTurtle();
      
      expect(typeof turtle).toBe("string");
      expect(turtle).toContain("ex:Person");
      expect(turtle).toContain("ex:Human");
    });

    test("should serialize with custom prefixes", async () => {
      const prefixes = {
        "custom": "http://custom.org/",
        "ex": "http://example.org/"
      };
      
      const turtle = await engine.serializeTurtle({ prefixes });
      
      expect(typeof turtle).toBe("string");
      expect(turtle).toContain("@prefix");
    });

    test("should serialize global store to N-Quads", async () => {
      const nquads = await engine.serializeNQuads();
      
      expect(typeof nquads).toBe("string");
      expect(nquads).toContain("<http://example.org/Person>");
      expect(nquads).toContain("<http://example.org/Human>");
    });
  });

  describe("Canonicalization and Isomorphism", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine();
      });
    });

    test("should canonicalize empty store", async () => {
      const canonical = await engine.canonicalize();
      
      expect(canonical).toBe("");
    });

    test("should canonicalize store with data", async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
        ex:John ex:name "John Doe" .
      `;
      engine.parseTurtle(ttl);
      
      const canonical = await engine.canonicalize();
      
      expect(typeof canonical).toBe("string");
      expect(canonical.length).toBeGreaterThan(0);
    });

    test("should check isomorphism between stores", async () => {
      const store1 = engine.getStore();
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
      `;
      engine.parseTurtle(ttl);
      
      // Create a second store with same data
      const { Store } = await import("n3");
      const store2 = new Store();
      store2.addQuads([...store1]);
      
      const isIsomorphic = await engine.isIsomorphic(store1, store2);
      
      expect(isIsomorphic).toBe(true);
    });

    test("should detect non-isomorphic stores", async () => {
      const store1 = engine.getStore();
      const ttl1 = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
      `;
      engine.parseTurtle(ttl1);
      
      const { Store } = await import("n3");
      const store2 = new Store();
      const ttl2 = `
        @prefix ex: <http://example.org/> .
        ex:Animal a ex:Creature .
      `;
      const parser = new (await import("n3")).Parser();
      store2.addQuads(parser.parse(ttl2));
      
      const isIsomorphic = await engine.isIsomorphic(store1, store2);
      
      expect(isIsomorphic).toBe(false);
    });
  });

  describe("SHACL Validation", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine();
      });
    });

    test("should validate data against shapes", async () => {
      const dataTtl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
        ex:John ex:name "John Doe" .
      `;
      engine.parseTurtle(dataTtl);
      const dataStore = engine.getStore();
      
      const shapesTtl = `
        @prefix ex: <http://example.org/> .
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Human ;
          sh:property [
            sh:path ex:name ;
            sh:datatype xsd:string ;
            sh:minCount 1
          ] .
      `;
      
      const result = await engine.validateShacl(dataStore, shapesTtl);
      
      expect(result).toHaveProperty("conforms");
      expect(result).toHaveProperty("results");
      expect(Array.isArray(result.results)).toBe(true);
    });

    test("should validate with Store input", async () => {
      const dataTtl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
        ex:John ex:name "John Doe" .
      `;
      engine.parseTurtle(dataTtl);
      const dataStore = engine.getStore();
      
      const shapesTtl = `
        @prefix ex: <http://example.org/> .
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Human .
      `;
      const shapesStore = engine.parseTurtle(shapesTtl);
      
      const result = await engine.validateShacl(dataStore, shapesStore);
      
      expect(result).toHaveProperty("conforms");
      expect(result).toHaveProperty("results");
    });

    test("should validate and throw on failure", async () => {
      const dataTtl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
      `;
      engine.parseTurtle(dataTtl);
      const dataStore = engine.getStore();
      
      const shapesTtl = `
        @prefix ex: <http://example.org/> .
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        
        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Human ;
          sh:property [
            sh:path ex:name ;
            sh:minCount 1
          ] .
      `;
      
      await expect(
        engine.validateShaclOrThrow(dataStore, shapesTtl)
      ).rejects.toThrow("SHACL validation failed");
    });
  });

  describe("SPARQL Query and Update", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
          ex:Jane ex:name "Jane Smith" .
        `;
        engine.parseTurtle(ttl);
      });
    });

    test("should execute SELECT queries", async () => {
      const query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
      
      const result = await engine.query(query);
      
      expect(result.type).toBe("select");
      expect(result.variables).toBeDefined();
      expect(Array.isArray(result.results)).toBe(true);
      expect(result.results.length).toBeGreaterThan(0);
    });

    test("should execute ASK queries", async () => {
      const query = "ASK WHERE { ?s <http://example.org/name> ?o }";
      
      const result = await engine.query(query);
      
      expect(result.type).toBe("ask");
      expect(typeof result.boolean).toBe("boolean");
      expect(result.boolean).toBe(true);
    });

    test("should execute CONSTRUCT queries", async () => {
      const query = `
        CONSTRUCT { ?s <http://example.org/hasName> ?o }
        WHERE { ?s <http://example.org/name> ?o }
      `;
      
      const result = await engine.query(query);
      
      expect(result.type).toBe("construct");
      expect(result.store).toBeDefined();
      expect(result.quads).toBeDefined();
    });

    test("should execute DESCRIBE queries", async () => {
      const query = "DESCRIBE <http://example.org/John>";
      
      const result = await engine.query(query);
      
      expect(result.type).toBe("describe");
      expect(result.store).toBeDefined();
      expect(result.quads).toBeDefined();
    });

    test("should execute INSERT updates", async () => {
      const update = `
        INSERT DATA {
          <http://example.org/Bob> <http://example.org/name> "Bob Wilson" .
        }
      `;
      
      const result = await engine.query(update);
      
      expect(result.type).toBe("update");
      expect(result.ok).toBe(true);
      
      // Verify the data was inserted
      const selectResult = await engine.query(
        "SELECT ?name WHERE { <http://example.org/Bob> <http://example.org/name> ?name }"
      );
      expect(selectResult.results.length).toBeGreaterThan(0);
    });

    test("should execute DELETE updates", async () => {
      const update = `
        DELETE DATA {
          <http://example.org/John> <http://example.org/name> "John Doe" .
        }
      `;
      
      const result = await engine.query(update);
      
      expect(result.type).toBe("update");
      expect(result.ok).toBe(true);
    });

    test("should handle query limits", async () => {
      const query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
      
      const result = await engine.query(query, { limit: 1 });
      
      expect(result.type).toBe("select");
      expect(result.results.length).toBeLessThanOrEqual(1);
    });

    test("should throw error for invalid queries", async () => {
      await expect(
        engine.query("")
      ).rejects.toThrow("query: non-empty SPARQL required");
      
      await expect(
        engine.query("INVALID QUERY")
      ).rejects.toThrow("query: unknown query type");
    });
  });

  describe("Graph Manipulation", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine();
        
        const ttl = `
          @prefix ex: <http://example.org/> .
          ex:Person a ex:Human .
          ex:John ex:name "John Doe" .
        `;
        engine.parseTurtle(ttl);
      });
    });

    test("should get Clownface pointer", () => {
      const pointer = engine.getClownface();
      
      expect(pointer).toBeDefined();
      expect(typeof pointer.out).toBe("function");
      expect(typeof pointer.in).toBe("function");
    });
  });

  describe("Reasoning", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine();
      });
    });

    test("should perform reasoning with empty stores", async () => {
      const { Store } = await import("n3");
      const dataStore = new Store();
      const rulesStore = new Store();
      
      const result = await engine.reason(dataStore, rulesStore);
      
      expect(result.size).toBe(0);
    });

    test("should perform reasoning with data but no rules", async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
      `;
      engine.parseTurtle(ttl);
      const dataStore = engine.getStore();
      
      const { Store } = await import("n3");
      const rulesStore = new Store();
      
      const result = await engine.reason(dataStore, rulesStore);
      
      expect(result.size).toBe(dataStore.size);
    });

    test("should perform reasoning with rules", async () => {
      const dataTtl = `
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        ex:John a ex:Person .
        ex:Person rdfs:subClassOf ex:Human .
      `;
      engine.parseTurtle(dataTtl);
      const dataStore = engine.getStore();
      
      const rulesTtl = `
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        
        { ?x rdfs:subClassOf ?y . ?z a ?x . } => { ?z a ?y . } .
      `;
      const rulesStore = engine.parseTurtle(rulesTtl);
      
      const result = await engine.reason(dataStore, rulesStore);
      
      expect(result.size).toBeGreaterThanOrEqual(dataStore.size);
    });
  });

  describe("JSON-LD I/O", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine();
      });
    });

    test("should convert store to JSON-LD", async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
        ex:John ex:name "John Doe" .
      `;
      engine.parseTurtle(ttl);
      const store = engine.getStore();
      
      const jsonld = await engine.toJSONLD(store);
      
      expect(jsonld).toBeDefined();
      expect(jsonld["@context"]).toBeDefined();
      expect(Array.isArray(jsonld["@graph"])).toBe(true);
    });

    test("should convert store to JSON-LD with context", async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
        ex:John ex:name "John Doe" .
      `;
      engine.parseTurtle(ttl);
      const store = engine.getStore();
      
      const context = {
        "ex": "http://example.org/",
        "name": "ex:name"
      };
      
      const jsonld = await engine.toJSONLD(store, { context });
      
      expect(jsonld["@context"]).toBeDefined();
    });

    test("should convert store to JSON-LD with frame", async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
        ex:John ex:name "John Doe" .
      `;
      engine.parseTurtle(ttl);
      const store = engine.getStore();
      
      const frame = {
        "@type": "ex:Human"
      };
      
      const jsonld = await engine.toJSONLD(store, { frame });
      
      expect(jsonld).toBeDefined();
    });

    test("should convert JSON-LD to store", async () => {
      const jsonldData = {
        "@context": { "ex": "http://example.org/" },
        "@id": "ex:person",
        "@type": "ex:Person",
        "ex:name": "John Doe"
      };
      
      const store = await engine.fromJSONLD(jsonldData);
      
      expect(store.size).toBeGreaterThan(0);
      expect(store).toBe(engine.getStore());
    });
  });

  describe("Set Operations and Utilities", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine();
      });
    });

    test("should perform union of stores", () => {
      const { Store } = require("n3");
      const store1 = new Store();
      const store2 = new Store();
      
      const q1 = engine.quad(
        engine.namedNode("http://example.org/s1"),
        engine.namedNode("http://example.org/p1"),
        engine.literal("o1")
      );
      const q2 = engine.quad(
        engine.namedNode("http://example.org/s2"),
        engine.namedNode("http://example.org/p2"),
        engine.literal("o2")
      );
      
      store1.add(q1);
      store2.add(q2);
      
      const union = engine.union(store1, store2);
      
      expect(union.size).toBe(2);
    });

    test("should perform difference of stores", () => {
      const { Store } = require("n3");
      const store1 = new Store();
      const store2 = new Store();
      
      const q1 = engine.quad(
        engine.namedNode("http://example.org/s1"),
        engine.namedNode("http://example.org/p1"),
        engine.literal("o1")
      );
      const q2 = engine.quad(
        engine.namedNode("http://example.org/s2"),
        engine.namedNode("http://example.org/p2"),
        engine.literal("o2")
      );
      
      store1.add(q1, q2);
      store2.add(q1);
      
      const difference = engine.difference(store1, store2);
      
      expect(difference.size).toBe(1);
      expect(difference.has(q2)).toBe(true);
      expect(difference.has(q1)).toBe(false);
    });

    test("should perform intersection of stores", () => {
      const { Store } = require("n3");
      const store1 = new Store();
      const store2 = new Store();
      
      const q1 = engine.quad(
        engine.namedNode("http://example.org/s1"),
        engine.namedNode("http://example.org/p1"),
        engine.literal("o1")
      );
      const q2 = engine.quad(
        engine.namedNode("http://example.org/s2"),
        engine.namedNode("http://example.org/p2"),
        engine.literal("o2")
      );
      
      store1.add(q1, q2);
      store2.add(q1);
      
      const intersection = engine.intersection(store1, store2);
      
      expect(intersection.size).toBe(1);
    });

    test("should skolemize blank nodes", () => {
      const { Store } = require("n3");
      const store = new Store();
      
      const q = engine.quad(
        engine.blankNode("b1"),
        engine.namedNode("http://example.org/p"),
        engine.blankNode("b2")
      );
      
      store.add(q);
      
      const skolemized = engine.skolemize(store);
      
      expect(skolemized.size).toBe(1);
      
      // Check that blank nodes were replaced with named nodes
      const quad = [...skolemized][0];
      expect(quad.subject.termType).toBe("NamedNode");
      expect(quad.object.termType).toBe("NamedNode");
    });

    test("should get store statistics", async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
        ex:John ex:name "John Doe" .
        ex:Jane ex:name "Jane Smith" .
      `;
      engine.parseTurtle(ttl);
      
      const stats = engine.getStats();
      
      expect(stats.quads).toBeGreaterThan(0);
      expect(stats.subjects).toBeGreaterThan(0);
      expect(stats.predicates).toBeGreaterThan(0);
      expect(stats.objects).toBeGreaterThan(0);
      expect(stats.graphs).toBeGreaterThan(0);
    });
  });

  describe("Edge Cases and Error Handling", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine();
      });
    });

    test("should handle timeout in operations", async () => {
      const shortTimeoutEngine = new RdfEngine({ timeoutMs: 1 });
      
      // This should timeout quickly
      await expect(
        shortTimeoutEngine.canonicalize()
      ).rejects.toThrow("timeout");
    });

    test("should handle malformed Turtle gracefully", () => {
      const malformedTtl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human
        // Missing period
      `;
      
      // Should throw for malformed Turtle
      expect(() => {
        engine.parseTurtle(malformedTtl);
      }).toThrow();
    });

    test("should handle empty JSON-LD", async () => {
      const emptyJsonld = {
        "@context": {},
        "@graph": []
      };
      
      const store = await engine.fromJSONLD(emptyJsonld);
      
      expect(store.size).toBe(0);
    });

    test("should handle reasoning with malformed rules", async () => {
      const { Store } = require("n3");
      const dataStore = new Store();
      const rulesStore = new Store();
      
      // Add malformed rule
      const malformedRule = engine.quad(
        engine.namedNode("http://example.org/rule"),
        engine.namedNode("http://example.org/malformed"),
        engine.literal("invalid")
      );
      rulesStore.add(malformedRule);
      
      // Should handle gracefully
      const result = await engine.reason(dataStore, rulesStore);
      
      expect(result).toBeDefined();
    });

    test("should handle canonicalization failure gracefully", async () => {
      // Create a store that might cause canonicalization issues
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
        _:b1 ex:name "John" .
        _:b2 ex:name "Jane" .
      `;
      engine.parseTurtle(ttl);
      
      // Should not throw even if canonicalization fails
      const canonical = await engine.canonicalize();
      
      expect(typeof canonical).toBe("string");
    });

    test("should handle metrics callback errors", async () => {
      const errorMetric = vi.fn(() => {
        throw new Error("Metrics error");
      });
      
      const engineWithErrorMetric = new RdfEngine({ onMetric: errorMetric });
      
      // Should not throw even if metrics callback fails
      const stats = engineWithErrorMetric.getStats();
      
      expect(stats).toBeDefined();
    });
  });

  describe("Performance and Deterministic Behavior", () => {
    beforeEach(async () => {
      await runApp(async () => {
        engine = new RdfEngine({ deterministic: true });
      });
    });

    test("should produce deterministic results", async () => {
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
        ex:John ex:name "John Doe" .
        ex:Jane ex:name "Jane Smith" .
      `;
      engine.parseTurtle(ttl);
      
      const result1 = await engine.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }");
      const result2 = await engine.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }");
      
      expect(result1.results).toEqual(result2.results);
    });

    test("should handle non-deterministic mode", async () => {
      const nonDeterministicEngine = new RdfEngine({ deterministic: false });
      
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
        ex:John ex:name "John Doe" .
        ex:Jane ex:name "Jane Smith" .
      `;
      nonDeterministicEngine.parseTurtle(ttl);
      
      const result = await nonDeterministicEngine.query("SELECT ?s ?p ?o WHERE { ?s ?p ?o }");
      
      expect(result.type).toBe("select");
      expect(result.results.length).toBeGreaterThan(0);
    });
  });
});
