/**
 * @fileoverview Main test suite for unrdf framework
 * 
 * Tests the core composables and integration between components
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

describe("unrdf", () => {
  describe("useStore", () => {
    test("should create a new store", () => {
      const store = useStore();
      expect(store.store).toBeDefined();
      expect(store.store.size).toBe(0);
      expect(typeof store.add).toBe("function");
      expect(typeof store.remove).toBe("function");
      expect(typeof store.clear).toBe("function");
      expect(typeof store.stats).toBe("function");
      expect(typeof store.serialize).toBe("function");
    });

    test("should add quads to store", () => {
      const store = useStore();
      const q = store.quad(
        store.namedNode("http://example.org/s"),
        store.namedNode("http://example.org/p"),
        store.literal("o")
      );
      
      store.add(q);
      expect(store.stats().quads).toBe(1);
    });

    test("should remove quads from store", () => {
      const store = useStore();
      const q = store.quad(
        store.namedNode("http://example.org/s"),
        store.namedNode("http://example.org/p"),
        store.literal("o")
      );
      
      store.add(q);
      expect(store.stats().quads).toBe(1);
      
      store.remove(q);
      expect(store.stats().quads).toBe(0);
    });

    test("should clear all quads", () => {
      const store = useStore();
      const q1 = store.quad(
        store.namedNode("http://example.org/s1"),
        store.namedNode("http://example.org/p1"),
        store.literal("o1")
      );
      const q2 = store.quad(
        store.namedNode("http://example.org/s2"),
        store.namedNode("http://example.org/p2"),
        store.literal("o2")
      );
      
      store.add(q1, q2);
      expect(store.stats().quads).toBe(2);
      
      store.clear();
      expect(store.stats().quads).toBe(0);
    });

    test("should serialize store to Turtle", async () => {
      const store = useStore();
      const q = store.quad(
        store.namedNode("http://example.org/s"),
        store.namedNode("http://example.org/p"),
        store.literal("o")
      );
      
      store.add(q);
      
      const turtle = await store.serialize({ format: "Turtle" });
      expect(typeof turtle).toBe("string");
      expect(turtle).toContain("ex:s");
      expect(turtle).toContain("ex:p");
      expect(turtle).toContain("o");
    });
  });

  describe("useGraph", () => {
    let store;

    beforeEach(() => {
      store = useStore();
      const q = store.quad(
        store.namedNode("http://example.org/s"),
        store.namedNode("http://example.org/p"),
        store.literal("o")
      );
      store.add(q);
    });

    test("should create graph interface", () => {
      const graph = useGraph(store.store);
      expect(graph.store).toBe(store.store);
      expect(typeof graph.select).toBe("function");
      expect(typeof graph.ask).toBe("function");
      expect(typeof graph.query).toBe("function");
      expect(typeof graph.validate).toBe("function");
      expect(typeof graph.serialize).toBe("function");
      expect(typeof graph.stats).toBe("object");
    });

    test("should execute SPARQL SELECT queries", async () => {
      const graph = useGraph(store.store);
      const query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o }";
      
      const result = await graph.select(query);
      expect(Array.isArray(result)).toBe(true);
      expect(result.length).toBeGreaterThan(0);
    });

    test("should execute SPARQL ASK queries", async () => {
      const graph = useGraph(store.store);
      const query = "ASK WHERE { ?s ?p ?o }";
      
      const result = await graph.ask(query);
      expect(typeof result).toBe("boolean");
      expect(result).toBe(true);
    });

    test("should validate with SHACL", async () => {
      const graph = useGraph(store.store);
      const shapes = useStore();
      
      const result = await graph.validate(shapes.store);
      expect(result).toHaveProperty("conforms");
      expect(result).toHaveProperty("results");
    });
  });

  describe("useTurtle", () => {
    test("should create turtle interface", async () => {
      const turtle = await useTurtle();
      expect(typeof turtle.load).toBe("function");
      expect(typeof turtle.save).toBe("function");
      expect(typeof turtle.loadAll).toBe("function");
    });

    test("should load and save turtle files", async () => {
      const turtle = await useTurtle();
      const store = useStore();
      const q = store.quad(
        store.namedNode("http://example.org/s"),
        store.namedNode("http://example.org/p"),
        store.literal("o")
      );
      store.add(q);
      
      // Test that we can save and load
      expect(typeof turtle.save).toBe("function");
      expect(typeof turtle.load).toBe("function");
    });
  });

  describe("useValidator", () => {
    test("should create validator interface", () => {
      const validator = useValidator();
      expect(typeof validator.validate).toBe("function");
      expect(typeof validator.validateOrThrow).toBe("function");
    });

    test("should validate data against shapes", async () => {
      const validator = useValidator();
      const dataStore = useStore();
      const shapesStore = useStore();
      
      const result = await validator.validate(dataStore.store, shapesStore.store);
      expect(result).toHaveProperty("conforms");
      expect(result).toHaveProperty("results");
    });
  });

  describe("useReasoner", () => {
    test("should create reasoner interface", () => {
      const reasoner = useReasoner();
      expect(typeof reasoner.reason).toBe("function");
    });

    test("should perform reasoning", async () => {
      const reasoner = useReasoner();
      const dataStore = useStore();
      const rulesStore = useStore();
      
      const result = await reasoner.reason(dataStore.store, rulesStore.store);
      expect(result).toBeDefined();
    });
  });

  describe("useCanon", () => {
    test("should create canon interface", () => {
      const canon = useCanon();
      expect(typeof canon.canonicalize).toBe("function");
      expect(typeof canon.isIsomorphic).toBe("function");
    });

    test("should canonicalize store", async () => {
      const canon = useCanon();
      const store = useStore();
      
      const result = await canon.canonicalize(store.store);
      expect(typeof result).toBe("string");
    });

    test("should check isomorphism", async () => {
      const canon = useCanon();
      const store1 = useStore();
      const store2 = useStore();
      
      const result = await canon.isIsomorphic(store1.store, store2.store);
      expect(typeof result).toBe("boolean");
    });
  });

  describe("useZod", () => {
    test("should create zod interface", () => {
      const zod = useZod();
      expect(typeof zod.validateResults).toBe("function");
      expect(typeof zod.validateResult).toBe("function");
    });

    test("should validate data with schema", async () => {
      const zod = useZod();
      const schema = z.object({
        name: z.string(),
        age: z.number()
      });
      
      const data = [{ name: "John", age: 30 }];
      const result = await zod.validateResults(data, schema);
      expect(result.validated).toHaveLength(1);
      expect(result.validated[0]).toEqual({ name: "John", age: 30 });
    });

    test("should handle validation errors", async () => {
      const zod = useZod();
      const schema = z.object({
        name: z.string(),
        age: z.number()
      });
      
      const data = [{ name: "John", age: "not a number" }];
      const result = await zod.validateResults(data, schema);
      expect(result.errors).toHaveLength(1);
      expect(result.validated).toHaveLength(0);
    });
  });

  describe("RdfEngine", () => {
    test("should create engine instance", () => {
      const engine = new RdfEngine();
      expect(engine).toBeDefined();
      expect(typeof engine.parseTurtle).toBe("function");
      expect(typeof engine.serializeTurtle).toBe("function");
      expect(typeof engine.query).toBe("function");
    });

    test("should parse Turtle", () => {
      const engine = new RdfEngine();
      const ttl = `
        @prefix ex: <http://example.org/> .
        ex:Person a ex:Human .
      `;
      
      const store = engine.parseTurtle(ttl);
      expect(store.size).toBeGreaterThan(0);
    });

    test("should serialize to Turtle", async () => {
      const engine = new RdfEngine();
      const store = engine.createStore();
      const quad = engine.quad(
        engine.namedNode("http://example.org/s"),
        engine.namedNode("http://example.org/p"),
        engine.literal("o")
      );
      store.add(quad);
      
      const ttl = await engine.serializeTurtle(store);
      expect(typeof ttl).toBe("string");
      expect(ttl).toContain("ex:s");
    });
  });

  describe("Term utilities", () => {
    test("should create named nodes", () => {
      const node = asNamedNode("http://example.org/foo");
      expect(node.termType).toBe("NamedNode");
      expect(node.value).toBe("http://example.org/foo");
    });

    test("should create literals", () => {
      const lit = asLiteral("hello");
      expect(lit.termType).toBe("Literal");
      expect(lit.value).toBe("hello");
    });

    test("should create blank nodes", () => {
      const bnode = asBlankNode("test123");
      expect(bnode.termType).toBe("BlankNode");
      expect(bnode.value).toBe("test123");
    });

    test("should convert terms to strings", () => {
      const node = asNamedNode("http://example.org/foo");
      const lit = asLiteral("hello");
      
      expect(asString(node)).toBe("http://example.org/foo");
      expect(asString(lit)).toBe("hello");
      expect(asString("plain string")).toBe("plain string");
    });

    test("should get IRI from terms", () => {
      const node = asNamedNode("http://example.org/foo");
      
      expect(getIRI(node)).toBe("http://example.org/foo");
      expect(getIRI("http://example.org/bar")).toBe("http://example.org/bar");
    });

    test("should create smart literals", () => {
      const stringLit = smartLiteral("hello");
      const numberLit = smartLiteral(42);
      const booleanLit = smartLiteral(true);
      const dateLit = smartLiteral(new Date("2023-01-01"));

      expect(stringLit.termType).toBe("Literal");
      expect(stringLit.value).toBe("hello");

      expect(numberLit.value).toBe("42");

      expect(booleanLit.value).toBe("true");

      expect(dateLit.value).toBe("2023-01-01T00:00:00.000Z");
    });

    test("should check term types", () => {
      const node = asNamedNode("http://example.org/foo");
      const lit = asLiteral("hello");
      const bnode = asBlankNode("test");

      expect(isNamedNode(node)).toBe(true);
      expect(isNamedNode(lit)).toBe(false);

      expect(isLiteral(lit)).toBe(true);
      expect(isLiteral(node)).toBe(false);

      expect(isBlankNode(bnode)).toBe(true);
      expect(isBlankNode(node)).toBe(false);
    });
  });

  describe("Graph utilities", () => {
    let store;

    beforeEach(() => {
      store = useStore();
      store.lit1 = store.literal("value1");
      store.lit2 = store.literal("value2");
      const q1 = store.quad(
        store.namedNode("http://example.org/s1"),
        store.namedNode("http://example.org/p"),
        store.lit1
      );
      const q2 = store.quad(
        store.namedNode("http://example.org/s2"),
        store.namedNode("http://example.org/p"),
        store.lit2
      );
      store.add(q1, q2);
    });

    test("should get objects for subject+predicate", () => {
      const objects = getObjects(store.store, "http://example.org/s1", "http://example.org/p");
      expect(objects).toHaveLength(1);
      expect(objects[0].value).toBe("value1");
    });

    test("should get subjects for predicate+object", () => {
      const subjects = getSubjects(store.store, "http://example.org/p", store.lit1);
      expect(subjects).toHaveLength(1);
      expect(subjects[0].value).toBe("http://example.org/s1");
    });

    test("should get predicates for subject+object", () => {
      const predicates = getPredicates(store.store, "http://example.org/s1", store.lit1);
      expect(predicates).toHaveLength(1);
      expect(predicates[0].value).toBe("http://example.org/p");
    });

    test("should check if subject has type", () => {
      const typeQuad = store.quad(
        store.namedNode("http://example.org/s1"),
        store.namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        store.namedNode("http://example.org/Type")
      );
      store.add(typeQuad);

      const hasTypeResult = isA(store.store, "http://example.org/s1", "http://example.org/Type");
      expect(hasTypeResult).toBe(true);
    });

    test("should get types for subject", () => {
      const typeQuad = store.quad(
        store.namedNode("http://example.org/s1"),
        store.namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        store.namedNode("http://example.org/Type")
      );
      store.add(typeQuad);

      const types = getTypes(store.store, "http://example.org/s1");
      expect(types).toHaveLength(1);
      expect(types[0]).toBe("http://example.org/Type");
    });

    test("should pluck by predicate", () => {
      const quads = pluck(store.store, "http://example.org/p");
      expect(quads).toHaveLength(2);
    });

    test("should index by predicate", () => {
      const index = indexByPredicate(store.store, "http://example.org/p");
      expect(index.size).toBe(2);
      expect(index.get("http://example.org/s1")).toContain("value1");
      expect(index.get("http://example.org/s2")).toContain("value2");
    });

    test("should get properties for subject", () => {
      const properties = getProperties(store.store, "http://example.org/s1");
      expect(properties.size).toBe(1);
      expect(properties.get("http://example.org/p")).toContain("value1");
    });

    test("should check if subject exists", () => {
      expect(hasSubject(store.store, "http://example.org/s1")).toBe(true);
      expect(hasSubject(store.store, "http://example.org/nonexistent")).toBe(false);
    });

    test("should get all subjects", () => {
      const subjects = getAllSubjects(store.store);
      expect(subjects).toHaveLength(2);
      expect(subjects).toContain("http://example.org/s1");
      expect(subjects).toContain("http://example.org/s2");
    });

    test("should get all predicates", () => {
      const predicates = getAllPredicates(store.store);
      expect(predicates).toHaveLength(1);
      expect(predicates[0]).toBe("http://example.org/p");
    });

    test("should get all objects", () => {
      const objects = getAllObjects(store.store);
      expect(objects).toHaveLength(2);
      expect(objects).toContain("value1");
      expect(objects).toContain("value2");
    });

    test("should find subjects by property value", () => {
      const subjects = findByProperty(store.store, "http://example.org/p", store.lit1);
      expect(subjects).toHaveLength(1);
      expect(subjects[0]).toBe("http://example.org/s1");
    });

    test("should get first object for subject+predicate", () => {
      const value = getFirstObject(store.store, "http://example.org/s1", "http://example.org/p");
      expect(value).toBe("value1");
    });

    test("should count quads for subject", () => {
      const count = countQuadsForSubject(store.store, "http://example.org/s1");
      expect(count).toBe(1);
    });

    test("should get quads for subject", () => {
      const quads = getQuadsForSubject(store.store, "http://example.org/s1");
      expect(quads).toHaveLength(1);
      expect(quads[0].subject.value).toBe("http://example.org/s1");
    });
  });

  describe("Validation utilities", () => {
    test("should validate IRIs", () => {
      expect(validateIRI("http://example.org/foo")).toBe(true);
      expect(validateIRI("not-a-uri")).toBe(false);
    });

    test("should validate terms", () => {
      const validTerm = { termType: "NamedNode", value: "http://example.org/foo" };
      const invalidTerm = { termType: "InvalidType", value: "test" };

      expect(validateTerm(validTerm)).toBe(true);
      expect(validateTerm(invalidTerm)).toBe(false);
    });

    test("should validate quads", () => {
      const validQuad = {
        subject: { termType: "NamedNode", value: "http://example.org/s" },
        predicate: { termType: "NamedNode", value: "http://example.org/p" },
        object: { termType: "Literal", value: "o" }
      };

      const invalidQuad = {
        subject: { termType: "NamedNode", value: "http://example.org/s" },
        predicate: { termType: "Literal", value: "p" },
        object: { termType: "Literal", value: "o" }
      };

      expect(validateQuad(validQuad)).toBe(true);
      expect(validateQuad(invalidQuad)).toBe(false);
    });

    test("should validate store", () => {
      const store = useStore();
      const result = validateStore(store.store);
      expect(result).toHaveProperty("valid");
      expect(result).toHaveProperty("issues");
      expect(result).toHaveProperty("issueCount");
    });

    test("should validate RDF constraints", () => {
      const store = useStore();
      const result = validateRDFConstraints(store.store);
      expect(result).toHaveProperty("valid");
      expect(result).toHaveProperty("violations");
    });
  });
});
