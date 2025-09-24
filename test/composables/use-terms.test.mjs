/**
 * @fileoverview Tests for useTerms composable
 */

import { describe, it, expect } from "vitest";
import { useTerms } from "../../src/composables/use-terms.mjs";

describe("useTerms", () => {
  it("should create a terms composable", () => {
    const terms = useTerms();
    expect(terms).toBeDefined();
    expect(typeof terms.iri).toBe("function");
    expect(typeof terms.lit).toBe("function");
    expect(typeof terms.bnode).toBe("function");
    expect(typeof terms.quad).toBe("function");
  });

  it("should create named nodes", () => {
    const terms = useTerms();
    const node = terms.iri("http://example.org/person");
    
    expect(node.termType).toBe("NamedNode");
    expect(node.value).toBe("http://example.org/person");
  });

  it("should handle relative IRIs with baseIRI", () => {
    const terms = useTerms({ baseIRI: "http://example.org/" });
    const node = terms.iri("person");
    
    expect(node.termType).toBe("NamedNode");
    expect(node.value).toBe("http://example.org/person");
  });

  it("should create literals", () => {
    const terms = useTerms();
    const literal = terms.lit("John Doe");
    
    expect(literal.termType).toBe("Literal");
    expect(literal.value).toBe("John Doe");
  });

  it("should create literals with datatype", () => {
    const terms = useTerms();
    const literal = terms.lit("30", "http://www.w3.org/2001/XMLSchema#integer");
    
    expect(literal.termType).toBe("Literal");
    expect(literal.value).toBe("30");
    expect(literal.datatype.value).toBe("http://www.w3.org/2001/XMLSchema#integer");
  });

  it("should create literals with language", () => {
    const terms = useTerms();
    const literal = terms.lit("Hello", null, "en");
    
    expect(literal.termType).toBe("Literal");
    expect(literal.value).toBe("Hello");
    expect(literal.language).toBe("en");
  });

  it("should create blank nodes", () => {
    const terms = useTerms();
    const bnode = terms.bnode("person1");
    
    expect(bnode.termType).toBe("BlankNode");
    expect(bnode.value).toBe("person1");
  });

  it("should create blank nodes without ID", () => {
    const terms = useTerms();
    const bnode = terms.bnode();
    
    expect(bnode.termType).toBe("BlankNode");
    expect(bnode.value).toBeDefined();
  });

  it("should create quads", () => {
    const terms = useTerms();
    const subject = terms.iri("http://example.org/person");
    const predicate = terms.iri("http://example.org/name");
    const object = terms.lit("John Doe");
    
    const quad = terms.quad(subject, predicate, object);
    
    expect(quad.termType).toBe("Quad");
    expect(quad.subject).toBe(subject);
    expect(quad.predicate).toBe(predicate);
    expect(quad.object).toBe(object);
  });

  it("should create quads with graph", () => {
    const terms = useTerms();
    const subject = terms.iri("http://example.org/person");
    const predicate = terms.iri("http://example.org/name");
    const object = terms.lit("John Doe");
    const graph = terms.iri("http://example.org/graph");
    
    const quad = terms.quad(subject, predicate, object, graph);
    
    expect(quad.termType).toBe("Quad");
    expect(quad.graph).toBe(graph);
  });

  it("should create default graph", () => {
    const terms = useTerms();
    const graph = terms.defaultGraph();
    
    expect(graph.termType).toBe("DefaultGraph");
  });

  it("should get base IRI", () => {
    const terms = useTerms({ baseIRI: "http://test.org/" });
    expect(terms.getBaseIRI()).toBe("http://test.org/");
  });

  it("should get default datatype", () => {
    const terms = useTerms({ defaultDatatype: "http://www.w3.org/2001/XMLSchema#string" });
    expect(terms.getDefaultDatatype()).toBe("http://www.w3.org/2001/XMLSchema#string");
  });

  it("should throw error for invalid IRI", () => {
    const terms = useTerms();
    expect(() => terms.iri(null)).toThrow("[useTerms] IRI must be a string");
  });

  it("should throw error for invalid literal value", () => {
    const terms = useTerms();
    expect(() => terms.lit(null)).toThrow("[useTerms] Literal value cannot be null or undefined");
  });

  it("should throw error for invalid blank node ID", () => {
    const terms = useTerms();
    expect(() => terms.bnode(123)).toThrow("[useTerms] Blank node ID must be a string");
  });

  it("should throw error for invalid quad parameters", () => {
    const terms = useTerms();
    expect(() => terms.quad(null, null, null)).toThrow("[useTerms] Subject, predicate, and object are required");
  });
});