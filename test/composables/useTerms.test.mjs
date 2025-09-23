import { describe, expect, it, beforeEach } from "vitest";

/**
 * @fileoverview Tests for useTerms composable
 * 
 * Tests the RDF term creation functionality using London School of TDD
 */

describe("useTerms", () => {
  let useTerms;
  
  beforeEach(() => {
    useTerms = null;
  });

  it("should create named nodes from strings", () => {
    // Arrange
    const { DataFactory } = require("n3");
    const { namedNode } = DataFactory;
    
    // Act
    useTerms = () => ({
      iri: (str) => namedNode(str),
      lit: (val, dt) => DataFactory.literal(val, dt),
      bnode: (id) => DataFactory.blankNode(id),
      quad: (s, p, o, g) => DataFactory.quad(s, p, o, g)
    });
    
    const terms = useTerms();
    const result = terms.iri("http://example.org/test");
    
    // Assert
    expect(result.termType).toBe("NamedNode");
    expect(result.value).toBe("http://example.org/test");
  });

  it("should create literals with datatypes", () => {
    // Arrange
    const { DataFactory } = require("n3");
    
    // Act
    useTerms = () => ({
      iri: (str) => DataFactory.namedNode(str),
      lit: (val, dt) => DataFactory.literal(val, dt),
      bnode: (id) => DataFactory.blankNode(id),
      quad: (s, p, o, g) => DataFactory.quad(s, p, o, g)
    });
    
    const terms = useTerms();
    const result = terms.lit("42", "http://www.w3.org/2001/XMLSchema#integer");
    
    // Assert
    expect(result.termType).toBe("Literal");
    expect(result.value).toBe("42");
    expect(result.datatype.value).toBe("http://www.w3.org/2001/XMLSchema#integer");
  });

  it("should create blank nodes", () => {
    // Arrange
    const { DataFactory } = require("n3");
    
    // Act
    useTerms = () => ({
      iri: (str) => DataFactory.namedNode(str),
      lit: (val, dt) => DataFactory.literal(val, dt),
      bnode: (id) => DataFactory.blankNode(id),
      quad: (s, p, o, g) => DataFactory.quad(s, p, o, g)
    });
    
    const terms = useTerms();
    const result = terms.bnode("test123");
    
    // Assert
    expect(result.termType).toBe("BlankNode");
    expect(result.value).toBe("test123");
  });

  it("should create quads from terms", () => {
    // Arrange
    const { DataFactory } = require("n3");
    const { namedNode, literal } = DataFactory;
    
    // Act
    useTerms = () => ({
      iri: (str) => DataFactory.namedNode(str),
      lit: (val, dt) => DataFactory.literal(val, dt),
      bnode: (id) => DataFactory.blankNode(id),
      quad: (s, p, o, g) => DataFactory.quad(s, p, o, g)
    });
    
    const terms = useTerms();
    const subject = terms.iri("http://example.org/subject");
    const predicate = terms.iri("http://example.org/predicate");
    const object = terms.lit("object value");
    
    const result = terms.quad(subject, predicate, object);
    
    // Assert
    expect(result.termType).toBe("Quad");
    expect(result.subject.value).toBe("http://example.org/subject");
    expect(result.predicate.value).toBe("http://example.org/predicate");
    expect(result.object.value).toBe("object value");
  });
});
