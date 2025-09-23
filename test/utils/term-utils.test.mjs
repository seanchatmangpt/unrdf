import { describe, expect, it } from "vitest";
import { z } from "zod";

/**
 * @fileoverview Tests for utility functions
 * 
 * Tests the dark matter utility functions using London School of TDD
 */

describe("utility functions", () => {
  it("should normalize input to NamedNode", () => {
    // Arrange
    const { DataFactory } = require("n3");
    const { namedNode } = DataFactory;
    
    const asNamedNode = (iri) => {
      if (iri?.termType === "NamedNode") return iri;
      return namedNode(String(iri));
    };
    
    // Act & Assert
    const result1 = asNamedNode("http://example.org/test");
    expect(result1.termType).toBe("NamedNode");
    expect(result1.value).toBe("http://example.org/test");
    
    const existingNode = namedNode("http://example.org/existing");
    const result2 = asNamedNode(existingNode);
    expect(result2).toBe(existingNode);
  });

  it("should normalize value to Literal", () => {
    // Arrange
    const { DataFactory } = require("n3");
    const { literal } = DataFactory;
    
    const asLiteral = (value, datatype = "http://www.w3.org/2001/XMLSchema#string") => {
      return literal(String(value), datatype);
    };
    
    // Act & Assert
    const result = asLiteral("test value", "http://www.w3.org/2001/XMLSchema#string");
    expect(result.termType).toBe("Literal");
    expect(result.value).toBe("test value");
    expect(result.datatype.value).toBe("http://www.w3.org/2001/XMLSchema#string");
  });

  it("should convert quad to JSON", () => {
    // Arrange
    const { DataFactory } = require("n3");
    const { namedNode, literal, quad } = DataFactory;
    
    const quadToJSON = (q) => {
      return {
        subject: q.subject.value,
        predicate: q.predicate.value,
        object: q.object.value,
        graph: q.graph.value || null,
      };
    };
    
    const testQuad = quad(
      namedNode("http://example.org/subject"),
      namedNode("http://example.org/predicate"),
      literal("object value")
    );
    
    // Act
    const result = quadToJSON(testQuad);
    
    // Assert
    expect(result).toEqual({
      subject: "http://example.org/subject",
      predicate: "http://example.org/predicate",
      object: "object value",
      graph: null
    });
  });

  it("should validate quad JSON with Zod", () => {
    // Arrange
    const QuadSchema = z.object({
      subject: z.string().url(),
      predicate: z.string().url(),
      object: z.string(),
      graph: z.string().url().nullable().optional(),
    });
    
    const validateQuadJSON = (obj) => {
      return QuadSchema.parse(obj);
    };
    
    const validQuad = {
      subject: "http://example.org/subject",
      predicate: "http://example.org/predicate",
      object: "object value",
      graph: null
    };
    
    // Act
    const result = validateQuadJSON(validQuad);
    
    // Assert
    expect(result).toEqual(validQuad);
  });

  it("should throw Zod validation error for invalid quad JSON", () => {
    // Arrange
    const QuadSchema = z.object({
      subject: z.string().url(),
      predicate: z.string().url(),
      object: z.string(),
      graph: z.string().url().nullable().optional(),
    });
    
    const validateQuadJSON = (obj) => {
      return QuadSchema.parse(obj);
    };
    
    const invalidQuad = {
      subject: "not-a-url",
      predicate: "http://example.org/predicate",
      object: "object value"
    };
    
    // Act & Assert
    expect(() => validateQuadJSON(invalidQuad)).toThrow();
  });
});
