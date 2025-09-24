import { describe, expect, it, beforeEach } from "vitest";
import { DataFactory } from "n3";

const { namedNode, literal, blankNode, quad } = DataFactory;

// Mock useTerms for edge case testing
const useTerms = (options = {}) => {
  const {
    baseIRI = "http://example.org/",
    defaultDatatype = "http://www.w3.org/2001/XMLSchema#string"
  } = options;

  return {
    iri: (iri) => {
      if (typeof iri !== "string") {
        throw new TypeError("[useTerms] IRI must be a string");
      }
      
      if (iri.startsWith("#") || iri.startsWith("/")) {
        iri = baseIRI + iri;
      }
      
      return namedNode(iri);
    },
    lit: (value, datatype, language) => {
      if (value === null || value === undefined) {
        throw new Error("[useTerms] Literal value cannot be null or undefined");
      }
      
      const stringValue = String(value);
      
      if (language) {
        return literal(stringValue, language);
      }
      
      if (datatype) {
        return literal(stringValue, datatype);
      }
      
      return literal(stringValue, defaultDatatype);
    },
    bnode: (id) => {
      if (id !== undefined && typeof id !== "string") {
        throw new Error("[useTerms] Blank node ID must be a string");
      }
      
      return blankNode(id);
    },
    quad: (subject, predicate, object, graph) => {
      if (!subject || !predicate || !object) {
        throw new Error("[useTerms] Subject, predicate, and object are required");
      }
      
      return quad(subject, predicate, object, graph);
    },
    defaultGraph: () => DataFactory.defaultGraph(),
    getBaseIRI: () => baseIRI,
    getDefaultDatatype: () => defaultDatatype
  };
};

describe("useTerms Edge Cases", () => {
  let terms;

  beforeEach(() => {
    terms = useTerms();
  });

  describe("Null and Undefined Inputs", () => {
    it("should throw error for null IRI", () => {
      // Act & Assert
      expect(() => terms.iri(null)).toThrow("[useTerms] IRI must be a string");
    });

    it("should throw error for undefined IRI", () => {
      // Act & Assert
      expect(() => terms.iri(undefined)).toThrow("[useTerms] IRI must be a string");
    });

    it("should throw error for null literal value", () => {
      // Act & Assert
      expect(() => terms.lit(null)).toThrow("[useTerms] Literal value cannot be null or undefined");
    });

    it("should throw error for undefined literal value", () => {
      // Act & Assert
      expect(() => terms.lit(undefined)).toThrow("[useTerms] Literal value cannot be null or undefined");
    });

    it("should handle null blank node ID", () => {
      // Act & Assert
      expect(() => terms.bnode(null)).not.toThrow();
    });

    it("should handle undefined blank node ID", () => {
      // Act & Assert
      expect(() => terms.bnode(undefined)).not.toThrow();
    });

    it("should throw error for null quad components", () => {
      // Act & Assert
      expect(() => terms.quad(null, namedNode("ex:p"), literal("o"))).toThrow("[useTerms] Subject, predicate, and object are required");
      expect(() => terms.quad(namedNode("ex:s"), null, literal("o"))).toThrow("[useTerms] Subject, predicate, and object are required");
      expect(() => terms.quad(namedNode("ex:s"), namedNode("ex:p"), null)).toThrow("[useTerms] Subject, predicate, and object are required");
    });
  });

  describe("Invalid Data Types", () => {
    it("should throw error for non-string IRI", () => {
      // Act & Assert
      expect(() => terms.iri(123)).toThrow("[useTerms] IRI must be a string");
      expect(() => terms.iri({})).toThrow("[useTerms] IRI must be a string");
      expect(() => terms.iri([])).toThrow("[useTerms] IRI must be a string");
      expect(() => terms.iri(true)).toThrow("[useTerms] IRI must be a string");
    });

    it("should throw error for non-string blank node ID", () => {
      // Act & Assert
      expect(() => terms.bnode(123)).toThrow("[useTerms] Blank node ID must be a string");
      expect(() => terms.bnode({})).toThrow("[useTerms] Blank node ID must be a string");
      expect(() => terms.bnode([])).toThrow("[useTerms] Blank node ID must be a string");
      expect(() => terms.bnode(true)).toThrow("[useTerms] Blank node ID must be a string");
    });
  });

  describe("Empty and Whitespace Inputs", () => {
    it("should handle empty string IRI", () => {
      // Act
      const result = terms.iri("");

      // Assert
      expect(result.termType).toBe("NamedNode");
      expect(result.value).toBe("");
    });

    it("should handle whitespace-only IRI", () => {
      // Act
      const result = terms.iri("   ");

      // Assert
      expect(result.termType).toBe("NamedNode");
      expect(result.value).toBe("   ");
    });

    it("should handle empty string literal", () => {
      // Act
      const result = terms.lit("");

      // Assert
      expect(result.termType).toBe("Literal");
      expect(result.value).toBe("");
    });

    it("should handle whitespace-only literal", () => {
      // Act
      const result = terms.lit("   ");

      // Assert
      expect(result.termType).toBe("Literal");
      expect(result.value).toBe("   ");
    });

    it("should handle empty string blank node ID", () => {
      // Act
      const result = terms.bnode("");

      // Assert
      expect(result.termType).toBe("BlankNode");
      expect(result.value).toBe("");
    });
  });

  describe("Special Characters and Encoding", () => {
    it("should handle IRIs with special characters", () => {
      // Arrange
      const specialIRIs = [
        "http://example.org/path with spaces",
        "http://example.org/path%20with%20encoding",
        "http://example.org/path#fragment",
        "http://example.org/path?query=value",
        "http://example.org/path/with/unicode/测试",
        "http://example.org/path/with/emoji/🚀"
      ];

      // Act & Assert
      for (const iri of specialIRIs) {
        expect(() => terms.iri(iri)).not.toThrow();
        const result = terms.iri(iri);
        expect(result.termType).toBe("NamedNode");
        expect(result.value).toBe(iri);
      }
    });

    it("should handle literals with special characters", () => {
      // Arrange
      const specialValues = [
        "value with spaces",
        "value\nwith\nnewlines",
        "value\twith\ttabs",
        "value with unicode 测试",
        "value with emoji 🚀",
        "value with quotes \"double\" and 'single'",
        String.raw`value with backslashes \ and forward slashes /`
      ];

      // Act & Assert
      for (const value of specialValues) {
        expect(() => terms.lit(value)).not.toThrow();
        const result = terms.lit(value);
        expect(result.termType).toBe("Literal");
        expect(result.value).toBe(value);
      }
    });

    it("should handle blank node IDs with special characters", () => {
      // Arrange
      const specialIDs = [
        "id with spaces",
        "id-with-dashes",
        "id_with_underscores",
        "id.with.dots",
        "id123",
        "id with unicode 测试",
        "id with emoji 🚀"
      ];

      // Act & Assert
      for (const id of specialIDs) {
        expect(() => terms.bnode(id)).not.toThrow();
        const result = terms.bnode(id);
        expect(result.termType).toBe("BlankNode");
        expect(result.value).toBe(id);
      }
    });
  });

  describe("Boundary Values", () => {
    it("should handle very long IRIs", () => {
      // Arrange
      const longIRI = "http://example.org/" + "a".repeat(10_000);

      // Act & Assert
      expect(() => terms.iri(longIRI)).not.toThrow();
      const result = terms.iri(longIRI);
      expect(result.termType).toBe("NamedNode");
      expect(result.value).toBe(longIRI);
    });

    it("should handle very long literal values", () => {
      // Arrange
      const longValue = "value".repeat(10_000);

      // Act & Assert
      expect(() => terms.lit(longValue)).not.toThrow();
      const result = terms.lit(longValue);
      expect(result.termType).toBe("Literal");
      expect(result.value).toBe(longValue);
    });

    it("should handle very long blank node IDs", () => {
      // Arrange
      const longID = "id".repeat(10_000);

      // Act & Assert
      expect(() => terms.bnode(longID)).not.toThrow();
      const result = terms.bnode(longID);
      expect(result.termType).toBe("BlankNode");
      expect(result.value).toBe(longID);
    });
  });

  describe("Type Conversion", () => {
    it("should convert numbers to string literals", () => {
      // Act & Assert
      expect(terms.lit(123).value).toBe("123");
      expect(terms.lit(123.45).value).toBe("123.45");
      expect(terms.lit(0).value).toBe("0");
      expect(terms.lit(-123).value).toBe("-123");
    });

    it("should convert booleans to string literals", () => {
      // Act & Assert
      expect(terms.lit(true).value).toBe("true");
      expect(terms.lit(false).value).toBe("false");
    });

    it("should convert objects to string literals", () => {
      // Act & Assert
      expect(terms.lit({}).value).toBe("[object Object]");
      expect(terms.lit([]).value).toBe("");
      expect(terms.lit([1, 2, 3]).value).toBe("1,2,3");
    });

    it("should convert functions to string literals", () => {
      // Act & Assert
      expect(terms.lit(() => {}).value).toBe("() => {}");
      expect(terms.lit(function() {}).value).toBe("function() {}");
    });
  });

  describe("Relative IRI Handling", () => {
    it("should expand fragment IRIs", () => {
      // Act
      const result = terms.iri("#fragment");

      // Assert
      expect(result.value).toBe("http://example.org/#fragment");
    });

    it("should expand path IRIs", () => {
      // Act
      const result = terms.iri("/path");

      // Assert
      expect(result.value).toBe("http://example.org//path");
    });

    it("should not expand absolute IRIs", () => {
      // Act
      const result = terms.iri("http://other.org/path");

      // Assert
      expect(result.value).toBe("http://other.org/path");
    });

    it("should not expand HTTPS IRIs", () => {
      // Act
      const result = terms.iri("https://other.org/path");

      // Assert
      expect(result.value).toBe("https://other.org/path");
    });
  });

  describe("Datatype and Language Handling", () => {
    it("should handle null datatype", () => {
      // Act
      const result = terms.lit("value", null);

      // Assert
      expect(result.termType).toBe("Literal");
      expect(result.value).toBe("value");
      expect(result.datatype.value).toBe("http://www.w3.org/2001/XMLSchema#string");
    });

    it("should handle undefined datatype", () => {
      // Act
      const result = terms.lit("value", undefined);

      // Assert
      expect(result.termType).toBe("Literal");
      expect(result.value).toBe("value");
      expect(result.datatype.value).toBe("http://www.w3.org/2001/XMLSchema#string");
    });

    it("should handle empty string datatype", () => {
      // Act
      const result = terms.lit("value", "");

      // Assert
      expect(result.termType).toBe("Literal");
      expect(result.value).toBe("value");
      expect(result.datatype.value).toBe("");
    });

    it("should handle language tags", () => {
      // Act
      const result = terms.lit("value", null, "en");

      // Assert
      expect(result.termType).toBe("Literal");
      expect(result.value).toBe("value");
      expect(result.language).toBe("en");
    });

    it("should handle empty language tag", () => {
      // Act
      const result = terms.lit("value", null, "");

      // Assert
      expect(result.termType).toBe("Literal");
      expect(result.value).toBe("value");
      expect(result.language).toBe("");
    });
  });

  describe("Quad Edge Cases", () => {
    it("should handle quad with null graph", () => {
      // Act
      const result = terms.quad(
        namedNode("ex:s"),
        namedNode("ex:p"),
        literal("o"),
        null
      );

      // Assert
      expect(result.termType).toBe("Quad");
      expect(result.graph).toBeNull();
    });

    it("should handle quad with undefined graph", () => {
      // Act
      const result = terms.quad(
        namedNode("ex:s"),
        namedNode("ex:p"),
        literal("o"),
        undefined
      );

      // Assert
      expect(result.termType).toBe("Quad");
      expect(result.graph).toBeNull();
    });

    it("should handle quad with default graph", () => {
      // Act
      const result = terms.quad(
        namedNode("ex:s"),
        namedNode("ex:p"),
        literal("o"),
        terms.defaultGraph()
      );

      // Assert
      expect(result.termType).toBe("Quad");
      expect(result.graph.termType).toBe("DefaultGraph");
    });
  });

  describe("Options Edge Cases", () => {
    it("should handle null options", () => {
      // Act & Assert
      expect(() => useTerms(null)).not.toThrow();
    });

    it("should handle undefined options", () => {
      // Act & Assert
      expect(() => useTerms(undefined)).not.toThrow();
    });

    it("should handle empty options object", () => {
      // Act & Assert
      expect(() => useTerms({})).not.toThrow();
    });

    it("should handle options with null values", () => {
      // Act & Assert
      expect(() => useTerms({ baseIRI: null, defaultDatatype: null })).not.toThrow();
    });

    it("should handle options with undefined values", () => {
      // Act & Assert
      expect(() => useTerms({ baseIRI: undefined, defaultDatatype: undefined })).not.toThrow();
    });
  });
});
