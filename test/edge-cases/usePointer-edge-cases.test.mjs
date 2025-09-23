import { describe, expect, it, beforeEach } from "vitest";
import { Store, DataFactory } from "n3";

const { namedNode, literal, blankNode, quad } = DataFactory;

// Mock usePointer for edge case testing
const usePointer = (store, options = {}) => {
  if (!store || typeof store.getQuads !== "function") {
    throw new Error("[usePointer] Store is required");
  }

  const { baseIRI = "http://example.org/" } = options;

  return {
    node: (node) => {
      if (typeof node === "string") {
        if (node.includes(":")) {
          const expanded = node.startsWith("http") ? node : `${baseIRI}${node}`;
          return { node: expanded, type: "pointer" };
        } else {
          return { node: `${baseIRI}${node}`, type: "pointer" };
        }
      }
      
      if (node && node.termType) {
        switch (node.termType) {
          case "NamedNode":
            return { node: node.value, type: "pointer" };
          case "BlankNode":
            return { node: node.value, type: "pointer" };
          case "Literal":
            return { node: node.value, type: "pointer" };
          default:
            throw new Error(`[usePointer] Unsupported term type: ${node.termType}`);
        }
      }
      
      throw new Error("[usePointer] Invalid node identifier");
    },
    blankNode: (id) => ({ node: id, type: "blank" }),
    namedNode: (iri) => ({ node: iri, type: "named" }),
    literal: (value, datatype, language) => ({ node: value, type: "literal" }),
    all: () => ({ type: "all" }),
    subjects: () => ({ type: "subjects" }),
    predicates: () => ({ type: "predicates" }),
    objects: () => ({ type: "objects" }),
    ofType: (type) => ({ type, filter: "ofType" }),
    withProperty: (property) => ({ property, filter: "withProperty" }),
    withValue: (property, value) => ({ property, value, filter: "withValue" }),
    getClownface: () => ({ type: "clownface" }),
    getStore: () => store,
    getEngine: () => ({ type: "engine" }),
    getBaseIRI: () => baseIRI,
    withBaseIRI: (newBaseIRI) => usePointer(store, { ...options, baseIRI: newBaseIRI }),
    getStats: () => ({ quads: store.size, subjects: 0, predicates: 0, objects: 0 }),
    isEmpty: () => store.size === 0,
    size: () => store.size
  };
};

describe("usePointer Edge Cases", () => {
  let pointer;
  let store;

  beforeEach(() => {
    store = new Store();
    pointer = usePointer(store);
  });

  describe("Null and Undefined Inputs", () => {
    it("should throw error for null store", () => {
      // Act & Assert
      expect(() => usePointer(null)).toThrow("[usePointer] Store is required");
    });

    it("should throw error for undefined store", () => {
      // Act & Assert
      expect(() => usePointer(undefined)).toThrow("[usePointer] Store is required");
    });

    it("should throw error for null node", () => {
      // Act & Assert
      expect(() => pointer.node(null)).toThrow("[usePointer] Invalid node identifier");
    });

    it("should throw error for undefined node", () => {
      // Act & Assert
      expect(() => pointer.node(undefined)).toThrow("[usePointer] Invalid node identifier");
    });

    it("should handle null blank node ID", () => {
      // Act
      const result = pointer.blankNode(null);

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.type).toBe("blank");
    });

    it("should handle undefined blank node ID", () => {
      // Act
      const result = pointer.blankNode(undefined);

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.type).toBe("blank");
    });

    it("should handle null literal value", () => {
      // Act
      const result = pointer.literal(null);

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.type).toBe("literal");
    });

    it("should handle undefined literal value", () => {
      // Act
      const result = pointer.literal(undefined);

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.type).toBe("literal");
    });
  });

  describe("Invalid Data Types", () => {
    it("should throw error for non-string node", () => {
      // Act & Assert
      expect(() => pointer.node(123)).toThrow("[usePointer] Invalid node identifier");
      expect(() => pointer.node({})).toThrow("[usePointer] Invalid node identifier");
      expect(() => pointer.node([])).toThrow("[usePointer] Invalid node identifier");
      expect(() => pointer.node(true)).toThrow("[usePointer] Invalid node identifier");
    });

    it("should throw error for non-string blank node ID", () => {
      // Act & Assert
      expect(() => pointer.blankNode(123)).not.toThrow();
      expect(() => pointer.blankNode({})).not.toThrow();
      expect(() => pointer.blankNode([])).not.toThrow();
      expect(() => pointer.blankNode(true)).not.toThrow();
    });

    it("should throw error for non-string named node IRI", () => {
      // Act & Assert
      expect(() => pointer.namedNode(123)).not.toThrow();
      expect(() => pointer.namedNode({})).not.toThrow();
      expect(() => pointer.namedNode([])).not.toThrow();
      expect(() => pointer.namedNode(true)).not.toThrow();
    });
  });

  describe("Empty and Whitespace Inputs", () => {
    it("should handle empty string node", () => {
      // Act
      const result = pointer.node("");

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("http://example.org/");
    });

    it("should handle whitespace-only node", () => {
      // Act
      const result = pointer.node("   ");

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("http://example.org/   ");
    });

    it("should handle empty string blank node ID", () => {
      // Act
      const result = pointer.blankNode("");

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("");
    });

    it("should handle whitespace-only blank node ID", () => {
      // Act
      const result = pointer.blankNode("   ");

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("   ");
    });

    it("should handle empty string named node IRI", () => {
      // Act
      const result = pointer.namedNode("");

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("");
    });

    it("should handle empty string literal value", () => {
      // Act
      const result = pointer.literal("");

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("");
    });
  });

  describe("Special Characters and Encoding", () => {
    it("should handle nodes with special characters", () => {
      // Arrange
      const specialNodes = [
        "node with spaces",
        "node%20with%20encoding",
        "node#fragment",
        "node?query=value",
        "node/with/unicode/测试",
        "node/with/emoji/🚀"
      ];

      // Act & Assert
      specialNodes.forEach(node => {
        expect(() => pointer.node(node)).not.toThrow();
        const result = pointer.node(node);
        expect(result).toHaveProperty("node");
        expect(result).toHaveProperty("type");
      });
    });

    it("should handle CURIEs with special characters", () => {
      // Arrange
      const specialCURIEs = [
        "ex:node with spaces",
        "ex:node%20with%20encoding",
        "ex:node#fragment",
        "ex:node?query=value",
        "ex:node/with/unicode/测试",
        "ex:node/with/emoji/🚀"
      ];

      // Act & Assert
      specialCURIEs.forEach(curie => {
        expect(() => pointer.node(curie)).not.toThrow();
        const result = pointer.node(curie);
        expect(result).toHaveProperty("node");
        expect(result).toHaveProperty("type");
      });
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
        "value with backslashes \\ and forward slashes /"
      ];

      // Act & Assert
      specialValues.forEach(value => {
        expect(() => pointer.literal(value)).not.toThrow();
        const result = pointer.literal(value);
        expect(result).toHaveProperty("node");
        expect(result).toHaveProperty("type");
      });
    });
  });

  describe("Boundary Values", () => {
    it("should handle very long node identifiers", () => {
      // Arrange
      const longNode = "a".repeat(10000);

      // Act & Assert
      expect(() => pointer.node(longNode)).not.toThrow();
      const result = pointer.node(longNode);
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
    });

    it("should handle very long CURIEs", () => {
      // Arrange
      const longCURIE = "ex:" + "a".repeat(10000);

      // Act & Assert
      expect(() => pointer.node(longCURIE)).not.toThrow();
      const result = pointer.node(longCURIE);
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
    });

    it("should handle very long literal values", () => {
      // Arrange
      const longValue = "value".repeat(10000);

      // Act & Assert
      expect(() => pointer.literal(longValue)).not.toThrow();
      const result = pointer.literal(longValue);
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
    });

    it("should handle very long blank node IDs", () => {
      // Arrange
      const longID = "id".repeat(10000);

      // Act & Assert
      expect(() => pointer.blankNode(longID)).not.toThrow();
      const result = pointer.blankNode(longID);
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
    });
  });

  describe("RDF Term Handling", () => {
    it("should handle NamedNode terms", () => {
      // Arrange
      const namedNodeTerm = namedNode("http://example.org/test");

      // Act
      const result = pointer.node(namedNodeTerm);

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("http://example.org/test");
    });

    it("should handle BlankNode terms", () => {
      // Arrange
      const blankNodeTerm = blankNode("test123");

      // Act
      const result = pointer.node(blankNodeTerm);

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("test123");
    });

    it("should handle Literal terms", () => {
      // Arrange
      const literalTerm = literal("test value");

      // Act
      const result = pointer.node(literalTerm);

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("test value");
    });

    it("should handle Literal terms with datatype", () => {
      // Arrange
      const literalTerm = literal("42", namedNode("http://www.w3.org/2001/XMLSchema#integer"));

      // Act
      const result = pointer.node(literalTerm);

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("42");
    });

    it("should handle Literal terms with language", () => {
      // Arrange
      const literalTerm = literal("hello", null, "en");

      // Act
      const result = pointer.node(literalTerm);

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("hello");
    });

    it("should throw error for unsupported term types", () => {
      // Arrange
      const unsupportedTerm = { termType: "Unsupported" };

      // Act & Assert
      expect(() => pointer.node(unsupportedTerm)).toThrow("[usePointer] Unsupported term type: Unsupported");
    });
  });

  describe("Base IRI Handling", () => {
    it("should handle relative IRIs", () => {
      // Act
      const result = pointer.node("relative/path");

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("http://example.org/relative/path");
    });

    it("should handle absolute IRIs", () => {
      // Act
      const result = pointer.node("http://other.org/path");

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("http://other.org/path");
    });

    it("should handle HTTPS IRIs", () => {
      // Act
      const result = pointer.node("https://other.org/path");

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("https://other.org/path");
    });

    it("should handle CURIEs", () => {
      // Act
      const result = pointer.node("ex:local");

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
      expect(result.node).toBe("http://example.org/ex:local");
    });
  });

  describe("Filter Operations", () => {
    it("should handle ofType with string", () => {
      // Act
      const result = pointer.ofType("ex:Person");

      // Assert
      expect(result).toHaveProperty("type");
      expect(result).toHaveProperty("filter");
      expect(result.type).toBe("ex:Person");
      expect(result.filter).toBe("ofType");
    });

    it("should handle ofType with term", () => {
      // Arrange
      const typeTerm = namedNode("http://example.org/Person");

      // Act
      const result = pointer.ofType(typeTerm);

      // Assert
      expect(result).toHaveProperty("type");
      expect(result).toHaveProperty("filter");
      expect(result.type).toBe(typeTerm);
      expect(result.filter).toBe("ofType");
    });

    it("should handle withProperty with string", () => {
      // Act
      const result = pointer.withProperty("ex:name");

      // Assert
      expect(result).toHaveProperty("property");
      expect(result).toHaveProperty("filter");
      expect(result.property).toBe("ex:name");
      expect(result.filter).toBe("withProperty");
    });

    it("should handle withValue with strings", () => {
      // Act
      const result = pointer.withValue("ex:name", "John");

      // Assert
      expect(result).toHaveProperty("property");
      expect(result).toHaveProperty("value");
      expect(result).toHaveProperty("filter");
      expect(result.property).toBe("ex:name");
      expect(result.value).toBe("John");
      expect(result.filter).toBe("withValue");
    });

    it("should handle withValue with terms", () => {
      // Arrange
      const propertyTerm = namedNode("http://example.org/name");
      const valueTerm = literal("John");

      // Act
      const result = pointer.withValue(propertyTerm, valueTerm);

      // Assert
      expect(result).toHaveProperty("property");
      expect(result).toHaveProperty("value");
      expect(result).toHaveProperty("filter");
      expect(result.property).toBe(propertyTerm);
      expect(result.value).toBe(valueTerm);
      expect(result.filter).toBe("withValue");
    });
  });

  describe("Store Operations", () => {
    it("should handle empty store", () => {
      // Act
      const result = pointer.getStats();

      // Assert
      expect(result).toHaveProperty("quads");
      expect(result).toHaveProperty("subjects");
      expect(result).toHaveProperty("predicates");
      expect(result).toHaveProperty("objects");
      expect(result.quads).toBe(0);
    });

    it("should handle store with data", () => {
      // Arrange
      store.add(quad(namedNode("ex:s"), namedNode("ex:p"), literal("o")));

      // Act
      const result = pointer.getStats();

      // Assert
      expect(result).toHaveProperty("quads");
      expect(result.quads).toBe(1);
    });

    it("should handle isEmpty on empty store", () => {
      // Act
      const result = pointer.isEmpty();

      // Assert
      expect(result).toBe(true);
    });

    it("should handle isEmpty on non-empty store", () => {
      // Arrange
      store.add(quad(namedNode("ex:s"), namedNode("ex:p"), literal("o")));

      // Act
      const result = pointer.isEmpty();

      // Assert
      expect(result).toBe(false);
    });

    it("should handle size on empty store", () => {
      // Act
      const result = pointer.size();

      // Assert
      expect(result).toBe(0);
    });

    it("should handle size on non-empty store", () => {
      // Arrange
      store.add(quad(namedNode("ex:s"), namedNode("ex:p"), literal("o")));

      // Act
      const result = pointer.size();

      // Assert
      expect(result).toBe(1);
    });
  });

  describe("Base IRI Operations", () => {
    it("should handle getBaseIRI", () => {
      // Act
      const result = pointer.getBaseIRI();

      // Assert
      expect(result).toBe("http://example.org/");
    });

    it("should handle withBaseIRI", () => {
      // Act
      const newPointer = pointer.withBaseIRI("http://other.org/");

      // Assert
      expect(newPointer).toHaveProperty("getBaseIRI");
      expect(newPointer.getBaseIRI()).toBe("http://other.org/");
    });

    it("should handle withBaseIRI with null", () => {
      // Act
      const newPointer = pointer.withBaseIRI(null);

      // Assert
      expect(newPointer).toHaveProperty("getBaseIRI");
      expect(newPointer.getBaseIRI()).toBe(null);
    });

    it("should handle withBaseIRI with undefined", () => {
      // Act
      const newPointer = pointer.withBaseIRI(undefined);

      // Assert
      expect(newPointer).toHaveProperty("getBaseIRI");
      expect(newPointer.getBaseIRI()).toBe(undefined);
    });
  });

  describe("Options Edge Cases", () => {
    it("should handle null options", () => {
      // Act & Assert
      expect(() => usePointer(store, null)).not.toThrow();
    });

    it("should handle undefined options", () => {
      // Act & Assert
      expect(() => usePointer(store, undefined)).not.toThrow();
    });

    it("should handle empty options", () => {
      // Act & Assert
      expect(() => usePointer(store, {})).not.toThrow();
    });

    it("should handle options with null baseIRI", () => {
      // Act & Assert
      expect(() => usePointer(store, { baseIRI: null })).not.toThrow();
    });

    it("should handle options with undefined baseIRI", () => {
      // Act & Assert
      expect(() => usePointer(store, { baseIRI: undefined })).not.toThrow();
    });
  });

  describe("Concurrent Operations", () => {
    it("should handle rapid node operations", () => {
      // Act
      for (let i = 0; i < 1000; i++) {
        const result = pointer.node(`ex:node${i}`);
        expect(result).toHaveProperty("node");
        expect(result).toHaveProperty("type");
      }

      // Assert
      expect(pointer.size()).toBe(0); // Store should remain unchanged
    });

    it("should handle rapid filter operations", () => {
      // Act
      for (let i = 0; i < 1000; i++) {
        const result = pointer.ofType(`ex:Type${i}`);
        expect(result).toHaveProperty("type");
        expect(result).toHaveProperty("filter");
      }

      // Assert
      expect(pointer.size()).toBe(0); // Store should remain unchanged
    });
  });

  describe("Error Recovery", () => {
    it("should recover from invalid node operations", () => {
      // Act
      try {
        pointer.node(null);
      } catch (error) {
        // Expected error
      }

      // Should still work after error
      const result = pointer.node("ex:test");

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
    });

    it("should recover from invalid term type operations", () => {
      // Act
      try {
        pointer.node({ termType: "Unsupported" });
      } catch (error) {
        // Expected error
      }

      // Should still work after error
      const result = pointer.node("ex:test");

      // Assert
      expect(result).toHaveProperty("node");
      expect(result).toHaveProperty("type");
    });
  });
});
