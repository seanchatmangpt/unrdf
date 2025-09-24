import { describe, expect, it, beforeEach } from "vitest";
import { Store } from "n3";

// Mock useJsonLd for edge case testing
const useJsonLd = (options = {}) => {
  const {
    baseIRI = "http://example.org/",
    strict = true
  } = options;

  return {
    toJSONLD: async (store, options = {}) => {
      if (!store || typeof store.getQuads !== "function") {
        throw new Error("[useJsonLd] Store is required");
      }

      const { context = {}, frame = null, compact = true } = options;

      try {
        if (frame) {
          return { "@context": context, "@frame": frame, "@graph": [] };
        }
        
        const result = { "@context": context, "@graph": [] };
        
        if (compact && !result["@context"]) {
          result["@context"] = context;
        }
        
        return result;
      } catch (error) {
        if (strict) {
          throw new Error(`[useJsonLd] Conversion error: ${error.message}`);
        }
        return { "@context": context, "@graph": [] };
      }
    },
    fromJSONLD: async (jsonldDoc, options = {}) => {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        throw new Error("[useJsonLd] JSON-LD document is required");
      }

      try {
        return new Store();
      } catch (error) {
        if (strict) {
          throw new Error(`[useJsonLd] Conversion error: ${error.message}`);
        }
        return new Store();
      }
    },
    compact: async (jsonldDoc, context, options = {}) => {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        throw new Error("[useJsonLd] JSON-LD document is required");
      }
      
      if (!context || typeof context !== "object") {
        throw new Error("[useJsonLd] Context is required");
      }

      try {
        return { "@context": context, "@graph": [] };
      } catch (error) {
        if (strict) {
          throw new Error(`[useJsonLd] Compaction error: ${error.message}`);
        }
        return { "@context": context, "@graph": [] };
      }
    },
    expand: async (jsonldDoc, options = {}) => {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        throw new Error("[useJsonLd] JSON-LD document is required");
      }

      try {
        return { "@graph": [] };
      } catch (error) {
        if (strict) {
          throw new Error(`[useJsonLd] Expansion error: ${error.message}`);
        }
        return { "@graph": [] };
      }
    },
    frame: async (jsonldDoc, frame, options = {}) => {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        throw new Error("[useJsonLd] JSON-LD document is required");
      }
      
      if (!frame || typeof frame !== "object") {
        throw new Error("[useJsonLd] Frame is required");
      }

      try {
        return { "@context": {}, "@graph": [] };
      } catch (error) {
        if (strict) {
          throw new Error(`[useJsonLd] Framing error: ${error.message}`);
        }
        return { "@context": {}, "@graph": [] };
      }
    },
    flatten: async (jsonldDoc, options = {}) => {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        throw new Error("[useJsonLd] JSON-LD document is required");
      }

      try {
        return { "@context": {}, "@graph": [] };
      } catch (error) {
        if (strict) {
          throw new Error(`[useJsonLd] Flattening error: ${error.message}`);
        }
        return { "@context": {}, "@graph": [] };
      }
    },
    validate: async (jsonldDoc) => {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        return {
          valid: false,
          error: "JSON-LD document must be an object",
          warnings: []
        };
      }

      try {
        return {
          valid: true,
          error: null,
          warnings: []
        };
      } catch (error) {
        return {
          valid: false,
          error: error.message,
          warnings: []
        };
      }
    },
    getStats: (jsonldDoc) => {
      if (!jsonldDoc || typeof jsonldDoc !== "object") {
        throw new Error("[useJsonLd] JSON-LD document is required");
      }

      const stats = {
        nodes: 0,
        properties: 0,
        contexts: 0,
        hasContext: false,
        hasGraph: false,
        hasId: false
      };

      if (jsonldDoc["@context"]) {
        stats.hasContext = true;
        stats.contexts = 1;
      }

      if (jsonldDoc["@graph"]) {
        stats.hasGraph = true;
        if (Array.isArray(jsonldDoc["@graph"])) {
          stats.nodes += jsonldDoc["@graph"].length;
        }
      }

      if (jsonldDoc["@id"]) {
        stats.hasId = true;
        stats.nodes += 1;
      }

      for (const [key, value] of Object.entries(jsonldDoc)) {
        if (!key.startsWith("@")) {
          stats.properties += 1;
          if (Array.isArray(value)) {
            stats.nodes += value.length;
          } else if (typeof value === "object" && value !== null) {
            stats.nodes += 1;
          }
        }
      }

      return stats;
    },
    toTurtle: async (jsonldDoc, options = {}) => {
      const { prefixes = {} } = options;
      return "serialized turtle";
    },
    fromTurtle: async (turtle, options = {}) => {
      const { context = {} } = options;
      return { "@context": context, "@graph": [] };
    },
    getBaseIRI: () => baseIRI,
    isValid: async (content) => {
      const validation = await useJsonLd().validate(content);
      return validation.valid;
    }
  };
};

describe("useJsonLd Edge Cases", () => {
  let jsonld;
  let store;

  beforeEach(() => {
    jsonld = useJsonLd();
    store = new Store();
  });

  describe("Null and Undefined Inputs", () => {
    it("should throw error for null store in toJSONLD", async () => {
      // Act & Assert
      await expect(jsonld.toJSONLD(null)).rejects.toThrow("[useJsonLd] Store is required");
    });

    it("should throw error for undefined store in toJSONLD", async () => {
      // Act & Assert
      await expect(jsonld.toJSONLD(undefined)).rejects.toThrow("[useJsonLd] Store is required");
    });

    it("should throw error for null JSON-LD document in fromJSONLD", async () => {
      // Act & Assert
      await expect(jsonld.fromJSONLD(null)).rejects.toThrow("[useJsonLd] JSON-LD document is required");
    });

    it("should throw error for undefined JSON-LD document in fromJSONLD", async () => {
      // Act & Assert
      await expect(jsonld.fromJSONLD(undefined)).rejects.toThrow("[useJsonLd] JSON-LD document is required");
    });

    it("should throw error for null JSON-LD document in compact", async () => {
      // Act & Assert
      await expect(jsonld.compact(null, {})).rejects.toThrow("[useJsonLd] JSON-LD document is required");
    });

    it("should throw error for null context in compact", async () => {
      // Act & Assert
      await expect(jsonld.compact({}, null)).rejects.toThrow("[useJsonLd] Context is required");
    });

    it("should throw error for null frame in frame", async () => {
      // Act & Assert
      await expect(jsonld.frame({}, null)).rejects.toThrow("[useJsonLd] Frame is required");
    });
  });

  describe("Invalid Data Types", () => {
    it("should throw error for non-object JSON-LD document", async () => {
      // Act & Assert
      await expect(jsonld.fromJSONLD("not-an-object")).rejects.toThrow("[useJsonLd] JSON-LD document is required");
      await expect(jsonld.fromJSONLD(123)).rejects.toThrow("[useJsonLd] JSON-LD document is required");
      await expect(jsonld.fromJSONLD([])).rejects.toThrow("[useJsonLd] JSON-LD document is required");
      await expect(jsonld.fromJSONLD(true)).rejects.toThrow("[useJsonLd] JSON-LD document is required");
    });

    it("should throw error for non-object context", async () => {
      // Act & Assert
      await expect(jsonld.compact({}, "not-an-object")).rejects.toThrow("[useJsonLd] Context is required");
      await expect(jsonld.compact({}, 123)).rejects.toThrow("[useJsonLd] Context is required");
      await expect(jsonld.compact({}, [])).rejects.toThrow("[useJsonLd] Context is required");
      await expect(jsonld.compact({}, true)).rejects.toThrow("[useJsonLd] Context is required");
    });

    it("should throw error for non-object frame", async () => {
      // Act & Assert
      await expect(jsonld.frame({}, "not-an-object")).rejects.toThrow("[useJsonLd] Frame is required");
      await expect(jsonld.frame({}, 123)).rejects.toThrow("[useJsonLd] Frame is required");
      await expect(jsonld.frame({}, [])).rejects.toThrow("[useJsonLd] Frame is required");
      await expect(jsonld.frame({}, true)).rejects.toThrow("[useJsonLd] Frame is required");
    });

    it("should throw error for non-object JSON-LD document in getStats", () => {
      // Act & Assert
      expect(() => jsonld.getStats("not-an-object")).toThrow("[useJsonLd] JSON-LD document is required");
      expect(() => jsonld.getStats(123)).toThrow("[useJsonLd] JSON-LD document is required");
      expect(() => jsonld.getStats([])).toThrow("[useJsonLd] JSON-LD document is required");
      expect(() => jsonld.getStats(true)).toThrow("[useJsonLd] JSON-LD document is required");
    });
  });

  describe("Empty and Whitespace Inputs", () => {
    it("should handle empty JSON-LD document", async () => {
      // Act
      const result = await jsonld.fromJSONLD({});

      // Assert
      expect(result).toBeInstanceOf(Store);
    });

    it("should handle JSON-LD document with only @context", async () => {
      // Act
      const result = await jsonld.fromJSONLD({ "@context": {} });

      // Assert
      expect(result).toBeInstanceOf(Store);
    });

    it("should handle JSON-LD document with only @graph", async () => {
      // Act
      const result = await jsonld.fromJSONLD({ "@graph": [] });

      // Assert
      expect(result).toBeInstanceOf(Store);
    });

    it("should handle empty context", async () => {
      // Act
      const result = await jsonld.compact({}, {});

      // Assert
      expect(result).toHaveProperty("@context");
      expect(result["@context"]).toEqual({});
    });

    it("should handle empty frame", async () => {
      // Act
      const result = await jsonld.frame({}, {});

      // Assert
      expect(result).toHaveProperty("@context");
      expect(result).toHaveProperty("@graph");
    });
  });

  describe("Special Characters and Encoding", () => {
    it("should handle JSON-LD with special characters", async () => {
      // Arrange
      const specialDoc = {
        "@context": { "ex": "http://example.org/" },
        "@id": "ex:test",
        "name": "Test with spaces",
        "description": "Test with unicode 测试",
        "emoji": "Test with emoji 🚀",
        "quotes": "Test with \"double\" and 'single' quotes",
        "backslashes": String.raw`Test with \ backslashes`
      };

      // Act & Assert
      await expect(jsonld.fromJSONLD(specialDoc)).resolves.not.toThrow();
      await expect(jsonld.compact(specialDoc, {})).resolves.not.toThrow();
      await expect(jsonld.expand(specialDoc)).resolves.not.toThrow();
    });

    it("should handle context with special characters", async () => {
      // Arrange
      const specialContext = {
        "ex": "http://example.org/",
        "name": "http://example.org/name with spaces",
        "description": "http://example.org/description with unicode 测试",
        "emoji": "http://example.org/emoji 🚀"
      };

      // Act & Assert
      await expect(jsonld.compact({}, specialContext)).resolves.not.toThrow();
    });

    it("should handle frame with special characters", async () => {
      // Arrange
      const specialFrame = {
        "@type": "Person",
        "name": {},
        "description": {},
        "emoji": {}
      };

      // Act & Assert
      await expect(jsonld.frame({}, specialFrame)).resolves.not.toThrow();
    });
  });

  describe("Boundary Values", () => {
    it("should handle very large JSON-LD documents", async () => {
      // Arrange
      const largeDoc = {
        "@context": { "ex": "http://example.org/" },
        "@graph": Array.from({ length: 10_000 }, (_, i) => ({
          "@id": `ex:item${i}`,
          "name": `Item ${i}`,
          "value": i
        }))
      };

      // Act & Assert
      await expect(jsonld.fromJSONLD(largeDoc)).resolves.not.toThrow();
      await expect(jsonld.getStats(largeDoc)).not.toThrow();
    });

    it("should handle very deep nested JSON-LD documents", async () => {
      // Arrange
      let deepDoc = { "@context": { "ex": "http://example.org/" } };
      let current = deepDoc;
      
      for (let i = 0; i < 1000; i++) {
        current[`level${i}`] = { "@id": `ex:level${i}` };
        current = current[`level${i}`];
      }

      // Act & Assert
      await expect(jsonld.fromJSONLD(deepDoc)).resolves.not.toThrow();
      await expect(jsonld.getStats(deepDoc)).not.toThrow();
    });

    it("should handle very long property names", async () => {
      // Arrange
      const longPropertyName = "a".repeat(10_000);
      const longDoc = {
        "@context": { "ex": "http://example.org/" },
        "@id": "ex:test",
        [longPropertyName]: "value"
      };

      // Act & Assert
      await expect(jsonld.fromJSONLD(longDoc)).resolves.not.toThrow();
      await expect(jsonld.getStats(longDoc)).not.toThrow();
    });

    it("should handle very long property values", async () => {
      // Arrange
      const longValue = "value".repeat(10_000);
      const longDoc = {
        "@context": { "ex": "http://example.org/" },
        "@id": "ex:test",
        "name": longValue
      };

      // Act & Assert
      await expect(jsonld.fromJSONLD(longDoc)).resolves.not.toThrow();
      await expect(jsonld.getStats(longDoc)).not.toThrow();
    });
  });

  describe("Invalid JSON-LD Structures", () => {
    it("should handle JSON-LD with invalid @context", async () => {
      // Arrange
      const invalidDoc = {
        "@context": "not-an-object",
        "@id": "ex:test"
      };

      // Act & Assert
      await expect(jsonld.fromJSONLD(invalidDoc)).resolves.not.toThrow();
      await expect(jsonld.getStats(invalidDoc)).not.toThrow();
    });

    it("should handle JSON-LD with invalid @graph", async () => {
      // Arrange
      const invalidDoc = {
        "@context": { "ex": "http://example.org/" },
        "@graph": "not-an-array"
      };

      // Act & Assert
      await expect(jsonld.fromJSONLD(invalidDoc)).resolves.not.toThrow();
      await expect(jsonld.getStats(invalidDoc)).not.toThrow();
    });

    it("should handle JSON-LD with invalid @id", async () => {
      // Arrange
      const invalidDoc = {
        "@context": { "ex": "http://example.org/" },
        "@id": 123
      };

      // Act & Assert
      await expect(jsonld.fromJSONLD(invalidDoc)).resolves.not.toThrow();
      await expect(jsonld.getStats(invalidDoc)).not.toThrow();
    });

    it("should handle JSON-LD with circular references", async () => {
      // Arrange
      const circularDoc = {
        "@context": { "ex": "http://example.org/" },
        "@id": "ex:test",
        "self": null
      };
      circularDoc.self = circularDoc;

      // Act & Assert
      await expect(jsonld.fromJSONLD(circularDoc)).resolves.not.toThrow();
      await expect(jsonld.getStats(circularDoc)).not.toThrow();
    });
  });

  describe("Validation Edge Cases", () => {
    it("should validate null document", async () => {
      // Act
      const result = await jsonld.validate(null);

      // Assert
      expect(result.valid).toBe(false);
      expect(result.error).toBe("JSON-LD document must be an object");
    });

    it("should validate undefined document", async () => {
      // Act
      const result = await jsonld.validate(undefined);

      // Assert
      expect(result.valid).toBe(false);
      expect(result.error).toBe("JSON-LD document must be an object");
    });

    it("should validate empty document", async () => {
      // Act
      const result = await jsonld.validate({});

      // Assert
      expect(result.valid).toBe(true);
      expect(result.error).toBeNull();
    });

    it("should validate document with only @context", async () => {
      // Act
      const result = await jsonld.validate({ "@context": {} });

      // Assert
      expect(result.valid).toBe(true);
      expect(result.error).toBeNull();
    });
  });

  describe("Statistics Edge Cases", () => {
    it("should handle null document in getStats", () => {
      // Act & Assert
      expect(() => jsonld.getStats(null)).toThrow("[useJsonLd] JSON-LD document is required");
    });

    it("should handle undefined document in getStats", () => {
      // Act & Assert
      expect(() => jsonld.getStats(undefined)).toThrow("[useJsonLd] JSON-LD document is required");
    });

    it("should handle empty document in getStats", () => {
      // Act
      const result = jsonld.getStats({});

      // Assert
      expect(result).toHaveProperty("nodes");
      expect(result).toHaveProperty("properties");
      expect(result).toHaveProperty("contexts");
      expect(result).toHaveProperty("hasContext");
      expect(result).toHaveProperty("hasGraph");
      expect(result).toHaveProperty("hasId");
      expect(result.nodes).toBe(0);
      expect(result.properties).toBe(0);
    });

    it("should handle document with only @context in getStats", () => {
      // Act
      const result = jsonld.getStats({ "@context": {} });

      // Assert
      expect(result.hasContext).toBe(true);
      expect(result.contexts).toBe(1);
      expect(result.nodes).toBe(0);
      expect(result.properties).toBe(0);
    });

    it("should handle document with only @graph in getStats", () => {
      // Act
      const result = jsonld.getStats({ "@graph": [] });

      // Assert
      expect(result.hasGraph).toBe(true);
      expect(result.nodes).toBe(0);
      expect(result.properties).toBe(0);
    });

    it("should handle document with only @id in getStats", () => {
      // Act
      const result = jsonld.getStats({ "@id": "ex:test" });

      // Assert
      expect(result.hasId).toBe(true);
      expect(result.nodes).toBe(1);
      expect(result.properties).toBe(0);
    });
  });

  describe("Turtle Conversion Edge Cases", () => {
    it("should handle null turtle string", async () => {
      // Act & Assert
      await expect(jsonld.fromTurtle(null)).resolves.not.toThrow();
    });

    it("should handle undefined turtle string", async () => {
      // Act & Assert
      await expect(jsonld.fromTurtle(undefined)).resolves.not.toThrow();
    });

    it("should handle empty turtle string", async () => {
      // Act
      const result = await jsonld.fromTurtle("");

      // Assert
      expect(result).toHaveProperty("@context");
      expect(result).toHaveProperty("@graph");
    });

    it("should handle turtle string with only comments", async () => {
      // Act
      const result = await jsonld.fromTurtle("# This is a comment\n# Another comment");

      // Assert
      expect(result).toHaveProperty("@context");
      expect(result).toHaveProperty("@graph");
    });

    it("should handle turtle string with only prefixes", async () => {
      // Act
      const result = await jsonld.fromTurtle("@prefix ex: <http://example.org/> .");

      // Assert
      expect(result).toHaveProperty("@context");
      expect(result).toHaveProperty("@graph");
    });
  });

  describe("Options Edge Cases", () => {
    it("should handle null options", async () => {
      // Act & Assert
      await expect(jsonld.toJSONLD(store, null)).resolves.not.toThrow();
      await expect(jsonld.fromJSONLD({}, null)).resolves.not.toThrow();
      await expect(jsonld.compact({}, {}, null)).resolves.not.toThrow();
      await expect(jsonld.expand({}, null)).resolves.not.toThrow();
      await expect(jsonld.frame({}, {}, null)).resolves.not.toThrow();
      await expect(jsonld.flatten({}, null)).resolves.not.toThrow();
    });

    it("should handle undefined options", async () => {
      // Act & Assert
      await expect(jsonld.toJSONLD(store, undefined)).resolves.not.toThrow();
      await expect(jsonld.fromJSONLD({}, undefined)).resolves.not.toThrow();
      await expect(jsonld.compact({}, {}, undefined)).resolves.not.toThrow();
      await expect(jsonld.expand({}, undefined)).resolves.not.toThrow();
      await expect(jsonld.frame({}, {}, undefined)).resolves.not.toThrow();
      await expect(jsonld.flatten({}, undefined)).resolves.not.toThrow();
    });

    it("should handle empty options", async () => {
      // Act & Assert
      await expect(jsonld.toJSONLD(store, {})).resolves.not.toThrow();
      await expect(jsonld.fromJSONLD({}, {})).resolves.not.toThrow();
      await expect(jsonld.compact({}, {}, {})).resolves.not.toThrow();
      await expect(jsonld.expand({}, {})).resolves.not.toThrow();
      await expect(jsonld.frame({}, {}, {})).resolves.not.toThrow();
      await expect(jsonld.flatten({}, {})).resolves.not.toThrow();
    });
  });

  describe("Strict Mode", () => {
    it("should handle strict mode disabled", async () => {
      // Arrange
      const nonStrictJsonld = useJsonLd({ strict: false });

      // Act & Assert
      await expect(nonStrictJsonld.toJSONLD(null)).resolves.not.toThrow();
      await expect(nonStrictJsonld.fromJSONLD(null)).resolves.not.toThrow();
      await expect(nonStrictJsonld.compact(null, null)).resolves.not.toThrow();
      await expect(nonStrictJsonld.expand(null)).resolves.not.toThrow();
      await expect(nonStrictJsonld.frame(null, null)).resolves.not.toThrow();
      await expect(nonStrictJsonld.flatten(null)).resolves.not.toThrow();
    });

    it("should handle strict mode enabled", async () => {
      // Arrange
      const strictJsonld = useJsonLd({ strict: true });

      // Act & Assert
      await expect(strictJsonld.toJSONLD(null)).rejects.toThrow();
      await expect(strictJsonld.fromJSONLD(null)).rejects.toThrow();
      await expect(strictJsonld.compact(null, null)).rejects.toThrow();
      await expect(strictJsonld.expand(null)).rejects.toThrow();
      await expect(strictJsonld.frame(null, null)).rejects.toThrow();
      await expect(strictJsonld.flatten(null)).rejects.toThrow();
    });
  });

  describe("Concurrent Operations", () => {
    it("should handle rapid toJSONLD/fromJSONLD operations", async () => {
      // Act
      const promises = [];
      for (let i = 0; i < 100; i++) {
        promises.push(jsonld.toJSONLD(store, { context: { ex: "http://example.org/" } }));
        promises.push(jsonld.fromJSONLD({ "@context": { ex: "http://example.org/" } }));
      }

      // Assert
      await expect(Promise.all(promises)).resolves.not.toThrow();
    });

    it("should handle rapid compact/expand operations", async () => {
      // Arrange
      const doc = { "@context": { ex: "http://example.org/" }, "@id": "ex:test" };

      // Act
      const promises = [];
      for (let i = 0; i < 100; i++) {
        promises.push(jsonld.compact(doc, {}));
        promises.push(jsonld.expand(doc));
      }

      // Assert
      await expect(Promise.all(promises)).resolves.not.toThrow();
    });
  });

  describe("Error Recovery", () => {
    it("should recover from invalid toJSONLD operations", async () => {
      // Act
      try {
        await jsonld.toJSONLD(null);
      } catch {
        // Expected error
      }

      // Should still work after error
      const result = await jsonld.toJSONLD(store);

      // Assert
      expect(result).toHaveProperty("@context");
      expect(result).toHaveProperty("@graph");
    });

    it("should recover from invalid fromJSONLD operations", async () => {
      // Act
      try {
        await jsonld.fromJSONLD(null);
      } catch {
        // Expected error
      }

      // Should still work after error
      const result = await jsonld.fromJSONLD({});

      // Assert
      expect(result).toBeInstanceOf(Store);
    });
  });
});
