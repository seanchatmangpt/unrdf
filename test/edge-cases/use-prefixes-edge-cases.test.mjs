import { describe, expect, it, beforeEach } from "vitest";
import { usePrefixes } from "../../src/composables/use-prefixes.mjs";

describe("usePrefixes Edge Cases", () => {
  let prefixes;

  beforeEach(() => {
    prefixes = usePrefixes();
  });

  describe("Null and Undefined Inputs", () => {
    it("should handle null initial prefixes", () => {
      // Act & Assert
      expect(() => usePrefixes(null)).not.toThrow();
    });

    it("should handle undefined initial prefixes", () => {
      // Act & Assert
      expect(() => usePrefixes(undefined)).not.toThrow();
    });

    it("should throw error for null prefix map", () => {
      // Act & Assert
      expect(() => prefixes.register(null)).toThrow("[usePrefixes] Prefix map must be an object");
    });

    it("should throw error for undefined prefix map", () => {
      // Act & Assert
      expect(() => prefixes.register(undefined)).toThrow("[usePrefixes] Prefix map must be an object");
    });

    it("should throw error for null CURIE", () => {
      // Act & Assert
      expect(() => prefixes.expand(null)).toThrow("[usePrefixes] CURIE must be a string");
    });

    it("should throw error for undefined CURIE", () => {
      // Act & Assert
      expect(() => prefixes.expand(undefined)).toThrow("[usePrefixes] CURIE must be a string");
    });

    it("should throw error for null IRI", () => {
      // Act & Assert
      expect(() => prefixes.shrink(null)).toThrow("[usePrefixes] IRI must be a string");
    });

    it("should throw error for undefined IRI", () => {
      // Act & Assert
      expect(() => prefixes.shrink(undefined)).toThrow("[usePrefixes] IRI must be a string");
    });
  });

  describe("Invalid Data Types", () => {
    it("should throw error for non-object prefix map", () => {
      // Act & Assert
      expect(() => prefixes.register("string")).toThrow("[usePrefixes] Prefix map must be an object");
      expect(() => prefixes.register(123)).toThrow("[usePrefixes] Prefix map must be an object");
      expect(() => prefixes.register([])).toThrow("[usePrefixes] Prefix map must be an object");
      expect(() => prefixes.register(true)).toThrow("[usePrefixes] Prefix map must be an object");
    });

    it("should throw error for non-string CURIE", () => {
      // Act & Assert
      expect(() => prefixes.expand(123)).toThrow("[usePrefixes] CURIE must be a string");
      expect(() => prefixes.expand({})).toThrow("[usePrefixes] CURIE must be a string");
      expect(() => prefixes.expand([])).toThrow("[usePrefixes] CURIE must be a string");
    });

    it("should throw error for non-string IRI", () => {
      // Act & Assert
      expect(() => prefixes.shrink(123)).toThrow("[usePrefixes] IRI must be a string");
      expect(() => prefixes.shrink({})).toThrow("[usePrefixes] IRI must be a string");
      expect(() => prefixes.shrink([])).toThrow("[usePrefixes] IRI must be a string");
    });
  });

  describe("Empty and Whitespace Inputs", () => {
    it("should handle empty string CURIE", () => {
      // Act & Assert
      expect(() => prefixes.expand("")).toThrow();
    });

    it("should handle whitespace-only CURIE", () => {
      // Act & Assert
      expect(() => prefixes.expand("   ")).toThrow();
    });

    it("should handle empty string IRI", () => {
      // Act
      const result = prefixes.shrink("");

      // Assert
      expect(result).toBe("");
    });

    it("should handle whitespace-only IRI", () => {
      // Act
      const result = prefixes.shrink("   ");

      // Assert
      expect(result).toBe("   ");
    });

    it("should handle empty prefix map", () => {
      // Act & Assert
      expect(() => prefixes.register({})).not.toThrow();
    });
  });

  describe("Invalid Prefix and URI Formats", () => {
    it("should throw error for non-string prefix", () => {
      // Note: JavaScript automatically converts object keys to strings
      // So { 123: "value" } becomes { "123": "value" }
      // Only non-string values can be tested
      
      // Act & Assert
      expect(() => prefixes.register({ "validKey": 123 })).toThrow("[usePrefixes] Prefix and URI must be strings");
    });

    it("should throw error for non-string URI", () => {
      // Act & Assert
      expect(() => prefixes.register({ "ex": 123 })).toThrow("[usePrefixes] Prefix and URI must be strings");
    });

    it("should throw error for URI not ending with / or #", () => {
      // Act & Assert
      expect(() => prefixes.register({ "ex": "http://example.org" })).toThrow("[usePrefixes] URI should end with '/' or '#'");
    });

    it("should accept URI ending with /", () => {
      // Act & Assert
      expect(() => prefixes.register({ "ex": "http://example.org/" })).not.toThrow();
    });

    it("should accept URI ending with #", () => {
      // Act & Assert
      expect(() => prefixes.register({ "ex": "http://example.org#" })).not.toThrow();
    });
  });

  describe("Invalid CURIE Formats", () => {
    it("should throw error for CURIE starting with colon", () => {
      // Act & Assert
      expect(() => prefixes.expand(":localname")).toThrow("[usePrefixes] Invalid CURIE format");
    });

    it("should throw error for CURIE ending with colon", () => {
      // Act & Assert
      expect(() => prefixes.expand("prefix:")).toThrow("[usePrefixes] Invalid CURIE format");
    });

    it("should throw error for CURIE with only colon", () => {
      // Act & Assert
      expect(() => prefixes.expand(":")).toThrow("[usePrefixes] Invalid CURIE format");
    });

    it("should handle CURIE with multiple colons", () => {
      // Arrange
      prefixes.register({ "ex": "http://example.org/" });

      // Act
      const result = prefixes.expand("ex:has:colon");

      // Assert
      expect(result).toBe("http://example.org/has:colon");
    });
  });

  describe("Unknown Prefix Handling", () => {
    it("should throw error for unknown prefix in expand", () => {
      // Act & Assert
      expect(() => prefixes.expand("unknown:localname")).toThrow("[usePrefixes] Unknown prefix");
    });

    it("should return original IRI for unknown prefix in shrink", () => {
      // Act
      const result = prefixes.shrink("http://unknown.org/test");

      // Assert
      expect(result).toBe("http://unknown.org/test");
    });

    it("should handle empty prefix in expand", () => {
      // Act & Assert
      expect(() => prefixes.expand("localname")).toThrow("[usePrefixes] Unknown prefix");
    });
  });

  describe("Special Characters and Encoding", () => {
    it("should handle prefixes with special characters", () => {
      // Act & Assert
      expect(() => prefixes.register({ "ex-1": "http://example.org/" })).not.toThrow();
      expect(() => prefixes.register({ "ex_2": "http://example.org/" })).not.toThrow();
    });

    it("should handle URIs with special characters", () => {
      // Act & Assert
      expect(() => prefixes.register({ "ex": "http://example.org/path%20with%20spaces/" })).not.toThrow();
    });

    it("should handle CURIEs with special characters", () => {
      // Arrange
      prefixes.register({ "ex": "http://example.org/" });

      // Act
      const result = prefixes.expand("ex:name%20with%20spaces");

      // Assert
      expect(result).toBe("http://example.org/name%20with%20spaces");
    });
  });

  describe("Boundary Values", () => {
    it("should handle very long prefixes", () => {
      // Arrange
      const longPrefix = "a".repeat(1000);

      // Act & Assert
      expect(() => prefixes.register({ [longPrefix]: "http://example.org/" })).not.toThrow();
    });

    it("should handle very long URIs", () => {
      // Arrange
      const longURI = "http://example.org/" + "a".repeat(1000) + "/";

      // Act & Assert
      expect(() => prefixes.register({ "ex": longURI })).not.toThrow();
    });

    it("should handle very long local names", () => {
      // Arrange
      prefixes.register({ "ex": "http://example.org/" });
      const longLocalName = "a".repeat(1000);

      // Act
      const result = prefixes.expand(`ex:${longLocalName}`);

      // Assert
      expect(result).toBe(`http://example.org/${longLocalName}`);
    });

    it("should handle very long IRIs", () => {
      // Arrange
      const longIRI = "http://example.org/" + "a".repeat(1000);

      // Act
      const result = prefixes.shrink(longIRI);

      // Assert
      expect(result).toBe(longIRI);
    });
  });

  describe("Multiple Prefix Handling", () => {
    it("should handle overlapping URIs in shrink", () => {
      // Arrange
      prefixes.register({
        "ex1": "http://example.org/",
        "ex2": "http://example.org/sub/"
      });

      // Act
      const result1 = prefixes.shrink("http://example.org/test");
      const result2 = prefixes.shrink("http://example.org/sub/test");

      // Assert
      expect(result1).toBe("ex1:test");
      expect(result2).toBe("ex2:test");
    });

    it("should handle exact URI matches", () => {
      // Arrange
      prefixes.register({ "ex": "http://example.org/" });

      // Act
      const result = prefixes.shrink("http://example.org/");

      // Assert
      expect(result).toBe("ex:");
    });

    it("should handle partial URI matches", () => {
      // Arrange
      prefixes.register({ "ex": "http://example.org/prefix/" });

      // Act
      const result = prefixes.shrink("http://example.org/pre");

      // Assert
      expect(result).toBe("http://example.org/pre");
    });
  });

  describe("Prefix Management Operations", () => {
    it("should handle has() with null prefix", () => {
      // Act
      const result = prefixes.has(null);

      // Assert
      expect(result).toBe(false);
    });

    it("should handle has() with undefined prefix", () => {
      // Act
      const result = prefixes.has(undefined);

      // Assert
      expect(result).toBe(false);
    });

    it("should handle get() with null prefix", () => {
      // Act
      const result = prefixes.get(null);

      // Assert
      expect(result).toBeNull();
    });

    it("should handle get() with undefined prefix", () => {
      // Act
      const result = prefixes.get(undefined);

      // Assert
      expect(result).toBeNull();
    });

    it("should handle remove() with null prefix", () => {
      // Act
      const result = prefixes.remove(null);

      // Assert
      expect(result).toBe(false);
    });

    it("should handle remove() with undefined prefix", () => {
      // Act
      const result = prefixes.remove(undefined);

      // Assert
      expect(result).toBe(false);
    });

    it("should handle clear() on empty prefixes", () => {
      // Act & Assert
      expect(() => prefixes.clear()).not.toThrow();
      expect(prefixes.size()).toBe(0);
    });
  });

  describe("Concurrent Operations", () => {
    it("should handle rapid register/remove operations", () => {
      // Act & Assert
      for (let i = 0; i < 1000; i++) {
        prefixes.register({ [`prefix${i}`]: `http://example${i}.org/` });
        prefixes.remove(`prefix${i}`);
      }
      expect(prefixes.size()).toBe(0);
    });

    it("should handle rapid expand/shrink operations", () => {
      // Arrange
      prefixes.register({ "ex": "http://example.org/" });

      // Act & Assert
      for (let i = 0; i < 1000; i++) {
        const expanded = prefixes.expand(`ex:item${i}`);
        const shrunk = prefixes.shrink(expanded);
        expect(shrunk).toBe(`ex:item${i}`);
      }
    });
  });

  describe("Error Recovery", () => {
    it("should recover from invalid register operations", () => {
      // Arrange
      prefixes.register({ "valid": "http://valid.org/" });

      // Act
      try {
        prefixes.register({ "invalid": "invalid-uri" });
      } catch {
        // Expected error
      }

      // Assert
      expect(prefixes.has("valid")).toBe(true);
      expect(prefixes.has("invalid")).toBe(false);
    });

    it("should maintain consistency after partial failures", () => {
      // Arrange
      prefixes.register({ "ex1": "http://example1.org/" });

      // Act
      try {
        prefixes.register({
          "ex2": "http://example2.org/",
          "invalid": "invalid-uri"
        });
      } catch {
        // Expected error for invalid URI
      }

      // Assert - should not have registered any from the failed batch
      expect(prefixes.size()).toBe(1);
      expect(prefixes.has("ex1")).toBe(true);
      expect(prefixes.has("ex2")).toBe(false);
    });
  });
});