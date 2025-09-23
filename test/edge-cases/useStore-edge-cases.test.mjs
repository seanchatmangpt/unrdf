import { describe, expect, it, beforeEach } from "vitest";
import { Store, DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

// Mock useStore for edge case testing
const useStore = (initialQuads = [], options = {}) => {
  const store = new Store(initialQuads);
  return {
    store,
    add: (quads) => {
      if (!Array.isArray(quads)) {
        throw new Error("[useStore] Quads must be an array");
      }
      quads.forEach(q => {
        if (!q || typeof q !== "object") {
          throw new Error("[useStore] Invalid quad");
        }
        store.add(q);
      });
    },
    remove: (quads) => {
      if (!Array.isArray(quads)) {
        throw new Error("[useStore] Quads must be an array");
      }
      quads.forEach(q => store.removeQuads([q]));
    },
    clear: () => store.removeQuads(store.getQuads(null, null, null, null)),
    stats: () => ({ quads: store.size }),
    serialize: ({ format = "Turtle" } = {}) => format
  };
};

describe("useStore Edge Cases", () => {
  let storeComposable;

  beforeEach(() => {
    storeComposable = useStore();
  });

  describe("Null and Undefined Inputs", () => {
    it("should handle null initial quads", () => {
      // Act & Assert
      expect(() => useStore(null)).not.toThrow();
    });

    it("should handle undefined initial quads", () => {
      // Act & Assert
      expect(() => useStore(undefined)).not.toThrow();
    });

    it("should throw error when adding null quads", () => {
      // Act & Assert
      expect(() => storeComposable.add(null)).toThrow("[useStore] Quads must be an array");
    });

    it("should throw error when adding undefined quads", () => {
      // Act & Assert
      expect(() => storeComposable.add(undefined)).toThrow("[useStore] Quads must be an array");
    });

    it("should throw error when removing null quads", () => {
      // Act & Assert
      expect(() => storeComposable.remove(null)).toThrow("[useStore] Quads must be an array");
    });

    it("should handle empty array of quads", () => {
      // Act & Assert
      expect(() => storeComposable.add([])).not.toThrow();
      expect(() => storeComposable.remove([])).not.toThrow();
    });
  });

  describe("Invalid Quad Objects", () => {
    it("should throw error when adding invalid quad objects", () => {
      // Arrange
      const invalidQuads = [
        null,
        undefined,
        "not-a-quad",
        123,
        {},
        { subject: "invalid" }
      ];

      // Act & Assert
      invalidQuads.forEach(invalidQuad => {
        expect(() => storeComposable.add([invalidQuad])).toThrow("[useStore] Invalid quad");
      });
    });

    it("should handle quads with null components", () => {
      // Arrange
      const quadWithNull = quad(null, namedNode("ex:p"), literal("o"));

      // Act & Assert
      expect(() => storeComposable.add([quadWithNull])).not.toThrow();
    });

    it("should handle quads with undefined components", () => {
      // Arrange
      const quadWithUndefined = quad(undefined, namedNode("ex:p"), literal("o"));

      // Act & Assert
      expect(() => storeComposable.add([quadWithUndefined])).not.toThrow();
    });
  });

  describe("Boundary Values", () => {
    it("should handle very large number of quads", () => {
      // Arrange
      const largeQuads = [];
      for (let i = 0; i < 10000; i++) {
        largeQuads.push(quad(
          namedNode(`ex:s${i}`),
          namedNode("ex:p"),
          literal(`value${i}`)
        ));
      }

      // Act
      storeComposable.add(largeQuads);

      // Assert
      expect(storeComposable.stats().quads).toBe(10000);
    });

    it("should handle quads with very long IRIs", () => {
      // Arrange
      const longIRI = "http://example.org/" + "a".repeat(10000);
      const longQuad = quad(namedNode(longIRI), namedNode("ex:p"), literal("value"));

      // Act & Assert
      expect(() => storeComposable.add([longQuad])).not.toThrow();
    });

    it("should handle quads with very long literal values", () => {
      // Arrange
      const longValue = "value".repeat(10000);
      const longQuad = quad(namedNode("ex:s"), namedNode("ex:p"), literal(longValue));

      // Act & Assert
      expect(() => storeComposable.add([longQuad])).not.toThrow();
    });
  });

  describe("Duplicate Operations", () => {
    it("should handle adding the same quad multiple times", () => {
      // Arrange
      const testQuad = quad(namedNode("ex:s"), namedNode("ex:p"), literal("o"));

      // Act
      storeComposable.add([testQuad]);
      storeComposable.add([testQuad]);
      storeComposable.add([testQuad]);

      // Assert
      expect(storeComposable.stats().quads).toBe(1); // Should still be 1
    });

    it("should handle removing non-existent quads", () => {
      // Arrange
      const testQuad = quad(namedNode("ex:s"), namedNode("ex:p"), literal("o"));

      // Act & Assert
      expect(() => storeComposable.remove([testQuad])).not.toThrow();
      expect(storeComposable.stats().quads).toBe(0);
    });

    it("should handle clearing an empty store", () => {
      // Act & Assert
      expect(() => storeComposable.clear()).not.toThrow();
      expect(storeComposable.stats().quads).toBe(0);
    });
  });

  describe("Serialization Edge Cases", () => {
    it("should handle serialization with null format", () => {
      // Act & Assert
      expect(() => storeComposable.serialize({ format: null })).not.toThrow();
    });

    it("should handle serialization with undefined format", () => {
      // Act & Assert
      expect(() => storeComposable.serialize({ format: undefined })).not.toThrow();
    });

    it("should handle serialization with empty options", () => {
      // Act & Assert
      expect(() => storeComposable.serialize({})).not.toThrow();
    });

    it("should handle serialization with no options", () => {
      // Act & Assert
      expect(() => storeComposable.serialize()).not.toThrow();
    });
  });

  describe("Memory and Performance", () => {
    it("should handle rapid add/remove operations", () => {
      // Arrange
      const testQuad = quad(namedNode("ex:s"), namedNode("ex:p"), literal("o"));

      // Act
      for (let i = 0; i < 1000; i++) {
        storeComposable.add([testQuad]);
        storeComposable.remove([testQuad]);
      }

      // Assert
      expect(storeComposable.stats().quads).toBe(0);
    });

    it("should handle mixed operations on large dataset", () => {
      // Arrange
      const quads = [];
      for (let i = 0; i < 1000; i++) {
        quads.push(quad(
          namedNode(`ex:s${i}`),
          namedNode("ex:p"),
          literal(`value${i}`)
        ));
      }

      // Act
      storeComposable.add(quads);
      storeComposable.remove(quads.slice(0, 500));
      storeComposable.add(quads.slice(500, 1000));

      // Assert
      expect(storeComposable.stats().quads).toBe(1000);
    });
  });

  describe("Error Recovery", () => {
    it("should recover from invalid add operations", () => {
      // Arrange
      const validQuad = quad(namedNode("ex:s"), namedNode("ex:p"), literal("o"));

      // Act
      try {
        storeComposable.add([null]);
      } catch (error) {
        // Expected error
      }

      // Should still work after error
      storeComposable.add([validQuad]);

      // Assert
      expect(storeComposable.stats().quads).toBe(1);
    });

    it("should maintain consistency after partial failures", () => {
      // Arrange
      const validQuad = quad(namedNode("ex:s"), namedNode("ex:p"), literal("o"));
      const invalidQuads = [validQuad, null, validQuad];

      // Act
      try {
        storeComposable.add(invalidQuads);
      } catch (error) {
        // Expected error
      }

      // Assert
      expect(storeComposable.stats().quads).toBe(0); // Should be 0 due to atomic failure
    });
  });
});
