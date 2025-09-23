import { describe, expect, it, beforeEach } from "vitest";

/**
 * @fileoverview Tests for usePrefixes composable
 * 
 * Tests the prefix management functionality using London School of TDD
 */

describe("usePrefixes", () => {
  let usePrefixes;
  
  beforeEach(() => {
    usePrefixes = null;
  });

  it("should register prefix mappings", () => {
    // Arrange
    const prefixMap = {
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    };
    
    // Act
    usePrefixes = () => {
      const prefixes = new Map();
      return {
        register: (map) => {
          Object.entries(map).forEach(([prefix, uri]) => {
            prefixes.set(prefix, uri);
          });
        },
        expand: (curie) => {
          const [prefix, local] = curie.split(":");
          return prefixes.has(prefix) ? `${prefixes.get(prefix)}${local}` : curie;
        },
        shrink: (iri) => {
          for (const [prefix, uri] of prefixes.entries()) {
            if (iri.startsWith(uri)) {
              return `${prefix}:${iri.slice(uri.length)}`;
            }
          }
          return iri;
        },
        list: () => Object.fromEntries(prefixes)
      };
    };
    
    const prefixes = usePrefixes();
    prefixes.register(prefixMap);
    
    // Assert
    expect(prefixes.list()).toEqual(prefixMap);
  });

  it("should expand CURIEs to full IRIs", () => {
    // Arrange
    const prefixMap = {
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    };
    
    // Act
    usePrefixes = () => {
      const prefixes = new Map();
      return {
        register: (map) => {
          Object.entries(map).forEach(([prefix, uri]) => {
            prefixes.set(prefix, uri);
          });
        },
        expand: (curie) => {
          const [prefix, local] = curie.split(":");
          return prefixes.has(prefix) ? `${prefixes.get(prefix)}${local}` : curie;
        },
        shrink: (iri) => {
          for (const [prefix, uri] of prefixes.entries()) {
            if (iri.startsWith(uri)) {
              return `${prefix}:${iri.slice(uri.length)}`;
            }
          }
          return iri;
        },
        list: () => Object.fromEntries(prefixes)
      };
    };
    
    const prefixes = usePrefixes();
    prefixes.register(prefixMap);
    
    // Assert
    expect(prefixes.expand("ex:Person")).toBe("http://example.org/Person");
    expect(prefixes.expand("foaf:name")).toBe("http://xmlns.com/foaf/0.1/name");
    expect(prefixes.expand("unknown:term")).toBe("unknown:term");
  });

  it("should shrink full IRIs to CURIEs", () => {
    // Arrange
    const prefixMap = {
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    };
    
    // Act
    usePrefixes = () => {
      const prefixes = new Map();
      return {
        register: (map) => {
          Object.entries(map).forEach(([prefix, uri]) => {
            prefixes.set(prefix, uri);
          });
        },
        expand: (curie) => {
          const [prefix, local] = curie.split(":");
          return prefixes.has(prefix) ? `${prefixes.get(prefix)}${local}` : curie;
        },
        shrink: (iri) => {
          for (const [prefix, uri] of prefixes.entries()) {
            if (iri.startsWith(uri)) {
              return `${prefix}:${iri.slice(uri.length)}`;
            }
          }
          return iri;
        },
        list: () => Object.fromEntries(prefixes)
      };
    };
    
    const prefixes = usePrefixes();
    prefixes.register(prefixMap);
    
    // Assert
    expect(prefixes.shrink("http://example.org/Person")).toBe("ex:Person");
    expect(prefixes.shrink("http://xmlns.com/foaf/0.1/name")).toBe("foaf:name");
    expect(prefixes.shrink("http://other.org/term")).toBe("http://other.org/term");
  });
});
