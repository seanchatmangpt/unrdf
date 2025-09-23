/**
 * @fileoverview Tests for usePrefixes composable
 */

import { describe, it, expect } from "vitest";
import { usePrefixes } from "../../src/composables/usePrefixes.mjs";

describe("usePrefixes", () => {
  it("should create a prefixes composable", () => {
    const prefixes = usePrefixes();
    expect(prefixes).toBeDefined();
    expect(typeof prefixes.register).toBe("function");
    expect(typeof prefixes.expand).toBe("function");
    expect(typeof prefixes.shrink).toBe("function");
    expect(typeof prefixes.list).toBe("function");
  });

  it("should register prefixes", () => {
    const prefixes = usePrefixes();
    const result = prefixes.register({
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    });
    
    expect(result).toBe(prefixes);
    expect(prefixes.has("ex")).toBe(true);
    expect(prefixes.has("foaf")).toBe(true);
  });

  it("should expand CURIEs", () => {
    const prefixes = usePrefixes({
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    });
    
    expect(prefixes.expand("ex:person")).toBe("http://example.org/person");
    expect(prefixes.expand("foaf:name")).toBe("http://xmlns.com/foaf/0.1/name");
  });

  it("should shrink IRIs to CURIEs", () => {
    const prefixes = usePrefixes({
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    });
    
    expect(prefixes.shrink("http://example.org/person")).toBe("ex:person");
    expect(prefixes.shrink("http://xmlns.com/foaf/0.1/name")).toBe("foaf:name");
  });

  it("should return original IRI if no prefix matches", () => {
    const prefixes = usePrefixes({
      "ex": "http://example.org/"
    });
    
    expect(prefixes.shrink("http://other.org/person")).toBe("http://other.org/person");
  });

  it("should return original string if not a CURIE", () => {
    const prefixes = usePrefixes({
      "ex": "http://example.org/"
    });
    
    expect(prefixes.expand("not-a-curie")).toBe("not-a-curie");
  });

  it("should list all prefixes", () => {
    const prefixes = usePrefixes({
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    });
    
    const list = prefixes.list();
    expect(list).toEqual({
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    });
  });

  it("should check if prefix exists", () => {
    const prefixes = usePrefixes({
      "ex": "http://example.org/"
    });
    
    expect(prefixes.has("ex")).toBe(true);
    expect(prefixes.has("foaf")).toBe(false);
  });

  it("should get prefix URI", () => {
    const prefixes = usePrefixes({
      "ex": "http://example.org/"
    });
    
    expect(prefixes.get("ex")).toBe("http://example.org/");
    expect(prefixes.get("foaf")).toBeUndefined();
  });

  it("should remove prefix", () => {
    const prefixes = usePrefixes({
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    });
    
    expect(prefixes.remove("ex")).toBe(true);
    expect(prefixes.has("ex")).toBe(false);
    expect(prefixes.has("foaf")).toBe(true);
  });

  it("should clear all prefixes", () => {
    const prefixes = usePrefixes({
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    });
    
    const result = prefixes.clear();
    expect(result).toBe(prefixes);
    expect(prefixes.size()).toBe(0);
  });

  it("should get size", () => {
    const prefixes = usePrefixes({
      "ex": "http://example.org/",
      "foaf": "http://xmlns.com/foaf/0.1/"
    });
    
    expect(prefixes.size()).toBe(2);
  });

  it("should throw error for invalid prefix map", () => {
    const prefixes = usePrefixes();
    expect(() => prefixes.register(null)).toThrow("[usePrefixes] Prefix map must be an object");
  });

  it("should throw error for non-string prefix", () => {
    const prefixes = usePrefixes();
    expect(() => prefixes.register({ "ex": 123 })).toThrow("[usePrefixes] Prefix and URI must be strings");
  });

  it("should throw error for URI not ending with / or #", () => {
    const prefixes = usePrefixes();
    expect(() => prefixes.register({ "ex": "http://example.org" })).toThrow("[usePrefixes] URI should end with '/' or '#'");
  });

  it("should throw error for invalid CURIE format", () => {
    const prefixes = usePrefixes({
      "ex": "http://example.org/"
    });
    
    expect(() => prefixes.expand("ex:")).toThrow("[usePrefixes] Invalid CURIE format");
    expect(() => prefixes.expand(":person")).toThrow("[usePrefixes] Invalid CURIE format");
  });

  it("should throw error for unknown prefix", () => {
    const prefixes = usePrefixes();
    expect(() => prefixes.expand("unknown:person")).toThrow("[usePrefixes] Unknown prefix: unknown");
  });

  it("should throw error for invalid IRI in shrink", () => {
    const prefixes = usePrefixes();
    expect(() => prefixes.shrink(null)).toThrow("[usePrefixes] IRI must be a string");
  });
});