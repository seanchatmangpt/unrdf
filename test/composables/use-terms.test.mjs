/**
 * @fileoverview Tests for useTerms composable
 * Tests RDF term creation and manipulation utilities
 */

import { describe, it, expect, beforeEach } from "vitest";
import { useTerms } from "../../src/composables/use-terms.mjs";
import { initStore } from "../../src/context/index.mjs";

describe("useTerms composable", () => {
  let runApp;

  beforeEach(() => {
    runApp = initStore();
  });

  it("provides terms creation interface", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      expect(terms).toBeDefined();
      expect(typeof terms.iri).toBe("function");
      expect(typeof terms.lit).toBe("function");
      expect(typeof terms.bnode).toBe("function");
      expect(typeof terms.quad).toBe("function");
      expect(typeof terms.graph).toBe("function");
    });
  });

  it("creates named nodes (IRIs) correctly", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const iri = terms.iri("https://example.org/person");
      
      expect(iri.termType).toBe("NamedNode");
      expect(iri.value).toBe("https://example.org/person");
    });
  });

  it("handles relative IRIs with base IRI", async () => {
    await runApp(async () => {
      const terms = useTerms({ baseIRI: "https://example.org/" });
      
      const iri = terms.iri("person");
      
      expect(iri.value).toBe("https://example.org/person");
    });
  });

  it("creates literals correctly", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const literal = terms.lit("John Doe");
      
      expect(literal.termType).toBe("Literal");
      expect(literal.value).toBe("John Doe");
    });
  });

  it("creates typed literals", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const typedLiteral = terms.lit("30", "http://www.w3.org/2001/XMLSchema#integer");
      
      expect(typedLiteral.termType).toBe("Literal");
      expect(typedLiteral.value).toBe("30");
      expect(typedLiteral.datatype.value).toBe("http://www.w3.org/2001/XMLSchema#integer");
    });
  });

  it("creates language-tagged literals", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const langLiteral = terms.lit("Hello", null, "en");
      
      expect(langLiteral.termType).toBe("Literal");
      expect(langLiteral.value).toBe("Hello");
      expect(langLiteral.language).toBe("en");
    });
  });

  it("creates blank nodes", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const bnode = terms.bnode("person1");
      
      expect(bnode.termType).toBe("BlankNode");
      expect(bnode.value).toBe("person1");
    });
  });

  it("creates quads", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const subject = terms.iri("https://example.org/person");
      const predicate = terms.iri("https://example.org/name");
      const object = terms.lit("John Doe");
      const quad = terms.quad(subject, predicate, object);
      
      expect(quad.termType).toBe("Quad");
      expect(quad.subject).toBe(subject);
      expect(quad.predicate).toBe(predicate);
      expect(quad.object).toBe(object);
    });
  });

  it("creates quads with graph", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const subject = terms.iri("https://example.org/person");
      const predicate = terms.iri("https://example.org/name");
      const object = terms.lit("John Doe");
      const graph = terms.graph("https://example.org/graph");
      const quad = terms.quad(subject, predicate, object, graph);
      
      expect(quad.graph).toBe(graph);
    });
  });

  it("handles different data types", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const stringLit = terms.lit("hello");
      const numberLit = terms.lit(42);
      const boolLit = terms.lit(true);
      
      expect(stringLit.value).toBe("hello");
      expect(numberLit.value).toBe("42");
      expect(boolLit.value).toBe("true");
    });
  });

  it("validates input parameters", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      expect(() => {
        terms.iri(123);
      }).toThrow(TypeError);
      
      expect(() => {
        terms.lit(null);
      }).toThrow();
      
      expect(() => {
        terms.lit(undefined);
      }).toThrow();
    });
  });

  it("handles complex term combinations", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const person = terms.iri("https://example.org/person");
      const name = terms.iri("https://example.org/name");
      const age = terms.iri("https://example.org/age");
      const address = terms.iri("https://example.org/address");
      
      const nameLiteral = terms.lit("John Doe");
      const ageLiteral = terms.lit("30", "http://www.w3.org/2001/XMLSchema#integer");
      const addressBnode = terms.bnode("addr1");
      
      const nameQuad = terms.quad(person, name, nameLiteral);
      const ageQuad = terms.quad(person, age, ageLiteral);
      const addressQuad = terms.quad(person, address, addressBnode);
      
      expect(nameQuad.subject).toBe(person);
      expect(ageQuad.object).toBe(ageLiteral);
      expect(addressQuad.object).toBe(addressBnode);
    });
  });

  it("uses default datatype when specified", async () => {
    await runApp(async () => {
      const terms = useTerms({ 
        defaultDatatype: "http://www.w3.org/2001/XMLSchema#string" 
      });
      
      const literal = terms.lit("test");
      
      expect(literal.datatype.value).toBe("http://www.w3.org/2001/XMLSchema#string");
    });
  });

  it("handles empty string literals", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const emptyLiteral = terms.lit("");
      
      expect(emptyLiteral.value).toBe("");
      expect(emptyLiteral.termType).toBe("Literal");
    });
  });

  it("creates terms with special characters", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const iri = terms.iri("https://example.org/person#with-hash");
      const literal = terms.lit("Special chars: !@#$%^&*()");
      
      expect(iri.value).toBe("https://example.org/person#with-hash");
      expect(literal.value).toBe("Special chars: !@#$%^&*()");
    });
  });

  it("handles Unicode characters", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const unicodeLiteral = terms.lit("Hello 世界", null, "zh");
      
      expect(unicodeLiteral.value).toBe("Hello 世界");
      expect(unicodeLiteral.language).toBe("zh");
    });
  });

  it("maintains term identity", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const iri1 = terms.iri("https://example.org/person");
      const iri2 = terms.iri("https://example.org/person");
      
      expect(iri1).not.toBe(iri2); // Different objects
      expect(iri1.value).toBe(iri2.value); // But equal values
    });
  });

  it("handles edge cases gracefully", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      // Empty string IRI - should be treated as relative and get base IRI
      const emptyIri = terms.iri("");
      expect(emptyIri.value).toBe("http://example.org/");
      
      // Very long literal
      const longLiteral = terms.lit("a".repeat(1000));
      expect(longLiteral.value).toHaveLength(1000);
    });
  });

  it("handles null and undefined inputs gracefully", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      expect(terms.getTermType(null)).toBe(null);
      expect(terms.getTermType(undefined)).toBe(null);
      expect(terms.isTerm(null)).toBe(false);
      expect(terms.isTerm(undefined)).toBe(false);
    });
  });

  it("creates default graph terms", async () => {
    await runApp(async () => {
      const terms = useTerms();
      
      const defaultGraph = terms.defaultGraph();
      
      expect(defaultGraph.termType).toBe("DefaultGraph");
      expect(terms.isDefaultGraph(defaultGraph)).toBe(true);
    });
  });
});
