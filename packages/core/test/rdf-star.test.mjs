/**
 * @file RDF-star Integration Tests
 * @description Comprehensive tests for RDF-star (W3C RDF 1.2) support
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore } from '@unrdf/oxigraph';
import {
  RDFStarFactory,
  rdfStarFactory,
  RDFSTAR,
  isQuotedTriple,
  extractBaseTriple,
  QuotedTriple,
  createQuotedTriple,
  AnnotationBuilder,
  createAnnotationBuilder,
  createProvenance,
  createTemporal,
  createConfidence,
  mergeAnnotations,
  validateProvenance,
  validateTemporal,
  validateConfidence,
} from '../src/index.mjs';
import {
  SPARQLStarQueryBuilder,
  createSPARQLStarBuilder,
  executeSPARQLStar,
} from '@unrdf/oxigraph';

describe('RDF-star Factory', () => {
  let factory;

  beforeEach(() => {
    factory = new RDFStarFactory();
  });

  it('should create a quoted triple', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/knows');
    const object = factory.namedNode('http://example.org/Bob');

    const quoted = factory.quotedTriple(subject, predicate, object);

    expect(quoted).toBeDefined();
    expect(isQuotedTriple(quoted)).toBe(true);
  });

  it('should create terms using data factory', () => {
    const namedNode = factory.namedNode('http://example.org/test');
    expect(namedNode.termType).toBe('NamedNode');
    expect(namedNode.value).toBe('http://example.org/test');

    const literal = factory.literal('test value');
    expect(literal.termType).toBe('Literal');
    expect(literal.value).toBe('test value');

    const blank = factory.blankNode();
    expect(blank.termType).toBe('BlankNode');
  });

  it('should add provenance annotation to quoted triple', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/age');
    const object = factory.literal('30');

    const quoted = factory.quotedTriple(subject, predicate, object);
    const provQuads = factory.addProvenance(quoted, {
      source: 'http://example.org/dataset',
      creator: 'Alice',
      created: '2026-01-11T10:00:00Z',
    });

    expect(provQuads).toHaveLength(3);
    expect(provQuads[0].subject.termType).toBe('BlankNode');
  });

  it('should add temporal annotation to quoted triple', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/age');
    const object = factory.literal('30');

    const quoted = factory.quotedTriple(subject, predicate, object);
    const tempQuads = factory.addTemporal(quoted, {
      validFrom: '2026-01-01T00:00:00Z',
      validTo: '2026-12-31T23:59:59Z',
    });

    expect(tempQuads).toHaveLength(2);
    expect(tempQuads[0].predicate.value).toBe(RDFSTAR.validFrom);
  });

  it('should add confidence annotation to quoted triple', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/age');
    const object = factory.literal('30');

    const quoted = factory.quotedTriple(subject, predicate, object);
    const confQuads = factory.addConfidence(quoted, {
      confidence: 0.95,
      method: 'machine-learning',
    });

    expect(confQuads).toHaveLength(1);
    expect(confQuads[0].predicate.value).toBe(RDFSTAR.confidence);
    expect(confQuads[0].object.value).toBe('0.95');
  });

  it('should create annotated triple with all annotation types', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/age');
    const object = factory.literal('30');

    const result = factory.createAnnotatedTriple(subject, predicate, object, {
      provenance: {
        source: 'http://example.org/dataset',
        creator: 'Alice',
      },
      temporal: {
        validFrom: '2026-01-01T00:00:00Z',
      },
      confidence: {
        confidence: 0.95,
      },
    });

    expect(result.quotedTriple).toBeDefined();
    expect(result.baseQuad).toBeDefined();
    expect(result.annotationQuads.length).toBeGreaterThanOrEqual(3);
    expect(result.reificationQuads).toHaveLength(4);
    expect(result.allQuads.length).toBeGreaterThanOrEqual(8);
  });

  it('should extract base triple from quoted triple', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/knows');
    const object = factory.namedNode('http://example.org/Bob');

    const quoted = factory.quotedTriple(subject, predicate, object);
    const base = extractBaseTriple(quoted);

    expect(base.subject.value).toBe('http://example.org/Alice');
    expect(base.predicate.value).toBe('http://xmlns.com/foaf/0.1/knows');
    expect(base.object.value).toBe('http://example.org/Bob');
  });

  it('should use singleton factory', () => {
    const subject = rdfStarFactory.namedNode('http://example.org/test');
    expect(subject.termType).toBe('NamedNode');
  });
});

describe('QuotedTriple Class', () => {
  let factory;

  beforeEach(() => {
    factory = new RDFStarFactory();
  });

  it('should create quoted triple instance', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/knows');
    const object = factory.namedNode('http://example.org/Bob');

    const qt = new QuotedTriple(subject, predicate, object);

    expect(qt.subject).toBe(subject);
    expect(qt.predicate).toBe(predicate);
    expect(qt.object).toBe(object);
    expect(qt.getQuoted()).toBeDefined();
  });

  it('should chain annotation methods', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/age');
    const object = factory.literal('30');

    const qt = new QuotedTriple(subject, predicate, object)
      .addProvenance({ source: 'http://example.org/dataset' })
      .addConfidence({ confidence: 0.95 })
      .addTemporal({ validFrom: '2026-01-01T00:00:00Z' });

    const annotations = qt.getAnnotations();
    expect(annotations.provenance.source).toBe('http://example.org/dataset');
    expect(annotations.confidence.confidence).toBe(0.95);
    expect(annotations.temporal.validFrom).toBe('2026-01-01T00:00:00Z');
  });

  it('should convert to quads', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/age');
    const object = factory.literal('30');

    const qt = new QuotedTriple(subject, predicate, object)
      .addProvenance({ source: 'http://example.org/dataset' })
      .addConfidence({ confidence: 0.95 });

    const quads = qt.toQuads();
    expect(quads.length).toBeGreaterThanOrEqual(2);
  });

  it('should convert to JSON', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/age');
    const object = factory.literal('30');

    const qt = new QuotedTriple(subject, predicate, object)
      .addProvenance({ source: 'http://example.org/dataset' });

    const json = qt.toJSON();
    expect(json.subject.value).toBe('http://example.org/Alice');
    expect(json.annotations.provenance.source).toBe('http://example.org/dataset');
  });

  it('should create from JSON', () => {
    const json = {
      subject: { termType: 'NamedNode', value: 'http://example.org/Alice' },
      predicate: { termType: 'NamedNode', value: 'http://xmlns.com/foaf/0.1/age' },
      object: { termType: 'Literal', value: '30' },
      annotations: {
        provenance: { source: 'http://example.org/dataset' },
        confidence: { confidence: 0.95 },
      },
    };

    const qt = QuotedTriple.fromJSON(json, factory);
    expect(qt.subject.value).toBe('http://example.org/Alice');
    expect(qt.getProvenance().source).toBe('http://example.org/dataset');
    expect(qt.getConfidence().confidence).toBe(0.95);
  });

  it('should validate quoted triple', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/knows');
    const object = factory.namedNode('http://example.org/Bob');

    const qt = new QuotedTriple(subject, predicate, object);
    expect(() => qt.validate()).not.toThrow();
  });

  it('should use factory function', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/knows');
    const object = factory.namedNode('http://example.org/Bob');

    const qt = createQuotedTriple(subject, predicate, object);
    expect(qt).toBeInstanceOf(QuotedTriple);
  });
});

describe('Annotation Builder', () => {
  it('should build annotation with fluent API', () => {
    const annotation = new AnnotationBuilder()
      .source('http://example.org/dataset')
      .creator('Alice')
      .confidence(0.95)
      .validFrom('2026-01-01T00:00:00Z')
      .build();

    expect(annotation.provenance.source).toBe('http://example.org/dataset');
    expect(annotation.provenance.creator).toBe('Alice');
    expect(annotation.confidence.confidence).toBe(0.95);
    expect(annotation.temporal.validFrom).toBe('2026-01-01T00:00:00Z');
  });

  it('should add custom annotations', () => {
    const annotation = new AnnotationBuilder()
      .custom('customKey', 'customValue')
      .custom('anotherKey', 123)
      .build();

    expect(annotation.custom.customKey).toBe('customValue');
    expect(annotation.custom.anotherKey).toBe(123);
  });

  it('should create builder using factory function', () => {
    const builder = createAnnotationBuilder();
    expect(builder).toBeInstanceOf(AnnotationBuilder);
  });
});

describe('Annotation Helper Functions', () => {
  it('should create provenance annotation', () => {
    const prov = createProvenance({
      source: 'http://example.org/dataset',
      creator: 'Alice',
      created: '2026-01-11T10:00:00Z',
    });

    expect(prov.source).toBe('http://example.org/dataset');
    expect(prov.creator).toBe('Alice');
  });

  it('should create temporal annotation', () => {
    const temp = createTemporal({
      validFrom: '2026-01-01T00:00:00Z',
      validTo: '2026-12-31T23:59:59Z',
    });

    expect(temp.validFrom).toBe('2026-01-01T00:00:00Z');
    expect(temp.validTo).toBe('2026-12-31T23:59:59Z');
  });

  it('should create confidence annotation', () => {
    const conf = createConfidence(0.95, 'machine-learning');

    expect(conf.confidence).toBe(0.95);
    expect(conf.method).toBe('machine-learning');
  });

  it('should merge multiple annotations', () => {
    const ann1 = { provenance: { source: 'http://example.org/dataset1' } };
    const ann2 = { confidence: { confidence: 0.95 } };
    const ann3 = { temporal: { validFrom: '2026-01-01T00:00:00Z' } };

    const merged = mergeAnnotations(ann1, ann2, ann3);

    expect(merged.provenance.source).toBe('http://example.org/dataset1');
    expect(merged.confidence.confidence).toBe(0.95);
    expect(merged.temporal.validFrom).toBe('2026-01-01T00:00:00Z');
  });
});

describe('RDF-star Schema Validation', () => {
  it('should validate provenance', () => {
    const prov = {
      source: 'http://example.org/dataset',
      creator: 'Alice',
      created: '2026-01-11T10:00:00Z',
    };

    expect(() => validateProvenance(prov)).not.toThrow();
  });

  it('should validate temporal annotation', () => {
    const temp = {
      validFrom: '2026-01-01T00:00:00Z',
      validTo: '2026-12-31T23:59:59Z',
    };

    expect(() => validateTemporal(temp)).not.toThrow();
  });

  it('should validate confidence annotation', () => {
    const conf = {
      confidence: 0.95,
      method: 'machine-learning',
    };

    expect(() => validateConfidence(conf)).not.toThrow();
  });

  it('should reject invalid confidence values', () => {
    const invalidConf = {
      confidence: 1.5,
    };

    expect(() => validateConfidence(invalidConf)).toThrow();
  });

  it('should reject invalid confidence values (negative)', () => {
    const invalidConf = {
      confidence: -0.1,
    };

    expect(() => validateConfidence(invalidConf)).toThrow();
  });
});

describe('SPARQL-star Query Builder', () => {
  it('should build basic SPARQL-star query', () => {
    const query = new SPARQLStarQueryBuilder()
      .select(['?s', '?p', '?o', '?confidence'])
      .where('?s', '?p', '?o')
      .whereQuoted('?s', '?p', '?o')
      .whereAnnotation('?confidence', 'confidence')
      .filter('?confidence > 0.9')
      .build();

    expect(query).toContain('SELECT ?s ?p ?o ?confidence');
    expect(query).toContain('<<?s ?p ?o>>');
    expect(query).toContain('FILTER(?confidence > 0.9)');
  });

  it('should add confidence threshold filter', () => {
    const query = new SPARQLStarQueryBuilder()
      .select(['?s', '?p', '?o'])
      .where('?s', '?p', '?o')
      .confidenceThreshold(0.9)
      .build();

    expect(query).toContain('?_confidence >= 0.9');
  });

  it('should add temporal filter', () => {
    const query = new SPARQLStarQueryBuilder()
      .select(['?s', '?p', '?o'])
      .where('?s', '?p', '?o')
      .temporalFilter('2026-01-11T10:00:00Z')
      .build();

    expect(query).toContain('validFrom');
    expect(query).toContain('validTo');
  });

  it('should add ORDER BY clause', () => {
    const query = new SPARQLStarQueryBuilder()
      .select(['?s', '?p', '?o', '?confidence'])
      .where('?s', '?p', '?o')
      .orderBy('?confidence', 'DESC')
      .build();

    expect(query).toContain('ORDER BY DESC(?confidence)');
  });

  it('should add LIMIT and OFFSET', () => {
    const query = new SPARQLStarQueryBuilder()
      .select(['?s', '?p', '?o'])
      .where('?s', '?p', '?o')
      .limit(10)
      .offset(5)
      .build();

    expect(query).toContain('LIMIT 10');
    expect(query).toContain('OFFSET 5');
  });

  it('should create builder using factory function', () => {
    const builder = createSPARQLStarBuilder();
    expect(builder).toBeInstanceOf(SPARQLStarQueryBuilder);
  });
});

describe('RDF-star with Oxigraph Integration', () => {
  let store;
  let factory;

  beforeEach(() => {
    store = createStore();
    factory = new RDFStarFactory();
  });

  it('should store and query quoted triples', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/age');
    const object = factory.literal('30');

    const result = factory.createAnnotatedTriple(subject, predicate, object, {
      confidence: { confidence: 0.95 },
    });

    result.allQuads.forEach((quad) => store.add(quad));

    const quads = store.match();
    expect(quads.length).toBeGreaterThanOrEqual(6);
  });

  it('should execute SPARQL-star query on store', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/age');
    const object = factory.literal('30');

    const qt = new QuotedTriple(subject, predicate, object)
      .addConfidence({ confidence: 0.95 });

    const quads = qt.toQuads();
    quads.forEach((quad) => store.add(quad));

    const query = `
      SELECT ?s ?p ?o
      WHERE {
        ?s ?p ?o
      }
    `;

    const results = executeSPARQLStar(store, query);
    expect(results).toBeDefined();
  });

  it('should handle multiple annotated triples', () => {
    const triples = [
      { s: 'Alice', p: 'age', o: '30', conf: 0.95 },
      { s: 'Bob', p: 'age', o: '25', conf: 0.85 },
      { s: 'Charlie', p: 'age', o: '35', conf: 0.75 },
    ];

    triples.forEach((t) => {
      const subject = factory.namedNode(`http://example.org/${t.s}`);
      const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/age');
      const object = factory.literal(t.o);

      const result = factory.createAnnotatedTriple(subject, predicate, object, {
        confidence: { confidence: t.conf },
      });

      result.allQuads.forEach((quad) => store.add(quad));
    });

    const allQuads = store.match();
    expect(allQuads.length).toBeGreaterThanOrEqual(18);
  });
});

describe('Performance Tests', () => {
  let factory;
  let store;

  beforeEach(() => {
    factory = new RDFStarFactory();
    store = createStore();
  });

  it('should create quoted triple in <1ms', () => {
    const start = performance.now();

    for (let i = 0; i < 100; i++) {
      const subject = factory.namedNode(`http://example.org/subject${i}`);
      const predicate = factory.namedNode('http://example.org/predicate');
      const object = factory.literal(`value${i}`);
      factory.quotedTriple(subject, predicate, object);
    }

    const end = performance.now();
    const avgTime = (end - start) / 100;

    expect(avgTime).toBeLessThan(1);
  });

  it('should add annotation in <0.5ms', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/age');
    const object = factory.literal('30');
    const quoted = factory.quotedTriple(subject, predicate, object);

    const start = performance.now();

    for (let i = 0; i < 100; i++) {
      factory.addConfidence(quoted, { confidence: 0.95 });
    }

    const end = performance.now();
    const avgTime = (end - start) / 100;

    expect(avgTime).toBeLessThan(0.5);
  });

  it('should handle 1000 annotated triples efficiently', () => {
    const start = performance.now();

    for (let i = 0; i < 1000; i++) {
      const subject = factory.namedNode(`http://example.org/subject${i}`);
      const predicate = factory.namedNode('http://example.org/predicate');
      const object = factory.literal(`value${i}`);

      const result = factory.createAnnotatedTriple(subject, predicate, object, {
        confidence: { confidence: 0.95 },
        provenance: { source: 'http://example.org/dataset' },
      });

      result.allQuads.forEach((quad) => store.add(quad));
    }

    const end = performance.now();
    const totalTime = end - start;

    expect(totalTime).toBeLessThan(2000);
    expect(store.size).toBeGreaterThanOrEqual(6000);
  });
});

describe('Backward Compatibility', () => {
  let store;
  let factory;

  beforeEach(() => {
    store = createStore();
    factory = new RDFStarFactory();
  });

  it('should work with regular triples', () => {
    const subject = factory.namedNode('http://example.org/Alice');
    const predicate = factory.namedNode('http://xmlns.com/foaf/0.1/knows');
    const object = factory.namedNode('http://example.org/Bob');

    const triple = factory.triple(subject, predicate, object);
    store.add(triple);

    const quads = store.match();
    expect(quads).toHaveLength(1);
  });

  it('should mix regular and quoted triples', () => {
    const regularSubject = factory.namedNode('http://example.org/Alice');
    const regularPredicate = factory.namedNode('http://xmlns.com/foaf/0.1/knows');
    const regularObject = factory.namedNode('http://example.org/Bob');

    const regularTriple = factory.triple(regularSubject, regularPredicate, regularObject);
    store.add(regularTriple);

    const quotedSubject = factory.namedNode('http://example.org/Charlie');
    const quotedPredicate = factory.namedNode('http://xmlns.com/foaf/0.1/age');
    const quotedObject = factory.literal('30');

    const result = factory.createAnnotatedTriple(quotedSubject, quotedPredicate, quotedObject, {
      confidence: { confidence: 0.95 },
    });

    result.allQuads.forEach((quad) => store.add(quad));

    const allQuads = store.match();
    expect(allQuads.length).toBeGreaterThanOrEqual(7);
  });
});
