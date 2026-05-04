/**
 * RDF Validator Tests
 *
 * **80/20 Focus**: Tests that prove validation cannot fail:
 * - IRI validation works correctly
 * - Literal/datatype validation enforced
 * - Shape validation catches constraint violations
 * - Pre-insertion validation guards store
 *
 * @module rdf-validator.test
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  RDFValidator,
  NAMESPACES,
  createPreInsertionValidator,
} from '../src/rdf-validator.mjs';

describe('RDFValidator', () => {
  let validator;

  beforeEach(() => {
    validator = new RDFValidator();
  });

  describe('constructor', () => {
    it('should create validator instance', () => {
      expect(validator).toBeDefined();
      expect(validator.shapes).toBeDefined();
      expect(validator.errors).toEqual([]);
      expect(validator.warnings).toEqual([]);
    });

    it('should register built-in shapes', () => {
      const shapes = validator.getRegisteredShapes();
      expect(shapes).toContain('foaf:Person');
      expect(shapes).toContain('schema:Person');
      expect(shapes).toContain('schema:Thing');
      expect(shapes).toContain('schema:Organization');
      expect(shapes).toContain('rdfs:Class');
      expect(shapes).toContain('owl:Class');
    });

    it('should accept optional oxigraphBridge', () => {
      const mockBridge = { query: () => {} };
      const validatorWithBridge = new RDFValidator(mockBridge);
      expect(validatorWithBridge.bridge).toBe(mockBridge);
    });
  });

  describe('validateIRI', () => {
    it('should accept valid HTTP IRI', () => {
      const result = validator.validateIRI('http://example.org/resource');
      expect(result.valid).toBe(true);
      expect(result.error).toBeUndefined();
    });

    it('should accept valid HTTPS IRI', () => {
      const result = validator.validateIRI('https://schema.org/Person');
      expect(result.valid).toBe(true);
    });

    it('should accept valid URN IRI', () => {
      const result = validator.validateIRI('urn:isbn:0451450523');
      expect(result.valid).toBe(true);
    });

    it('should accept prefixed name and expand it', () => {
      const result = validator.validateIRI('foaf:name');
      expect(result.valid).toBe(true);
    });

    it('should reject empty string', () => {
      const result = validator.validateIRI('');
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('IRI_INVALID');
    });

    it('should reject null', () => {
      const result = validator.validateIRI(null);
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('IRI_INVALID');
    });

    it('should reject undefined', () => {
      const result = validator.validateIRI(undefined);
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('IRI_INVALID');
    });

    it('should reject malformed IRI (missing scheme)', () => {
      const result = validator.validateIRI('example.org/resource');
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('IRI_INVALID');
    });

    it('should reject IRI with spaces', () => {
      const result = validator.validateIRI('http://example.org/my resource');
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('IRI_INVALID');
    });

    it('should reject IRI with illegal characters', () => {
      const result = validator.validateIRI('http://example.org/<resource>');
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('IRI_INVALID');
    });
  });

  describe('validateLiteral', () => {
    it('should accept valid xsd:string', () => {
      const result = validator.validateLiteral('Hello World', 'xsd:string');
      expect(result.valid).toBe(true);
    });

    it('should accept valid xsd:integer', () => {
      const result = validator.validateLiteral('42', 'xsd:integer');
      expect(result.valid).toBe(true);
    });

    it('should accept negative xsd:integer', () => {
      const result = validator.validateLiteral('-123', 'xsd:integer');
      expect(result.valid).toBe(true);
    });

    it('should reject non-integer for xsd:integer', () => {
      const result = validator.validateLiteral('3.14', 'xsd:integer');
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('DATATYPE_MISMATCH');
    });

    it('should reject text for xsd:integer', () => {
      const result = validator.validateLiteral('forty-two', 'xsd:integer');
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('DATATYPE_MISMATCH');
    });

    it('should accept valid xsd:decimal', () => {
      const result = validator.validateLiteral('3.14159', 'xsd:decimal');
      expect(result.valid).toBe(true);
    });

    it('should accept valid xsd:boolean true', () => {
      const result = validator.validateLiteral('true', 'xsd:boolean');
      expect(result.valid).toBe(true);
    });

    it('should accept valid xsd:boolean false', () => {
      const result = validator.validateLiteral('false', 'xsd:boolean');
      expect(result.valid).toBe(true);
    });

    it('should accept 1 as xsd:boolean', () => {
      const result = validator.validateLiteral('1', 'xsd:boolean');
      expect(result.valid).toBe(true);
    });

    it('should reject invalid xsd:boolean', () => {
      const result = validator.validateLiteral('yes', 'xsd:boolean');
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('DATATYPE_MISMATCH');
    });

    it('should accept valid xsd:date', () => {
      const result = validator.validateLiteral('2024-01-15', 'xsd:date');
      expect(result.valid).toBe(true);
    });

    it('should reject invalid xsd:date', () => {
      const result = validator.validateLiteral('15-01-2024', 'xsd:date');
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('DATATYPE_MISMATCH');
    });

    it('should accept valid xsd:dateTime', () => {
      const result = validator.validateLiteral('2024-01-15T10:30:00Z', 'xsd:dateTime');
      expect(result.valid).toBe(true);
    });

    it('should accept valid xsd:nonNegativeInteger', () => {
      const result = validator.validateLiteral('0', 'xsd:nonNegativeInteger');
      expect(result.valid).toBe(true);
    });

    it('should reject negative for xsd:nonNegativeInteger', () => {
      const result = validator.validateLiteral('-1', 'xsd:nonNegativeInteger');
      expect(result.valid).toBe(false);
    });

    it('should accept valid xsd:positiveInteger', () => {
      const result = validator.validateLiteral('1', 'xsd:positiveInteger');
      expect(result.valid).toBe(true);
    });

    it('should reject zero for xsd:positiveInteger', () => {
      const result = validator.validateLiteral('0', 'xsd:positiveInteger');
      expect(result.valid).toBe(false);
    });

    it('should reject null value', () => {
      const result = validator.validateLiteral(null, 'xsd:string');
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('LITERAL_INVALID');
    });

    it('should warn on unknown datatype', () => {
      const result = validator.validateLiteral('custom', 'custom:datatype');
      expect(result.valid).toBe(true);
      expect(result.error?.type).toBe('DATATYPE_UNKNOWN');
      expect(result.error?.severity).toBe('warning');
    });

    it('should accept value without datatype', () => {
      const result = validator.validateLiteral('plain text', undefined);
      expect(result.valid).toBe(true);
    });

    it('should handle full IRI datatype', () => {
      const result = validator.validateLiteral('42', NAMESPACES.xsd + 'integer');
      expect(result.valid).toBe(true);
    });
  });

  describe('validateProperty', () => {
    const triples = [
      { subject: 'http://example.org/person1', predicate: 'foaf:name', value: 'Alice' },
      { subject: 'http://example.org/person1', predicate: 'foaf:age', value: '30' },
    ];

    it('should pass when required property present', () => {
      const result = validator.validateProperty(
        'http://example.org/person1',
        'foaf:name',
        triples
      );
      expect(result.valid).toBe(true);
    });

    it('should fail when required property missing', () => {
      const result = validator.validateProperty(
        'http://example.org/person1',
        'foaf:mbox',
        triples
      );
      expect(result.valid).toBe(false);
      expect(result.error?.type).toBe('PROPERTY_MISSING');
    });

    it('should handle prefixed subject', () => {
      validator.addPrefix('ex', 'http://example.org/');
      const triplesWithPrefix = [
        { subject: 'ex:person1', predicate: 'foaf:name', value: 'Bob' },
      ];
      const result = validator.validateProperty('ex:person1', 'foaf:name', triplesWithPrefix);
      expect(result.valid).toBe(true);
    });
  });

  describe('registerShape', () => {
    it('should register custom shape', () => {
      validator.registerShape('ex:CustomShape', [
        { property: 'ex:field', required: true },
      ]);
      expect(validator.getRegisteredShapes()).toContain('ex:CustomShape');
    });

    it('should throw on empty shape name', () => {
      expect(() => validator.registerShape('', [])).toThrow('Shape name must be');
    });

    it('should throw on non-array rules', () => {
      expect(() => validator.registerShape('ex:Shape', {})).toThrow('Rules must be an array');
    });

    it('should store shape definition correctly', () => {
      validator.registerShape('ex:TestShape', [
        { property: 'ex:prop', required: true, datatype: 'xsd:string' },
      ]);
      const shape = validator.getShape('ex:TestShape');
      expect(shape).toBeDefined();
      expect(shape.rules).toHaveLength(1);
      expect(shape.rules[0].required).toBe(true);
    });
  });

  describe('validateTriple', () => {
    it('should accept valid triple', () => {
      const result = validator.validateTriple({
        subject: 'http://example.org/s',
        predicate: 'http://example.org/p',
        value: 'object value',
      });
      expect(result.valid).toBe(true);
      expect(result.errors).toHaveLength(0);
    });

    it('should accept triple with blank node subject', () => {
      const result = validator.validateTriple({
        subject: '_:b1',
        predicate: 'http://example.org/p',
        value: 'object',
      });
      expect(result.valid).toBe(true);
    });

    it('should reject null triple', () => {
      const result = validator.validateTriple(null);
      expect(result.valid).toBe(false);
      expect(result.errors[0].type).toBe('TRIPLE_INVALID');
    });

    it('should reject triple without subject', () => {
      const result = validator.validateTriple({
        predicate: 'http://example.org/p',
        value: 'object',
      });
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'SUBJECT_MISSING')).toBe(true);
    });

    it('should reject triple without predicate', () => {
      const result = validator.validateTriple({
        subject: 'http://example.org/s',
        value: 'object',
      });
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'PREDICATE_MISSING')).toBe(true);
    });

    it('should reject triple without object', () => {
      const result = validator.validateTriple({
        subject: 'http://example.org/s',
        predicate: 'http://example.org/p',
      });
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'OBJECT_MISSING')).toBe(true);
    });

    it('should validate literal with datatype', () => {
      const result = validator.validateTriple({
        subject: 'http://example.org/s',
        predicate: 'http://example.org/p',
        value: 'not-an-integer',
        datatype: 'xsd:integer',
      });
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'DATATYPE_MISMATCH')).toBe(true);
    });

    it('should accept valid literal with datatype', () => {
      const result = validator.validateTriple({
        subject: 'http://example.org/s',
        predicate: 'http://example.org/p',
        value: '42',
        datatype: 'xsd:integer',
      });
      expect(result.valid).toBe(true);
    });

    it('should collect multiple errors', () => {
      const result = validator.validateTriple({});
      expect(result.valid).toBe(false);
      expect(result.errors.length).toBeGreaterThan(1);
    });
  });

  describe('validateAgainstShape', () => {
    it('should pass valid foaf:Person', () => {
      const triples = [
        { subject: 'http://example.org/alice', predicate: 'rdf:type', value: 'foaf:Person' },
        { subject: 'http://example.org/alice', predicate: 'foaf:name', value: 'Alice' },
      ];
      const result = validator.validateAgainstShape('foaf:Person', 'http://example.org/alice', triples);
      expect(result.valid).toBe(true);
    });

    it('should fail foaf:Person without name', () => {
      const triples = [
        { subject: 'http://example.org/alice', predicate: 'rdf:type', value: 'foaf:Person' },
        { subject: 'http://example.org/alice', predicate: 'foaf:age', value: '30' },
      ];
      const result = validator.validateAgainstShape('foaf:Person', 'http://example.org/alice', triples);
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'PROPERTY_MISSING')).toBe(true);
    });

    it('should validate age range', () => {
      const triples = [
        { subject: 'http://example.org/alice', predicate: 'foaf:name', value: 'Alice' },
        { subject: 'http://example.org/alice', predicate: 'foaf:age', value: '200', datatype: 'xsd:integer' },
      ];
      const result = validator.validateAgainstShape('foaf:Person', 'http://example.org/alice', triples);
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'MAX_VALUE_VIOLATION')).toBe(true);
    });

    it('should validate negative age', () => {
      const triples = [
        { subject: 'http://example.org/alice', predicate: 'foaf:name', value: 'Alice' },
        { subject: 'http://example.org/alice', predicate: 'foaf:age', value: '-5', datatype: 'xsd:integer' },
      ];
      const result = validator.validateAgainstShape('foaf:Person', 'http://example.org/alice', triples);
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'MIN_VALUE_VIOLATION')).toBe(true);
    });

    it('should validate nodeKind IRI', () => {
      const triples = [
        { subject: 'http://example.org/alice', predicate: 'foaf:name', value: 'Alice' },
        { subject: 'http://example.org/alice', predicate: 'foaf:mbox', value: 'not-an-iri' },
      ];
      const result = validator.validateAgainstShape('foaf:Person', 'http://example.org/alice', triples);
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'NODE_KIND_VIOLATION')).toBe(true);
    });

    it('should accept valid nodeKind IRI', () => {
      const triples = [
        { subject: 'http://example.org/alice', predicate: 'foaf:name', value: 'Alice' },
        { subject: 'http://example.org/alice', predicate: 'foaf:mbox', value: 'mailto:alice@example.org' },
      ];
      const result = validator.validateAgainstShape('foaf:Person', 'http://example.org/alice', triples);
      expect(result.valid).toBe(true);
    });

    it('should fail on unknown shape', () => {
      const result = validator.validateAgainstShape('unknown:Shape', 'http://example.org/x', []);
      expect(result.valid).toBe(false);
      expect(result.errors[0].type).toBe('SHAPE_NOT_FOUND');
    });
  });

  describe('validateGraph', () => {
    it('should validate all triples in graph', async () => {
      const triples = [
        { subject: 'http://example.org/s1', predicate: 'http://example.org/p', value: 'v1' },
        { subject: 'http://example.org/s2', predicate: 'http://example.org/p', value: 'v2' },
      ];
      const result = await validator.validateGraph(triples);
      expect(result.valid).toBe(true);
      expect(result.triplesValidated).toBe(2);
    });

    it('should collect errors from invalid triples', async () => {
      const triples = [
        { subject: 'http://example.org/s1', predicate: 'http://example.org/p', value: 'v1' },
        { subject: '', predicate: 'http://example.org/p', value: 'v2' },
      ];
      const result = await validator.validateGraph(triples);
      expect(result.valid).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
    });

    it('should validate against specified shapes', async () => {
      const triples = [
        { subject: 'http://example.org/alice', predicate: 'rdf:type', value: 'foaf:Person' },
        { subject: 'http://example.org/alice', predicate: 'foaf:name', value: 'Alice' },
      ];
      const result = await validator.validateGraph(triples, { shapes: ['foaf:Person'] });
      expect(result.valid).toBe(true);
    });

    it('should skip structure validation when disabled', async () => {
      const triples = [
        { subject: '', predicate: '', value: '' },
      ];
      const result = await validator.validateGraph(triples, { validateStructure: false });
      expect(result.triplesValidated).toBe(0);
    });

    it('should warn on unknown shape', async () => {
      const triples = [
        { subject: 'http://example.org/s', predicate: 'rdf:type', value: 'unknown:Type' },
      ];
      const result = await validator.validateGraph(triples, { shapes: ['unknown:Shape'] });
      expect(result.warnings.some((w) => w.type === 'SHAPE_NOT_FOUND')).toBe(true);
    });
  });

  describe('custom shape validation', () => {
    beforeEach(() => {
      validator.registerShape('ex:Product', [
        { property: 'ex:name', required: true, datatype: 'xsd:string', minLength: 1, maxLength: 100 },
        { property: 'ex:price', required: true, datatype: 'xsd:decimal', minValue: 0 },
        { property: 'ex:category', required: true, in: ['electronics', 'clothing', 'food'] },
        { property: 'ex:sku', required: true, pattern: /^[A-Z]{2}-\d{6}$/ },
        { property: 'ex:tags', required: false, minCount: 0, maxCount: 5 },
      ]);
    });

    it('should validate string length constraints', () => {
      const triples = [
        { subject: 'http://example.org/p1', predicate: 'ex:name', value: '' },
        { subject: 'http://example.org/p1', predicate: 'ex:price', value: '10.00' },
        { subject: 'http://example.org/p1', predicate: 'ex:category', value: 'electronics' },
        { subject: 'http://example.org/p1', predicate: 'ex:sku', value: 'AB-123456' },
      ];
      const result = validator.validateAgainstShape('ex:Product', 'http://example.org/p1', triples);
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'MIN_LENGTH_VIOLATION')).toBe(true);
    });

    it('should validate in constraint', () => {
      const triples = [
        { subject: 'http://example.org/p1', predicate: 'ex:name', value: 'Product' },
        { subject: 'http://example.org/p1', predicate: 'ex:price', value: '10.00' },
        { subject: 'http://example.org/p1', predicate: 'ex:category', value: 'invalid-category' },
        { subject: 'http://example.org/p1', predicate: 'ex:sku', value: 'AB-123456' },
      ];
      const result = validator.validateAgainstShape('ex:Product', 'http://example.org/p1', triples);
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'IN_VIOLATION')).toBe(true);
    });

    it('should validate pattern constraint', () => {
      const triples = [
        { subject: 'http://example.org/p1', predicate: 'ex:name', value: 'Product' },
        { subject: 'http://example.org/p1', predicate: 'ex:price', value: '10.00' },
        { subject: 'http://example.org/p1', predicate: 'ex:category', value: 'electronics' },
        { subject: 'http://example.org/p1', predicate: 'ex:sku', value: 'invalid-sku' },
      ];
      const result = validator.validateAgainstShape('ex:Product', 'http://example.org/p1', triples);
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'PATTERN_VIOLATION')).toBe(true);
    });

    it('should validate maxCount constraint', () => {
      const triples = [
        { subject: 'http://example.org/p1', predicate: 'ex:name', value: 'Product' },
        { subject: 'http://example.org/p1', predicate: 'ex:price', value: '10.00' },
        { subject: 'http://example.org/p1', predicate: 'ex:category', value: 'electronics' },
        { subject: 'http://example.org/p1', predicate: 'ex:sku', value: 'AB-123456' },
        { subject: 'http://example.org/p1', predicate: 'ex:tags', value: 'tag1' },
        { subject: 'http://example.org/p1', predicate: 'ex:tags', value: 'tag2' },
        { subject: 'http://example.org/p1', predicate: 'ex:tags', value: 'tag3' },
        { subject: 'http://example.org/p1', predicate: 'ex:tags', value: 'tag4' },
        { subject: 'http://example.org/p1', predicate: 'ex:tags', value: 'tag5' },
        { subject: 'http://example.org/p1', predicate: 'ex:tags', value: 'tag6' },
      ];
      const result = validator.validateAgainstShape('ex:Product', 'http://example.org/p1', triples);
      expect(result.valid).toBe(false);
      expect(result.errors.some((e) => e.type === 'MAX_COUNT_VIOLATION')).toBe(true);
    });

    it('should pass valid product', () => {
      const triples = [
        { subject: 'http://example.org/p1', predicate: 'ex:name', value: 'Widget' },
        { subject: 'http://example.org/p1', predicate: 'ex:price', value: '29.99' },
        { subject: 'http://example.org/p1', predicate: 'ex:category', value: 'electronics' },
        { subject: 'http://example.org/p1', predicate: 'ex:sku', value: 'WD-000001' },
      ];
      const result = validator.validateAgainstShape('ex:Product', 'http://example.org/p1', triples);
      expect(result.valid).toBe(true);
    });
  });

  describe('prefix management', () => {
    it('should expand known prefixes', () => {
      expect(validator.expandPrefix('foaf:name')).toBe('http://xmlns.com/foaf/0.1/name');
      expect(validator.expandPrefix('xsd:integer')).toBe('http://www.w3.org/2001/XMLSchema#integer');
    });

    it('should return full IRIs unchanged', () => {
      const iri = 'http://example.org/resource';
      expect(validator.expandPrefix(iri)).toBe(iri);
    });

    it('should add custom prefix', () => {
      validator.addPrefix('ex', 'http://example.org/');
      expect(validator.expandPrefix('ex:resource')).toBe('http://example.org/resource');
    });

    it('should throw on invalid prefix', () => {
      expect(() => validator.addPrefix('', 'http://example.org/')).toThrow();
      expect(() => validator.addPrefix('ex', '')).toThrow();
    });
  });

  describe('error management', () => {
    it('should accumulate errors', async () => {
      const invalidTriples = [
        { subject: '', predicate: 'http://ex.org/p', value: 'v' },
        { subject: 'http://ex.org/s', predicate: '', value: 'v' },
      ];
      await validator.validateGraph(invalidTriples);
      expect(validator.getValidationErrors().length).toBeGreaterThan(0);
    });

    it('should clear errors', async () => {
      const invalidTriples = [{ subject: '', predicate: '', value: '' }];
      await validator.validateGraph(invalidTriples);
      expect(validator.getValidationErrors().length).toBeGreaterThan(0);
      validator.clearErrors();
      expect(validator.getValidationErrors()).toHaveLength(0);
      expect(validator.getValidationWarnings()).toHaveLength(0);
    });
  });
});

describe('createPreInsertionValidator', () => {
  let validator;

  beforeEach(() => {
    validator = new RDFValidator();
  });

  it('should return validator function', () => {
    const preValidator = createPreInsertionValidator(validator);
    expect(typeof preValidator).toBe('function');
  });

  it('should pass valid triples', async () => {
    const preValidator = createPreInsertionValidator(validator);
    const triples = [
      { subject: 'http://example.org/s', predicate: 'http://example.org/p', value: 'v' },
    ];
    const result = await preValidator(triples);
    expect(result.valid).toBe(true);
    expect(result.triples).toBe(triples);
  });

  it('should throw on invalid triples by default', async () => {
    const preValidator = createPreInsertionValidator(validator);
    const triples = [{ subject: '', predicate: '', value: '' }];
    await expect(preValidator(triples)).rejects.toThrow('RDF validation failed');
  });

  it('should not throw when throwOnError is false', async () => {
    const preValidator = createPreInsertionValidator(validator, { throwOnError: false });
    const triples = [{ subject: '', predicate: '', value: '' }];
    const result = await preValidator(triples);
    expect(result.valid).toBe(false);
    expect(result.errors.length).toBeGreaterThan(0);
  });

  it('should validate against specified shapes', async () => {
    const preValidator = createPreInsertionValidator(validator, {
      shapes: ['foaf:Person'],
      throwOnError: false,
    });
    const triples = [
      { subject: 'http://example.org/alice', predicate: 'rdf:type', value: 'foaf:Person' },
    ];
    const result = await preValidator(triples);
    expect(result.valid).toBe(false); // Missing foaf:name
  });

  it('should truncate error messages when many errors', async () => {
    const preValidator = createPreInsertionValidator(validator);
    const triples = Array(10)
      .fill(null)
      .map((_, i) => ({ subject: '', predicate: '', value: `v${i}` }));
    await expect(preValidator(triples)).rejects.toThrow('and');
  });
});

describe('NAMESPACES', () => {
  it('should export common namespace prefixes', () => {
    expect(NAMESPACES.xsd).toBe('http://www.w3.org/2001/XMLSchema#');
    expect(NAMESPACES.rdf).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
    expect(NAMESPACES.rdfs).toBe('http://www.w3.org/2000/01/rdf-schema#');
    expect(NAMESPACES.foaf).toBe('http://xmlns.com/foaf/0.1/');
    expect(NAMESPACES.schema).toBe('http://schema.org/');
    expect(NAMESPACES.owl).toBe('http://www.w3.org/2002/07/owl#');
  });
});
