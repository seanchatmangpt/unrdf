/**
 * @file SHACL Validation Test Suite
 * @description Comprehensive tests for SHACL validator and schema builder
 */

import { describe, it, expect } from 'vitest';
import {
  createValidator,
  validateGraph,
  validateConstraint,
  generateReport,
  validateNodeKind,
  fastValidate,
  ConstraintType,
} from '../src/validation/shacl-validator.mjs';
import {
  shacl,
  CommonShapes,
  ShapeBuilder,
} from '../src/validation/rdf-schema-builder.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad, blankNode } = dataFactory;

describe('SHACL Validator', () => {
  describe('createValidator', () => {
    it('should create validator from Turtle string', async () => {
      // Arrange
      const shapes = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person .
      `;

      // Act
      const validator = await createValidator(shapes);

      // Assert
      expect(validator).toBeDefined();
      expect(typeof validator.validate).toBe('function');
    });

    it('should create validator from quad array', async () => {
      // Arrange
      const shapes = [
        quad(
          namedNode('http://example.org/PersonShape'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://www.w3.org/ns/shacl#NodeShape')
        ),
      ];

      // Act
      const validator = await createValidator(shapes);

      // Assert
      expect(validator).toBeDefined();
    });

    it('should create validator from Oxigraph store', async () => {
      // Arrange
      const store = createStore();
      store.add(quad(
        namedNode('http://example.org/PersonShape'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://www.w3.org/ns/shacl#NodeShape')
      ));

      // Act
      const validator = await createValidator(store);

      // Assert
      expect(validator).toBeDefined();
    });

    it('should throw error for invalid shapes format', async () => {
      // Arrange
      const invalidShapes = 123;

      // Act & Assert
      await expect(createValidator(invalidShapes)).rejects.toThrow('Shapes must be a Store, Turtle string, or Quad array');
    });
  });

  describe('validateGraph', () => {
    it('should validate conforming data', async () => {
      // Arrange
      const shapes = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person ;
          sh:property [
            sh:path ex:name ;
            sh:minCount 1 ;
            sh:datatype xsd:string ;
          ] .
      `;

      const data = [
        quad(
          namedNode('http://example.org/john'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://example.org/Person')
        ),
        quad(
          namedNode('http://example.org/john'),
          namedNode('http://example.org/name'),
          literal('John Doe')
        ),
      ];

      // Act
      const report = await validateGraph(data, shapes);

      // Assert
      expect(report.conforms).toBe(true);
      expect(report.results).toEqual([]);
    });

    it('should validate with Oxigraph store data', async () => {
      // Arrange
      const shapes = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:PersonShape a sh:NodeShape ;
          sh:targetClass ex:Person ;
          sh:property [
            sh:path ex:name ;
            sh:minCount 1 ;
          ] .
      `;

      const store = createStore();
      store.add(quad(
        namedNode('http://example.org/jane'),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/Person')
      ));
      store.add(quad(
        namedNode('http://example.org/jane'),
        namedNode('http://example.org/name'),
        literal('Jane Smith')
      ));

      // Act
      const report = await validateGraph(store, shapes);

      // Assert
      expect(report.conforms).toBe(true);
      expect(report.results).toEqual([]);
    });

    it('should respect maxErrors option', async () => {
      // Arrange
      const shapes = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
      `;
      const data = [];

      // Act
      const report = await validateGraph(data, shapes, { maxErrors: 5 });

      // Assert
      expect(report.results.length).toBeLessThanOrEqual(5);
    });
  });

  describe('validateConstraint', () => {
    it('should validate MIN_COUNT constraint', () => {
      // Arrange
      const values = ['a', 'b', 'c'];

      // Act
      const valid = validateConstraint(values, ConstraintType.MIN_COUNT, 2);

      // Assert
      expect(valid).toBe(true);
    });

    it('should validate MAX_COUNT constraint', () => {
      // Arrange
      const values = ['a', 'b'];

      // Act
      const valid = validateConstraint(values, ConstraintType.MAX_COUNT, 3);

      // Assert
      expect(valid).toBe(true);
    });

    it('should validate DATATYPE constraint', () => {
      // Arrange
      const value = 'test string';

      // Act
      const valid = validateConstraint(value, ConstraintType.DATATYPE, 'xsd:string');

      // Assert
      expect(valid).toBe(true);
    });

    it('should validate PATTERN constraint', () => {
      // Arrange
      const value = 'test@example.com';

      // Act
      const valid = validateConstraint(value, ConstraintType.PATTERN, '^[a-z]+@[a-z]+\\.[a-z]+$');

      // Assert
      expect(valid).toBe(true);
    });

    it('should validate MIN_INCLUSIVE constraint', () => {
      // Arrange
      const value = 10;

      // Act
      const valid = validateConstraint(value, ConstraintType.MIN_INCLUSIVE, 5);

      // Assert
      expect(valid).toBe(true);
    });

    it('should validate MAX_INCLUSIVE constraint', () => {
      // Arrange
      const value = 10;

      // Act
      const valid = validateConstraint(value, ConstraintType.MAX_INCLUSIVE, 15);

      // Assert
      expect(valid).toBe(true);
    });

    it('should validate UNIQUE_LANG constraint', () => {
      // Arrange
      const values = [
        { language: 'en' },
        { language: 'fr' },
      ];

      // Act
      const valid = validateConstraint(values, ConstraintType.UNIQUE_LANG, true);

      // Assert
      expect(valid).toBe(true);
    });
  });

  describe('generateReport', () => {
    it('should generate conforming report with no violations', () => {
      // Arrange
      const violations = [];

      // Act
      const report = generateReport(violations);

      // Assert
      expect(report.conforms).toBe(true);
      expect(report.results).toEqual([]);
    });

    it('should generate non-conforming report with violations', () => {
      // Arrange
      const violations = [
        {
          path: 'ex:name',
          message: 'Required property missing',
          focusNode: 'ex:john',
        },
      ];

      // Act
      const report = generateReport(violations);

      // Assert
      expect(report.conforms).toBe(false);
      expect(report.results).toHaveLength(1);
      expect(report.results[0].message).toBe('Required property missing');
    });
  });

  describe('validateNodeKind', () => {
    it('should validate IRI node kind', () => {
      // Arrange
      const node = namedNode('http://example.org/test');

      // Act
      const valid = validateNodeKind(node, 'http://www.w3.org/ns/shacl#IRI');

      // Assert
      expect(valid).toBe(true);
    });

    it('should validate Literal node kind', () => {
      // Arrange
      const node = literal('test');

      // Act
      const valid = validateNodeKind(node, 'http://www.w3.org/ns/shacl#Literal');

      // Assert
      expect(valid).toBe(true);
    });

    it('should validate BlankNode node kind', () => {
      // Arrange
      const node = blankNode();

      // Act
      const valid = validateNodeKind(node, 'http://www.w3.org/ns/shacl#BlankNode');

      // Assert
      expect(valid).toBe(true);
    });

    it('should validate composite node kinds', () => {
      // Arrange
      const node = namedNode('http://example.org/test');

      // Act
      const valid = validateNodeKind(node, 'http://www.w3.org/ns/shacl#BlankNodeOrIRI');

      // Assert
      expect(valid).toBe(true);
    });
  });

  describe('fastValidate', () => {
    it('should perform fast validation', () => {
      // Arrange
      const store = createStore();
      const constraints = {
        targetClass: 'ex:Person',
        properties: {
          'ex:name': { minCount: 1, datatype: 'xsd:string' },
        },
      };

      // Act
      const report = fastValidate(store, constraints);

      // Assert
      expect(report.conforms).toBeDefined();
      expect(Array.isArray(report.results)).toBe(true);
    });
  });
});

describe('RDF Schema Builder', () => {
  describe('shacl', () => {
    it('should create a new shape builder', () => {
      // Act
      const builder = shacl();

      // Assert
      expect(builder).toBeInstanceOf(ShapeBuilder);
    });

    it('should create shape builder with IRI', () => {
      // Act
      const builder = shacl('http://example.org/PersonShape');

      // Assert
      expect(builder).toBeInstanceOf(ShapeBuilder);
    });
  });

  describe('ShapeBuilder', () => {
    it('should build shape with target class', () => {
      // Act
      const quads = shacl()
        .targetClass('http://example.org/Person')
        .build();

      // Assert
      expect(Array.isArray(quads)).toBe(true);
      expect(quads.length).toBeGreaterThan(0);
    });

    it('should build shape with property constraints', () => {
      // Act
      const quads = shacl()
        .targetClass('http://example.org/Person')
        .property('http://xmlns.com/foaf/0.1/name')
          .minCount(1)
          .datatype('xsd:string')
        .build();

      // Assert
      expect(quads.length).toBeGreaterThan(2);
    });

    it('should support method chaining', () => {
      // Act
      const quads = shacl()
        .targetClass('http://example.org/Person')
        .property('http://xmlns.com/foaf/0.1/name')
          .minCount(1)
          .maxCount(1)
          .datatype('xsd:string')
        .property('http://xmlns.com/foaf/0.1/age')
          .datatype('xsd:integer')
          .minInclusive(0)
        .build();

      // Assert
      expect(quads.length).toBeGreaterThan(5);
    });

    it('should convert to Turtle format', () => {
      // Act
      const turtle = shacl()
        .targetClass('http://example.org/Person')
        .toTurtle();

      // Assert
      expect(turtle).toContain('@prefix sh:');
      expect(turtle).toContain('NodeShape');
    });
  });

  describe('PropertyBuilder', () => {
    it('should set minCount constraint', () => {
      // Act
      const quads = shacl()
        .property('http://example.org/name')
          .minCount(1)
        .build();

      // Assert
      expect(quads.some(q => q.predicate.value.includes('minCount'))).toBe(true);
    });

    it('should set maxCount constraint', () => {
      // Act
      const quads = shacl()
        .property('http://example.org/name')
          .maxCount(1)
        .build();

      // Assert
      expect(quads.some(q => q.predicate.value.includes('maxCount'))).toBe(true);
    });

    it('should set datatype constraint', () => {
      // Act
      const quads = shacl()
        .property('http://example.org/name')
          .datatype('xsd:string')
        .build();

      // Assert
      expect(quads.some(q => q.predicate.value.includes('datatype'))).toBe(true);
    });

    it('should set nodeKind constraint', () => {
      // Act
      const quads = shacl()
        .property('http://example.org/ref')
          .nodeKind('IRI')
        .build();

      // Assert
      expect(quads.some(q => q.predicate.value.includes('nodeKind'))).toBe(true);
    });

    it('should set pattern constraint', () => {
      // Act
      const quads = shacl()
        .property('http://example.org/email')
          .pattern('^[a-z]+@[a-z]+\\.[a-z]+$')
        .build();

      // Assert
      expect(quads.some(q => q.predicate.value.includes('pattern'))).toBe(true);
    });

    it('should set range constraints', () => {
      // Act
      const quads = shacl()
        .property('http://example.org/age')
          .minInclusive(0)
          .maxInclusive(120)
        .build();

      // Assert
      expect(quads.some(q => q.predicate.value.includes('minInclusive'))).toBe(true);
      expect(quads.some(q => q.predicate.value.includes('maxInclusive'))).toBe(true);
    });

    it('should validate constraint values with Zod', () => {
      // Act & Assert
      expect(() => shacl().property('test').minCount(-1)).toThrow();
      expect(() => shacl().property('test').datatype('')).toThrow();
    });
  });

  describe('CommonShapes', () => {
    it('should create IRI shape pattern', () => {
      // Act
      const builder = CommonShapes.iri('http://example.org/ref', { minCount: 1 });

      // Assert
      expect(builder).toBeDefined();
    });

    it('should create literal shape pattern', () => {
      // Act
      const builder = CommonShapes.literal('http://example.org/name', 'xsd:string', { minCount: 1 });

      // Assert
      expect(builder).toBeDefined();
    });

    it('should create string shape pattern', () => {
      // Act
      const builder = CommonShapes.string('http://example.org/name', { minCount: 1 });

      // Assert
      expect(builder).toBeDefined();
    });

    it('should create integer shape pattern', () => {
      // Act
      const builder = CommonShapes.integer('http://example.org/age', {
        minCount: 1,
        minInclusive: 0,
        maxInclusive: 120,
      });

      // Assert
      expect(builder).toBeDefined();
    });
  });

  describe('Integration - Builder + Validator', () => {
    it('should validate data against builder-generated shapes', async () => {
      // Arrange
      const shapes = shacl('http://example.org/PersonShape')
        .targetClass('http://example.org/Person')
        .property('http://example.org/name')
          .minCount(1)
          .datatype('xsd:string')
        .build();

      const conformingData = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://example.org/Person')
        ),
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://example.org/name'),
          literal('Alice')
        ),
      ];

      // Act
      const report = await validateGraph(conformingData, shapes);

      // Assert
      expect(report).toBeDefined();
      expect(report.conforms).toBe(true);
      expect(report.results).toEqual([]);
    });

    it('should build shapes that can be serialized', () => {
      // Arrange & Act
      const shape = shacl()
        .targetClass('http://example.org/Person')
        .property('http://example.org/name')
          .minCount(1)
        .build();

      // Assert
      expect(shape).toBeDefined();
      expect(Array.isArray(shape)).toBe(true);
      expect(shape.length).toBeGreaterThan(0);
    });
  });
});
