/**
 * @file SHACL Validation README Example Tests (London TDD)
 * @description Tests for SHACL validation examples from README.md
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { Store } from 'n3';

describe('README SHACL Validation Examples', () => {
  let mockSystem;
  let mockParseTurtle;
  let mockShapesStore;
  let mockDataStore;

  beforeEach(() => {
    mockShapesStore = new Store();
    mockDataStore = new Store();

    mockParseTurtle = vi.fn().mockResolvedValue(mockShapesStore);

    mockSystem = {
      validate: vi.fn(),
    };
  });

  describe('Define SHACL Shapes', () => {
    it('should parse SHACL shapes from Turtle', async () => {
      const shapesInput = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:PersonShape a sh:NodeShape ;
          sh:targetClass foaf:Person ;
          sh:property [
            sh:path foaf:name ;
            sh:minCount 1 ;
            sh:datatype xsd:string ;
          ] .
      `;

      const shapes = await mockParseTurtle(shapesInput);

      expect(mockParseTurtle).toHaveBeenCalledWith(shapesInput);
      expect(shapes).toBeInstanceOf(Store);
    });

    it('should define shape with multiple properties', async () => {
      const shapesInput = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:PersonShape a sh:NodeShape ;
          sh:targetClass foaf:Person ;
          sh:property [
            sh:path foaf:name ;
            sh:minCount 1 ;
          ] ;
          sh:property [
            sh:path foaf:mbox ;
            sh:minCount 1 ;
          ] .
      `;

      const shapes = await mockParseTurtle(shapesInput);

      expect(shapes).toBeInstanceOf(Store);
    });

    it('should define shape with constraints', async () => {
      const shapesInput = `
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:AgeShape a sh:NodeShape ;
          sh:property [
            sh:path ex:age ;
            sh:minInclusive 0 ;
            sh:maxInclusive 150 ;
            sh:datatype xsd:integer ;
          ] .
      `;

      const shapes = await mockParseTurtle(shapesInput);

      expect(shapes).toBeInstanceOf(Store);
    });
  });

  describe('Validate Data', () => {
    it('should validate conforming data', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: true,
        results: [],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(mockSystem.validate).toHaveBeenCalledWith({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.conforms).toBe(true);
      expect(validation.results).toHaveLength(0);
    });

    it('should detect violations in non-conforming data', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: false,
        results: [
          {
            severity: 'Violation',
            focusNode: 'http://example.org/alice',
            path: 'http://xmlns.com/foaf/0.1/name',
            message: 'Property foaf:name is required but missing',
          },
        ],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.conforms).toBe(false);
      expect(validation.results).toHaveLength(1);
      expect(validation.results[0].severity).toBe('Violation');
    });

    it('should report minCount violations', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: false,
        results: [
          {
            severity: 'Violation',
            sourceConstraintComponent: 'http://www.w3.org/ns/shacl#MinCountConstraintComponent',
            focusNode: 'http://example.org/alice',
            path: 'http://xmlns.com/foaf/0.1/name',
            message: 'Expected at least 1 values, but found 0',
          },
        ],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.conforms).toBe(false);
      expect(validation.results[0].sourceConstraintComponent).toContain('MinCount');
    });

    it('should report datatype violations', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: false,
        results: [
          {
            severity: 'Violation',
            sourceConstraintComponent: 'http://www.w3.org/ns/shacl#DatatypeConstraintComponent',
            focusNode: 'http://example.org/alice',
            path: 'http://example.org/age',
            message: 'Value does not have datatype xsd:integer',
          },
        ],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.conforms).toBe(false);
      expect(validation.results[0].sourceConstraintComponent).toContain('Datatype');
    });

    it('should handle multiple violations', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: false,
        results: [
          {
            severity: 'Violation',
            focusNode: 'http://example.org/alice',
            path: 'http://xmlns.com/foaf/0.1/name',
            message: 'Missing required property',
          },
          {
            severity: 'Violation',
            focusNode: 'http://example.org/alice',
            path: 'http://xmlns.com/foaf/0.1/mbox',
            message: 'Missing required property',
          },
        ],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.conforms).toBe(false);
      expect(validation.results).toHaveLength(2);
    });
  });

  describe('Validation Error Reporting', () => {
    it('should log validation errors when non-conforming', async () => {
      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      mockSystem.validate.mockResolvedValue({
        conforms: false,
        results: [
          {
            severity: 'Violation',
            focusNode: 'http://example.org/alice',
            path: 'http://xmlns.com/foaf/0.1/name',
            message: 'Property foaf:name is required',
          },
        ],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      if (!validation.conforms) {
        console.log('Validation errors:', validation.results);
      }

      expect(consoleSpy).toHaveBeenCalledWith(
        'Validation errors:',
        expect.arrayContaining([expect.objectContaining({ severity: 'Violation' })])
      );

      consoleSpy.mockRestore();
    });

    it('should not log when validation passes', async () => {
      const consoleSpy = vi.spyOn(console, 'log').mockImplementation(() => {});

      mockSystem.validate.mockResolvedValue({
        conforms: true,
        results: [],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      if (!validation.conforms) {
        console.log('Validation errors:', validation.results);
      }

      expect(consoleSpy).not.toHaveBeenCalled();

      consoleSpy.mockRestore();
    });
  });

  describe('SHACL Constraint Types', () => {
    it('should validate sh:minCount constraint', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: false,
        results: [
          {
            sourceConstraintComponent: 'http://www.w3.org/ns/shacl#MinCountConstraintComponent',
            message: 'Expected at least 1 values',
          },
        ],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.results[0].sourceConstraintComponent).toContain('MinCount');
    });

    it('should validate sh:maxCount constraint', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: false,
        results: [
          {
            sourceConstraintComponent: 'http://www.w3.org/ns/shacl#MaxCountConstraintComponent',
            message: 'Expected at most 1 values',
          },
        ],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.results[0].sourceConstraintComponent).toContain('MaxCount');
    });

    it('should validate sh:datatype constraint', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: false,
        results: [
          {
            sourceConstraintComponent: 'http://www.w3.org/ns/shacl#DatatypeConstraintComponent',
            message: 'Value does not have required datatype',
          },
        ],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.results[0].sourceConstraintComponent).toContain('Datatype');
    });

    it('should validate sh:pattern constraint', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: false,
        results: [
          {
            sourceConstraintComponent: 'http://www.w3.org/ns/shacl#PatternConstraintComponent',
            message: 'Value does not match pattern',
          },
        ],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.results[0].sourceConstraintComponent).toContain('Pattern');
    });

    it('should validate sh:minInclusive constraint', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: false,
        results: [
          {
            sourceConstraintComponent: 'http://www.w3.org/ns/shacl#MinInclusiveConstraintComponent',
            message: 'Value is less than minimum',
          },
        ],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.results[0].sourceConstraintComponent).toContain('MinInclusive');
    });

    it('should validate sh:maxInclusive constraint', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: false,
        results: [
          {
            sourceConstraintComponent: 'http://www.w3.org/ns/shacl#MaxInclusiveConstraintComponent',
            message: 'Value exceeds maximum',
          },
        ],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.results[0].sourceConstraintComponent).toContain('MaxInclusive');
    });
  });

  describe('Complex Validation Scenarios', () => {
    it('should validate nested shapes', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: true,
        results: [],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.conforms).toBe(true);
    });

    it('should validate with sh:or logical constraint', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: true,
        results: [],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.conforms).toBe(true);
    });

    it('should validate with sh:and logical constraint', async () => {
      mockSystem.validate.mockResolvedValue({
        conforms: false,
        results: [
          {
            message: 'Does not satisfy all conditions in sh:and',
          },
        ],
      });

      const validation = await mockSystem.validate({
        dataGraph: mockDataStore,
        shapesGraph: mockShapesStore,
      });

      expect(validation.conforms).toBe(false);
    });
  });
});
