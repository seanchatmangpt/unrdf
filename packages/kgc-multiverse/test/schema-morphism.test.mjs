/**
 * KGC Multiverse - Schema Morphism Tests
 * Comprehensive tests for RDF schema/ontology transformations
 */

import { describe, it, expect } from 'vitest';
import {
  createClassRenameMorphism,
  createPropertyRenameMorphism,
  createNamespaceMigrationMorphism,
  createDatatypeConversionMorphism,
  createPropertyChainMorphism,
} from '../src/schema-morphism.mjs';
import { MorphismType, applyMorphism } from '../src/morphism.mjs';

describe('Schema Morphisms', () => {
  describe('createClassRenameMorphism', () => {
    it('creates morphism with correct metadata', async () => {
      const phi = await createClassRenameMorphism(
        'http://old.com/Person',
        'http://new.com/Person'
      );

      expect(phi.id).toMatch(/^Φ_[a-f0-9]{16}$/);
      expect(phi.type).toBe(MorphismType.SCHEMA);
      expect(phi.name).toBe('rename-class-Person');
      expect(phi.metadata.oldClass).toBe('http://old.com/Person');
      expect(phi.metadata.newClass).toBe('http://new.com/Person');
    });

    it('renames class instances correctly', async () => {
      const phi = await createClassRenameMorphism(
        'http://old.com/Person',
        'http://new.com/Person'
      );

      const quads = [
        {
          subject: { value: 'ex:john' },
          predicate: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
          object: { value: 'http://old.com/Person', termType: 'NamedNode' },
        },
        {
          subject: { value: 'ex:jane' },
          predicate: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
          object: { value: 'http://old.com/Person', termType: 'NamedNode' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      // 2 quads × (delete + add) = 4 deltas
      expect(deltas.length).toBe(4);

      // First pair (ex:john)
      expect(deltas[0].type).toBe('delete');
      expect(deltas[0].subject).toBe('ex:john');
      expect(deltas[0].object.value).toBe('http://old.com/Person');

      expect(deltas[1].type).toBe('add');
      expect(deltas[1].subject).toBe('ex:john');
      expect(deltas[1].object.value).toBe('http://new.com/Person');

      // Second pair (ex:jane)
      expect(deltas[2].type).toBe('delete');
      expect(deltas[2].subject).toBe('ex:jane');

      expect(deltas[3].type).toBe('add');
      expect(deltas[3].subject).toBe('ex:jane');
    });

    it('ignores non-matching classes', async () => {
      const phi = await createClassRenameMorphism(
        'http://old.com/Person',
        'http://new.com/Person'
      );

      const quads = [
        {
          subject: { value: 'ex:org' },
          predicate: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
          object: { value: 'http://schema.org/Organization', termType: 'NamedNode' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(0);
    });

    it('preserves RDF type predicate', async () => {
      const phi = await createClassRenameMorphism('http://ex.com/A', 'http://ex.com/B');

      const quads = [
        {
          subject: { value: 'ex:entity' },
          predicate: { value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
          object: { value: 'http://ex.com/A', termType: 'NamedNode' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas[0].predicate).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
      expect(deltas[1].predicate).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    });
  });

  describe('createPropertyRenameMorphism', () => {
    it('creates morphism with correct metadata', async () => {
      const phi = await createPropertyRenameMorphism(
        'http://schema.org/name',
        'http://schema.org/fullName'
      );

      expect(phi.type).toBe(MorphismType.SCHEMA);
      expect(phi.name).toBe('rename-property-name');
      expect(phi.metadata.oldProperty).toBe('http://schema.org/name');
      expect(phi.metadata.newProperty).toBe('http://schema.org/fullName');
    });

    it('renames property with literal object', async () => {
      const phi = await createPropertyRenameMorphism(
        'http://ex.com/oldProp',
        'http://ex.com/newProp'
      );

      const quads = [
        {
          subject: { value: 'ex:s1' },
          predicate: { value: 'http://ex.com/oldProp' },
          object: { value: 'literal value', termType: 'Literal' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(2);
      expect(deltas[0].type).toBe('delete');
      expect(deltas[0].predicate).toBe('http://ex.com/oldProp');
      expect(deltas[1].type).toBe('add');
      expect(deltas[1].predicate).toBe('http://ex.com/newProp');
      expect(deltas[1].object.value).toBe('literal value');
    });

    it('preserves datatype in literal objects', async () => {
      const phi = await createPropertyRenameMorphism('http://ex.com/p1', 'http://ex.com/p2');

      const quads = [
        {
          subject: { value: 'ex:s' },
          predicate: { value: 'http://ex.com/p1' },
          object: {
            value: '42',
            termType: 'Literal',
            datatype: { value: 'http://www.w3.org/2001/XMLSchema#integer' },
          },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas[1].object.datatype).toBe('http://www.w3.org/2001/XMLSchema#integer');
    });

    it('preserves language tag in literal objects', async () => {
      const phi = await createPropertyRenameMorphism('http://ex.com/label', 'http://ex.com/title');

      const quads = [
        {
          subject: { value: 'ex:s' },
          predicate: { value: 'http://ex.com/label' },
          object: {
            value: 'Hello',
            termType: 'Literal',
            language: 'en',
          },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas[1].object.language).toBe('en');
    });

    it('handles NamedNode objects', async () => {
      const phi = await createPropertyRenameMorphism('http://ex.com/link', 'http://ex.com/ref');

      const quads = [
        {
          subject: { value: 'ex:s' },
          predicate: { value: 'http://ex.com/link' },
          object: { value: 'http://ex.com/target', termType: 'NamedNode' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(2);
      expect(deltas[1].object.type).toBe('NamedNode');
      expect(deltas[1].object.value).toBe('http://ex.com/target');
    });

    it('ignores non-matching properties', async () => {
      const phi = await createPropertyRenameMorphism('http://ex.com/p1', 'http://ex.com/p2');

      const quads = [
        {
          subject: { value: 'ex:s' },
          predicate: { value: 'http://ex.com/other' },
          object: { value: 'value', termType: 'Literal' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(0);
    });
  });

  describe('createNamespaceMigrationMorphism', () => {
    it('creates morphism with correct metadata', async () => {
      const phi = await createNamespaceMigrationMorphism(
        'http://old-schema.org/',
        'http://new-schema.org/'
      );

      expect(phi.type).toBe(MorphismType.SCHEMA);
      expect(phi.name).toBe('migrate-namespace');
      expect(phi.metadata.oldNamespace).toBe('http://old-schema.org/');
      expect(phi.metadata.newNamespace).toBe('http://new-schema.org/');
    });

    it('migrates subject namespace', async () => {
      const phi = await createNamespaceMigrationMorphism(
        'http://old.com/',
        'http://new.com/'
      );

      const quads = [
        {
          subject: { value: 'http://old.com/entity1' },
          predicate: { value: 'http://other.com/prop' },
          object: { value: 'literal', termType: 'Literal' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(2);
      expect(deltas[0].type).toBe('delete');
      expect(deltas[0].subject).toBe('http://old.com/entity1');
      expect(deltas[1].type).toBe('add');
      expect(deltas[1].subject).toBe('http://new.com/entity1');
    });

    it('migrates predicate namespace', async () => {
      const phi = await createNamespaceMigrationMorphism(
        'http://old.com/',
        'http://new.com/'
      );

      const quads = [
        {
          subject: { value: 'http://other.com/s' },
          predicate: { value: 'http://old.com/property' },
          object: { value: 'literal', termType: 'Literal' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas[1].predicate).toBe('http://new.com/property');
    });

    it('migrates object namespace for NamedNode', async () => {
      const phi = await createNamespaceMigrationMorphism(
        'http://old.com/',
        'http://new.com/'
      );

      const quads = [
        {
          subject: { value: 'http://other.com/s' },
          predicate: { value: 'http://other.com/p' },
          object: { value: 'http://old.com/object', termType: 'NamedNode' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas[1].object.value).toBe('http://new.com/object');
    });

    it('does not migrate literal object values', async () => {
      const phi = await createNamespaceMigrationMorphism(
        'http://old.com/',
        'http://new.com/'
      );

      const quads = [
        {
          subject: { value: 'http://old.com/s' },
          predicate: { value: 'http://other.com/p' },
          object: { value: 'http://old.com/literal-text', termType: 'Literal' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      // Subject migrated, but literal object value unchanged
      expect(deltas[1].object.value).toBe('http://old.com/literal-text');
    });

    it('migrates all matching components in one quad', async () => {
      const phi = await createNamespaceMigrationMorphism(
        'http://old.com/',
        'http://new.com/'
      );

      const quads = [
        {
          subject: { value: 'http://old.com/s' },
          predicate: { value: 'http://old.com/p' },
          object: { value: 'http://old.com/o', termType: 'NamedNode' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(2);
      expect(deltas[1].subject).toBe('http://new.com/s');
      expect(deltas[1].predicate).toBe('http://new.com/p');
      expect(deltas[1].object.value).toBe('http://new.com/o');
    });

    it('preserves datatype in literals', async () => {
      const phi = await createNamespaceMigrationMorphism('http://old/', 'http://new/');

      const quads = [
        {
          subject: { value: 'http://old/s' },
          predicate: { value: 'http://other/p' },
          object: {
            value: '42',
            termType: 'Literal',
            datatype: { value: 'http://www.w3.org/2001/XMLSchema#integer' },
          },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas[1].object.datatype).toBe('http://www.w3.org/2001/XMLSchema#integer');
    });
  });

  describe('createDatatypeConversionMorphism', () => {
    it('creates morphism with correct metadata', async () => {
      const phi = await createDatatypeConversionMorphism(
        'http://www.w3.org/2001/XMLSchema#string',
        'http://www.w3.org/2001/XMLSchema#token'
      );

      expect(phi.type).toBe(MorphismType.SCHEMA);
      expect(phi.name).toBe('convert-datatype-string');
      expect(phi.metadata.oldDatatype).toBe('http://www.w3.org/2001/XMLSchema#string');
      expect(phi.metadata.newDatatype).toBe('http://www.w3.org/2001/XMLSchema#token');
    });

    it('converts datatype without converter', async () => {
      const phi = await createDatatypeConversionMorphism(
        'http://www.w3.org/2001/XMLSchema#string',
        'http://www.w3.org/2001/XMLSchema#token'
      );

      const quads = [
        {
          subject: { value: 'ex:s' },
          predicate: { value: 'ex:p' },
          object: {
            value: 'test value',
            termType: 'Literal',
            datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' },
          },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(2);
      expect(deltas[0].type).toBe('delete');
      expect(deltas[0].object.datatype).toBe('http://www.w3.org/2001/XMLSchema#string');
      expect(deltas[1].type).toBe('add');
      expect(deltas[1].object.datatype).toBe('http://www.w3.org/2001/XMLSchema#token');
      expect(deltas[1].object.value).toBe('test value');
    });

    it('applies value converter function', async () => {
      const phi = await createDatatypeConversionMorphism(
        'http://www.w3.org/2001/XMLSchema#string',
        'http://www.w3.org/2001/XMLSchema#token',
        (value) => value.trim().toUpperCase()
      );

      const quads = [
        {
          subject: { value: 'ex:s' },
          predicate: { value: 'ex:p' },
          object: {
            value: '  hello world  ',
            termType: 'Literal',
            datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' },
          },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas[1].object.value).toBe('HELLO WORLD');
    });

    it('ignores non-matching datatypes', async () => {
      const phi = await createDatatypeConversionMorphism(
        'http://www.w3.org/2001/XMLSchema#string',
        'http://www.w3.org/2001/XMLSchema#token'
      );

      const quads = [
        {
          subject: { value: 'ex:s' },
          predicate: { value: 'ex:p' },
          object: {
            value: '42',
            termType: 'Literal',
            datatype: { value: 'http://www.w3.org/2001/XMLSchema#integer' },
          },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(0);
    });

    it('ignores literals without datatype', async () => {
      const phi = await createDatatypeConversionMorphism(
        'http://www.w3.org/2001/XMLSchema#string',
        'http://www.w3.org/2001/XMLSchema#token'
      );

      const quads = [
        {
          subject: { value: 'ex:s' },
          predicate: { value: 'ex:p' },
          object: { value: 'plain literal', termType: 'Literal' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(0);
    });

    it('ignores NamedNode objects', async () => {
      const phi = await createDatatypeConversionMorphism(
        'http://www.w3.org/2001/XMLSchema#string',
        'http://www.w3.org/2001/XMLSchema#token'
      );

      const quads = [
        {
          subject: { value: 'ex:s' },
          predicate: { value: 'ex:p' },
          object: { value: 'http://ex.com/resource', termType: 'NamedNode' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(0);
    });
  });

  describe('createPropertyChainMorphism', () => {
    it('creates morphism with correct metadata', async () => {
      const phi = await createPropertyChainMorphism(
        ['http://ex.com/address', 'http://ex.com/city'],
        'http://ex.com/cityOfResidence'
      );

      expect(phi.type).toBe(MorphismType.SCHEMA);
      expect(phi.name).toBe('property-chain-collapse');
      expect(phi.metadata.propertyChain).toEqual([
        'http://ex.com/address',
        'http://ex.com/city',
      ]);
      expect(phi.metadata.targetProperty).toBe('http://ex.com/cityOfResidence');
    });

    it('collapses two-property chain', async () => {
      const phi = await createPropertyChainMorphism(
        ['http://ex.com/address', 'http://ex.com/city'],
        'http://ex.com/cityOfResidence'
      );

      const quads = [
        {
          subject: { value: 'ex:john' },
          predicate: { value: 'http://ex.com/address' },
          object: { value: 'ex:addr1', termType: 'NamedNode' },
        },
        {
          subject: { value: 'ex:addr1' },
          predicate: { value: 'http://ex.com/city' },
          object: { value: 'ex:boston', termType: 'NamedNode' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(1);
      expect(deltas[0].type).toBe('add');
      expect(deltas[0].subject).toBe('ex:john');
      expect(deltas[0].predicate).toBe('http://ex.com/cityOfResidence');
      expect(deltas[0].object.value).toBe('ex:boston');
    });

    it('handles three-property chain', async () => {
      const phi = await createPropertyChainMorphism(
        ['http://ex.com/p1', 'http://ex.com/p2', 'http://ex.com/p3'],
        'http://ex.com/direct'
      );

      const quads = [
        {
          subject: { value: 'ex:start' },
          predicate: { value: 'http://ex.com/p1' },
          object: { value: 'ex:mid1', termType: 'NamedNode' },
        },
        {
          subject: { value: 'ex:mid1' },
          predicate: { value: 'http://ex.com/p2' },
          object: { value: 'ex:mid2', termType: 'NamedNode' },
        },
        {
          subject: { value: 'ex:mid2' },
          predicate: { value: 'http://ex.com/p3' },
          object: { value: 'ex:end', termType: 'NamedNode' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(1);
      expect(deltas[0].subject).toBe('ex:start');
      expect(deltas[0].object.value).toBe('ex:end');
    });

    it('handles incomplete chains gracefully', async () => {
      const phi = await createPropertyChainMorphism(
        ['http://ex.com/p1', 'http://ex.com/p2'],
        'http://ex.com/direct'
      );

      const quads = [
        {
          subject: { value: 'ex:start' },
          predicate: { value: 'http://ex.com/p1' },
          object: { value: 'ex:mid', termType: 'NamedNode' },
        },
        // Missing p2 from ex:mid
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      // No complete chain - no deltas
      expect(deltas.length).toBe(0);
    });

    it('handles multiple complete chains', async () => {
      const phi = await createPropertyChainMorphism(
        ['http://ex.com/address', 'http://ex.com/city'],
        'http://ex.com/cityOfResidence'
      );

      const quads = [
        // Chain 1: john -> addr1 -> boston
        {
          subject: { value: 'ex:john' },
          predicate: { value: 'http://ex.com/address' },
          object: { value: 'ex:addr1', termType: 'NamedNode' },
        },
        {
          subject: { value: 'ex:addr1' },
          predicate: { value: 'http://ex.com/city' },
          object: { value: 'ex:boston', termType: 'NamedNode' },
        },
        // Chain 2: jane -> addr2 -> newyork
        {
          subject: { value: 'ex:jane' },
          predicate: { value: 'http://ex.com/address' },
          object: { value: 'ex:addr2', termType: 'NamedNode' },
        },
        {
          subject: { value: 'ex:addr2' },
          predicate: { value: 'http://ex.com/city' },
          object: { value: 'ex:newyork', termType: 'NamedNode' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(2);
      expect(deltas[0].subject).toBe('ex:john');
      expect(deltas[0].object.value).toBe('ex:boston');
      expect(deltas[1].subject).toBe('ex:jane');
      expect(deltas[1].object.value).toBe('ex:newyork');
    });

    it('ignores non-chain predicates', async () => {
      const phi = await createPropertyChainMorphism(
        ['http://ex.com/address', 'http://ex.com/city'],
        'http://ex.com/cityOfResidence'
      );

      const quads = [
        {
          subject: { value: 'ex:john' },
          predicate: { value: 'http://ex.com/name' },
          object: { value: 'John', termType: 'Literal' },
        },
      ];

      const deltas = applyMorphism(phi, quads, 'ACTIVE');

      expect(deltas.length).toBe(0);
    });
  });
});
