/**
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { loadPictlOntology, queryPictlKnowledge, PICTL_CLASSES, PICTL_PROPERTIES } from '../src/ontology-loader.mjs';

describe('PICTL Ontology Loader', () => {
  let ontology;

  beforeEach(async () => {
    ontology = await loadPictlOntology();
  });

  describe('Ontology Loading', () => {
    it('should load PICTL ontology', async () => {
      expect(ontology).toBeDefined();
      expect(ontology.initialized).toBe(true);
    });

    it('should have all PICTL classes', () => {
      const classNames = Object.keys(PICTL_CLASSES);
      expect(classNames).toContain('Log');
      expect(classNames).toContain('Event');
      expect(classNames).toContain('Activity');
      expect(classNames).toContain('ProcessModel');
      expect(classNames).toContain('Fitness');
    });

    it('should have all PICTL properties', () => {
      const propNames = Object.keys(PICTL_PROPERTIES);
      expect(propNames).toContain('hasEvent');
      expect(propNames).toContain('hasActivity');
      expect(propNames).toContain('fitnessScore');
      expect(propNames).toContain('precisionScore');
    });

    it('should return statistics', () => {
      const stats = ontology.getStats();
      expect(stats.tripleCount).toBeGreaterThan(0);
      expect(stats.classCount).toBe(Object.keys(PICTL_CLASSES).length);
      expect(stats.propertyCount).toBe(Object.keys(PICTL_PROPERTIES).length);
      expect(stats.initialized).toBe(true);
    });
  });

  describe('Adding Triples', () => {
    it('should add new triple to ontology', () => {
      const initialCount = ontology.triples.length;

      const triple = {
        subject: 'urn:pictl:MyLog',
        predicate: PICTL_PROPERTIES.hasEvent,
        object: 'urn:pictl:MyEvent',
      };

      const added = ontology.addTriple(triple);
      expect(added).toBe(true);
      expect(ontology.triples).toHaveLength(initialCount + 1);
    });

    it('should prevent duplicate triples', () => {
      const triple = {
        subject: 'urn:pictl:MyLog',
        predicate: PICTL_PROPERTIES.hasEvent,
        object: 'urn:pictl:MyEvent',
      };

      const added1 = ontology.addTriple(triple);
      const added2 = ontology.addTriple(triple);

      expect(added1).toBe(true);
      expect(added2).toBe(false); // Duplicate rejected
    });

    it('should add literal triples', () => {
      const triple = {
        subject: 'urn:pictl:MyLog',
        predicate: 'http://www.w3.org/2000/01/rdf-schema#label',
        object: 'My Log Label',
        objectType: 'literal',
      };

      const added = ontology.addTriple(triple);
      expect(added).toBe(true);

      // Verify it was stored with literal type
      const stored = ontology.triples.find(
        t =>
          t.subject === triple.subject &&
          t.predicate === triple.predicate
      );
      expect(stored.objectType).toBe('literal');
    });
  });

  describe('SPARQL Queries', () => {
    it('should execute SELECT query', () => {
      const result = ontology.query(`
        SELECT ?class WHERE {
          ?class a <http://www.w3.org/2002/07/owl#Class>
        }
      `);

      expect(result.head).toBeDefined();
      expect(result.head.vars).toContain('class');
      expect(result.results).toBeDefined();
      expect(Array.isArray(result.results.bindings)).toBe(true);
    });

    it('should execute ASK query', () => {
      const result = ontology.query(`
        ASK {
          <urn:pictl:Log> a <http://www.w3.org/2002/07/owl#Class>
        }
      `);

      expect(result.boolean).toBeDefined();
      expect(typeof result.boolean).toBe('boolean');
    });

    it('should handle variable binding in queries', () => {
      // Add test data
      ontology.addTriple({
        subject: 'urn:test:Log1',
        predicate: PICTL_PROPERTIES.hasEvent,
        object: 'urn:test:Event1',
      });

      const result = ontology.query(`
        SELECT ?log ?event WHERE {
          ?log a <urn:pictl:Log> .
          ?log <urn:pictl:hasEvent> ?event
        }
      `);

      expect(result.head.vars).toContain('log');
      expect(result.head.vars).toContain('event');
    });

    it('should return empty results for non-matching patterns', () => {
      const result = ontology.query(`
        SELECT ?x WHERE {
          ?x a <urn:nonexistent:Class>
        }
      `);

      expect(result.results.bindings).toHaveLength(0);
    });
  });

  describe('Querying Knowledge Graph', () => {
    it('should query PICTL knowledge', async () => {
      const result = await queryPictlKnowledge(`
        SELECT ?class WHERE {
          ?class a <http://www.w3.org/2002/07/owl#Class>
        }
      `, ontology);

      expect(result.head).toBeDefined();
      expect(result.results).toBeDefined();
    });

    it('should use default ontology if not provided', async () => {
      const result = await queryPictlKnowledge(`
        ASK {
          ?x a <http://www.w3.org/2002/07/owl#Class>
        }
      `);

      expect(result.boolean !== undefined).toBe(true);
    });

    it('should return formatted SPARQL results', async () => {
      ontology.addTriple({
        subject: 'urn:test:Entity1',
        predicate: 'http://www.w3.org/2000/01/rdf-schema#label',
        object: 'Entity 1',
        objectType: 'literal',
      });

      const result = await queryPictlKnowledge(`
        SELECT ?entity ?label WHERE {
          ?entity <http://www.w3.org/2000/01/rdf-schema#label> ?label
        }
      `, ontology);

      expect(result.head.vars).toContain('entity');
      expect(result.head.vars).toContain('label');
    });
  });

  describe('N-Triples Export', () => {
    it('should export ontology as N-Triples', () => {
      const triples = ontology.exportNTriples();

      expect(Array.isArray(triples)).toBe(true);
      expect(triples.length).toBeGreaterThan(0);

      // Check format
      const nTriplePattern = /^<.+>\s+<.+>\s+.+\s+\.$/;
      for (const triple of triples.slice(0, 5)) {
        expect(triple).toMatch(nTriplePattern);
      }
    });

    it('should properly quote literals in N-Triples', () => {
      ontology.addTriple({
        subject: 'urn:test:Thing',
        predicate: 'http://www.w3.org/2000/01/rdf-schema#label',
        object: 'Label Text',
        objectType: 'literal',
      });

      const triples = ontology.exportNTriples();
      const literalTriple = triples.find(t => t.includes('Label Text'));

      expect(literalTriple).toMatch(/"Label Text"/);
    });
  });

  describe('Namespaces', () => {
    it('should have standard RDF namespaces', () => {
      expect(ontology.namespaces).toBeDefined();
      expect(ontology.namespaces.rdf).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
      expect(ontology.namespaces.rdfs).toBe('http://www.w3.org/2000/01/rdf-schema#');
      expect(ontology.namespaces.owl).toBe('http://www.w3.org/2002/07/owl#');
    });

    it('should have PICTL namespace', () => {
      expect(ontology.namespaces.pictl).toBe('urn:pictl:');
    });
  });

  describe('Domain and Range Axioms', () => {
    it('should have domain axioms for event properties', () => {
      const eventDomainTriples = ontology.triples.filter(
        t =>
          t.predicate === 'http://www.w3.org/2000/01/rdf-schema#domain' &&
          t.object === PICTL_CLASSES.Event
      );

      expect(eventDomainTriples.length).toBeGreaterThan(0);
    });

    it('should have range axioms for metric properties', () => {
      const doubleRangeTriples = ontology.triples.filter(
        t =>
          t.predicate === 'http://www.w3.org/2000/01/rdf-schema#range' &&
          t.object === 'http://www.w3.org/2001/XMLSchema#double'
      );

      expect(doubleRangeTriples.length).toBeGreaterThan(0);
    });
  });
});
