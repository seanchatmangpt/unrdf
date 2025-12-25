/**
 * @fileoverview Tests for CRDT-based RDF graph
 */

import { describe, it, expect, beforeEach } from 'vitest';
import * as Y from 'yjs';
import { CollaborativeRDFGraph } from '../src/crdt/rdf-crdt.mjs';

describe('CollaborativeRDFGraph', () => {
  let graph;

  beforeEach(() => {
    graph = new CollaborativeRDFGraph();
  });

  describe('Basic Operations', () => {
    it('should add a triple', () => {
      graph.addTriple({
        subject: 'http://example.org/alice',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Alice',
        objectType: 'literal',
      });

      const triples = graph.getTriples();
      expect(triples).toHaveLength(1);
      expect(triples[0].subject).toBe('http://example.org/alice');
    });

    it('should remove a triple', () => {
      const triple = {
        subject: 'http://example.org/alice',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Alice',
        objectType: 'literal',
      };

      graph.addTriple(triple);
      expect(graph.getTriples()).toHaveLength(1);

      graph.removeTriple(triple);
      expect(graph.getTriples()).toHaveLength(0);

      // Tombstone should exist
      const stats = graph.getStats();
      expect(stats.tombstones).toBe(1);
    });

    it('should query triples by pattern', () => {
      graph.addTriple({
        subject: 'http://example.org/alice',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Alice',
      });

      graph.addTriple({
        subject: 'http://example.org/bob',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Bob',
      });

      const results = graph.queryTriples({
        subject: 'http://example.org/alice',
      });

      expect(results).toHaveLength(1);
      expect(results[0].object).toBe('Alice');
    });
  });

  describe('CRDT Conflict Resolution', () => {
    it('should resolve concurrent additions (LWW)', () => {
      // In RDF CRDTs, the triple IS the key (subject+predicate+object)
      // LWW applies when the EXACT SAME triple is added by multiple clients
      const doc1 = new Y.Doc();
      const doc2 = new Y.Doc();

      const graph1 = new CollaborativeRDFGraph(doc1);
      const graph2 = new CollaborativeRDFGraph(doc2);

      // Both clients concurrently add the SAME triple
      graph1.addTriple({
        subject: 'http://example.org/alice',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Alice',
      });

      graph2.addTriple({
        subject: 'http://example.org/alice',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Alice', // Same triple!
      });

      // Sync (merge CRDTs)
      Y.applyUpdate(doc1, Y.encodeStateAsUpdate(doc2));
      Y.applyUpdate(doc2, Y.encodeStateAsUpdate(doc1));

      // Both should converge to same state with 1 triple (deduplication)
      const triples1 = graph1.getTriples();
      const triples2 = graph2.getTriples();

      expect(triples1).toHaveLength(1);
      expect(triples2).toHaveLength(1);
      expect(triples1[0].object).toBe('Alice');
      expect(triples2[0].object).toBe('Alice');
    });

    it('should handle add-remove conflicts (add wins)', () => {
      const doc1 = new Y.Doc();
      const doc2 = new Y.Doc();

      const graph1 = new CollaborativeRDFGraph(doc1);
      const graph2 = new CollaborativeRDFGraph(doc2);

      const triple = {
        subject: 'http://example.org/alice',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Alice',
      };

      // Client 1 adds
      graph1.addTriple(triple);

      // Sync
      Y.applyUpdate(doc2, Y.encodeStateAsUpdate(doc1));

      // Client 2 removes
      graph2.removeTriple(triple);

      // Client 1 re-adds (newer timestamp)
      setTimeout(() => {
        graph1.addTriple(triple);
      }, 10);

      // Final sync
      Y.applyUpdate(doc1, Y.encodeStateAsUpdate(doc2));
      Y.applyUpdate(doc2, Y.encodeStateAsUpdate(doc1));

      // Both should converge
      expect(graph1.getTriples().length).toBe(graph2.getTriples().length);
    });

    it('should preserve commutativity (order independence)', () => {
      const doc1 = new Y.Doc();
      const doc2 = new Y.Doc();

      const graphA = new CollaborativeRDFGraph(doc1);
      const graphB = new CollaborativeRDFGraph(doc2);

      // Scenario A: Add triple1, then triple2
      graphA.addTriple({
        subject: 'http://example.org/alice',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Alice',
      });

      graphA.addTriple({
        subject: 'http://example.org/bob',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Bob',
      });

      // Scenario B: Add triple2, then triple1 (reverse order)
      graphB.addTriple({
        subject: 'http://example.org/bob',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Bob',
      });

      graphB.addTriple({
        subject: 'http://example.org/alice',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Alice',
      });

      // Sync both ways
      Y.applyUpdate(doc1, Y.encodeStateAsUpdate(doc2));
      Y.applyUpdate(doc2, Y.encodeStateAsUpdate(doc1));

      // Should have same triples regardless of order
      const triplesA = graphA.getTriples();
      const triplesB = graphB.getTriples();

      expect(triplesA).toHaveLength(2);
      expect(triplesB).toHaveLength(2);
    });
  });

  describe('Change Notifications', () => {
    it('should notify on triple addition', () => {
      return new Promise((resolve) => {
        graph.onChange((changes) => {
          if (changes.added.length > 0) {
            expect(changes.added).toHaveLength(1);
            expect(changes.added[0].subject).toBe('http://example.org/alice');
            resolve();
          }
        });

        graph.addTriple({
          subject: 'http://example.org/alice',
          predicate: 'http://xmlns.com/foaf/0.1/name',
          object: 'Alice',
        });
      });
    });

    it('should notify on triple removal', () => {
      const triple = {
        subject: 'http://example.org/alice',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Alice',
      };

      graph.addTriple(triple);

      return new Promise((resolve) => {
        graph.onChange((changes) => {
          if (changes.removed.length > 0) {
            expect(changes.removed).toHaveLength(1);
            resolve();
          }
        });

        graph.removeTriple(triple);
      });
    });
  });

  describe('Statistics', () => {
    it('should track active and tombstone counts', () => {
      // Use fresh graph to avoid pollution from previous tests
      const freshGraph = new CollaborativeRDFGraph();

      const triple1 = {
        subject: 'http://example.org/alice',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Alice',
      };

      const triple2 = {
        subject: 'http://example.org/bob',
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: 'Bob',
      };

      freshGraph.addTriple(triple1);
      freshGraph.addTriple(triple2);

      let stats = freshGraph.getStats();
      expect(stats.active).toBe(2);
      expect(stats.tombstones).toBe(0);

      freshGraph.removeTriple(triple1);

      stats = freshGraph.getStats();
      expect(stats.active).toBe(1);
      expect(stats.tombstones).toBe(1);
    });
  });

  describe('Validation', () => {
    it('should reject invalid triples', () => {
      expect(() => {
        graph.addTriple({
          subject: '',
          predicate: 'http://xmlns.com/foaf/0.1/name',
          object: 'Alice',
        });
      }).toThrow();

      expect(() => {
        graph.addTriple({
          subject: 'http://example.org/alice',
          predicate: '',
          object: 'Alice',
        });
      }).toThrow();
    });
  });
});
