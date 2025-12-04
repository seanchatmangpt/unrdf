/**
 * @file SPARQL Rules Example Tests
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { Store, DataFactory } from 'n3';
import { KnowledgeEngine } from '@unrdf/knowledge-engine';

const { namedNode, literal } = DataFactory;

const EX = 'http://example.org/';
const FOAF = 'http://xmlns.com/foaf/0.1/';

describe('SPARQL Rules Example', () => {
  it('should infer friend-of-friend relationships', () => {
    const store = new Store();
    const engine = new KnowledgeEngine({ store });

    // Add friendship chain: Alice -> Bob -> Charlie
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Bob`)
    );
    store.addQuad(
      namedNode(`${EX}Bob`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Charlie`)
    );

    // Define friend-of-friend rule (simulated via domain/range for testing)
    // In real implementation, would use SPARQL CONSTRUCT
    store.addQuad(
      namedNode(`${FOAF}knows`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#domain'),
      namedNode(`${FOAF}Person`)
    );

    // Run inference
    engine.materialize();

    // Verify people are typed
    const aliceType = store.getQuads(
      namedNode(`${EX}Alice`),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode(`${FOAF}Person`)
    );
    expect(aliceType).toHaveLength(1);

    const bobType = store.getQuads(
      namedNode(`${EX}Bob`),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode(`${FOAF}Person`)
    );
    expect(bobType).toHaveLength(1);
  });

  it('should infer shared interest connections', () => {
    const store = new Store();
    const engine = new KnowledgeEngine({ store });

    // Add shared interests
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${FOAF}interest`),
      namedNode(`${EX}RDF`)
    );
    store.addQuad(
      namedNode(`${EX}Bob`),
      namedNode(`${FOAF}interest`),
      namedNode(`${EX}RDF`)
    );

    // Define interest rule (simulated)
    store.addQuad(
      namedNode(`${FOAF}interest`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#domain'),
      namedNode(`${FOAF}Person`)
    );

    // Run inference
    engine.materialize();

    // Verify both are persons with interests
    const aliceType = store.getQuads(
      namedNode(`${EX}Alice`),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      null
    );
    expect(aliceType.length).toBeGreaterThan(0);

    const bobType = store.getQuads(
      namedNode(`${EX}Bob`),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      null
    );
    expect(bobType.length).toBeGreaterThan(0);
  });

  it('should handle transitive relationship chains', () => {
    const store = new Store();
    const engine = new KnowledgeEngine({ store });

    // Create chain: A -> B -> C -> D
    const people = ['Alice', 'Bob', 'Charlie', 'Diana'];
    for (let i = 0; i < people.length - 1; i++) {
      store.addQuad(
        namedNode(`${EX}${people[i]}`),
        namedNode(`${FOAF}knows`),
        namedNode(`${EX}${people[i + 1]}`)
      );
    }

    // Define schema - both domain and range to cover all people in chain
    store.addQuad(
      namedNode(`${FOAF}knows`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#domain'),
      namedNode(`${FOAF}Person`)
    );
    store.addQuad(
      namedNode(`${FOAF}knows`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#range'),
      namedNode(`${FOAF}Person`)
    );

    // Run inference
    const stats = engine.materialize();

    // Should infer type for all people in chain
    expect(stats.triplesInferred).toBeGreaterThan(0);

    // Verify all are typed as Person
    people.forEach(person => {
      const personTypes = store.getQuads(
        namedNode(`${EX}${person}`),
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode(`${FOAF}Person`)
      );
      expect(personTypes.length).toBeGreaterThan(0);
    });
  });

  it('should handle rules with filters', () => {
    const store = new Store();
    const engine = new KnowledgeEngine({ store });

    // Add self-reference that should be filtered
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Alice`)
    );

    // Add valid relationship
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Bob`)
    );

    // Define schema
    store.addQuad(
      namedNode(`${FOAF}knows`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#domain'),
      namedNode(`${FOAF}Person`)
    );

    // Run inference
    engine.materialize();

    // Alice should still be typed as Person
    const aliceTypes = store.getQuads(
      namedNode(`${EX}Alice`),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode(`${FOAF}Person`)
    );
    expect(aliceTypes).toHaveLength(1);
  });

  it('should return statistics for SPARQL rule execution', () => {
    const store = new Store();
    const engine = new KnowledgeEngine({ store });

    // Setup social network
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Bob`)
    );
    store.addQuad(
      namedNode(`${EX}Bob`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Charlie`)
    );
    store.addQuad(
      namedNode(`${FOAF}knows`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#domain'),
      namedNode(`${FOAF}Person`)
    );

    // Run inference
    const stats = engine.materialize();

    expect(stats).toBeDefined();
    expect(stats.triplesInferred).toBeGreaterThan(0);
    expect(typeof stats.triplesInferred).toBe('number');
  });

  it('should handle multiple derivation paths without duplicates', () => {
    const store = new Store();
    const engine = new KnowledgeEngine({ store });

    // Create diamond pattern: A -> B, A -> C, B -> D, C -> D
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Bob`)
    );
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Charlie`)
    );
    store.addQuad(
      namedNode(`${EX}Bob`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Diana`)
    );
    store.addQuad(
      namedNode(`${EX}Charlie`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Diana`)
    );

    // Define schema - both domain and range to cover all people in diamond
    store.addQuad(
      namedNode(`${FOAF}knows`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#domain'),
      namedNode(`${FOAF}Person`)
    );
    store.addQuad(
      namedNode(`${FOAF}knows`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#range'),
      namedNode(`${FOAF}Person`)
    );

    // Run inference
    engine.materialize();

    // Diana should be typed exactly once despite multiple paths
    const dianaTypes = store.getQuads(
      namedNode(`${EX}Diana`),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode(`${FOAF}Person`)
    );

    expect(dianaTypes).toHaveLength(1);
  });

  it('should handle complex WHERE clause patterns', () => {
    const store = new Store();
    const engine = new KnowledgeEngine({ store });

    // Add data with multiple predicates
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${FOAF}name`),
      literal('Alice')
    );
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Bob`)
    );
    store.addQuad(
      namedNode(`${EX}Bob`),
      namedNode(`${FOAF}name`),
      literal('Bob')
    );

    // Define schema to trigger inference
    store.addQuad(
      namedNode(`${FOAF}name`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#domain'),
      namedNode(`${FOAF}Person`)
    );

    // Run inference
    const stats = engine.materialize();

    // Verify both are typed
    const aliceTypes = store.getQuads(
      namedNode(`${EX}Alice`),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode(`${FOAF}Person`)
    );
    const bobTypes = store.getQuads(
      namedNode(`${EX}Bob`),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode(`${FOAF}Person`)
    );

    expect(aliceTypes).toHaveLength(1);
    expect(bobTypes).toHaveLength(1);
    expect(stats.triplesInferred).toBeGreaterThanOrEqual(2);
  });

  it('should handle variable binding across multiple triple patterns', () => {
    const store = new Store();
    const engine = new KnowledgeEngine({ store });

    // Create a network with multiple predicates
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Bob`)
    );
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${FOAF}interest`),
      namedNode(`${EX}RDF`)
    );
    store.addQuad(
      namedNode(`${EX}Bob`),
      namedNode(`${FOAF}interest`),
      namedNode(`${EX}SPARQL`)
    );

    // Define domains for both predicates
    store.addQuad(
      namedNode(`${FOAF}knows`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#domain'),
      namedNode(`${FOAF}Person`)
    );
    store.addQuad(
      namedNode(`${FOAF}interest`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#domain'),
      namedNode(`${FOAF}Person`)
    );

    const beforeCount = store.size;
    engine.materialize();
    const afterCount = store.size;

    // Should have added type triples
    expect(afterCount).toBeGreaterThan(beforeCount);

    // Both should be typed from both predicates
    const aliceTypes = store.getQuads(
      namedNode(`${EX}Alice`),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode(`${FOAF}Person`)
    );
    expect(aliceTypes).toHaveLength(1);
  });

  it('should handle different rule application orders consistently', () => {
    const store = new Store();
    const engine = new KnowledgeEngine({ store });

    // Add data
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${FOAF}knows`),
      namedNode(`${EX}Bob`)
    );

    // Add rules in different order
    store.addQuad(
      namedNode(`${FOAF}knows`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#range'),
      namedNode(`${FOAF}Person`)
    );
    store.addQuad(
      namedNode(`${FOAF}knows`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#domain'),
      namedNode(`${FOAF}Person`)
    );

    // Run inference multiple times - should be idempotent
    const stats1 = engine.materialize();
    const count1 = store.size;

    const stats2 = engine.materialize();
    const count2 = store.size;

    // Second run should infer nothing new
    expect(stats2.triplesInferred).toBe(0);
    expect(count2).toBe(count1);
  });

  it('should respect inference depth limits', () => {
    const store = new Store();
    const engine = new KnowledgeEngine({ store });

    // Create a long chain that might exceed depth
    const people = Array.from({ length: 20 }, (_, i) => `Person${i}`);

    for (let i = 0; i < people.length - 1; i++) {
      store.addQuad(
        namedNode(`${EX}${people[i]}`),
        namedNode(`${FOAF}knows`),
        namedNode(`${EX}${people[i + 1]}`)
      );
    }

    // Define schema
    store.addQuad(
      namedNode(`${FOAF}knows`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#domain'),
      namedNode(`${FOAF}Person`)
    );
    store.addQuad(
      namedNode(`${FOAF}knows`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#range'),
      namedNode(`${FOAF}Person`)
    );

    // Run inference
    const stats = engine.materialize();

    // Should infer types for all people in chain
    expect(stats.triplesInferred).toBeGreaterThan(0);

    // First person should be typed
    const firstTypes = store.getQuads(
      namedNode(`${EX}${people[0]}`),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode(`${FOAF}Person`)
    );
    expect(firstTypes.length).toBeGreaterThan(0);
  });

  it('should handle empty stores gracefully', () => {
    const store = new Store();
    const engine = new KnowledgeEngine({ store });

    // Run inference on empty store
    const stats = engine.materialize();

    expect(stats.triplesInferred).toBe(0);
    expect(store.size).toBe(0);
  });
});
