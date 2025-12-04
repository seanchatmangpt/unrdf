/**
 * @file Basic Inference Example Tests
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { KnowledgeEngine } from '@unrdf/knowledge-engine';

const { namedNode } = dataFactory;

const EX = 'http://example.org/';
const RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
const RDFS = 'http://www.w3.org/2000/01/rdf-schema#';

describe('Basic Inference Example', () => {
  it('should infer types from domain constraints', () => {
    const store = createStore();
    const engine = new KnowledgeEngine({ store });

    // Define domain: ex:worksFor domain ex:Person
    store.addQuad(
      namedNode(`${EX}worksFor`),
      namedNode(`${RDFS}domain`),
      namedNode(`${EX}Person`)
    );

    // Add instance: ex:Alice ex:worksFor ex:AcmeCorp
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${EX}worksFor`),
      namedNode(`${EX}AcmeCorp`)
    );

    // Run inference
    engine.materialize();

    // Check inferred type: ex:Alice rdf:type ex:Person
    const aliceTypes = store.getQuads(
      namedNode(`${EX}Alice`),
      namedNode(`${RDF}type`),
      namedNode(`${EX}Person`)
    );

    expect(aliceTypes).toHaveLength(1);
    expect(aliceTypes[0].subject.value).toBe(`${EX}Alice`);
    expect(aliceTypes[0].object.value).toBe(`${EX}Person`);
  });

  it('should infer types from range constraints', () => {
    const store = createStore();
    const engine = new KnowledgeEngine({ store });

    // Define range: ex:worksFor range ex:Organization
    store.addQuad(
      namedNode(`${EX}worksFor`),
      namedNode(`${RDFS}range`),
      namedNode(`${EX}Organization`)
    );

    // Add instance: ex:Bob ex:worksFor ex:TechCo
    store.addQuad(
      namedNode(`${EX}Bob`),
      namedNode(`${EX}worksFor`),
      namedNode(`${EX}TechCo`)
    );

    // Run inference
    engine.materialize();

    // Check inferred type: ex:TechCo rdf:type ex:Organization
    const techCoTypes = store.getQuads(
      namedNode(`${EX}TechCo`),
      namedNode(`${RDF}type`),
      namedNode(`${EX}Organization`)
    );

    expect(techCoTypes).toHaveLength(1);
    expect(techCoTypes[0].subject.value).toBe(`${EX}TechCo`);
    expect(techCoTypes[0].object.value).toBe(`${EX}Organization`);
  });

  it('should infer triples from subPropertyOf relationships', () => {
    const store = createStore();
    const engine = new KnowledgeEngine({ store });

    // Define property hierarchy: ex:manages subPropertyOf ex:worksFor
    store.addQuad(
      namedNode(`${EX}manages`),
      namedNode(`${RDFS}subPropertyOf`),
      namedNode(`${EX}worksFor`)
    );

    // Add instance: ex:Alice ex:manages ex:AcmeCorp
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${EX}manages`),
      namedNode(`${EX}AcmeCorp`)
    );

    // Run inference
    engine.materialize();

    // Check inferred triple: ex:Alice ex:worksFor ex:AcmeCorp
    const aliceWorksFor = store.getQuads(
      namedNode(`${EX}Alice`),
      namedNode(`${EX}worksFor`),
      namedNode(`${EX}AcmeCorp`)
    );

    expect(aliceWorksFor).toHaveLength(1);
    expect(aliceWorksFor[0].predicate.value).toBe(`${EX}worksFor`);
  });

  it('should infer multiple types from combined domain/range rules', () => {
    const store = createStore();
    const engine = new KnowledgeEngine({ store });

    // Define schema
    store.addQuad(
      namedNode(`${EX}manages`),
      namedNode(`${RDFS}domain`),
      namedNode(`${EX}Manager`)
    );
    store.addQuad(
      namedNode(`${EX}manages`),
      namedNode(`${RDFS}range`),
      namedNode(`${EX}Company`)
    );
    store.addQuad(
      namedNode(`${EX}manages`),
      namedNode(`${RDFS}subPropertyOf`),
      namedNode(`${EX}worksFor`)
    );

    // Add instance
    store.addQuad(
      namedNode(`${EX}Charlie`),
      namedNode(`${EX}manages`),
      namedNode(`${EX}StartupCo`)
    );

    // Run inference
    engine.materialize();

    // Check Charlie is a Manager
    const charlieTypes = store.getQuads(
      namedNode(`${EX}Charlie`),
      namedNode(`${RDF}type`),
      null
    );
    expect(charlieTypes.length).toBeGreaterThanOrEqual(1);
    expect(charlieTypes.some(q => q.object.value === `${EX}Manager`)).toBe(true);

    // Check StartupCo is a Company
    const startupTypes = store.getQuads(
      namedNode(`${EX}StartupCo`),
      namedNode(`${RDF}type`),
      null
    );
    expect(startupTypes.length).toBeGreaterThanOrEqual(1);
    expect(startupTypes.some(q => q.object.value === `${EX}Company`)).toBe(true);

    // Check inferred worksFor relationship
    const charlieWorksFor = store.getQuads(
      namedNode(`${EX}Charlie`),
      namedNode(`${EX}worksFor`),
      namedNode(`${EX}StartupCo`)
    );
    expect(charlieWorksFor).toHaveLength(1);
  });

  it('should return inference statistics', () => {
    const store = createStore();
    const engine = new KnowledgeEngine({ store });

    // Define simple schema
    store.addQuad(
      namedNode(`${EX}knows`),
      namedNode(`${RDFS}domain`),
      namedNode(`${EX}Person`)
    );

    // Add instances
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${EX}knows`),
      namedNode(`${EX}Bob`)
    );

    // Run inference
    const stats = engine.materialize();

    expect(stats).toBeDefined();
    expect(stats.triplesInferred).toBeGreaterThan(0);
  });

  it('should infer domain types for multiple subjects', () => {
    const store = createStore();
    const engine = new KnowledgeEngine({ store });

    // Define domain
    store.addQuad(
      namedNode(`${EX}knows`),
      namedNode(`${RDFS}domain`),
      namedNode(`${EX}Person`)
    );

    // Add multiple subjects using the property
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${EX}knows`),
      namedNode(`${EX}Bob`)
    );
    store.addQuad(
      namedNode(`${EX}Charlie`),
      namedNode(`${EX}knows`),
      namedNode(`${EX}Diana`)
    );
    store.addQuad(
      namedNode(`${EX}Eve`),
      namedNode(`${EX}knows`),
      namedNode(`${EX}Frank`)
    );

    // Run inference
    engine.materialize();

    // All subjects should be inferred as Person
    const aliceType = store.getQuads(
      namedNode(`${EX}Alice`),
      namedNode(`${RDF}type`),
      namedNode(`${EX}Person`)
    );
    const charlieType = store.getQuads(
      namedNode(`${EX}Charlie`),
      namedNode(`${RDF}type`),
      namedNode(`${EX}Person`)
    );
    const eveType = store.getQuads(
      namedNode(`${EX}Eve`),
      namedNode(`${RDF}type`),
      namedNode(`${EX}Person`)
    );

    expect(aliceType).toHaveLength(1);
    expect(charlieType).toHaveLength(1);
    expect(eveType).toHaveLength(1);
  });

  it('should infer range types for multiple objects', () => {
    const store = createStore();
    const engine = new KnowledgeEngine({ store });

    // Define range
    store.addQuad(
      namedNode(`${EX}livesIn`),
      namedNode(`${RDFS}range`),
      namedNode(`${EX}City`)
    );

    // Add multiple objects
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${EX}livesIn`),
      namedNode(`${EX}NewYork`)
    );
    store.addQuad(
      namedNode(`${EX}Bob`),
      namedNode(`${EX}livesIn`),
      namedNode(`${EX}London`)
    );
    store.addQuad(
      namedNode(`${EX}Charlie`),
      namedNode(`${EX}livesIn`),
      namedNode(`${EX}Tokyo`)
    );

    // Run inference
    engine.materialize();

    // All objects should be inferred as City
    const nyType = store.getQuads(
      namedNode(`${EX}NewYork`),
      namedNode(`${RDF}type`),
      namedNode(`${EX}City`)
    );
    const londonType = store.getQuads(
      namedNode(`${EX}London`),
      namedNode(`${RDF}type`),
      namedNode(`${EX}City`)
    );
    const tokyoType = store.getQuads(
      namedNode(`${EX}Tokyo`),
      namedNode(`${RDF}type`),
      namedNode(`${EX}City`)
    );

    expect(nyType).toHaveLength(1);
    expect(londonType).toHaveLength(1);
    expect(tokyoType).toHaveLength(1);
  });

  it('should handle transitive subPropertyOf chains', () => {
    const store = createStore();
    const engine = new KnowledgeEngine({ store });

    // Define property hierarchy: manages -> supervises -> relatedTo
    store.addQuad(
      namedNode(`${EX}manages`),
      namedNode(`${RDFS}subPropertyOf`),
      namedNode(`${EX}supervises`)
    );
    store.addQuad(
      namedNode(`${EX}supervises`),
      namedNode(`${RDFS}subPropertyOf`),
      namedNode(`${EX}relatedTo`)
    );

    // Add instance
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${EX}manages`),
      namedNode(`${EX}Bob`)
    );

    // Run inference
    engine.materialize();

    // Check both levels of inference
    const aliceSupervises = store.getQuads(
      namedNode(`${EX}Alice`),
      namedNode(`${EX}supervises`),
      namedNode(`${EX}Bob`)
    );
    const aliceRelatedTo = store.getQuads(
      namedNode(`${EX}Alice`),
      namedNode(`${EX}relatedTo`),
      namedNode(`${EX}Bob`)
    );

    expect(aliceSupervises).toHaveLength(1);
    expect(aliceRelatedTo).toHaveLength(1);
  });

  it('should handle multiple properties on same subject', () => {
    const store = createStore();
    const engine = new KnowledgeEngine({ store });

    // Define domains for different properties
    store.addQuad(
      namedNode(`${EX}teaches`),
      namedNode(`${RDFS}domain`),
      namedNode(`${EX}Teacher`)
    );
    store.addQuad(
      namedNode(`${EX}researches`),
      namedNode(`${RDFS}domain`),
      namedNode(`${EX}Researcher`)
    );

    // Add subject with multiple properties
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${EX}teaches`),
      namedNode(`${EX}Math`)
    );
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${EX}researches`),
      namedNode(`${EX}AI`)
    );

    // Run inference
    engine.materialize();

    // Alice should have both types
    const aliceTypes = store.getQuads(
      namedNode(`${EX}Alice`),
      namedNode(`${RDF}type`),
      null
    );

    const typeValues = aliceTypes.map(q => q.object.value);
    expect(typeValues).toContain(`${EX}Teacher`);
    expect(typeValues).toContain(`${EX}Researcher`);
    expect(aliceTypes.length).toBeGreaterThanOrEqual(2);
  });

  it('should handle inference with blank nodes', () => {
    const store = createStore();
    const engine = new KnowledgeEngine({ store });
    const { blankNode } = dataFactory;

    // Define domain
    store.addQuad(
      namedNode(`${EX}hasPart`),
      namedNode(`${RDFS}domain`),
      namedNode(`${EX}Whole`)
    );

    // Add blank node as subject
    const bn = blankNode();
    store.addQuad(
      bn,
      namedNode(`${EX}hasPart`),
      namedNode(`${EX}Component1`)
    );

    // Run inference
    engine.materialize();

    // Blank node should have inferred type
    const bnTypes = store.getQuads(
      bn,
      namedNode(`${RDF}type`),
      namedNode(`${EX}Whole`)
    );

    expect(bnTypes).toHaveLength(1);
  });

  it('should handle multiple materialization cycles efficiently', () => {
    const store = createStore();
    const engine = new KnowledgeEngine({ store });

    // Define schema
    store.addQuad(
      namedNode(`${EX}knows`),
      namedNode(`${RDFS}domain`),
      namedNode(`${EX}Person`)
    );

    // Add data
    store.addQuad(
      namedNode(`${EX}Alice`),
      namedNode(`${EX}knows`),
      namedNode(`${EX}Bob`)
    );

    // First cycle
    const stats1 = engine.materialize();
    expect(stats1.triplesInferred).toBeGreaterThan(0);

    // Second cycle should not infer duplicates
    const stats2 = engine.materialize();
    expect(stats2.triplesInferred).toBe(0);

    // Add new data
    store.addQuad(
      namedNode(`${EX}Charlie`),
      namedNode(`${EX}knows`),
      namedNode(`${EX}Diana`)
    );

    // Third cycle should infer only new triples
    const stats3 = engine.materialize();
    expect(stats3.triplesInferred).toBeGreaterThan(0);
    expect(stats3.triplesInferred).toBeLessThan(stats1.triplesInferred * 2);
  });
});
