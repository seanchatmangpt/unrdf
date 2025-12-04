/**
 * @file Basic Inference Example Tests
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { Store, DataFactory } from 'n3';
import { KnowledgeEngine } from '@unrdf/knowledge-engine';

const { namedNode } = DataFactory;

const EX = 'http://example.org/';
const RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
const RDFS = 'http://www.w3.org/2000/01/rdf-schema#';

describe('Basic Inference Example', () => {
  it('should infer types from domain constraints', () => {
    const store = new Store();
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
    const store = new Store();
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
    const store = new Store();
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
    const store = new Store();
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
    const store = new Store();
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
});
