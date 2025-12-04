/**
 * @file Basic Inference Example
 * @description Demonstrates RDFS domain/range inference and property chains
 */

import { Store, DataFactory } from 'n3';
import { KnowledgeEngine } from '@unrdf/knowledge-engine';

const { namedNode, literal } = DataFactory;

// Define namespaces
const EX = 'http://example.org/';
const RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
const RDFS = 'http://www.w3.org/2000/01/rdf-schema#';

/**
 * Create and configure knowledge engine with inference rules
 */
function createEngine() {
  const store = new Store();
  const engine = new KnowledgeEngine({ store });

  // Define RDFS schema - domain and range constraints
  store.addQuad(
    namedNode(`${EX}worksFor`),
    namedNode(`${RDFS}domain`),
    namedNode(`${EX}Person`)
  );
  store.addQuad(
    namedNode(`${EX}worksFor`),
    namedNode(`${RDFS}range`),
    namedNode(`${EX}Organization`)
  );

  // Define property hierarchy
  store.addQuad(
    namedNode(`${EX}manages`),
    namedNode(`${RDFS}subPropertyOf`),
    namedNode(`${EX}worksFor`)
  );

  console.log('📋 Schema defined:');
  console.log('  - ex:worksFor has domain ex:Person, range ex:Organization');
  console.log('  - ex:manages is subPropertyOf ex:worksFor');
  console.log();

  return { store, engine };
}

/**
 * Add instance data to the store
 */
function addInstanceData(store) {
  // Alice manages Acme Corp
  store.addQuad(
    namedNode(`${EX}Alice`),
    namedNode(`${EX}manages`),
    namedNode(`${EX}AcmeCorp`)
  );

  // Bob worksFor TechCo
  store.addQuad(
    namedNode(`${EX}Bob`),
    namedNode(`${EX}worksFor`),
    namedNode(`${EX}TechCo`)
  );

  console.log('📝 Instance data added:');
  console.log('  - ex:Alice ex:manages ex:AcmeCorp');
  console.log('  - ex:Bob ex:worksFor ex:TechCo');
  console.log();
}

/**
 * Run inference and display results
 */
function runInference(engine, store) {
  console.log('🔍 Running inference...');

  const stats = engine.materialize();

  console.log(`✅ Inference complete: ${stats.triplesInferred} new triples inferred`);
  console.log();

  // Query inferred type information
  const aliceTypes = store.getQuads(namedNode(`${EX}Alice`), namedNode(`${RDF}type`), null);
  const acmeTypes = store.getQuads(namedNode(`${EX}AcmeCorp`), namedNode(`${RDF}type`), null);
  const bobTypes = store.getQuads(namedNode(`${EX}Bob`), namedNode(`${RDF}type`), null);

  console.log('🎯 Inferred types:');
  aliceTypes.forEach(quad => {
    console.log(`  - ex:Alice is a ${quad.object.value.replace(EX, 'ex:')}`);
  });
  acmeTypes.forEach(quad => {
    console.log(`  - ex:AcmeCorp is a ${quad.object.value.replace(EX, 'ex:')}`);
  });
  bobTypes.forEach(quad => {
    console.log(`  - ex:Bob is a ${quad.object.value.replace(EX, 'ex:')}`);
  });
  console.log();

  // Query inferred worksFor relationships (from manages subproperty)
  const aliceWorksFor = store.getQuads(namedNode(`${EX}Alice`), namedNode(`${EX}worksFor`), null);
  console.log('🔗 Inferred relationships:');
  aliceWorksFor.forEach(quad => {
    console.log(`  - ex:Alice ex:worksFor ${quad.object.value.replace(EX, 'ex:')}`);
    console.log('    (inferred from ex:manages subPropertyOf ex:worksFor)');
  });
  console.log();
}

/**
 * Display all triples in store
 */
function displayAllTriples(store) {
  console.log('📊 All triples in store:');
  const allQuads = store.getQuads();
  allQuads.forEach(quad => {
    const s = quad.subject.value.replace(EX, 'ex:').replace(RDFS, 'rdfs:').replace(RDF, 'rdf:');
    const p = quad.predicate.value.replace(EX, 'ex:').replace(RDFS, 'rdfs:').replace(RDF, 'rdf:');
    const o = quad.object.value.replace(EX, 'ex:').replace(RDFS, 'rdfs:').replace(RDF, 'rdf:');
    console.log(`  ${s} ${p} ${o}`);
  });
  console.log();
  console.log(`Total triples: ${allQuads.length}`);
}

/**
 * Main execution
 */
export function runExample() {
  console.log('🚀 UNRDF Knowledge Engine - Basic Inference Example\n');
  console.log('=' .repeat(60));
  console.log();

  const { store, engine } = createEngine();
  addInstanceData(store);
  runInference(engine, store);
  displayAllTriples(store);

  console.log('=' .repeat(60));
  console.log('✨ Example complete!');
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runExample();
}
