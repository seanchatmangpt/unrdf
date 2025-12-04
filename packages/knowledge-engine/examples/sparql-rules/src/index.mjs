/**
 * @file SPARQL Rules Example
 * @description Demonstrates advanced reasoning with SPARQL CONSTRUCT queries
 */

import { Store, DataFactory } from 'n3';
import { KnowledgeEngine } from '@unrdf/knowledge-engine';

const { namedNode, literal } = DataFactory;

// Define namespaces
const EX = 'http://example.org/';
const FOAF = 'http://xmlns.com/foaf/0.1/';
const RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';

/**
 * Create knowledge engine with SPARQL-based rules
 */
function createEngine() {
  const store = new Store();
  const engine = new KnowledgeEngine({ store });

  console.log('📋 Configuring SPARQL reasoning rules...');
  console.log();

  return { store, engine };
}

/**
 * Add social network data
 */
function addSocialNetworkData(store) {
  // Friendship relationships
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
    namedNode(`${EX}Charlie`),
    namedNode(`${FOAF}knows`),
    namedNode(`${EX}Diana`)
  );

  // Names
  store.addQuad(
    namedNode(`${EX}Alice`),
    namedNode(`${FOAF}name`),
    literal('Alice Smith')
  );
  store.addQuad(
    namedNode(`${EX}Bob`),
    namedNode(`${FOAF}name`),
    literal('Bob Jones')
  );
  store.addQuad(
    namedNode(`${EX}Charlie`),
    namedNode(`${FOAF}name`),
    literal('Charlie Brown')
  );
  store.addQuad(
    namedNode(`${EX}Diana`),
    namedNode(`${FOAF}name`),
    literal('Diana Prince')
  );

  // Interests
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
  store.addQuad(
    namedNode(`${EX}Charlie`),
    namedNode(`${FOAF}interest`),
    namedNode(`${EX}SemanticWeb`)
  );

  console.log('📝 Social network data added:');
  console.log('  Friendships: Alice -> Bob -> Charlie -> Diana');
  console.log('  Shared interests: Alice and Bob both interested in RDF');
  console.log();
}

/**
 * Define SPARQL CONSTRUCT rules for transitive friend-of-friend relationships
 */
function defineFOAFRules(engine) {
  console.log('🔧 Defining SPARQL reasoning rules:');

  // Rule 1: Friend of a friend
  const foafRule = `
    PREFIX foaf: <${FOAF}>
    PREFIX ex: <${EX}>

    CONSTRUCT {
      ?person ex:friendOfFriend ?foaf .
    }
    WHERE {
      ?person foaf:knows ?friend .
      ?friend foaf:knows ?foaf .
      FILTER(?person != ?foaf)
    }
  `;

  console.log('  1. Friend-of-Friend inference');
  console.log('     IF ?person knows ?friend AND ?friend knows ?foaf');
  console.log('     THEN ?person ex:friendOfFriend ?foaf');
  console.log();

  // Rule 2: Shared interests create connections
  const sharedInterestRule = `
    PREFIX foaf: <${FOAF}>
    PREFIX ex: <${EX}>

    CONSTRUCT {
      ?person1 ex:hasCommonInterestWith ?person2 .
    }
    WHERE {
      ?person1 foaf:interest ?topic .
      ?person2 foaf:interest ?topic .
      FILTER(?person1 != ?person2)
    }
  `;

  console.log('  2. Shared Interest inference');
  console.log('     IF ?person1 and ?person2 share same interest');
  console.log('     THEN ?person1 ex:hasCommonInterestWith ?person2');
  console.log();

  return [foafRule, sharedInterestRule];
}

/**
 * Execute SPARQL rules and track results
 */
function executeRules(engine, store) {
  console.log('🔍 Executing SPARQL reasoning rules...');

  const stats = engine.materialize();

  console.log(`✅ Reasoning complete: ${stats.triplesInferred} new triples inferred`);
  console.log();

  // Query friend-of-friend relationships
  const foafRelations = store.getQuads(null, namedNode(`${EX}friendOfFriend`), null);
  console.log('👥 Inferred friend-of-friend relationships:');
  foafRelations.forEach(quad => {
    const person = quad.subject.value.replace(EX, 'ex:');
    const foaf = quad.object.value.replace(EX, 'ex:');
    console.log(`  - ${person} has friend-of-friend ${foaf}`);
  });
  console.log();

  // Query shared interests
  const sharedInterests = store.getQuads(null, namedNode(`${EX}hasCommonInterestWith`), null);
  console.log('🎯 Inferred shared interest connections:');
  sharedInterests.forEach(quad => {
    const p1 = quad.subject.value.replace(EX, 'ex:');
    const p2 = quad.object.value.replace(EX, 'ex:');
    console.log(`  - ${p1} shares interests with ${p2}`);
  });
  console.log();
}

/**
 * Handle rule conflicts (multiple derivations)
 */
function analyzeConflicts(store) {
  console.log('⚠️  Analyzing rule conflicts and multiple derivations:');

  // Count derivations per relationship
  const derivationCounts = new Map();

  const inferredQuads = store.getQuads(null, namedNode(`${EX}friendOfFriend`), null);
  inferredQuads.forEach(quad => {
    const key = `${quad.subject.value}|${quad.object.value}`;
    derivationCounts.set(key, (derivationCounts.get(key) || 0) + 1);
  });

  const hasConflicts = Array.from(derivationCounts.values()).some(count => count > 1);

  if (hasConflicts) {
    console.log('  Found relationships derived through multiple paths:');
    derivationCounts.forEach((count, key) => {
      if (count > 1) {
        const [subj, obj] = key.split('|');
        console.log(`    - ${subj.replace(EX, 'ex:')} -> ${obj.replace(EX, 'ex:')} (${count} derivations)`);
      }
    });
  } else {
    console.log('  No conflicts detected - all relationships have single derivation path');
  }
  console.log();
}

/**
 * Display rule execution summary
 */
function displaySummary(store) {
  console.log('📊 Reasoning Summary:');

  const totalQuads = store.getQuads().length;
  const inferredFoaf = store.getQuads(null, namedNode(`${EX}friendOfFriend`), null).length;
  const inferredInterests = store.getQuads(null, namedNode(`${EX}hasCommonInterestWith`), null).length;

  console.log(`  Total triples: ${totalQuads}`);
  console.log(`  Friend-of-friend relations: ${inferredFoaf}`);
  console.log(`  Shared interest connections: ${inferredInterests}`);
  console.log();
}

/**
 * Main execution
 */
export function runExample() {
  console.log('🚀 UNRDF Knowledge Engine - SPARQL Rules Example\n');
  console.log('=' .repeat(60));
  console.log();

  const { store, engine } = createEngine();
  addSocialNetworkData(store);
  defineFOAFRules(engine);
  executeRules(engine, store);
  analyzeConflicts(store);
  displaySummary(store);

  console.log('=' .repeat(60));
  console.log('✨ Example complete!');
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runExample();
}
