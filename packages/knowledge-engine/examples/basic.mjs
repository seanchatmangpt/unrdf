/**
 * @unrdf/knowledge-engine - Basic Example
 *
 * Demonstrates rule-based inference with RDFS and custom rules.
 */

import { createStore, addQuad, quad, namedNode, literal, getQuads } from '@unrdf/core';
import {
  defineRule,
  createInferenceEngine,
  addRules,
  runInference,
  getInferredQuads,
  getBuiltinRules,
  getRDFSRules,
} from '../src/index.mjs';

console.log('🧠 @unrdf/knowledge-engine - Basic Inference Example\n');

// Create an RDF store
const store = createStore();

// Add some base facts
console.log('📊 Adding base facts...');

// Define class hierarchy
addQuad(store, quad(
  namedNode('http://example.org/Student'),
  namedNode('http://www.w3.org/2000/01/rdf-schema#subClassOf'),
  namedNode('http://xmlns.com/foaf/0.1/Person')
));

addQuad(store, quad(
  namedNode('http://example.org/Undergraduate'),
  namedNode('http://www.w3.org/2000/01/rdf-schema#subClassOf'),
  namedNode('http://example.org/Student')
));

// Add instances
addQuad(store, quad(
  namedNode('http://example.org/alice'),
  namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  namedNode('http://example.org/Undergraduate')
));

addQuad(store, quad(
  namedNode('http://example.org/alice'),
  namedNode('http://xmlns.com/foaf/0.1/name'),
  literal('Alice')
));

addQuad(store, quad(
  namedNode('http://example.org/bob'),
  namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  namedNode('http://example.org/Student')
));

console.log('  ✓ Added class hierarchy (Undergraduate → Student → Person)');
console.log('  ✓ Added instances (Alice: Undergraduate, Bob: Student)\n');

// Create inference engine
console.log('⚙️  Creating inference engine...');
const engine = createInferenceEngine(store);

// Add RDFS rules
const rdfsRules = getRDFSRules();
addRules(engine, rdfsRules);
console.log(`  ✓ Loaded ${rdfsRules.length} RDFS inference rules\n`);

// Define a custom rule: Students get a student label
console.log('📝 Defining custom rule...');
const studentLabelRule = defineRule({
  name: 'student-label',
  description: 'Add label to students',
  pattern: {
    subject: '?student',
    predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    object: 'http://example.org/Student',
  },
  consequent: {
    subject: '?student',
    predicate: 'http://www.w3.org/2000/01/rdf-schema#label',
    object: literal('Student'),
  },
  salience: 60,
});

addRules(engine, [studentLabelRule]);
console.log('  ✓ Added custom "student-label" rule\n');

// Run inference
console.log('🚀 Running forward-chaining inference...');
const initialQuadCount = store.size;
const results = runInference(engine);

console.log(`\n📈 Inference Results:`);
console.log(`  • Iterations: ${results.iterations}`);
console.log(`  • New facts inferred: ${results.inferredCount}`);
console.log(`  • Fixpoint reached: ${results.fixpointReached ? 'Yes' : 'No'}`);
console.log(`  • Total quads: ${initialQuadCount} → ${store.size}\n`);

// Display inferred facts
const inferred = getInferredQuads(engine);
console.log('✨ Inferred Facts:');

for (const inferredQuad of inferred) {
  const s = inferredQuad.subject.value;
  const p = inferredQuad.predicate.value;
  const o = inferredQuad.object.value;

  const subjectLabel = s.split('/').pop() || s;
  const predicateLabel = p.split('#').pop() || p.split('/').pop() || p;
  const objectLabel = o.split('#').pop() || o.split('/').pop() || o;

  console.log(`  • ${subjectLabel} --${predicateLabel}--> ${objectLabel}`);
}

// Query for all types of Alice
console.log('\n🔍 Alice\'s inferred types:');
const aliceTypes = getQuads(
  store,
  namedNode('http://example.org/alice'),
  namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  null,
  null
);

for (const typeQuad of aliceTypes) {
  const typeLabel = typeQuad.object.value.split('/').pop();
  console.log(`  • ${typeLabel}`);
}

// Query for all students
console.log('\n👥 All Students (via inference):');
const allStudents = getQuads(
  store,
  null,
  namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
  namedNode('http://example.org/Student'),
  null
);

for (const studentQuad of allStudents) {
  const studentLabel = studentQuad.subject.value.split('/').pop();
  console.log(`  • ${studentLabel}`);
}

console.log('\n✅ Example complete!\n');
