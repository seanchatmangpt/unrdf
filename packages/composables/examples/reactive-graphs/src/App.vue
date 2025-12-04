<template>
  <div class="app-container">
    <h1>Reactive Graphs Example</h1>

    <section class="controls">
      <h2>Add Triple</h2>
      <div class="form-group">
        <input v-model="newTriple.subject" placeholder="Subject" />
        <input v-model="newTriple.predicate" placeholder="Predicate" />
        <input v-model="newTriple.object" placeholder="Object" />
        <button @click="addTriple">Add Triple</button>
      </div>
    </section>

    <section class="stats">
      <h2>Graph Statistics</h2>
      <p>Total Quads: {{ quadCount }}</p>
      <p>Subjects: {{ subjects.length }}</p>
      <p>Predicates: {{ predicates.length }}</p>
    </section>

    <section class="quads-list">
      <h2>Quads in Store</h2>
      <div v-if="quads.length === 0" class="empty-state">
        No quads in store. Add some triples above!
      </div>
      <table v-else>
        <thead>
          <tr>
            <th>Subject</th>
            <th>Predicate</th>
            <th>Object</th>
            <th>Actions</th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="(quad, index) in quads" :key="index">
            <td>{{ formatTerm(quad.subject) }}</td>
            <td>{{ formatTerm(quad.predicate) }}</td>
            <td>{{ formatTerm(quad.object) }}</td>
            <td>
              <button @click="removeQuad(quad)" class="btn-remove">Remove</button>
            </td>
          </tr>
        </tbody>
      </table>
    </section>

    <section class="actions">
      <button @click="clearStore" class="btn-clear">Clear All</button>
      <button @click="loadSampleData" class="btn-sample">Load Sample Data</button>
    </section>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue';
import { useGraph } from '@unrdf/composables';
import { DataFactory } from 'n3';

const { namedNode, literal } = DataFactory;

// Initialize reactive graph
const { store, quads, quadCount, subjects, predicates, objects } = useGraph();

// Form state for adding new triples
const newTriple = ref({
  subject: '',
  predicate: '',
  object: ''
});

/**
 * Add a new triple to the store
 */
function addTriple() {
  const { subject, predicate, object } = newTriple.value;

  if (!subject || !predicate || !object) {
    alert('Please fill in all fields');
    return;
  }

  try {
    const quad = DataFactory.quad(
      namedNode(subject),
      namedNode(predicate),
      isUri(object) ? namedNode(object) : literal(object)
    );

    store.value.addQuad(quad);

    // Clear form
    newTriple.value = { subject: '', predicate: '', object: '' };
  } catch (error) {
    alert(`Error adding triple: ${error.message}`);
  }
}

/**
 * Remove a quad from the store
 * @param {import('n3').Quad} quad
 */
function removeQuad(quad) {
  store.value.removeQuad(quad);
}

/**
 * Clear all quads from the store
 */
function clearStore() {
  const allQuads = store.value.getQuads();
  allQuads.forEach(quad => store.value.removeQuad(quad));
}

/**
 * Load sample data into the store
 */
function loadSampleData() {
  const sampleQuads = [
    DataFactory.quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Alice')
    ),
    DataFactory.quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/age'),
      literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    ),
    DataFactory.quad(
      namedNode('http://example.org/alice'),
      namedNode('http://xmlns.com/foaf/0.1/knows'),
      namedNode('http://example.org/bob')
    ),
    DataFactory.quad(
      namedNode('http://example.org/bob'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Bob')
    )
  ];

  sampleQuads.forEach(quad => store.value.addQuad(quad));
}

/**
 * Format RDF term for display
 * @param {import('n3').Term} term
 * @returns {string}
 */
function formatTerm(term) {
  if (term.termType === 'NamedNode') {
    return term.value;
  }
  if (term.termType === 'Literal') {
    return `"${term.value}"${term.datatype && term.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string' ? `^^${term.datatype.value}` : ''}`;
  }
  return term.value;
}

/**
 * Check if string is a URI
 * @param {string} str
 * @returns {boolean}
 */
function isUri(str) {
  try {
    new URL(str);
    return true;
  } catch {
    return false;
  }
}
</script>

<style scoped>
.app-container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 2rem;
  font-family: system-ui, -apple-system, sans-serif;
}

h1 {
  color: #2c3e50;
  margin-bottom: 2rem;
}

section {
  margin-bottom: 2rem;
  padding: 1.5rem;
  background: #f8f9fa;
  border-radius: 8px;
}

h2 {
  color: #34495e;
  margin-bottom: 1rem;
  font-size: 1.2rem;
}

.form-group {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

input {
  flex: 1;
  min-width: 200px;
  padding: 0.5rem;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 0.9rem;
}

button {
  padding: 0.5rem 1rem;
  background: #3498db;
  color: white;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.9rem;
  transition: background 0.2s;
}

button:hover {
  background: #2980b9;
}

.btn-remove {
  background: #e74c3c;
  padding: 0.25rem 0.75rem;
  font-size: 0.85rem;
}

.btn-remove:hover {
  background: #c0392b;
}

.btn-clear {
  background: #e74c3c;
}

.btn-clear:hover {
  background: #c0392b;
}

.btn-sample {
  background: #27ae60;
}

.btn-sample:hover {
  background: #229954;
}

.stats p {
  margin: 0.5rem 0;
  color: #555;
}

table {
  width: 100%;
  border-collapse: collapse;
  background: white;
}

thead {
  background: #34495e;
  color: white;
}

th, td {
  padding: 0.75rem;
  text-align: left;
  border-bottom: 1px solid #ddd;
}

th {
  font-weight: 600;
}

tbody tr:hover {
  background: #f8f9fa;
}

.empty-state {
  text-align: center;
  color: #95a5a6;
  padding: 2rem;
  font-style: italic;
}

.actions {
  display: flex;
  gap: 1rem;
}
</style>
