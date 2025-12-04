<template>
  <div class="query-container">
    <h1>Query Integration Example</h1>

    <section class="query-editor">
      <h2>SPARQL Query Editor</h2>
      <div class="editor-controls">
        <button @click="loadSampleQuery('select')" class="btn-sample">SELECT Query</button>
        <button @click="loadSampleQuery('construct')" class="btn-sample">CONSTRUCT Query</button>
        <button @click="loadSampleQuery('ask')" class="btn-sample">ASK Query</button>
      </div>
      <textarea
        v-model="queryText"
        placeholder="Enter SPARQL query..."
        rows="10"
        class="query-textarea"
      ></textarea>
      <div class="query-actions">
        <button @click="executeQuery" :disabled="isExecuting" class="btn-execute">
          {{ isExecuting ? 'Executing...' : 'Execute Query' }}
        </button>
        <button @click="clearQuery" class="btn-clear">Clear</button>
      </div>
    </section>

    <section v-if="error" class="error-section">
      <h3>Error</h3>
      <pre>{{ error }}</pre>
    </section>

    <section v-if="results" class="results-section">
      <h2>Query Results</h2>
      <div class="results-meta">
        <span>Execution Time: {{ executionTime }}ms</span>
        <span v-if="results.type === 'bindings'">Results: {{ results.data.length }}</span>
      </div>

      <!-- SELECT results -->
      <div v-if="results.type === 'bindings'" class="results-table">
        <table v-if="results.data.length > 0">
          <thead>
            <tr>
              <th v-for="variable in results.variables" :key="variable">
                {{ variable }}
              </th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="(binding, index) in results.data" :key="index">
              <td v-for="variable in results.variables" :key="variable">
                {{ formatBinding(binding[variable]) }}
              </td>
            </tr>
          </tbody>
        </table>
        <div v-else class="empty-results">
          No results found
        </div>
      </div>

      <!-- ASK results -->
      <div v-if="results.type === 'boolean'" class="results-boolean">
        <div :class="['boolean-result', results.data ? 'true' : 'false']">
          {{ results.data ? 'TRUE' : 'FALSE' }}
        </div>
      </div>

      <!-- CONSTRUCT results -->
      <div v-if="results.type === 'quads'" class="results-quads">
        <table v-if="results.data.length > 0">
          <thead>
            <tr>
              <th>Subject</th>
              <th>Predicate</th>
              <th>Object</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="(quad, index) in results.data" :key="index">
              <td>{{ formatTerm(quad.subject) }}</td>
              <td>{{ formatTerm(quad.predicate) }}</td>
              <td>{{ formatTerm(quad.object) }}</td>
            </tr>
          </tbody>
        </table>
        <div v-else class="empty-results">
          No triples constructed
        </div>
      </div>
    </section>

    <section class="data-section">
      <h2>RDF Data Store</h2>
      <div class="data-stats">
        <span>Total Quads: {{ quadCount }}</span>
        <button @click="loadSampleData" class="btn-sample">Load Sample Data</button>
        <button @click="clearStore" class="btn-clear">Clear Store</button>
      </div>
      <div class="data-preview">
        <h3>Current Triples</h3>
        <table v-if="quads.length > 0">
          <thead>
            <tr>
              <th>Subject</th>
              <th>Predicate</th>
              <th>Object</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="(quad, index) in quads" :key="index">
              <td>{{ formatTerm(quad.subject) }}</td>
              <td>{{ formatTerm(quad.predicate) }}</td>
              <td>{{ formatTerm(quad.object) }}</td>
            </tr>
          </tbody>
        </table>
        <div v-else class="empty-state">
          No data in store. Load sample data to get started.
        </div>
      </div>
    </section>
  </div>
</template>

<script setup>
import { ref, computed } from 'vue';
import { useGraph, useQuery } from '@unrdf/composables';
import { DataFactory } from 'n3';

const { namedNode, literal } = DataFactory;

// Initialize reactive graph and query executor
const { store, quads, quadCount } = useGraph();
const { execute } = useQuery(store);

// Query state
const queryText = ref('');
const results = ref(null);
const error = ref(null);
const isExecuting = ref(false);
const executionTime = ref(0);

/**
 * Execute SPARQL query
 */
async function executeQuery() {
  if (!queryText.value.trim()) {
    error.value = 'Please enter a query';
    return;
  }

  isExecuting.value = true;
  error.value = null;
  results.value = null;

  const startTime = performance.now();

  try {
    const result = await execute(queryText.value);
    executionTime.value = Math.round(performance.now() - startTime);
    results.value = result;
  } catch (err) {
    error.value = err.message;
  } finally {
    isExecuting.value = false;
  }
}

/**
 * Clear query editor
 */
function clearQuery() {
  queryText.value = '';
  results.value = null;
  error.value = null;
}

/**
 * Load sample query
 * @param {'select' | 'construct' | 'ask'} type
 */
function loadSampleQuery(type) {
  const queries = {
    select: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/>

SELECT ?person ?name ?age
WHERE {
  ?person foaf:name ?name .
  OPTIONAL { ?person foaf:age ?age }
}
ORDER BY ?name`,

    construct: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/>

CONSTRUCT {
  ?person foaf:name ?name .
  ?person a foaf:Person .
}
WHERE {
  ?person foaf:name ?name .
}`,

    ask: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/>

ASK {
  ?person foaf:knows ?friend .
}`
  };

  queryText.value = queries[type];
  results.value = null;
  error.value = null;
}

/**
 * Load sample data into store
 */
function loadSampleData() {
  clearStore();

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
    ),
    DataFactory.quad(
      namedNode('http://example.org/bob'),
      namedNode('http://xmlns.com/foaf/0.1/age'),
      literal('25', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
    ),
    DataFactory.quad(
      namedNode('http://example.org/bob'),
      namedNode('http://xmlns.com/foaf/0.1/knows'),
      namedNode('http://example.org/charlie')
    ),
    DataFactory.quad(
      namedNode('http://example.org/charlie'),
      namedNode('http://xmlns.com/foaf/0.1/name'),
      literal('Charlie')
    )
  ];

  sampleQuads.forEach(quad => store.value.addQuad(quad));
}

/**
 * Clear all data from store
 */
function clearStore() {
  const allQuads = store.value.getQuads();
  allQuads.forEach(quad => store.value.removeQuad(quad));
  results.value = null;
}

/**
 * Format RDF term for display
 * @param {import('n3').Term} term
 * @returns {string}
 */
function formatTerm(term) {
  if (!term) return '';

  if (term.termType === 'NamedNode') {
    return term.value;
  }
  if (term.termType === 'Literal') {
    let result = `"${term.value}"`;
    if (term.language) {
      result += `@${term.language}`;
    } else if (term.datatype && term.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string') {
      result += `^^${term.datatype.value}`;
    }
    return result;
  }
  if (term.termType === 'BlankNode') {
    return `_:${term.value}`;
  }
  return term.value;
}

/**
 * Format binding value for display
 * @param {import('n3').Term | undefined} term
 * @returns {string}
 */
function formatBinding(term) {
  return term ? formatTerm(term) : '-';
}
</script>

<style scoped>
.query-container {
  max-width: 1400px;
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

h3 {
  color: #34495e;
  margin: 1rem 0 0.5rem;
  font-size: 1rem;
}

.editor-controls {
  display: flex;
  gap: 0.5rem;
  margin-bottom: 1rem;
  flex-wrap: wrap;
}

.query-textarea {
  width: 100%;
  padding: 1rem;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
  font-size: 0.9rem;
  line-height: 1.5;
  resize: vertical;
}

.query-actions {
  display: flex;
  gap: 0.5rem;
  margin-top: 1rem;
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

button:hover:not(:disabled) {
  background: #2980b9;
}

button:disabled {
  opacity: 0.5;
  cursor: not-allowed;
}

.btn-execute {
  background: #27ae60;
}

.btn-execute:hover:not(:disabled) {
  background: #229954;
}

.btn-clear {
  background: #e74c3c;
}

.btn-clear:hover {
  background: #c0392b;
}

.btn-sample {
  background: #9b59b6;
  font-size: 0.85rem;
  padding: 0.4rem 0.8rem;
}

.btn-sample:hover {
  background: #8e44ad;
}

.error-section {
  background: #fee;
  border-left: 4px solid #e74c3c;
}

.error-section pre {
  color: #c0392b;
  white-space: pre-wrap;
  word-wrap: break-word;
}

.results-meta {
  display: flex;
  gap: 2rem;
  margin-bottom: 1rem;
  color: #666;
  font-size: 0.9rem;
}

table {
  width: 100%;
  border-collapse: collapse;
  background: white;
  margin-top: 1rem;
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

.boolean-result {
  display: inline-block;
  padding: 2rem 4rem;
  border-radius: 8px;
  font-size: 2rem;
  font-weight: bold;
  margin: 1rem 0;
}

.boolean-result.true {
  background: #d5f4e6;
  color: #27ae60;
}

.boolean-result.false {
  background: #fadbd8;
  color: #e74c3c;
}

.data-stats {
  display: flex;
  gap: 1rem;
  align-items: center;
  margin-bottom: 1rem;
}

.data-stats span {
  color: #666;
}

.empty-state, .empty-results {
  text-align: center;
  color: #95a5a6;
  padding: 2rem;
  font-style: italic;
}

.data-preview {
  max-height: 400px;
  overflow-y: auto;
}
</style>
