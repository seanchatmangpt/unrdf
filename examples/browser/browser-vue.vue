<template>
  <!--
    UNRDF v3.1.0 Vue Example

    This example demonstrates using UNRDF in a Vue 3 application with:
    - Vue 3 Composition API
    - IndexedDB persistent storage
    - Web Worker query execution
    - Performance profiling

    To use:
    1. npm install unrdf vue
    2. Add to your bundler (Vite recommended)
    3. Configure browser entry point (see BROWSER-COMPATIBILITY.md)
  -->
  <div class="container">
    <h1>UNRDF v3.1.0 Vue Example</h1>
    <p class="subtitle">Vue 3 + IndexedDB + Web Workers</p>

    <!-- Loading State -->
    <div v-if="loading" class="card">
      <p>Initializing knowledge engine...</p>
    </div>

    <!-- Error State -->
    <div v-else-if="error" class="card error-card">
      <h2>Initialization Error</h2>
      <p>{{ error.message }}</p>
      <pre>{{ error.stack }}</pre>
    </div>

    <!-- Main UI -->
    <template v-else>
      <!-- Status Card -->
      <div class="card">
        <h2>System Status</h2>
        <div class="status">{{ status }}</div>
        <div class="stats">
          <div class="stat-card">
            <div class="stat-label">Engine Status</div>
            <div class="stat-value">Ready ✅</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Total Triples</div>
            <div class="stat-value">{{ tripleCount }}</div>
          </div>
        </div>
      </div>

      <!-- Actions Card -->
      <div class="card">
        <h2>Actions</h2>
        <div class="button-group">
          <button @click="handleAddData" class="btn-primary">
            Add Sample Data
          </button>
          <button @click="handleQuery" class="btn-primary">
            Run Query
          </button>
          <button @click="handleProfile" class="btn-primary">
            Show Performance
          </button>
          <button @click="handleClear" class="btn-danger">
            Clear All Data
          </button>
        </div>
      </div>

      <!-- Query Results -->
      <div v-if="queryResults" class="card">
        <h2>Query Results ({{ queryResults.length }} rows)</h2>
        <table class="results-table">
          <thead>
            <tr>
              <th v-for="key in resultKeys" :key="key">{{ key }}</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="(row, i) in queryResults" :key="i">
              <td v-for="key in resultKeys" :key="key">{{ row[key] }}</td>
            </tr>
          </tbody>
        </table>
      </div>

      <!-- Performance Profile -->
      <div v-if="profile" class="card">
        <h2>Performance Profile</h2>
        <div class="stats">
          <div class="stat-card">
            <div class="stat-label">p50 Latency</div>
            <div class="stat-value">{{ profile.latency.p50.toFixed(2) }}ms</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">p95 Latency</div>
            <div class="stat-value">{{ profile.latency.p95.toFixed(2) }}ms</div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Heap Used</div>
            <div class="stat-value">
              {{ (profile.memory.heapUsed / 1024 / 1024).toFixed(2) }}MB
            </div>
          </div>
          <div class="stat-card">
            <div class="stat-label">Cache Hit Rate</div>
            <div class="stat-value">
              {{ (profile.cache.hitRate * 100).toFixed(1) }}%
            </div>
          </div>
        </div>
      </div>
    </template>
  </div>
</template>

<script setup>
import { ref, computed, onMounted, onUnmounted } from 'vue';
import { createBrowserKnowledgeEngine, parseTurtle } from 'unrdf/browser';

// State
const engine = ref(null);
const loading = ref(true);
const error = ref(null);
const status = ref('Initializing...');
const tripleCount = ref(0);
const queryResults = ref(null);
const profile = ref(null);

// Computed
const resultKeys = computed(() => {
  if (!queryResults.value || queryResults.value.length === 0) return [];
  return Object.keys(queryResults.value[0]);
});

// Update stats
async function updateStats() {
  if (!engine.value) return;

  try {
    const countResult = await engine.value.query({
      query: 'SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }',
      type: 'sparql-select'
    });
    tripleCount.value = countResult[0]?.count || 0;
  } catch (err) {
    console.error('Failed to update stats:', err);
  }
}

// Add sample data
async function handleAddData() {
  if (!engine.value) return;

  status.value = 'Adding sample data...';

  try {
    const ttl = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:alice a foaf:Person ;
         foaf:name "Alice" ;
         foaf:age "30"^^xsd:integer ;
         foaf:knows ex:bob .

ex:bob a foaf:Person ;
       foaf:name "Bob" ;
       foaf:age "25"^^xsd:integer ;
       foaf:knows ex:alice .

ex:charlie a foaf:Person ;
           foaf:name "Charlie" ;
           foaf:age "35"^^xsd:integer .
    `;

    const store = await parseTurtle(ttl);

    await engine.value.executeTransaction({
      additions: [...store],
      removals: [],
      actor: 'vue-app'
    });

    status.value = `Added ${[...store].length} triples ✅`;
    await updateStats();
  } catch (err) {
    status.value = `Error: ${err.message}`;
    console.error(err);
  }
}

// Run query
async function handleQuery() {
  if (!engine.value) return;

  status.value = 'Running query...';

  try {
    const results = await engine.value.query({
      query: `
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?person ?name ?age
        WHERE {
          ?person a foaf:Person ;
                  foaf:name ?name ;
                  foaf:age ?age .
        }
        ORDER BY DESC(?age)
      `,
      type: 'sparql-select'
    });

    queryResults.value = results;
    status.value = `Query returned ${results.length} results ✅`;
  } catch (err) {
    status.value = `Query error: ${err.message}`;
    console.error(err);
  }
}

// Show performance profile
async function handleProfile() {
  if (!engine.value) return;

  status.value = 'Generating performance profile...';

  try {
    const perf = await engine.value.getPerformanceProfile();
    profile.value = perf;
    status.value = 'Performance profile ready ✅';
  } catch (err) {
    status.value = `Profiling error: ${err.message}`;
    console.error(err);
  }
}

// Clear all data
async function handleClear() {
  if (!engine.value) return;

  if (!window.confirm('Clear all data? This cannot be undone.')) {
    return;
  }

  status.value = 'Clearing all data...';

  try {
    await engine.value.clearAll();
    queryResults.value = null;
    profile.value = null;
    await updateStats();
    status.value = 'Data cleared ✅';
  } catch (err) {
    status.value = `Clear error: ${err.message}`;
    console.error(err);
  }
}

// Lifecycle
onMounted(async () => {
  try {
    engine.value = await createBrowserKnowledgeEngine({
      storage: {
        type: 'indexeddb',
        name: 'vue-demo-graph',
        quota: 100 * 1024 * 1024  // 100MB
      },
      workers: {
        enabled: true,
        maxWorkers: navigator.hardwareConcurrency || 4
      },
      profiling: {
        enabled: true,
        slowQueryThreshold: 100
      }
    });

    status.value = 'Ready ✅';
    loading.value = false;
    await updateStats();
  } catch (err) {
    error.value = err;
    loading.value = false;
  }
});

onUnmounted(() => {
  if (engine.value) {
    engine.value.close().catch(console.error);
  }
});
</script>

<style scoped>
.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 20px;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
}

.subtitle {
  color: #666;
  margin-bottom: 30px;
}

.card {
  background: white;
  border-radius: 8px;
  padding: 20px;
  margin-bottom: 20px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.error-card {
  background: #f8d7da;
  color: #721c24;
}

.error-card pre {
  background: #fff;
  padding: 10px;
  border-radius: 4px;
  overflow-x: auto;
  font-size: 12px;
}

h2 {
  color: #444;
  margin-bottom: 15px;
  font-size: 18px;
}

.status {
  padding: 10px;
  background: #d1ecf1;
  color: #0c5460;
  border-radius: 4px;
  margin-bottom: 15px;
}

.stats {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 15px;
}

.stat-card {
  background: #f8f9fa;
  padding: 15px;
  border-radius: 4px;
  border-left: 4px solid #007bff;
}

.stat-label {
  font-size: 12px;
  color: #666;
  text-transform: uppercase;
  margin-bottom: 5px;
}

.stat-value {
  font-size: 24px;
  font-weight: bold;
  color: #333;
}

.button-group {
  display: flex;
  gap: 10px;
  flex-wrap: wrap;
}

button {
  padding: 10px 20px;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-size: 14px;
  font-weight: 500;
  transition: all 0.2s;
}

.btn-primary {
  background: #007bff;
  color: white;
}

.btn-primary:hover {
  background: #0056b3;
  transform: translateY(-1px);
}

.btn-danger {
  background: #dc3545;
  color: white;
}

.btn-danger:hover {
  background: #c82333;
  transform: translateY(-1px);
}

.results-table {
  width: 100%;
  border-collapse: collapse;
  margin-top: 10px;
}

.results-table th,
.results-table td {
  padding: 10px;
  text-align: left;
  border-bottom: 1px solid #dee2e6;
}

.results-table th {
  background: #f8f9fa;
  font-weight: 600;
  color: #495057;
}

.results-table tr:hover {
  background: #f8f9fa;
}
</style>
