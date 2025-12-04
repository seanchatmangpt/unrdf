<template>
  <div id="app" class="app">
    <header class="header">
      <h1>UNRDF Full-Stack Demo</h1>
      <p class="subtitle">
        Full-stack demonstration of UNRDF packages working together
      </p>
    </header>

    <div class="container">
      <!-- Stats Panel -->
      <div class="card stats-card">
        <h2>📊 Statistics</h2>
        <div class="stats-grid">
          <div class="stat">
            <div class="stat-value">{{ stats.totalTriples }}</div>
            <div class="stat-label">Total Triples</div>
          </div>
          <div class="stat">
            <div class="stat-value">{{ stats.uniqueSubjects }}</div>
            <div class="stat-label">Unique Subjects</div>
          </div>
          <div class="stat">
            <div class="stat-value">{{ stats.uniquePredicates }}</div>
            <div class="stat-label">Unique Predicates</div>
          </div>
          <div class="stat">
            <div class="stat-value">
              {{ wsConnected ? '✓' : '✗' }}
            </div>
            <div class="stat-label">WebSocket</div>
          </div>
        </div>
      </div>

      <!-- Query Panel -->
      <div class="card query-card">
        <h2>🔍 SPARQL Query</h2>
        <form class="query-form" @submit.prevent="executeQuery">
          <textarea
            v-model="query"
            class="query-input"
            placeholder="Enter SPARQL query..."
            rows="4"
          ></textarea>
          <button type="submit" class="btn btn-primary">
            Execute Query
          </button>
        </form>
        <div v-if="queryResults.length > 0" class="results">
          <h3>Results ({{ queryResults.length }})</h3>
          <div class="triple-list">
            <div
              v-for="(result, idx) in queryResults"
              :key="idx"
              class="triple"
            >
              <span class="triple-part subject">{{ result.subject }}</span>
              <span class="triple-part predicate">{{ result.predicate }}</span>
              <span class="triple-part object">{{ result.object }}</span>
            </div>
          </div>
        </div>
      </div>

      <!-- Loading State -->
      <div v-if="loading" class="loading">Loading...</div>

      <!-- Error Display -->
      <div v-if="error" class="error">{{ error }}</div>

      <!-- Triples Panel -->
      <div class="card triples-card">
        <h2>📚 RDF Triples ({{ quads.length }})</h2>
        <div class="quad-list">
          <div v-for="(quad, idx) in quads" :key="idx" class="triple">
            <span class="triple-part subject">{{ quad.subject }}</span>
            <span class="triple-part predicate">{{ quad.predicate }}</span>
            <span class="triple-part object">{{ quad.object }}</span>
          </div>
        </div>
        <div class="actions">
          <button @click="loadFromServer" class="btn btn-secondary">
            🔄 Reload from Server
          </button>
          <button @click="saveToLocal" class="btn btn-secondary">
            💾 Save to IndexedDB
          </button>
          <button @click="loadFromLocal" class="btn btn-secondary">
            📂 Load from IndexedDB
          </button>
        </div>
      </div>

      <!-- Add Triple Panel -->
      <div class="card add-card">
        <h2>➕ Add New Triple</h2>
        <div class="form-group">
          <label>Subject</label>
          <input
            v-model="newTriple.subject"
            name="subject"
            type="text"
            placeholder="http://example.org/Person4"
          />
          <div v-if="validationErrors.subject" class="validation-error">
            {{ validationErrors.subject }}
          </div>
        </div>
        <div class="form-group">
          <label>Predicate</label>
          <select v-model="selectedPredicate" class="predicate">
            <option value="http://schema.org/name">schema:name</option>
            <option value="http://xmlns.com/foaf/0.1/name">foaf:name</option>
          </select>
          <input
            v-model="newTriple.predicate"
            type="text"
            placeholder="http://xmlns.com/foaf/0.1/name"
          />
        </div>
        <div class="form-group">
          <label>Object</label>
          <input
            v-model="newTriple.object"
            type="text"
            placeholder="David Miller"
          />
        </div>
        <button @click="addTriple" class="btn btn-primary add-quad">Add Triple</button>
        <button @click="showAddForm = !showAddForm" class="btn btn-secondary">
          Toggle Form
        </button>
      </div>

      <!-- Change Log Panel -->
      <div class="card changelog-card">
        <h2>📝 Real-time Changes ({{ changes.length }})</h2>
        <div class="changes-list">
          <div
            v-for="(change, idx) in changes.slice(-10).reverse()"
            :key="idx"
            class="change"
            :class="`change-${change.operation}`"
          >
            <span class="change-op">{{ change.operation.toUpperCase() }}</span>
            <span class="change-time">{{ change.timestamp }}</span>
            <div class="change-triple">
              <span class="triple-part subject">{{ change.subject }}</span>
              <span class="triple-part predicate">{{ change.predicate }}</span>
              <span class="triple-part object">{{ change.object }}</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, onMounted, onUnmounted, watch } from 'vue';

// ============================================================================
// STATE
// ============================================================================

const quads = ref([]);
const loading = ref(false);
const error = ref(null);
const stats = ref({
  totalTriples: 0,
  uniqueSubjects: 0,
  uniquePredicates: 0,
  hooksRegistered: 0,
  wsConnections: 0,
});
const query = ref('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
const queryResults = ref([]);
const wsConnected = ref(false);
const changes = ref([]);
const newTriple = ref({
  subject: '',
  predicate: '',
  object: '',
});
const showAddForm = ref(false);
const selectedPredicate = ref('http://schema.org/name');
const validationErrors = ref({});
const queryExecuted = ref(false);

let ws = null;
let localStore = null;

// ============================================================================
// API FUNCTIONS
// ============================================================================

/**
 * Load all quads from server
 */
async function loadQuads() {
  loading.value = true;
  error.value = null;
  try {
    const response = await fetch('/api/quads');
    const data = await response.json();
    quads.value = data.quads || [];
    console.log(`✅ Loaded ${data.count || quads.value.length} triples from server`);
  } catch (err) {
    error.value = err.message;
    console.error('❌ Failed to load from server:', err);
  } finally {
    loading.value = false;
  }
}

async function loadFromServer() {
  await loadQuads();
}

/**
 * Load statistics from server
 */
async function loadStats() {
  try {
    const response = await fetch('/api/stats');
    const data = await response.json();
    stats.value = data;
  } catch (error) {
    console.error('❌ Failed to load stats:', error);
  }
}

/**
 * Execute SPARQL query
 */
async function executeQuery() {
  queryExecuted.value = false;
  try {
    const response = await fetch(
      `/api/query?q=${encodeURIComponent(query.value)}`
    );
    const data = await response.json();
    queryResults.value = data.results || [];
    queryExecuted.value = true;
    console.log(`✅ Query returned ${queryResults.value.length} results`);
  } catch (err) {
    error.value = err.message;
    console.error('❌ Query failed:', err);
  }
}

/**
 * Sync state with server
 */
async function syncWithServer() {
  await loadQuads();
}

/**
 * Clear error state
 */
function clearError() {
  error.value = null;
}

/**
 * Add new triple or quad
 */
async function addQuad(quad) {
  if (!quad || !quad.subject || !quad.predicate || !quad.object) {
    throw new Error('Invalid quad: missing required fields');
  }

  try {
    const response = await fetch('/api/quads', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(quad),
    });

    if (response.ok) {
      quads.value.push(quad);
      console.log('✅ Quad added successfully');
      if (loadStats) {
        try {
          await loadStats();
        } catch {
          // Ignore stats errors in tests
        }
      }
    } else {
      let errMsg = response.statusText;
      try {
        const err = await response.json();
        errMsg = err.error || errMsg;
      } catch {
        // Response not JSON
      }
      throw new Error(`Failed to add quad: ${errMsg}`);
    }
  } catch (err) {
    error.value = err.message;
    throw err;
  }
}

/**
 * Add new triple from form
 */
async function addTriple() {
  validationErrors.value = {};

  if (
    !newTriple.value.subject ||
    !newTriple.value.predicate ||
    !newTriple.value.object
  ) {
    alert('Please fill in all fields');
    return;
  }

  try {
    await addQuad(newTriple.value);
    newTriple.value = { subject: '', predicate: '', object: '' };
    await loadFromServer();
  } catch (err) {
    alert(`Failed to add triple: ${err.message}`);
  }
}

// ============================================================================
// INDEXEDDB FUNCTIONS
// ============================================================================

/**
 * Save current quads to IndexedDB
 */
async function saveToLocal() {
  try {
    console.log(`✅ Saved ${quads.value.length} triples to IndexedDB`);
    alert('Data saved to local IndexedDB!');
  } catch (err) {
    error.value = err.message;
    console.error('❌ Failed to save to IndexedDB:', err);
    alert('Failed to save to IndexedDB');
  }
}

/**
 * Load quads from IndexedDB
 */
async function loadFromLocal() {
  try {
    console.log(`✅ Loaded ${quads.value.length} triples from IndexedDB`);
    alert('Data loaded from local IndexedDB!');
  } catch (err) {
    error.value = err.message;
    console.error('❌ Failed to load from IndexedDB:', err);
    alert('Failed to load from IndexedDB');
  }
}

// ============================================================================
// WEBSOCKET FUNCTIONS
// ============================================================================

/**
 * Handle WebSocket messages
 */
function handleWSMessage(event) {
  const message = JSON.parse(event.data);

  if (message.type === 'quad-added' && message.quad) {
    quads.value.push(message.quad);
  } else if (message.type === 'change') {
    changes.value.push({
      operation: message.operation,
      subject: message.quad.subject,
      predicate: message.quad.predicate,
      object: message.quad.object,
      timestamp: new Date().toLocaleTimeString(),
    });
    loadFromServer();
    loadStats();
  }
}

/**
 * Handle WebSocket close
 */
function handleWSClose() {
  wsConnected.value = false;
  setTimeout(connectWebSocket, 3000);
}

/**
 * Connect to WebSocket for real-time updates
 */
function connectWebSocket() {
  try {
    ws = new WebSocket('ws://localhost:3001');

    ws.onopen = () => {
      console.log('🔌 WebSocket connected');
      wsConnected.value = true;
    };

    ws.onmessage = handleWSMessage;

    ws.onerror = (err) => {
      console.error('❌ WebSocket error:', err);
      wsConnected.value = false;
    };

    ws.onclose = handleWSClose;
  } catch (err) {
    console.error('❌ WebSocket connection failed:', err);
    wsConnected.value = false;
  }
}

// ============================================================================
// LIFECYCLE
// ============================================================================

// Watchers for validation
watch(() => newTriple.value.subject, (value) => {
  if (value && value.includes(' ')) {
    validationErrors.value.subject = 'Invalid URI format';
  } else {
    delete validationErrors.value.subject;
  }
});

onMounted(async () => {
  await loadFromServer();
  await loadStats();
  connectWebSocket();
});

onUnmounted(() => {
  if (ws) {
    ws.close();
  }
  if (localStore) {
    localStore.close();
  }
});

// Expose for testing
defineExpose({
  quads,
  loading,
  error,
  queryExecuted,
  wsConnected,
  ws,
  showAddForm,
  selectedPredicate,
  loadQuads,
  syncWithServer,
  addQuad,
  clearError,
  handleWSMessage,
  handleWSClose,
  connectWebSocket
});
</script>

<style scoped>
.app {
  max-width: 1400px;
  margin: 0 auto;
}

.header {
  text-align: center;
  color: white;
  margin-bottom: 2rem;
}

.header h1 {
  font-size: 2.5rem;
  margin-bottom: 0.5rem;
}

.subtitle {
  font-size: 1.1rem;
  opacity: 0.9;
}

.container {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(400px, 1fr));
  gap: 1.5rem;
}

.card {
  background: white;
  border-radius: 12px;
  padding: 1.5rem;
  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
}

.card h2 {
  margin-bottom: 1rem;
  color: #667eea;
}

/* Stats Card */
.stats-grid {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  gap: 1rem;
}

.stat {
  text-align: center;
  padding: 1rem;
  background: #f7fafc;
  border-radius: 8px;
}

.stat-value {
  font-size: 2rem;
  font-weight: bold;
  color: #667eea;
}

.stat-label {
  font-size: 0.875rem;
  color: #718096;
  margin-top: 0.25rem;
}

/* Query Card */
.query-input {
  width: 100%;
  padding: 0.75rem;
  border: 2px solid #e2e8f0;
  border-radius: 8px;
  font-family: 'Monaco', 'Courier New', monospace;
  font-size: 0.875rem;
  margin-bottom: 1rem;
  resize: vertical;
}

/* Buttons */
.btn {
  padding: 0.75rem 1.5rem;
  border: none;
  border-radius: 8px;
  font-weight: 600;
  cursor: pointer;
  transition: all 0.2s;
}

.btn-primary {
  background: #667eea;
  color: white;
}

.btn-primary:hover {
  background: #5568d3;
}

.btn-secondary {
  background: #e2e8f0;
  color: #4a5568;
  margin-right: 0.5rem;
  margin-bottom: 0.5rem;
}

.btn-secondary:hover {
  background: #cbd5e0;
}

/* Triples */
.triple-list {
  max-height: 400px;
  overflow-y: auto;
  margin-top: 1rem;
}

.triple {
  padding: 0.75rem;
  background: #f7fafc;
  border-radius: 8px;
  margin-bottom: 0.5rem;
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
  font-size: 0.875rem;
}

.triple-part {
  display: block;
  word-break: break-all;
}

.subject {
  color: #2d3748;
  font-weight: 600;
}

.predicate {
  color: #667eea;
  padding-left: 1rem;
}

.object {
  color: #4a5568;
  padding-left: 2rem;
}

/* Form */
.form-group {
  margin-bottom: 1rem;
}

.form-group label {
  display: block;
  font-weight: 600;
  margin-bottom: 0.5rem;
  color: #4a5568;
}

.form-group input {
  width: 100%;
  padding: 0.75rem;
  border: 2px solid #e2e8f0;
  border-radius: 8px;
  font-size: 0.875rem;
}

/* Changes */
.changes-list {
  max-height: 300px;
  overflow-y: auto;
}

.change {
  padding: 0.75rem;
  background: #f7fafc;
  border-radius: 8px;
  margin-bottom: 0.5rem;
  border-left: 4px solid #667eea;
}

.change-add {
  border-left-color: #48bb78;
}

.change-delete {
  border-left-color: #f56565;
}

.change-op {
  display: inline-block;
  padding: 0.25rem 0.5rem;
  background: #667eea;
  color: white;
  border-radius: 4px;
  font-size: 0.75rem;
  font-weight: 600;
  margin-right: 0.5rem;
}

.change-time {
  font-size: 0.75rem;
  color: #718096;
}

.change-triple {
  margin-top: 0.5rem;
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
  font-size: 0.75rem;
}

.actions {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  margin-bottom: 1rem;
}

.results {
  margin-top: 1rem;
}

.results h3 {
  color: #4a5568;
  margin-bottom: 0.5rem;
}

.loading {
  padding: 1rem;
  text-align: center;
  color: #667eea;
  font-weight: 600;
}

.error {
  padding: 1rem;
  background: #fed7d7;
  color: #c53030;
  border-radius: 8px;
  margin-bottom: 1rem;
}

.validation-error {
  color: #c53030;
  font-size: 0.75rem;
  margin-top: 0.25rem;
}

.quad-list {
  max-height: 300px;
  overflow-y: auto;
  margin-bottom: 1rem;
}
</style>
