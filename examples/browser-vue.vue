<template>
  <div class="unrdf-demo">
    <header class="header">
      <h1>🌐 UNRDF Browser Demo</h1>
      <p class="subtitle">RDF Knowledge Graphs in Your Browser - Vue.js</p>
    </header>

    <div v-if="!isInitialized" class="loading">
      <h2>⏳ Initializing UNRDF...</h2>
    </div>

    <div v-else class="container">
      <!-- Error Display -->
      <div v-if="error" class="error"><strong>❌ Error:</strong> {{ error }}</div>

      <!-- Statistics -->
      <div class="stats">
        <div class="stat-card">
          <div class="stat-value">{{ stats.quads }}</div>
          <div class="stat-label">Quads Stored</div>
        </div>
        <div class="stat-card">
          <div class="stat-value">{{ stats.graphs }}</div>
          <div class="stat-label">Graphs</div>
        </div>
        <div class="stat-card">
          <div class="stat-value">{{ stats.commits }}</div>
          <div class="stat-label">Commits</div>
        </div>
      </div>

      <!-- Parse Section -->
      <section class="section">
        <h2>📝 Parse Turtle Data</h2>
        <textarea
          v-model="turtleInput"
          class="textarea"
          rows="8"
          placeholder="Enter Turtle RDF data..."
        ></textarea>
        <div class="controls">
          <button @click="handleParse" class="button">Parse and Store</button>
          <button @click="handleClear" class="button">Clear Store</button>
        </div>
      </section>

      <!-- Query Section -->
      <section class="section">
        <h2>🔍 SPARQL Query</h2>
        <textarea
          v-model="sparqlInput"
          class="textarea"
          rows="6"
          placeholder="Enter SPARQL query..."
        ></textarea>
        <div class="controls">
          <button @click="handleQuery" class="button">Execute Query</button>
          <button @click="loadSampleQueries('names')" class="button">Query Names</button>
          <button @click="loadSampleQueries('connections')" class="button">
            Query Connections
          </button>
        </div>

        <div v-if="queryResults" class="output">
          <p v-if="queryResults.loading">⏳ Running query...</p>

          <template v-else-if="queryResults.type === 'bindings'">
            <p class="success">✅ Found {{ queryResults.bindings.length }} results:</p>
            <div v-for="(binding, i) in queryResults.bindings" :key="i" class="result-row">
              <strong>Result {{ i + 1 }}:</strong>
              {{ formatBinding(binding) }}
            </div>
          </template>

          <p v-else-if="queryResults.type === 'boolean'" class="success">
            ✅ Result: {{ queryResults.value }}
          </p>

          <p v-else class="success">✅ Found {{ queryResults.quads.length }} quads</p>
        </div>
      </section>

      <!-- Lockchain Section -->
      <section class="section">
        <h2>🔗 Lockchain Audit Trail</h2>
        <div class="controls">
          <button @click="handleHistory" class="button">View History</button>
          <button @click="handleVerify" class="button">Verify Chain</button>
        </div>

        <div v-if="lockchainOutput" class="output">
          <template v-if="lockchainOutput.type === 'history'">
            <p v-if="lockchainOutput.data.length === 0">No commits yet</p>
            <template v-else>
              <p class="success">✅ {{ lockchainOutput.data.length }} recent commits:</p>
              <div v-for="(commit, i) in lockchainOutput.data" :key="i" class="result-row">
                <strong>#{{ i + 1 }}</strong>
                {{ commit.hash.substring(0, 8) }}... by <strong>{{ commit.author }}</strong> -
                {{ commit.message }}
                <small>({{ formatDate(commit.timestamp) }})</small>
              </div>
            </template>
          </template>

          <p v-else :class="lockchainOutput.data.valid ? 'success' : 'error'">
            {{ lockchainOutput.data.valid ? '✅' : '❌' }}
            {{ lockchainOutput.data.message }} ({{ lockchainOutput.data.commits }}
            commits)
          </p>
        </div>
      </section>
    </div>
  </div>
</template>

<script>
import { ref, onMounted, onUnmounted } from 'vue';
import { IndexedDBQuadStore } from '../src/browser/indexeddb-store.mjs';
import { BrowserQueryExecutor } from '../src/browser/comunica-browser-adapter.mjs';
import { BrowserLockchainWriter } from '../src/browser/browser-lockchain-writer.mjs';
import { Parser } from 'n3';

const SAMPLE_TURTLE = `@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .

ex:alice a foaf:Person ;
  foaf:name "Alice Wonderland" ;
  foaf:age 30 ;
  foaf:knows ex:bob .

ex:bob a foaf:Person ;
  foaf:name "Bob Builder" ;
  foaf:age 35 .

ex:charlie a foaf:Person ;
  foaf:name "Charlie Chaplin" ;
  foaf:age 40 ;
  foaf:knows ex:alice, ex:bob .`;

const SAMPLE_QUERY = `SELECT ?person ?name ?age WHERE {
  ?person a <http://xmlns.com/foaf/0.1/Person> ;
          <http://xmlns.com/foaf/0.1/name> ?name .
  OPTIONAL { ?person <http://xmlns.com/foaf/0.1/age> ?age }
}`;

export default {
  name: 'UnrdfDemo',

  setup() {
    const isInitialized = ref(false);
    const stats = ref({ quads: 0, graphs: 0, commits: 0 });
    const turtleInput = ref(SAMPLE_TURTLE);
    const sparqlInput = ref(SAMPLE_QUERY);
    const queryResults = ref(null);
    const lockchainOutput = ref(null);
    const error = ref(null);

    let store, queryExecutor, lockchain;

    const updateStats = async () => {
      if (!isInitialized.value) return;

      try {
        const quadCount = await store.size();
        const graphs = await store.getGraphs();
        const history = await lockchain.getHistory(Infinity);

        stats.value = {
          quads: quadCount,
          graphs: graphs.length,
          commits: history.length,
        };
      } catch (err) {
        console.error('Failed to update stats:', err);
      }
    };

    const handleParse = async () => {
      try {
        error.value = null;
        const parser = new Parser();
        const quads = parser.parse(turtleInput.value);

        await store.addQuads(quads);

        await lockchain.recordChange({
          type: 'add',
          data: turtleInput.value,
          author: 'vue-user',
          message: `Added ${quads.length} quads`,
        });

        await updateStats();
        alert(`✅ Parsed and stored ${quads.length} quads`);
      } catch (err) {
        error.value = 'Parse error: ' + err.message;
      }
    };

    const handleQuery = async () => {
      try {
        error.value = null;
        queryResults.value = { loading: true };

        const result = await queryExecutor.query(sparqlInput.value);
        queryResults.value = result;
      } catch (err) {
        error.value = 'Query error: ' + err.message;
        queryResults.value = null;
      }
    };

    const handleHistory = async () => {
      try {
        error.value = null;
        const history = await lockchain.getHistory(10);
        lockchainOutput.value = { type: 'history', data: history };
      } catch (err) {
        error.value = 'History error: ' + err.message;
      }
    };

    const handleVerify = async () => {
      try {
        error.value = null;
        const verification = await lockchain.verifyChain();
        lockchainOutput.value = { type: 'verify', data: verification };
      } catch (err) {
        error.value = 'Verification error: ' + err.message;
      }
    };

    const handleClear = async () => {
      if (confirm('Clear all data?')) {
        try {
          error.value = null;
          await store.clear();
          await updateStats();
          queryResults.value = null;
          lockchainOutput.value = null;
          alert('✅ Store cleared');
        } catch (err) {
          error.value = 'Clear error: ' + err.message;
        }
      }
    };

    const loadSampleQueries = type => {
      if (type === 'names') {
        sparqlInput.value = `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name WHERE {
  ?person foaf:name ?name
}`;
      } else if (type === 'connections') {
        sparqlInput.value = `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?person ?friend WHERE {
  ?person foaf:knows ?friend
}`;
      }
    };

    const formatBinding = binding => {
      return Object.entries(binding)
        .map(([k, v]) => `${k}: ${v}`)
        .join(', ');
    };

    const formatDate = timestamp => {
      return new Date(timestamp).toLocaleString();
    };

    onMounted(async () => {
      try {
        store = new IndexedDBQuadStore();
        queryExecutor = new BrowserQueryExecutor(store);
        lockchain = new BrowserLockchainWriter();

        await store.init();
        await queryExecutor.init();
        await lockchain.init();

        isInitialized.value = true;
        await updateStats();
      } catch (err) {
        console.error('Initialization failed:', err);
        error.value = 'Failed to initialize UNRDF: ' + err.message;
      }
    });

    onUnmounted(() => {
      if (store) store.close();
      if (queryExecutor) queryExecutor.close();
      if (lockchain) lockchain.close();
    });

    return {
      isInitialized,
      stats,
      turtleInput,
      sparqlInput,
      queryResults,
      lockchainOutput,
      error,
      handleParse,
      handleQuery,
      handleHistory,
      handleVerify,
      handleClear,
      loadSampleQueries,
      formatBinding,
      formatDate,
    };
  },
};
</script>

<style scoped>
.unrdf-demo {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  max-width: 1200px;
  margin: 0 auto;
  padding: 2rem;
}

.header {
  text-align: center;
  margin-bottom: 2rem;
  padding: 2rem;
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
  border-radius: 12px;
}

.subtitle {
  opacity: 0.9;
  font-size: 1.1rem;
  margin-top: 0.5rem;
}

.loading {
  text-align: center;
  padding: 4rem;
  color: #667eea;
}

.error {
  color: #dc3545;
  padding: 1rem;
  background: #ffe0e0;
  border-radius: 6px;
  margin-bottom: 1rem;
}

.stats {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
  margin-bottom: 2rem;
}

.stat-card {
  background: #f8f9fa;
  padding: 1.5rem;
  border-radius: 8px;
  text-align: center;
  border: 2px solid #e0e0e0;
}

.stat-value {
  font-size: 2rem;
  font-weight: bold;
  color: #667eea;
  margin-bottom: 0.5rem;
}

.stat-label {
  color: #666;
  font-size: 0.9rem;
}

.section {
  margin-bottom: 2rem;
  padding: 1.5rem;
  background: #f8f9fa;
  border-radius: 8px;
}

.textarea {
  width: 100%;
  padding: 1rem;
  border: 2px solid #e0e0e0;
  border-radius: 6px;
  font-family: 'Courier New', monospace;
  font-size: 0.9rem;
  margin-bottom: 1rem;
  resize: vertical;
}

.controls {
  display: flex;
  gap: 1rem;
  flex-wrap: wrap;
}

.button {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: white;
  border: none;
  padding: 0.75rem 1.5rem;
  border-radius: 6px;
  font-size: 1rem;
  cursor: pointer;
  transition: transform 0.2s;
}

.button:hover {
  transform: translateY(-2px);
}

.output {
  background: white;
  padding: 1rem;
  border-radius: 6px;
  border: 2px solid #e0e0e0;
  margin-top: 1rem;
  max-height: 400px;
  overflow-y: auto;
}

.result-row {
  padding: 0.5rem;
  margin-bottom: 0.5rem;
  background: #f8f9fa;
  border-radius: 4px;
  border-left: 3px solid #667eea;
}

.success {
  color: #28a745;
  font-weight: bold;
}
</style>
