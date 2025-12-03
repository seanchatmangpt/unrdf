/**
 * @unrdf/composables - Basic Example
 *
 * Demonstrates Vue 3 composables for reactive RDF.
 * This example shows a complete Vue 3 component using UNRDF composables.
 */

import { createApp, ref, watch } from 'vue';
import { useGraph, useQuery, useDelta, useTerms } from '@unrdf/composables';

/**
 * Example Vue 3 Component using UNRDF Composables
 */
const RDFGraphComponent = {
  name: 'RDFGraphComponent',

  setup() {
    // 1. Create reactive RDF graph
    const { store, loading, error, quads, refresh } = useGraph(null, {
      enableIndexedDB: false,
    });

    // 2. Create SPARQL query handler
    const sparqlQuery = ref('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
    const { results, execute, clear } = useQuery(store, sparqlQuery.value, {
      memoize: true,
    });

    // 3. Setup undo/redo for graph changes
    const { push, undo, redo, canUndo, canRedo } = useDelta(store, {
      maxDepth: 50,
    });

    // 4. Get RDF term factory
    const { namedNode, literal, quad } = useTerms();

    // 5. Watch for graph changes
    watch(quads, (newQuads) => {
      console.log(`Graph updated: ${newQuads.length} quads`);
    });

    // 6. Example: Add a triple to the graph
    function addTriple() {
      if (!store.value) return;

      const subject = namedNode('http://example.org/person/1');
      const predicate = namedNode('http://xmlns.com/foaf/0.1/name');
      const object = literal('Alice', 'en');
      const triple = quad(subject, predicate, object);

      // Track change for undo
      push({
        additions: [triple],
        deletions: [],
      });

      // Add to store
      store.value.add(triple);

      console.log('Added triple:', triple);
    }

    // 7. Example: Execute SPARQL query
    async function runQuery() {
      await execute();
      console.log('Query results:', results.value);
    }

    // 8. Example: Undo last change
    function undoLastChange() {
      if (canUndo.value) {
        undo();
        console.log('Undone last change');
      }
    }

    // 9. Example: Redo last undone change
    function redoLastChange() {
      if (canRedo.value) {
        redo();
        console.log('Redone change');
      }
    }

    return {
      // Reactive state
      store,
      loading,
      error,
      quads,
      results,
      canUndo,
      canRedo,
      sparqlQuery,

      // Methods
      addTriple,
      runQuery,
      undoLastChange,
      redoLastChange,
      refresh,
      clear,
    };
  },

  template: `
    <div class="rdf-graph-component">
      <h2>UNRDF Composables Example</h2>

      <!-- Loading State -->
      <div v-if="loading" class="loading">Loading graph...</div>

      <!-- Error State -->
      <div v-if="error" class="error">Error: {{ error.message }}</div>

      <!-- Graph Stats -->
      <div class="stats">
        <p>Total Quads: {{ quads.length }}</p>
        <p>Can Undo: {{ canUndo }}</p>
        <p>Can Redo: {{ canRedo }}</p>
      </div>

      <!-- Actions -->
      <div class="actions">
        <button @click="addTriple">Add Triple</button>
        <button @click="runQuery">Run Query</button>
        <button @click="undoLastChange" :disabled="!canUndo">Undo</button>
        <button @click="redoLastChange" :disabled="!canRedo">Redo</button>
        <button @click="refresh">Refresh Graph</button>
        <button @click="clear">Clear Results</button>
      </div>

      <!-- SPARQL Query Input -->
      <div class="query-section">
        <label>
          SPARQL Query:
          <textarea v-model="sparqlQuery" rows="3"></textarea>
        </label>
      </div>

      <!-- Query Results -->
      <div v-if="results" class="results">
        <h3>Query Results:</h3>
        <pre>{{ JSON.stringify(results, null, 2) }}</pre>
      </div>

      <!-- Quads List -->
      <div class="quads-list">
        <h3>Graph Quads:</h3>
        <ul>
          <li v-for="(quad, index) in quads.slice(0, 10)" :key="index">
            {{ quad.subject.value }} - {{ quad.predicate.value }} - {{ quad.object.value }}
          </li>
        </ul>
        <p v-if="quads.length > 10">... and {{ quads.length - 10 }} more</p>
      </div>
    </div>
  `,
};

/**
 * Example: Subscription to real-time changes
 *
 * Demonstrates useSubscription composable
 */
function setupRealtimeSubscription() {
  // This would be used in a component setup() function
  const { createChangeFeed } = require('@unrdf/streaming');
  const { useSubscription } = require('@unrdf/composables');

  return function subscriptionExample(store) {
    const feed = createChangeFeed(store);

    // Subscribe to changes
    const { changes, count, lastChange } = useSubscription(
      feed,
      (delta) => delta.additions.length > 0, // Filter: only additions
      { maxChanges: 100 }
    );

    // Watch for new changes
    watch(changes, (newChanges) => {
      console.log(`Received ${newChanges.length} changes`);
    });

    return { changes, count, lastChange };
  };
}

/**
 * Example: Stream processing with batching
 *
 * Demonstrates useStreaming composable
 */
function setupStreamProcessing() {
  const { useStreaming } = require('@unrdf/composables');

  return function streamExample(feed) {
    const { events, eventCount, start, stop } = useStreaming(feed, {
      batchSize: 20,
      debounceMs: 100,
      maxEvents: 1000,
    });

    // Watch for stream events
    watch(events, (stream) => {
      console.log(`Stream updated: ${stream.length} events`);
    });

    return { events, eventCount, start, stop };
  };
}

// Export component and examples
export default RDFGraphComponent;
export { setupRealtimeSubscription, setupStreamProcessing };

// Example usage (not executed in module context)
if (typeof window !== 'undefined' && process.env.NODE_ENV !== 'test') {
  console.log('=== @unrdf/composables Basic Example ===');
  console.log('Import this module in your Vue 3 app:');
  console.log('');
  console.log('import RDFGraphComponent from "@unrdf/composables/examples/basic.mjs"');
  console.log('import { createApp } from "vue"');
  console.log('');
  console.log('createApp(RDFGraphComponent).mount("#app")');
  console.log('');
  console.log('Available composables:');
  console.log('- useGraph: Reactive RDF graph management');
  console.log('- useQuery: SPARQL query execution');
  console.log('- useDelta: Undo/redo tracking');
  console.log('- useTerms: RDF term factory');
  console.log('- useSubscription: Real-time change feeds');
  console.log('- useStreaming: Stream processing');
}
