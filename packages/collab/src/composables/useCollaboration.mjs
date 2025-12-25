/**
 * @fileoverview useCollaboration - Vue composable for collaborative RDF editing
 *
 * Provides reactive state for collaborative RDF graphs with real-time sync.
 * Integrates CRDT, WebSocket sync, and IndexedDB persistence.
 *
 * @module @unrdf/collab/composables
 */

import { ref, computed, onUnmounted } from 'vue';
import { CollaborativeRDFGraph } from '../crdt/rdf-crdt.mjs';
import { WebSocketSync } from '../sync/websocket-sync.mjs';
import { IndexedDBPersist } from '../sync/indexeddb-persist.mjs';

/**
 * @typedef {Object} CollabOptions
 * @property {string} [wsUrl] - WebSocket server URL
 * @property {string} [roomName] - Room name for collaboration
 * @property {string} [dbName] - IndexedDB database name
 * @property {Object} [awareness] - Initial awareness state
 * @property {boolean} [enableSync=true] - Enable WebSocket sync
 * @property {boolean} [enablePersist=true] - Enable IndexedDB persistence
 */

/**
 * useCollaboration - Vue composable for collaborative RDF graphs
 *
 * Manages collaborative RDF graph with:
 * - CRDT-based conflict-free editing
 * - Real-time WebSocket synchronization
 * - Offline-first IndexedDB persistence
 * - Reactive state for Vue
 *
 * @param {CollabOptions} [options={}] - Collaboration options
 * @returns {Object} Collaboration interface
 *
 * @example
 * <script setup>
 * import { useCollaboration } from '@unrdf/collab/composables';
 *
 * const {
 *   graph,
 *   triples,
 *   addTriple,
 *   removeTriple,
 *   isConnected,
 *   isSynced
 * } = useCollaboration({
 *   wsUrl: 'ws://localhost:1234',
 *   roomName: 'my-graph',
 *   dbName: 'my-graph-db',
 *   awareness: {
 *     user: { name: 'Alice', color: '#ff0000' }
 *   }
 * });
 * </script>
 *
 * <template>
 *   <div>
 *     <p>Connected: {{ isConnected }}</p>
 *     <p>Synced: {{ isSynced }}</p>
 *     <p>Triples: {{ triples.length }}</p>
 *   </div>
 * </template>
 */
export function useCollaboration(options = {}) {
  // Create collaborative graph
  const graph = new CollaborativeRDFGraph();

  // Reactive state
  const triples = ref([]);
  const isConnected = ref(false);
  const isSynced = ref(false);
  const stats = ref({ active: 0, tombstones: 0, total: 0, clientID: 0 });

  // Sync providers (optional)
  let syncProvider = null;
  let persistProvider = null;

  // Setup WebSocket sync if enabled
  if (options.enableSync !== false && options.wsUrl && options.roomName) {
    syncProvider = new WebSocketSync(graph, {
      url: options.wsUrl,
      roomName: options.roomName,
      awareness: options.awareness,
    });

    // Update reactive state on connection changes
    syncProvider.on('status', (event) => {
      isConnected.value = event.status === 'connected';
    });

    syncProvider.on('synced', (event) => {
      isSynced.value = event.isSynced;
    });
  }

  // Setup IndexedDB persistence if enabled
  if (options.enablePersist !== false && options.dbName) {
    persistProvider = new IndexedDBPersist(graph, {
      dbName: options.dbName,
      autoLoad: true,
    });

    // Wait for initial load before using graph
    persistProvider.whenSynced().then(() => {
      updateTriples();
    });
  }

  // Update triples from graph
  function updateTriples() {
    triples.value = graph.getTriples();
    stats.value = graph.getStats();
  }

  // Subscribe to graph changes
  const unsubscribeGraph = graph.onChange(() => {
    updateTriples();
  });

  // Initial load
  updateTriples();

  // Cleanup on unmount
  onUnmounted(() => {
    unsubscribeGraph();
    if (syncProvider) syncProvider.destroy();
    if (persistProvider) persistProvider.destroy();
  });

  return {
    // Core graph
    graph,
    triples: computed(() => triples.value),

    // Actions
    addTriple: (triple) => {
      graph.addTriple(triple);
    },

    removeTriple: (triple) => {
      graph.removeTriple(triple);
    },

    queryTriples: (pattern) => graph.queryTriples(pattern),

    clear: () => {
      graph.clear();
      updateTriples();
    },

    // State
    isConnected: computed(() => isConnected.value),
    isSynced: computed(() => isSynced.value),
    stats: computed(() => stats.value),

    // Sync control
    connect: () => syncProvider?.connect(),
    disconnect: () => syncProvider?.disconnect(),

    // Awareness (presence)
    setAwareness: (state) => syncProvider?.setAwareness(state),
    getAwareness: () => syncProvider?.getAllAwareness() || [],

    // Persistence
    forceSave: () => persistProvider?.forceSave(),
    clearDatabase: () => persistProvider?.clearDatabase(),

    // Advanced
    toStore: () => graph.toStore(),
    getYDoc: () => graph.getYDoc(),
  };
}
