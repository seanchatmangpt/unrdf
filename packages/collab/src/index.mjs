/**
 * @unrdf/collab
 *
 * Real-time collaborative RDF editing using CRDTs (Yjs)
 *
 * Features:
 * - Conflict-free collaborative editing
 * - Real-time WebSocket synchronization
 * - Offline-first with IndexedDB persistence
 * - Presence awareness (who's editing what)
 * - Vue composables for reactive state
 *
 * @module @unrdf/collab
 */

// CRDT
export { CollaborativeRDFGraph } from './crdt/index.mjs';

// Sync
export { WebSocketSync, IndexedDBPersist } from './sync/index.mjs';

// Composables
export { useCollaboration, usePresence } from './composables/index.mjs';
