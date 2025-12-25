/**
 * @fileoverview usePresence - Vue composable for presence awareness in collaborative editing
 *
 * Tracks who's currently editing, their cursors, selections, and custom state.
 *
 * @module @unrdf/collab/composables
 */

import { ref, computed, onUnmounted } from 'vue';

/**
 * usePresence - Track presence of collaborators
 *
 * Provides reactive state for:
 * - Who's online
 * - User info (name, color, avatar, etc.)
 * - Cursor positions / current selection
 * - Custom user state
 *
 * @param {import('../sync/websocket-sync.mjs').WebSocketSync} sync - WebSocket sync provider
 * @param {Object} [localState] - Initial local presence state
 * @returns {Object} Presence interface
 *
 * @example
 * const sync = new WebSocketSync(graph, { ... });
 * const { users, setLocalState } = usePresence(sync, {
 *   user: { name: 'Alice', color: '#ff0000' },
 *   cursor: null
 * });
 *
 * // Update when user selects a triple
 * setLocalState({
 *   cursor: { subject: 'http://example.org/alice' }
 * });
 */
export function usePresence(sync, localState = {}) {
  // Reactive state
  const users = ref([]);
  const localClientID = ref(null);

  // Set initial local state
  if (localState && Object.keys(localState).length > 0) {
    sync.setAwareness(localState);
  }

  // Update users list
  function updateUsers(awarenessState) {
    users.value = awarenessState.states.filter(
      (state) => state.clientID !== localClientID.value
    );
  }

  // Subscribe to awareness changes
  const unsubscribe = sync.on('awareness', (awarenessState) => {
    updateUsers(awarenessState);
  });

  // Get initial state
  const initialAwareness = sync.getAllAwareness();
  localClientID.value = sync.provider.awareness.clientID;
  updateUsers({ states: initialAwareness });

  // Cleanup
  onUnmounted(() => {
    unsubscribe();
  });

  return {
    /** @type {import('vue').ComputedRef<Array<Object>>} All other users */
    users: computed(() => users.value),

    /** @type {import('vue').ComputedRef<number>} Local client ID */
    localClientID: computed(() => localClientID.value),

    /** @type {import('vue').ComputedRef<number>} Number of users online */
    userCount: computed(() => users.value.length + 1), // +1 for local user

    /**
     * Update local presence state
     * @param {Object} state - New state (merged with existing)
     */
    setLocalState: (state) => {
      const current = sync.getClientAwareness(localClientID.value) || {};
      sync.setAwareness({ ...current, ...state });
    },

    /**
     * Get a specific user's state
     * @param {number} clientID - Client ID
     * @returns {Object|null} User state
     */
    getUserState: (clientID) => {
      return sync.getClientAwareness(clientID);
    },

    /**
     * Check if a user is online
     * @param {number} clientID - Client ID
     * @returns {boolean} True if online
     */
    isUserOnline: (clientID) => {
      return users.value.some((user) => user.clientID === clientID);
    },
  };
}
