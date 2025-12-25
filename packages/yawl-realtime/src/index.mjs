/**
 * @file YAWL Realtime - Main entry point
 * @module @unrdf/yawl-realtime
 *
 * @description
 * Real-time collaboration framework for YAWL workflows using Socket.io.
 * Enables multiple users to collaboratively execute workflows with:
 * - Real-time event broadcasting
 * - Optimistic locking with Lamport timestamps
 * - CRDT-inspired state synchronization
 * - Receipt-based conflict detection
 */

export {
  YAWLRealtimeServer,
  OptimisticLockManager,
  StateSyncManager,
} from './server.mjs';

export {
  YAWLRealtimeClient,
} from './client.mjs';

export default {
  YAWLRealtimeServer: (await import('./server.mjs')).YAWLRealtimeServer,
  YAWLRealtimeClient: (await import('./client.mjs')).YAWLRealtimeClient,
};
