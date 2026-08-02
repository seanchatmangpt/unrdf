/**
 * @fileoverview UNRDF Consensus - Production-grade Raft Consensus
 * @module consensus
 *
 * @description
 * Distributed consensus system using Raft algorithm for workflow coordination.
 * Provides leader election, log replication, and strong consistency guarantees.
 *
 * @example
 * import { createRaftCoordinator, createClusterManager, createDistributedStateMachine } from '@unrdf/consensus';
 *
 * const coordinator = createRaftCoordinator({ nodeId: 'node-1', port: 8080 });
 * await coordinator.initialize();
 */

export { RaftCoordinator, createRaftCoordinator } from './raft/admitted-raft-coordinator.mjs';
export { ClusterManager, createClusterManager, NodeHealth } from './membership/cluster-manager.mjs';
export {
  DistributedStateMachine,
  createDistributedStateMachine,
  StateOperation,
} from './state/distributed-state-machine.mjs';
export { WebSocketTransport, createWebSocketTransport } from './transport/websocket-transport.mjs';
