import { RaftCoordinator as BaseRaftCoordinator } from './raft-coordinator.mjs';

/**
 * Raft coordinator with an admitted distinction between configured membership
 * and currently connected transport peers.
 */
export class RaftCoordinator extends BaseRaftCoordinator {
  /**
   * Get the base coordinator state, reporting configured peers in `peers`
   * and the base state's peer list as `connectedPeers`.
   * @returns {Object} Coordinator state with `peers` (configured peer IDs) and `connectedPeers`
   */
  getState() {
    const state = super.getState();
    return {
      ...state,
      peers: [...this.peers.keys()],
      connectedPeers: state.peers,
    };
  }
}

/**
 * Create an admitted-membership Raft coordinator.
 * @param {Object} config - Raft configuration passed to the RaftCoordinator constructor
 * @returns {RaftCoordinator} New coordinator instance
 */
export function createRaftCoordinator(config) {
  return new RaftCoordinator(config);
}
