import { RaftCoordinator as BaseRaftCoordinator } from './raft-coordinator.mjs';

/**
 * Raft coordinator with an admitted distinction between configured membership
 * and currently connected transport peers.
 */
export class RaftCoordinator extends BaseRaftCoordinator {
  getState() {
    const state = super.getState();
    return {
      ...state,
      peers: [...this.peers.keys()],
      connectedPeers: state.peers,
    };
  }
}

export function createRaftCoordinator(config) {
  return new RaftCoordinator(config);
}
