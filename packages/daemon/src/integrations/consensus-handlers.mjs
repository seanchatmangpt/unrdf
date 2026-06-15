/**
 * @file Consensus Event Handlers
 * @module @unrdf/daemon/integrations/consensus-handlers
 * @description Event handlers for Raft consensus integration
 */

import { PartitionState, ConsensusOperationState } from './consensus-state.mjs';

/**
 * Handle leader elected event from Raft coordinator
 * @param {object} manager - ConsensusManager instance
 * @param {object} event - Leader elected event
 * @example
 */
export function handleLeaderElected(manager, event) {
  try {
    const { leaderId, term } = event || {};
    manager.leaderId = leaderId;
    manager.currentTerm = term || 0;
    manager.isLeader = leaderId === manager.nodeId;

    if (manager.partitionState === PartitionState.PARTITIONED) {
      manager.partitionState = PartitionState.RECOVERING;
      manager.emit('partition:recovered', {
        timestamp: Date.now(),
        newLeader: leaderId,
      });
    }

    manager.lastHeartbeat = Date.now();

    manager.emit('consensus:leader_elected', {
      leaderId,
      term: manager.currentTerm,
      isLeader: manager.isLeader,
      timestamp: new Date(),
    });
  } catch (error) {
    // Handle event errors gracefully
  }
}

/**
 * Handle leader lost event
 * @param {object} manager - ConsensusManager instance
 * @example
 */
export function handleLeaderLost(manager) {
  try {
    manager.isLeader = false;
    manager.leaderId = null;

    manager.emit('consensus:leader_lost', {
      timestamp: new Date(),
    });
  } catch (error) {
    // Handle event errors gracefully
  }
}

/**
 * Handle term update event
 * @param {object} manager - ConsensusManager instance
 * @param {object} event - Term updated event
 * @example
 */
export function handleTermUpdated(manager, event) {
  const { term } = event || {};
  if (term > manager.currentTerm) {
    manager.currentTerm = term;
    manager.emit('consensus:term_updated', { term, timestamp: new Date() });
  }
}

/**
 * Handle node joined event
 * @param {object} manager - ConsensusManager instance
 * @param {object} event - Node joined event
 * @example
 */
export function handleNodeJoined(manager, event) {
  try {
    const { nodeId } = event || {};

    if (manager.isLeader) {
      manager.emit('consensus:replicate_to_node', {
        nodeId,
        logEntries: manager.operationLog,
        timestamp: new Date(),
      });
    }
  } catch (error) {
    // Handle event errors gracefully
  }
}

/**
 * Handle node left event
 * @param {object} manager - ConsensusManager instance
 * @param {object} event - Node left event
 * @example
 */
export function handleNodeLeft(manager, event) {
  try {
    const { nodeId } = event || {};

    if (manager.isLeader) {
      manager.emit('consensus:trigger_election', {
        reason: 'node_left',
        nodeId,
        timestamp: new Date(),
      });
    }
  } catch (error) {
    // Handle event errors gracefully
  }
}

/**
 * Handle health degraded event
 * @param {object} manager - ConsensusManager instance
 * @param {object} event - Health degraded event
 * @example
 */
export function handleHealthDegraded(manager, event) {
  try {
    const { nodeId, health } = event || {};

    if (manager.partitionState === PartitionState.HEALTHY) {
      manager.partitionState = PartitionState.PARTITIONED;
      manager.partitionCounter.add(1);

      manager.emit('partition:detected', {
        nodeId,
        health,
        timestamp: new Date(),
      });
    }
  } catch (error) {
    // Handle event errors gracefully
  }
}

/**
 * Handle successful operation execution
 * @param {object} manager - ConsensusManager instance
 * @param {object} event - Operation success event
 * @example
 */
export function handleOperationSuccess(manager, event) {
  const { operationId } = event || {};
  if (operationId && manager.replicatedOperations.has(operationId)) {
    const currentState = manager.replicatedOperations.get(operationId);
    if (currentState === ConsensusOperationState.REPLICATED) {
      manager.replicatedOperations.set(operationId, ConsensusOperationState.COMMITTED);
      manager.committedCounter.add(1);
    }
  }
}

/**
 * Handle operation failure
 * @param {object} manager - ConsensusManager instance
 * @param {object} event - Operation failure event
 * @example
 */
export function handleOperationFailure(manager, event) {
  const { operationId, error } = event || {};
  if (operationId && manager.replicatedOperations.has(operationId)) {
    manager.replicatedOperations.set(operationId, ConsensusOperationState.FAILED);

    manager.emit('consensus:operation_failed', {
      operationId,
      error,
      timestamp: new Date(),
    });
  }
}

/**
 * Start partition detection timer
 * @param {object} manager - ConsensusManager instance
 * @example
 */
export function startPartitionDetection(manager) {
  manager.partitionTimer = setInterval(() => {
    const now = Date.now();
    const timeSinceLastHeartbeat = now - manager.lastHeartbeat;

    if (timeSinceLastHeartbeat > manager.config.partitionDetectionMs) {
      if (manager.partitionState === PartitionState.HEALTHY) {
        manager.partitionState = PartitionState.PARTITIONED;
        manager.partitionCounter.add(1);

        manager.emit('partition:detected_by_timeout', {
          timeSinceLastHeartbeat,
          threshold: manager.config.partitionDetectionMs,
          timestamp: new Date(),
        });
      }
    } else if (manager.partitionState === PartitionState.RECOVERING) {
      manager.partitionState = PartitionState.HEALTHY;

      manager.emit('partition:resolved', {
        timeSinceLastHeartbeat,
        timestamp: new Date(),
      });
    }
  }, manager.config.partitionDetectionMs / 2);
}

/**
 * Stop partition detection
 * @param {object} manager - ConsensusManager instance
 * @example
 */
export function stopPartitionDetection(manager) {
  if (manager.partitionTimer) {
    clearInterval(manager.partitionTimer);
    manager.partitionTimer = null;
  }
}
