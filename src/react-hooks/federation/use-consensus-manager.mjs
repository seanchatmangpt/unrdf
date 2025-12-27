/**
 * @file use-consensus-manager.mjs
 * @description React hook for managing consensus protocols (RAFT, Gossip, Byzantine)
 * @since 3.2.0
 */

import { useState, useCallback, useEffect, useRef } from 'react';
import { useKnowledgeEngineContext } from '../core/use-knowledge-engine-context.mjs';

/**
 * Hook for managing distributed consensus in federated systems
 *
 * @since 3.2.0
 * @param {Object} config - Consensus configuration
 * @param {string} [config.protocol='raft'] - Consensus protocol
 * @param {string} config.nodeId - This node's identifier
 * @param {string[]} config.peers - Peer node identifiers
 * @param {number} [config.electionTimeout=5000] - Leader election timeout (ms)
 * @param {number} [config.heartbeatInterval=1000] - Heartbeat interval (ms)
 * @returns {Object} Consensus state and operations
 * @throws {Error} When consensus manager not initialized
 * @throws {Error} When propose is called on non-leader node
 * @throws {Error} When stepDown is called on non-leader node
 * @throws {Error} When proposal fails to reach quorum within timeout
 * @performance Leader election and heartbeats create network overhead. Increase heartbeatInterval
 *   for lower overhead, but slower failure detection. Log grows unbounded - implement compaction.
 *
 * @example
 * // Basic RAFT consensus
 * const {
 *   state,          // 'follower', 'candidate', 'leader'
 *   leader,         // Current leader node ID
 *   term,           // Current election term
 *   propose,        // Propose a value for consensus
 *   commitIndex,    // Last committed log index
 *   loading
 * } = useConsensusManager({
 *   protocol: 'raft',
 *   nodeId: 'node-1',
 *   peers: ['node-2', 'node-3']
 * });
 *
 * @example
 * // Propose value for consensus (leader only)
 * if (state === 'leader') {
 *   await propose({ type: 'update', data: newData });
 * }
 */
export function useConsensusManager(config = {}) {
  const { engine } = useKnowledgeEngineContext();
  const [manager, setManager] = useState(null);
  const [state, setState] = useState('follower'); // follower, candidate, leader
  const [leader, setLeader] = useState(null);
  const [term, setTerm] = useState(0);
  const [commitIndex, setCommitIndex] = useState(0);
  const [log, setLog] = useState([]);
  const [peers, _setPeers] = useState(config.peers || []);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const managerRef = useRef(null);

  // Initialize consensus manager
  useEffect(() => {
    let mounted = true;

    async function initializeConsensus() {
      try {
        setLoading(true);

        // Import consensus module
        const { ConsensusManager } = await import(
          '../../knowledge-engine/federation/consensus-manager.mjs'
        );

        // Create consensus manager
        const consensusManager = new ConsensusManager({
          ...config,
          engine,
          onStateChange: newState => {
            if (!mounted) return;
            setState(newState.role);
            setLeader(newState.leader);
            setTerm(newState.term);
          },
          onCommit: (index, _entry) => {
            if (!mounted) return;
            setCommitIndex(index);
          },
          onLogAppend: entry => {
            if (!mounted) return;
            setLog(prev => [...prev, entry]);
          },
        });

        // Start consensus protocol
        await consensusManager.start();

        if (!mounted) return;

        managerRef.current = consensusManager;
        setManager(consensusManager);
        setLoading(false);
      } catch (err) {
        if (!mounted) return;
        setError(err);
        setLoading(false);
      }
    }

    initializeConsensus();

    return () => {
      mounted = false;
      if (managerRef.current) {
        managerRef.current.stop?.();
      }
    };
  }, [engine, JSON.stringify(config)]);

  // Propose a value for consensus
  const propose = useCallback(
    async (value, options = {}) => {
      if (!manager) {
        throw new Error('Consensus manager not initialized');
      }

      if (state !== 'leader') {
        throw new Error('Only leader can propose values');
      }

      try {
        const logEntry = {
          term,
          value,
          timestamp: new Date().toISOString(),
          metadata: options.metadata,
        };

        const result = await manager.propose(logEntry, {
          timeout: options.timeout || 10000,
          quorum: options.quorum || Math.floor(peers.length / 2) + 1,
        });

        return result;
      } catch (err) {
        setError(err);
        throw err;
      }
    },
    [manager, state, term, peers]
  );

  // Request vote in leader election
  const requestVote = useCallback(
    async (candidateId, candidateTerm) => {
      if (!manager) {
        throw new Error('Consensus manager not initialized');
      }

      try {
        const voteGranted = await manager.requestVote({
          candidateId,
          term: candidateTerm,
          lastLogIndex: log.length - 1,
          lastLogTerm: log[log.length - 1]?.term || 0,
        });

        return { voteGranted };
      } catch (err) {
        setError(err);
        throw err;
      }
    },
    [manager, log]
  );

  // Append entries (replication)
  const appendEntries = useCallback(
    async (entries, leaderCommit) => {
      if (!manager) {
        throw new Error('Consensus manager not initialized');
      }

      try {
        const success = await manager.appendEntries({
          entries,
          leaderCommit,
          prevLogIndex: log.length - 1,
          prevLogTerm: log[log.length - 1]?.term || 0,
        });

        return { success };
      } catch (err) {
        setError(err);
        throw err;
      }
    },
    [manager, log]
  );

  // Get consensus status
  const getStatus = useCallback(() => {
    return {
      state,
      leader,
      term,
      commitIndex,
      logLength: log.length,
      peers,
      isLeader: state === 'leader',
      canPropose: state === 'leader',
    };
  }, [state, leader, term, commitIndex, log, peers]);

  // Step down from leader
  const stepDown = useCallback(async () => {
    if (!manager) {
      throw new Error('Consensus manager not initialized');
    }

    if (state !== 'leader') {
      throw new Error('Not currently leader');
    }

    try {
      await manager.stepDown();
      setState('follower');
      return { success: true };
    } catch (err) {
      setError(err);
      throw err;
    }
  }, [manager, state]);

  return {
    manager,
    state,
    leader,
    term,
    commitIndex,
    log,
    peers,
    loading,
    error,
    propose,
    requestVote,
    appendEntries,
    getStatus,
    stepDown,
  };
}
