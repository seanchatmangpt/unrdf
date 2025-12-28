/**
 * @file consensus/index.mjs
 * @description Consensus protocols for distributed KGC-SWARM
 *
 * Exports:
 * - Raft: Leader-based strong consistency (CP in CAP)
 * - Byzantine: Byzantine fault tolerance with 3f+1 nodes
 * - CRDT: Conflict-free replicated data types (AP in CAP)
 * - Membership: Node discovery and failure detection
 * - DistributedOrchestrator: Multi-node orchestration
 *
 * @module @unrdf/kgc-swarm/consensus
 */

// Raft consensus
export {
  RaftNode,
  createRaftNode,
  NodeState,
  RaftConfigSchema,
  LogEntrySchema,
  RequestVoteArgsSchema,
  RequestVoteReplySchema,
  AppendEntriesArgsSchema,
  AppendEntriesReplySchema,
} from './raft.mjs';

// Byzantine fault tolerance
export {
  ByzantineNode,
  createByzantineNode,
  ConsensusPhase,
  MessageType,
  ByzantineConfigSchema,
  RequestSchema,
  PrePrepareSchema,
  PrepareSchema,
  CommitSchema,
  SignedMessageSchema,
} from './byzantine.mjs';

// CRDT data types
export {
  VectorClock,
  GSet,
  TwoPhaseSet,
  LWWElementSet,
  ORSet,
  createCRDT,
  VectorClockSchema,
  TimestampedElementSchema,
  TaggedElementSchema,
} from './crdt.mjs';

// Membership management
export {
  MembershipManager,
  createMembershipManager,
  NodeStatus,
  GossipMessageType,
  MembershipConfigSchema,
  MemberSchema,
  GossipMessageSchema,
} from './membership.mjs';

// Distributed orchestrator
export {
  DistributedOrchestrator,
  createDistributedOrchestrator,
  ConsensusMode,
  DistributedConfigSchema,
} from './distributed-orchestrator.mjs';
