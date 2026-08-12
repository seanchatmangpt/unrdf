# PICTL Federation Integration Design

**Version:** 1.0
**Date:** 2026-04-14
**Status:** Design Document

## Overview

This document describes the distributed process mining workflow for pictl across multiple federation nodes. The design integrates:

1. **RAFT Consensus** (`@unrdf/federation/consensus-manager`) — Leader election, log replication
2. **Quorum Voting** (`@unrdf/pictl-semantics/quorum`) — M-of-N consensus on results
3. **Multi-Master Replication** (`@unrdf/federation/data-replication`) — Conflict resolution
4. **Distributed Query Engine** (`@unrdf/federation/distributed-query-engine`) — Federated SPARQL

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        Federation Process Mining Layer                       │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐       │
│  │  PICTL Node A   │    │  PICTL Node B   │    │  PICTL Node C   │       │
│  │  (Leader)       │    │  (Follower)     │    │  (Follower)     │       │
│  │                 │    │                 │    │                 │       │
│  │  ┌───────────┐  │    │  ┌───────────┐  │    │  ┌───────────┐  │       │
│  │  │   Local   │  │    │  │   Local   │  │    │  │   Local   │  │       │
│  │  │  Mining   │  │    │  │  Mining   │  │    │  │  Mining   │  │       │
│  │  └─────┬─────┘  │    │  └─────┬─────┘  │    │  └─────┬─────┘  │       │
│  │        │        │    │        │        │    │        │        │       │
│  │  ┌─────▼─────┐  │    │  ┌─────▼─────┐  │    │  ┌─────▼─────┐  │       │
│  │  │   SHACL   │  │    │  │   SHACL   │  │    │  │   SHACL   │  │       │
│  │  │ Validate  │  │    │  │ Validate  │  │    │  │ Validate  │  │       │
│  │  └─────┬─────┘  │    │  └─────┬─────┘  │    │  └─────┬─────┘  │       │
│  └────────┼────────┘    └────────┼────────┘    └────────┼────────┘       │
│           │                      │                      │                  │
│           └──────────────────────┼──────────────────────┘                  │
│                                  ▼                                         │
│                    ┌─────────────────────────┐                             │
│                    │   Quorum Coordinator    │                             │
│                    │   (M-of-N Consensus)    │                             │
│                    └─────────────┬───────────┘                             │
│                                  │                                         │
│                    ┌─────────────▼───────────┐                             │
│                    │   BLAKE3 Receipt Chain  │                             │
│                    │   (Provenance Tracking)  │                             │
│                    └─────────────────────────┘                             │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                        Federation Infrastructure                             │
│                                                                             │
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐       │
│  │ RAFT Consensus │    │  Data Replica   │    │  Distributed    │       │
│  │    Manager     │◄──►│     Manager     │◄──►│  Query Engine   │       │
│  └─────────────────┘    └─────────────────┘    └─────────────────┘       │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Message Flows

### Flow 1: Distributed Process Discovery

```
1. Client Request
   └─> POST /api/v1/federation/discover
       {
         "query": "SELECT * WHERE { ?s a :ProcessTrace }",
         "algorithm": "inductive",
         "quorum_threshold": 2
       }

2. RAFT Leader Broadcast
   Leader (Node A) broadcasts via RAFT:
   └─> consensus.replicate({
         type: "DISCOVER_PROCESS",
         queryId: "uuid-123",
         algorithm: "inductive",
         timestamp: 1645123456789
       })

3. Parallel Local Mining
   Each node executes locally:
   ┌─────────────────────────────────────────────────────────────┐
   │ Node A                                        Node B         │
   │   │                                             │           │
   │   ▼                                             ▼           │
   │ ┌─────────────┐                          ┌─────────────┐    │
   │ │ fetch_traces│                          │ fetch_traces│    │
   │ │ from Tempo  │                          │ from Tempo  │    │
   │ └──────┬──────┘                          └──────┬──────┘    │
   │        │                                        │           │
   │        ▼                                        ▼           │
   │ ┌─────────────┐                          ┌─────────────┐    │
   │ │pm4py.discover│                          │pm4py.discover│    │
   │ │  _inductive  │                          │  _inductive  │    │
   │ └──────┬──────┘                          └──────┬──────┘    │
   │        │                                        │           │
   │        ▼                                        ▼           │
   │ ┌─────────────┐                          ┌─────────────┐    │
   │ │  fitness:   │                          │  fitness:   │    │
   │ │   0.92      │                          │   0.89      │    │
   │ │ precision:  │                          │ precision:  │    │
   │ │   0.88      │                          │   0.85      │    │
   │ └──────┬──────┘                          └──────┬──────┘    │
   └────────┼────────────────────────────────────────┼───────────┘
           │                                        │
           └────────────────┬───────────────────────┘
                            │
                            ▼
4. SHACL Validation (Local)
   Each node validates against local shapes:
   ┌─────────────────────────────────────────────────────────────┐
   │ Node A                                        Node B         │
   │   │                                             │           │
   │   ▼                                             ▼           │
   │ ┌─────────────┐                          ┌─────────────┐    │
   │ │ SHACL check │                          │ SHACL check │    │
   │ │ fitness ∈   │                          │ fitness ∈   │    │
   │ │ [0.0, 1.0]  │                          │ [0.0, 1.0]  │    │
   │ └──────┬──────┘                          └──────┬──────┘    │
   │        │ PASS                                     │ PASS    │
   └────────┼────────────────────────────────────────┼───────────┘
```

### Flow 2: Quorum Consensus on Models

```
5. Propose Results
   Each node proposes its result to quorum:
   ┌─────────────────────────────────────────────────────────────┐
   │ Node A                                        Node B         │
   │   │                                             │           │
   │   ▼                                             ▼           │
   │ proposeResult({                                proposeResult({
   │   resultId: "hash-abc",                          resultId: "hash-def",
   │   fitness: 0.92,                                 fitness: 0.89,
   │   precision: 0.88                                precision: 0.85
   │ }, quorumState)                                 }, quorumState)
   │   │                                             │           │
   │   ▼                                             ▼           │
   │ ┌─────────────────────────────────────────────────────────┐ │
   │ │ Shared Quorum State (replicated via RAFT)               │ │
   │ │ {                                                       │ │
   │ │   results: {                                           │ │
   │ │     "hash-abc": { proposer: "A", votes: [], status: "pending" },│ │
   │ │     "hash-def": { proposer: "B", votes: [], status: "pending" } │ │
   │ │   }                                                     │ │
   │ │ }                                                       │ │
   │ └─────────────────────────────────────────────────────────┘ │
   └─────────────────────────────────────────────────────────────┘

6. Vote on Results (M-of-N)
   Each node votes on OTHER nodes' results:
   ┌─────────────────────────────────────────────────────────────┐
   │ Node A votes on B's result:                  Node B votes on A's:│
   │   │                                             │           │
   │   ▼                                             ▼           │
   │ votePictlResult(                               votePictlResult(
   │   resultId: "hash-def",                          resultId: "hash-abc",
   │   vote: "approve",                               vote: "approve",
   │   reason: "SHACL passed, metrics acceptable"     reason: "SHACL passed"
   │ )                                              )             │
   │   │                                             │           │
   │   ▼                                             ▼           │
   │ ┌─────────────────────────────────────────────────────────┐ │
   │ │ Quorum State Update:                                   │ │
   │ │ {                                                       │ │
   │ │   "hash-abc": {                                        │ │
   │ │     proposer: "A",                                     │ │
   │ │     votes: [{node:"B",vote:"approve"}],                │ │
   │ │     approvalCount: 1,                                  │ │
   │ │     status: "pending"  // Need 2/3 ≈ 2 approvals        │ │
   │ │   }                                                    │ │
   │ │ }                                                       │ │
   │ └─────────────────────────────────────────────────────────┘ │
   └─────────────────────────────────────────────────────────────┘

7. Consensus Reached
   When threshold (⌈2/3 * n⌉) reached:
   ┌─────────────────────────────────────────────────────────────┐
   │ All nodes receive consensus event:                          │
   │   │                                                          │
   │   ▼                                                          │
   │ ┌─────────────────────────────────────────────────────────┐ │
   │ │ Consensus Achieved:                                     │ │
   │ │ {                                                       │ │
   │ │   resultId: "hash-abc",                                 │ │
   │ │   status: "approved",                                   │ │
   │ │   receipt: {                                            │ │
   │ │     hash: "blake3-...",                                 │ │
   │ │     previousHash: "blake3-...",                          │ │
   │ │     voteCount: 3,                                       │ │
   │ │     approvalCount: 2                                    │ │
   │ │   }                                                      │ │
   │ │ }                                                       │ │
   │ └─────────────────────────────────────────────────────────┘ │
   └─────────────────────────────────────────────────────────────┘
```

### Flow 3: Conflict Resolution

```
When nodes disagree on process models:

┌─────────────────────────────────────────────────────────────────────┐
│ Scenario: Node A discovers model with fitness=0.92                  │
│           Node B discovers model with fitness=0.75                  │
│           Node C discovers model with fitness=0.88                  │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│ 1. SHACL Filter Step                                               │
│    ┌─────────────────────────────────────────────────────────────┐  │
│    │ Node B's result REJECTED (fitness < 0.8 threshold)          │  │
│    │ Remaining candidates: Node A (0.92), Node C (0.88)         │  │
│    └─────────────────────────────────────────────────────────────┘  │
│                                                                     │
│ 2. Model Comparison (Semantic Drift Detection)                     │
│    ┌─────────────────────────────────────────────────────────────┐  │
│    │ Compare A's model vs C's model:                             │  │
│    │ - Activity overlap: 95%                                    │  │
│    │ - Edge overlap: 88%                                        │  │
│    │ - Structural similarity: 0.91                               │  │
│    │                                                             │  │
│    │ Decision: Models are SEMANTICALLY EQUIVALENT               │  │
│    └─────────────────────────────────────────────────────────────┘  │
│                                                                     │
│ 3. Merge Strategy                                                  │
│    ┌─────────────────────────────────────────────────────────────┐  │
│    │ Create MERGED MODEL:                                       │  │
│    │ - Take union of activities from both models                │  │
│    │ - Resolve edges via majority voting                        │  │
│    │ - Compute aggregate metrics (mean fitness)                 │  │
│    │                                                             │  │
│    │ Result: {                                                  │  │
│    │   model: merged_petri_net,                                 │  │
│    │   fitness: 0.90,  // (0.92 + 0.88) / 2                    │  │
│    │   precision: 0.865,                                        │  │
│    │   source_models: ["hash-abc", "hash-ghi"]                 │  │
│    │   merge_type: "semantic_equivalence"                      │  │
│    │ }                                                          │  │
│    └─────────────────────────────────────────────────────────────┘  │
│                                                                     │
│ 4. Receipt Chain Extension                                         │
│    ┌─────────────────────────────────────────────────────────────┐  │
│    │ BLAKE3 Chain:                                               │  │
│    │ [receipt_A] ← [receipt_C] ← [receipt_merged]               │  │
│    │                                                             │  │
│    │ receipt_merged = {                                         │  │
│    │   hash: blake3(merged_model + receipt_A.hash + receipt_C.hash),│
│    │   previousHash: receipt_C.hash,                            │  │
│    │   mergeOf: [receipt_A.hash, receipt_C.hash],               │  │
│    │   conflictResolution: "semantic_merge"                     │  │
│    │ }                                                          │  │
│    └─────────────────────────────────────────────────────────────┘  │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

### Flow 4: BLAKE3 Receipt Chain for Provenance

```
Full receipt chain for a federated mining operation:

┌─────────────────────────────────────────────────────────────────────┐
│ Receipt Chain (Immutable Audit Trail)                              │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│ Block 1: Query Proposal                                            │
│ ┌─────────────────────────────────────────────────────────────────┐ │
│ │ {                                                               │ │
│ │   hash: "0xabc123...",                                         │ │
│ │   previousHash: "0".repeat(64),  // Genesis                     │ │
│ │   timestamp: 1645123456789,                                    │ │
│ │   nodeId: "client-request",                                    │ │
│ │   type: "query_proposal",                                      │ │
│ │   data: { query: "SELECT * WHERE { ... }" }                    │ │
│ │ }                                                               │ │
│ └─────────────────────────────────────────────────────────────────┘ │
│                           │                                         │
│                           ▼                                         │
│ Block 2: RAFT Log Replication                                      │
│ ┌─────────────────────────────────────────────────────────────────┐ │
│ │ {                                                               │ │
│ │   hash: "0xdef456...",                                         │ │
│ │   previousHash: "0xabc123...",                                 │ │
│ │   timestamp: 1645123456890,                                    │ │
│ │   nodeId: "raft-leader",                                       │ │
│ │   type: "raft_log_entry",                                     │ │
│ │   data: { term: 5, index: 123, command: "DISCOVER_PROCESS" }  │ │
│ │ }                                                               │ │
│ └─────────────────────────────────────────────────────────────────┘ │
│                           │                                         │
│                           ▼                                         │
│ Block 3: Node A Local Mining Result                                │
│ ┌─────────────────────────────────────────────────────────────────┐ │
│ │ {                                                               │ │
│ │   hash: "0xghi789...",                                         │ │
│ │   previousHash: "0xdef456...",                                 │ │
│ │   timestamp: 1645123459999,                                    │ │
│ │   nodeId: "pictl-node-a",                                      │ │
│ │   type: "mining_result",                                       │ │
│ │   data: { algorithm: "inductive", fitness: 0.92, ... }        │ │
│ │ }                                                               │ │
│ └─────────────────────────────────────────────────────────────────┘ │
│                           │                                         │
│                           ▼                                         │
│ Block 4: Node B Local Mining Result                                │
│ ┌─────────────────────────────────────────────────────────────────┐ │
│ │ {                                                               │ │
│ │   hash: "0xjkl012...",                                         │ │
│ │   previousHash: "0xghi789...",                                 │ │
│ │   timestamp: 1645123461000,                                    │ │
│ │   nodeId: "pictl-node-b",                                      │ │
│ │   type: "mining_result",                                       │ │
│ │   data: { algorithm: "inductive", fitness: 0.89, ... }        │ │
│ │ }                                                               │ │
│ └─────────────────────────────────────────────────────────────────┘ │
│                           │                                         │
│                           ▼                                         │
│ Block 5: Quorum Consensus Receipt                                  │
│ ┌─────────────────────────────────────────────────────────────────┐ │
│ │ {                                                               │ │
│ │   hash: "0xmno345...",                                         │ │
│ │   previousHash: "0xjkl012...",                                 │ │
│ │   timestamp: 1645123462000,                                    │ │
│ │   nodeId: "quorum-coordinator",                                │ │
│ │   type: "consensus_receipt",                                   │ │
│ │   data: {                                                      │ │
│ │     resultId: "merged-result-xyz",                             │ │
│ │     voteCount: 3,                                              │ │
│ │     approvalCount: 3,                                          │ │
│ │     consensus: "unanimous",                                    │ │
│ │     modelHash: "0xmerged678..."                                │ │
│ │   }                                                            │ │
│ │ }                                                               │ │
│ └─────────────────────────────────────────────────────────────────┘ │
│                                                                     │
│ Validation: Each block's previousHash MUST match the prior block's hash│
└─────────────────────────────────────────────────────────────────────┘
```

## API Specification

### POST /api/v1/federation/discover

Initiate distributed process discovery across federation.

**Request:**
```json
{
  "query": "SELECT * WHERE { ?s a :ProcessTrace }",
  "algorithm": "inductive",
  "quorum_threshold": 2,
  "shacl_shapes": ["urn:pictl:FitnessShape", "urn:pictl:PrecisionShape"],
  "timeout_ms": 30000
}
```

**Response:**
```json
{
  "query_id": "uuid-123",
  "status": "pending",
  "leader_node": "pictl-node-a",
  "participant_nodes": ["pictl-node-a", "pictl-node-b", "pictl-node-c"],
  "estimated_completion_ms": 5000
}
```

### GET /api/v1/federation/result/{query_id}

Retrieve federated mining result.

**Response:**
```json
{
  "query_id": "uuid-123",
  "status": "approved",
  "consensus": {
    "vote_count": 3,
    "approval_count": 3,
    "threshold": 2,
    "achieved": true
  },
  "result": {
    "model": { /* Petri net or process tree */ },
    "quality": {
      "fitness": 0.90,
      "precision": 0.865,
      "generalization": 0.82,
      "simplicity": 0.75
    }
  },
  "receipt_chain": [
    { "hash": "0xabc123...", "previousHash": "0".repeat(64), ... },
    { "hash": "0xdef456...", "previousHash": "0xabc123...", ... },
    ...
  ],
  "provenance": {
    "input_hash": "blake3(original_query)",
    "model_hash": "blake3(consensus_model)",
    "validation_timestamps": ["2026-04-14T12:34:56Z", ...]
  }
}
```

### POST /api/v1/federation/validate

Validate receipt chain integrity.

**Request:**
```json
{
  "receipt_chain": [
    { "hash": "0xabc123...", "previousHash": "0".repeat(64), ... },
    ...
  ]
}
```

**Response:**
```json
{
  "valid": true,
  "receipt_count": 5,
  "chain_hash": "0xfinal...",
  "integrity_check": "All receipts form valid chain"
}
```

## Implementation Modules

### 1. ProcessMiningFederationCoordinator

```javascript
/**
 * @fileoverview Federation Coordinator for PICTL Process Mining
 * @module pictl-federation/coordinator
 */

import { createConsensusManager } from '@unrdf/federation/consensus-manager.mjs';
import { createDataReplicationManager } from '@unrdf/federation/data-replication.mjs';
import { createDistributedQueryEngine } from '@unrdf/federation/distributed-query-engine.mjs';
import { proposeResult, votePictlResult, getQuorumStatus, validateVoteTally } from '@unrdf/pictl-semantics/quorum.mjs';
import { validatePictlResult, validateAgainstShapes } from '@unrdf/pictl-semantics/result-validator.mjs';
import { blake3Hash } from '@unrdf/pictl-semantics/quorum.mjs';
import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('pictl-federation');

export class ProcessMiningFederationCoordinator {
  constructor(config) {
    this.config = config;
    this.nodeId = config.nodeId;
    this.quorumState = {
      nodeId: this.nodeId,
      quorumThreshold: config.quorumThreshold || 2,
      results: new Map(),
      votes: new Map(),
      receipts: [],
    };

    // Initialize federation components
    this.consensus = createConsensusManager({
      nodeId: this.nodeId,
      electionTimeoutMin: 150,
      electionTimeoutMax: 300,
    });

    this.replication = createDataReplicationManager(
      config.federationCoordinator,
      {
        topology: 'full-mesh',
        conflictResolution: 'merge',
      }
    );

    this.queryEngine = createDistributedQueryEngine(
      config.federationCoordinator,
      {
        executionStrategy: 'parallel',
        enablePushdown: true,
      }
    );

    // Local process mining (delegates to pictl-semantics)
    this.localMiner = config.localMiner;
  }

  async initialize() {
    await this.consensus.initialize();
    await this.replication.initialize();
    await this.consensus.addPeer('node-b', 'http://node-b:8080');
    await this.consensus.addPeer('node-c', 'http://node-c:8080');
  }

  /**
   * Execute distributed process discovery
   *
   * Workflow:
   * 1. Leader broadcasts discovery request via RAFT
   * 2. Each node executes local mining
   * 3. Each node validates result against SHACL shapes
   * 4. Each node proposes result to quorum
   * 5. Each node votes on other nodes' results
   * 6. Consensus achieved → receipt chain created
   */
  async discoverProcess(query, options = {}) {
    return tracer.startActiveSpan('federation.discover', async span => {
      try {
        const queryId = blake3Hash(JSON.stringify({ query, timestamp: Date.now() }));

        span.setAttribute('query.id', queryId);
        span.setAttribute('query.algorithm', options.algorithm || 'inductive');

        // Step 1: Replicate discovery request via RAFT
        if (this.consensus.state === 'leader') {
          await this.consensus.replicate({
            type: 'DISCOVER_PROCESS',
            queryId,
            query,
            algorithm: options.algorithm || 'inductive',
            timestamp: Date.now(),
          });
        }

        // Step 2: Execute local mining
        const localResult = await this.localMiner.discover(query, options);

        // Step 3: SHACL validation
        const validation = validatePictlResult(localResult);
        if (!validation.valid) {
          throw new Error(`Local mining result failed SHACL validation: ${validation.errors.join(', ')}`);
        }

        // Step 4: Propose result to quorum
        const proposed = proposeResult(localResult, this.nodeId, this.quorumState);

        span.addEvent('result_proposed', {
          'result.id': proposed.resultId,
          'result.fitness': localResult.fitness,
        });

        // Step 5: Wait for other nodes' proposals and vote
        await this.participateInQuorum(proposed.resultId, options.timeoutMs || 30000);

        // Step 6: Check consensus
        const status = getQuorumStatus(this.quorumState);
        const finalResult = status.approvedResults.find(r => r.resultId === proposed.resultId);

        if (!finalResult) {
          throw new Error('Consensus not reached within timeout');
        }

        // Step 7: Validate receipt chain
        const chainValidation = validateVoteTally(this.quorumState);
        if (!chainValidation.valid) {
          throw new Error(`Receipt chain broken: ${chainValidation.error}`);
        }

        span.setStatus({ code: SpanStatusCode.OK });
        return {
          queryId,
          result: finalResult.result,
          consensus: {
            voteCount: finalResult.voteCount,
            approvalCount: finalResult.approvalCount,
            threshold: this.quorumState.quorumThreshold,
          },
          receiptChain: this.quorumState.receipts,
          chainValidation,
        };

      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Participate in quorum voting
   *
   * Vote on other nodes' proposals after validating against SHACL shapes
   */
  async participateInQuorum(myResultId, timeoutMs) {
    const startTime = Date.now();

    while (Date.now() - startTime < timeoutMs) {
      // Get all pending results
      const pending = Array.from(this.quorumState.results.values())
        .filter(r => r.status === 'pending' && r.resultId !== myResultId);

      for (const proposal of pending) {
        // Check if we already voted
        const alreadyVoted = this.quorumState.votes.has(`${proposal.resultId}:${this.nodeId}`);
        if (alreadyVoted) continue;

        // Validate against SHACL shapes
        const validation = validatePictlResult(proposal.result);

        // Vote based on validation
        const vote = validation.valid ? 'approve' : 'reject';
        const reason = validation.valid
          ? 'SHACL validation passed'
          : `SHACL validation failed: ${validation.errors.join(', ')}`;

        await votePictlResult(proposal.resultId, vote, reason, this.quorumState);
      }

      // Check if consensus reached
      const myProposal = this.quorumState.results.get(myResultId);
      if (myProposal && myProposal.status !== 'pending') {
        return; // Consensus reached
      }

      // Wait before next check
      await new Promise(resolve => setTimeout(resolve, 100));
    }

    throw new Error('Quorum timeout');
  }

  /**
   * Handle incoming quorum message from peer
   */
  async handleQuorumMessage(message) {
    return tracer.startActiveSpan('federation.handle_message', async span => {
      try {
        span.setAttribute('message.type', message.type);

        switch (message.type) {
          case 'propose':
            // Store proposal
            this.quorumState.results.set(message.resultId, {
              resultId: message.resultId,
              result: message.result,
              proposerId: message.proposerId,
              timestamp: message.timestamp,
              votes: [],
              status: 'pending',
            });
            break;

          case 'vote':
            // Record vote
            const vote = votePictlResult(
              message.resultId,
              message.vote,
              message.reason,
              this.quorumState
            );
            return vote;

          default:
            throw new Error(`Unknown message type: ${message.type}`);
        }

        span.setStatus({ code: SpanStatusCode.OK });
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Resolve conflicts between different process models
   *
   * Uses semantic drift detection to determine if models are equivalent
   */
  async resolveModelConflict(results) {
    return tracer.startActiveSpan('federation.resolve_conflict', async span => {
      try {
        span.setAttribute('conflict.result_count', results.length);

        // Filter by SHACL validation
        const validResults = results.filter(r => {
          const validation = validatePictlResult(r.result);
          return validation.valid;
        });

        if (validResults.length === 0) {
          throw new Error('No valid results after SHACL filtering');
        }

        if (validResults.length === 1) {
          return validResults[0].result; // Single valid result
        }

        // Compare models for semantic equivalence
        const similarities = [];
        for (let i = 0; i < validResults.length; i++) {
          for (let j = i + 1; j < validResults.length; j++) {
            const similarity = await this.computeModelSimilarity(
              validResults[i].result.model,
              validResults[j].result.model
            );
            similarities.push({ i, j, similarity });
          }
        }

        // Check if all models are semantically equivalent (similarity > 0.9)
        const avgSimilarity = similarities.reduce((sum, s) => sum + s.similarity, 0) / similarities.length;

        if (avgSimilarity > 0.9) {
          // Merge models: take union of activities, aggregate metrics
          return this.mergeModels(validResults.map(r => r.result));
        }

        // Conflict: models differ significantly
        // Return result with highest fitness
        const bestResult = validResults
          .sort((a, b) => b.result.fitness - a.result.fitness)[0];

        span.addEvent('conflict_resolved', {
          'resolution': 'highest_fitness',
          'avg_similarity': avgSimilarity,
        });

        span.setStatus({ code: SpanStatusCode.OK });
        return bestResult.result;

      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Compute semantic similarity between two process models
   */
  async computeModelSimilarity(modelA, modelB) {
    // Extract activities and edges
    const activitiesA = new Set(modelA.activities || []);
    const activitiesB = new Set(modelB.activities || []);

    const edgesA = new Set((modelA.edges || []).map(e => JSON.stringify(e)));
    const edgesB = new Set((modelB.edges || []).map(e => JSON.stringify(e)));

    // Jaccard similarity for activities
    const activityIntersection = new Set([...activitiesA].filter(x => activitiesB.has(x)));
    const activityUnion = new Set([...activitiesA, ...activitiesB]);
    const activitySimilarity = activityIntersection.size / activityUnion.size;

    // Jaccard similarity for edges
    const edgeIntersection = new Set([...edgesA].filter(x => edgesB.has(x)));
    const edgeUnion = new Set([...edgesA, ...edgesB]);
    const edgeSimilarity = edgeIntersection.size / edgeUnion.size;

    // Weighted average (activities more important than edges)
    return 0.6 * activitySimilarity + 0.4 * edgeSimilarity;
  }

  /**
   * Merge multiple process models into one
   */
  mergeModels(results) {
    // Union of activities
    const activities = new Set();
    results.forEach(r => {
      (r.model.activities || []).forEach(a => activities.add(a));
    });

    // Majority voting for edges
    const edgeCounts = {};
    results.forEach(r => {
      (r.model.edges || []).forEach(e => {
        const key = JSON.stringify(e);
        edgeCounts[key] = (edgeCounts[key] || 0) + 1;
      });
    });

    const edges = Object.keys(edgeCounts)
      .filter(key => edgeCounts[key] >= results.length / 2)
      .map(key => JSON.parse(key));

    // Aggregate metrics (mean)
    const metrics = {
      fitness: results.reduce((sum, r) => sum + (r.fitness || 0), 0) / results.length,
      precision: results.reduce((sum, r) => sum + (r.precision || 0), 0) / results.length,
      generalization: results.reduce((sum, r) => sum + (r.generalization || 0), 0) / results.length,
      simplicity: results.reduce((sum, r) => sum + (r.simplicity || 0), 0) / results.length,
    };

    return {
      model: {
        activities: Array.from(activities),
        edges,
        type: 'merged',
      },
      ...metrics,
      source_results: results.map(r => r.resultId),
      merge_type: 'semantic_aggregation',
    };
  }

  async shutdown() {
    await this.consensus.shutdown();
    await this.replication.shutdown();
  }
}

export function createProcessMiningFederationCoordinator(config) {
  return new ProcessMiningFederationCoordinator(config);
}
```

## Testing Strategy

### 1. Unit Tests

- **RAFT Integration**: Mock leader election, log replication
- **Quorum Voting**: Test M-of-N thresholds, approval/rejection
- **SHACL Validation**: Test shape constraints, value ranges
- **Receipt Chain**: Test chain integrity, break detection

### 2. Integration Tests

- **Multi-node setup**: Spin up 3 local federation nodes
- **End-to-end discovery**: Client request → consensus → result
- **Conflict scenarios**: Inject different models, verify resolution
- **Network partitions**: Test behavior during node failure

### 3. Property-Based Tests

- **Receipt chain immutability**: Tampering should break validation
- **Quorum convergence**: All nodes should reach same consensus
- **BLAKE3 determinism**: Same input → same hash

## Deployment Considerations

### Node Configuration

```yaml
# pictl-federation/config.yaml
node:
  id: "pictl-node-1"
  role: "leader"  # or "follower"
  endpoint: "http://node-1:8080"

federation:
  peers:
    - id: "pictl-node-2"
      endpoint: "http://node-2:8080"
    - id: "pictl-node-3"
      endpoint: "http://node-3:8080"

quorum:
  threshold: 2  # M-of-N
  timeout_ms: 30000

shacl:
  shapes:
    - "urn:pictl:FitnessShape"
    - "urn:pictl:PrecisionShape"
    - "urn:pictl:GeneralizationShape"

mining:
  algorithms:
    - "inductive"
    - "heuristics"
    - "alpha"
  default_algorithm: "inductive"
```

### Monitoring

- **RAFT metrics**: election count, term changes, log replication lag
- **Quorum metrics**: proposal rate, approval rate, consensus time
- **Receipt metrics**: chain length, validation failures
- **Mining metrics**: discovery duration, quality scores

## Security Considerations

1. **Receipt Chain Integrity**: BLAKE3 hashes prevent tampering
2. **SHACL Validation**: Reject results violating shape constraints
3. **RAFT Authentication**: Secure peer-to-peer communication
4. **Quorum Sybil Resistance**: Each node has unique ID, authenticated via TLS

## References

- **RAFT**: Ongaro & Okiaro, "In Search of an Understandable Consensus Algorithm" (2014)
- **Van der Aalst**: "Process Mining: Data Science in Action" (2016)
- **BLAKE3**: JPEG, "The BLAKE3 Hash Function" (2021)
- **SHACL**: W3C, "Shapes Constraint Language" (2017)
