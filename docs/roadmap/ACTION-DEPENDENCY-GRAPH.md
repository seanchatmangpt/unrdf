# UNRDF 2028 Action Dependency Graph

> Visual representation of GOAP action sequences, dependencies, and parallel execution opportunities

**Version:** 1.0.0
**Generated:** 2025-11-18
**Related:** UNRDF-2028-GOAP-ROADMAP.md

---

## Overview

This document visualizes the dependency relationships between all 30 actions across the 5 transformational goals. It identifies:
- **Critical paths** (longest sequential chains)
- **Parallel execution** opportunities
- **Bottlenecks** and resource constraints
- **Optimal scheduling** for 2-year execution

---

## Legend

```
[Action] → Depends on previous action
[Action] ⇉ Can execute in parallel
[Action] ⚠️ Critical path (blocking)
[Action] ✓ Non-blocking (parallel)
```

---

## Goal 1: AI-Powered Knowledge Platform

### Action Sequence

```
v3.1.1 (Current State)
  ↓
[1.1] integrate_vector_db ⚠️
  │ Cost: 5, Duration: 2 weeks
  │ Team: AI Platform Engineer
  ↓
[1.2] generate_embeddings ⚠️
  │ Cost: 3, Duration: 1 week
  │ Team: ML Engineer
  │ Precondition: vector_db_integrated
  ↓
[1.3] build_vector_search ⚠️
  │ Cost: 4, Duration: 2 weeks
  │ Team: ML Engineer + Backend Engineer
  │ Precondition: embeddings_available
  ↓
[1.4] train_query_optimizer
  │ Cost: 7, Duration: 4 weeks
  │ Team: Data Scientist + ML Engineer
  │ Precondition: vector_search_enabled
  ↓
[1.5] implement_neural_reasoning
  │ Cost: 8, Duration: 6 weeks
  │ Team: 2x ML Engineers
  │ Precondition: ai_query_optimization
  ↓
[1.6] build_ontology_generator
  │ Cost: 6, Duration: 4 weeks
  │ Team: ML Engineer + Data Scientist
  │ Precondition: neural_reasoning_enabled
  ↓
✓ GOAL 1 ACHIEVED (Total: 19 weeks critical path)
```

**Critical Path:** 19 weeks (assuming sequential execution)
**Parallelization:** None within Goal 1 (linear dependency chain)
**Bottleneck:** Action 1.5 (6 weeks, requires 2 ML engineers)

---

## Goal 2: Distributed Federation

### Action Sequence

```
v3.1.1 (Current State)
  ↓
[2.1] setup_multi_node_cluster ⚠️
  │ Cost: 5, Duration: 3 weeks
  │ Team: SRE + 2x Backend Engineers
  ↓
[2.2] implement_consensus ⚠️
  │ Cost: 8, Duration: 6 weeks
  │ Team: 2x Senior Backend Engineers
  │ Precondition: multi_node_cluster
  ↓
[2.3] enable_data_replication ⚠️
  │ Cost: 6, Duration: 4 weeks
  │ Team: 2x Backend Engineers
  │ Precondition: consensus_protocol_enabled
  ↓
[2.4] build_federated_query_engine
  │ Cost: 7, Duration: 5 weeks
  │ Team: 2x Backend Engineers + Performance Engineer
  │ Precondition: data_replication_active
  ↓
[2.5] setup_global_deployment
  │ Cost: 5, Duration: 3 weeks
  │ Team: DevOps + SRE
  │ Precondition: federated_queries_enabled
  ↓
✓ GOAL 2 ACHIEVED (Total: 21 weeks critical path)
```

**Critical Path:** 21 weeks (longest sequential chain across all goals)
**Parallelization:** None within Goal 2 (linear dependency)
**Bottleneck:** Action 2.2 (6 weeks, complex consensus implementation)

---

## Goal 3: Real-Time Streaming

### Action Sequence

```
v3.1.1 (Current State)
  ↓
[3.1] integrate_kafka ⚠️
  │ Cost: 4, Duration: 2 weeks
  │ Team: Backend Engineer + DevOps
  ↓
[3.2] implement_cdc ⚠️
  │ Cost: 5, Duration: 3 weeks
  │ Team: 2x Backend Engineers
  │ Precondition: event_streaming_enabled
  ↓
[3.3] build_graphql_subscriptions ✓
  │ Cost: 6, Duration: 4 weeks
  │ Team: Backend Engineer + Frontend Engineer
  │ Precondition: change_data_capture
  ↓
  ├──→ [3.4] add_websocket_support ✓
  │     │ Cost: 3, Duration: 2 weeks
  │     │ Team: Backend Engineer
  │     │ Precondition: subscriptions_enabled
  │     ↓
  │     └──→ [3.5] optimize_event_latency
  │           Cost: 4, Duration: 2 weeks
  │           Team: Performance Engineer
  │           Precondition: websocket_realtime
  ↓
✓ GOAL 3 ACHIEVED (Total: 13 weeks critical path)
```

**Critical Path:** 13 weeks
**Parallelization:** Actions 3.3 and 3.4 can partially overlap
**Bottleneck:** Action 3.3 (4 weeks, GraphQL subscriptions complexity)

---

## Goal 4: Web3 Integration

### Action Sequence

```
v3.1.1 (Current State)
  ↓
[4.1] integrate_ethereum ⚠️
  │ Cost: 6, Duration: 4 weeks
  │ Team: Blockchain Engineer + Backend Engineer
  ↓
  ├──→ [4.2] add_ipfs_storage ✓
  │     │ Cost: 5, Duration: 3 weeks
  │     │ Team: Backend Engineer
  │     │ Precondition: blockchain_integration
  │     ↓
  │     └──→ [4.5] build_nft_marketplace
  │           Cost: 8, Duration: 6 weeks
  │           Team: Blockchain + Full-Stack Engineer
  │           Precondition: ipfs_integration
  │
  ├──→ [4.3] implement_did_support ✓
  │     │ Cost: 7, Duration: 5 weeks
  │     │ Team: Blockchain Engineer
  │     │ Precondition: blockchain_integration
  │     ↓
  │     └──→ [4.4] add_verifiable_credentials
  │           Cost: 6, Duration: 4 weeks
  │           Team: Blockchain Engineer
  │           Precondition: did_enabled
  ↓
✓ GOAL 4 ACHIEVED (Total: 13 weeks critical path, 15 weeks with parallel)
```

**Critical Path:** 15 weeks (parallel execution of two branches)
**Parallelization:**
- Branch 1: 4.1 → 4.2 → 4.5 (13 weeks)
- Branch 2: 4.1 → 4.3 → 4.4 (13 weeks)
- Both branches run in parallel after 4.1

**Bottleneck:** Action 4.5 (6 weeks, NFT marketplace UI/contracts)

---

## Goal 5: Enterprise Governance

### Action Sequence

```
v3.1.1 (Current State)
  ↓
[5.1] implement_multi_tenancy ⚠️
  │ Cost: 6, Duration: 4 weeks
  │ Team: 2x Backend Engineers
  ↓
[5.2] add_rbac ⚠️
  │ Cost: 5, Duration: 3 weeks
  │ Team: Backend Engineer + Security Engineer
  │ Precondition: multi_tenant_isolation
  ↓
[5.3] build_data_lineage
  │ Cost: 7, Duration: 5 weeks
  │ Team: 2x Backend Engineers
  │ Precondition: rbac_enabled
  ↓
[5.4] create_compliance_reports
  │ Cost: 6, Duration: 4 weeks
  │ Team: Backend Engineer + Compliance Specialist
  │ Precondition: lineage_tracking
  ↓
[5.5] setup_sla_monitoring
  │ Cost: 4, Duration: 2 weeks
  │ Team: SRE
  │ Precondition: compliance_certified
  ↓
✓ GOAL 5 ACHIEVED (Total: 18 weeks critical path)
```

**Critical Path:** 18 weeks
**Parallelization:** None within Goal 5 (linear dependency)
**Bottleneck:** Action 5.3 (5 weeks, data lineage complexity)

---

## Cross-Goal Dependency Analysis

### Independent Goals (Parallel Execution)

```
TIMELINE (Week 0 → Week 21)

Week  0 ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━→ 21
      │
Goal1 ├──[1.1]──┤[1.2]┤───[1.3]───┤──────[1.4]──────┤────────[1.5]────────┤──────[1.6]──────┤
      │  (2w)   (1w)      (2w)           (4w)               (6w)               (4w)
      │  ⚠️                                                  BOTTLENECK
      │
Goal2 ├────[2.1]────┤────────[2.2]────────┤──────[2.3]──────┤───────[2.4]───────┤────[2.5]────┤
      │    (3w)            (6w)                 (4w)               (5w)             (3w)
      │                   ⚠️ CRITICAL PATH (LONGEST)
      │
Goal3 ├──[3.1]──┤────[3.2]────┤──────[3.3]──────┤──[3.4]──┤──[3.5]──┤
      │  (2w)       (3w)            (4w)           (2w)      (2w)
      │
Goal4 ├──────[4.1]──────┤
      │      (4w)        ├───[4.2]───┤──────────[4.5]──────────┤
      │                  │   (3w)            (6w)
      │                  │
      │                  └───[4.3]───────┤──────[4.4]──────┤
      │                      (5w)              (4w)
      │                     (Parallel branches)
      │
Goal5 ├──────[5.1]──────┤────[5.2]────┤───────[5.3]───────┤──────[5.4]──────┤──[5.5]──┤
      │      (4w)            (3w)             (5w)               (4w)          (2w)
      │
      └────────────────────────────────────────────────────────────────────────────────→
```

### Resource Allocation by Timeline

**Q1 2025 (Weeks 1-13):**
```
Goal 1: integrate_vector_db → generate_embeddings → build_vector_search → [start train_query_optimizer]
Goal 2: setup_multi_node_cluster → implement_consensus (ongoing)
Goal 3: integrate_kafka → implement_cdc → build_graphql_subscriptions → add_websocket_support
Goal 4: integrate_ethereum → add_ipfs_storage (parallel) + implement_did_support (parallel)
Goal 5: implement_multi_tenancy → add_rbac → build_data_lineage (start)

Team Utilization:
- AI/ML Team: Goal 1 (100%)
- Distributed Systems Team: Goal 2 (100%)
- Real-Time Team: Goal 3 (100%)
- Web3 Team: Goal 4 (100%)
- Enterprise Team: Goal 5 (100%)
```

**Q2 2025 (Weeks 14-26):**
```
Goal 1: train_query_optimizer (complete) → implement_neural_reasoning (start)
Goal 2: implement_consensus (complete) → enable_data_replication → build_federated_query_engine
Goal 3: optimize_event_latency → GOAL 3 COMPLETE ✓
Goal 4: add_verifiable_credentials → build_nft_marketplace
Goal 5: build_data_lineage (complete) → create_compliance_reports → setup_sla_monitoring

Team Utilization:
- AI/ML Team: Goal 1 (100%)
- Distributed Systems Team: Goal 2 (100%)
- Real-Time Team: IDLE (can support other goals)
- Web3 Team: Goal 4 (100%)
- Enterprise Team: Goal 5 (100%)
```

**Q3-Q4 2025 (Weeks 27-52):**
```
Goal 1: implement_neural_reasoning (complete) → build_ontology_generator
Goal 2: setup_global_deployment → GOAL 2 COMPLETE ✓
Goal 4: build_nft_marketplace (complete) → GOAL 4 COMPLETE ✓
Goal 5: setup_sla_monitoring (complete) → GOAL 5 COMPLETE ✓

Team Utilization:
- AI/ML Team: Goal 1 (50%)
- Other teams: Available for Phase 3-4 work
```

**Q1-Q4 2026 (Weeks 53-104):**
```
Goal 1: build_ontology_generator (complete) → GOAL 1 COMPLETE ✓
Phase 3: Web3 & Developer Experience
Phase 4: Enterprise & Intelligence (advanced AI features)

Team Utilization:
- Full team on developer tools, visual editor, advanced features
```

---

## Critical Path Summary

**Longest Sequential Path:** Goal 2 (Distributed Federation)
```
Duration: 21 weeks
Actions: 2.1 → 2.2 → 2.3 → 2.4 → 2.5
Bottleneck: implement_consensus (6 weeks, complexity)
Team Required: 4-6 distributed systems engineers
```

**Second Longest:** Goal 1 (AI-Powered Knowledge)
```
Duration: 19 weeks
Actions: 1.1 → 1.2 → 1.3 → 1.4 → 1.5 → 1.6
Bottleneck: implement_neural_reasoning (6 weeks, ML complexity)
Team Required: 3-5 ML engineers
```

**Third Longest:** Goal 5 (Enterprise Governance)
```
Duration: 18 weeks
Actions: 5.1 → 5.2 → 5.3 → 5.4 → 5.5
Bottleneck: build_data_lineage (5 weeks, tracking complexity)
Team Required: 3-4 backend + security engineers
```

---

## Parallelization Opportunities

### Maximum Parallelization (5 Goals Simultaneously)

**Assumptions:**
- 5 independent teams (AI, Distributed, Real-Time, Web3, Enterprise)
- No resource sharing between goals
- All teams fully staffed

**Total Duration:** 21 weeks (limited by Goal 2 critical path)

**Speedup:** 5x (vs sequential execution of 89 weeks)

### Resource-Constrained Parallelization

**Real-World Scenario:**
- Limited ML engineers (can't fully staff both Goal 1 and Goal 4 neural features)
- Shared DevOps team across goals
- Budget constraints limit simultaneous execution

**Recommended Approach:**
```
Phase 1 (Q1-Q2 2025): Goals 1, 2, 3 (Foundation)
  - Focus on AI, Federation, Real-Time
  - Duration: 26 weeks (includes some sequential work)

Phase 2 (Q3-Q4 2025): Goals 4, 5 (Security & Web3)
  - Start after Phase 1 completes or overlaps
  - Duration: 26 weeks

Total with 50% parallelization: ~40 weeks (vs 21 weeks full parallel, 89 weeks sequential)
```

---

## Bottleneck Identification

### Top 5 Bottlenecks (By Duration)

1. **Action 2.2: implement_consensus** (6 weeks)
   - Impact: Blocks entire Goal 2
   - Mitigation: Use proven libraries (etcd), hire consensus expert

2. **Action 1.5: implement_neural_reasoning** (6 weeks)
   - Impact: Blocks Goal 1 completion
   - Mitigation: Pre-trained models, phased rollout

3. **Action 4.5: build_nft_marketplace** (6 weeks)
   - Impact: Delays Goal 4 completion
   - Mitigation: Use existing marketplace contracts (OpenSea)

4. **Action 2.4: build_federated_query_engine** (5 weeks)
   - Impact: Delays Goal 2 deployment
   - Mitigation: Extend existing SPARQL engine incrementally

5. **Action 5.3: build_data_lineage** (5 weeks)
   - Impact: Blocks compliance work
   - Mitigation: Use existing lineage tools (Apache Atlas)

### Resource Bottlenecks

**ML Engineers:**
- Required for: Goal 1 (all actions), Goal 4 (partially)
- Peak demand: Q2-Q4 2025 (6 engineers needed)
- Mitigation: Hire early, use contractors

**Distributed Systems Engineers:**
- Required for: Goal 2 (all actions)
- Peak demand: Q3-Q4 2025 (6 engineers needed)
- Mitigation: Training program, consultants

**DevOps/SRE:**
- Required for: All goals (infrastructure)
- Peak demand: Continuous (2-3 engineers needed)
- Mitigation: Automation, managed services

---

## Dependency Constraints

### Hard Dependencies (Cannot Parallelize)

```
1.1 → 1.2 → 1.3 (embeddings require vector DB)
2.1 → 2.2 → 2.3 (consensus requires cluster)
3.1 → 3.2 (CDC requires Kafka)
5.1 → 5.2 (RBAC requires multi-tenancy)
```

### Soft Dependencies (Can Overlap)

```
1.4 ⇉ 1.5 (can start neural reasoning before query optimizer completes)
3.3 ⇉ 3.4 (WebSocket can start before GraphQL subscriptions fully done)
4.2 ⇉ 4.3 (IPFS and DID can proceed in parallel after blockchain)
```

---

## Optimal Execution Schedule

### Recommended Phasing

**Phase 1a (Weeks 1-13, Q1 2025):**
```
Priority: Foundation
- Goal 1: 1.1 → 1.2 → 1.3 (Vector search ready)
- Goal 2: 2.1 → 2.2 (start) (Cluster + consensus started)
- Goal 3: 3.1 → 3.2 → 3.3 (start) (Kafka + CDC ready)
- Goal 5: 5.1 → 5.2 (Multi-tenancy + RBAC)
```

**Phase 1b (Weeks 14-26, Q2 2025):**
```
Priority: Intelligence + Real-Time
- Goal 1: 1.4 (Query optimizer)
- Goal 2: 2.2 (complete) → 2.3 → 2.4 (start) (Consensus + replication)
- Goal 3: 3.3 (complete) → 3.4 → 3.5 (GraphQL + WebSocket)
- Goal 5: 5.3 (Data lineage)
```

**Phase 2a (Weeks 27-39, Q3 2025):**
```
Priority: Federation + Compliance
- Goal 2: 2.4 (complete) → 2.5 (Federated queries + global)
- Goal 5: 5.4 → 5.5 (Compliance + SLA)
```

**Phase 2b (Weeks 40-52, Q4 2025):**
```
Priority: Web3
- Goal 4: 4.1 → 4.2 → 4.3 (Ethereum + IPFS + DID)
```

**Phase 3 (Weeks 53-78, Q1-Q2 2026):**
```
Priority: Web3 + Developer Tools
- Goal 4: 4.4 → 4.5 (VC + NFT marketplace)
- Developer Experience: Visual editor, IDE tools
```

**Phase 4 (Weeks 79-104, Q3-Q4 2026):**
```
Priority: Advanced AI
- Goal 1: 1.5 → 1.6 (Neural reasoning + ontology)
- Enterprise: Advanced governance features
```

---

## Visualization: Gantt Chart (ASCII)

```
Action         Q1 2025    Q2 2025    Q3 2025    Q4 2025    Q1 2026    Q2 2026    Q3 2026    Q4 2026
             ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
1.1          ▓▓▓
1.2             ▓▓
1.3               ▓▓▓
1.4                  ▓▓▓▓▓▓
1.5                        ▓▓▓▓▓▓▓▓▓▓▓▓                                                        ▓▓▓▓▓▓▓▓▓
1.6                                                                                                      ▓▓▓▓▓▓
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
2.1          ▓▓▓▓
2.2              ▓▓▓▓▓▓▓▓▓▓
2.3                        ▓▓▓▓▓▓
2.4                              ▓▓▓▓▓▓▓▓
2.5                                      ▓▓▓▓
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
3.1          ▓▓▓
3.2             ▓▓▓▓
3.3                 ▓▓▓▓▓▓
3.4                       ▓▓▓
3.5                          ▓▓▓
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
4.1                                          ▓▓▓▓▓
4.2                                              ▓▓▓▓                  ▓▓▓▓▓▓▓▓▓▓
4.3                                              ▓▓▓▓▓▓▓
4.4                                                     ▓▓▓▓▓
4.5                                                               ▓▓▓▓▓▓▓▓▓▓▓▓
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
5.1          ▓▓▓▓▓
5.2               ▓▓▓▓
5.3                   ▓▓▓▓▓▓▓
5.4                          ▓▓▓▓▓▓
5.5                                ▓▓▓
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Legend: ▓ = Action in progress
```

---

## Conclusion

**Key Findings:**
1. **Critical Path:** 21 weeks (Goal 2: Distributed Federation)
2. **Maximum Parallelization:** 5x speedup with fully independent teams
3. **Realistic Timeline:** 40-52 weeks with resource constraints
4. **Top Bottleneck:** Consensus implementation (6 weeks)
5. **Resource Bottleneck:** ML engineers (6 needed at peak)

**Recommendations:**
1. **Prioritize hiring** distributed systems and ML engineers early
2. **Use proven libraries** for complex components (consensus, ML models)
3. **Execute Goals 1, 2, 3** in parallel during Q1-Q2 2025
4. **Defer Goal 4** (Web3) to Q4 2025 when teams free up
5. **Continuous monitoring** of dependencies to adjust schedule

---

**Document Status:** Final
**Owner:** Engineering Team
**Next Review:** Q1 2025 kickoff
