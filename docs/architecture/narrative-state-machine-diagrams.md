# Narrative State Machine - Visual Architecture

## System Overview (C4 Context Diagram)

```
                          ┌─────────────────────────────────────────┐
                          │   POST-CYBERPUNK GOVERNANCE SYSTEM      │
                          │                                         │
                          │  ┌───────────────────────────────────┐  │
   ┌──────────┐          │  │    Narrative State Machine        │  │          ┌──────────┐
   │  Human   │──────────┼─►│                                   │◄─┼──────────│  Sensor  │
   │  Agents  │  Vote/   │  │  • Universe Management            │  │  Data    │  Network │
   │          │  Action  │  │  • Scene Processing               │  │          │          │
   └──────────┘          │  │  • Receipt Generation             │  │          └──────────┘
                          │  │  • Bridge Translation             │  │
   ┌──────────┐          │  │  • Guard Enforcement              │  │          ┌──────────┐
   │  Smart   │──────────┼─►│                                   │◄─┼──────────│ External │
   │ Contracts│  Event   │  └───────────────┬───────────────────┘  │  Query   │   APIs   │
   │          │          │                  │                      │          │          │
   └──────────┘          │                  ▼                      │          └──────────┘
                          │  ┌───────────────────────────────────┐  │
                          │  │      RDF Triple Store             │  │
                          │  │   (@unrdf/oxigraph)               │  │
                          │  │                                   │  │
                          │  │  • Current Universe State         │  │
                          │  │  • SPARQL Query Endpoint          │  │
                          │  └───────────────────────────────────┘  │
                          │                                         │
                          │  ┌───────────────────────────────────┐  │
                          │  │   Immutable Receipt Ledger        │  │
                          │  │   (Content-Addressed Storage)     │  │
                          │  │                                   │  │
                          │  │  • Cryptographic Audit Trail      │  │
                          │  │  • Scene History (Git-like)       │  │
                          │  └───────────────────────────────────┘  │
                          │                                         │
                          └─────────────────────────────────────────┘
```

---

## Container Diagram (Internal Components)

```
┌───────────────────────────────────────────────────────────────────────────────┐
│                    NARRATIVE STATE MACHINE SYSTEM                             │
├───────────────────────────────────────────────────────────────────────────────┤
│                                                                               │
│  ┌─────────────────────────────────────────────────────────────────────────┐ │
│  │                         INGESTION LAYER                                 │ │
│  ├─────────────────────────────────────────────────────────────────────────┤ │
│  │                                                                         │ │
│  │  ┌─────────────┐   ┌─────────────┐   ┌─────────────┐                  │ │
│  │  │ Observation │   │  RDF Parser │   │   Schema    │                  │ │
│  │  │   Router    │──►│  (Turtle,   │──►│  Validator  │                  │ │
│  │  │             │   │   JSON-LD)  │   │   (SHACL)   │                  │ │
│  │  └─────────────┘   └─────────────┘   └──────┬──────┘                  │ │
│  │                                              │                         │ │
│  └──────────────────────────────────────────────┼─────────────────────────┘ │
│                                                 │ Validated Quads           │
│  ┌──────────────────────────────────────────────▼─────────────────────────┐ │
│  │                        GUARD LAYER (H)                                 │ │
│  ├─────────────────────────────────────────────────────────────────────────┤ │
│  │                                                                         │ │
│  │  ┌──────────────────────────────────────────────────────────────┐      │ │
│  │  │  Guard Registry                                              │      │ │
│  │  │  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐         │      │ │
│  │  │  │ AuthGuard    │ │ RateLimitGuard│ │CustomGuard  │         │      │ │
│  │  │  │              │ │              │ │             │  ...     │      │ │
│  │  │  │ (agent,      │ │ (per-agent   │ │ (user-      │         │      │ │
│  │  │  │  action,     │ │  quotas)     │ │  defined)   │         │      │ │
│  │  │  │  target)     │ │              │ │             │         │      │ │
│  │  │  │  → bool      │ │              │ │             │         │      │ │
│  │  │  └──────────────┘ └──────────────┘ └──────────────┘         │      │ │
│  │  └──────────────────────────────────────────────────────────────┘      │ │
│  │                                │                                       │ │
│  │                                │ GuardResult[]                         │ │
│  └────────────────────────────────┼───────────────────────────────────────┘ │
│                                   │                                         │
│                                   │ If ALL pass                             │
│  ┌────────────────────────────────▼───────────────────────────────────────┐ │
│  │                    RECONCILIATION LAYER (μ)                            │ │
│  ├─────────────────────────────────────────────────────────────────────────┤ │
│  │                                                                         │ │
│  │  ┌────────────────────────────────────────────────────────────────┐    │ │
│  │  │  Reconciliation Engine                                         │    │ │
│  │  │                                                                 │    │ │
│  │  │  1. Load Current State ──┐                                     │    │ │
│  │  │                           ▼                                     │    │ │
│  │  │                    ┌──────────────┐                            │    │ │
│  │  │  2. Compute       │ Triple Store │                            │    │ │
│  │  │     Minimal ◄─────┤  (Current    │                            │    │ │
│  │  │     Delta         │   Universe)  │                            │    │ │
│  │  │                    └──────────────┘                            │    │ │
│  │  │         │                                                      │    │ │
│  │  │         ▼                                                      │    │ │
│  │  │  3. Apply Delta ──► New State (provisional)                   │    │ │
│  │  │         │                                                      │    │ │
│  │  │         ▼                                                      │    │ │
│  │  │  4. Check Invariants (Q) ──► SPARQL Queries                   │    │ │
│  │  │         │                                                      │    │ │
│  │  │         ▼                                                      │    │ │
│  │  │  5. Generate Artifacts ──► Deterministic Outputs              │    │ │
│  │  │         │                                                      │    │ │
│  │  │         ▼                                                      │    │ │
│  │  │    Consequence{status, graph, checks, sideEffectTokens}       │    │ │
│  │  └────────────────────────────────────────────────────────────────┘    │ │
│  │                                │                                       │ │
│  └────────────────────────────────┼───────────────────────────────────────┘ │
│                                   │                                         │
│                                   │ Consequence                             │
│  ┌────────────────────────────────▼───────────────────────────────────────┐ │
│  │                     INVARIANT CHECKER (Q)                              │ │
│  ├─────────────────────────────────────────────────────────────────────────┤ │
│  │                                                                         │ │
│  │  ┌──────────────────────────────────────────────────────────────┐      │ │
│  │  │  For each invariant in Q:                                    │      │ │
│  │  │                                                               │      │ │
│  │  │  ┌─────────────────────────────────────────────┐             │      │ │
│  │  │  │  SPARQL ASK Query                           │             │      │ │
│  │  │  │  ┌──────────────────────────────────────┐   │             │      │ │
│  │  │  │  │  "Total votes = sum of individual    │   │             │      │ │
│  │  │  │  │   votes"                             │   │             │      │ │
│  │  │  │  └──────────────────────────────────────┘   │             │      │ │
│  │  │  │                  │                           │             │      │ │
│  │  │  │                  ▼                           │             │      │ │
│  │  │  │  Execute against new_state                  │             │      │ │
│  │  │  │                  │                           │             │      │ │
│  │  │  │                  ▼                           │             │      │ │
│  │  │  │  TRUE ──► Pass                              │             │      │ │
│  │  │  │  FALSE ──► Check severity                   │             │      │ │
│  │  │  │              │                               │             │      │ │
│  │  │  │              ├─► 'error' ──► REJECT         │             │      │ │
│  │  │  │              └─► 'warning' ──► LOG          │             │      │ │
│  │  │  └─────────────────────────────────────────────┘             │      │ │
│  │  └──────────────────────────────────────────────────────────────┘      │ │
│  │                                │                                       │ │
│  └────────────────────────────────┼───────────────────────────────────────┘ │
│                                   │                                         │
│                                   │ InvariantCheckResult[]                  │
│  ┌────────────────────────────────▼───────────────────────────────────────┐ │
│  │                       RECEIPT GENERATOR                                │ │
│  ├─────────────────────────────────────────────────────────────────────────┤ │
│  │                                                                         │ │
│  │  ┌────────────────────────────────────────────────────────────────┐    │ │
│  │  │  1. Hash Scene Content                                         │    │ │
│  │  │     sha256(observations || delta || universe_id)               │    │ │
│  │  │                                                                 │    │ │
│  │  │  2. Build Receipt Payload:                                     │    │ │
│  │  │     {                                                           │    │ │
│  │  │       sceneId: "sha256:...",                                   │    │ │
│  │  │       timestamp: 1703674801,                                   │    │ │
│  │  │       admissibilityChecks: [...GuardResults],                  │    │ │
│  │  │       invariantChecks: [...InvariantResults],                  │    │ │
│  │  │       minimalityProof: {...},                                  │    │ │
│  │  │       forkParents: ["sha256:..."]                              │    │ │
│  │  │     }                                                           │    │ │
│  │  │                                                                 │    │ │
│  │  │  3. Sign with Dual Keys:                                       │    │ │
│  │  │     - Universe authority key (system)                          │    │ │
│  │  │     - Scene author key (agent)                                 │    │ │
│  │  │                                                                 │    │ │
│  │  │  4. Compute Receipt Hash                                       │    │ │
│  │  │     sha256(canonical(receipt))                                 │    │ │
│  │  └────────────────────────────────────────────────────────────────┘    │ │
│  │                                │                                       │ │
│  └────────────────────────────────┼───────────────────────────────────────┘ │
│                                   │                                         │
│                                   │ Signed Receipt                          │
│  ┌────────────────────────────────▼───────────────────────────────────────┐ │
│  │                       STORAGE & OUTPUT LAYER                           │ │
│  ├─────────────────────────────────────────────────────────────────────────┤ │
│  │                                                                         │ │
│  │  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐     │ │
│  │  │  Update Triple   │  │  Append Receipt  │  │  Emit Artifacts  │     │ │
│  │  │  Store           │  │  to Log          │  │                  │     │ │
│  │  │                  │  │                  │  │  - Files         │     │ │
│  │  │  Apply delta     │  │  receipts.ndjson │  │  - Notifications │     │ │
│  │  │  (add/delete     │  │  (append-only)   │  │  - Webhooks      │     │ │
│  │  │   quads)         │  │                  │  │                  │     │ │
│  │  └──────────────────┘  └──────────────────┘  └──────────────────┘     │ │
│  │                                                                         │ │
│  │  ┌──────────────────────────────────────────────────────────────┐      │ │
│  │  │  Side Effect Executor (Async, Non-blocking)                  │      │ │
│  │  │                                                               │      │ │
│  │  │  For each token in sideEffectTokens:                         │      │ │
│  │  │    - Email sender                                            │      │ │
│  │  │    - Webhook dispatcher                                      │      │ │
│  │  │    - Event bus publisher                                     │      │ │
│  │  │                                                               │      │ │
│  │  │  Log: {receiptId, token, timestamp, result}                 │      │ │
│  │  └──────────────────────────────────────────────────────────────┘      │ │
│  │                                                                         │ │
│  └─────────────────────────────────────────────────────────────────────────┘ │
│                                                                               │
└───────────────────────────────────────────────────────────────────────────────┘
```

---

## Component Diagram (Reconciliation Engine Detail)

```
┌───────────────────────────────────────────────────────────────────────┐
│                      RECONCILIATION ENGINE (μ)                        │
│                                                                       │
│  Input: Observation[] (validated quads)                              │
│  Output: Consequence{status, graph, checks, tokens}                  │
├───────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │  PHASE 1: State Loading                                         │ │
│  ├─────────────────────────────────────────────────────────────────┤ │
│  │                                                                 │ │
│  │  ┌──────────────────────┐                                      │ │
│  │  │  Triple Store        │                                      │ │
│  │  │  (@unrdf/oxigraph)   │                                      │ │
│  │  │                      │                                      │ │
│  │  │  current_state =     │                                      │ │
│  │  │    store.match(      │                                      │ │
│  │  │      null,           │  ← Load all quads in universe graph │ │
│  │  │      null,           │                                      │ │
│  │  │      null,           │                                      │ │
│  │  │      universe_graph) │                                      │ │
│  │  │                      │                                      │ │
│  │  └──────────┬───────────┘                                      │ │
│  │             │                                                  │ │
│  │             ▼                                                  │ │
│  │      current_state: Quad[]                                    │ │
│  └─────────────────────────────────────────────────────────────────┘ │
│                                                                       │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │  PHASE 2: Delta Computation                                    │ │
│  ├─────────────────────────────────────────────────────────────────┤ │
│  │                                                                 │ │
│  │  ┌──────────────────────────────────────────────────────────┐  │ │
│  │  │  Graph Diff Algorithm                                    │  │ │
│  │  │                                                           │  │ │
│  │  │  1. Convert observations to desired_state:               │  │ │
│  │  │     - Extract quads from Observation[]                   │  │ │
│  │  │     - Apply business rules (e.g., increment counters)    │  │ │
│  │  │                                                           │  │ │
│  │  │  2. Compute set difference:                              │  │ │
│  │  │     additions = desired_state ∖ current_state            │  │ │
│  │  │     deletions = current_state ∖ desired_state            │  │ │
│  │  │                                                           │  │ │
│  │  │  3. Minimize delta:                                      │  │ │
│  │  │     - Remove redundant quads                             │  │ │
│  │  │     - Apply domain-specific optimizations               │  │ │
│  │  │     - Generate minimality proof                          │  │ │
│  │  │                                                           │  │ │
│  │  │  4. Hash delta:                                          │  │ │
│  │  │     delta_hash = sha256(canonical(additions||deletions)) │  │ │
│  │  │                                                           │  │ │
│  │  └──────────────────────────────────────────────────────────┘  │ │
│  │                                │                               │ │
│  │                                ▼                               │ │
│  │                  Delta{additions, deletions, hash, proof}      │ │
│  └─────────────────────────────────────────────────────────────────┘ │
│                                                                       │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │  PHASE 3: Provisional State Application                       │ │
│  ├─────────────────────────────────────────────────────────────────┤ │
│  │                                                                 │ │
│  │  new_state = clone(current_state)                              │ │
│  │                                                                 │ │
│  │  for quad in delta.deletions:                                  │ │
│  │    new_state.remove(quad)                                      │ │
│  │                                                                 │ │
│  │  for quad in delta.additions:                                  │ │
│  │    new_state.add(quad)                                         │ │
│  │                                                                 │ │
│  │  ──► new_state (not yet committed)                             │ │
│  │                                                                 │ │
│  └─────────────────────────────────────────────────────────────────┘ │
│                                                                       │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │  PHASE 4: Invariant Validation                                │ │
│  ├─────────────────────────────────────────────────────────────────┤ │
│  │                                                                 │ │
│  │  invariant_results = []                                        │ │
│  │                                                                 │ │
│  │  for invariant in universe.invariants.rules:                   │ │
│  │    result = execute_sparql_ask(                                │ │
│  │      query: invariant.sparql,                                  │ │
│  │      graph: new_state                                          │ │
│  │    )                                                            │ │
│  │                                                                 │ │
│  │    if result == false AND severity == 'error':                 │ │
│  │      return Consequence{                                       │ │
│  │        status: 'rejected',                                     │ │
│  │        reason: invariant.description,                          │ │
│  │        ...                                                      │ │
│  │      }                                                          │ │
│  │                                                                 │ │
│  │    invariant_results.push({                                    │ │
│  │      invariantId: invariant.id,                                │ │
│  │      satisfied: result,                                        │ │
│  │      ...                                                        │ │
│  │    })                                                           │ │
│  │                                                                 │ │
│  └─────────────────────────────────────────────────────────────────┘ │
│                                                                       │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │  PHASE 5: Artifact Generation                                 │ │
│  ├─────────────────────────────────────────────────────────────────┤ │
│  │                                                                 │ │
│  │  artifacts = []                                                │ │
│  │                                                                 │ │
│  │  // Domain-specific artifact generators                        │ │
│  │  if universe.type == 'governance':                             │ │
│  │    artifacts.push(                                             │ │
│  │      generate_vote_ledger(new_state),                          │ │
│  │      generate_metrics_json(new_state)                          │ │
│  │    )                                                            │ │
│  │                                                                 │ │
│  │  // Generic artifacts (always generated)                       │ │
│  │  artifacts.push(                                               │ │
│  │    {                                                            │ │
│  │      type: 'application/n-quads',                              │ │
│  │      data: serialize_nquads(delta)                             │ │
│  │    }                                                            │ │
│  │  )                                                              │ │
│  │                                                                 │ │
│  └─────────────────────────────────────────────────────────────────┘ │
│                                                                       │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │  PHASE 6: Side Effect Token Generation                        │ │
│  ├─────────────────────────────────────────────────────────────────┤ │
│  │                                                                 │ │
│  │  side_effect_tokens = []                                       │ │
│  │                                                                 │ │
│  │  // Extract triggers from observations                         │ │
│  │  if observations.some(o => o.quad.predicate == 'ex:votedFor'): │ │
│  │    side_effect_tokens.push({                                   │ │
│  │      type: 'email',                                            │ │
│  │      to: extract_voter_email(observations),                    │ │
│  │      template: 'vote-confirmation',                            │ │
│  │      data: {proposal_id: '...'}                                │ │
│  │    })                                                           │ │
│  │                                                                 │ │
│  │  // Webhook notifications                                      │ │
│  │  if universe.config.webhooks:                                  │ │
│  │    side_effect_tokens.push({                                   │ │
│  │      type: 'webhook',                                          │ │
│  │      url: universe.config.webhooks.url,                        │ │
│  │      payload: serialize_delta(delta)                           │ │
│  │    })                                                           │ │
│  │                                                                 │ │
│  └─────────────────────────────────────────────────────────────────┘ │
│                                                                       │
│  ┌─────────────────────────────────────────────────────────────────┐ │
│  │  RETURN: Consequence                                           │ │
│  ├─────────────────────────────────────────────────────────────────┤ │
│  │                                                                 │ │
│  │  return {                                                       │ │
│  │    status: 'accepted',                                         │ │
│  │    resultingGraph: new_state,                                  │ │
│  │    invariantChecks: invariant_results,                         │ │
│  │    sideEffectTokens: side_effect_tokens,                       │ │
│  │    artifacts: artifacts,                                       │ │
│  │    delta: delta                                                │ │
│  │  }                                                              │ │
│  │                                                                 │ │
│  └─────────────────────────────────────────────────────────────────┘ │
│                                                                       │
└───────────────────────────────────────────────────────────────────────┘
```

---

## Sequence Diagram (Complete Scene Processing)

```
Agent     Guard     Reconcile   Invariant   Receipt    Storage    SideEffect
  │         │           │           │          │          │          │
  │ Observe │           │           │          │          │          │
  ├────────►│           │           │          │          │          │
  │         │           │           │          │          │          │
  │         │ Check     │           │          │          │          │
  │         │ Authz     │           │          │          │          │
  │         ├──────┐    │           │          │          │          │
  │         │      │    │           │          │          │          │
  │         │◄─────┘    │           │          │          │          │
  │         │           │           │          │          │          │
  │         │ PASS      │           │          │          │          │
  │         ├──────────►│           │          │          │          │
  │         │           │           │          │          │          │
  │         │           │ Load State│          │          │          │
  │         │           ├──────────►│          │          │          │
  │         │           │           │          │          │          │
  │         │           │◄──────────┤          │          │          │
  │         │           │ Compute Δ │          │          │          │
  │         │           ├──────┐    │          │          │          │
  │         │           │      │    │          │          │          │
  │         │           │◄─────┘    │          │          │          │
  │         │           │           │          │          │          │
  │         │           │ Check Q   │          │          │          │
  │         │           ├──────────►│          │          │          │
  │         │           │           │ Execute  │          │          │
  │         │           │           │ SPARQL   │          │          │
  │         │           │           ├─────┐    │          │          │
  │         │           │           │     │    │          │          │
  │         │           │           │◄────┘    │          │          │
  │         │           │           │          │          │          │
  │         │           │◄──────────┤ VALID    │          │          │
  │         │           │           │          │          │          │
  │         │           │ Generate  │          │          │          │
  │         │           │ Artifacts │          │          │          │
  │         │           ├──────┐    │          │          │          │
  │         │           │      │    │          │          │          │
  │         │           │◄─────┘    │          │          │          │
  │         │           │           │          │          │          │
  │         │           │ Consequence│          │          │          │
  │         │           ├───────────┼─────────►│          │          │
  │         │           │           │          │          │          │
  │         │           │           │          │ Hash     │          │
  │         │           │           │          │ Scene    │          │
  │         │           │           │          ├─────┐    │          │
  │         │           │           │          │     │    │          │
  │         │           │           │          │◄────┘    │          │
  │         │           │           │          │          │          │
  │         │           │           │          │ Sign     │          │
  │         │           │           │          │ (dual)   │          │
  │         │           │           │          ├─────┐    │          │
  │         │           │           │          │     │    │          │
  │         │           │           │          │◄────┘    │          │
  │         │           │           │          │          │          │
  │         │           │           │          │ Receipt  │          │
  │         │           │           │          ├─────────►│          │
  │         │           │           │          │          │          │
  │         │           │           │          │          │ Persist  │
  │         │           │           │          │          │ Δ to     │
  │         │           │           │          │          │ Store    │
  │         │           │           │          │          ├─────┐    │
  │         │           │           │          │          │     │    │
  │         │           │           │          │          │◄────┘    │
  │         │           │           │          │          │          │
  │         │           │           │          │          │ Append   │
  │         │           │           │          │          │ Receipt  │
  │         │           │           │          │          ├─────┐    │
  │         │           │           │          │          │     │    │
  │         │           │           │          │          │◄────┘    │
  │         │           │           │          │          │          │
  │         │           │           │          │          │ Execute  │
  │         │           │           │          │          │ Tokens   │
  │         │           │           │          │          ├─────────►│
  │         │           │           │          │          │          │
  │         │           │           │          │          │          │ Email
  │         │           │           │          │          │          │ Send
  │         │           │           │          │          │          ├────┐
  │         │           │           │          │          │          │    │
  │         │           │           │          │          │          │◄───┘
  │         │           │           │          │          │          │
  │         │           │           │          │          │◄─────────┤ Done
  │         │           │           │          │          │          │
  │◄────────┴───────────┴───────────┴──────────┴──────────┤          │
  │                   Receipt (signed)                    │          │
  │                                                        │          │
```

---

## Bridge Architecture (Cross-Universe Communication)

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         BRIDGE SYSTEM (Φ)                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ┌───────────────────┐          ┌───────────────────┐                  │
│  │  Universe A       │          │  Universe B       │                  │
│  │  (Governance)     │          │  (Archival)       │                  │
│  │                   │          │                   │                  │
│  │  Schema:          │          │  Schema:          │                  │
│  │   ex:Person       │          │   foaf:Person     │                  │
│  │   ex:votedFor     │          │   dc:creator      │                  │
│  │                   │          │                   │                  │
│  │  Invariants:      │          │  Invariants:      │                  │
│  │   Q1: "1 vote/    │          │   Q1: "Immutable  │                  │
│  │        person"    │          │        records"   │                  │
│  │                   │          │                   │                  │
│  │  Guards:          │          │  Guards:          │                  │
│  │   H1: "Registered │          │   H1: "Public     │                  │
│  │        voters"    │          │        read-only" │                  │
│  └─────────┬─────────┘          └─────────▲─────────┘                  │
│            │                              │                            │
│            │                              │                            │
│            │   ┌──────────────────────┐   │                            │
│            │   │  Bridge Φ            │   │                            │
│            │   │  (A → B)             │   │                            │
│            │   │                      │   │                            │
│            │   │  ┌────────────────┐  │   │                            │
│            │   │  │ Type Coercion  │  │   │                            │
│            └──►│  │                │  ├───┘                            │
│                │  │ ex:Person      │  │                                │
│                │  │   → foaf:Person│  │                                │
│                │  │                 │  │                                │
│                │  │ ex:votedFor    │  │                                │
│                │  │   → dc:relation│  │                                │
│                │  └────────────────┘  │                                │
│                │                      │                                │
│                │  ┌────────────────┐  │                                │
│                │  │ Invariant      │  │                                │
│                │  │ Preservation   │  │                                │
│                │  │                │  │                                │
│                │  │ Q1_A ⟹ Q1_B   │  │                                │
│                │  │                │  │                                │
│                │  │ Proof:         │  │                                │
│                │  │  "1 vote/person│  │                                │
│                │  │   guarantees   │  │                                │
│                │  │   immutable    │  │                                │
│                │  │   record count"│  │                                │
│                │  └────────────────┘  │                                │
│                │                      │                                │
│                │  ┌────────────────┐  │                                │
│                │  │ Access Grants  │  │                                │
│                │  │                │  │                                │
│                │  │ Bypass H1_B    │  │                                │
│                │  │ (public read   │  │                                │
│                │  │  restriction)  │  │                                │
│                │  │                │  │                                │
│                │  │ Reason:        │  │                                │
│                │  │  "Archives are │  │                                │
│                │  │   public       │  │                                │
│                │  │   record"      │  │                                │
│                │  └────────────────┘  │                                │
│                │                      │                                │
│                │  ┌────────────────┐  │                                │
│                │  │ Signatures     │  │                                │
│                │  │                │  │                                │
│                │  │ Universe A:    │  │                                │
│                │  │  sig_A         │  │                                │
│                │  │                │  │                                │
│                │  │ Universe B:    │  │                                │
│                │  │  sig_B         │  │                                │
│                │  │                │  │                                │
│                │  │ Witness:       │  │                                │
│                │  │  sig_W         │  │                                │
│                │  └────────────────┘  │                                │
│                └──────────────────────┘                                │
│                                                                         │
│  EXECUTION FLOW:                                                       │
│  ──────────────                                                        │
│                                                                         │
│  1. Scene exits Universe A with Receipt_A                              │
│  2. Bridge receives Receipt_A                                          │
│  3. Validate Receipt_A signatures                                      │
│  4. Transform quads: Σ_A → Σ_B                                         │
│  5. Prove Q_A ⟹ Q_B (invariant preservation)                          │
│  6. Apply access grants (bypass incompatible guards)                   │
│  7. Submit transformed scene to Universe B                             │
│  8. Universe B validates and generates Receipt_B                       │
│  9. Bridge links Receipt_A ↔ Receipt_B                                 │
│ 10. Return BridgeProof with dual receipts                              │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Data Model (Entity-Relationship)

```
┌──────────────┐
│  Universe    │
├──────────────┤
│ id (hash)    │◄────────────┐
│ schema       │              │
│ reconcile    │              │ parent_id
│ invariants   │              │
│ guards       │              │
│ metadata     │              │
└──────┬───────┘              │
       │ 1                    │
       │ universe_id          │
       │                      │
       │ N              ┌─────┴────────┐
       ▼                │ UniverseFork │
┌──────────────┐        ├──────────────┤
│    Scene     │        │ id           │
├──────────────┤        │ parent_id    │
│ id (hash)    │        │ child_id     │
│ universe_id  │────────┤ forked_at    │
│ observations │        │ reason       │
│ delta        │        └──────────────┘
│ consequences │
│ artifacts    │
│ receipt      │
│ metadata     │
└──────┬───────┘
       │ 1
       │ scene_id
       │
       │ 1
       ▼
┌──────────────┐
│   Receipt    │
├──────────────┤
│ id (hash)    │
│ scene_id     │
│ timestamp    │
│ admissibility│◄───┐
│ minimality   │    │
│ fork_parents │    │
│ signatures   │    │ receipt_id
└──────┬───────┘    │
       │ 1          │
       │            │ N
       │ N    ┌─────┴────────┐
       ▼      │  GuardCheck  │
┌──────────────┐├──────────────┤
│ Artifact     ││ id           │
├──────────────┤│ receipt_id   │
│ id (hash)    ││ guard_id     │
│ scene_id     ││ allowed      │
│ type         ││ deny_reason  │
│ data         ││ timestamp    │
│ generated_by │└──────────────┘
│ dependencies │
└──────────────┘
       │
       │ source_artifact
       │
       │ N
       ▼
┌──────────────┐
│ BridgeProof  │
├──────────────┤
│ id (hash)    │
│ source_univ  │
│ target_univ  │
│ type_coerce  │
│ invariant    │
│ access_grant │
│ validity     │
└──────────────┘
       │
       │ bridge_id
       │
       │ N
       ▼
┌──────────────────┐
│ BridgeExecution  │
├──────────────────┤
│ id               │
│ bridge_id        │
│ receipt_A_id     │
│ receipt_B_id     │
│ timestamp        │
│ success          │
└──────────────────┘
```

---

## Storage Layout (File System)

```
/unrdf-data/
│
├── universes/
│   ├── sha256-abc123.../            # Universe directory
│   │   ├── metadata.json            # Universe definition
│   │   ├── schema.ttl               # SHACL shapes
│   │   ├── invariants/
│   │   │   ├── q1-vote-once.sparql
│   │   │   ├── q2-total-count.sparql
│   │   │   └── ...
│   │   ├── guards/
│   │   │   ├── h1-auth.mjs
│   │   │   ├── h2-rate-limit.mjs
│   │   │   └── ...
│   │   ├── state/
│   │   │   └── current.oxigraph     # Triple store DB
│   │   └── scenes/
│   │       ├── sha256-001.nq        # Scene quads
│   │       ├── sha256-002.nq
│   │       └── ...
│   │
│   └── sha256-def456.../
│       └── ...
│
├── receipts/
│   ├── 2025-12-27.ndjson            # Daily log
│   ├── 2025-12-28.ndjson
│   └── index/
│       ├── by-scene/                # scene_id → receipt lookup
│       ├── by-universe/             # universe_id → receipts
│       └── by-agent/                # agent_id → receipts
│
├── bridges/
│   ├── sha256-bridge1.../
│   │   ├── definition.json          # Bridge config
│   │   ├── type-mapping.json
│   │   ├── invariant-proofs/
│   │   │   └── q1-preservation.txt
│   │   └── executions.ndjson        # Bridge crossing log
│   │
│   └── sha256-bridge2.../
│       └── ...
│
└── artifacts/
    ├── sha256-artifact1             # Content-addressed
    ├── sha256-artifact2
    └── ...
```

---

## Performance Characteristics

```
┌──────────────────────────────────────────────────────────────────┐
│                    PERFORMANCE PROFILE                           │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│  OPERATION                    LATENCY       THROUGHPUT           │
│  ─────────────────────────────────────────────────────────────   │
│                                                                  │
│  Guard Evaluation             0.1-1ms       10K checks/sec      │
│  (per guard)                                                     │
│                                                                  │
│  Reconciliation (μ)           10-50ms       100-1000 scenes/sec │
│  (depends on delta size)                                        │
│                                                                  │
│  SPARQL Invariant Check       1-10ms        1K queries/sec      │
│  (per invariant)                                                 │
│                                                                  │
│  Receipt Generation           5-15ms        500-2000/sec        │
│  (signing overhead)                                              │
│                                                                  │
│  Triple Store Update          5-20ms        500-2000 deltas/sec │
│  (oxigraph persist)                                              │
│                                                                  │
│  ─────────────────────────────────────────────────────────────   │
│  TOTAL (end-to-end)           50-100ms      200-500 scenes/sec  │
│  ─────────────────────────────────────────────────────────────   │
│                                                                  │
│  OTEL TARGET: <100ms per scene (p95)                            │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘

BOTTLENECKS:
───────────
1. SPARQL queries on large graphs (>1M triples)
   → Mitigation: Materialized views, query optimization

2. Cryptographic signing (Ed25519)
   → Mitigation: Batch signing, hardware acceleration

3. Disk I/O for receipt append
   → Mitigation: Write-ahead log, async flush

SCALABILITY:
────────────
- Horizontal: Each universe is independent → shard by universe_id
- Vertical: Triple store supports 10M+ triples (tested)
- Concurrency: Optimistic locking for conflicting scenes
```

---

## Security Model

```
┌──────────────────────────────────────────────────────────────────┐
│                       THREAT MODEL                               │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│  THREAT                     MITIGATION                           │
│  ─────────────────────────────────────────────────────────────   │
│                                                                  │
│  T1: Unauthorized scene     Guards (H) enforce access control   │
│      submission             Dual signature (universe + agent)   │
│                                                                  │
│  T2: Receipt forgery        Cryptographic signatures            │
│                             Content-addressed hashing           │
│                             Merkle tree proofs                  │
│                                                                  │
│  T3: Replay attacks         Scene IDs include timestamp         │
│                             Fork parent links prevent reorder   │
│                                                                  │
│  T4: Invariant bypass       Strict enforcement in μ             │
│                             SPARQL checks BEFORE commit         │
│                                                                  │
│  T5: Bridge tampering       Multi-sig (source + target + witness)│
│                             Invariant preservation proofs       │
│                                                                  │
│  T6: Side effect failure    Non-blocking execution              │
│                             Audit log for all attempts          │
│                             Idempotency tokens                  │
│                                                                  │
│  T7: State divergence       Deterministic μ function            │
│      (across replicas)      Canonical serialization             │
│                             Replay from genesis verifies        │
│                                                                  │
│  T8: Key compromise         Key rotation protocol               │
│                             Time-bounded access grants          │
│                             Revocation via new scene            │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

---

## Conclusion

This architecture provides:

✅ **Determinism**: All state transitions reproducible
✅ **Auditability**: Cryptographic receipts for every scene
✅ **Composability**: Bridges enable cross-universe workflows
✅ **Flexibility**: Pluggable guards, invariants, reconciliation logic
✅ **Performance**: <100ms median latency (OTEL target)
✅ **Security**: Multi-signature, content-addressing, guard enforcement

**Next Steps**: Consensus on open questions → Type implementation → Core engine → Production hardening
