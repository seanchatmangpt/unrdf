# Architecture Diagrams for Thesis

Publication-quality diagrams for visual documentation. All diagrams are in Mermaid format for LaTeX compatibility.

---

## 1. YAWL Execution Flow

**Figure 1.1: YAWL Hook-Native Execution Model**

```mermaid
graph TB
    subgraph "YAWL Workflow Specification"
        WF[Workflow Definition]
        T1[Task A: Approve]
        T2[Task B: Reject]
        T3[Task C: Finalize]

        WF --> T1
        WF --> T2
        WF --> T3

        T1 -->|approved=true| T3
        T1 -->|approved=false| T2
    end

    subgraph "Hook-Native Policy Pack"
        PP[createYAWLPolicyPack]

        subgraph "Enablement Hooks"
            E1[enable:approve]
            E2[enable:reject]
            E3[enable:finalize]
        end

        subgraph "Completion Hooks"
            C1[complete:approve<br/>XOR-split router]
            C2[complete:reject]
        end

        subgraph "Cancellation Hooks"
            X1[timeout:approve]
            X2[cancel:approve]
        end

        PP --> E1
        PP --> E2
        PP --> E3
        PP --> C1
        PP --> C2
        PP --> X1
        PP --> X2
    end

    subgraph "Execution Engine"
        RDF[(RDF Store<br/>Oxigraph)]

        subgraph "Hook Evaluation"
            SPARQL[SPARQL-ASK<br/>Condition Evaluator]
            ROUTE[Control Flow<br/>Router]
            RECEIPT[Receipt<br/>Generator]
        end

        RDF --> SPARQL
        SPARQL --> ROUTE
        ROUTE --> RECEIPT
    end

    subgraph "Cryptographic Audit Trail"
        R1[Receipt: Task Enablement<br/>hookType: enablement<br/>decision: allow/deny<br/>sparqlQuery: ...<br/>sha256: ...]
        R2[Receipt: Control Flow<br/>hookType: completion<br/>decision: route<br/>enabledTasks: [finalize]<br/>sha256: ...]

        RECEIPT --> R1
        RECEIPT --> R2
    end

    E1 --> SPARQL
    C1 --> ROUTE

    style WF fill:#e1f5ff
    style PP fill:#fff4e1
    style RDF fill:#f0f0f0
    style R1 fill:#e8f5e9
    style R2 fill:#e8f5e9
```

**Caption:** YAWL workflow execution using hook-native control flow. Policy pack translates YAWL specification into enablement hooks (validate task readiness), completion hooks (route control flow via SPARQL-ASK), and cancellation hooks (timeout enforcement). Each evaluation produces cryptographic receipts proving governance compliance.

---

## 2. KGC-4D Temporal Architecture

**Figure 2.1: KGC-4D Four-Dimensional Knowledge Graph**

```mermaid
graph TB
    subgraph "Temporal Dimensions"
        direction TB

        subgraph "Dimension 1: Delta Layer"
            D[Delta Graph<br/>Change Events Only]
            D1[+ Add Event]
            D2[- Remove Event]

            D --> D1
            D --> D2
        end

        subgraph "Dimension 2: State Layer"
            S[State Graph<br/>Current Snapshot]
            S1[Entity A]
            S2[Entity B]
            S3[Relationship]

            S --> S1
            S --> S2
            S1 --> S3
            S3 --> S2
        end

        subgraph "Dimension 3: Shape Layer"
            SH[Shape Graph<br/>SHACL Constraints]
            SH1[NodeShape: Person]
            SH2[PropertyShape: age]
            SH3[ValidationReport]

            SH --> SH1
            SH1 --> SH2
            SH --> SH3
        end

        subgraph "Dimension 4: Provenance Layer"
            P[Provenance Graph<br/>Audit Trail]
            P1[Receipt: Change X<br/>timestamp: 2024-01-15T10:30:00Z]
            P2[Receipt: Change Y<br/>timestamp: 2024-01-15T10:31:00Z]
            P3[Signature Chain]

            P --> P1
            P --> P2
            P1 --> P3
            P2 --> P3
        end
    end

    subgraph "Hook Evaluation Engine"
        direction LR

        HE[Hook Manager]

        subgraph "Channel Views"
            V1[view: before]
            V2[view: after]
            V3[view: delta]
        end

        subgraph "Condition Evaluation"
            CE1[SPARQL-ASK]
            CE2[SHACL Validation]
            CE3[Custom Function]
        end

        HE --> V1
        HE --> V2
        HE --> V3

        V1 --> CE1
        V2 --> CE2
        V3 --> CE3
    end

    subgraph "Temporal Query Operators"
        TQ[Temporal SPARQL Extensions]
        TQ1[AS OF timestamp]
        TQ2[BETWEEN t1 AND t2]
        TQ3[CHANGES SINCE t]

        TQ --> TQ1
        TQ --> TQ2
        TQ --> TQ3
    end

    D -.->|materializes| S
    S -.->|validates against| SH
    D -.->|audited by| P

    HE -.->|queries| D
    HE -.->|queries| S
    HE -.->|validates| SH

    TQ -.->|temporal traversal| P

    style D fill:#e3f2fd
    style S fill:#f3e5f5
    style SH fill:#fff9c4
    style P fill:#e8f5e9
    style HE fill:#fce4ec
    style TQ fill:#e0f2f1
```

**Caption:** KGC-4D (Knowledge Graph Context - 4 Dimensional) architecture. Four named graphs provide orthogonal concerns: (1) Delta captures change events, (2) State materializes current view, (3) Shape enforces constraints via SHACL, (4) Provenance stores cryptographic receipts. Hook evaluation engine selects graph views (before/after/delta) based on hook channel configuration. Temporal SPARQL extensions enable time-travel queries.

---

## 3. Federation Topology

**Figure 3.1: Hook-Native Federation Architecture**

```mermaid
graph TB
    subgraph "Client Layer"
        C1[Browser Client<br/>React + @unrdf/react]
        C2[CLI Client<br/>Node.js]
        C3[Agent Client<br/>Claude Code]
    end

    subgraph "Federation Gateway"
        GW[Federation Gateway<br/>Load Balancer + Router]

        subgraph "Hook Registry"
            HR[Centralized Hook Registry<br/>Policy Pack Distribution]
            HR1[Policy Pack: Compliance]
            HR2[Policy Pack: YAWL Workflows]
            HR3[Policy Pack: Security]

            HR --> HR1
            HR --> HR2
            HR --> HR3
        end

        GW --> HR
    end

    subgraph "Federated Nodes"
        direction TB

        subgraph "Node A: Primary"
            N1[Oxigraph Instance A]
            H1A[Local Hook Engine]
            K1A[KGC-4D Store A]

            N1 --> H1A
            H1A --> K1A
        end

        subgraph "Node B: Replica"
            N2[Oxigraph Instance B]
            H2A[Local Hook Engine]
            K2A[KGC-4D Store B]

            N2 --> H2A
            H2A --> K2A
        end

        subgraph "Node C: Edge"
            N3[Oxigraph Instance C<br/>Offline-First]
            H3A[Local Hook Engine]
            K3A[KGC-4D Store C]

            N3 --> H3A
            H3A --> K3A
        end
    end

    subgraph "Synchronization Layer"
        SYNC[Delta Sync Protocol<br/>Event Sourcing]

        SYNC1[Delta Stream A→B]
        SYNC2[Delta Stream B→C]
        SYNC3[Conflict Resolution<br/>Vector Clocks]

        SYNC --> SYNC1
        SYNC --> SYNC2
        SYNC --> SYNC3
    end

    C1 --> GW
    C2 --> GW
    C3 --> GW

    GW --> N1
    GW --> N2
    GW --> N3

    HR -.->|distribute policies| H1A
    HR -.->|distribute policies| H2A
    HR -.->|distribute policies| H3A

    N1 <-.->|replicate deltas| SYNC
    N2 <-.->|replicate deltas| SYNC
    N3 <-.->|sync when online| SYNC

    style GW fill:#e1f5ff
    style HR fill:#fff4e1
    style N1 fill:#e8f5e9
    style N2 fill:#fff3e0
    style N3 fill:#f3e5f5
    style SYNC fill:#e0f2f1
```

**Caption:** Federation topology for distributed hook-native knowledge graphs. Centralized hook registry distributes policy packs to federated nodes. Each node runs local Oxigraph instance with independent hook evaluation engine. Delta sync protocol replicates changes using event sourcing. Node C demonstrates offline-first edge deployment with opportunistic synchronization. Vector clocks resolve conflicts during merge.

---

## 4. Hook Execution Model

**Figure 4.1: Hook Lifecycle Reflex Arc**

```mermaid
sequenceDiagram
    autonumber

    participant Client as Client<br/>(Application)
    participant Store as RDF Store<br/>(Oxigraph)
    participant Hook as Hook Manager<br/>(Condition Evaluator)
    participant Before as before()<br/>(Afferent Gate)
    participant Run as run()<br/>(Core Processing)
    participant After as after()<br/>(Efferent Cleanup)
    participant Receipt as Receipt Anchor<br/>(Git Notes)

    Client->>Store: Quad Add/Remove Request
    activate Store

    Store->>Hook: Trigger Hook Evaluation
    activate Hook

    Hook->>Hook: Load Condition from URI<br/>(file://hooks/condition.rq)
    Hook->>Hook: Verify SHA-256 Hash<br/>(Integrity Check)

    Hook->>Before: Execute before({ payload, context })
    activate Before

    alt Validation Fails
        Before-->>Hook: { cancel: true, reason: "Invalid input" }
        Hook-->>Store: Abort Transaction
        Store-->>Client: Error Response
    else Validation Passes
        Before-->>Hook: { ...payload, enrichedData }
        deactivate Before

        Hook->>Run: Execute run({ payload, context })
        activate Run

        Run->>Run: Evaluate SPARQL-ASK Condition
        Run->>Run: Execute Business Logic

        alt Condition Not Met
            Run-->>Hook: { result: { decision: "deny" } }
        else Condition Met
            Run-->>Hook: { result: { decision: "allow" },<br/>assertions: [quads] }
        end
        deactivate Run

        Hook->>After: Execute after({ result, cancelled })
        activate After

        After->>After: Post-processing<br/>(Notifications, Cleanup)
        After-->>Hook: { result: { finalStatus } }
        deactivate After

        Hook->>Receipt: Generate Cryptographic Receipt<br/>{ hookId, timestamp, conditionHash,<br/>inputHash, outputHash, signature }
        activate Receipt

        Receipt->>Receipt: Sign with ED25519
        Receipt->>Receipt: Anchor to Git Notes
        Receipt-->>Hook: Receipt ID
        deactivate Receipt

        Hook-->>Store: Allow/Deny Transaction<br/>+ Optional Assertions
        deactivate Hook

        Store-->>Client: Success/Failure Response
    end

    deactivate Store
```

**Caption:** Hook execution follows autonomic reflex arc pattern: (1) Stimulus triggers evaluation, (2) before() validates and enriches input (afferent), (3) run() evaluates SPARQL-ASK condition and executes core logic (processing), (4) after() handles cleanup and notifications (efferent), (5) Receipt generator produces cryptographic audit trail. Integrity verification (SHA-256) prevents condition tampering.

---

## 5. Microframework Hub Pattern

**Figure 5.1: UNRDF Microframework Ecosystem**

```mermaid
graph TB
    subgraph "Core Infrastructure"
        direction TB

        CORE[UNRDF Core<br/>@unrdf/core]
        OXI[Oxigraph Bindings<br/>@unrdf/oxigraph]
        HOOKS[Hooks Engine<br/>@unrdf/hooks]
        KGC[KGC-4D Runtime<br/>@unrdf/kgc-4d]

        CORE --> OXI
        CORE --> HOOKS
        HOOKS --> KGC
    end

    subgraph "Vertical Frameworks (Domain-Specific)"
        direction LR

        YAWL[YAWL Engine<br/>@unrdf/yawl<br/>Workflow Orchestration]
        EVENTS[Event Sourcing<br/>@unrdf/events<br/>Temporal Event Log]
        VALIDATION[Validation<br/>@unrdf/validation<br/>SHACL + Custom Rules]

        YAWL --> HOOKS
        EVENTS --> KGC
        VALIDATION --> HOOKS
    end

    subgraph "Horizontal Frameworks (Cross-Cutting)"
        direction LR

        REACT[React Integration<br/>@unrdf/react<br/>Hooks + Components]
        CLI[CLI Framework<br/>@unrdf/cli<br/>Command Patterns]
        OTEL[Observability<br/>@unrdf/otel<br/>OpenTelemetry Spans]

        REACT --> CORE
        CLI --> CORE
        OTEL --> HOOKS
    end

    subgraph "Experimental Frameworks (Innovation Hub)"
        direction TB

        SWARM[AI Swarm<br/>@unrdf/swarm<br/>Multi-Agent Coordination]
        QUANTUM[Quantum DSL<br/>@unrdf/quantum<br/>Qiskit Integration]
        BLOCKCHAIN[Blockchain Bridge<br/>@unrdf/blockchain<br/>Receipt Anchoring]

        SWARM --> HOOKS
        QUANTUM --> CORE
        BLOCKCHAIN --> KGC
    end

    subgraph "Application Layer"
        direction LR

        APP1[App: Document Editor<br/>YAWL + React + OTEL]
        APP2[App: Workflow Designer<br/>CLI + YAWL + Events]
        APP3[App: Compliance Dashboard<br/>Validation + React + OTEL]

        APP1 --> YAWL
        APP1 --> REACT
        APP1 --> OTEL

        APP2 --> CLI
        APP2 --> YAWL
        APP2 --> EVENTS

        APP3 --> VALIDATION
        APP3 --> REACT
        APP3 --> OTEL
    end

    CORE -.->|foundational| YAWL
    CORE -.->|foundational| EVENTS
    CORE -.->|foundational| VALIDATION

    HOOKS -.->|reactive primitives| REACT
    HOOKS -.->|reactive primitives| CLI

    style CORE fill:#e3f2fd
    style HOOKS fill:#f3e5f5
    style KGC fill:#fff9c4
    style YAWL fill:#e8f5e9
    style REACT fill:#fce4ec
    style OTEL fill:#e0f2f1
    style SWARM fill:#fff3e0
```

**Caption:** UNRDF microframework hub-and-spoke architecture. Core infrastructure (Oxigraph, Hooks, KGC-4D) provides foundational RDF + reactive primitives. Vertical frameworks add domain-specific capabilities (YAWL workflows, event sourcing, validation). Horizontal frameworks provide cross-cutting concerns (React integration, CLI patterns, observability). Experimental hub explores novel integrations (AI swarm, quantum computing, blockchain). Applications compose frameworks à la carte based on requirements.

---

## Usage Notes

**LaTeX Integration:**

To include these diagrams in a LaTeX thesis, use the `mermaid` package or export to PDF via:

```bash
# Install mermaid-cli
npm install -g @mermaid-js/mermaid-cli

# Convert to PDF
mmdc -i DIAGRAMS.md -o diagrams.pdf -t default
```

**Editing:**

All diagrams are editable at [Mermaid Live Editor](https://mermaid.live/). Copy/paste diagram code blocks for visual editing.

**Styling:**

Diagrams use color-coding:
- **Blue (#e3f2fd)**: Core infrastructure
- **Purple (#f3e5f5)**: Hook-related components
- **Yellow (#fff9c4)**: Shape/validation layers
- **Green (#e8f5e9)**: YAWL/workflow components
- **Pink (#fce4ec)**: UI/presentation layers
- **Teal (#e0f2f1)**: Temporal/sync components
- **Orange (#fff3e0)**: Experimental/edge features

**Accessibility:**

All diagrams include descriptive captions and use colorblind-friendly palettes (verified with [ColorBrewer](https://colorbrewer2.org/)).

---

## Related Files

- **TABLES.md**: Performance comparison tables
- **CODE-LISTINGS.md**: Syntax-highlighted code examples
- **SUPPLEMENTARY-MATERIALS.md**: Glossary, acronyms, index
