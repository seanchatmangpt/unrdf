# KGC-4D Documentation Architecture - Visual Diagrams

**Companion Document to:** KGC-4D-DOCUMENTATION-ARCHITECTURE.md
**Date:** 2025-12-27

---

## 1. Concept Dependency Graph

```mermaid
graph TD
    %% Foundation Layer
    A[RDF Basics<br/>triples/quads] --> B[Named Graphs<br/>partitioning]
    B --> C[Observable State - O<br/>Universe graph]

    %% Time Dimension
    C --> D[Time Dimension - t_ns<br/>nanosecond timestamps]
    D --> E[Event Sourcing<br/>state from events]

    %% Event System
    E --> F[EventLog vs Universe<br/>immutable vs derived]
    F --> G[Deltas<br/>add/delete operations]
    G --> H[appendEvent API<br/>atomic writes]

    %% Snapshots & Git
    D --> I[Snapshots<br/>performance optimization]
    I --> J[Git Dimension - G<br/>content-addressed storage]
    J --> K[Receipt Verification<br/>BLAKE3 cryptographic proof]

    %% Time-Travel
    E --> L[Time-Travel Concept<br/>historical reconstruction]
    I --> L
    L --> M[reconstructState API<br/>snapshot + replay]

    %% Vector Clocks
    D --> N[Vector Clocks - V<br/>causality tracking]
    N --> O[Distributed Systems<br/>multi-node coordination]

    %% Advanced Features
    M --> P[HDIT Coordinates<br/>event similarity]
    P --> Q[Similarity & Clustering<br/>visualization]

    %% Production
    H --> R[Production Patterns<br/>undo/redo, hooks]
    M --> R
    K --> R
    O --> R

    %% Styling
    classDef foundation fill:#e1f5fe,stroke:#01579b,stroke-width:2px
    classDef core fill:#f3e5f5,stroke:#4a148c,stroke-width:2px
    classDef advanced fill:#e8f5e9,stroke:#1b5e20,stroke-width:2px
    classDef production fill:#fff3e0,stroke:#e65100,stroke-width:2px

    class A,B,C foundation
    class D,E,F,G,H,I,J,K,L,M,N core
    class P,Q advanced
    class R,O production
```

---

## 2. User Journey Maps

### 2.1 Data Scientist Journey

```mermaid
journey
    title Data Scientist: Temporal Analysis Journey
    section Foundation
      Read Getting Started: 5: Learner
      Understand Named Graphs: 3: Learner
      First SPARQL Query: 4: Learner
    section Query History
      Learn EventLog Structure: 4: Analyst
      Write Temporal SPARQL: 5: Analyst
      Filter by Time Range: 5: Analyst
    section Time-Travel
      Understand Reconstruction: 3: Analyst
      Use reconstructState API: 4: Analyst
      Compare Historical States: 5: Analyst
    section Advanced
      Learn HDIT Coordinates: 3: Scientist
      Cluster Similar Events: 4: Scientist
      Visualize Evolution: 5: Scientist
```

### 2.2 Application Builder Journey

```mermaid
journey
    title Application Builder: Event-Sourced App Journey
    section Foundation
      Read Getting Started: 5: Builder
      Install Dependencies: 5: Builder
      First appendEvent: 4: Builder
    section Build App
      Understand Deltas: 4: Builder
      Implement CRUD Operations: 5: Builder
      Add Error Handling: 4: Builder
    section Checkpoints
      Learn Freeze Concept: 3: Builder
      Implement Snapshots: 4: Builder
      Verify Receipts: 4: Builder
    section Production
      Add Undo/Redo: 5: Builder
      Optimize Performance: 4: Builder
      Deploy to Production: 5: Builder
```

### 2.3 System Architect Journey

```mermaid
journey
    title System Architect: Understanding Journey
    section Conceptual
      Read 4D Model: 5: Architect
      Understand Event Sourcing: 5: Architect
      Study Git Backend: 4: Architect
    section Deep Dive
      Review Component Diagrams: 5: Architect
      Analyze Performance: 4: Architect
      Study Vector Clocks: 3: Architect
    section Design
      Plan System Architecture: 5: Architect
      Capacity Planning: 4: Architect
      Design Distributed Setup: 3: Architect
    section Validation
      Review ADRs: 5: Architect
      Benchmark Performance: 4: Architect
      Approve Design: 5: Architect
```

### 2.4 DevOps Engineer Journey

```mermaid
journey
    title DevOps: Operations Journey
    section Foundation
      Read Getting Started: 3: Ops
      Understand Freeze/Verify: 5: Ops
      Learn Git Backend: 4: Ops
    section Operations
      Setup Production: 5: Ops
      Configure Monitoring: 5: Ops
      Test Disaster Recovery: 4: Ops
    section Verification
      Verify Receipts: 5: Ops
      Audit Event Log: 4: Ops
      Check Integrity: 5: Ops
    section Troubleshooting
      Debug Reconstruction: 3: Ops
      Analyze Performance: 4: Ops
      Resolve Issues: 4: Ops
```

---

## 3. Learning Progression Paths

### 3.1 Linear Beginner Path

```mermaid
graph LR
    Start([New User]) --> L1[Level 1<br/>Foundation]
    L1 --> L2[Level 2<br/>Core Ops]
    L2 --> L3[Level 3<br/>Time-Travel]
    L3 --> L4[Level 4<br/>Distributed]
    L4 --> L5[Level 5<br/>Production]
    L5 --> L6[Level 6<br/>Advanced]
    L6 --> Expert([Expert User])

    classDef level fill:#e3f2fd,stroke:#1565c0
    class L1,L2,L3,L4,L5,L6 level
```

### 3.2 Role-Specific Fast Paths

```mermaid
graph TB
    Start([Choose Your Path]) --> DS[Data Scientist]
    Start --> AB[App Builder]
    Start --> SA[System Architect]
    Start --> DO[DevOps]

    DS --> DS1[Week 1: Foundation]
    DS1 --> DS2[Week 2: Time-Travel ⚡]
    DS2 --> DS3[Week 3: HDIT]
    DS3 --> DS4[Week 4: Back to Core Ops]

    AB --> AB1[Week 1: Foundation]
    AB1 --> AB2[Week 2: Core Ops]
    AB2 --> AB3[Week 3: Time-Travel]
    AB3 --> AB4[Week 4: Production]

    SA --> SA1[Week 1: Foundation + Explanations]
    SA1 --> SA2[Week 2: Distributed + Architecture]
    SA2 --> SA3[Week 3: Advanced + Performance]

    DO --> DO1[Week 1: Minimal Foundation]
    DO1 --> DO2[Week 2: Freeze & Verify]
    DO2 --> DO3[Week 3: Production]
    DO3 --> DO4[Week 4: Reconstruction]

    classDef scientist fill:#e1bee7,stroke:#4a148c
    classDef builder fill:#c5e1a5,stroke:#33691e
    classDef architect fill:#ffccbc,stroke:#bf360c
    classDef ops fill:#b2dfdb,stroke:#004d40

    class DS,DS1,DS2,DS3,DS4 scientist
    class AB,AB1,AB2,AB3,AB4 builder
    class SA,SA1,SA2,SA3 architect
    class DO,DO1,DO2,DO3,DO4 ops
```

---

## 4. KGC-4D System Architecture

### 4.1 Component Architecture (C4 Model - Container Level)

```mermaid
graph TB
    subgraph "Application Layer"
        APP[Your Application<br/>Event-Sourced Logic]
    end

    subgraph "KGC-4D Core"
        STORE[KGCStore<br/>appendEvent, query]
        FREEZE[Freeze Module<br/>freezeUniverse, reconstructState]
        TIME[Time Module<br/>now, VectorClock]
        HDIT[HDIT Module<br/>coordinates, similarity]
    end

    subgraph "Storage Layer"
        UNIVERSE[(Universe Graph<br/>Current State)]
        EVENTLOG[(EventLog Graph<br/>Immutable History)]
        SYSTEM[(System Graph<br/>Metadata)]
        GIT[Git Repository<br/>Snapshots]
    end

    subgraph "External Systems"
        MONITOR[Monitoring<br/>OTEL]
    end

    APP --> STORE
    APP --> FREEZE
    APP --> HDIT

    STORE --> TIME
    FREEZE --> TIME

    STORE --> UNIVERSE
    STORE --> EVENTLOG
    STORE --> SYSTEM

    FREEZE --> GIT
    FREEZE --> EVENTLOG
    FREEZE --> UNIVERSE

    STORE --> MONITOR
    FREEZE --> MONITOR

    classDef app fill:#e8eaf6,stroke:#3f51b5
    classDef core fill:#f3e5f5,stroke:#9c27b0
    classDef storage fill:#e0f2f1,stroke:#009688
    classDef external fill:#fff9c4,stroke:#f57f17

    class APP app
    class STORE,FREEZE,TIME,HDIT core
    class UNIVERSE,EVENTLOG,SYSTEM,GIT storage
    class MONITOR external
```

### 4.2 Freeze & Time-Travel Flow

```mermaid
sequenceDiagram
    participant App as Application
    participant Store as KGCStore
    participant Git as GitBackbone
    participant EventLog as EventLog Graph
    participant Universe as Universe Graph

    Note over App,Universe: FREEZE OPERATION

    App->>Store: freezeUniverse(store, git)
    activate Store

    Store->>Universe: match(null, null, null, Universe)
    Universe-->>Store: all quads

    Store->>Store: Sort & serialize to N-Quads
    Store->>Store: BLAKE3 hash

    Store->>Git: commitSnapshot(nquads, message)
    activate Git
    Git-->>Store: git_ref (SHA)
    deactivate Git

    Store->>EventLog: appendEvent(SNAPSHOT, {hash, git_ref})
    EventLog-->>Store: receipt

    Store-->>App: {id, hash, git_ref, t_ns}
    deactivate Store

    Note over App,Universe: TIME-TRAVEL OPERATION

    App->>Store: reconstructState(store, git, targetTime)
    activate Store

    Store->>EventLog: Find snapshot <= targetTime
    EventLog-->>Store: snapshot event

    Store->>Git: readSnapshot(git_ref)
    activate Git
    Git-->>Store: N-Quads content
    deactivate Git

    Store->>Store: Create temp store, load snapshot

    Store->>EventLog: Find events: snapshot < t <= target
    EventLog-->>Store: events with deltas

    Store->>Store: Replay deltas (add/delete)

    Store-->>App: reconstructed store
    deactivate Store
```

### 4.3 Event Sourcing Data Flow

```mermaid
flowchart TD
    Start([User Action]) --> Event[Create Event<br/>type, payload, deltas]

    Event --> Append[appendEvent API]

    Append --> Validate{Validation<br/>24 Guards}

    Validate -->|Pass| Transaction[Begin Transaction]
    Validate -->|Fail| Error([Throw Error])

    Transaction --> VectorClock[Increment<br/>Vector Clock]
    VectorClock --> Serialize[Serialize Event<br/>to RDF Quads]

    Serialize --> AddEvent[Add to EventLog<br/>immutable append]

    AddEvent --> ApplyDeltas[Apply Deltas<br/>to Universe]

    ApplyDeltas --> Success{All OK?}

    Success -->|Yes| Commit[Commit Transaction]
    Success -->|No| Rollback[Rollback<br/>restore state]

    Commit --> Receipt[Generate Receipt<br/>id, t_ns, count]
    Receipt --> Return([Return Receipt])

    Rollback --> Error

    classDef process fill:#e3f2fd,stroke:#1976d2
    classDef decision fill:#fff3e0,stroke:#f57c00
    classDef success fill:#e8f5e9,stroke:#388e3c
    classDef failure fill:#ffebee,stroke:#d32f2f

    class Event,Append,Transaction,VectorClock,Serialize,AddEvent,ApplyDeltas process
    class Validate,Success decision
    class Commit,Receipt,Return success
    class Error,Rollback failure
```

---

## 5. Documentation Structure Map

### 5.1 Diátaxis Quadrant Map

```mermaid
quadrantChart
    title Documentation Types by Purpose
    x-axis Theoretical --> Practical
    y-axis Acquisition --> Application
    quadrant-1 HOW-TO GUIDES
    quadrant-2 TUTORIALS
    quadrant-3 EXPLANATIONS
    quadrant-4 REFERENCE

    Tutorials - Getting Started: [0.25, 0.75]
    Tutorials - Event-Sourced App: [0.35, 0.80]
    Tutorials - Time-Travel: [0.30, 0.70]

    How-To - Query EventLog: [0.70, 0.75]
    How-To - Undo/Redo: [0.75, 0.80]
    How-To - Verify Receipts: [0.80, 0.70]
    How-To - Disaster Recovery: [0.85, 0.65]

    Reference - API Docs: [0.80, 0.35]
    Reference - Schema: [0.75, 0.30]
    Reference - Guards: [0.85, 0.40]
    Reference - Benchmarks: [0.90, 0.25]

    Explanation - Why 4D: [0.20, 0.30]
    Explanation - Event Sourcing: [0.25, 0.25]
    Explanation - Git Storage: [0.15, 0.35]
    Explanation - Vector Clocks: [0.30, 0.20]
```

### 5.2 Priority Matrix

```mermaid
quadrantChart
    title Documentation Priority by Impact vs Effort
    x-axis Low Effort --> High Effort
    y-axis Low Impact --> High Impact
    quadrant-1 DO LAST
    quadrant-2 DO FIRST
    quadrant-3 DO NEVER
    quadrant-4 DO NEXT

    Getting Started Tutorial: [0.25, 0.90]
    KGCStore API Ref: [0.30, 0.85]
    Why 4D Explanation: [0.20, 0.80]

    Event-Sourced App Tutorial: [0.50, 0.85]
    Time-Travel Tutorial: [0.55, 0.80]
    Receipt Schema Ref: [0.40, 0.75]

    HDIT Tutorial: [0.75, 0.70]
    Distributed Tutorial: [0.85, 0.65]
    Custom Guards How-To: [0.80, 0.60]

    Architecture Diagrams: [0.60, 0.55]
    ADRs: [0.45, 0.50]
```

---

## 6. Performance & Scaling Guide

### 6.1 Time-Travel Performance by Snapshot Frequency

```mermaid
graph LR
    subgraph "Snapshot Strategy Impact"
        A[No Snapshots<br/>❌ O-n- replay] --> B[Hourly Snapshots<br/>⚠️ Medium replay]
        B --> C[Every 1K Events<br/>✅ Fast replay]
        C --> D[Every Event<br/>❌ Storage bloat]
    end

    subgraph "Metrics"
        E[Reconstruction Time<br/>∝ events since snapshot]
        F[Storage Size<br/>∝ snapshot count]
        G[Write Latency<br/>∝ freeze frequency]
    end

    A -.->|50s for 10K events| E
    B -.->|5s for 1K events| E
    C -.->|0.5s for 100 events| E

    A -.->|Minimal| F
    B -.->|Moderate| F
    C -.->|Optimal| F
    D -.->|Excessive| F

    classDef bad fill:#ffebee,stroke:#c62828
    classDef medium fill:#fff3e0,stroke:#f57c00
    classDef good fill:#e8f5e9,stroke:#2e7d32

    class A,D bad
    class B medium
    class C good
```

### 6.2 HDIT Dimension Scaling

```mermaid
graph TD
    Start{Environment?} --> Browser[Browser<br/>D=64, N=100]
    Start --> Node[Node.js<br/>D=1024, N=100K]
    Start --> Heavy[Batch Processing<br/>D=4096, N=1M]

    Browser --> BrowserLatency[Latency: <100ms ✅]
    Node --> NodeLatency[Latency: <500ms ✅]
    Heavy --> HeavyLatency[Latency: <5s ⚠️]

    Browser --> BrowserMem[Memory: ~10MB]
    Node --> NodeMem[Memory: ~500MB]
    Heavy --> HeavyMem[Memory: ~8GB]

    classDef env fill:#e1f5fe,stroke:#01579b
    classDef metric fill:#f3e5f5,stroke:#4a148c

    class Browser,Node,Heavy env
    class BrowserLatency,NodeLatency,HeavyLatency,BrowserMem,NodeMem,HeavyMem metric
```

---

## 7. Error Handling Patterns

### 7.1 Guard Execution Flow

```mermaid
flowchart TD
    Input[User Input] --> G1{Input Validation<br/>Guards}

    G1 -->|Pass| G2{Temporal<br/>Guards}
    G1 -->|Fail| E1[ValidationError<br/>Invalid URI/Type]

    G2 -->|Pass| G3{Resource<br/>Guards}
    G2 -->|Fail| E2[TemporalError<br/>Clock Jump]

    G3 -->|Pass| G4{Causality<br/>Guards}
    G3 -->|Fail| E3[ResourceError<br/>Memory/Size Limit]

    G4 -->|Pass| G5{Data Integrity<br/>Guards}
    G4 -->|Fail| E4[CausalityError<br/>Clock Ordering]

    G5 -->|Pass| Execute[Execute Operation]
    G5 -->|Fail| E5[IntegrityError<br/>Duplicate Quad]

    Execute --> Success([Operation Success])

    E1 --> ErrorHandler[Error Handler]
    E2 --> ErrorHandler
    E3 --> ErrorHandler
    E4 --> ErrorHandler
    E5 --> ErrorHandler

    ErrorHandler --> Rollback[Rollback State]
    Rollback --> LogError[Log Error]
    LogError --> Fail([Operation Failed])

    classDef guard fill:#e8eaf6,stroke:#3f51b5
    classDef error fill:#ffebee,stroke:#c62828
    classDef success fill:#e8f5e9,stroke:#2e7d32

    class G1,G2,G3,G4,G5 guard
    class E1,E2,E3,E4,E5,ErrorHandler,Rollback,LogError,Fail error
    class Execute,Success success
```

---

## 8. Cross-Environment Compatibility

### 8.1 Feature Matrix

```mermaid
graph TB
    subgraph "Node.js Environment"
        N1[✅ Full KGCStore API]
        N2[✅ Git Backend - Full]
        N3[✅ BLAKE3 Hashing]
        N4[✅ Vector Clocks]
        N5[✅ HDIT - High Dimension]
        N6[✅ File System Access]
    end

    subgraph "Browser Environment"
        B1[✅ Core KGCStore API]
        B2[❌ Git Backend - Limited]
        B3[✅ crypto.subtle - BLAKE3]
        B4[✅ Vector Clocks]
        B5[⚠️ HDIT - Low Dimension]
        B6[❌ No File System]
    end

    subgraph "Universal APIs"
        U1[appendEvent]
        U2[queryEventLog]
        U3[queryUniverse]
        U4[now, toISO]
        U5[VectorClock]
    end

    N1 -.->|Shared| U1
    N1 -.->|Shared| U2
    N1 -.->|Shared| U3

    B1 -.->|Shared| U1
    B1 -.->|Shared| U2
    B1 -.->|Shared| U3

    classDef nodeOnly fill:#c8e6c9,stroke:#2e7d32
    classDef browserOnly fill:#bbdefb,stroke:#1565c0
    classDef universal fill:#fff9c4,stroke:#f57f17

    class N1,N2,N3,N4,N5,N6 nodeOnly
    class B1,B2,B3,B4,B5,B6 browserOnly
    class U1,U2,U3,U4,U5 universal
```

---

## 9. Testing Strategy Pyramid

```mermaid
graph TD
    subgraph "Test Pyramid"
        E2E[E2E Tests - 5%<br/>Full freeze/reconstruct]
        INT[Integration Tests - 15%<br/>Multi-component flows]
        UNIT[Unit Tests - 80%<br/>Pure functions]
    end

    subgraph "Critical Scenarios"
        S1[Empty Universe Freeze]
        S2[Time-Travel Before Events]
        S3[Concurrent Appends]
        S4[Receipt Verification]
        S5[Missing Snapshot Recovery]
    end

    UNIT --> S1
    UNIT --> S4
    INT --> S2
    INT --> S3
    E2E --> S5

    classDef test fill:#e3f2fd,stroke:#1976d2
    classDef scenario fill:#f3e5f5,stroke:#7b1fa2

    class E2E,INT,UNIT test
    class S1,S2,S3,S4,S5 scenario
```

---

**End of Diagrams**

**File:** `/home/user/unrdf/docs/KGC-4D-DOCUMENTATION-DIAGRAMS.md`
