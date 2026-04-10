# UNRDF v26.4.10 — Complete Capabilities Overview

## Architecture

UNRDF is a **RDF Knowledge Graph Substrate Platform** built on 7 layers:

```
Application Layer (CLI, APIs, Browser, MCP)
        ↓
O* Innovations (Federation, Marketplace, Streaming)
        ↓
Knowledge Substrate (Hooks, Transactions, Validation)
        ↓
RDF Core (SPARQL, SHACL, Storage)
        ↓
Backends (Memory, Oxigraph, Remote)
```

---

## Core Packages (20 packages total)

### 1. **@unrdf/core** — RDF Foundation

- **Purpose**: Core RDF operations, in-memory graph store, DataFactory
- **Key exports**: `createStore()`, `namedNode()`, `literal()`, quad operations
- **Capabilities**:
  - Memory-based RDF store
  - SPARQL execution (via Comunica)
  - SHACL validation
  - JSON-LD context handling
  - Quad manipulation and traversal

### 2. **@unrdf/oxigraph** — Persistent Storage

- **Purpose**: RDF persistence via Oxigraph triplestore
- **Capabilities**:
  - Store graphs on disk
  - SPARQL queries on persistent data
  - Transactional updates
  - SPARQL UPDATE operations
  - Full-text search integration

### 3. **@unrdf/hooks** — Knowledge Hooks (Innovation 5)

- **Purpose**: SPARQL CONSTRUCT + N3 forward-chaining rules
- **Capabilities**:
  - Automatic data transformation (N3 rules)
  - Validation hooks (SHACL soft-fail)
  - Pre/post insert/update/delete triggers
  - Receipt chaining (BLAKE3 hashing)
  - Hook templates and catalog system
  - Execute hooks by trigger name
  - Evaluate conditions against RDF store

### 4. **@unrdf/federation** — Distributed Consensus (Innovation 4)

- **Purpose**: M-of-N quorum voting with BLAKE3 receipt chaining
- **Capabilities**:
  - Raft-based cluster coordination
  - Byzantine fault tolerance
  - Cryptographic receipt chaining
  - Quorum voting (approve/reject proposals)
  - Delta propagation across nodes
  - Cluster status monitoring

### 5. **@unrdf/streaming** — Delta Admission (Innovation 6)

- **Purpose**: Streaming RDF deltas with receipt chaining
- **Capabilities**:
  - I/O BLAKE3 hashing for delta integrity
  - Batch admission of quads
  - Streaming delta processing
  - Receipt chaining for audit trail
  - Backpressure handling

### 6. **@unrdf/daemon** — Background Services (MCP Server + 15 Tools)

- **Purpose**: Background operations, scheduling, MCP protocol server
- **Capabilities**:
  - Start/stop MCP server
  - 15 ontology tools for RDF operations
  - Scheduled operations (cron-like)
  - Raft cluster management
  - Health monitoring
  - Operation logging with filtering
  - Configuration hot-reload

### 7. **@unrdf/cli** — Command-Line Interface

- **Purpose**: Interactive RDF operations from terminal
- **Capabilities**: (see CLI section below)

### 8. **@unrdf/v6-core** — YAWL v6 Integration

- **Purpose**: YAWL workflow engine integration
- **Capabilities**:
  - Workflow execution via YAWL v6
  - Delta-based workflow state
  - Transactional workflow operations

### 9. **@unrdf/otel** — OpenTelemetry Integration

- **Purpose**: Observability and distributed tracing
- **Capabilities**:
  - Span generation for RDF operations
  - OTel weaver-generated semconv
  - Integration with Jaeger tracing

---

## CLI Commands (13 Command Groups + 60 Subcommands)

### **1. Graph Operations** (`unrdf graph`)

```
create    Create a new RDF graph
load      Load RDF data (Turtle, N-Triples, N-Quads)
query     Execute SPARQL on graph
dump      Export graph to file
stats     Show graph statistics
```

### **2. SPARQL Queries** (`unrdf query` + `unrdf query-file`)

```
Execute SPARQL queries with multiple output formats:
  --format: table, json, csv
Supports SPARQL SELECT, CONSTRUCT, ASK, DESCRIBE
```

### **3. Format Conversion** (`unrdf convert`, `to-turtle`, `to-ntriples`, `to-json`)

```
convert RDF between formats:
  - Turtle (.ttl)
  - N-Triples (.nt)
  - N-Quads (.nq)
  - JSON representation
Auto-detects format if not specified
```

### **4. JSON-LD Contexts** (`unrdf context`)

```
create    Create new JSON-LD context
add       Add prefix to context (uri:prefix mapping)
list      List all context prefixes
remove    Remove prefix from context
```

### **5. Knowledge Hooks** (`unrdf hooks`)

```
execute              Execute hooks against RDF store
define               Define hooks from SPARQL/N3 config
evaluate-condition   Test a single condition
list-conditions      Show available condition types
receipts            Display receipt chain from executions
template            Generate hook templates (predefined patterns)
```

### **6. Daemon Management** (`unrdf daemon`)

```
list      List all configured operations
run       Execute operation immediately
schedule  Add scheduled trigger (cron-like)
status    Health metrics and daemon status
logs      View operation logs (with filtering)
config    Display daemon configuration
cluster   Show Raft cluster status and members
```

### **7. MCP (Model Context Protocol)** (`unrdf mcp`)

```
start     Start the MCP server
status    Check if MCP server is running
inspect   List all exposed tools, resources, prompts
stop      Stop the MCP server
```

### **8. Code Generation** (`unrdf sync`)

```
Generate code artifacts from RDF ontology:
  --config="unrdf.toml"    Path to config file
  --dry-run                Preview changes without writing
  -v, --verbose            Verbose output
  -f, --force              Overwrite without prompting
  --rule                   Run specific rule by name
  --output="text|json"     Output format
  -w, --watch              Watch files for changes
```

### **9. Templates** (`unrdf template`)

```
generate   Generate files from RDF + Nunjucks templates
list       List templates from catalog or directory
query      Run SPARQL SELECT, return as template context
extract    Extract subject properties as JSON (debugging)
validate   Validate template frontmatter against schema
catalog    Show template catalog information
```

### **10. Health Check** (`unrdf doctor`)

```
Comprehensive system health check:
  -m, --mode="standard"      Check mode: quick (30s), standard (2min), full (5min)
  -c, --category             Specific category: env, system, quality, integration, otel, kubernetes
  -f, --format="human"       Output: human, json, yaml
  -x, --fix                  Attempt automatic fixes
  -w, --watch                Continuous monitoring (refresh 5s)
```

---

## MCP (Model Context Protocol) — 15 Tools

When running `unrdf mcp start`, exposes these ontology tools:

```
1. onto_validate        SHACL validation against shapes
2. onto_query           SPARQL SELECT queries
3. onto_reason          RDFS/OWL reasoning
4. onto_import          Load RDF data (all formats)
5. onto_export          Serialize RDF to various formats
6. onto_shapes          List SHACL shapes in store
7. onto_classes         List ontology classes
8. onto_properties      List properties (predicates)
9. onto_individuals     List individuals (instances)
10. onto_prefixes       Manage namespace prefixes
11. onto_namespaces     List namespaces
12. onto_construct      SPARQL CONSTRUCT (data generation)
13. onto_update         SPARQL UPDATE (modify data)
14. onto_explain        Query explanation and analysis
15. onto_stats          Store statistics and metrics
```

---

## Advanced Features

### **O\* Innovations**

#### **Innovation 4: Federation Quorum**

- M-of-N voting consensus
- BLAKE3 receipt chaining
- Cryptographic proof of agreement
- Distributed data integrity

#### **Innovation 5: Hooks Marketplace**

- SPARQL CONSTRUCT hooks (data transformation)
- N3 forward-chaining rules (logic rules)
- SHACL validation hooks (data quality)
- Pre/post triggers on insert/update/delete
- Hook receipts with cryptographic chaining

#### **Innovation 6: Streaming Admission**

- Delta RDF processing
- BLAKE3 hash chaining for delta integrity
- I/O audit trail
- Backpressure handling for bulk operations

### **Storage Backends**

- **Memory Store** (`@unrdf/core`) — fast, in-process
- **Oxigraph** (`@unrdf/oxigraph`) — persistent, SPARQL-capable
- **Remote** — HTTP REST API access to remote stores

### **Validation & Quality**

- **SHACL** — Shape-based validation
- **SPARQL** — Query-based validation
- **Zod** — Schema validation for config
- **N3** — Rule-based validation and transformation

### **Observability**

- **OpenTelemetry** — Distributed tracing
- **Jaeger** — Trace visualization
- **Health checks** — Daemon monitoring
- **Operation logs** — Audit trails with receipt chaining

---

## Use Cases

### **1. Knowledge Graphs**

- Store entities, relationships, hierarchies
- SPARQL queries for graph traversal
- Federation for distributed knowledge
- Hooks for automatic enrichment

### **2. Data Quality**

- SHACL validation on ingestion
- N3 rules for data transformation
- Soft-fail validation hooks
- Audit trails with receipts

### **3. Compliance & Audit**

- Receipt chaining (BLAKE3) for proof
- Immutable operation logs
- Raft consensus for distributed ledgers
- MCP tools for regulatory queries

### **4. Workflow Automation**

- YAWL v6 integration (workflows)
- Hook triggers on state changes
- Distributed coordination (federation)
- Delta streaming for real-time sync

### **5. Code Generation**

- RDF-driven templates (Nunjucks)
- Ontology-based config (`unrdf.toml`)
- Multi-language generation (Zod schemas, Docs, etc.)
- Watch mode for live regeneration

### **6. Integration Hub**

- MCP server for Claude AI integration
- 15 ontology tools for LLM access
- SPARQL queries from LLMs
- Hook execution from external systems

---

## Testing & Quality

- **1249 tests** (all passing, pre-push)
- **Multiple benchmarks**:
  - Hook execution bench
  - Receipt generation bench
  - SPARQL query bench
  - Task activation bench
  - E2E workflow bench
- **Coverage tracking**
- **Memory leak detection**
- **Performance profiling** (CPU, memory)

---

## Development Features

- **Monorepo** — 20 packages, unified versioning (26.4.10)
- **Hot reload** — Configuration changes without restart
- **Watch mode** — File monitoring for sync, templates
- **Dry run** — Preview changes before applying
- **Verbose logging** — Debug operations step-by-step
- **Package discovery** — Auto-discovery of exported packages

---

## Key Technical Stack

| Layer             | Technology                            |
| ----------------- | ------------------------------------- |
| **CLI Framework** | Citty (command router)                |
| **RDF Core**      | RDF.js libraries, Comunica            |
| **Persistence**   | Oxigraph (SPARQL-capable triplestore) |
| **Validation**    | SHACL (rdf-validate-shacl)            |
| **Rules**         | N3 (forward-chaining)                 |
| **Hashing**       | BLAKE3 (via hash-wasm)                |
| **Templates**     | Nunjucks + Hygen                      |
| **Config**        | TOML (@iarna/toml)                    |
| **Schemas**       | Zod v4                                |
| **Tests**         | Vitest                                |
| **Tracing**       | OpenTelemetry                         |
| **LLM**           | Groq AI (@ai-sdk/groq)                |

---

## Summary

UNRDF is a **complete RDF platform** combining:

- ✅ Core RDF operations (triples, quads, stores)
- ✅ SPARQL queries (SELECT, CONSTRUCT, UPDATE)
- ✅ Data validation (SHACL, N3 rules)
- ✅ Distributed consensus (Raft + quorum voting)
- ✅ Automatic transformation (hooks, CONSTRUCT)
- ✅ Code generation (Nunjucks templates)
- ✅ CLI tools (13 command groups)
- ✅ LLM integration (MCP server + 15 tools)
- ✅ Observability (OTEL + Jaeger)
- ✅ Audit trails (BLAKE3 receipt chaining)
