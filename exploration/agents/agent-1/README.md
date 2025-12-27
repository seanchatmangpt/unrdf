# UNRDF Substrate Scanner - Agent 1

## Overview

The UNRDF Substrate Scanner (Agent 1) is a capability discovery tool that systematically scans all `@unrdf/*` packages to extract:

- **Package metadata** (name, version, description)
- **Exported APIs** (from `package.json` exports field and `src/index.mjs`)
- **Role classification** (store, io, derive, enforce, render, core, utility)
- **Maturity assessment** (mature, stable, documented, experimental)
- **Examples and test coverage**

## Usage

### Run the Scanner

```bash
node /home/user/unrdf/exploration/agents/agent-1/index.mjs
```

### Output

The scanner generates `/home/user/unrdf/exploration/capability-map.json` containing:

```json
{
  "timestamp": "2025-12-27T02:50:45.444Z",
  "scannerVersion": "1.0.0",
  "totalPackages": 40,
  "packages": [
    {
      "name": "@unrdf/oxigraph",
      "path": "packages/oxigraph",
      "version": "5.0.1",
      "description": "RDF triple/quad store using Oxigraph...",
      "role": ["store", "io", "derive", "render"],
      "exports": ["createStore", "dataFactory", "OxigraphStore"],
      "mainExport": "src/index.mjs",
      "maturity": "mature",
      "signals": {
        "hasTests": true,
        "hasExamples": true,
        "hasReadme": true,
        "hasChangeLog": false
      },
      "examples": "examples",
      "keywords": ["rdf", "sparql", "store"],
      "dependencies": ["oxigraph", "zod"],
      "notes": ["ℹ Standard package structure"]
    }
  ],
  "roles_mapping": {
    "store": ["@unrdf/oxigraph", "@unrdf/kgc-substrate", "@unrdf/engine-gateway"],
    "io": [29 packages],
    "derive": [13 packages],
    "enforce": [3 packages],
    "render": [9 packages],
    "utility": [6 packages]
  },
  "role_descriptions": {
    "store": "RDF triple/quad storage and querying",
    "io": "Input/Output, serialization, streaming, and synchronization",
    "derive": "Derivation, inference, reasoning, and knowledge processing",
    "enforce": "Enforcement, validation, policy execution, and access control",
    "render": "Visualization, rendering, and UI components",
    "core": "Core utilities and foundational infrastructure",
    "utility": "General utility functions"
  }
}
```

## Role Classification

### store

RDF triple/quad storage and querying. Packages:

- `@unrdf/oxigraph` - Primary Oxigraph-backed store
- `@unrdf/kgc-substrate` - 4D substrate
- `@unrdf/engine-gateway` - Multi-role gateway

### io (29 packages)

Input/Output, serialization, streaming, and synchronization. Examples:

- `@unrdf/streaming` - Real-time change feeds
- `@unrdf/kgc-4d` - Event sourcing with Git snapshots
- `@unrdf/kgc-cli` - CLI tools
- `@unrdf/yawl-*` - Workflow automation layer

### derive (13 packages)

Derivation, inference, reasoning, and knowledge processing. Examples:

- `@unrdf/knowledge-engine` - Rule engine and inference
- `@unrdf/graph-analytics` - Graph analysis
- `@unrdf/semantic-search` - Semantic search capabilities
- `@unrdf/ml-inference` - ML-based inference

### enforce (3 packages)

Enforcement, validation, policy execution, and access control. Examples:

- `@unrdf/hooks` - Policy and validation framework
- `@unrdf/fusion` - Policy fusion
- `@unrdf/engine-gateway` - Access control

### render (9 packages)

Visualization, rendering, and UI components. Examples:

- `@unrdf/blockchain` - Blockchain visualization
- `@unrdf/rdf-graphql` - GraphQL rendering
- `@unrdf/yawl-viz` - Workflow visualization

### utility (6 packages)

General utilities. Examples:

- `@unrdf/observability` - Observability infrastructure
- `@unrdf/yawl-observability` - Workflow observability
- `@unrdf/atomvm` - WASM runtime

## Maturity Levels

| Level            | Criteria                         | Examples                                              |
| ---------------- | -------------------------------- | ----------------------------------------------------- |
| **mature**       | Has tests, examples, and README  | `@unrdf/oxigraph`, `@unrdf/hooks`, `@unrdf/streaming` |
| **stable**       | Has tests or examples and README | `@unrdf/caching`, `@unrdf/observability`              |
| **documented**   | Has README but no tests          | `@unrdf/kgc-substrate`                                |
| **experimental** | Minimal documentation            | (none in current scan)                                |

## Validation

### Check JSON validity

```bash
jq '.' /home/user/unrdf/exploration/capability-map.json > /dev/null && echo "✓ Valid JSON"
```

### Count packages by role

```bash
jq '.roles_mapping | to_entries | map("\(.key): \(.value | length)") | .[]' /home/user/unrdf/exploration/capability-map.json
```

### Find packages with missing tests

```bash
jq '.packages[] | select(.signals.hasTests == false) | "\(.name)"' /home/user/unrdf/exploration/capability-map.json
```

### List all store packages

```bash
jq '.packages[] | select(.role[] == "store") | .name' /home/user/unrdf/exploration/capability-map.json
```

### View role distribution

```bash
jq '.roles_mapping | to_entries | map({role: .key, count: (.value | length)})' /home/user/unrdf/exploration/capability-map.json
```

## Integration Points

The capability map identifies key integration vectors:

1. **Store-IO Pipeline**: `oxigraph` → `streaming` → `kgc-4d`
2. **Knowledge Flow**: `oxigraph` → `knowledge-engine` → `semantic-search`
3. **Policy Enforcement**: `hooks` → `engine-gateway` → `fusion`
4. **Visualization**: `rdf-graphql` → `blockchain`, `yawl-viz`

## Key Findings

- **40 public packages** across all roles
- **40 packages with examples or tests** (100% documented maturity)
- **3 store implementations** providing RDF persistence
- **29 IO packages** for integration, streaming, and workflow
- **13 reasoning/derivation packages** for knowledge processing
- **High interconnectivity** - packages build on @unrdf core

## Limitations

1. **Export detection** - Simple regex-based (not AST parsing)
2. **Role classification** - Keyword-based heuristic
3. **Maturity signals** - File existence only (not quality metrics)
4. **Private packages** - Excluded (e.g., `@unrdf/validation`, `@unrdf/domain`)

## Generated Files

- **capability-map.json** - Machine-readable capability map (absolute path: `/home/user/unrdf/exploration/capability-map.json`)
- **index.mjs** - Scanner implementation
- **README.md** - This file (documentation)
- **notes.md** - Detailed findings and observations

## Next Steps

1. **API Contract Analysis** - Deep-dive into critical packages
2. **Dependency Graph** - Visualize package relationships
3. **Coverage Gaps** - Identify missing functionality
4. **Performance Baseline** - Benchmark key operations
