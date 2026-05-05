# V6 Noun-Verb Matrix - Complete Command Mapping

## Matrix Overview

This document provides the authoritative mapping of all valid noun-verb combinations for the V6 unified CLI.

**Total Nouns**: 10
**Total Verbs**: 25
**Valid Combinations**: 45

---

## Full Matrix Table

| Noun | Verbs | Count | Implementation Status |
|------|-------|-------|----------------------|
| universe | create, export, freeze, reconstruct, restore, verify | 6 | 🔄 Partial (kgc-4d) |
| eventlog | append, export, reconstruct, replay, verify | 5 | 🔄 Partial (kgc-4d) |
| receipt | anchor, chain, export, verify | 4 | ✅ Complete (v6-core) |
| policy | apply, export, test, validate | 4 | ⏳ Pending (hooks) |
| workflow | export, pause, replay, resume, start, verify | 6 | ⏳ Pending (yawl) |
| resource | allocate, export, query, release | 4 | ⏳ Pending (core) |
| grammar | compile, export, parse, validate, verify | 5 | ⏳ Pending (validation) |
| thesis | compile, export, render, validate, verify | 5 | ⏳ Pending (docs) |
| package | export, install, list, validate | 4 | ⏳ Pending (kgc-cli) |
| delta | apply, export, propose, verify | 4 | ✅ Complete (v6-core) |

**Legend**:
- ✅ Complete: Fully implemented with tests
- 🔄 Partial: Some commands exist, need migration to v6 spine
- ⏳ Pending: Not yet implemented

---

## Detailed Noun-Verb Combinations

### 1. universe (KGC-4D Universe Operations)

| Verb | Command | Description | Auth | Side Effects | Receipt |
|------|---------|-------------|------|--------------|---------|
| create | `kgc universe create` | Create new KGC-4D universe | No | mutate | Yes |
| export | `kgc universe export` | Export universe to format | No | write | Yes |
| freeze | `kgc universe freeze` | Create immutable snapshot | No | write | Yes |
| reconstruct | `kgc universe reconstruct` | Rebuild from event log | No | mutate | Yes |
| restore | `kgc universe restore` | Restore from snapshot | No | mutate | Yes |
| verify | `kgc universe verify` | Verify universe integrity | No | read | Yes |

**Example**:
```bash
kgc universe create --name "Production KG" --json
kgc universe freeze --id universe-abc123
kgc universe restore --snapshot snapshot-xyz789
```

---

### 2. eventlog (Event Sourcing)

| Verb | Command | Description | Auth | Side Effects | Receipt |
|------|---------|-------------|------|--------------|---------|
| append | `kgc eventlog append` | Append event to log | No | write | Yes |
| export | `kgc eventlog export` | Export event log | No | write | Yes |
| reconstruct | `kgc eventlog reconstruct` | Rebuild state from events | No | mutate | Yes |
| replay | `kgc eventlog replay` | Replay events from index | No | read | Yes |
| verify | `kgc eventlog verify` | Verify log integrity | No | read | Yes |

**Example**:
```bash
kgc eventlog append --event '{"type": "TripleAdded", "data": {...}}'
kgc eventlog replay --from 1000 --to 2000
kgc eventlog reconstruct --to 1500
```

---

### 3. receipt (Cryptographic Proofs)

**Status**: ✅ IMPLEMENTED

| Verb | Command | Description | Auth | Side Effects | Receipt |
|------|---------|-------------|------|--------------|---------|
| anchor | `kgc receipt anchor` | Anchor to merkle tree | Yes | write | Yes |
| chain | `kgc receipt chain` | Display receipt lineage | No | read | Yes |
| export | `kgc receipt export` | Export receipt to format | No | write | Yes |
| verify | `kgc receipt verify` | Verify receipt chain | No | read | Yes |

**Implementation**: `/home/user/unrdf/packages/v6-core/src/cli/commands/receipt.mjs`

**Example**:
```bash
kgc receipt verify --hash abc123def456 --depth 10
kgc receipt chain --id receipt-xyz --format tree
kgc receipt anchor --hash merkle-root --blockchain ethereum
kgc receipt export --id receipt-123 --format rdf --includeChain
```

---

### 4. policy (Hook & Policy Governance)

| Verb | Command | Description | Auth | Side Effects | Receipt |
|------|---------|-------------|------|--------------|---------|
| apply | `kgc policy apply` | Apply policy pack | Yes | mutate | Yes |
| export | `kgc policy export` | Export policy definition | No | write | Yes |
| test | `kgc policy test` | Test policy execution | No | read | Yes |
| validate | `kgc policy validate` | Validate policy structure | No | read | Yes |

**Example**:
```bash
kgc policy validate --file pre-commit-hooks.mjs
kgc policy apply --name data-quality-pack
kgc policy test --suite integration
```

---

### 5. workflow (YAWL Process Orchestration)

| Verb | Command | Description | Auth | Side Effects | Receipt |
|------|---------|-------------|------|--------------|---------|
| export | `kgc workflow export` | Export workflow definition | No | write | Yes |
| pause | `kgc workflow pause` | Pause workflow execution | Yes | mutate | Yes |
| replay | `kgc workflow replay` | Replay workflow events | No | read | Yes |
| resume | `kgc workflow resume` | Resume paused workflow | Yes | mutate | Yes |
| start | `kgc workflow start` | Start new workflow | Yes | mutate | Yes |
| verify | `kgc workflow verify` | Verify workflow definition | No | read | Yes |

**Example**:
```bash
kgc workflow start --definition etl-pipeline.yaml
kgc workflow pause --id workflow-abc123
kgc workflow resume --id workflow-abc123
kgc workflow verify --definition complex-workflow.yaml
```

---

### 6. resource (Resource Allocation)

| Verb | Command | Description | Auth | Side Effects | Receipt |
|------|---------|-------------|------|--------------|---------|
| allocate | `kgc resource allocate` | Allocate resources | Yes | mutate | Yes |
| export | `kgc resource export` | Export resource config | No | write | Yes |
| query | `kgc resource query` | Query resource status | No | read | No |
| release | `kgc resource release` | Release allocated resources | Yes | mutate | Yes |

**Example**:
```bash
kgc resource allocate --type memory --amount 2GB --priority high
kgc resource query --status active --json
kgc resource release --id resource-xyz789
```

---

### 7. grammar (SPARQL/SHACL/N3/OWL)

| Verb | Command | Description | Auth | Side Effects | Receipt |
|------|---------|-------------|------|--------------|---------|
| compile | `kgc grammar compile` | AOT compile grammar | No | write | Yes |
| export | `kgc grammar export` | Export grammar definition | No | write | Yes |
| parse | `kgc grammar parse` | Parse input with grammar | No | read | Yes |
| validate | `kgc grammar validate` | Validate grammar syntax | No | read | Yes |
| verify | `kgc grammar verify` | Verify grammar correctness | No | read | Yes |

**Example**:
```bash
kgc grammar validate --file query.sparql --type sparql
kgc grammar compile --input shapes.shacl --output compiled.json
kgc grammar parse --format n3 --file rules.n3
```

---

### 8. thesis (Documentation & LaTeX)

| Verb | Command | Description | Auth | Side Effects | Receipt |
|------|---------|-------------|------|--------------|---------|
| compile | `kgc thesis compile` | Compile LaTeX thesis | No | write | Yes |
| export | `kgc thesis export` | Export to format | No | write | Yes |
| render | `kgc thesis render` | Render markdown to PDF | No | write | Yes |
| validate | `kgc thesis validate` | Validate references/citations | No | read | Yes |
| verify | `kgc thesis verify` | Verify thesis structure | No | read | Yes |

**Example**:
```bash
kgc thesis compile --file main.tex --output thesis.pdf
kgc thesis render --input README.md --output docs.pdf
kgc thesis validate --citations --references --links
```

---

### 9. package (Package Registry)

| Verb | Command | Description | Auth | Side Effects | Receipt |
|------|---------|-------------|------|--------------|---------|
| export | `kgc package export` | Export package metadata | No | write | Yes |
| install | `kgc package install` | Install package | Yes | mutate | Yes |
| list | `kgc package list` | List registered packages | No | read | No |
| validate | `kgc package validate` | Validate package contracts | No | read | Yes |

**Example**:
```bash
kgc package list --status enabled --json
kgc package install --name @unrdf/new-extension
kgc package validate --contracts --schema
```

---

### 10. delta (Δ Change Carriers)

**Status**: ✅ IMPLEMENTED

| Verb | Command | Description | Auth | Side Effects | Receipt |
|------|---------|-------------|------|--------------|---------|
| apply | `kgc delta apply` | Apply delta with checks | Yes | mutate | Yes |
| export | `kgc delta export` | Export delta to format | No | write | Yes |
| propose | `kgc delta propose` | Propose state change | No | write | Yes |
| verify | `kgc delta verify` | Verify delta application | No | read | Yes |

**Implementation**: `/home/user/unrdf/packages/v6-core/src/cli/commands/delta.mjs`

**Example**:
```bash
kgc delta propose --file change.json --description "Add provenance triples"
kgc delta apply --id delta-abc123 --dryRun
kgc delta apply --id delta-abc123  # Actually apply
kgc delta verify --id delta-abc123 --against state-hash-xyz
kgc delta export --id delta-abc123 --format patch
```

---

## Verb Reference

### Verb Classification

| Category | Verbs | Count |
|----------|-------|-------|
| **Creation** | create, propose, install | 3 |
| **Modification** | apply, allocate, append, pause, resume, start | 6 |
| **Persistence** | freeze, anchor, release | 3 |
| **Reconstruction** | reconstruct, restore, replay | 3 |
| **Validation** | verify, validate, test | 3 |
| **Transformation** | compile, parse, render | 3 |
| **Extraction** | export, chain, list, query | 4 |

---

## Common Patterns

### Pattern 1: Create-Verify-Export

```bash
# Create entity
kgc universe create --name "Test"

# Verify it works
kgc universe verify --id universe-123

# Export for backup
kgc universe export --id universe-123 --format rdf
```

### Pattern 2: Propose-Verify-Apply

```bash
# Propose change
kgc delta propose --file changes.json

# Verify it's valid
kgc delta verify --id delta-abc

# Apply the change
kgc delta apply --id delta-abc
```

### Pattern 3: Start-Monitor-Verify

```bash
# Start workflow
kgc workflow start --definition etl.yaml

# Query status
kgc resource query --workflow-id workflow-123

# Verify completion
kgc workflow verify --id workflow-123
```

---

## Migration from V5 to V6

| V5 Command | V6 Equivalent | Notes |
|------------|---------------|-------|
| `kgc snapshot create` | `kgc universe freeze` | Renamed for clarity |
| `kgc snapshot restore` | `kgc universe restore` | No change in function |
| `kgc event append` | `kgc eventlog append` | Noun standardized |
| `kgc hooks validate` | `kgc policy validate` | Noun standardized |
| `kgc yawl start` | `kgc workflow start` | Noun abstracted |

---

## Coverage Metrics

```
Total Commands: 45
Implemented: 8 (18%)
Partial: 11 (24%)
Pending: 26 (58%)
```

**Coverage by Noun**:
- universe: 0/6 (0%)
- eventlog: 0/5 (0%)
- receipt: 4/4 (100%) ✅
- policy: 0/4 (0%)
- workflow: 0/6 (0%)
- resource: 0/4 (0%)
- grammar: 0/5 (0%)
- thesis: 0/5 (0%)
- package: 0/4 (0%)
- delta: 4/4 (100%) ✅

---

**Document Version**: 1.0
**Last Updated**: 2025-12-27
**Frozen for V6**: Yes
**Next Review**: V7 planning
