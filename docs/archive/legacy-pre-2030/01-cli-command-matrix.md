# CLI Command Matrix Reference

**Version**: vlatest.1
**Last Updated**: 2025-12-27

---

## Overview

UNRDF v6 uses a **unified CLI spine** with the pattern:

```bash
kgc <noun> <verb> [options]
```

**10 Canonical Nouns** × **25 Canonical Verbs** = **45 Valid Combinations** (not all combinations are valid)

This reference documents all valid `noun × verb` combinations.

---

## Quick Reference: Noun Summary

| Noun | Package | Description | Valid Verbs Count |
|------|---------|-------------|-------------------|
| **universe** | @unrdf/kgc-4d | KGC-4D spacetime snapshots | 5 |
| **eventlog** | @unrdf/kgc-4d | Event sourcing log | 5 |
| **receipt** | @unrdf/blockchain | Cryptographic proofs | 4 |
| **policy** | @unrdf/hooks | Governance rules | 4 |
| **workflow** | @unrdf/yawl | Process orchestration | 5 |
| **resource** | @unrdf/core | Resource management | 4 |
| **grammar** | @unrdf/validation | SPARQL/SHACL/N3/OWL | 4 |
| **thesis** | @unrdf/docs | Documentation operations | 4 |
| **package** | @unrdf/kgc-cli | Package registry | 4 |
| **delta** | @unrdf/v6-core | State transitions | 4 |

**Total**: 10 nouns

---

## Quick Reference: Verb Summary

| Verb | Description | Side Effects | Emits Receipt? | Requires Auth? |
|------|-------------|--------------|----------------|----------------|
| **compile** | AOT compilation | write | ✅ | ❌ |
| **verify** | Verification | read | ✅ | ❌ |
| **freeze** | Immutable snapshot | write | ✅ | ❌ |
| **reconstruct** | Rebuild state | mutate | ✅ | ❌ |
| **replay** | Replay events | read | ✅ | ❌ |
| **allocate** | Allocate resources | mutate | ✅ | ✅ |
| **validate** | Schema validation | read | ✅ | ❌ |
| **render** | Render output | write | ✅ | ❌ |
| **export** | Export data | write | ✅ | ❌ |
| **create** | Create entity | mutate | ✅ | ❌ |
| **restore** | Restore snapshot | mutate | ✅ | ❌ |
| **append** | Append to log | write | ✅ | ❌ |
| **chain** | Show lineage | read | ✅ | ❌ |
| **anchor** | Anchor to merkle | write | ✅ | ✅ |
| **apply** | Apply change | mutate | ✅ | ✅ |
| **test** | Test execution | read | ✅ | ❌ |
| **start** | Start process | mutate | ✅ | ✅ |
| **pause** | Pause execution | mutate | ✅ | ✅ |
| **resume** | Resume execution | mutate | ✅ | ✅ |
| **release** | Release resources | mutate | ✅ | ✅ |
| **query** | Query state | read | ❌ | ❌ |
| **parse** | Parse input | read | ✅ | ❌ |
| **list** | List entities | read | ❌ | ❌ |
| **install** | Install package | mutate | ✅ | ✅ |
| **propose** | Propose change | write | ✅ | ❌ |

**Total**: 25 verbs

---

## Complete Noun-Verb Matrix

### Legend
- ✅ = Implemented
- 🚧 = Planned (vlatest+)
- ❌ = Invalid combination

---

## 1. `universe` (KGC-4D Universe Operations)

**Package**: `@unrdf/kgc-4d`
**Description**: Manage spacetime snapshots and reconstruction

| Verb | Status | Description | Example |
|------|--------|-------------|---------|
| **create** | ✅ | Create new universe | `kgc universe create --name "Test Universe"` |
| **freeze** | ✅ | Snapshot to Git | `kgc universe freeze --id <uuid>` |
| **restore** | ✅ | Restore from snapshot | `kgc universe restore --snapshot <hash>` |
| **verify** | ✅ | Verify integrity | `kgc universe verify --id <uuid>` |
| **export** | ✅ | Export to RDF | `kgc universe export --id <uuid> --format ttl` |
| reconstruct | 🚧 | Rebuild from event log | `kgc universe reconstruct --from <eventlog-id>` |

**Common Options**:
- `--id <uuid>`: Universe identifier
- `--name <string>`: Human-readable name
- `--snapshot <hash>`: Snapshot hash
- `--format <ttl|jsonld|nq>`: Output format

---

## 2. `eventlog` (Event Sourcing)

**Package**: `@unrdf/kgc-4d`
**Description**: Append-only event log operations

| Verb | Status | Description | Example |
|------|--------|-------------|---------|
| **append** | ✅ | Append event | `kgc eventlog append --event <json>` |
| **replay** | ✅ | Replay events | `kgc eventlog replay --from <index>` |
| **reconstruct** | ✅ | Rebuild state | `kgc eventlog reconstruct --to <index>` |
| **verify** | ✅ | Verify log integrity | `kgc eventlog verify --id <uuid>` |
| **export** | ✅ | Export log | `kgc eventlog export --id <uuid> --format json` |

**Common Options**:
- `--id <uuid>`: Event log identifier
- `--event <json>`: Event payload
- `--from <index>`: Start index
- `--to <index>`: End index

---

## 3. `receipt` (Cryptographic Proofs)

**Package**: `@unrdf/blockchain`
**Description**: Receipt management and verification

| Verb | Status | Description | Example |
|------|--------|-------------|---------|
| **verify** | ✅ | Verify receipt | `kgc receipt verify --hash <sha256>` |
| **chain** | ✅ | Show chain | `kgc receipt chain --hash <sha256>` |
| **anchor** | ✅ | Anchor to blockchain | `kgc receipt anchor --hash <merkle-root> --blockchain ethereum` |
| **export** | ✅ | Export receipt | `kgc receipt export --hash <sha256> --format json` |

**Common Options**:
- `--hash <sha256>`: Receipt hash
- `--blockchain <ethereum|polygon>`: Blockchain network
- `--format <json|yaml>`: Output format

---

## 4. `policy` (Hooks & Governance)

**Package**: `@unrdf/hooks`
**Description**: Policy pack operations

| Verb | Status | Description | Example |
|------|--------|-------------|---------|
| **validate** | ✅ | Validate policy | `kgc policy validate --file hooks.mjs` |
| **apply** | ✅ | Activate policy | `kgc policy apply --name pre-commit` |
| **test** | ✅ | Test policy | `kgc policy test --suite integration` |
| **export** | ✅ | Export policy | `kgc policy export --name <policy> --format yaml` |

**Common Options**:
- `--file <path>`: Policy file
- `--name <string>`: Policy name
- `--suite <unit|integration>`: Test suite

---

## 5. `workflow` (YAWL Orchestration)

**Package**: `@unrdf/yawl`
**Description**: Workflow process management

| Verb | Status | Description | Example |
|------|--------|-------------|---------|
| **start** | ✅ | Start workflow | `kgc workflow start --definition workflow.yaml` |
| **pause** | ✅ | Pause workflow | `kgc workflow pause --id <workflow-id>` |
| **resume** | ✅ | Resume workflow | `kgc workflow resume --id <workflow-id>` |
| **verify** | ✅ | Verify definition | `kgc workflow verify --definition workflow.yaml` |
| **export** | ✅ | Export workflow | `kgc workflow export --id <workflow-id> --format yaml` |
| replay | 🚧 | Replay workflow | `kgc workflow replay --id <workflow-id>` |

**Common Options**:
- `--id <uuid>`: Workflow identifier
- `--definition <path>`: Workflow YAML
- `--format <yaml|json>`: Output format

---

## 6. `resource` (Resource Management)

**Package**: `@unrdf/core`
**Description**: Allocate and manage resources

| Verb | Status | Description | Example |
|------|--------|-------------|---------|
| **allocate** | ✅ | Allocate resource | `kgc resource allocate --type memory --amount 1GB` |
| **release** | ✅ | Release resource | `kgc resource release --id <resource-id>` |
| **query** | ✅ | Query resources | `kgc resource query --status active` |
| **export** | ✅ | Export resources | `kgc resource export --format json` |

**Common Options**:
- `--id <uuid>`: Resource identifier
- `--type <memory|cpu|disk>`: Resource type
- `--amount <size>`: Allocation size
- `--status <active|inactive>`: Filter status

---

## 7. `grammar` (SPARQL/SHACL/N3/OWL)

**Package**: `@unrdf/validation`
**Description**: Grammar validation and compilation

| Verb | Status | Description | Example |
|------|--------|-------------|---------|
| **validate** | ✅ | Validate grammar | `kgc grammar validate --file query.sparql` |
| **compile** | ✅ | Compile grammar | `kgc grammar compile --input shapes.shacl` |
| **parse** | ✅ | Parse input | `kgc grammar parse --format n3 --file rules.n3` |
| **export** | ✅ | Export grammar | `kgc grammar export --file <input> --format ast` |

**Common Options**:
- `--file <path>`: Input file
- `--format <sparql|shacl|n3|owl|ast>`: Grammar format
- `--input <path>`: Input file (alias)

---

## 8. `thesis` (Documentation)

**Package**: `@unrdf/docs`
**Description**: LaTeX thesis and documentation operations

| Verb | Status | Description | Example |
|------|--------|-------------|---------|
| **render** | ✅ | Render document | `kgc thesis render --input thesis.md --output pdf` |
| **compile** | ✅ | Compile LaTeX | `kgc thesis compile --file main.tex` |
| **validate** | ✅ | Validate citations | `kgc thesis validate --citations --references` |
| **export** | ✅ | Export document | `kgc thesis export --file thesis.md --format docx` |

**Common Options**:
- `--input <path>`: Input file
- `--output <pdf|html|docx>`: Output format
- `--file <path>`: File to process
- `--citations`: Validate citations
- `--references`: Validate references

---

## 9. `package` (Package Registry)

**Package**: `@unrdf/kgc-cli`
**Description**: Manage UNRDF packages

| Verb | Status | Description | Example |
|------|--------|-------------|---------|
| **list** | ✅ | List packages | `kgc package list --status enabled` |
| **install** | ✅ | Install package | `kgc package install --name @unrdf/new-package` |
| **validate** | ✅ | Validate package | `kgc package validate --contracts` |
| **export** | ✅ | Export metadata | `kgc package export --name <package> --format json` |

**Common Options**:
- `--name <string>`: Package name
- `--status <enabled|disabled>`: Filter status
- `--contracts`: Validate contracts
- `--format <json|yaml>`: Output format

---

## 10. `delta` (State Transitions)

**Package**: `@unrdf/v6-core`
**Description**: Δ (change carrier) operations

| Verb | Status | Description | Example |
|------|--------|-------------|---------|
| **propose** | ✅ | Propose delta | `kgc delta propose --file change.json` |
| **apply** | ✅ | Apply delta | `kgc delta apply --id <delta-id>` |
| **verify** | ✅ | Verify delta | `kgc delta verify --id <delta-id> --against <state-hash>` |
| **export** | ✅ | Export delta | `kgc delta export --id <delta-id> --format json` |
| compose | 🚧 | Compose deltas | `kgc delta compose --input delta1.json --input delta2.json` |
| invert | 🚧 | Invert delta (undo) | `kgc delta invert --file delta.json --output undo.json` |

**Common Options**:
- `--id <uuid>`: Delta identifier
- `--file <path>`: Delta file
- `--against <hash>`: State hash to verify against
- `--format <json|yaml>`: Output format

---

## Global Options

All commands support these global options:

```bash
kgc <noun> <verb> [command-options] [global-options]
```

| Option | Description | Default | Example |
|--------|-------------|---------|---------|
| `--help` | Show help | - | `kgc delta apply --help` |
| `--version` | Show version | - | `kgc --version` |
| `--verbose` | Verbose output | `false` | `kgc delta verify --verbose` |
| `--quiet` | Suppress output | `false` | `kgc delta apply --quiet` |
| `--json` | JSON output | `false` | `kgc receipt verify --json` |
| `--timeout <ms>` | Operation timeout | `5000` | `kgc workflow start --timeout 10000` |
| `--receipt` | Save receipt | `true` | `kgc delta apply --receipt false` |
| `--receipt-dir <path>` | Receipt directory | `.kgc/receipts` | `kgc delta apply --receipt-dir /tmp/receipts` |

---

## Receipt Output

All commands that emit receipts (25/25 verbs except `query` and `list`) save receipts to:

```
.kgc/receipts/<hash>.json
```

**Example Receipt**:
```json
{
  "hash": "sha256:abc123...",
  "timestamp": 1704067200000,
  "operation": "delta.apply",
  "inputs": { "deltaId": "delta-001" },
  "outputs": { "success": true },
  "proof": {
    "merkleRoot": "sha256:...",
    "signature": "ed25519:..."
  }
}
```

---

## Exit Codes

| Code | Meaning | Example |
|------|---------|---------|
| `0` | Success | Operation completed successfully |
| `1` | General error | Invalid arguments |
| `2` | Validation failed | Schema validation error |
| `3` | Receipt verification failed | Receipt hash mismatch |
| `4` | Timeout | Operation exceeded timeout |
| `5` | Permission denied | Requires authentication |
| `6` | Not found | Entity not found |

---

## Environment Variables

```bash
# Receipt storage directory
export KGC_RECEIPT_DIR=/custom/receipts

# Default timeout (ms)
export KGC_DEFAULT_TIMEOUT=10000

# Verbosity level (0=quiet, 1=normal, 2=verbose)
export KGC_VERBOSITY=1

# Blockchain network for anchoring
export KGC_BLOCKCHAIN_NETWORK=ethereum

# OTEL collector endpoint
export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
```

---

## Examples by Use Case

### Development Workflow

```bash
# Create universe
kgc universe create --name "Dev Universe"

# Propose delta
kgc delta propose --file my-change.json

# Verify delta
kgc delta verify --file my-change.json

# Apply delta
kgc delta apply --file my-change.json --universe <uuid>

# Verify receipt
kgc receipt verify --hash <sha256>
```

### Production Deployment

```bash
# Freeze production universe
kgc universe freeze --id <prod-uuid>

# Anchor snapshot to blockchain
kgc receipt anchor --hash <snapshot-hash> --blockchain ethereum

# Verify anchored snapshot
kgc receipt verify --hash <snapshot-hash> --blockchain ethereum
```

### Compliance Audit

```bash
# Export receipt chain
kgc receipt chain --hash <sha256> --export chain.json

# Verify entire chain
kgc receipt chain --hash <sha256> --verify

# Export universe state
kgc universe export --id <uuid> --format ttl > universe.ttl
```

---

## Summary

✅ 10 canonical nouns
✅ 25 canonical verbs
✅ 45 valid combinations
✅ All commands emit receipts (except `query` and `list`)
✅ Unified CLI pattern: `kgc <noun> <verb>`
✅ Global options for all commands
✅ Receipt storage and verification

**Next**: [API Reference](./02-api-reference.md)
