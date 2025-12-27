# UNRDF Substrate End-to-End Demo

This demo proves that UNRDF can function as a complete substrate across all 4 critical roles:

```
STORE -> DERIVE -> ENFORCE -> RENDER
```

## Quick Start

```bash
# Run the end-to-end demo
node exploration/substrate-demo/index.mjs
```

## What This Demo Proves

### Hypothesis

> "UNRDF can be used as a complete substrate by:
>
> 1. STORing RDF (ingest + canonicalize)
> 2. DERIVing new facts (SPARQL CONSTRUCT)
> 3. ENFORCing policies (hooks/validation)
> 4. RENDERing results (CLI reports + JSON output)"

### Proof Target

Single command that:

1. Loads RDF data into Oxigraph store
2. Canonicalizes and hashes the input
3. Derives new relationships via SPARQL CONSTRUCT
4. Applies policy hooks to derived data
5. Renders a human-readable report + JSON output

## Pipeline Steps

### STEP 1: STORE (RDF Ingestion)

- Loads test dataset (Alice/Bob network with 5 quads)
- Canonicalizes quad ordering (lexicographic sort by S, P, O, G)
- Computes SHA256 hash of canonical form

**Input**: 5 RDF quads (foaf:Person, foaf:name, foaf:knows)

### STEP 2: DERIVE (SPARQL CONSTRUCT)

- Executes SPARQL CONSTRUCT query to derive new relationships
- Query pattern: `foaf:knows` -> derives `hasFriend`, `isFriendOf`, `isConnected`
- Adds derived quads to store
- Hashes derived graph

**Output**: 3 derived quads

### STEP 3: ENFORCE (Policy/Hooks)

- Applies predicate whitelist policy
- Validates each derived quad against allowed predicates
- Generates receipt for each validation
- Counts allowed vs rejected

**Policy**: `predicate-whitelist` - only allows predicates in defined whitelist

### STEP 4: RENDER (Output)

- Generates human-readable console report
- Writes structured JSON report to `result-report.json`
- Includes timing metrics for each step

## Output Files

| File                 | Description               |
| -------------------- | ------------------------- |
| `index.mjs`          | Main orchestrator script  |
| `result-report.json` | JSON output from demo run |
| `README.md`          | This documentation        |

## JSON Report Structure

```json
{
  "timestamp": "ISO8601",
  "pipeline": "store-derive-enforce-render",
  "steps": {
    "store": { "input_quads": 5, "canonical_hash": "...", "status": "success" },
    "derive": { "derived_quads": 3, "sparql_query": "CONSTRUCT...", "status": "success" },
    "enforce": { "allowed": 3, "rejected": 0, "receipts": [...], "status": "success" },
    "render": { "report_format": "json+console", "status": "success" }
  },
  "summary": "5 input quads -> 3 derived -> 3 allowed -> JSON report"
}
```

## Evidence

### Verification Checklist

- [x] Store step completes (5 quads loaded)
- [x] Derive step completes (3+ quads derived via SPARQL CONSTRUCT)
- [x] Enforce step completes (receipts generated, all allowed)
- [x] Render step outputs JSON report + console summary
- [x] `result-report.json` written to disk
- [x] All 4 roles (STORE/DERIVE/ENFORCE/RENDER) executed in single pipeline

### Agent Integrations

This demo integrates patterns from:

| Agent   | Contribution                     |
| ------- | -------------------------------- |
| Agent 2 | Canonicalization + hashing logic |
| Agent 3 | SPARQL CONSTRUCT query patterns  |
| Agent 6 | Policy/hook machinery + receipts |
| Agent 7 | CLI output formatting patterns   |

## Dependencies

No additional dependencies beyond `@unrdf` packages:

- `@unrdf/oxigraph` - RDF store with SPARQL support
- Node.js crypto (SHA256 hashing)
- Node.js fs (file output)

## Surprises/Findings

1. SPARQL CONSTRUCT returns array of quads (not a store)
2. Canonicalization is purely lexicographic (not URDNA2015)
3. Policy receipts can be generated independently of hooks library
4. All steps complete in <50ms total

## Next Steps

To extend this demo:

1. Add SHACL validation in ENFORCE step
2. Add N3 reasoning in DERIVE step
3. Add multiple policy composition
4. Add streaming output for large datasets
