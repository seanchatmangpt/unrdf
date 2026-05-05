# UNRDF Substrate Exploration

This directory contains a 10-agent concurrent exploration of UNRDF capabilities when used as a substrate (not a library).

## Quick Start

```bash
# Run the end-to-end substrate demo (crossing all 4 roles: Store → Derive → Enforce → Render)
node exploration/substrate-demo/index.mjs

# Or run individual agent explorations
node exploration/agents/agent-1/index.mjs  # Capability scanner
node exploration/agents/agent-2/index.mjs  # RDF IO
node exploration/agents/agent-3/index.mjs  # SPARQL Derivation
# ... and so on
```

## Directory Structure

```
exploration/
  spine/                      # Shared core module (all agents use)
    index.mjs
    jsdoc.md
  agents/
    agent-0/                  # Integrator notes (optional)
    agent-1/                  # Repository scanner → capability-map.json
    agent-2/                  # RDF IO + Canonicalization
    agent-3/                  # SPARQL Derivation / CONSTRUCT
    agent-4/                  # SHACL Validation
    agent-5/                  # N3 / Rule paths
    agent-6/                  # Workflow / Hooks / Receipts
    agent-7/                  # CLI Patterns / Citty integration
    agent-8/                  # Cross-Runtime feasibility
    agent-9/                  # End-to-End Demo assembly
  capability-map.json         # Machine-readable capability registry (Agent 1 output)
  substrate-demo/             # Final integrated demo
    index.mjs
    README.md
```

## Agent Missions

| Agent | Mission                                                 | Output                               | Proof                                    |
| ----- | ------------------------------------------------------- | ------------------------------------ | ---------------------------------------- |
| **1** | Scan UNRDF packages; map exports/APIs                   | `capability-map.json` + file paths   | Runnable `node agents/agent-1/index.mjs` |
| **2** | Load RDF → canonicalize → hash → report                 | Canonicalized triples + hashes       | Triple count, before/after               |
| **3** | SPARQL SELECT + CONSTRUCT derivation                    | Derivation report with counts        | Query results + derived graph            |
| **4** | Validate dataset against SHACL                          | Validation report (passed/failed)    | SHACL report output                      |
| **5** | N3 parse / reason / roundtrip                           | Parsed rules + roundtrip proof       | Serialization success                    |
| **6** | Define policy/hook; apply + emit receipt                | Receipt-like object                  | Hook applied + artifact                  |
| **7** | Build exploration CLI (noun-verb commands)              | Runnable CLI with help               | Command structure demo                   |
| **8** | Identify Node-only vs browser-capable modules           | Boundary map with evidence           | Cross-runtime test                       |
| **9** | Assemble end-to-end: ingest → derive → enforce → render | Single command running full pipeline | CLI report output                        |

## Shared Spine Module

All agents import from `exploration/spine/index.mjs`, which provides:

- `initializeStore()` - RDF store setup
- `createTestDataset()` - Sample quads (Alice/Bob network)
- `loadQuads(store, quads)` - Insert into store
- `createReport(...)` - Standardized report structure
- `printReport(report)` - Console output
- `discoverPackages()` - Package scanner helper

## Running the Full Exploration

```bash
# Run all agents sequentially (or in parallel if CI supports)
for i in {1..9}; do
  echo "Running Agent $i..."
  node exploration/agents/agent-$i/index.mjs || true
done

# View final capability map
cat exploration/capability-map.json | jq .

# Run the integrated demo
node exploration/substrate-demo/index.mjs
```

## Evidence-Based Requirements

Every exploration must:

1. **Have a hypothesis** - What are we testing?
2. **Define proof target** - What "works" means (e.g., "SPARQL CONSTRUCT returns N triples")
3. **Provide runnable command** - `node agents/agent-X/index.mjs` produces output
4. **Document surprises** - What broke? What wasn't expected?

## Capability Map Format

`capability-map.json` aggregates findings:

```json
{
  "packages": [
    {
      "name": "@unrdf/oxigraph",
      "path": "packages/oxigraph",
      "role": "store",
      "exports": ["createStore", "dataFactory"],
      "maturity": "mature",
      "notes": "Supports SPARQL query + CONSTRUCT"
    }
  ],
  "gaps": [
    {
      "capability": "SHACL validation",
      "status": "partial",
      "path": "packages/validation",
      "notes": "Found SHACL basics but CONSTRUCT not tested"
    }
  ]
}
```

## Success Criteria

✅ **Substrate Exploration is successful if:**

1. Each agent runs without error and produces a report
2. The capability map is machine-readable JSON
3. The end-to-end demo executes: **ingest → derive → enforce → render**
4. Every claim has runnable proof (not prose)
5. All modules are discoverable in `exploration/agents/`

## Notes

- No external dependencies beyond what's in `@unrdf` packages
- All code is `.mjs` + JSDoc (no TypeScript)
- No TypeScript compilation step required
- All explorations runnable with plain `node` in dev environment
