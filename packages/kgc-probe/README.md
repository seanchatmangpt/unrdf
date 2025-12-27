# @unrdf/kgc-probe

**Knowledge Graph Construction Probe** - Receipt-driven codebase analysis with deterministic observation records.

## Overview

KGC Probe is a multi-agent system that analyzes codebases and generates verifiable observations in RDF/Turtle format. All outputs are deterministic and cryptographically verifiable via hash chains.

## Features

- ✅ **Deterministic**: Same input → same output (stable sorting, hashing)
- ✅ **Verifiable**: Receipt chains with BLAKE3 hashing
- ✅ **Composable**: 10-agent architecture with independent shards
- ✅ **Secure**: Poka-yoke guards enforce filesystem/network boundaries
- ✅ **Fast**: Respects 5s timeout SLAs by default

## Installation

```bash
pnpm install @unrdf/kgc-probe
```

## Usage

### Scan Codebase

```bash
kgc probe scan --root ./src --out ./kgc-output
```

Options:
- `--root, -r <path>`: Allowed root path (repeatable)
- `--out, -o <dir>`: Output directory (default: `./kgc-output`)
- `--budget-ms <n>`: Time budget per agent in ms (default: 5000)
- `--samples <n>`: Benchmark sample count (default: 10)
- `--net-allow <host>`: Allowed network host (repeatable)

### Verify Receipt Chain

```bash
kgc probe verify --in ./kgc-output
```

### Generate Report

```bash
kgc probe report --in ./kgc-output
```

### Diff Two Scans

```bash
kgc probe diff ./scan1 ./scan2
```

## Output Files

After running `kgc probe scan`, the output directory contains:

- `observations.json` - JSON index of all observations
- `observations.ttl` - RDF/Turtle representation
- `receipts.json` - Receipt chain for verification
- `manifest.json` - Chain manifest with summary hash
- `guard-stats.json` - Poka-yoke guard denial statistics

## Architecture

### Agents

1. **Agent 1 - Orchestrator**: Package structure, CLI, merge logic
2. **Agent 2 - Import Analyzer**: ESM imports/exports
3. **Agent 3 - Dependency Analyzer**: package.json, versions, vulnerabilities
4. **Agent 4 - Pattern Detector**: Code patterns, anti-patterns
5. **Agent 5 - Metrics Collector**: Complexity, LOC, coupling
6. **Agent 6 - Security Scanner**: Secrets, vulnerabilities
7. **Agent 7 - Quality Auditor**: Lint violations, style issues
8. **Agent 8 - Performance Profiler**: Benchmarks, hot paths
9. **Agent 9 - Test Analyzer**: Coverage, assertions, flakiness
10. **Agent 10 - Doc Analyzer**: JSDoc, README completeness

### Observation Schema

All agents emit `Observation` records:

```javascript
{
  id: string,              // Deterministic hash
  category: string,        // 'file' | 'dependency' | 'pattern' | ...
  severity: string,        // 'trace' | 'debug' | 'info' | 'warn' | 'error' | 'fatal'
  message: string,         // Human-readable summary
  location?: {             // Source location (optional)
    file: string,
    line?: number,
    column?: number
  },
  data: Record<string, any>, // Structured payload
  metadata: {
    agentId: string,
    timestamp: string,
    probeVersion: string,
    budgetMs: number,
    actualMs: number
  },
  tags: string[],
  receiptHash?: string     // BLAKE3 hash for verification
}
```

### Poka-Yoke Guards

Guards enforce operational boundaries:

- **Filesystem**: Only access files within `--root` paths
- **Secrets**: Deny access to `.env`, `.pem`, SSH keys, etc.
- **Network**: Only access hosts in `--net-allow` (default: deny all)
- **Timeouts**: Maximum 10x default budget (50s if default is 5s)

All denials are logged as `guard` category observations.

### Receipt Chain

Every observation gets a receipt with:

```javascript
{
  observationId: string,
  hash: string,            // BLAKE3 of observation content
  previousHash: string,    // Hash of previous receipt (chain)
  index: number,           // Position in chain
  timestamp: string
}
```

Chain manifest:

```javascript
{
  count: number,
  firstHash: string,
  lastHash: string,
  chainHash: string,       // Hash of entire chain
  timestamp: string
}
```

## Programmatic API

```javascript
import { createOrchestrator } from '@unrdf/kgc-probe';
import { createObservation } from '@unrdf/kgc-probe/observation';

const orchestrator = createOrchestrator({
  outputDir: './output',
  allowedRoots: ['./src'],
  budgetMs: 5000
});

const obs = createObservation({
  category: 'file',
  severity: 'info',
  message: 'Found module',
  data: { path: './src/index.mjs' },
  metadata: {
    agentId: 'my-agent',
    probeVersion: '0.1.0',
    budgetMs: 5000,
    actualMs: 100,
    timestamp: new Date().toISOString()
  }
});

orchestrator.registerAgentOutput('my-agent', [obs]);
await orchestrator.writeOutputs();
```

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for guidelines.

## License

MIT
