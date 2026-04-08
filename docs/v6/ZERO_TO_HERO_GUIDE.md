# UNRDF Zero to Hero Guide

> Complete learning path from absolute beginner to advanced UNRDF practitioner
>
> **Last Updated:** 2026-04-07  
> **UNRDF Version:** 26.4.8

---

## 🎯 What This Guide Covers

This guide takes you from zero knowledge of UNRDF to advanced usage across all four Diátaxis documentation types:

1. **Tutorials** - Hands-on, step-by-step lessons
2. **How-to Guides** - Practical solutions to specific problems
3. **Reference Material** - Technical specifications and APIs
4. **Explanation** - Deep dives into concepts and architecture

---

## 📚 Prerequisites

Before starting, ensure you have:

- **Node.js** >= 18.0.0 (check with `unrdf doctor`)
- **pnpm** >= 7.0.0 (check with `unrdf doctor`)
- **Git** for cloning repositories
- **Basic familiarity** with RDF/SPARQL concepts

**Quick Health Check:**

```bash
# Run the doctor command to verify your environment
unrdf doctor
```

---

## 🎓 Learning Path Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    ZERO TO HERO PATH                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Phase 1: Foundations      (30-60 min)  ← START HERE          │
│  ├─ Tutorial: Getting Started                                    │
│  ├─ How-to: Migrate from Legacy Versions                        │
│  └─ Explanation: O* Innovations Architecture                    │
│                                                                 │
│  Phase 2: Core Skills      (2-4 hours)                           │
│  ├─ Tutorial: Compose Deltas                                   │
│  ├─ How-to: Verify Receipt Chain                               │
│  ├─ How-to: Cross-Package Integration                          │
│  └─ Reference: CLI Command Matrix                               │
│                                                                 │
│  Phase 3: Advanced Topics  (4-8 hours)                          │
│  ├─ How-to: Implement L5 Maturity                              │
│  ├─ Explanation: Federation Quorum                              │
│  ├─ Explanation: Hooks Marketplace                             │
│  └─ Explanation: Streaming Admission                           │
│                                                                 │
│  Phase 4: Production       (ongoing)                             │
│  ├─ Doctor Command: Health Monitoring                          │
│  ├─ How-to: Kubernetes Deployment                              │
│  └─ Reference: Troubleshooting Guide                            │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Phase 1: Foundations (30-60 minutes)

### Goal: Set up your environment and understand core concepts

### 1.1 Tutorial: Getting Started

**File:** [`docs/v6/diataxis/tutorials/01-getting-started.md`](tutorials/01-getting-started.md)

**What You'll Learn:**

- Install UNRDF CLI and verify setup
- Create your first RDF store
- Run basic SPARQL queries
- Understand the architecture

**Commands:**

```bash
# Install UNRDF CLI
npm install -g @unrdf/cli

# Verify installation
unrdf --version

# Run health check
unrdf doctor

# Initialize a new project
unrdf init my-first-project
cd my-first-project

# Start the daemon
unrdf daemon start
```

**Expected Outcome:**

- ✅ UNRDF CLI installed and working
- ✅ Doctor reports all systems healthy
- ✅ Daemon running on port 8089
- ✅ Basic RDF operations working

---

### 1.2 How-to: Migrate from Legacy Versions

**File:** [`docs/v6/diataxis/how-to/01-migrate-from-legacy.md`](how-to/01-migrate-from-legacy.md)

**What You'll Learn:**

- Key differences between legacy and current versions
- Breaking changes and migration steps
- Update your code for current APIs
- Migrate receipts and chains

**Key Changes:**

```javascript
// Legacy approach (deprecated)
const store = new Store();

// Current approach
import { createStore } from '@unrdf/core';
const store = await createStore();
```

**Migration Checklist:**

- [ ] Update package.json dependencies
- [ ] Run `unrdf doctor` to check environment
- [ ] Update imports to use ESM syntax
- [ ] Migrate receipt chains
- [ ] Test all SPARQL queries

---

### 1.3 Explanation: O\* Innovations Architecture

**File:** [`docs/v6/ARCHITECTURE.md`](../ARCHITECTURE.md)

**What You'll Learn:**

- The 4 O\* Innovations (Federation, Marketplace, Streaming, Admission)
- How they work together
- Why they matter for knowledge graphs

**Core Concepts:**

- **Federation Quorum (O\*4)**: Distributed query execution with M-of-N voting
- **Hooks Marketplace (O\*5)**: Autonomous behavior via SPARQL CONSTRUCT
- **Streaming Admission (O\*6)**: Delta receipts with chaining

---

## Phase 2: Core Skills (2-4 hours)

### Goal: Master day-to-day UNRDF operations

### 2.1 Tutorial: Compose Deltas

**File:** [`docs/v6/diataxis/how-to/02-compose-deltas.md`](how-to/02-compose-deltas.md)

**What You'll Learn:**

- Create receipt chains
- Compose multiple deltas
- Verify chain integrity
- Handle chain failures

**Example:**

```javascript
import { composeDeltas, createDelta } from '@unrdf/v6-core';

const delta1 = await createDelta({
  input: quads,
  output: newQuads,
});

const delta2 = await createDelta({
  input: newQuads,
  output: finalQuads,
});

const chain = await composeDeltas([delta1, delta2]);
console.log(chain.hash); // BLAKE3 hash of entire chain
```

---

### 2.2 How-to: Verify Receipt Chain

**File:** [`docs/v6/diataxis/how-to/03-verify-receipt-chain.md`](how-to/03-verify-receipt-chain.md)

**What You'll Learn:**

- Validate receipt signatures
- Check chain continuity
- Debug chain failures
- Use verification tools

**Commands:**

```bash
# Verify a chain
unrdf chain verify <chain-id>

# Check chain integrity
unrdf chain integrity <chain-id>

# Debug chain issues
unrdf chain debug <chain-id> --verbose
```

---

### 2.3 How-to: Cross-Package Integration

**File:** [`docs/v6/diataxis/how-to/05-cross-package-integration.md`](how-to/05-cross-package-integration.md)

**What You'll Learn:**

- Integrate @unrdf/core with @unrdf/daemon
- Use hooks from @unrdf/hooks
- Stream data with @unrdf/streaming
- Build federated queries

**Example Integration:**

```javascript
import { createStore } from '@unrdf/core';
import { createHooksEngine } from '@unrdf/hooks';
import { streamQuads } from '@unrdf/streaming';

const store = await createStore();
const hooks = await createHooksEngine(store);

// Stream quads through hooks
await streamQuads(quads, {
  onQuad: async quad => {
    await hooks.execute('pre-insert', { quad });
  },
});
```

---

### 2.4 Reference: CLI Command Matrix

**File:** [`docs/v6/diataxis/reference/01-cli-command-matrix.md`](reference/01-cli-command-matrix.md)

**What You'll Find:**

- Complete list of all CLI commands
- Command options and flags
- Usage examples
- Exit codes

**Essential Commands:**

```bash
# Health check
unrdf doctor [--mode quick|standard|full] [--format json|yaml]

# Daemon operations
unrdf daemon start|stop|status|restart

# Hooks operations
unrdf hooks list|execute|validate

# Federation operations
unrdf federation query|peers|status

# Streaming operations
unrdf stream start|stop|status
```

---

## Phase 3: Advanced Topics (4-8 hours)

### Goal: Build production-ready applications

### 3.1 How-to: Implement L5 Maturity

**File:** [`docs/v6/diataxis/how-to/04-implement-l5-maturity.md`](how-to/04-implement-l5-maturity.md)

**What You'll Learn:**

- What L5 maturity means
- Implement L5 governance
- Add L5 validation
- Test L5 compliance

**L5 Maturity Levels:**

- **L1**: Basic RDF storage
- **L2**: SPARQL queries
- **L3**: Hooks integration
- **L4**: Federation support
- **L5**: Full governance with receipts

---

### 3.2 Explanation: Federation Quorum

**Related Files:**

- [`docs/v6/ARCHITECTURE.md`](../ARCHITECTURE.md) (overview)
- [`packages/federation/README.md`](../../../packages/federation/README.md) (implementation)

**What You'll Learn:**

- M-of-N voting mechanics
- Quorum computation
- Consensus algorithms
- Fault tolerance

**Key Concept:**

```javascript
// M-of-N quorum: need M of N peers to agree
const quorum = {
  m: 3, // Need 3 agreements
  n: 5, // Out of 5 total peers
  threshold: 0.6, // 60% agreement required
};

// Execute federated query
const result = await federation.execute(query, { quorum });
```

---

### 3.3 Explanation: Hooks Marketplace

**Related Files:**

- [`docs/v6/ARCHITECTURE.md`](../ARCHITECTURE.md) (overview)
- [`packages/hooks/README.md`](../../../packages/hooks/README.md) (implementation)

**What You'll Learn:**

- SPARQL CONSTRUCT normalization
- N3 forward-chaining
- SHACL soft-fail validation
- Hook dependency resolution

**Example Hook:**

```sparql
# Hook: Validate incoming quads
CONSTRUCT {
  ?hook a :HookValid ;
    :status ?status .
}
WHERE {
  # N3 forward-chaining rules
  { ?quad a :InvalidQuad } => { ?hook :status :fail } .
  { ?quad a :ValidQuad } => { ?hook :status :pass } .
}
```

---

### 3.4 Explanation: Streaming Admission

**Related Files:**

- [`docs/v6/ARCHITECTURE.md`](../ARCHITECTURE.md) (overview)
- [`packages/streaming/README.md`](../../../packages/streaming/README.md) (implementation)

**What You'll Learn:**

- Delta receipts with chaining
- BLAKE3 hash verification
- Input/output/delta hashes
- Streaming large graphs

**Example:**

```javascript
import { createStream } from '@unrdf/streaming';

const stream = await createStream({
  onDelta: delta => {
    console.log('Delta hash:', delta.hash);
    console.log('Input hash:', delta.inputHash);
    console.log('Output hash:', delta.outputHash);
  },
});

await stream.process(largeDataset);
```

---

## Phase 4: Production (ongoing)

### Goal: Deploy and maintain UNRDF in production

### 4.1 Doctor Command: Health Monitoring

**File:** [`packages/cli/docs/doctor-command.md`](../../../packages/cli/docs/doctor-command.md)

**What You'll Learn:**

- Run comprehensive health checks
- Monitor system status
- Debug environment issues
- Use doctor in CI/CD pipelines

**Daily Workflow:**

```bash
# Morning health check
unrdf doctor

# Watch mode for monitoring
unrdf doctor --watch

# CI/CD integration
unrdf doctor --mode quick --format json --exit-code
```

**Health Categories:**

- **Environment**: Node.js, pnpm, env vars
- **System**: Build artifacts, daemon, MCP server, RDF store, ports, disk space
- **Quality**: Test coverage, linting, file size, circular deps
- **Integration**: Federation peers, GraphQL, sidecar, OTEL, Kubernetes

---

### 4.2 How-to: Kubernetes Deployment

**Related Files:**

- [`k8s/helm/unrdf-observability/`](../../../k8s/helm/unrdf-observability/) (Helm charts)
- [`playground/OTEL-K8S-DEPLOYMENT-GUIDE.md`](../../playground/OTEL-K8S-DEPLOYMENT-GUIDE.md)

**What You'll Learn:**

- Deploy UNRDF to Kubernetes
- Configure observability stack
- Set up monitoring and alerting
- Scale UNRDF services

**Quick Start:**

```bash
# Create Kind cluster
make k8s-create

# Deploy observability stack
make k8s-up

# Verify deployment
kubectl get pods -n unrdf-observability
unrdf doctor --category kubernetes
```

**Services Deployed:**

- Prometheus (metrics collection)
- Grafana (visualization)
- Tempo (trace storage)
- Loki (log aggregation)
- Pyroscope (profiling)

---

### 4.3 Reference: Troubleshooting Guide

**File:** [`docs/v6/TROUBLESHOOTING.md`](../TROUBLESHOOTING.md)

**What You'll Find:**

- Common issues and solutions
- Error message reference
- Debug techniques
- Performance tuning

**Common Issues:**

| Issue                  | Symptom                         | Solution                          | Command                                  |
| ---------------------- | ------------------------------- | --------------------------------- | ---------------------------------------- |
| **Daemon won't start** | "Port 8089 in use"              | Find and kill conflicting process | `unrdf doctor --category system`         |
| **Tests failing**      | "Cannot find module"            | Run pnpm install                  | `pnpm install`                           |
| **Memory issues**      | "JavaScript heap out of memory" | Increase Node.js memory limit     | `NODE_OPTIONS=--max-old-space-size=8192` |
| **Slow queries**       | Queries take >10s               | Check query plan, add indexes     | `unrdf doctor --category quality`        |

---

## 📖 Diátaxis Documentation Types

UNRDF documentation follows the **Diátaxis** framework, which organizes content into four types:

### 1. Tutorials 📚

**Purpose:** Learning-oriented, step-by-step lessons

**Characteristics:**

- Hands-on exercises
- Build something complete
- Minimal explanation, maximal doing
- "Follow these steps"

**Examples:**

- Getting Started with v6
- Build Your First Knowledge Graph
- Create a Custom Hook

**When to Use:**

- Learning a new concept
- Onboarding new team members
- Exploring features

---

### 2. How-to Guides 🛠️

**Purpose:** Problem-solving, practical solutions

**Characteristics:**

- Solve specific problems
- Assume basic knowledge
- Focus on "how", not "why"
- "Do this to achieve that"

**Examples:**

- Migrate v5 to v6
- Verify Receipt Chain
- Implement L5 Maturity

**When to Use:**

- Solving a specific problem
- Looking up a procedure
- Reference during development

---

### 3. Reference Material 📋

**Purpose:** Information lookup, technical specs

**Characteristics:**

- Factual, structured
- No tutorials or explanations
- "What is X?"
- API documentation, command references

**Examples:**

- CLI Command Matrix
- API Reference
- Configuration Options

**When to Use:**

- Looking up syntax/options
- Checking API signatures
- Quick fact checking

---

### 4. Explanation 🧠

**Purpose:** Understanding, deep dives

**Characteristics:**

- Context and background
- "Why" more than "how"
- Conceptual discussions
- Architecture and design

**Examples:**

- O\* Innovations Architecture
- Federation Quorum
- Hooks Marketplace

**When to Use:**

- Understanding architecture
- Learning design decisions
- Exploring concepts deeply

---

## 🎯 Quick Reference Card

### Essential Commands

```bash
# Health check (your new best friend)
unrdf doctor                    # Standard check (2min)
unrdf doctor --mode quick       # Quick check (30s)
unrdf doctor --mode full        # Full check (5min)
unrdf doctor --watch            # Continuous monitoring
unrdf doctor --format json       # Machine-readable output

# Daemon
unrdf daemon start              # Start background daemon
unrdf daemon status             # Check daemon status
unrdf daemon logs               # View daemon logs

# Core operations
unrdf init <project>            # Initialize new project
unrdf store create              # Create RDF store
unrdf query execute <file.sparql>  # Run SPARQL query

# Hooks
unrdf hooks list                # List available hooks
unrdf hooks execute <hook-name> # Execute a hook
unrdf hooks validate            # Validate hook definitions

# Federation
unrdf federation peers           # List federation peers
unrdf federation query           # Execute federated query

# Streaming
unrdf stream start              # Start streaming
unrdf stream status             # Check stream status
```

### Environment Variables

```bash
# Core
export UNRDF_STORE_PATH=./.unrdf/store    # RDF store location
export UNRDF_DAEMON_PORT=8089              # Daemon port
export UNRDF_LOG_LEVEL=info                # Log level

# OTEL
export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4317
export OTEL_SERVICE_NAME=my-service

# Federation
export FEDERATION_QUORUM_M=3              # M-of-N quorum
export FEDERATION_QUORUM_THRESHOLD=0.6     # Consensus threshold
```

---

## 🔗 Documentation Index

### Core Documentation

- **Architecture:** [`docs/v6/ARCHITECTURE.md`](../ARCHITECTURE.md)
- **Breaking Changes:** [`docs/v6/BREAKING-CHANGES.md`](../BREAKING-CHANGES.md)
- **Migration Guide:** [`docs/v6/MIGRATION-GUIDE.md`](../MIGRATION-GUIDE.md)
- **Troubleshooting:** [`docs/v6/TROUBLESHOOTING.md`](../TROUBLESHOOTING.md)

### Tutorials

- **Getting Started:** [`docs/v6/diataxis/tutorials/01-getting-started.md`](tutorials/01-getting-started.md)

### How-to Guides

- **Migrate from Legacy:** [`docs/v6/diataxis/how-to/01-migrate-from-legacy.md`](how-to/01-migrate-from-legacy.md)
- **Compose Deltas:** [`docs/v6/diataxis/how-to/02-compose-deltas.md`](how-to/02-compose-deltas.md)
- **Verify Receipt Chain:** [`docs/v6/diataxis/how-to/03-verify-receipt-chain.md`](how-to/03-verify-receipt-chain.md)
- **Implement L5 Maturity:** [`docs/v6/diataxis/how-to/04-implement-l5-maturity.md`](how-to/04-implement-l5-maturity.md)
- **Cross-Package Integration:** [`docs/v6/diataxis/how-to/05-cross-package-integration.md`](how-to/05-cross-package-integration.md)

### Reference

- **CLI Command Matrix:** [`docs/v6/diataxis/reference/01-cli-command-matrix.md`](reference/01-cli-command-matrix.md)

### Package-Specific Documentation

- **@unrdf/cli:** [`packages/cli/docs/doctor-command.md`](../../../packages/cli/docs/doctor-command.md)
- **@unrdf/daemon:** [`packages/daemon/README.md`](../../../packages/daemon/README.md)
- **@unrdf/federation:** [`packages/federation/README.md`](../../../packages/federation/README.md)
- **@unrdf/hooks:** [`packages/hooks/README.md`](../../../packages/hooks/README.md)
- **@unrdf/streaming:** [`packages/streaming/README.md`](../../../packages/streaming/README.md)

### Deployment

- **Kubernetes Deployment:** [`k8s/helm/unrdf-observability/README.md`](../../../k8s/helm/unrdf-observability/)
- **OTEL Deployment Guide:** [`playground/OTEL-K8S-DEPLOYMENT-GUIDE.md`](../../playground/OTEL-K8S-DEPLOYMENT-GUIDE.md)

---

## 🎓 Learning Checklist

Use this checklist to track your progress:

### Phase 1: Foundations

- [ ] Install UNRDF CLI
- [ ] Run `unrdf doctor` successfully
- [ ] Complete "Getting Started with v6" tutorial
- [ ] Read "O\* Innovations Architecture" explanation
- [ ] Understand Diátaxis documentation types

### Phase 2: Core Skills

- [ ] Complete "Compose Deltas" tutorial
- [ ] Verify a receipt chain
- [ ] Integrate 2+ packages
- [ ] Reference CLI command matrix
- [ ] Use `unrdf doctor` for troubleshooting

### Phase 3: Advanced Topics

- [ ] Implement L5 maturity
- [ ] Understand Federation Quorum
- [ ] Understand Hooks Marketplace
- [ ] Understand Streaming Admission
- [ ] Build a production-ready application

### Phase 4: Production

- [ ] Deploy to Kubernetes
- [ ] Set up monitoring with doctor command
- [ ] Configure observability stack
- [ ] Implement health checks in CI/CD
- [ ] Troubleshoot common issues

---

## 💡 Pro Tips

### Tip 1: Always Run Doctor First

When something doesn't work, run `unrdf doctor` first:

```bash
unrdf doctor
```

It checks 22 different aspects of your environment and often identifies the issue immediately.

### Tip 2: Use Watch Mode During Development

Monitor your environment in real-time:

```bash
unrdf doctor --watch
```

Refreshes every 5 seconds. Press Ctrl+C to exit.

### Tip 3: Machine-Readable Output for CI/CD

Get JSON output for automated processing:

```bash
unrdf doctor --format json > health-report.json
```

### Tip 4: Category-Specific Checks

Check only what you need:

```bash
unrdf doctor --category env          # Environment only
unrdf doctor --category system       # System only
unrdf doctor --category quality       # Code quality only
unrdf doctor --category integration   # Integrations only
```

### Tip 5: Auto-Fix Issues

Let doctor attempt to fix issues automatically:

```bash
unrdf doctor --fix
```

---

## 🆘 Getting Help

### Community

- **GitHub Issues:** https://github.com/seanchatmangpt/unrdf/issues
- **Documentation:** https://github.com/seanchatmangpt/unrdf/tree/main/docs

### Built-in Help

```bash
unrdf --help                    # General help
unrdf doctor --help             # Doctor command help
unrdf daemon --help            # Daemon help
```

### Debug Mode

```bash
# Enable verbose logging
export UNRDF_LOG_LEVEL=debug
unrdf doctor
```

---

## 📈 Next Steps

After completing this guide:

1. **Explore Examples:** Check out [`docs/v6/diataxis/examples/`](examples/) for real-world examples
2. **Build Something:** Create your own knowledge graph application
3. **Contribute:** Contribute hooks, tutorials, or bug fixes
4. **Stay Updated:** Watch the GitHub repository for new releases

---

## 📝 Version History

| Version | Date             | Changes                                                |
| ------- | ---------------- | ------------------------------------------------------ |
| 26.4.8  | 2026-04-07       | Added doctor command, created zero-to-hero guide       |
| 26.4.7  | 2026-04-07       | Open-Ontologies integration, Groq LLM, OTEL deployment |
| 26.4.6  | Earlier versions | See [CHANGELOG.md](../../CHANGELOG.md)                 |

---

**Happy Learning! 🚀**

_This guide is a living document. Contributions and suggestions are welcome._
