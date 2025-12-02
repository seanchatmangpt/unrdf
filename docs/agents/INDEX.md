# UNRDF Agent Documentation Index

**Complete navigation guide for autonomic, hyper-intelligent coding agents**

```json
{
  "@context": {
    "@vocab": "urn:unrdf:agent-docs:",
    "schema": "http://schema.org/"
  },
  "@id": "urn:unrdf:agent-docs-index:v4.0.0",
  "@type": "agent-docs:Index",
  "version": "4.0.0",
  "structure": "diataxis",
  "targetAudience": ["AutonomicAgent", "HyperintelligentSystem", "CodingAgent"]
}
```

---

## Documentation Structure

### 📚 Tutorials (Learning-Oriented)

Step-by-step guides for acquiring capabilities:

1. **[01-capability-discovery.mjs](./tutorials/01-capability-discovery.mjs)**
   - Discover available capabilities via SPARQL
   - Verify preconditions
   - Invoke capabilities programmatically

2. **[02-mapek-integration.mjs](./tutorials/02-mapek-integration.mjs)**
   - Initialize project model
   - Run MAPEK cycles
   - Create Knowledge Hooks from findings

3. **[03-knowledge-hook-creation.mjs](./tutorials/03-knowledge-hook-creation.mjs)**
   - Create content-addressed conditions
   - Define and validate hooks
   - Register and execute hooks

### 🔧 How-To Guides (Task-Focused)

Problem-solving guides for specific tasks:

1. **[01-discover-capabilities.mjs](./how-to/01-discover-capabilities.mjs)**
   - Find capabilities by criteria
   - Match capabilities to tasks
   - Verify preconditions

2. **[02-execute-mapek-cycle.mjs](./how-to/02-execute-mapek-cycle.mjs)**
   - Execute single MAPEK cycle
   - Run continuous MAPEK loop
   - Interpret MAPEK results

### 📖 Reference (Technical Specifications)

Complete API contracts and schemas:

1. **[api-contracts.json](./reference/api-contracts.json)**
   - Function signatures
   - Parameter schemas
   - Return types
   - Error codes

2. **[error-codes.json](./reference/error-codes.json)**
   - Error code registry
   - Recovery procedures
   - Error categories

3. **[capabilities.jsonld](./reference/capabilities.jsonld)**
   - Capability registry
   - SLOs and preconditions
   - Input/output schemas

4. **[implementation.md](./reference/implementation.md)** + **[implementation.mjs](./reference/implementation.mjs)**
   - **Implementation file references**
   - **Code location mapping**
   - **Function line ranges**
   - **Test file locations**
   - **Pattern implementations**
   - **Machine-executable discovery functions**

5. **[quick-reference.md](./reference/quick-reference.md)**
   - Common operations
   - Quick lookup tables
   - Performance SLOs

### 💡 Explanation (Conceptual Understanding)

Deep architectural understanding:

1. **[architecture.md](./explanation/architecture.md)**
   - Core principles
   - MAPEK architecture
   - Knowledge Hooks architecture
   - Integration patterns

---

## Quick Start Paths

### Path 1: New Agent Integration

1. Read [README.md](./README.md) - Overview
2. Complete [Tutorial 01](./tutorials/01-capability-discovery.mjs) - Discover capabilities
3. Complete [Tutorial 02](./tutorials/02-mapek-integration.mjs) - MAPEK integration
4. Reference [api-contracts.json](./reference/api-contracts.json) - API specs

### Path 2: Adding New Capabilities

1. Read [architecture.md](./explanation/architecture.md) - Understand architecture
2. Reference [capabilities.jsonld](./reference/capabilities.jsonld) - Capability format
3. Complete [Tutorial 03](./tutorials/03-knowledge-hook-creation.mjs) - Hook creation
4. Reference [error-codes.json](./reference/error-codes.json) - Error handling

### Path 3: Troubleshooting

1. Reference [error-codes.json](./reference/error-codes.json) - Error lookup
2. Read [architecture.md](./explanation/architecture.md) - Understand system
3. Use [How-To 01](./how-to/01-discover-capabilities.mjs) - Verify capabilities
4. Use [How-To 02](./how-to/02-execute-mapek-cycle.mjs) - Debug MAPEK

---

## Key Concepts

### MAPEK (Monitor-Analyze-Plan-Execute-Knowledge)

Autonomic loop for continuous self-management:
- **Monitor:** Observe system state (gaps, types, hotspots, drift)
- **Analyze:** Calculate health scores and prioritize issues
- **Plan:** Decide auto-fixable vs. manual actions
- **Execute:** Apply fixes via Knowledge Hooks
- **Knowledge:** Learn patterns for future cycles

### Knowledge Hooks

Declarative, policy-driven triggers:
- **Content-Addressed:** Conditions identified by SHA-256 hash
- **Lifecycle Phases:** `before`, `run`, `after`
- **Transaction Integration:** Execute during ACID transactions
- **Cryptographic Provenance:** Signed receipts for audit

### Capability Discovery

Semantic discovery via SPARQL:
- Query capability registry
- Filter by domain, latency, throughput
- Verify preconditions
- Invoke programmatically

---

## Machine-Readable Formats

All documentation includes machine-readable formats:

- **JSON-LD:** Structured data with RDF context
- **JSON Schemas:** Formal API contracts
- **Executable Code:** `.mjs` files ready to run
- **SPARQL Queries:** Semantic discovery queries

---

## Integration Examples

### Example 1: Discover and Invoke Capability

```javascript
import { discoverCapabilitiesByCriteria, verifyCapabilityPreconditions } from './how-to/01-discover-capabilities.mjs';

// Find validation capabilities
const caps = await discoverCapabilitiesByCriteria({
  domain: 'validation',
  maxLatency: 10
});

// Verify preconditions
const verification = await verifyCapabilityPreconditions(caps[0].uri);

// Invoke if preconditions met
if (verification.allSatisfied) {
  // Invoke capability...
}
```

### Example 2: Run MAPEK and Create Hooks

```javascript
import { executeSingleMapekCycle } from './how-to/02-execute-mapek-cycle.mjs';
import { createAutonomicHooks } from 'unrdf/project-engine';

// Run MAPEK
const result = await executeSingleMapekCycle(process.cwd());

// Create hooks from findings
const hooks = createAutonomicHooks(result, projectStore);

// Register hooks
for (const hook of hooks) {
  await system.registerHook(hook);
}
```

---

## Performance Targets

| Operation | p99 Latency | Throughput |
|-----------|-------------|------------|
| Capability Discovery | < 50ms | 100/s |
| MAPEK Iteration | < 200ms | 5/s |
| Hook Definition | < 1ms | 50k/s |
| Hook Execution | < 10ms | 10k/min |
| Transaction | < 100ms | 500/s |

---

## Error Recovery

All errors include recovery procedures:
- **Validation Errors:** Fix input format
- **Precondition Errors:** Meet requirements
- **Execution Errors:** Retry or rollback
- **System Errors:** Restart or degrade

See [error-codes.json](./reference/error-codes.json) for complete error registry.

---

## Next Steps

1. **Start with Tutorials** - Learn capabilities step-by-step
2. **Use How-To Guides** - Solve specific problems
3. **Reference API Contracts** - Complete technical specs
4. **Understand Architecture** - Deep conceptual knowledge

---

## Version Information

- **Documentation Version:** 4.0.0
- **UNRDF Version:** 4.0.0
- **Last Updated:** 2024-01-01
- **Format:** Machine-readable (JSON-LD, executable code)

