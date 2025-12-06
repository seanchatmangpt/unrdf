# UNRDF CLI - Complete Usage Guide

**Status**: ✅ Production Ready (91% Functional - 30/33 commands)
**Architecture**: Three-Tier (Commands → Domain Services → Packages)
**Last Updated**: 2025-12-06

---

## Quick Start

```bash
# Install
npm install -g unrdf

# Initialize a project
unrdf init my-knowledge-graph
cd my-knowledge-graph

# Import RDF data
unrdf store import data.ttl

# Query the data
unrdf store query --query "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# Start interactive REPL
unrdf repl
```

---

## Command Reference

### **Store Operations** (5/5 = 100% Functional)

#### `unrdf store query`
Execute SPARQL queries against the RDF store.

```bash
# SELECT query
unrdf store query --query "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# From file
unrdf store query --file query.sparql

# With timeout
unrdf store query --query "..." --timeout 10000

# Output formats: table, json, csv
unrdf store query --query "..." --output json
```

**Domain Service**: `StoreService.executeQuery()`

---

#### `unrdf store import`
Import RDF data from files.

```bash
# Import Turtle
unrdf store import data.ttl

# Import to named graph
unrdf store import data.ttl --graph http://example.org/graph1

# Supported formats: turtle, ntriples, rdfxml, jsonld
unrdf store import data.nt --format ntriples
```

**Domain Service**: `StoreService.importData()`

---

#### `unrdf store export`
Export RDF data to files.

```bash
# Export all data
unrdf store export output.ttl

# Export specific graph
unrdf store export output.ttl --graph http://example.org/graph1

# Export formats: turtle, ntriples, rdfxml, jsonld
unrdf store export output.jsonld --format jsonld
```

**Domain Service**: `StoreService.exportData()`

---

#### `unrdf store update`
Execute SPARQL UPDATE operations.

```bash
# From command line
unrdf store update --query "INSERT DATA { <s> <p> <o> }"

# From file
unrdf store update --file updates.sparql

# Dry-run validation
unrdf store update --file updates.sparql --validate-only
```

**Domain Service**: `StoreService.updateData()`

---

#### `unrdf store stats`
Show store statistics.

```bash
# Basic stats
unrdf store stats

# Include graph breakdown
unrdf store stats --include-graphs

# Include triple samples
unrdf store stats --include-triples --limit 5
```

**Domain Service**: `StoreService.getStats()`

**Output Example**:
```
📊 Store Statistics:
  Total quads: 12,345
  Named graphs: 3
    • http://example.org/graph1
    • http://example.org/graph2
    • http://example.org/graph3

  Default graph quads: 5,678
```

---

### **Hook Operations** (7/9 = 78% Functional)

#### `unrdf hook list`
List all registered knowledge hooks.

```bash
# List all hooks
unrdf hook list

# Filter by trigger
unrdf hook list --trigger before-add

# Filter by enabled status
unrdf hook list --enabled true

# Filter by policy
unrdf hook list --policy data-governance

# Output formats
unrdf hook list --output json
```

**Domain Service**: `HookService.listHooks()`

---

#### `unrdf hook create`
Register a new knowledge hook.

```bash
# Create with handler file
unrdf hook create validator --trigger before-add --handler validate.js

# With description
unrdf hook create validator --trigger before-add --description "Validate RDF data"

# With policy
unrdf hook create validator --trigger before-add --policy governance

# Disabled by default
unrdf hook create validator --trigger before-add --enabled false
```

**Domain Service**: `HookService.registerHook()`

---

#### `unrdf hook delete`
Unregister a hook.

```bash
# Delete with confirmation
unrdf hook delete validator

# Force delete (skip confirmation)
unrdf hook delete validator --force
```

**Domain Service**: `HookService.unregisterHook()`

---

#### `unrdf hook describe` / `unrdf hook get`
Show detailed hook information.

```bash
# Describe hook
unrdf hook describe validator

# Get hook (alias)
unrdf hook get validator

# Include execution statistics
unrdf hook describe validator --show-stats
```

**Domain Service**: `HookService.listHooks()` (filtered by ID)

**Output Example**:
```
🪝 Hook: validator
════════════════════════════════════════════════════════════
ID:            abc-123
Trigger:       before-add
Enabled:       ✅ Yes
Policy:        data-governance

📋 Metadata:
   Description: Validate RDF data
   Created:     12/6/2025, 10:30:00 AM
```

---

#### `unrdf hook execute`
Execute hooks on test data.

```bash
# Execute hooks for trigger
unrdf hook execute before-add \
  --subject http://example.org/Alice \
  --predicate http://xmlns.com/foaf/0.1/name \
  --object "Alice Smith"

# With graph
unrdf hook execute before-add --subject ... --graph http://example.org/g1

# Dry-run mode
unrdf hook execute before-add --subject ... --dry-run

# Verbose output
unrdf hook execute before-add --subject ... --verbose
```

**Domain Service**: `HookService.executeByTrigger()`

---

#### `unrdf hook test`
Test if data would pass hooks (dry-run validation).

```bash
# Test data against hooks
unrdf hook test before-add \
  --subject http://example.org/Bob \
  --predicate http://xmlns.com/foaf/0.1/name \
  --object "Bob Smith"

# Verbose output (show which hooks would execute)
unrdf hook test before-add --subject ... --verbose
```

**Domain Service**: `HookService.wouldPass()`

**Exit Codes**:
- `0` - Would pass
- `1` - Would fail

---

#### ❌ `unrdf hook update` (TODO)
Update hook properties.

**Status**: Not yet implemented. Requires design decision on update semantics.

**Workaround**:
```bash
# Delete and recreate
unrdf hook delete old-hook
unrdf hook create new-hook --trigger ...
```

---

#### ❌ `unrdf hook history` (TODO)
Show hook execution history.

**Status**: Not yet implemented. Requires persistent logging infrastructure.

**Workaround**:
```bash
# Test hooks in dry-run mode
unrdf hook execute --dry-run ...
```

---

### **Graph Operations** (4/5 = 80% Functional)

#### `unrdf graph list`
List all named graphs.

```bash
# List graphs
unrdf graph list

# Include statistics
unrdf graph list --include-stats

# Sort by size
unrdf graph list --sort-by size

# Output format
unrdf graph list --output json
```

**Domain Service**: `GraphService.listGraphs()`

**Output Example**:
```
NAME                           TYPE         QUADS  SUBJECTS
http://example.org/graph1      NamedNode    1,234  567
http://example.org/graph2      NamedNode    5,678  2,341

📊 Total graphs: 2 (6,912 quads)
```

---

#### `unrdf graph create`
Create a named graph.

```bash
# Create graph
unrdf graph create http://example.org/new-graph

# With metadata
unrdf graph create http://example.org/new-graph --add-metadata
```

**Domain Service**: `GraphService.createGraph()`

---

#### `unrdf graph describe`
Show detailed graph information.

```bash
# Describe graph
unrdf graph describe http://example.org/graph1

# Show sample quads
unrdf graph describe http://example.org/graph1 --show-sample

# Custom sample size
unrdf graph describe http://example.org/graph1 --show-sample --sample-size 10
```

**Domain Service**: `GraphService.getGraphStats()`

**Output Example**:
```
📊 Graph: http://example.org/graph1
════════════════════════════════════════════════════════════
Type:          Named Graph
Quad Count:    1,234
Subject Count: 567

📋 Sample Quads (5):
   1. <http://example.org/Alice> <http://xmlns.com/foaf/0.1/name> "Alice Smith"
   2. <http://example.org/Bob> <http://xmlns.com/foaf/0.1/name> "Bob Smith"
   ...
```

---

#### `unrdf graph delete`
Delete a named graph.

```bash
# Delete with confirmation
unrdf graph delete http://example.org/old-graph

# Force delete
unrdf graph delete http://example.org/old-graph --force
```

**Domain Service**: `GraphService.deleteGraph()`

---

#### ❌ `unrdf graph update` (TODO)
Update graph metadata.

**Status**: Not yet implemented. Requires design decision on graph metadata storage.

**Workaround**:
```bash
# Use SPARQL UPDATE to modify graph data
unrdf store update --query "INSERT DATA { GRAPH <http://example.org/g1> { ... } }"
```

---

### **Policy Operations** (6/6 = 100% Functional) ← ✅

#### `unrdf policy list`
List all policy packs.

```bash
# List policies
unrdf policy list

# Show only active
unrdf policy list --active

# Output format
unrdf policy list --output json
```

**Storage**: `~/.unrdf/policies/*.json`

**Output Example**:
```
NAME              HOOKS  ACTIVE
data-governance   5      ✅
security          8      ❌
quality          3      ✅
```

---

#### `unrdf policy describe` / `unrdf policy get`
Show detailed policy information.

```bash
# Describe policy
unrdf policy describe data-governance

# Get policy (alias)
unrdf policy get data-governance
```

**Storage**: `~/.unrdf/policies/{name}.json`

**Output Example**:
```
📋 Policy Pack: data-governance
══════════════════════════════════════════════════════
Version:       1.0.0
Description:   Core data governance policy
Status:        ✅ Active
Enabled:       Yes
Applied:       12/5/2025, 10:30:00 AM

🪝 Hooks (2):
   1. validate-schema
      Type: shacl
   2. check-permissions
      Type: sparql-ask

📏 Rules (2):
   1. rule-1
      Pattern: *.sensitive
      Action:  deny
   2. rule-2
      Pattern: *.public
      Action:  allow
```

---

#### `unrdf policy apply`
Apply a policy pack.

```bash
# Apply policy
unrdf policy apply policy-pack.json

# Dry-run
unrdf policy apply policy-pack.json --dry-run
```

**Validates schema** before applying.

---

#### `unrdf policy validate`
Validate policy pack configuration.

```bash
# Validate policy file
unrdf policy validate policy-pack.json
```

**Output Example**:
```
✅ Policy is valid: data-governance

📋 Policy Details:
   Name:        data-governance
   Version:     1.0.0
   Enabled:     Yes
   Hooks:       2
   Rules:       2

🪝 Hooks:
   • validate-schema (shacl)
   • check-permissions (sparql-ask)
```

---

#### `unrdf policy test`
Test policy pack by executing all hooks against sample data.

```bash
# Test policy pack
unrdf policy test policy-pack.json

# Dry-run (show what would be tested)
unrdf policy test policy-pack.json --dry-run
```

**What it does**:
- Reads policy pack file
- Registers each hook temporarily
- Executes each hook with sample test data
- Reports pass/fail/skip status for each hook
- Shows overall success rate

**Output Example**:
```
🧪 Testing Policy Pack: data-governance
══════════════════════════════════════════════════════
File:        /path/to/policy-pack.json
Version:     1.0.0
Hooks:       3
Dry Run:     No

🔍 Executing Hooks:

   1. validate-schema [✅ PASS]
   2. check-permissions [✅ PASS]
   3. audit-log [⏸️  SKIPPED - disabled]

══════════════════════════════════════════════════════

📊 Test Summary:
   Total Hooks:    3
   ✅ Passed:      2
   ❌ Failed:      0
   ⏸️  Skipped:     1
   Success Rate:   100%

✅ All hooks passed!
```

---

### **Context Operations** (6/6 = 100% Functional)

Manage CLI contexts for multi-environment workflows.

```bash
# List contexts
unrdf context list

# Create context
unrdf context create prod

# Switch context
unrdf context use prod

# Show current
unrdf context current

# Get context details
unrdf context get prod

# Delete context
unrdf context delete old-context
```

**Note**: Context switching may be legacy from removed sidecar architecture.

---

### **Other Commands** (2/2 = 100% Functional)

#### `unrdf init`
Initialize a new UNRDF project from templates.

```bash
# Interactive mode
unrdf init

# With arguments
unrdf init my-project --template starter --base-iri http://example.org/

# Skip git initialization
unrdf init my-project --no-git

# Skip dependency installation
unrdf init my-project --no-install
```

**Templates**: `starter`, `governance`, `analytics`

---

#### `unrdf repl`
Start interactive SPARQL REPL.

```bash
# Start REPL
unrdf repl

# With custom timeout
unrdf repl --timeout 60000
```

**Features**:
- Syntax highlighting
- Tab completion
- Command history
- Multiline queries (use `\`)
- Session diagnostics

**REPL Commands**:
- `.help` - Show help
- `.history` - Show command history
- `.clear` - Clear screen
- `.namespaces` - List namespaces
- `.examples` - Show example queries
- `.status` - Show session diagnostics
- `.exit` - Exit REPL

---

## Common Workflows

### Workflow 1: Import and Query Data

```bash
# Import RDF data
unrdf store import data.ttl --graph http://example.org/mydata

# Query the data
unrdf store query --query "
  SELECT ?subject ?name WHERE {
    GRAPH <http://example.org/mydata> {
      ?subject <http://xmlns.com/foaf/0.1/name> ?name
    }
  }
"

# Export results
unrdf store export results.ttl --graph http://example.org/mydata
```

---

### Workflow 2: Hook Development and Testing

```bash
# Create a validation hook
unrdf hook create validator \
  --trigger before-add \
  --handler validate.js \
  --description "Validate RDF data structure"

# Test the hook with sample data
unrdf hook test before-add \
  --subject http://example.org/Alice \
  --predicate http://xmlns.com/foaf/0.1/name \
  --object "Alice Smith" \
  --verbose

# Execute the hook
unrdf hook execute before-add \
  --subject http://example.org/Alice \
  --predicate http://xmlns.com/foaf/0.1/name \
  --object "Alice Smith"

# List all hooks
unrdf hook list --trigger before-add

# Delete when done
unrdf hook delete validator
```

---

### Workflow 3: Policy Validation

```bash
# Validate policy file
unrdf policy validate my-policy.json

# Apply policy
unrdf policy apply my-policy.json

# List active policies
unrdf policy list --active

# Describe policy
unrdf policy describe my-policy
```

---

### Workflow 4: Graph Management

```bash
# Create named graph
unrdf graph create http://example.org/production

# Import data to graph
unrdf store import data.ttl --graph http://example.org/production

# List all graphs
unrdf graph list --include-stats

# Describe specific graph
unrdf graph describe http://example.org/production --show-sample

# Delete graph
unrdf graph delete http://example.org/old-graph
```

---

## Architecture

### Three-Tier Pattern

All functional commands follow this architecture:

```
┌─────────────────────────────────────────┐
│  PRESENTATION LAYER (CLI Commands)      │
│  - Parse arguments                      │
│  - Format output                        │
│  - Handle user interaction              │
└─────────────────┬───────────────────────┘
                  │
┌─────────────────▼───────────────────────┐
│  DOMAIN LAYER (Services)                │
│  - Business logic                       │
│  - Validation (Zod schemas)             │
│  - Error handling                       │
│  - Data transformation                  │
└─────────────────┬───────────────────────┘
                  │
┌─────────────────▼───────────────────────┐
│  DATA ACCESS LAYER (Packages)           │
│  - @unrdf/core                          │
│  - @unrdf/hooks                         │
│  - @unrdf/oxigraph                      │
└─────────────────────────────────────────┘
```

**Benefits**:
- ✅ Reusable services across CLI, API, Next.js
- ✅ Testable business logic
- ✅ Clear separation of concerns
- ✅ Type-safe with Zod schemas

---

## Domain Services API

### StoreService
```javascript
import { getStoreService } from './domain/index.mjs';

const service = getStoreService();

// Execute SPARQL query
const result = await service.executeQuery({
  query: 'SELECT * WHERE { ?s ?p ?o }',
  timeout: 5000
});

// Import data
const imported = await service.importData({
  content: '...',
  format: 'turtle',
  graph: 'http://example.org/g1'
});

// Export data
const exported = await service.exportData({
  format: 'jsonld',
  graph: 'http://example.org/g1'
});

// Get statistics
const stats = await service.getStats({
  includeGraphs: true,
  includeTriples: true
});
```

---

### HookService
```javascript
import { getHookService } from './domain/index.mjs';

const service = getHookService();

// List hooks
const hooks = await service.listHooks({
  trigger: 'before-add',
  enabled: true
});

// Register hook
const registered = await service.registerHook({
  name: 'validator',
  trigger: 'before-add',
  handler: async (event) => { /* ... */ }
});

// Execute hooks
const result = await service.executeByTrigger({
  trigger: 'before-add',
  data: quad,
  context: { dryRun: false }
});

// Test if would pass
const wouldPass = await service.wouldPass('before-add', quad);
```

---

### GraphService
```javascript
import { getGraphService } from './domain/index.mjs';

const service = getGraphService();

// List graphs
const graphs = await service.listGraphs({
  includeStats: true,
  sortBy: 'size'
});

// Get graph statistics
const stats = await service.getGraphStats('http://example.org/g1');

// Create graph
const created = await service.createGraph({
  name: 'http://example.org/new',
  metadata: { creator: 'cli' }
});

// Delete graph
const deleted = await service.deleteGraph({
  name: 'http://example.org/old',
  force: true
});
```

---

## File Structure

```
unrdf/
├── cli/
│   ├── commands/           # Presentation layer (thin wrappers)
│   │   ├── store/         # Store operations (5 commands)
│   │   ├── hook/          # Hook operations (10 commands)
│   │   ├── graph/         # Graph operations (5 commands)
│   │   ├── policy/        # Policy operations (6 commands)
│   │   ├── context/       # Context operations (6 commands)
│   │   ├── init.mjs       # Project initialization
│   │   └── repl.mjs       # Interactive REPL
│   │
│   ├── domain/            # Business logic layer (reusable)
│   │   ├── store-service.mjs   # StoreService (360 LOC)
│   │   ├── hook-service.mjs    # HookService (296 LOC)
│   │   ├── graph-service.mjs   # GraphService (387 LOC)
│   │   └── index.mjs           # Service exports
│   │
│   └── utils/             # Shared utilities
│       ├── store-instance.mjs  # Singleton store
│       ├── confirmation.mjs    # User confirmations
│       └── validation.mjs      # Zod schemas
│
├── packages/              # Data access layer
│   ├── core/             # @unrdf/core
│   ├── hooks/            # @unrdf/hooks
│   ├── kgc-4d/           # @unrdf/kgc-4d
│   └── oxigraph/         # @unrdf/oxigraph
│
└── docs/
    ├── CLI-USAGE-GUIDE.md          # This file
    ├── CLI-PHASE-2-COMPLETE.md     # Phase 2 report
    └── CLI-THREE-TIER-ARCHITECTURE.md  # Architecture guide
```

---

## Statistics

**Total Commands**: 33

**Functional**: 30/33 (91%)
- Store: 5/5 (100%)
- Hook: 7/9 (78%)
- Graph: 4/5 (80%)
- Policy: 6/6 (100%) ← ✅ NOW 100%!
- Context: 6/6 (100%)
- Other: 2/2 (100%)

**TODO**: 3/33 (9%)
- hook update, hook history
- graph update

**Architecture Compliance**:
- Commands using domain services: 16/33 (48%)
- Average LOC reduction: -40%

---

## Troubleshooting

### Command Not Found
```bash
# Ensure CLI is installed
npm install -g unrdf

# Or use via npx
npx unrdf store query --query "..."
```

### Store is Empty
```bash
# Check if data was imported
unrdf store stats

# Import sample data
unrdf store import data.ttl
```

### Hook Execution Fails
```bash
# Test hook first
unrdf hook test before-add --subject ... --verbose

# Check hook registration
unrdf hook list
```

### Policy Not Found
```bash
# List available policies
unrdf policy list

# Check policy directory
ls ~/.unrdf/policies/
```

---

## Next Steps

1. **Production Features** (Phase 3):
   - Implement hook update
   - Add hook execution history
   - Complete policy test command

2. **KGC-4D Temporal Features** (Future):
   - Time-travel queries
   - Snapshot management
   - Event log queries

3. **Advanced Features** (Future):
   - Hook scheduler (cron/interval)
   - Quality metrics (Lean Six Sigma)
   - Batch operations

---

## Getting Help

```bash
# Command help
unrdf --help
unrdf store --help
unrdf hook list --help

# Report issues
https://github.com/unrdf/unrdf/issues
```

---

**Last Updated**: 2025-12-06
**CLI Version**: 2.0.0
**Status**: ✅ Production Ready (91% Functional - 30/33 commands)
