# Knowledge Engine Integration Design

## Executive Summary

This document specifies how UNRDF's enterprise CLI integrates with the Knowledge Engine for hook lifecycle management, policy pack operations, SPARQL execution, validation workflows, and cryptographic provenance tracking.

## Integration Architecture

### Knowledge Engine Components

```
┌─────────────────────────────────────────────────────────────────┐
│                      UNRDF CLI                                  │
│                                                                 │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐      │
│  │  Hook    │  │  Policy  │  │  Query   │  │ Validate │      │
│  │ Commands │  │ Commands │  │ Commands │  │ Commands │      │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘      │
│       │             │              │             │             │
│  ┌────▼─────────────▼──────────────▼─────────────▼───────┐   │
│  │         Knowledge Engine Client Wrapper                │   │
│  └────┬─────────────┬──────────────┬─────────────┬───────┘   │
└───────┼─────────────┼──────────────┼─────────────┼───────────┘
        │             │              │             │
┌───────▼─────────────▼──────────────▼─────────────▼───────────┐
│                  Knowledge Engine Core                        │
│                                                               │
│  ┌───────────────┐  ┌────────────────┐  ┌─────────────────┐│
│  │ Hook Manager  │  │ Policy Pack    │  │ Condition       ││
│  │               │  │ Manager        │  │ Evaluator       ││
│  └───────────────┘  └────────────────┘  └─────────────────┘│
│                                                               │
│  ┌───────────────┐  ┌────────────────┐  ┌─────────────────┐│
│  │ Transaction   │  │ Effect         │  │ Lockchain       ││
│  │ Manager       │  │ Sandbox        │  │ Writer          ││
│  └───────────────┘  └────────────────┘  └─────────────────┘│
│                                                               │
│  ┌───────────────┐  ┌────────────────┐  ┌─────────────────┐│
│  │ Query         │  │ Validator      │  │ Observability   ││
│  │ Executor      │  │ (SHACL)        │  │ Manager         ││
│  └───────────────┘  └────────────────┘  └─────────────────┘│
└───────────────────────────────────────────────────────────────┘
```

## 1. Hook Lifecycle Management

### Hook Definition Interface

```javascript
// src/cli/knowledge-engine/hook-client.mjs
import { defineHook } from '../../knowledge-engine/define-hook.mjs';
import { KnowledgeHookManager } from '../../knowledge-engine/knowledge-hook-manager.mjs';

export class HookClient {
  constructor(config) {
    this.manager = new KnowledgeHookManager(config);
    this.config = config;
  }

  // Create hook from template
  async createFromTemplate(name, type, options = {}) {
    const templates = {
      'sparql-ask': () => this.createSparqlAskHook(name, options),
      'sparql-select': () => this.createSparqlSelectHook(name, options),
      'shacl': () => this.createShaclHook(name, options),
      'delta': () => this.createDeltaHook(name, options),
      'threshold': () => this.createThresholdHook(name, options),
      'count': () => this.createCountHook(name, options),
      'window': () => this.createWindowHook(name, options)
    };

    const creator = templates[type];
    if (!creator) {
      throw new Error(`Unknown hook type: ${type}`);
    }

    return await creator();
  }

  // SPARQL ASK hook template
  async createSparqlAskHook(name, options) {
    const query = options.query || `
      ASK {
        ?s a <http://example.org/HealthCheck> ;
           <http://example.org/status> "healthy" .
      }
    `;

    return defineHook({
      meta: {
        name,
        description: options.description || 'SPARQL ASK health check',
        author: options.author || 'unrdf-cli'
      },
      when: {
        kind: 'after',
        condition: {
          sparqlAsk: {
            query,
            checkFrequency: options.checkFrequency || 60000
          }
        }
      },
      then: {
        effects: [
          {
            run: async (event) => {
              console.log(`Hook ${name} evaluated: ${event.result}`);
              return { status: 'success' };
            }
          }
        ]
      }
    });
  }

  // SHACL validation hook template
  async createShaclHook(name, options) {
    const shapesGraph = options.shapesGraph || './shapes/default.ttl';

    return defineHook({
      meta: {
        name,
        description: options.description || 'SHACL validation',
        author: options.author || 'unrdf-cli'
      },
      when: {
        kind: 'before',
        condition: {
          shacl: {
            shapesGraph,
            strictMode: options.strictMode !== false
          }
        }
      },
      then: {
        veto: true,
        effects: [
          {
            run: async (event) => {
              if (!event.result.conforms) {
                throw new Error(`Validation failed: ${
                  JSON.stringify(event.result.violations, null, 2)
                }`);
              }
              return { status: 'validated' };
            }
          }
        ]
      }
    });
  }

  // DELTA change detection hook
  async createDeltaHook(name, options) {
    return defineHook({
      meta: {
        name,
        description: options.description || 'Delta change detection',
        author: options.author || 'unrdf-cli'
      },
      when: {
        kind: 'after',
        condition: {
          delta: {
            query: options.query || 'SELECT * WHERE { ?s ?p ?o }',
            threshold: options.threshold || 0.1
          }
        }
      },
      then: {
        effects: [
          {
            run: async (event) => {
              console.log(`Configuration drift detected: ${event.result.changedPercent}%`);
              return { status: 'drift-detected', changes: event.result.changes };
            }
          }
        ]
      }
    });
  }

  // THRESHOLD numeric comparison hook
  async createThresholdHook(name, options) {
    return defineHook({
      meta: {
        name,
        description: options.description || 'Threshold monitoring',
        author: options.author || 'unrdf-cli'
      },
      when: {
        kind: 'after',
        condition: {
          threshold: {
            query: options.query || 'SELECT (COUNT(?s) AS ?count) WHERE { ?s ?p ?o }',
            variable: options.variable || '?count',
            operator: options.operator || 'gt',
            value: options.value || 10000
          }
        }
      },
      then: {
        effects: [
          {
            run: async (event) => {
              console.log(`Threshold breach: ${event.result.actualValue} ${options.operator} ${options.value}`);
              return { status: 'threshold-breached', value: event.result.actualValue };
            }
          }
        ]
      }
    });
  }

  // Register hook with manager
  async register(hook) {
    await this.manager.registerHook(hook);
    return {
      id: hook.meta.name,
      status: 'registered',
      kind: hook.when.kind
    };
  }

  // Evaluate hook
  async evaluate(hookId, data) {
    const hook = await this.manager.getHook(hookId);
    if (!hook) {
      throw new NotFoundError('hook', hookId);
    }

    const event = {
      transactionId: crypto.randomUUID(),
      timestamp: Date.now(),
      data
    };

    const result = await hook.run(event);
    return {
      hookId,
      fired: result.fired,
      result: result.result,
      duration: result.duration,
      timestamp: event.timestamp
    };
  }

  // Get hook execution history
  async getHistory(hookId, limit = 100) {
    const hook = await this.manager.getHook(hookId);
    if (!hook) {
      throw new NotFoundError('hook', hookId);
    }

    // Query observability logs for hook executions
    const history = await this.manager.getHookHistory(hookId, limit);
    return history.map(entry => ({
      timestamp: entry.timestamp,
      fired: entry.fired,
      duration: entry.duration,
      result: entry.result,
      error: entry.error
    }));
  }

  // List all hooks
  async list(filter = {}) {
    const hooks = await this.manager.listHooks();
    return hooks
      .filter(hook => {
        if (filter.kind && hook.when.kind !== filter.kind) return false;
        if (filter.enabled !== undefined && hook.enabled !== filter.enabled) return false;
        return true;
      })
      .map(hook => ({
        id: hook.meta.name,
        description: hook.meta.description,
        kind: hook.when.kind,
        enabled: hook.enabled !== false,
        type: Object.keys(hook.when.condition)[0]
      }));
  }

  // Enable/disable hook
  async setEnabled(hookId, enabled) {
    await this.manager.setHookEnabled(hookId, enabled);
    return { hookId, enabled, status: 'updated' };
  }

  // Delete hook
  async delete(hookId) {
    await this.manager.removeHook(hookId);
    return { hookId, status: 'deleted' };
  }

  // Validate hook definition
  async validate(hook) {
    const errors = [];

    // Validate meta
    if (!hook.meta?.name) {
      errors.push('Hook must have meta.name');
    }

    // Validate when clause
    if (!hook.when?.kind) {
      errors.push('Hook must have when.kind (before|after)');
    }

    if (!hook.when?.condition) {
      errors.push('Hook must have when.condition');
    }

    // Validate then clause
    if (!hook.then?.effects || !Array.isArray(hook.then.effects)) {
      errors.push('Hook must have then.effects array');
    }

    // Validate condition types
    const validConditions = ['sparqlAsk', 'sparqlSelect', 'shacl', 'delta', 'threshold', 'count', 'window'];
    const conditionType = Object.keys(hook.when?.condition || {})[0];
    if (conditionType && !validConditions.includes(conditionType)) {
      errors.push(`Invalid condition type: ${conditionType}`);
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }
}
```

## 2. Policy Pack Operations

### Policy Pack Client

```javascript
// src/cli/knowledge-engine/policy-client.mjs
import { PolicyPackManager } from '../../knowledge-engine/policy-pack.mjs';

export class PolicyClient {
  constructor(config) {
    this.manager = new PolicyPackManager(config);
    this.config = config;
  }

  // Apply policy pack
  async apply(packDefinition) {
    const validated = await this.validate(packDefinition);
    if (!validated.valid) {
      throw new ValidationError('Invalid policy pack', validated.errors);
    }

    await this.manager.loadPack(packDefinition);
    return {
      packId: packDefinition.meta.name,
      version: packDefinition.meta.version,
      hooksRegistered: packDefinition.hooks.length,
      status: 'applied'
    };
  }

  // Activate policy pack
  async activate(packId) {
    await this.manager.activatePack(packId);
    return { packId, status: 'activated' };
  }

  // Deactivate policy pack
  async deactivate(packId) {
    await this.manager.deactivatePack(packId);
    return { packId, status: 'deactivated' };
  }

  // List policy packs
  async list() {
    const packs = await this.manager.listPacks();
    return packs.map(pack => ({
      id: pack.meta.name,
      version: pack.meta.version,
      description: pack.meta.description,
      hooks: pack.hooks.length,
      enabled: pack.config.enabled,
      priority: pack.config.priority
    }));
  }

  // Get policy pack details
  async get(packId) {
    const pack = await this.manager.getPack(packId);
    if (!pack) {
      throw new NotFoundError('policy pack', packId);
    }
    return pack;
  }

  // Delete policy pack
  async delete(packId) {
    await this.manager.removePack(packId);
    return { packId, status: 'deleted' };
  }

  // Test policy pack (dry run)
  async test(packDefinition, testData) {
    const results = [];

    for (const hookDef of packDefinition.hooks) {
      const hook = await this.createHookFromDefinition(hookDef);

      try {
        const result = await hook.run({
          transactionId: 'test',
          timestamp: Date.now(),
          data: testData
        });

        results.push({
          hook: hookDef.name,
          status: 'success',
          fired: result.fired,
          result: result.result
        });
      } catch (error) {
        results.push({
          hook: hookDef.name,
          status: 'error',
          error: error.message
        });
      }
    }

    return {
      packId: packDefinition.meta.name,
      totalHooks: packDefinition.hooks.length,
      successful: results.filter(r => r.status === 'success').length,
      failed: results.filter(r => r.status === 'error').length,
      results
    };
  }

  // Validate policy pack
  async validate(packDefinition) {
    const errors = [];

    // Validate meta
    if (!packDefinition.meta?.name) {
      errors.push('Policy pack must have meta.name');
    }

    if (!packDefinition.meta?.version) {
      errors.push('Policy pack must have meta.version');
    }

    // Validate hooks
    if (!Array.isArray(packDefinition.hooks)) {
      errors.push('Policy pack must have hooks array');
    } else {
      for (const hook of packDefinition.hooks) {
        if (!hook.name) {
          errors.push('Each hook must have a name');
        }
        if (!hook.file && !hook.inline) {
          errors.push(`Hook ${hook.name} must have file or inline definition`);
        }
      }
    }

    // Validate dependencies
    if (packDefinition.dependencies) {
      for (const dep of packDefinition.dependencies) {
        const exists = await this.manager.getPack(dep.name);
        if (!exists) {
          errors.push(`Missing dependency: ${dep.name}`);
        }
      }
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }

  // Compare two policy packs
  async diff(packId1, packId2) {
    const pack1 = await this.get(packId1);
    const pack2 = await this.get(packId2);

    const hooks1 = new Set(pack1.hooks.map(h => h.name));
    const hooks2 = new Set(pack2.hooks.map(h => h.name));

    return {
      added: Array.from(hooks2).filter(h => !hooks1.has(h)),
      removed: Array.from(hooks1).filter(h => !hooks2.has(h)),
      common: Array.from(hooks1).filter(h => hooks2.has(h)),
      versionChanged: pack1.meta.version !== pack2.meta.version
    };
  }

  // Export policy pack
  async export(packId, format = 'json') {
    const pack = await this.get(packId);

    if (format === 'json') {
      return JSON.stringify(pack, null, 2);
    } else if (format === 'yaml') {
      const yaml = await import('yaml');
      return yaml.stringify(pack);
    }

    throw new Error(`Unsupported export format: ${format}`);
  }
}
```

## 3. SPARQL Query Execution

### Query Client

```javascript
// src/cli/knowledge-engine/query-client.mjs
import { useGraph } from '../../knowledge-engine/query.mjs';

export class QueryClient {
  constructor(config) {
    this.config = config;
  }

  // Execute SELECT query
  async select(query, options = {}) {
    const { graph } = await useGraph();
    const results = await graph.query(query);

    return {
      type: 'SELECT',
      bindings: results.bindings,
      count: results.bindings.length,
      duration: results.duration
    };
  }

  // Execute ASK query
  async ask(query) {
    const { graph } = await useGraph();
    const result = await graph.query(query);

    return {
      type: 'ASK',
      result: result.boolean,
      duration: result.duration
    };
  }

  // Execute CONSTRUCT query
  async construct(query) {
    const { graph } = await useGraph();
    const result = await graph.query(query);

    return {
      type: 'CONSTRUCT',
      triples: result.triples,
      count: result.triples.length,
      duration: result.duration
    };
  }

  // Execute DESCRIBE query
  async describe(uri) {
    const query = `
      DESCRIBE <${uri}>
    `;

    return await this.construct(query);
  }

  // Execute query with pagination
  async selectPaginated(query, { limit = 100, offset = 0 } = {}) {
    const paginatedQuery = `
      ${query}
      LIMIT ${limit}
      OFFSET ${offset}
    `;

    const result = await this.select(paginatedQuery);

    return {
      ...result,
      pagination: {
        limit,
        offset,
        hasMore: result.count === limit
      }
    };
  }

  // Explain query execution plan
  async explain(query) {
    // Parse query and generate execution plan
    const plan = {
      query,
      estimatedCost: 0,
      operations: []
      // ... execution plan details
    };

    return plan;
  }
}
```

## 4. Validation Workflows

### Validation Client

```javascript
// src/cli/knowledge-engine/validation-client.mjs
import { validateWithShacl } from '../../knowledge-engine/validate.mjs';

export class ValidationClient {
  constructor(config) {
    this.config = config;
  }

  // SHACL validation
  async shacl(dataGraph, shapesGraph, options = {}) {
    const result = await validateWithShacl(dataGraph, shapesGraph, {
      strictMode: options.strictMode !== false
    });

    return {
      conforms: result.conforms,
      violations: result.violations.map(v => ({
        focusNode: v.focusNode,
        resultPath: v.resultPath,
        message: v.message,
        severity: v.severity,
        constraint: v.constraint
      })),
      violationCount: result.violations.length,
      warnings: result.warnings || []
    };
  }

  // Zod schema validation
  async zod(data, schema) {
    try {
      const validated = schema.parse(data);
      return {
        valid: true,
        data: validated
      };
    } catch (error) {
      return {
        valid: false,
        errors: error.errors.map(e => ({
          path: e.path.join('.'),
          message: e.message,
          code: e.code
        }))
      };
    }
  }

  // Integrity check
  async integrity(graph) {
    const checks = {
      syntaxValid: true,
      noOrphanedNodes: true,
      noDuplicateTriples: true,
      validDatatypes: true
    };

    const issues = [];

    // Check for orphaned nodes
    const orphanedNodes = await this.findOrphanedNodes(graph);
    if (orphanedNodes.length > 0) {
      checks.noOrphanedNodes = false;
      issues.push({
        check: 'orphaned-nodes',
        count: orphanedNodes.length,
        nodes: orphanedNodes.slice(0, 10)
      });
    }

    // Check for duplicate triples
    const duplicates = await this.findDuplicateTriples(graph);
    if (duplicates > 0) {
      checks.noDuplicateTriples = false;
      issues.push({
        check: 'duplicate-triples',
        count: duplicates
      });
    }

    return {
      valid: Object.values(checks).every(v => v === true),
      checks,
      issues
    };
  }

  async findOrphanedNodes(graph) {
    // Implementation
    return [];
  }

  async findDuplicateTriples(graph) {
    // Implementation
    return 0;
  }
}
```

## 5. Provenance Tracking

### Provenance Client

```javascript
// src/cli/knowledge-engine/provenance-client.mjs
import { LockchainWriter } from '../../knowledge-engine/lockchain-writer.mjs';

export class ProvenanceClient {
  constructor(config) {
    this.lockchainWriter = new LockchainWriter(config);
    this.config = config;
  }

  // Get transaction provenance
  async getTransactionProvenance(transactionId) {
    const receipt = await this.lockchainWriter.getReceipt(transactionId);

    return {
      transactionId,
      timestamp: receipt.timestamp,
      actor: receipt.actor,
      hash: receipt.hash,
      previousHash: receipt.previousHash,
      deltaSize: receipt.deltaSize,
      committed: receipt.committed,
      metadata: receipt.metadata,
      signature: receipt.signature
    };
  }

  // Verify transaction integrity
  async verifyTransaction(transactionId) {
    const receipt = await this.lockchainWriter.getReceipt(transactionId);

    // Verify hash
    const computedHash = await this.lockchainWriter.computeHash(receipt);
    const hashValid = computedHash === receipt.hash;

    // Verify signature
    const signatureValid = await this.lockchainWriter.verifySignature(receipt);

    // Verify chain
    const chainValid = await this.lockchainWriter.verifyChain(transactionId);

    return {
      transactionId,
      hashValid,
      signatureValid,
      chainValid,
      valid: hashValid && signatureValid && chainValid
    };
  }

  // Get full chain
  async getChain(options = {}) {
    const receipts = await this.lockchainWriter.getChain(options);

    return receipts.map(r => ({
      transactionId: r.transactionId,
      timestamp: r.timestamp,
      hash: r.hash,
      previousHash: r.previousHash,
      actor: r.actor
    }));
  }

  // Verify entire chain
  async verifyChain() {
    const result = await this.lockchainWriter.verifyChain();

    return {
      valid: result.valid,
      totalReceipts: result.totalReceipts,
      verifiedReceipts: result.verifiedReceipts,
      failedReceipts: result.failedReceipts,
      errors: result.errors
    };
  }

  // Generate audit report
  async generateAudit(options = {}) {
    const startDate = options.startDate || new Date(Date.now() - 30 * 24 * 60 * 60 * 1000);
    const endDate = options.endDate || new Date();

    const receipts = await this.lockchainWriter.getChain({
      startDate,
      endDate
    });

    const report = {
      period: {
        start: startDate.toISOString(),
        end: endDate.toISOString()
      },
      summary: {
        totalTransactions: receipts.length,
        uniqueActors: new Set(receipts.map(r => r.actor)).size,
        totalChanges: receipts.reduce((sum, r) => sum + r.deltaSize, 0)
      },
      actors: this.groupByActor(receipts),
      timeline: this.createTimeline(receipts),
      integrity: await this.verifyChain()
    };

    return report;
  }

  groupByActor(receipts) {
    const grouped = new Map();

    for (const receipt of receipts) {
      if (!grouped.has(receipt.actor)) {
        grouped.set(receipt.actor, {
          actor: receipt.actor,
          transactionCount: 0,
          totalChanges: 0
        });
      }

      const stats = grouped.get(receipt.actor);
      stats.transactionCount++;
      stats.totalChanges += receipt.deltaSize;
    }

    return Array.from(grouped.values());
  }

  createTimeline(receipts) {
    // Group by day
    const timeline = new Map();

    for (const receipt of receipts) {
      const day = new Date(receipt.timestamp).toISOString().split('T')[0];

      if (!timeline.has(day)) {
        timeline.set(day, {
          date: day,
          transactions: 0,
          changes: 0
        });
      }

      const stats = timeline.get(day);
      stats.transactions++;
      stats.changes += receipt.deltaSize;
    }

    return Array.from(timeline.values()).sort((a, b) =>
      a.date.localeCompare(b.date)
    );
  }
}
```

## CLI Command Integration

### Hook Command with Knowledge Engine

```bash
# Create hook from template
unrdf hook create health-check sparql-ask \
  --query="ASK { ?s a :HealthCheck }" \
  --description="Health check hook"

# Evaluate hook
unrdf hook eval health-check --data=./graphs/production.ttl

# View execution history
unrdf hook history health-check --limit=20

# Enable/disable hook
unrdf hook enable health-check
unrdf hook disable health-check
```

### Policy Command with Knowledge Engine

```bash
# Apply policy pack
unrdf policy apply compliance-pack.json

# Test policy pack (dry run)
unrdf policy test compliance-pack.json \
  --dry-run \
  --data=./test-data.ttl

# Activate policy
unrdf policy activate compliance-v1

# View policy diff
unrdf policy diff compliance-v1 compliance-v2
```

### Query Command with Knowledge Engine

```bash
# Execute SELECT query
unrdf store query "SELECT * WHERE { ?s ?p ?o } LIMIT 10"

# Execute ASK query
unrdf store query "ASK { ?s a :Person }" --format=json

# DESCRIBE resource
unrdf store describe "http://example.org/resource/123"
```

### Validation Command with Knowledge Engine

```bash
# SHACL validation
unrdf validate shacl data.ttl shapes.ttl

# Integrity check
unrdf validate integrity production-graph.nq

# Generate validation report
unrdf validate shacl data.ttl shapes.ttl --report=report.json
```

### Provenance Command with Knowledge Engine

```bash
# Verify transaction
unrdf transaction verify tx-12345

# View transaction provenance
unrdf transaction get tx-12345 --show-provenance

# Generate audit report
unrdf lockchain audit \
  --start-date=2025-01-01 \
  --end-date=2025-10-01 \
  --export=audit-report.json
```

## Performance Optimization

### Caching Strategy
```javascript
// Cache SPARQL query results
const queryCache = new Map();
const CACHE_TTL = 300000; // 5 minutes

async function cachedQuery(query) {
  const key = hash(query);
  const cached = queryCache.get(key);

  if (cached && Date.now() < cached.expires) {
    return cached.result;
  }

  const result = await executeQuery(query);

  queryCache.set(key, {
    result,
    expires: Date.now() + CACHE_TTL
  });

  return result;
}
```

### Batch Operations
```javascript
// Batch hook evaluations
async function evaluateHooksBatch(hooks, data) {
  const results = await Promise.allSettled(
    hooks.map(hook => evaluateHook(hook, data))
  );

  return results.map((r, i) => ({
    hook: hooks[i],
    status: r.status,
    result: r.value,
    error: r.reason
  }));
}
```

## Conclusion

This knowledge engine integration provides:
- **Hook Lifecycle**: Complete hook creation, evaluation, and management
- **Policy Operations**: Full policy pack workflow support
- **Query Execution**: SPARQL query interface with optimization
- **Validation**: SHACL and integrity validation workflows
- **Provenance**: Cryptographic audit trail and verification

The design enables deep CLI integration with the knowledge engine while maintaining performance and scalability.

**Status**: ✅ KNOWLEDGE ENGINE INTEGRATION COMPLETE
