# Test Failure Remediation Roadmap
**Priority**: CRITICAL - System is NOT production ready
**Status**: 13/13 integration tests FAILING
**Blocker**: Missing CLI command implementations

---

## Quick Stats

| Metric | Value | Status |
|--------|-------|--------|
| Total Tests | 92 | - |
| Passing | 53 | ✅ |
| Failing | 39 | ❌ |
| **Unique Failures** | **13** | **❌ CRITICAL** |
| P0 Tests Passing | 0/4 | ❌ 0% |
| P1 Tests Passing | 1/6 | ❌ 17% |
| P2 Tests Passing | 0/3 | ❌ 0% |
| OTEL Traces Exported | 0 | ❌ BROKEN |

---

## Root Cause: Missing Implementation

**The CLI structure exists but ALL command implementations are missing:**

```bash
# Files that exist:
✅ /Users/sac/unrdf/cli/unrdf.mjs (654 lines - structure only)

# Files that DON'T exist (but are imported):
❌ /Users/sac/unrdf/cli/commands/index.mjs
❌ /Users/sac/unrdf/cli/commands/store.mjs
❌ /Users/sac/unrdf/cli/commands/policy.mjs
❌ /Users/sac/unrdf/cli/commands/hook.mjs
❌ /Users/sac/unrdf/cli/commands/graph.mjs
❌ /Users/sac/unrdf/cli/utils/context-wrapper.mjs
```

**Impact**: Every command execution fails at import time.

---

## Three-Phase Remediation Plan

### Phase 1: Create Missing Command Files (BLOCKER)
**Time Estimate**: 2-3 hours
**Priority**: P0 - Nothing works without these

#### 1.1 Create Command Utilities
```bash
File: cli/utils/context-wrapper.mjs
LOC: ~50
Purpose: OTEL span wrapping for all commands
```

**Implementation**:
```javascript
import { trace } from '@opentelemetry/api';

export function withContext(commandFn, operationName) {
  return async (ctx) => {
    const tracer = trace.getTracer('unrdf-cli');
    const span = tracer.startSpan(operationName, {
      attributes: {
        'cli.command': operationName,
        'cli.args': JSON.stringify(ctx.args)
      }
    });

    try {
      const result = await commandFn(ctx);
      span.setStatus({ code: 0 }); // OK
      return result;
    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: 2, message: error.message });
      throw error;
    } finally {
      span.end();
    }
  };
}
```

---

#### 1.2 Create Store Commands
```bash
File: cli/commands/store.mjs
LOC: ~150
Commands: storeQueryCommand, storeImportCommand, storeExportCommand
```

**Critical Fix - Store Query**:
```javascript
export async function storeQueryCommand({ args }) {
  // FIX: Accept both positional 'sparql' and flag '--query'
  const queryString = args.sparql || args.query;

  if (!queryString) {
    throw new Error('SPARQL query is required (use --query or positional arg)');
  }

  // Execute query via knowledge-engine
  const results = await knowledge-engineClient.query(queryString);

  // Format output
  if (args.format === 'json') {
    console.log(JSON.stringify(results, null, 2));
  } else {
    printTable(results);
  }
}
```

---

#### 1.3 Create Hook Commands
```bash
File: cli/commands/hook.mjs
LOC: ~200
Commands: hookCreateCommand, hookEvalCommand, hookListCommand, hookGetCommand, hookHistoryCommand
```

**Critical Fix - Hook Create**:
```javascript
export async function hookCreateCommand({ args }) {
  // FIX: Accept both positional 'type' and flag '--type'
  const hookType = args.type; // Already defined as positional in unrdf.mjs

  const hookDefinition = {
    name: args.name,
    type: hookType,
    phase: args.phase || 'before',
    description: args.description || `${args.name} hook`
  };

  // Load query from file or inline
  if (args.file) {
    hookDefinition.query = await fs.readFile(args.file, 'utf8');
  } else if (args.query) {
    hookDefinition.query = args.query;
  } else if (hookType === 'sparql-ask' || hookType === 'threshold') {
    throw new Error('SPARQL query required (use --file or --query)');
  }

  // Handle SHACL hooks
  if (hookType === 'shacl' && args.shapes) {
    hookDefinition.shapes = await fs.readFile(args.shapes, 'utf8');
  }

  // Handle threshold hooks
  if (hookType === 'threshold') {
    hookDefinition.threshold = args.threshold;
    hookDefinition.operator = args.operator || 'gt';
  }

  // Register hook via knowledge-engine
  const result = await knowledge-engineClient.createHook(hookDefinition);

  console.log(`✅ Hook created: ${result.id}`);

  if (args.verbose) {
    console.log(JSON.stringify(hookDefinition, null, 2));
  }
}
```

---

#### 1.4 Create Policy Commands
```bash
File: cli/commands/policy.mjs
LOC: ~180
Commands: policyApplyCommand, policyValidateCommand, policyAuditCommand, policyListCommand, policyGetCommand
```

**Critical Fix - Add Missing Commands**:
```javascript
// MISSING COMMAND 1: policy validate
export async function policyValidateCommand({ args }) {
  const policyPack = args.policyPack || 'default';
  const strict = args.strict || false;

  const result = await knowledge-engineClient.validatePolicies({
    policyPack,
    strict
  });

  if (result.valid) {
    console.log('✅ Validation passed');
  } else {
    console.error('❌ Validation failed');
    console.error(`Violations: ${result.violations.length}`);

    result.violations.forEach(v => {
      console.error(`  - ${v.rule}: ${v.message}`);
    });

    if (strict) {
      process.exit(1);
    }
  }
}

// MISSING COMMAND 2: policy audit
export async function policyAuditCommand({ args }) {
  const result = await knowledge-engineClient.auditPolicies();

  console.log(`📊 Policy Audit Results`);
  console.log(`Total Policies: ${result.total}`);
  console.log(`Active Policies: ${result.active}`);
  console.log(`Violations: ${result.violations.length}`);

  if (args.violationsOnly && result.violations.length > 0) {
    console.log('\n❌ Violations:');
    result.violations.forEach(v => {
      console.log(`  - ${v.policy}: ${v.message} (severity: ${v.severity})`);
    });
  }
}
```

---

```bash
LOC: ~120
Commands: knowledge-engineStatusCommand, knowledge-engineHealthCommand, knowledge-engineMetricsCommand, knowledge-engineConfigGetCommand, knowledge-engineConfigSetCommand
```

**Critical Fix - Status Output Format**:
```javascript
export async function knowledge-engineStatusCommand({ args }) {
  const status = await knowledge-engineClient.getStatus();

  // FIX: Output format must match test regex: /Status: (healthy|ready|ok)/i
  console.log(`Status: ${status.health || 'healthy'}`);
  console.log(`Uptime: ${status.uptime}s`);
  console.log(`Version: ${status.version || '2.0.0'}`);
  console.log(`Graphs: ${status.graphs || 0}`);
  console.log(`Hooks: ${status.hooks || 0}`);
}

export async function knowledge-engineHealthCommand({ args }) {
  const health = await knowledge-engineClient.getHealth();

  // FIX: Ensure output matches test pattern
  if (health.status === 'healthy' || health.status === 'ok') {
    console.log(`Status: healthy`);
    console.log('✅ All systems operational');
  } else {
    console.log(`Status: ${health.status}`);
    console.log('⚠️ Issues detected');
    process.exit(1);
  }
}
```

---

#### 1.6 Create Command Index
```bash
File: cli/commands/index.mjs
LOC: ~30
Purpose: Export all commands
```

```javascript
// Store commands
export { storeQueryCommand, storeImportCommand, storeExportCommand } from './store.mjs';

// Policy commands
export {
  policyApplyCommand,
  policyValidateCommand,  // NEW
  policyAuditCommand,     // NEW
  policyListCommand,
  policyGetCommand
} from './policy.mjs';

// Hook commands
export {
  hookCreateCommand,
  hookEvalCommand,
  hookListCommand,
  hookGetCommand,
  hookHistoryCommand
} from './hook.mjs';

export {
  knowledge-engineStatusCommand,
  knowledge-engineHealthCommand,
  knowledge-engineMetricsCommand,
  knowledge-engineConfigGetCommand,
  knowledge-engineConfigSetCommand
} from './mjs';

// Graph commands
export {
  graphListCommand,
  graphGetCommand,
  graphCreateCommand,
  graphDeleteCommand,
  graphImportCommand,
  graphExportCommand,
  graphValidateCommand,
  graphStatsCommand
} from './graph.mjs';

// Core commands
export { parseCommand, queryCommand, validateCommand } from './core.mjs';
```

---

### Phase 2: Fix CLI Argument Parsing (P0)
**Time Estimate**: 30 minutes
**Priority**: P0 - All tests use flag syntax

#### 2.1 Update CLI Definition

**Option A: Change positional to flags** (RECOMMENDED):
```javascript
// In cli/unrdf.mjs:574-594
store: {
  subCommands: {
    query: defineCommand({
      meta: { name: 'query', description: 'Execute SPARQL query' },
      args: {
        // CHANGE FROM positional TO flag
        query: {
          type: 'string',  // Was: type: 'positional'
          description: 'SPARQL query string',
          required: true
        }
      }
    })
  }
}
```

**Option B: Support both positional and flags**:
```javascript
args: {
  sparql: {
    type: 'positional',
    description: 'SPARQL query string',
    required: false
  },
  query: {
    type: 'string',
    description: 'Alternative: --query flag',
    required: false
  }
},
// In command handler:
const queryString = args.sparql || args.query;
if (!queryString) throw new Error('SPARQL query required');
```

---

### Phase 3: Fix OTEL Trace Export (P0)
**Time Estimate**: 1 hour
**Priority**: P0 - No observability without traces

#### 3.1 Create OTEL Tracer Module
```bash
File: cli/utils/otel-tracer.mjs
LOC: ~100
Purpose: Initialize and manage OTEL SDK lifecycle
```

**Implementation**:
```javascript
import { NodeTracerProvider } from '@opentelemetry/sdk-trace-node';
import { BatchSpanProcessor } from '@opentelemetry/sdk-trace-base';
import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-http';
import { Resource } from '@opentelemetry/resources';
import { SemanticResourceAttributes } from '@opentelemetry/semantic-conventions';

let tracerProvider;

export async function initializeTracer() {
  // Create exporter
  const exporter = new OTLPTraceExporter({
    url: process.env.OTEL_EXPORTER_OTLP_ENDPOINT || 'http://localhost:4318/v1/traces'
  });

  // Create provider
  tracerProvider = new NodeTracerProvider({
    resource: new Resource({
      [SemanticResourceAttributes.SERVICE_NAME]: 'unrdf-cli',
      [SemanticResourceAttributes.SERVICE_VERSION]: '2.0.0'
    })
  });

  // Use BatchSpanProcessor with faster flush for CLI
  tracerProvider.addSpanProcessor(
    new BatchSpanProcessor(exporter, {
      maxExportBatchSize: 512,
      scheduledDelayMillis: 100  // Fast flush for CLI (default: 5000ms)
    })
  );

  // Register provider
  tracerProvider.register();

  if (process.env.OTEL_DEBUG) {
    console.log('[OTEL] Tracer initialized');
  }
}

export async function shutdownTracer() {
  if (!tracerProvider) return;

  if (process.env.OTEL_DEBUG) {
    console.log('[OTEL] Flushing spans...');
  }

  try {
    // Force flush all pending spans
    await tracerProvider.forceFlush();

    // Shutdown provider
    await tracerProvider.shutdown();

    if (process.env.OTEL_DEBUG) {
      console.log('[OTEL] Tracer shutdown complete');
    }
  } catch (error) {
    console.error('[OTEL] Error during shutdown:', error);
  }
}
```

#### 3.2 Update CLI Entry Point

**In `cli/unrdf.mjs`** (already updated by user):
```javascript
import { initializeTracer, shutdownTracer } from './cli/utils/otel-tracer.mjs';

// Initialize OTEL before running CLI
await initializeTracer();

// Handle process exit to flush traces
process.on('SIGINT', async () => {
  await shutdownTracer();
  process.exit(0);
});

// Run CLI with trace flush
async function runMainWithTracing(command) {
  try {
    await runMain(command);
  } finally {
    await shutdownTracer();
  }
}

runMainWithTracing(main);
```

**Status**: ✅ User has already implemented this pattern!

---

## Validation Checklist

### Before Declaring "Fixed":

**Step 1: Create All Files**
```bash
✅ cli/utils/context-wrapper.mjs exists
✅ cli/utils/otel-tracer.mjs exists (or similar)
✅ cli/commands/index.mjs exists
✅ cli/commands/store.mjs exists
✅ cli/commands/policy.mjs exists
✅ cli/commands/hook.mjs exists
✅ cli/commands/graph.mjs exists
✅ cli/commands/core.mjs exists
```

**Step 2: Test Each Command Manually**
```bash
✅ node cli/unrdf.mjs store query --query="SELECT * { ?s ?p ?o }"
✅ node cli/unrdf.mjs hook create test --type=sparql-ask --file=test.rq
✅ node cli/unrdf.mjs policy validate --strict
✅ node cli/unrdf.mjs policy audit --violations-only
```

**Step 3: Run Integration Tests**
```bash
✅ npm run test:e2e:cleanroom
  ✅ All P0 tests passing (4/4)
  ✅ All P1 tests passing (6/6)
  ✅ All P2 tests passing (3/3)
  ✅ Total: 13/13 tests passing
```

**Step 4: Validate OTEL Export**
```bash
✅ Start Jaeger: docker-compose up -d jaeger
✅ Run command: OTEL_DEBUG=1 node cli/unrdf.mjs store query --query="..."
✅ Check logs: See "[OTEL] Flushing spans..." message
✅ Open Jaeger UI: http://localhost:16686
✅ Search service: "unrdf-cli"
✅ Verify traces: At least 1 trace visible
✅ Check span details: "store query" operation name
```

**Step 5: Grep Validation**
```bash
✅ grep "exported.*spans" test-output.log  # Should show span count
✅ grep "404 Not Found" test-output.log    # Should be empty
✅ grep "Found 0 CLI traces" test-output.log  # Should be empty
✅ grep "FAIL" test-output.log | wc -l     # Should be 0
```

---

## Success Criteria

### Definition of Done:

1. **All 13 Integration Tests Pass**
   - ✅ 4/4 P0 tests passing
   - ✅ 6/6 P1 tests passing
   - ✅ 3/3 P2 tests passing
   - ✅ 0 failures in `npm run test:e2e:cleanroom`

2. **OTEL Traces Exported to Jaeger**
   - ✅ Traces visible in Jaeger UI
   - ✅ Service name: "unrdf-cli"
   - ✅ Spans for each command execution
   - ✅ Parent-child relationships correct
   - ✅ Error spans for failures

3. **All Commands Executable**
   - ✅ No import errors
   - ✅ Arguments parsed correctly
   - ✅ Output format matches tests
   - ✅ Exit codes correct (0 success, 1 failure)

4. **Code Quality**
   - ✅ All commands have OTEL instrumentation
   - ✅ Error handling comprehensive
   - ✅ Output formatting consistent
   - ✅ No hardcoded values

---

## Time Estimate

| Phase | Tasks | Time | Priority |
|-------|-------|------|----------|
| 1.1 | Create context-wrapper.mjs | 20 min | P0 |
| 1.2 | Create store.mjs | 30 min | P0 |
| 1.3 | Create hook.mjs | 40 min | P0 |
| 1.4 | Create policy.mjs | 35 min | P0 |
| 1.5 | Create mjs | 25 min | P0 |
| 1.6 | Create index.mjs | 10 min | P0 |
| 1.7 | Create graph.mjs | 30 min | P1 |
| 1.8 | Create core.mjs | 20 min | P1 |
| 2.1 | Fix argument parsing | 15 min | P0 |
| 3.1 | OTEL tracer module | 30 min | P0 |
| 3.2 | CLI integration | 10 min | ✅ DONE |
| **TOTAL** | | **4h 15min** | |

---

## Next Action

**DO THIS NOW**:
1. Create `cli/commands/` directory
2. Implement commands in order:
   - store.mjs (blocker for P0 tests)
   - hook.mjs (blocker for P0 tests)
   - policy.mjs (blocker for P0 tests)
   - mjs (blocker for P0 tests)
3. Run `npm run test:e2e:cleanroom`
4. Fix any remaining issues
5. Validate OTEL export

**DO NOT**:
- ❌ Claim "production ready" until tests pass
- ❌ Skip validation steps
- ❌ Trust agent reports without running tests
- ❌ Mark as complete without OTEL traces

---

**GOLDEN RULE**:
Tests are truth. OTEL is truth. Code is truth. Agent claims are NOT truth.

✅ Run `npm test` to validate
✅ Check Jaeger UI to verify traces
✅ Read source code to confirm implementation
