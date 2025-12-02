# Tutorial 4: Advanced Knowledge Hooks

**Time to Complete**: 25 minutes
**Difficulty**: Advanced
**Prerequisites**: Tutorials 1-3 completed

## What You'll Learn

In this tutorial, you'll learn how to:
- Use the `KnowledgeHookManager` to coordinate multiple hooks
- Create transaction-driven workflows with `TransactionManager`
- Build policy packs for governance
- Use SPARQL conditions for complex triggers
- Create audit trails with receipts

## Introduction

Advanced Knowledge Hooks enable you to build sophisticated autonomic systems that automatically validate, transform, and audit RDF data changes. This tutorial covers production-ready patterns for real-world applications.

## Step 1: Understanding Hook Types

Knowledge Hooks can be triggered by different conditions:

### SPARQL ASK Triggers
```javascript
when: {
  kind: 'sparql-ask',
  ref: {
    uri: 'file://hooks/check-condition.ask.rq',
    sha256: '...',
    mediaType: 'application/sparql-query',
  },
}
```

### SPARQL SELECT Triggers
```javascript
when: {
  kind: 'sparql-select',
  ref: {
    uri: 'file://hooks/find-violations.select.rq',
    sha256: '...',
    mediaType: 'application/sparql-query',
  },
}
```

### SHACL Shape Triggers
```javascript
when: {
  kind: 'shacl',
  ref: {
    uri: 'file://hooks/quality-shapes.shacl.ttl',
    sha256: '...',
    mediaType: 'text/turtle',
  },
}
```

## Step 2: Multiple Hooks with KnowledgeHookManager

The `KnowledgeHookManager` coordinates multiple hooks:

```javascript
import { defineHook } from 'unrdf/knowledge-engine';
import { DataFactory } from 'n3';

const { namedNode, literal, quad } = DataFactory;

// Hook 1: Validate data quality
const qualityHook = defineHook({
  meta: {
    name: 'quality:validation',
    description: 'Validate data quality on all changes',
    ontology: ['quality', 'shacl'],
  },
  channel: {
    graphs: ['urn:graph:data'],
    view: 'delta',
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/quality-check.ask.rq',
      sha256: 'abc123...',
      mediaType: 'application/sparql-query',
    },
  },
  determinism: { seed: 100 },
  receipt: { anchor: 'git-notes' },

  async before({ payload }) {
    console.log('[Quality Hook - BEFORE] Validating payload...');
    if (!payload || !payload.delta) {
      return { cancel: true, reason: 'Missing delta' };
    }
    return { ...payload, validated: true };
  },

  async run({ payload }) {
    console.log('[Quality Hook - RUN] Checking data quality...');

    // Simulate quality checks
    const violations = [];
    if (payload.delta.additions.length === 0) {
      violations.push('No additions found');
    }

    return {
      result: {
        passed: violations.length === 0,
        violations,
      },
      assertions: violations.length > 0 ? [
        quad(
          namedNode('urn:quality:report'),
          namedNode('urn:quality:status'),
          literal('failed')
        ),
      ] : [],
    };
  },

  async after({ result, cancelled }) {
    if (cancelled) {
      console.log('[Quality Hook - AFTER] Cancelled');
      return { result: { finalStatus: 'cancelled' } };
    }
    console.log('[Quality Hook - AFTER] Quality check complete');
    return { result: { ...result, audited: true } };
  },
});

// Hook 2: Audit trail
const auditHook = defineHook({
  meta: {
    name: 'audit:trail',
    description: 'Create audit trail for all transactions',
    ontology: ['prov', 'audit'],
  },
  channel: {
    graphs: ['urn:graph:data'],
    view: 'after',
  },
  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/audit-trigger.ask.rq',
      sha256: 'def456...',
      mediaType: 'application/sparql-query',
    },
  },
  determinism: { seed: 200 },
  receipt: { anchor: 'git-notes' },

  async before({ payload }) {
    console.log('[Audit Hook - BEFORE] Preparing audit...');
    return { ...payload, auditTimestamp: Date.now() };
  },

  async run({ payload }) {
    console.log('[Audit Hook - RUN] Creating audit record...');

    const auditRecord = {
      timestamp: payload.auditTimestamp,
      user: payload.user || 'system',
      action: 'transaction',
      delta: payload.delta,
    };

    return {
      result: auditRecord,
      assertions: [
        quad(
          namedNode(`urn:audit:${payload.auditTimestamp}`),
          namedNode('urn:prov:wasGeneratedBy'),
          literal('transaction-manager')
        ),
        quad(
          namedNode(`urn:audit:${payload.auditTimestamp}`),
          namedNode('urn:prov:atTime'),
          literal(new Date(payload.auditTimestamp).toISOString())
        ),
      ],
    };
  },

  async after({ result }) {
    console.log('[Audit Hook - AFTER] Audit trail created');
    return { result };
  },
});

// Hook 3: Notification
const notifyHook = defineHook({
  meta: {
    name: 'notify:changes',
    description: 'Notify subscribers of data changes',
    ontology: ['notify'],
  },
  channel: {
    graphs: ['urn:graph:data'],
    view: 'after',
  },
  when: {
    kind: 'sparql-select',
    ref: {
      uri: 'file://hooks/notify-changes.select.rq',
      sha256: 'ghi789...',
      mediaType: 'application/sparql-query',
    },
  },
  determinism: { seed: 300 },
  receipt: { anchor: 'none' },

  async before({ payload }) {
    console.log('[Notify Hook - BEFORE] Preparing notification...');
    return payload;
  },

  async run({ payload }) {
    console.log('[Notify Hook - RUN] Sending notifications...');

    // Simulate notification sending
    const notifications = {
      recipients: ['user@example.com'],
      message: `Data changed: ${payload.delta?.additions?.length || 0} additions`,
      sent: true,
    };

    return {
      result: notifications,
      assertions: [],
    };
  },

  async after({ result }) {
    console.log('[Notify Hook - AFTER] Notifications sent');
    return { result };
  },
});

console.log('✓ Three hooks defined');
console.log(`  1. ${qualityHook.meta.name}`);
console.log(`  2. ${auditHook.meta.name}`);
console.log(`  3. ${notifyHook.meta.name}`);
```

## Step 3: Transaction-Driven Workflow

Use `TransactionManager` to execute hooks on data changes:

```javascript
import { TransactionManager } from 'unrdf/knowledge-engine';
import { DataFactory, Store } from 'n3';

const { namedNode, literal, quad } = DataFactory;

async function runTransactionWorkflow() {
  console.log('\n=== Transaction Workflow ===\n');

  // Create transaction manager
  const txManager = new TransactionManager();

  // Register all hooks
  txManager.addHook(qualityHook);
  txManager.addHook(auditHook);
  txManager.addHook(notifyHook);

  console.log(`Registered ${txManager.getStats().hooksRegistered} hooks\n`);

  // Create a store
  const store = new Store();

  // Define a transaction delta
  const delta = {
    additions: [
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice Smith')
      ),
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/age'),
        literal('30', 'http://www.w3.org/2001/XMLSchema#integer')
      ),
    ],
    removals: [],
  };

  console.log('Applying transaction...\n');

  // Apply transaction (this triggers all hooks)
  const result = await txManager.apply(store, delta);

  console.log('\n=== Transaction Results ===');
  console.log(`Success: ${result.success}`);
  console.log(`Store size after: ${store.size}`);
  console.log(`Hooks executed: ${result.hooksExecuted?.length || 0}`);

  // Get statistics
  const stats = txManager.getStats();
  console.log('\n=== Transaction Manager Stats ===');
  console.log(`Hooks registered: ${stats.hooksRegistered}`);
  console.log(`Total transactions: ${stats.totalTransactions}`);
  console.log(`Successful: ${stats.successfulTransactions}`);
  console.log(`Failed: ${stats.failedTransactions}`);

  return result;
}

// Run the workflow
await runTransactionWorkflow();
```

**Output:**
```
=== Transaction Workflow ===

Registered 3 hooks

Applying transaction...

[Quality Hook - BEFORE] Validating payload...
[Quality Hook - RUN] Checking data quality...
[Quality Hook - AFTER] Quality check complete
[Audit Hook - BEFORE] Preparing audit...
[Audit Hook - RUN] Creating audit record...
[Audit Hook - AFTER] Audit trail created
[Notify Hook - BEFORE] Preparing notification...
[Notify Hook - RUN] Sending notifications...
[Notify Hook - AFTER] Notifications sent

=== Transaction Results ===
Success: true
Store size after: 2
Hooks executed: 3

=== Transaction Manager Stats ===
Hooks registered: 3
Total transactions: 1
Successful: 1
Failed: 0
```

## Step 4: Policy Packs for Governance

Policy packs group related hooks for governance:

```javascript
import { defineHook } from 'unrdf/knowledge-engine';

// Policy Pack: Financial Compliance
const financialCompliancePack = {
  name: 'financial:compliance-pack',
  version: '1.0.0',
  description: 'Financial transaction compliance policies',

  hooks: [
    // Policy 1: Large transaction monitoring
    defineHook({
      meta: {
        name: 'financial:large-transaction',
        description: 'Monitor transactions over $10,000',
        ontology: ['fibo', 'prov'],
      },
      channel: {
        graphs: ['urn:graph:financial:transactions'],
        view: 'delta',
      },
      when: {
        kind: 'sparql-select',
        ref: {
          uri: 'file://policies/large-transaction.select.rq',
          sha256: 'policy1...',
          mediaType: 'application/sparql-query',
        },
      },
      determinism: { seed: 1000 },
      receipt: { anchor: 'git-notes' },

      async before({ payload }) {
        return { ...payload, threshold: 10000 };
      },

      async run({ payload }) {
        console.log('[Policy 1] Checking large transactions...');

        // Check transaction amount
        const amount = payload.amount || 0;
        const flagged = amount > payload.threshold;

        return {
          result: {
            flagged,
            amount,
            threshold: payload.threshold,
          },
          assertions: flagged ? [
            quad(
              namedNode(`urn:transaction:${payload.id}`),
              namedNode('urn:financial:flagged'),
              literal('true')
            ),
          ] : [],
        };
      },

      async after({ result }) {
        if (result.result.flagged) {
          console.log(`  ⚠️  Transaction flagged: $${result.result.amount}`);
        }
        return { result };
      },
    }),

    // Policy 2: Fraud detection
    defineHook({
      meta: {
        name: 'financial:fraud-detection',
        description: 'Detect potentially fraudulent transactions',
        ontology: ['fibo', 'fraud'],
      },
      channel: {
        graphs: ['urn:graph:financial:transactions'],
        view: 'delta',
      },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://policies/fraud-check.ask.rq',
          sha256: 'policy2...',
          mediaType: 'application/sparql-query',
        },
      },
      determinism: { seed: 2000 },
      receipt: { anchor: 'git-notes' },

      async before({ payload }) {
        return payload;
      },

      async run({ payload }) {
        console.log('[Policy 2] Running fraud detection...');

        // Simulate fraud detection logic
        const suspicious = payload.amount > 50000 && payload.country !== 'US';

        return {
          result: {
            suspicious,
            reason: suspicious ? 'Large international transaction' : null,
          },
          assertions: suspicious ? [
            quad(
              namedNode(`urn:transaction:${payload.id}`),
              namedNode('urn:fraud:suspicious'),
              literal('true')
            ),
          ] : [],
        };
      },

      async after({ result }) {
        if (result.result.suspicious) {
          console.log(`  🚨 Suspicious activity: ${result.result.reason}`);
        }
        return { result };
      },
    }),

    // Policy 3: Audit trail
    defineHook({
      meta: {
        name: 'financial:audit-trail',
        description: 'Create compliance audit trail',
        ontology: ['prov', 'audit'],
      },
      channel: {
        graphs: ['urn:graph:financial:transactions'],
        view: 'after',
      },
      when: {
        kind: 'sparql-ask',
        ref: {
          uri: 'file://policies/audit-all.ask.rq',
          sha256: 'policy3...',
          mediaType: 'application/sparql-query',
        },
      },
      determinism: { seed: 3000 },
      receipt: { anchor: 'git-notes' },

      async before({ payload }) {
        return { ...payload, auditTime: Date.now() };
      },

      async run({ payload }) {
        console.log('[Policy 3] Creating audit record...');

        return {
          result: {
            audited: true,
            timestamp: payload.auditTime,
          },
          assertions: [
            quad(
              namedNode(`urn:audit:${payload.auditTime}`),
              namedNode('urn:prov:wasGeneratedBy'),
              literal('compliance-system')
            ),
          ],
        };
      },

      async after({ result }) {
        console.log('  ✓ Audit trail created');
        return { result };
      },
    }),
  ],
};

console.log('\n=== Financial Compliance Policy Pack ===');
console.log(`Name: ${financialCompliancePack.name}`);
console.log(`Version: ${financialCompliancePack.version}`);
console.log(`Policies: ${financialCompliancePack.hooks.length}`);
financialCompliancePack.hooks.forEach((hook, i) => {
  console.log(`  ${i + 1}. ${hook.meta.name}: ${hook.meta.description}`);
});
```

## Step 5: Execute Policy Pack

Apply the policy pack to transactions:

```javascript
async function applyPolicyPack() {
  console.log('\n=== Applying Policy Pack ===\n');

  // Create transaction manager
  const txManager = new TransactionManager();

  // Register all policy hooks
  financialCompliancePack.hooks.forEach(hook => {
    txManager.addHook(hook);
  });

  console.log(`Registered ${txManager.getStats().hooksRegistered} policy hooks\n`);

  // Test transaction 1: Normal transaction
  console.log('--- Transaction 1: Normal ($5,000) ---');
  const tx1Result = await executeTransaction(txManager, {
    id: 'tx001',
    amount: 5000,
    country: 'US',
  });
  console.log(`Result: ${tx1Result.success ? '✓ Approved' : '✗ Rejected'}\n`);

  // Test transaction 2: Large transaction
  console.log('--- Transaction 2: Large ($15,000) ---');
  const tx2Result = await executeTransaction(txManager, {
    id: 'tx002',
    amount: 15000,
    country: 'US',
  });
  console.log(`Result: ${tx2Result.success ? '✓ Approved' : '✗ Rejected'}\n`);

  // Test transaction 3: Suspicious transaction
  console.log('--- Transaction 3: Suspicious ($60,000 international) ---');
  const tx3Result = await executeTransaction(txManager, {
    id: 'tx003',
    amount: 60000,
    country: 'RU',
  });
  console.log(`Result: ${tx3Result.success ? '✓ Approved' : '✗ Rejected'}\n`);

  // Final statistics
  const stats = txManager.getStats();
  console.log('=== Final Statistics ===');
  console.log(`Total transactions: ${stats.totalTransactions}`);
  console.log(`Successful: ${stats.successfulTransactions}`);
  console.log(`Failed: ${stats.failedTransactions}`);
}

async function executeTransaction(txManager, transactionData) {
  const store = new Store();

  const delta = {
    additions: [
      quad(
        namedNode(`urn:transaction:${transactionData.id}`),
        namedNode('urn:financial:amount'),
        literal(transactionData.amount.toString())
      ),
      quad(
        namedNode(`urn:transaction:${transactionData.id}`),
        namedNode('urn:financial:country'),
        literal(transactionData.country)
      ),
    ],
    removals: [],
  };

  // Apply transaction with policies
  return await txManager.apply(store, delta, transactionData);
}

// Run policy pack
await applyPolicyPack();
```

**Output:**
```
=== Applying Policy Pack ===

Registered 3 policy hooks

--- Transaction 1: Normal ($5,000) ---
[Policy 1] Checking large transactions...
[Policy 2] Running fraud detection...
[Policy 3] Creating audit record...
  ✓ Audit trail created
Result: ✓ Approved

--- Transaction 2: Large ($15,000) ---
[Policy 1] Checking large transactions...
  ⚠️  Transaction flagged: $15000
[Policy 2] Running fraud detection...
[Policy 3] Creating audit record...
  ✓ Audit trail created
Result: ✓ Approved

--- Transaction 3: Suspicious ($60,000 international) ---
[Policy 1] Checking large transactions...
  ⚠️  Transaction flagged: $60000
[Policy 2] Running fraud detection...
  🚨 Suspicious activity: Large international transaction
[Policy 3] Creating audit record...
  ✓ Audit trail created
Result: ✓ Approved

=== Final Statistics ===
Total transactions: 3
Successful: 3
Failed: 0
```

## What You Learned

- How to create multiple specialized hooks
- How to use `KnowledgeHookManager` for coordination
- How to use `TransactionManager` for hook-driven transactions
- How to build policy packs for governance
- How to use different hook trigger types (SPARQL ASK, SELECT, SHACL)
- How to create audit trails with receipts
- How to monitor and track hook execution statistics

## Production Best Practices

1. **Determinism**: Always set a `seed` for reproducible execution
2. **Receipts**: Use `anchor: 'git-notes'` for production audit trails
3. **Error Handling**: Always validate payloads in `before` phase
4. **Cancellation**: Return `cancel: true` to abort execution
5. **Assertions**: Return RDF quads in `assertions` array to update the graph
6. **Statistics**: Monitor `TransactionManager.getStats()` for performance
7. **Testing**: Execute hooks manually before registering them

## Next Steps

You've completed all UNRDF tutorials! Here's what to explore next:

- **How-To Guides**: Learn specific patterns and techniques
- **API Reference**: Comprehensive API documentation
- **Explanation Guides**: Deep dives into architecture and concepts
- **Examples**: More complex real-world examples

## Complete Example

See the full working example at: `examples/define-hook-example.mjs`

## Reference

- [KnowledgeHookManager API](../reference/api/knowledge-engine.md#KnowledgeHookManager)
- [TransactionManager API](../reference/api/knowledge-engine.md#TransactionManager)
- [defineHook Reference](../reference/api/knowledge-engine.md#defineHook)
- [Policy Packs How-To](../how-to/hooks/policy-packs.md)
- [Audit Trails How-To](../how-to/transactions/audit-trails.md)
