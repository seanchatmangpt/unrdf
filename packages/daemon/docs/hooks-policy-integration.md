# Hooks Policy Integration Guide

## Overview

The Daemon Hooks Policy Integration provides a comprehensive framework for policy-driven operation control within the UnRDF daemon. This system enables organizations to enforce governance policies on operations before execution, maintaining complete audit trails, supporting policy versioning with rollback capabilities, and resolving conflicts between multiple policies.

### Key Features

- **Pre-execution Policy Evaluation**: Evaluate policies before executing daemon operations
- **Multiple Policy Types**: Approval, time-window, resource-limit, rate-limit, and custom policies
- **Conflict Resolution**: Resolve conflicts between policies using configurable strategies
- **Audit Trail**: Complete tracking of all policy decisions and lifecycle changes
- **Policy Versioning**: Automatic versioning with rollback support
- **Real-time Updates**: Update policies without restarting the daemon
- **Performance**: Sub-millisecond policy evaluation for low-latency execution

## Policy Framework Architecture

### Core Components

The policy integration consists of three main components:

1. **DaemonHookPolicyAdapter**: Main integration point between daemon, hooks scheduler, and policies
2. **Policy Registry**: Stores and manages policies with versioning
3. **Audit & Decision Logs**: Comprehensive tracking of all decisions

```
┌─────────────────────────────────────┐
│       UnrdfDaemon                   │
│  ┌────────────────────────────────┐ │
│  │  Operation Queue               │ │
│  │  - Scheduled operations        │ │
│  │  - Event-driven operations     │ │
│  └────────────────────────────────┘ │
└────────┬──────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────┐
│  DaemonHookPolicyAdapter                    │
│  ┌──────────────────────────────────────┐   │
│  │ Policy Evaluation Engine             │   │
│  │ - Evaluate all enabled policies      │   │
│  │ - Resolve conflicts                  │   │
│  │ - Record decisions                   │   │
│  └──────────────────────────────────────┘   │
└────────┬──────────────────────┬──────────────┘
         │                      │
         ▼                      ▼
┌──────────────────┐  ┌──────────────────────┐
│  Policy Registry │  │  HookScheduler       │
│  - Approval      │  │  - Cron triggers     │
│  - Time-window   │  │  - Interval triggers │
│  - Rate-limit    │  │  - Event triggers    │
│  - Resource-limit│  │  - Idle triggers     │
│  - Custom        │  │  - Startup triggers  │
└──────────────────┘  └──────────────────────┘
```

## Policy Types

### 1. Approval Policy

Requires explicit approval before operation execution.

**Configuration**:
```javascript
{
  id: 'admin-approval',
  name: 'Administrative Approval Required',
  type: 'approval',
  priority: 90,
  config: {
    requiresApproval: true,
    approvers: ['admin@example.com', 'ops@example.com']
  }
}
```

**Use Cases**:
- Sensitive operations (delete, truncate, migration)
- Data access requests
- Configuration changes
- Access to restricted resources

### 2. Time-Window Policy

Restricts operations to specific time windows.

**Configuration**:
```javascript
{
  id: 'business-hours',
  name: 'Business Hours Only',
  type: 'time-window',
  priority: 70,
  config: {
    timeWindows: [
      {
        start: 9,    // 9 AM
        end: 17,     // 5 PM
        days: [1, 2, 3, 4, 5]  // Mon-Fri
      },
      {
        start: 10,
        end: 14,
        days: [6]  // Saturday 10 AM - 2 PM
      }
    ]
  }
}
```

**Use Cases**:
- Maintenance windows
- Non-disruptive operation scheduling
- Customer-facing system protection
- Peak traffic avoidance

### 3. Rate-Limit Policy

Limits the number of operations per time period.

**Configuration**:
```javascript
{
  id: 'bulk-operation-limiter',
  name: 'Bulk Operation Rate Limit',
  type: 'rate-limit',
  priority: 60,
  config: {
    maxPerMinute: 10
  }
}
```

**Use Cases**:
- API rate limiting
- Bulk operation throttling
- Prevention of resource exhaustion
- Fair resource distribution

### 4. Resource-Limit Policy

Prevents concurrent operations from exceeding resource limits.

**Configuration**:
```javascript
{
  id: 'concurrent-limiter',
  name: 'Concurrent Operation Limit',
  type: 'resource-limit',
  priority: 80,
  config: {
    maxConcurrent: 5
  }
}
```

**Use Cases**:
- Memory constraint enforcement
- Database connection pooling
- Concurrent request limits
- CPU utilization control

### 5. Custom Policy

Implement custom evaluation logic.

**Configuration**:
```javascript
{
  id: 'custom-validation',
  name: 'Custom Business Logic',
  type: 'custom',
  priority: 85,
  config: {
    evaluatorFn: async (context) => {
      // Custom logic
      const isValid = validateOperationContext(context);
      return {
        decision: isValid ? 'allow' : 'deny',
        reason: isValid ? 'Validation passed' : 'Validation failed'
      };
    }
  }
}
```

**Use Cases**:
- Domain-specific validation
- Complex business rules
- Integration with external systems
- Conditional approval based on payload

## Getting Started

### Basic Setup

```javascript
import { Daemon } from '@unrdf/daemon';
import { createHookScheduler } from '@unrdf/hooks';
import { DaemonHookPolicyAdapter } from '@unrdf/daemon/integrations/hooks-policy';

// Initialize daemon and scheduler
const daemon = new Daemon({
  daemonId: '550e8400-e29b-41d4-a716-446655440000',
  name: 'policy-daemon',
  concurrency: 10
});

const scheduler = createHookScheduler();

// Create policy adapter
const adapter = new DaemonHookPolicyAdapter(daemon, scheduler, {
  adapterId: 'main-policy-adapter',
  conflictStrategy: 'highest-priority',
  auditEnabled: true
});

// Register policies
adapter.registerPolicy({
  id: 'approval-gate',
  name: 'Approval Gate',
  type: 'approval',
  priority: 90,
  config: { requiresApproval: true }
});

// Start services
await daemon.start();
scheduler.start();
```

### Registering a Policy

```javascript
const policy = adapter.registerPolicy({
  id: 'data-deletion-approval',
  name: 'Data Deletion Requires Approval',
  type: 'approval',
  priority: 95,
  description: 'All data deletion operations require approval from operations team',
  config: {
    requiresApproval: true,
    approvers: ['ops-lead@example.com', 'compliance@example.com']
  },
  metadata: {
    author: 'platform-team',
    enforceFrom: '2024-01-15',
    reviewCycle: 'quarterly'
  }
});

console.log(`Policy registered: v${policy.version}`);
```

### Evaluating Policies

```javascript
const context = {
  operationId: 'op-delete-users',
  operationType: 'bulk-delete',
  payload: {
    entityType: 'User',
    count: 1000,
    criteria: { status: 'inactive', daysInactive: 90 }
  },
  metadata: {
    requestedBy: 'data-team@example.com',
    reason: 'GDPR compliance'
  }
};

const decision = await adapter.evaluatePolicies('op-delete-users', context);

if (decision.decision === 'allow') {
  console.log('Operation approved:', decision.reason);
  // Proceed with execution
} else if (decision.decision === 'deny') {
  console.error('Operation denied:', decision.reason);
  // Handle denial
} else if (decision.decision === 'defer') {
  console.log('Operation deferred:', decision.reason);
  // Wait for async approval or retry later
}
```

### Executing with Policy Validation

```javascript
try {
  const result = await adapter.executeWithPolicy('op-id', context);

  if (result.success) {
    console.log('Operation completed:', result.result);
  } else {
    console.log('Operation failed:', result.reason);
  }
} catch (error) {
  if (error.code === 'POLICY_DENIED') {
    console.error('Policy violation:', error.message);
  } else {
    console.error('Execution error:', error.message);
  }
}
```

## Policy Versioning & Rollback

### Understanding Versions

Each policy update automatically increments the version number:

```javascript
// Create policy (v1)
adapter.registerPolicy({
  id: 'my-policy',
  name: 'My Policy',
  type: 'approval',
  config: { requiresApproval: true }
});

// Update policy (v2)
adapter.registerPolicy({
  id: 'my-policy',
  name: 'My Policy',
  type: 'approval',
  priority: 80,
  config: { requiresApproval: true, approvers: ['admin@example.com'] }
});

const policy = adapter.getPolicy('my-policy');
console.log(`Current version: ${policy.version}`);  // Output: 2
```

### Policy History

View all versions of a policy:

```javascript
const history = adapter.getPolicyHistory('my-policy');

history.forEach(entry => {
  console.log(`v${entry.version}: ${entry.timestamp}`);
  console.log(`Priority: ${entry.policy.priority}`);
});
```

### Rolling Back

Revert to a previous policy version:

```javascript
const rolledBack = adapter.rollbackPolicy('my-policy', 1);

console.log(`Rolled back to v1`);
console.log(`Current version: ${rolledBack.version}`);
```

## Conflict Resolution

When multiple policies are enabled, conflicts may occur. The adapter resolves them using configurable strategies.

### Resolution Strategies

#### 1. Highest-Priority (Default)

Policies are evaluated in priority order. First non-deferred decision wins.

```javascript
const adapter = new DaemonHookPolicyAdapter(daemon, scheduler, {
  conflictStrategy: 'highest-priority'
});
```

**Behavior**:
- Policies sorted by priority (descending)
- First policy with allow/deny decision wins
- Deferred policies are skipped

#### 2. Unanimous

All policies must agree, or operation is deferred.

```javascript
const adapter = new DaemonHookPolicyAdapter(daemon, scheduler, {
  conflictStrategy: 'unanimous'
});
```

**Behavior**:
- All policies must return allow
- Any deny immediately denies operation
- Any defer causes operation to defer

#### 3. Majority

Majority vote determines outcome.

```javascript
const adapter = new DaemonHookPolicyAdapter(daemon, scheduler, {
  conflictStrategy: 'majority'
});
```

**Behavior**:
- Allow if majority of policies allow
- Deny if majority of policies deny

#### 4. First-Match

First policy result is used.

```javascript
const adapter = new DaemonHookPolicyAdapter(daemon, scheduler, {
  conflictStrategy: 'first-match'
});
```

**Behavior**:
- Highest priority policy decision is final
- Other policies are not evaluated

## Audit Trail & Compliance

### Recording Policies Decisions

All policy decisions are automatically recorded:

```javascript
const decisions = adapter.getDecisionLog('operation-id');

decisions.forEach(decision => {
  console.log(`${decision.timestamp}: ${decision.decision}`);
  console.log(`Reason: ${decision.reason}`);
  console.log(`Policies: ${decision.policiesEvaluated.join(', ')}`);
});
```

### Audit Trail

View complete audit history for compliance:

```javascript
// Get audit trail for specific policy
const policyAudit = adapter.getAuditTrail('my-policy');

// Get all audits
const allAudit = adapter.getAuditTrail();

allAudit.forEach(entry => {
  console.log(`${entry.timestamp} - ${entry.action}`);
  console.log(`Policy: ${entry.policyId} v${entry.version}`);
  console.log(`Changes: ${JSON.stringify(entry.changes)}`);
});
```

### Compliance Export

Export audit logs for compliance reports:

```javascript
function exportComplianceReport(adapter) {
  const stats = adapter.getStats();
  const audit = adapter.getAuditTrail();
  const decisions = adapter.getDecisionLog();

  return {
    exportDate: new Date().toISOString(),
    adapter: {
      id: stats.adapterId,
      totalPolicies: stats.totalPolicies,
      enabledPolicies: stats.enabledPolicies
    },
    policies: adapter.listPolicies().map(p => ({
      id: p.id,
      name: p.name,
      type: p.type,
      version: p.version,
      enabled: p.enabled,
      priority: p.priority
    })),
    auditLog: audit,
    decisionLog: decisions
  };
}
```

## Best Practices

### 1. Policy Design

**✅ DO**:
- Use meaningful policy IDs (e.g., `data-deletion-approval`)
- Set priorities based on severity (0-100, higher = more important)
- Document policy purpose in name and description
- Use specific policy types for clear intent

**❌ DON'T**:
- Use generic names like `policy1`, `policy2`
- Create overlapping policies without clear conflict strategy
- Enable policies without understanding their impact
- Ignore audit trails and decision logs

### 2. Priority Assignment

```javascript
// Priority levels
const CRITICAL = 95;      // Security, compliance
const HIGH = 80;          // Data integrity, availability
const MEDIUM = 60;        // Performance, optimization
const LOW = 30;           // Informational, logging
```

### 3. Time-Window Policy Best Practices

```javascript
// Define clear business hours
{
  id: 'maintenance-window',
  type: 'time-window',
  config: {
    timeWindows: [
      {
        start: 2,      // 2 AM - 4 AM UTC
        end: 4,
        days: [0],     // Sunday only
        reason: 'Weekly maintenance'
      }
    ]
  }
}
```

### 4. Custom Policy Best Practices

```javascript
// Good: Clear logic with proper error handling
{
  id: 'custom-validator',
  type: 'custom',
  config: {
    evaluatorFn: async (context) => {
      try {
        const valid = await validatePayload(context.payload);
        return {
          decision: valid ? 'allow' : 'deny',
          reason: valid ? 'Payload valid' : 'Payload validation failed'
        };
      } catch (error) {
        return {
          decision: 'defer',
          reason: `Validation error: ${error.message}`
        };
      }
    }
  }
}

// Bad: Silent failures, unclear logic
{
  id: 'mysterious-policy',
  type: 'custom',
  config: {
    evaluatorFn: async (context) => {
      // What does this do?
      return context.payload?.id > 100 ? 'allow' : 'deny';
    }
  }
}
```

### 5. Monitoring & Alerts

```javascript
// Monitor policy decisions
adapter.on('policy:decision', (event) => {
  if (event.decision.decision === 'deny') {
    console.warn('Policy denial:', event.decision);
    // Send alert
  }
});

// Monitor policy changes
adapter.on('policy:audit', (event) => {
  if (['created', 'updated', 'deleted'].includes(event.audit.action)) {
    console.log('Policy changed:', event.audit);
    // Log for compliance
  }
});

// Monitor adapter health
setInterval(() => {
  const stats = adapter.getStats();
  console.log(`Active policies: ${stats.enabledPolicies}/${stats.totalPolicies}`);
  console.log(`Decisions logged: ${stats.decisionLogSize}`);
}, 60000);
```

## Advanced Scenarios

### Multi-Stage Approval

```javascript
// Stage 1: Manager approval
adapter.registerPolicy({
  id: 'manager-approval',
  type: 'approval',
  priority: 80,
  config: {
    requiresApproval: true,
    approvers: ['manager@example.com']
  }
});

// Stage 2: Security review
adapter.registerPolicy({
  id: 'security-review',
  type: 'approval',
  priority: 90,
  config: {
    requiresApproval: true,
    approvers: ['security@example.com']
  }
});

// Use unanimous strategy to require both approvals
adapter.conflictStrategy = 'unanimous';
```

### Conditional Policies

```javascript
adapter.registerPolicy({
  id: 'conditional-approval',
  type: 'custom',
  priority: 75,
  config: {
    evaluatorFn: async (context) => {
      // Only require approval for large operations
      const isLargeOperation = context.payload.count > 1000;

      if (isLargeOperation) {
        return {
          decision: 'defer',
          reason: 'Large operations require approval'
        };
      }

      return {
        decision: 'allow',
        reason: 'Operation size within limits'
      };
    }
  }
});
```

### Emergency Override

```javascript
// Create an override policy with maximum priority
const emergencyOverride = adapter.registerPolicy({
  id: 'emergency-override',
  type: 'custom',
  priority: 100,  // Highest priority
  config: {
    evaluatorFn: async (context) => {
      // In real implementation, check emergency flag
      if (context.metadata?.emergencyOverride) {
        return {
          decision: 'allow',
          reason: 'Emergency override activated'
        };
      }
      return {
        decision: 'allow',
        reason: 'No override'
      };
    }
  }
});

// Use with highest-priority conflict strategy
adapter.conflictStrategy = 'highest-priority';
```

## Performance Considerations

### Policy Evaluation Overhead

Policy evaluation adds minimal overhead:
- Single policy: <1ms
- 10 policies: <5ms
- 50 policies: <25ms

### Optimization Tips

1. **Priority Ordering**: Place most frequently matching policies first
2. **Policy Disabling**: Disable unused policies instead of deleting
3. **Audit Trimming**: Configure appropriate log retention
4. **Conflict Strategy**: Choose highest-priority for best performance

```javascript
// Optimize for performance
const adapter = new DaemonHookPolicyAdapter(daemon, scheduler, {
  conflictStrategy: 'highest-priority',  // Fastest resolution
  maxHistoryVersions: 5,                 // Smaller history
  auditEnabled: true                     // Keep audit trail
});
```

## Troubleshooting

### Policy Not Being Evaluated

**Check**:
```javascript
const policy = adapter.getPolicy('policy-id');
console.log(`Policy enabled: ${policy.enabled}`);
console.log(`Policy priority: ${policy.priority}`);

const allPolicies = adapter.listPolicies();
console.log(`Total policies: ${allPolicies.length}`);
```

### Unexpected Deny Decision

**Check**:
```javascript
const decisions = adapter.getDecisionLog('operation-id');
decisions.forEach(d => {
  console.log(`Decision: ${d.decision}`);
  console.log(`Reason: ${d.reason}`);
  console.log(`Policies: ${d.policiesEvaluated.join(', ')}`);
});
```

### Missing Audit Entries

**Check**:
```javascript
const auditTrail = adapter.getAuditTrail('policy-id');
console.log(`Audit entries: ${auditTrail.length}`);
console.log(`Audit enabled: ${adapter.auditEnabled}`);
```

## API Reference

See the integration source code for complete API documentation:
- `DaemonHookPolicyAdapter` - Main adapter class
- `registerPolicy(config)` - Register new policy
- `evaluatePolicies(operationId, context)` - Evaluate policies
- `executeWithPolicy(operationId, context)` - Execute with validation
- `getPolicy(policyId)` - Get single policy
- `listPolicies()` - List all policies
- `rollbackPolicy(policyId, version)` - Rollback to version
- `getAuditTrail(policyId)` - Get audit entries
- `getDecisionLog(operationId)` - Get decisions

## Summary

The Hooks Policy Integration provides a robust framework for policy-driven operation control in UnRDF daemon environments. By combining multiple policy types, versioning, audit trails, and conflict resolution, organizations can enforce governance requirements while maintaining complete compliance records and operational flexibility.

For questions or issues, refer to the test suite and code examples in the integration module.
