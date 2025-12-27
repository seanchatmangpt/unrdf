# How to Configure Autonomy Guards

Autonomy guards enforce bounded autonomy through explicit budget/capacity limits per epoch, preventing unbounded autonomous operation.

## Problem

You need to:

- Limit Claude's resource consumption per time window
- Prevent excessive file modifications
- Cap tool operation counts
- Create denial receipts for rejected operations
- Manage budget across multiple epochs

## Solution

Use autonomy guards with configurable budgets and hard enforcement.

## Step-by-Step

### 1. Create a Basic Guard

```javascript
import { createAutonomyGuard } from '@unrdf/kgc-claude';

const guard = createAutonomyGuard({
  maxDeltaSize: 100, // Max 100 delta operations
  maxToolOps: 50, // Max 50 tool calls
  maxFilesTouched: 20, // Max 20 unique files
  maxRewriteCost: 1000, // Max 1000 abstract cost units
  epochDuration: 3600000000000n, // 1 hour in nanoseconds
});

console.log('Guard created with budget:', guard.getBudget());
```

### 2. Check Operation Allowance

```javascript
// Check if operation is within budget
const check = await guard.check({
  deltaSize: 5,
  toolOps: 2,
  files: ['src/index.mjs', 'src/utils.mjs'],
  rewriteCost: 50,
});

if (check.allowed) {
  console.log('Operation allowed');
  // Proceed with operation
} else {
  console.error('Operation denied');
  console.error('Reason:', check.denialReceipt.reason);
  console.error('Receipt hash:', check.denialReceipt.receiptHash);
}
```

### 3. Consume Budget After Execution

```javascript
if (check.allowed) {
  // Execute operation
  // ... do work ...

  // Consume budget
  guard.consume({
    deltaSize: 5,
    toolOps: 2,
    files: ['src/index.mjs', 'src/utils.mjs'],
    rewriteCost: 50,
  });

  console.log('Budget consumed');
  console.log('Remaining:', guard.getRemaining());
}
```

### 4. Use Production Guard Preset

```javascript
import { createProductionGuard } from '@unrdf/kgc-claude';

// Pre-configured for production use
const prodGuard = createProductionGuard();
// maxDeltaSize: 500
// maxToolOps: 100
// maxFilesTouched: 50
// maxRewriteCost: 10000
// epochDuration: 1 hour

console.log('Production guard budget:', prodGuard.getBudget());
```

### 5. Use Strict Guard for High-Risk Operations

```javascript
import { createStrictGuard } from '@unrdf/kgc-claude';

// Tight limits for critical operations
const strictGuard = createStrictGuard();
// maxDeltaSize: 50
// maxToolOps: 10
// maxFilesTouched: 5
// maxRewriteCost: 500
// epochDuration: 30 minutes

const check = await strictGuard.check({
  deltaSize: 3,
  toolOps: 1,
  files: ['package.json'],
});
```

### 6. Execute with Guard Protection

```javascript
import { withGuard } from '@unrdf/kgc-claude';

const result = await withGuard(
  guard,
  { deltaSize: 10, toolOps: 5, files: ['config.mjs'] },
  async () => {
    // Operation executed only if guard allows
    const run = createRunCapsule();
    run.addToolCall({ name: 'Edit', input: { file: 'config.mjs' } });

    const capsule = await run.seal();
    await persistRunCapsule(store, capsule);

    return { success: true };
  }
);

if (result.success) {
  console.log('Operation succeeded:', result.result);
} else {
  console.error('Guard denied:', result.denialReceipt.reason);
}
```

### 7. Monitor Usage and Remaining Capacity

```javascript
// Get current usage
const usage = guard.getUsage();
console.log('Current usage:', usage);
// { deltaSize: 25, toolOps: 10, filesTouched: 5, rewriteCost: 200, epochStart: 123456789n }

// Get remaining capacity
const remaining = guard.getRemaining();
console.log('Remaining capacity:', remaining);
// { deltaSize: 75, toolOps: 40, filesTouched: 15, rewriteCost: 800 }

// Calculate utilization
const utilization = {
  deltaSize: (usage.deltaSize / guard.getBudget().maxDeltaSize) * 100,
  toolOps: (usage.toolOps / guard.getBudget().maxToolOps) * 100,
  filesTouched: (usage.filesTouched / guard.getBudget().maxFilesTouched) * 100,
};
console.log('Utilization:', utilization);
```

### 8. Review Denial History

```javascript
const denials = guard.getDenialHistory();

console.log(`Total denials: ${denials.length}`);

denials.forEach((denial, index) => {
  console.log(`\nDenial ${index + 1}:`);
  console.log('  Time:', denial.timestamp_iso);
  console.log('  Reason:', denial.reason);
  console.log('  Requested:', denial.requested);
  console.log('  Budget at time:', denial.budget);
  console.log('  Usage at time:', denial.usage);
  console.log('  Receipt hash:', denial.receiptHash);
});
```

## Advanced Patterns

### Dynamic Budget Adjustment

```javascript
class AdaptiveGuard {
  constructor(baseGuard) {
    this.baseGuard = baseGuard;
    this.denialRate = 0;
  }

  async check(request) {
    const result = await this.baseGuard.check(request);

    // Track denial rate
    if (!result.allowed) {
      this.denialRate = this.denialRate * 0.9 + 0.1; // Exponential moving average
    } else {
      this.denialRate = this.denialRate * 0.95;
    }

    // If denial rate high, suggest budget increase
    if (this.denialRate > 0.3) {
      console.warn('High denial rate:', this.denialRate);
      console.warn('Consider increasing budget');
    }

    return result;
  }

  consume(consumption) {
    this.baseGuard.consume(consumption);
  }
}

const adaptiveGuard = new AdaptiveGuard(createAutonomyGuard());
```

### Scoped Guards for Sub-Operations

```javascript
// Main guard for entire operation
const mainGuard = createAutonomyGuard({
  maxDeltaSize: 100,
  maxToolOps: 50,
  maxFilesTouched: 20,
});

// Create scoped guard with fraction of budget
const scopedGuard = mainGuard.createScope({
  maxDeltaSize: 10, // 10% of parent
  maxToolOps: 5,
  maxFilesTouched: 2,
});

// Use scoped guard for risky sub-operation
const check = await scopedGuard.check({
  deltaSize: 5,
  toolOps: 2,
  files: ['risky.mjs'],
});

if (check.allowed) {
  // Execute with limited budget
  scopedGuard.consume({ deltaSize: 5, toolOps: 2, files: ['risky.mjs'] });

  // Main guard is not affected unless scoped guard succeeds
  mainGuard.consume({ deltaSize: 5, toolOps: 2, files: ['risky.mjs'] });
}
```

### Circuit Breaker Pattern

```javascript
class CircuitBreakerGuard {
  constructor(guard, threshold = 5, resetTime = 60000) {
    this.guard = guard;
    this.threshold = threshold;
    this.resetTime = resetTime;
    this.denialCount = 0;
    this.state = 'closed'; // closed, open, half-open
    this.lastDenialTime = 0;
  }

  async check(request) {
    // If circuit open, always deny
    if (this.state === 'open') {
      const timeSinceDenial = Date.now() - this.lastDenialTime;

      if (timeSinceDenial > this.resetTime) {
        this.state = 'half-open';
        console.log('Circuit breaker: half-open (testing)');
      } else {
        return {
          allowed: false,
          denialReceipt: {
            reason: 'circuit_breaker_open',
            threshold: this.threshold,
            denialCount: this.denialCount,
          },
        };
      }
    }

    // Check underlying guard
    const result = await this.guard.check(request);

    if (!result.allowed) {
      this.denialCount++;
      this.lastDenialTime = Date.now();

      if (this.denialCount >= this.threshold) {
        this.state = 'open';
        console.error('Circuit breaker: OPEN (too many denials)');
      }
    } else if (this.state === 'half-open') {
      // Success in half-open, reset
      this.state = 'closed';
      this.denialCount = 0;
      console.log('Circuit breaker: closed (recovered)');
    }

    return result;
  }
}

const cbGuard = new CircuitBreakerGuard(createProductionGuard());
```

### Per-Agent Guards in Multi-Agent Systems

```javascript
class MultiAgentGuardManager {
  constructor(totalBudget) {
    this.totalBudget = totalBudget;
    this.agentGuards = new Map();
  }

  createAgentGuard(agentId, share = 0.1) {
    const agentBudget = {
      maxDeltaSize: Math.floor(this.totalBudget.maxDeltaSize * share),
      maxToolOps: Math.floor(this.totalBudget.maxToolOps * share),
      maxFilesTouched: Math.floor(this.totalBudget.maxFilesTouched * share),
      maxRewriteCost: Math.floor(this.totalBudget.maxRewriteCost * share),
    };

    const guard = createAutonomyGuard(agentBudget);
    this.agentGuards.set(agentId, guard);

    return guard;
  }

  getGuard(agentId) {
    return this.agentGuards.get(agentId);
  }

  getTotalUsage() {
    let total = { deltaSize: 0, toolOps: 0, filesTouched: 0, rewriteCost: 0 };

    for (const guard of this.agentGuards.values()) {
      const usage = guard.getUsage();
      total.deltaSize += usage.deltaSize;
      total.toolOps += usage.toolOps;
      total.filesTouched += usage.filesTouched;
      total.rewriteCost += usage.rewriteCost;
    }

    return total;
  }
}

const manager = new MultiAgentGuardManager({
  maxDeltaSize: 1000,
  maxToolOps: 500,
  maxFilesTouched: 100,
  maxRewriteCost: 10000,
});

const agent1Guard = manager.createAgentGuard('agent-1', 0.3); // 30% share
const agent2Guard = manager.createAgentGuard('agent-2', 0.3); // 30% share
const agent3Guard = manager.createAgentGuard('agent-3', 0.4); // 40% share
```

### Epoch Reset Notifications

```javascript
class GuardWithNotifications {
  constructor(guard, onEpochReset) {
    this.guard = guard;
    this.onEpochReset = onEpochReset;
    this.lastEpochStart = guard.getUsage().epochStart;
  }

  async check(request) {
    const currentEpochStart = this.guard.getUsage().epochStart;

    // Detect epoch reset
    if (currentEpochStart !== this.lastEpochStart) {
      console.log('Epoch reset detected');
      this.onEpochReset({
        oldEpoch: this.lastEpochStart,
        newEpoch: currentEpochStart,
      });
      this.lastEpochStart = currentEpochStart;
    }

    return this.guard.check(request);
  }

  consume(consumption) {
    this.guard.consume(consumption);
  }
}

const notifyingGuard = new GuardWithNotifications(createAutonomyGuard(), epochInfo => {
  console.log('Budget refreshed!');
  console.log('Previous epoch:', epochInfo.oldEpoch);
  console.log('New epoch:', epochInfo.newEpoch);
});
```

## Best Practices

1. **Always check before consuming**: Never consume without checking first
2. **Use withGuard for automatic handling**: Simplifies guard integration
3. **Monitor denial rates**: High rates indicate budget too low
4. **Review denial receipts**: Understand why operations are rejected
5. **Reset epochs manually for testing**: Use `resetEpoch()` to simulate
6. **Scope guards for sub-operations**: Limit blast radius
7. **Persist denial history**: Keep audit trail of rejections

## Common Issues

**Issue**: Operations always denied despite budget

- **Cause**: Epoch already exhausted from previous operations
- **Fix**: Check current usage, wait for epoch reset, or increase limits

**Issue**: File counting incorrect

- **Cause**: Same file counted multiple times
- **Fix**: Guards deduplicate by path, ensure consistent paths

**Issue**: Budget never resets

- **Cause**: Epoch duration too long
- **Fix**: Reduce `epochDuration` or manually call `resetEpoch()`

**Issue**: Denial receipts missing details

- **Cause**: Request object incomplete
- **Fix**: Always pass all relevant fields to `check()`

## See Also

- [API Reference: Autonomy Guard](../reference.md#autonomy-guard)
- [Explanation: Why Bounded Autonomy](../explanation.md#bounded-autonomy)
- [Tutorial: Step 8](../tutorial.md#step-8-configure-custom-autonomy-guard)
