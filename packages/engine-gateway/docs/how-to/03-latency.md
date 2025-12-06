# How To: Solve Latency

**Time estimate:** 1 hour
**Difficulty:** Advanced
**Context:** When you encounter Latency

---

## Problem Statement

Latency is a common challenge when using @unrdf/engine-gateway. This guide shows you exactly how to diagnose and fix it.

---

## Symptoms

You might be experiencing Latency if:
- Errors mentioning Latency appear in logs
- Operations fail unexpectedly
- Performance degrades unexpectedly
- Resources become exhausted

---

## Root Cause Analysis

Latency typically occurs when:
- Configuration isn't optimized for your use case
- System resources reach their limits
- Concurrent operations aren't properly managed
- Data size exceeds expected bounds

---

## Solution (Step by Step)

### Step 1: Diagnosis

Enable detailed logging to identify the issue:

```javascript
instance.enableDiagnostics();
instance.setLogLevel('debug');

try {
  const result = await instance.createGateway();
} catch (error) {
  console.error('Diagnostic info:', error.diagnostics);
}
```

### Step 2: Root Cause Identification

Review logs and metrics to understand what's happening:

```javascript
const health = await instance.getHealth();
console.log('System status:', health);
```

### Step 3: Apply the Fix

Implement the appropriate solution:

```javascript
// Fix for Latency
const instance = await createGateway({
  engines: true,
  timeout: 1000,
  retries: 3
});
```

### Step 4: Verification

Verify the fix works:

```javascript
const result = await instance.createGateway();
console.log('Success! Latency resolved.');
```

---

## Real-World Example

Here's a complete example addressing Latency in production:

```javascript
import { createGateway } from '@unrdf/engine-gateway';

async function handleProduction() {
  // Initialize with production config
  const instance = await createGateway({
    engines: true,
    timeout: 5000,
    timeout: 30000
  });

  // Monitor system health
  setInterval(async () => {
    const health = await instance.getHealth();
    if (!health.ok) {
      console.error('Health check failed:', health);
    }
  }, 60000);

  return instance;
}
```

---

## Prevention Strategies

To avoid Latency in the future:

1. **Use appropriate configuration** - Adjust settings based on your workload
2. **Implement monitoring** - Track system health continuously
3. **Load test** - Test with realistic data volumes
4. **Plan capacity** - Provision adequate resources
5. **Document decisions** - Record why you chose specific settings

---

## Additional Resources

- See [../reference/03-configuration.md](../reference/03-configuration.md) for all configuration options
- Check [../reference/04-errors.md](../reference/04-errors.md) for error reference
- Read [../explanation/](../explanation/) for deeper understanding

---

## Summary

You now know how to:
- ✅ Diagnose Latency
- ✅ Identify root causes
- ✅ Implement fixes
- ✅ Prevent future occurrences
- ✅ Monitor for recurrence
