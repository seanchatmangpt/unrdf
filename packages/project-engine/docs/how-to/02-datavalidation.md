# How To: Solve DataValidation

**Time estimate:** 30-45 minutes
**Difficulty:** Intermediate
**Context:** When you encounter DataValidation

---

## Problem Statement

DataValidation is a common challenge when using @unrdf/project-engine. This guide shows you exactly how to diagnose and fix it.

---

## Symptoms

You might be experiencing DataValidation if:
- Errors mentioning DataValidation appear in logs
- Operations fail unexpectedly
- Performance degrades unexpectedly
- Resources become exhausted

---

## Root Cause Analysis

DataValidation typically occurs when:
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
  const result = await instance.createProject();
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
// Fix for DataValidation
const instance = await createProject({
  storageBackend: true,
  persistenceLevel: 1000,
  retries: 3
});
```

### Step 4: Verification

Verify the fix works:

```javascript
const result = await instance.createProject();
console.log('Success! DataValidation resolved.');
```

---

## Real-World Example

Here's a complete example addressing DataValidation in production:

```javascript
import { createProject } from '@unrdf/project-engine';

async function handleProduction() {
  // Initialize with production config
  const instance = await createProject({
    storageBackend: true,
    persistenceLevel: 5000,
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

To avoid DataValidation in the future:

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
- ✅ Diagnose DataValidation
- ✅ Identify root causes
- ✅ Implement fixes
- ✅ Prevent future occurrences
- ✅ Monitor for recurrence
