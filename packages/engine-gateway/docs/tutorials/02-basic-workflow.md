# Basic Workflow: engine-gateway

**Time estimate:** 1-2 hours
**Difficulty:** Intermediate
**Prerequisites:** Complete Getting Started tutorial

---

## Overview

This tutorial teaches the standard workflow for developing with @unrdf/engine-gateway. You'll learn patterns used in real-world applications.

---

## Core Workflow

The standard workflow has these phases:

### Phase 1: Initialize
Set up your instance with proper configuration.

### Phase 2: Configure
Adjust settings for your specific use case.

### Phase 3: Execute
Perform your core operations.

### Phase 4: Monitor
Track progress and handle events.

### Phase 5: Cleanup
Clean up resources when done.

---

## Common Use Cases

### Use Case 1: EngineFailure

EngineFailure is a frequent requirement. Learn how to handle it effectively.

### Use Case 2: SchemaAlignment

SchemaAlignment is a frequent requirement. Learn how to handle it effectively.

### Use Case 3: Latency

Latency is a frequent requirement. Learn how to handle it effectively.

### Use Case 4: ResultMerging

ResultMerging is a frequent requirement. Learn how to handle it effectively.

---

## Practical Workflow Example

```javascript
import { createGateway } from '@unrdf/engine-gateway';

// Phase 1: Initialize
const instance = await createGateway();

// Phase 2: Configure
instance.configure({
  engines: true,
  timeout: 1000
});

// Phase 3: Execute operations
const result = await instance.addEngine();

// Phase 4: Monitor
console.log('Operation complete:', result);

// Phase 5: Cleanup
await instance.close();
```

---

## Error Handling

Always implement error handling:

```javascript
try {
  const result = await instance.createGateway();
} catch (error) {
  console.error('Error:', error.message);
  // Implement recovery strategy
}
```

---

## Performance Tips

- Batch operations when possible
- Use appropriate caching strategies
- Monitor system resources
- Profile slow operations
- Configure appropriately for your use case

---

## Summary

You've learned:
- ✅ The standard workflow pattern
- ✅ How to handle common use cases
- ✅ Error handling best practices
- ✅ Performance optimization tips

Next, explore advanced patterns for production systems.
