# Getting Started with @unrdf/engine-gateway

**Time estimate:** 30-45 minutes
**Difficulty:** Beginner
**What you'll learn:** Basic usage of @unrdf/engine-gateway

---

## What You'll Do

In this tutorial, you'll:
1. Install @unrdf/engine-gateway
2. query federation across multiple rdf engines
3. Run your first working example
4. Understand the core concepts

---

## Installation

```bash
npm install @unrdf/engine-gateway
```

---

## Your First Example

```javascript
import { createGateway } from '@unrdf/engine-gateway';

// Initialize
const instance = await createGateway();

console.log('Success! @unrdf/engine-gateway is ready.');
```

---

## Core Concepts

Understanding these foundational ideas will help you use @unrdf/engine-gateway effectively:

- **createGateway**: Main entry point for the library
- **Configuration**: Customizing behavior for your use case
- **Data flow**: How information moves through the system
- **Error handling**: Managing failures gracefully

---

## Next Steps

You now have @unrdf/engine-gateway installed and working. Ready to learn the workflow?

- Read [02-basic-workflow.md](02-basic-workflow.md) to understand common patterns
- Check [../how-to/](../how-to/) for specific problem solutions
- Explore [../reference/01-api.md](../reference/01-api.md) for complete API details

---

## Summary

You've learned:
- ✅ How to install @unrdf/engine-gateway
- ✅ Basic initialization
- ✅ Your first working example
- ✅ Where to go next
