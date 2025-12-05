# Getting Started with @unrdf/composables

**Time estimate:** 30-45 minutes
**Difficulty:** Beginner
**What you'll learn:** Basic usage of @unrdf/composables

---

## What You'll Do

In this tutorial, you'll:
1. Install @unrdf/composables
2. vue 3 composables for reactive rdf data binding
3. Run your first working example
4. Understand the core concepts

---

## Installation

```bash
npm install @unrdf/composables
```

---

## Your First Example

```javascript
import { useRdfStore } from '@unrdf/composables';

// Initialize
const instance = await useRdfStore();

console.log('Success! @unrdf/composables is ready.');
```

---

## Core Concepts

Understanding these foundational ideas will help you use @unrdf/composables effectively:

- **useRdfStore**: Main entry point for the library
- **Configuration**: Customizing behavior for your use case
- **Data flow**: How information moves through the system
- **Error handling**: Managing failures gracefully

---

## Next Steps

You now have @unrdf/composables installed and working. Ready to learn the workflow?

- Read [02-basic-workflow.md](02-basic-workflow.md) to understand common patterns
- Check [../how-to/](../how-to/) for specific problem solutions
- Explore [../reference/01-api.md](../reference/01-api.md) for complete API details

---

## Summary

You've learned:
- ✅ How to install @unrdf/composables
- ✅ Basic initialization
- ✅ Your first working example
- ✅ Where to go next
