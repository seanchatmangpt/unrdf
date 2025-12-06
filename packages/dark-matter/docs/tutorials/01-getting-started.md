# Getting Started with @unrdf/dark-matter

**Time estimate:** 30-45 minutes
**Difficulty:** Beginner
**What you'll learn:** Basic usage of @unrdf/dark-matter

---

## What You'll Do

In this tutorial, you'll:
1. Install @unrdf/dark-matter
2. automatic rdf query optimization and caching
3. Run your first working example
4. Understand the core concepts

---

## Installation

```bash
npm install @unrdf/dark-matter
```

---

## Your First Example

```javascript
import { optimizeQuery } from '@unrdf/dark-matter';

// Initialize
const instance = await optimizeQuery();

console.log('Success! @unrdf/dark-matter is ready.');
```

---

## Core Concepts

Understanding these foundational ideas will help you use @unrdf/dark-matter effectively:

- **optimizeQuery**: Main entry point for the library
- **Configuration**: Customizing behavior for your use case
- **Data flow**: How information moves through the system
- **Error handling**: Managing failures gracefully

---

## Next Steps

You now have @unrdf/dark-matter installed and working. Ready to learn the workflow?

- Read [02-basic-workflow.md](02-basic-workflow.md) to understand common patterns
- Check [../how-to/](../how-to/) for specific problem solutions
- Explore [../reference/01-api.md](../reference/01-api.md) for complete API details

---

## Summary

You've learned:
- ✅ How to install @unrdf/dark-matter
- ✅ Basic initialization
- ✅ Your first working example
- ✅ Where to go next
