# Getting Started with @unrdf/project-engine

**Time estimate:** 30-45 minutes
**Difficulty:** Beginner
**What you'll learn:** Basic usage of @unrdf/project-engine

---

## What You'll Do

In this tutorial, you'll:
1. Install @unrdf/project-engine
2. project management for rdf-based systems with workflows
3. Run your first working example
4. Understand the core concepts

---

## Installation

```bash
npm install @unrdf/project-engine
```

---

## Your First Example

```javascript
import { createProject } from '@unrdf/project-engine';

// Initialize
const instance = await createProject();

console.log('Success! @unrdf/project-engine is ready.');
```

---

## Core Concepts

Understanding these foundational ideas will help you use @unrdf/project-engine effectively:

- **createProject**: Main entry point for the library
- **Configuration**: Customizing behavior for your use case
- **Data flow**: How information moves through the system
- **Error handling**: Managing failures gracefully

---

## Next Steps

You now have @unrdf/project-engine installed and working. Ready to learn the workflow?

- Read [02-basic-workflow.md](02-basic-workflow.md) to understand common patterns
- Check [../how-to/](../how-to/) for specific problem solutions
- Explore [../reference/01-api.md](../reference/01-api.md) for complete API details

---

## Summary

You've learned:
- ✅ How to install @unrdf/project-engine
- ✅ Basic initialization
- ✅ Your first working example
- ✅ Where to go next
