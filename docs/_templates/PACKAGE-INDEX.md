# Package Documentation Hub

Template for `packages/{name}/docs/INDEX.md`

---

# @unrdf/{package-name}

[1-2 sentence description of what this package does]

**Latest version:** [version]
**Status:** [✅ Production / 🔶 Alpha / 🔒 Private]

---

## 📚 Documentation

Choose your path based on what you need:

### 👶 New to {package}?
Start with **[Tutorials](./TUTORIALS/)** - Learn by doing
- [Getting Started](./TUTORIALS/01-getting-started.md) - Your first experience (10 min)
- [Basic Workflow](./TUTORIALS/02-basic-workflow.md) - Common patterns (15 min)
- [Advanced Patterns](./TUTORIALS/03-advanced-patterns.md) - Next level (20 min)

**Perfect for:** First-time users, learning by doing

### 🔧 Building something specific?
Use **[How-To Guides](./HOW-TO/)** - Problem solving
- [Common Task 1](./HOW-TO/common-task-1.md)
- [Common Task 2](./HOW-TO/common-task-2.md)
- [Troubleshooting](./HOW-TO/troubleshooting.md)
- [Performance Tuning](./HOW-TO/performance-tuning.md)

**Perfect for:** Solving a specific problem you have right now

### 📖 Looking up details?
Check **[Reference](./REFERENCE/)** - Complete information
- [API Reference](./REFERENCE/API.md) - All functions & options
- [Type Reference](./REFERENCE/TYPES.md) - Data types & schemas
- [Configuration](./REFERENCE/CONFIGURATION.md) - All config options
- [Error Reference](./REFERENCE/ERRORS.md) - Error codes & solutions

**Perfect for:** Finding exact details, copy-pasting code, IDE reference

### 💡 Want to understand how it works?
Read **[Explanation](./EXPLANATION/)** - Conceptual knowledge
- [Architecture](./EXPLANATION/architecture.md) - How it's organized
- [Design Decisions](./EXPLANATION/design-decisions.md) - Why this design?
- [Key Concepts](./EXPLANATION/concepts.md) - Foundational ideas
- [Performance](./EXPLANATION/performance.md) - Performance details

**Perfect for:** Understanding concepts, design decisions, contributing

---

## Quick Links

| I want to... | Read this | Time |
|---|---|---|
| Get started in 10 minutes | [Getting Started Tutorial](./TUTORIALS/01-getting-started.md) | 10 min |
| Learn the common workflow | [Basic Workflow Tutorial](./TUTORIALS/02-basic-workflow.md) | 15 min |
| Solve a specific problem | [How-To Guides](./HOW-TO/) | 5-10 min |
| Look up a function | [API Reference](./REFERENCE/API.md) | 2 min |
| Understand the design | [Architecture](./EXPLANATION/architecture.md) | 15 min |
| Debug an error | [Error Reference](./REFERENCE/ERRORS.md) | 5 min |
| Optimize performance | [How-To: Performance](./HOW-TO/performance-tuning.md) | 10 min |

---

## Examples

Live code examples (if available):

```bash
# Run the getting started example
npm run example:getting-started

# Run advanced patterns
npm run example:advanced
```

See [examples/](../../examples/) for more complete projects.

---

## Key Concepts at a Glance

### [Concept 1]
[1-2 sentence explanation]
👉 Learn more: [Explanation: Concept 1](./EXPLANATION/concept-1.md)

### [Concept 2]
[1-2 sentence explanation]
👉 Learn more: [Explanation: Concept 2](./EXPLANATION/concept-2.md)

---

## Dependencies

This package depends on:
- **@unrdf/core** (required)
- **@unrdf/[other]** (optional, for feature X)

See [dependency graph](../../docs/PACKAGE-GUIDES/dependency-graph.md) for all package relationships.

---

## Installation

### npm
```bash
npm install @unrdf/{package-name}
```

### pnpm (recommended)
```bash
pnpm add @unrdf/{package-name}
```

**Requirements:**
- Node.js 18+
- @unrdf/core

For detailed installation, see [Getting Started](./TUTORIALS/01-getting-started.md).

---

## API Quick Reference

### Main Function

```javascript
import { mainFunction } from '@unrdf/{package-name}';

const result = mainFunction(options);
```

📖 Full API reference: [API.md](./REFERENCE/API.md)

---

## Contributing

Want to improve this package? See [CONTRIBUTING.md](./CONTRIBUTING.md)

---

## Roadmap

- [ ] Feature 1 (Planned)
- [ ] Feature 2 (In Progress)
- [x] Feature 3 (Complete)

---

## Getting Help

- **📚 Documentation:** You're reading it!
- **❓ FAQ:** See [Troubleshooting](./HOW-TO/troubleshooting.md)
- **🐛 Report a bug:** [GitHub Issues](https://github.com/unrdf/unrdf/issues)
- **💬 Ask a question:** [GitHub Discussions](https://github.com/unrdf/unrdf/discussions)
- **🔗 Related:** [@unrdf/core](../core/docs/), [@unrdf/hooks](../hooks/docs/)

---

## Next Steps

- 👶 **First time?** → [Getting Started Tutorial](./TUTORIALS/01-getting-started.md)
- 🏗️ **Ready to build?** → [How-To Guides](./HOW-TO/)
- 🧠 **Curious?** → [Explanation: Architecture](./EXPLANATION/architecture.md)
- 🤝 **Contributing?** → [Contributing Guide](./CONTRIBUTING.md)

---

**Last updated:** [date]
**Package version:** [version]
**Maintained by:** [name/team]
