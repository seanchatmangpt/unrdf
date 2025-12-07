# AtomVM Browser Runtime Documentation

**Complete Diataxis-structured documentation for the AtomVM browser runtime package.**

This package enables running AtomVM (Erlang/BEAM VM) in the browser using WebAssembly and Service Workers for SharedArrayBuffer support.

---

## Quick Navigation

### 🎓 I want to learn AtomVM in the browser
Start with tutorials:
1. [Getting Started with AtomVM Browser](./tutorials/01-getting-started.md) - 15 min
2. [Running Your First BEAM Code](./tutorials/02-first-beam-code.md) - 20 min

**Time:** ~35 minutes total

---

### 🔧 I want to solve a specific problem
Browse how-to guides:
- [Enable SharedArrayBuffer Support](./how-to/enable-sharedarraybuffer.md) - Fix COI issues
- [Integrate Real AtomVM WASM](./how-to/integrate-wasm.md) - Connect actual WASM binary
- [Debug Service Worker Issues](./how-to/debug-service-worker.md) - Troubleshoot registration
- [Build for Production](./how-to/build-production.md) - Create production bundle

**Time:** 5-15 minutes per guide

---

### 📖 I need API documentation
Check references:
- [API Reference](./reference/api.md) - Complete API documentation
- [Service Worker Manager](./reference/service-worker-manager.md) - COI registration API
- [AtomVM Runtime](./reference/atomvm-runtime.md) - Runtime class API
- [Terminal UI](./reference/terminal-ui.md) - UI component API

**Format:** Complete API signatures, parameters, examples

---

### 💡 I want to understand the design
Read explanations:
- [Architecture Overview](./explanation/architecture.md) - System design and principles
- [Cross-Origin-Isolation](./explanation/cross-origin-isolation.md) - Why COI is needed
- [Service Worker Strategy](./explanation/service-worker-strategy.md) - How coi-serviceworker works
- [Coding Patterns](./coding-patterns.md) - Code standards and improvement patterns

**Purpose:** Concepts, rationale, tradeoffs

---

## Documentation Structure

```
docs/
├── README.md                    # This file (navigation hub)
├── tutorials/                   # Learning-oriented guides
│   ├── 01-getting-started.md
│   └── 02-first-beam-code.md
├── how-to/                      # Task-oriented guides
│   ├── enable-sharedarraybuffer.md
│   ├── integrate-wasm.md
│   ├── debug-service-worker.md
│   └── build-production.md
├── reference/                   # Information-oriented docs
│   ├── api.md
│   ├── service-worker-manager.md
│   ├── atomvm-runtime.md
│   └── terminal-ui.md
└── explanation/                 # Understanding-oriented docs
    ├── architecture.md
    ├── cross-origin-isolation.md
    └── service-worker-strategy.md
```

---

## Package Overview

**@unrdf/atomvm** provides a browser-based runtime for AtomVM, enabling Erlang/BEAM bytecode execution in web browsers through WebAssembly.

### Key Features

- ✅ **Cross-Origin-Isolation**: Automatic COI setup via service workers
- ✅ **SharedArrayBuffer Support**: Enables multi-threaded WASM execution
- ✅ **Service Worker Architecture**: Uses `coi-serviceworker` for header injection
- ✅ **Modern UI**: Terminal interface for BEAM code execution
- ✅ **Production Ready**: Verified build and test suite

### Browser Compatibility

- Chrome/Edge: 92+ ✅
- Firefox: 95+ ✅
- Safari: 15.2+ ✅

---

## Getting Help

- **Issues**: Check [troubleshooting section](../README.md#troubleshooting) in main README
- **API Questions**: See [Reference Documentation](./reference/)
- **Concept Questions**: See [Explanation Documentation](./explanation/)

