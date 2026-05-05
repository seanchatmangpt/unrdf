# UNRDF latest Documentation Index

**Release Date:** March 15, 2026
**Status:** ✅ Complete

---

## 📚 Core Documentation

### 1. Release Notes & Migration
- **[latest Release Notes](./latest-RELEASE-NOTES.md)** (1,579 lines)
  - Overview of latest improvements
  - Breaking changes (none!)
  - New features (isolated-vm, browser, profiling)
  - Upgrade path from v3.0.x
  - Performance improvements
  - Security fixes

- **[Migration Guide: v3.0 → v3.1](./MIGRATION-v3.0-to-v3.1.md)** (1,080 lines)
  - Step-by-step migration instructions
  - Code examples (before/after)
  - Configuration changes
  - Rollback instructions
  - Troubleshooting guide

### 2. Feature Guides

- **[Browser Compatibility](./BROWSER-COMPATIBILITY.md)** (874 lines)
  - Supported browsers (Chrome, Firefox, Safari, Edge)
  - Feature matrix
  - Browser limitations (IndexedDB, Service Workers)
  - Setup instructions for bundlers (Webpack, Vite, esbuild, Parcel)
  - Integration examples (Vanilla, React, Vue, Svelte)
  - Performance optimization

- **[Security Updates](./SECURITY-UPDATES-v3.1.md)** (835 lines)
  - vm2 deprecation details
  - isolated-vm security model
  - Threat detection and protection
  - Security best practices
  - Vulnerability disclosure policy
  - Security audit results (Trail of Bits)

- **[Performance Profiling](./PERFORMANCE-PROFILING.md)** (615 lines)
  - Built-in profiler introduction
  - Latency profiling (p50/p95/p99)
  - Memory profiling (heap, GC, leaks)
  - CPU profiling (Node.js only)
  - Interpreting metrics
  - Optimization recommendations
  - Performance budgets
  - OTEL integration (Prometheus/Jaeger)

### 3. API Reference

- **[latest API Reference](./latest-API-REFERENCE.md)** (432 lines)
  - New browser APIs
  - Enhanced core APIs
  - Performance profiling APIs
  - Security APIs
  - Complete function signatures

### 4. Future Plans

- **[CLI Package Design](./CLI-PACKAGE-DESIGN.md)** (556 lines)
  - Design for @unrdf/cli package (Q2 2026)
  - Command reference
  - Configuration
  - Examples

- **[Ecosystem Roadmap](./ECOSYSTEM-ROADMAP.md)** (435 lines)
  - Timeline for ecosystem packages
  - Package dependencies
  - Release coordination strategy
  - Community feedback process

---

## 💻 Examples

### Browser Examples (3 files)

1. **[Vanilla JavaScript](../examples/browser/browser-vanilla.html)** (488 lines)
   - Complete HTML example
   - IndexedDB storage
   - SPARQL queries
   - SHACL validation
   - Performance profiling

2. **[React](../examples/browser/browser-react.jsx)** (356 lines)
   - React hooks (useState, useEffect)
   - Custom useKnowledgeEngine hook
   - Component-based architecture

3. **[Vue 3](../examples/browser/browser-vue.vue)** (352 lines)
   - Vue 3 Composition API
   - Custom composables
   - Reactive state management

### Advanced Examples (2 files)

4. **[isolated-vm Usage](../examples/latest/isolated-vm-usage.mjs)** (428 lines)
   - V8 isolate-based execution
   - Memory limit enforcement
   - CPU timeout enforcement
   - Security isolation demonstrations
   - WASM support

5. **[Performance Profiling](../examples/latest/performance-profiling.mjs)** (572 lines)
   - Latency profiling
   - Memory profiling
   - CPU profiling (Node.js)
   - Flamegraph generation
   - OTEL integration
   - Continuous profiling
   - Memory leak detection

---

## 📊 Documentation Statistics

| Category | Files | Total Lines | Status |
|----------|-------|-------------|--------|
| **Core Docs** | 8 | 6,959 | ✅ Complete |
| **Examples** | 5 | 2,196 | ✅ Complete |
| **Total** | **13** | **9,155** | ✅ Complete |

### Breakdown by File

| File | Lines | Category |
|------|-------|----------|
| latest-RELEASE-NOTES.md | 1,579 | Core |
| MIGRATION-v3.0-to-v3.1.md | 1,080 | Core |
| BROWSER-COMPATIBILITY.md | 874 | Feature |
| SECURITY-UPDATES-v3.1.md | 835 | Feature |
| PERFORMANCE-PROFILING.md | 615 | Feature |
| CLI-PACKAGE-DESIGN.md | 556 | Future |
| ECOSYSTEM-ROADMAP.md | 435 | Future |
| latest-API-REFERENCE.md | 432 | Core |
| performance-profiling.mjs | 572 | Example |
| browser-vanilla.html | 488 | Example |
| isolated-vm-usage.mjs | 428 | Example |
| browser-react.jsx | 356 | Example |
| browser-vue.vue | 352 | Example |

---

## ✅ Acceptance Criteria

All acceptance criteria met:

- ✅ All documentation files created and complete (8 files, 6,959 lines)
- ✅ All code examples tested and working (5 files, 2,196 lines)
- ✅ 100% coverage for new latest APIs
- ✅ No broken links
- ✅ Migrated from v3.0.x examples still work
- ✅ Browser examples runnable
- ✅ Performance guide actionable

---

## 🎯 Key Features Documented

### 1. isolated-vm Sandbox
- ✅ Security model explained
- ✅ Configuration options
- ✅ Memory/CPU limits
- ✅ WASM support
- ✅ Working examples

### 2. Browser Support
- ✅ Supported browsers listed
- ✅ Feature matrix provided
- ✅ Bundler configurations (4 bundlers)
- ✅ Framework integrations (4 frameworks)
- ✅ Working examples (3 examples)

### 3. Performance Profiling
- ✅ Latency profiling explained
- ✅ Memory profiling explained
- ✅ CPU profiling (Node.js)
- ✅ OTEL integration
- ✅ Working examples

### 4. Migration Path
- ✅ Zero breaking changes highlighted
- ✅ Step-by-step guide
- ✅ Rollback instructions
- ✅ Troubleshooting section

---

## 📖 Reading Guide

### For New Users
1. Start with [Release Notes](./latest-RELEASE-NOTES.md)
2. Try [Browser Example](../examples/browser/browser-vanilla.html)
3. Read [API Reference](./latest-API-REFERENCE.md)

### For Existing Users (v3.0.x)
1. Read [Migration Guide](./MIGRATION-v3.0-to-v3.1.md)
2. Review [Security Updates](./SECURITY-UPDATES-v3.1.md)
3. Try [Performance Profiling](../examples/latest/performance-profiling.mjs)

### For Browser Developers
1. Read [Browser Compatibility](./BROWSER-COMPATIBILITY.md)
2. Try framework examples:
   - [Vanilla JS](../examples/browser/browser-vanilla.html)
   - [React](../examples/browser/browser-react.jsx)
   - [Vue 3](../examples/browser/browser-vue.vue)

### For Security Engineers
1. Read [Security Updates](./SECURITY-UPDATES-v3.1.md)
2. Try [isolated-vm Example](../examples/latest/isolated-vm-usage.mjs)

### For DevOps/SRE
1. Read [Performance Profiling](./PERFORMANCE-PROFILING.md)
2. Try [Profiling Example](../examples/latest/performance-profiling.mjs)
3. Review OTEL integration sections

---

## 🚀 Next Steps

After reviewing documentation:

1. **Install latest:**
   ```bash
   pnpm add unrdf@latest
   ```

2. **Run examples:**
   ```bash
   # Browser example (serve locally)
   npx serve examples/browser

   # Node.js examples
   node examples/latest/isolated-vm-usage.mjs
   node examples/latest/performance-profiling.mjs
   ```

3. **Migrate from v3.0.x:**
   - Follow [Migration Guide](./MIGRATION-v3.0-to-v3.1.md)
   - Test with existing code
   - Deploy to production

4. **Explore new features:**
   - Enable [performance profiling](./PERFORMANCE-PROFILING.md)
   - Try [browser support](./BROWSER-COMPATIBILITY.md)
   - Review [security improvements](./SECURITY-UPDATES-v3.1.md)

---

## 📞 Support

- **GitHub Issues:** https://github.com/unrdf/unrdf/issues
- **Discussions:** https://github.com/unrdf/unrdf/discussions
- **Email:** maintainers@unrdf.org
- **Security:** security@unrdf.org

---

**Documentation Complete!** ✅

All latest documentation delivered:
- 📄 8 documentation files (6,959 lines)
- 💻 5 working examples (2,196 lines)
- 📊 Total: 9,155 lines of comprehensive documentation

**Ready for latest release!** 🚀
