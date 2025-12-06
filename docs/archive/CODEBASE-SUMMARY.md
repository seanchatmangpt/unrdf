# UNRDF Codebase Summary

**Quick Reference Guide for UNRDF Project**

---

## Project At-A-Glance

| Aspect | Details |
|--------|---------|
| **Current Version** | 3.0.4 (patch) / 3.0.0 (major release) |
| **Release Date** | October 2, 2025 |
| **Status** | Production-Ready |
| **Primary Purpose** | RDF Knowledge Graph Library with Autonomic Hooks |
| **Test Coverage** | 100% core (114/114 tests passing) |
| **Lines of Code** | ~2,000+ modules across src/ |

---

## Quick Links

- **Full Analysis:** [v3.1.0 PRD & Codebase Analysis](./v3.1.0-PRD-CODEBASE-ANALYSIS.md)
- **Release Notes:** [v3.0.0 Release Notes](./v3.0.0-RELEASE-NOTES.md)
- **Vision Document:** [v3.0.0 Vision](./v3.0.0-VISION.md)
- **Changelog:** [CHANGELOG.md](./CHANGELOG.md)
- **Roadmap:** [ROADMAP.md](./ROADMAP.md)

---

## Key Findings

### Strengths

1. **Mature Architecture** - Dark Matter 80/20 optimization framework is solid
2. **Production-Ready** - 100% test coverage on core functionality
3. **Security-Focused** - Merkle verification, sandboxed execution, audit trails
4. **Observable** - Comprehensive OpenTelemetry instrumentation (62 tests)
5. **Well-Documented** - 50+ analysis docs, clear API, JSDoc throughout
6. **Clear Vision** - v3.0 was strategic refocus to core (removed CLI/knowledge-engine)

### Known Issues

1. **vm2 Deprecation** (CRITICAL) - Deprecated library for sandboxing
2. **Browser Support** (MEDIUM) - Incomplete browser compatibility shims
3. **OTEL Validation Score** (MEDIUM) - 81/100, includes legacy CLI checks
4. **No Performance Profiling** (MEDIUM) - Limited visibility into bottlenecks

### Next Release Plan (v3.1.0, Q1 2026)

**Primary Objectives:**
1. Replace vm2 with isolated-vm (security fix)
2. Complete browser compatibility
3. Update OTEL validation framework
4. Add performance profiling tools
5. Prepare ecosystem packages (CLI, knowledge-engine)

**Estimated Effort:** 20-25 days (4-5 weeks)

**Recommendation:** ✅ **GO** - vm2 migration is critical, browser support unblocks use cases

---

## Core Features Inventory

### Complete & Production-Ready

- RDF Parsing/Serialization (Turtle, N-Quads, JSON-LD)
- SPARQL Query Execution (all types)
- SHACL Validation
- Knowledge Hooks (autonomic policy system)
- Transaction Management (ACID)
- Dark Matter Optimization
- Cryptographic Provenance (lockchain)
- OpenTelemetry Observability
- Effect Sandboxing
- Query Optimization & Caching

### Planned for v3.1.0

- Isolated-vm sandbox replacement
- Browser compatibility fixes
- Performance profiling dashboard
- CLI/knowledge-engine package design docs

### Future Roadmap (v3.2.0+)

- Streaming RDF processing
- Advanced query optimization
- GraphQL integration
- Ecosystem packages (@unrdf/cli, @unrdf/knowledge-engine, @unrdf/web, @unrdf/ui)

---

## Technology Stack

**Language & Runtime:**
- JavaScript (ESM, .mjs only)
- Node.js 18.0.0+
- No TypeScript (JSDoc + Zod for types)

**Core Dependencies:**
- N3.js (RDF operations)
- Comunica (SPARQL)
- rdf-validate-shacl (SHACL)
- OpenTelemetry (observability)
- vm2 (sandboxing - to be replaced)

**Development Tools:**
- pnpm (package manager)
- Vitest (testing)
- ESLint + Prettier (code quality)
- obuild (zero-transpile builds)

**Infrastructure:**
- Kubernetes manifests
- Terraform configs
- GitHub Actions CI/CD
- TestContainers for integration testing

---

## Project Structure Overview

```
src/
├── knowledge-engine/       # 41 core modules (RDF operations)
├── cli/                   # CLI commands (experimental)
├── knowledge-engine/              # gRPC integration (experimental)
├── validation/           # OTEL validation framework
├── security/             # Security utilities
└── ...others

test/
├── knowledge-engine/     # 114 core tests (100% passing)
├── readme-validation/    # Documentation example tests
└── e2e/                 # End-to-end integration tests

docs/
├── v3.1.0-PRD-CODEBASE-ANALYSIS.md  # THIS ANALYSIS
├── v3.0.0-RELEASE-NOTES.md
├── ROADMAP.md
└── 50+ other docs
```

---

## Success Metrics (v3.1.0 Target)

| Metric | Current | Target |
|--------|---------|--------|
| Test Coverage | 100% | 100% |
| OTEL Validation Score | 81/100 | 90+/100 |
| Core Latency (p95) | <100ms | <100ms |
| Browser Support | Limited | 4+ major browsers |
| Documentation | 95% | 100% |
| Security Issues | 0 (vm2 pending) | 0 |

---

## Decision Checklist for v3.1.0

**Before Kickoff:**
- [ ] Finalize vm2 → isolated-vm strategy
- [ ] Define browser compatibility scope (Chrome, Firefox, Safari, Edge?)
- [ ] Decide on performance tooling approach (dashboard vs logs?)
- [ ] Schedule ecosystem package planning (CLI/knowledge-engine)
- [ ] Assign team roles (security lead, backend engineers, infra)

**During Development:**
- [ ] Maintain 100% test coverage
- [ ] Security audit for isolated-vm
- [ ] Browser testing setup (Playwright)
- [ ] OTEL validation framework rewrite
- [ ] Performance baseline establishment

**Before Release:**
- [ ] All v3.0.0 tests still pass
- [ ] Security validation complete
- [ ] Browser examples runnable
- [ ] OTEL score ≥85/100
- [ ] Ecosystem packages designed

---

## References

**Full Analysis Document:** `/home/user/unrdf/docs/v3.1.0-PRD-CODEBASE-ANALYSIS.md`

**Repository:** https://github.com/unrdf/unrdf

**Key Documentation Files:**
- v3.0.0 Vision: `docs/v3.0.0-VISION.md`
- Release Notes: `docs/v3.0.0-RELEASE-NOTES.md`
- Roadmap: `docs/ROADMAP.md`
- FAQ: `docs/FAQ.md`
- Troubleshooting: `docs/TROUBLESHOOTING.md`

---

**Prepared:** November 16, 2025  
**Analyst:** Claude Code (Codebase Explorer)  
**Status:** Analysis Complete - Ready for v3.1.0 Planning
