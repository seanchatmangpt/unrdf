# UNRDF latest Architecture Documentation Index

**Version:** latest
**Created:** 2025-11-16
**Status:** Ready for Review

---

## Document Overview

This directory contains the complete technical architecture for UNRDF latest implementation. The architecture covers four major subsystems with zero breaking changes to the public API.

---

## Core Documents

### 1. Comprehensive Architecture Specification
**File:** `/docs/latest-ARCHITECTURE.md`
**Size:** ~75 KB
**Audience:** Architects, Senior Developers, Technical Leads

**Contents:**
- Executive Summary
- Isolated-VM Sandbox Architecture (detailed design)
- Browser Compatibility Architecture (IndexedDB, Web Workers)
- OTEL Validation Framework v3 (90+/100 target)
- Performance Profiling Architecture (built-in profiler)
- Deployment & Bundling Considerations
- Breaking Changes Analysis (zero breaking changes)
- Implementation Roadmap (8-week plan)
- Success Metrics (scorecard & KPIs)
- Risks & Mitigations
- Appendices (ADRs, API compatibility matrix)

**Key Sections:**
- Section 1: Isolated-VM Sandbox Architecture
- Section 2: Browser Compatibility Architecture
- Section 3: OTEL Validation Framework v3 Architecture
- Section 4: Performance Profiling Architecture
- Section 5: Deployment & Bundling Considerations
- Section 6: Breaking Changes Analysis
- Section 7: Implementation Roadmap
- Section 8: Success Metrics
- Section 9: Risks & Mitigations
- Section 10: Conclusion

### 2. Architecture Summary (Quick Reference)
**File:** `/docs/latest-ARCHITECTURE-SUMMARY.md`
**Size:** ~30 KB
**Audience:** All Developers, Product Managers

**Contents:**
- Quick reference for each subsystem
- Text-based architecture diagrams
- Implementation priority matrix
- Breaking changes analysis
- Success metrics dashboard
- Risk mitigation matrix
- Quick start guide for developers
- ADR summaries
- Next steps

**Use This For:**
- Daily development reference
- Quick architecture lookups
- Onboarding new team members
- Sprint planning

### 3. System Architecture Diagram
**File:** `/docs/latest-architecture.puml`
**Format:** PlantUML
**Audience:** Visual learners, Presentations

**Renders:**
- Complete system architecture
- Component relationships
- Runtime environments (Node.js vs Browser)
- Sandbox execution flow
- Storage abstraction
- Query engine architecture
- Profiling subsystem
- OTEL validation framework
- Legend and notes

**How to Render:**
```bash
# Using PlantUML CLI
plantuml latest-architecture.puml

# Using VS Code extension
# Install: PlantUML extension
# Right-click → Preview Current Diagram

# Online
# Copy content to: http://www.plantuml.com/plantuml/uml/
```

### 4. Deployment Architecture Diagram
**File:** `/docs/latest-deployment.puml`
**Format:** PlantUML
**Audience:** DevOps, Build Engineers, Release Managers

**Renders:**
- Source code organization
- Build pipeline flow
- Distribution bundles (Node/Browser/Universal)
- Package exports strategy
- Deployment targets
- Module dependencies
- Bundle size analysis
- Color-coded by target environment

**Key Information:**
- Node.js bundle: ~200KB
- Browser bundle: ~450KB (minified)
- Universal bundle: ~300KB
- Dependencies breakdown
- Tree-shaking strategy

---

## Quick Navigation

### By Role

**Architects & Tech Leads:**
1. Read: `/docs/latest-ARCHITECTURE.md` (comprehensive)
2. Review: ADRs in Appendix A
3. Review: Breaking Changes Analysis (Section 6)

**Developers (Implementation):**
1. Read: `/docs/latest-ARCHITECTURE-SUMMARY.md` (quick ref)
2. Review: Implementation Priority Matrix (Section 5)
3. View: `/docs/latest-architecture.puml` (system diagram)
4. Follow: Quick Start Guide (Section 9)

**DevOps & Build Engineers:**
1. Review: Deployment & Bundling (Section 5 in comprehensive doc)
2. View: `/docs/latest-deployment.puml` (deployment diagram)
3. Review: Package Exports Strategy

**Product Managers:**
1. Read: Executive Summary (Section 1 in comprehensive doc)
2. Review: Success Metrics Dashboard (Summary doc, Section 7)
3. Review: Implementation Roadmap (Comprehensive doc, Section 7)

### By Subsystem

**Isolated-VM Sandbox:**
- Comprehensive: Section 1
- Summary: Section 1
- Diagram: Sandbox Layer in architecture.puml
- Files: `src/security/executors/`

**Browser Compatibility:**
- Comprehensive: Section 2
- Summary: Section 2
- Diagram: Storage/Query layers in architecture.puml
- Files: `src/knowledge-engine/storage/`

**OTEL Validation v3:**
- Comprehensive: Section 3
- Summary: Section 3
- Diagram: Validation Layer in architecture.puml
- Files: `src/validation/features/`

**Performance Profiling:**
- Comprehensive: Section 4
- Summary: Section 4
- Diagram: Profiling Layer in architecture.puml
- Files: `src/knowledge-engine/profiling/`

---

## Key Architectural Decisions

### ADR-001: Isolated-VM over vm2
**Decision:** Multi-executor strategy with auto-detection
**Rationale:** vm2 deprecated, security vulnerabilities, no maintenance
**Impact:** Enhanced security, async support, 50ms initialization overhead
**See:** Comprehensive doc, Appendix A

### ADR-002: IndexedDB for Browser File System
**Decision:** IndexedDB with file-like API
**Rationale:** Standard browser storage, persistent, async
**Impact:** Storage quota limits, but works across all browsers
**See:** Comprehensive doc, Appendix A

### ADR-003: Remove Legacy CLI Validation
**Decision:** Remove CLI validation from OTEL scoring
**Rationale:** Knowledge Hooks API is primary interface, CLI is convenience
**Impact:** Cleaner validation, focus on core features
**See:** Comprehensive doc, Appendix A

### ADR-004: Built-in Profiler (No External Deps)
**Decision:** Custom profiler using OTEL + native APIs
**Rationale:** Zero dependencies, full control, OTEL integration
**Impact:** Limited features vs clinic.js, but zero deps
**See:** Comprehensive doc, Appendix A

---

## Implementation Artifacts

### New Files to Create (35 files)

**Sandbox Subsystem (6 files):**
```
src/security/
├── sandbox-detector.mjs              🆕
├── executors/
│   ├── isolated-vm-executor.mjs      🆕
│   ├── worker-executor.mjs           🆕
│   ├── vm2-executor.mjs              🆕
│   └── browser-executor.mjs          🆕
└── sandbox-adapter.mjs               ✏️ Update
```

**Storage Subsystem (3 files):**
```
src/knowledge-engine/storage/
├── fs-adapter.mjs                    🆕
├── node-fs.mjs                       🆕
└── indexeddb-fs.mjs                  🆕
```

**Profiling Subsystem (11 files):**
```
src/knowledge-engine/profiling/
├── index.mjs                         🆕
├── latency-profiler.mjs              🆕
├── memory-profiler.mjs               🆕
├── cpu-profiler.mjs                  🆕
├── metrics-collector.mjs             🆕
├── profiler-config.mjs               🆕
└── reporters/
    ├── json-reporter.mjs             🆕
    ├── html-reporter.mjs             🆕
    └── terminal-reporter.mjs         🆕
```

**Validation Subsystem (10 files):**
```
src/validation/features/
├── knowledge-engine.validator.mjs    ✏️ Update
├── knowledge-hooks.validator.mjs     🆕
├── policy-packs.validator.mjs        🆕
├── lockchain.validator.mjs           🆕
└── browser.validator.mjs             🆕

src/validation/rules/
├── span-rules.mjs                    🆕
├── attribute-rules.mjs               🆕
├── performance-rules.mjs             🆕
└── custom-rules.mjs                  🆕
```

**Build & Distribution (5 files):**
```
dist/
├── unrdf.node.mjs                    🆕 (generated)
├── unrdf.browser.mjs                 🆕 (generated)
└── unrdf.esm.mjs                     🆕 (generated)

build.config.mjs                      ✏️ Update
package.json                          ✏️ Update exports
```

**Legend:**
- 🆕 New file
- ✏️ Update existing file

---

## Success Metrics

### Current vs Target

| Metric | v3.0.x | latest Target | Delta |
|--------|--------|---------------|-------|
| OTEL Validation Score | 81/100 | 92/100 | +11 |
| Browser Support | None | Full | New |
| Isolated-VM Coverage | 0% | 100% | New |
| Bundle Size (Browser) | N/A | ≤500KB | Target |
| Breaking Changes | 0 | 0 | ✅ |
| Performance Overhead | Baseline | ≤10% | Target |

### KGC PRD Compliance

| Requirement | Status |
|-------------|--------|
| p50 hook eval ≤ 200µs | ✅ Met |
| p99 ≤ 2ms | ✅ Met |
| 10k exec/min | ✅ Met |
| 100% error isolation | ✅ Enhanced |
| Receipt write ≤ 5ms | ✅ Met |

---

## Implementation Phases

### Phase 1: Foundation (Week 1-2)
**Focus:** Isolated-VM Sandbox + Browser File System

**Deliverables:**
- Sandbox detector with capability detection
- Isolated-VM executor (primary)
- Worker thread executor (fallback)
- IndexedDB file system for browser
- Unified FS adapter

**Success Criteria:**
- All executors functional
- Browser file system working
- Zero breaking changes
- Tests passing

### Phase 2: Enhancement (Week 3-4)
**Focus:** OTEL Validation v3 + Profiling Foundation

**Deliverables:**
- Remove legacy CLI validations
- Knowledge Hooks validator
- Policy Packs validator
- Latency profiler
- Memory profiler

**Success Criteria:**
- OTEL score ≥ 85/100
- Profiler functional
- JSON export working

### Phase 3: Polish (Week 5-6)
**Focus:** Advanced Features + Target Achievement

**Deliverables:**
- Lockchain validator
- Browser compatibility validator
- CPU profiler (Node.js)
- HTML dashboard
- Weighted scoring

**Success Criteria:**
- OTEL score ≥ 90/100
- All profilers functional
- Browser demo working

### Phase 4: Testing & Release (Week 7-8)
**Focus:** Integration, Documentation, Release

**Deliverables:**
- E2E testing (all executors)
- Browser compatibility testing
- Performance regression testing
- Migration guide
- Release candidate

**Success Criteria:**
- All success metrics met
- Documentation complete
- Release latest

---

## Testing Strategy

### Unit Tests
```bash
pnpm run test
# Vitest for all new modules
# Coverage target: 90%
```

### Integration Tests
```bash
node validation/run-all.mjs comprehensive
# OTEL span-based validation
# Target: 90+/100 score
```

### Browser Tests
```bash
# Manual testing in browser-demo/
open browser-demo/index.html
# Test in Chrome, Firefox, Safari
```

### Performance Tests
```bash
node examples/performance-baseline.mjs
# Baseline vs v3.0.x
# Overhead target: ≤10%
```

---

## Dependencies

### New Dependencies (latest)

**Production:**
```json
{
  "isolated-vm": "^latest"  // Optional, Node 18+ only
}
```

**Development:**
```json
{
  "obuild": "^latest"  // Already exists, updated config
}
```

**No Removals:** vm2 kept for backward compatibility (fallback)

---

## Breaking Changes

### Public API: Zero Breaking Changes

All v3.0.x code works unchanged in latest:

```javascript
// v3.0.x code - still works in latest
import { KnowledgeEngine } from 'unrdf';
const engine = new KnowledgeEngine();
await engine.parse(data, { format: 'turtle' });
// ✅ Works identically, now with isolated-vm under the hood
```

### New APIs (Additive Only)

```javascript
// latest - new optional features
import { createPerformanceProfiler } from 'unrdf/profiling';

const profiler = createPerformanceProfiler();
await profiler.profile('parse', () => engine.parse(data));
```

### Deprecations (Non-Breaking)

- vm2 executor: Deprecated, but still works
- CLI validation: Removed from OTEL scoring, CLI still functional

---

## Next Steps

### Week 1 Actions

1. **Architecture Review**
   - [ ] Team review meeting (1 hour)
   - [ ] Approve/modify design decisions
   - [ ] Identify blockers/unknowns

2. **Project Setup**
   - [ ] Create feature branch: `feature/latest`
   - [ ] Set up GitHub project board
   - [ ] Create implementation issues (35+ files)

3. **Begin Development**
   - [ ] Implement `sandbox-detector.mjs`
   - [ ] Implement `isolated-vm-executor.mjs`
   - [ ] Implement `indexeddb-fs.mjs`

4. **Infrastructure**
   - [ ] Set up CI for isolated-vm tests
   - [ ] Configure browser testing (Playwright/Puppeteer)
   - [ ] Set up OTEL validation baseline

---

## Questions & Support

### Architecture Questions
- Review comprehensive doc: `/docs/latest-ARCHITECTURE.md`
- Review ADRs in Appendix A
- Contact: Architecture Team

### Implementation Questions
- Review summary doc: `/docs/latest-ARCHITECTURE-SUMMARY.md`
- Review Quick Start Guide (Section 9)
- Review Implementation Priority Matrix (Section 5)

### Diagrams & Visuals
- System architecture: `/docs/latest-architecture.puml`
- Deployment architecture: `/docs/latest-deployment.puml`
- Render online: http://www.plantuml.com/plantuml/uml/

---

## Document History

| Version | Date | Changes | Author |
|---------|------|---------|--------|
| 1.0 | 2025-11-16 | Initial architecture design | System Architecture Team |

---

## Related Documentation

- **latest Release Notes:** `/docs/latest-RELEASE-NOTES.md`
- **latest Vision:** `/docs/latest-VISION.md`
- **Migration Guide (v2→v3):** `/docs/migration-v2-to-v3.md`
- **Production Sign-Off:** `/PRODUCTION-SIGN-OFF.md`
- **KGC Sidecar Implementation:** `/KGC-SIDECAR-CLIENT-IMPLEMENTATION.md`

---

**Status:** Ready for Review
**Review Deadline:** Week 1
**Implementation Start:** Week 2
**Target Release:** latest (8 weeks from approval)

---

For questions or clarifications, please contact the System Architecture Team.
