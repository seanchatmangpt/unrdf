# KGC Probe Specification - Document Index

**Status**: ✅ Complete and Implementation Ready  
**Date**: 2025-12-27  
**Methodology**: SPARC (Specification Phase)

---

## Quick Navigation

### Start Here (5 min read)
- **[KGC-PROBE-SPECIFICATION-SUMMARY.md](/home/user/unrdf/docs/KGC-PROBE-SPECIFICATION-SUMMARY.md)** - Executive overview, key claims, success metrics

### Implementation Guides

**For Developers** (2 hour read):
- **[KGC-PROBE-SPECIFICATION.md](/home/user/unrdf/docs/KGC-PROBE-SPECIFICATION.md)** - Full narrative specification with 10 domains detailed, examples, test cases

**For Quick Reference** (30 min):
- **[KGC-PROBE-QUICK-REFERENCE.md](/home/user/unrdf/docs/KGC-PROBE-QUICK-REFERENCE.md)** - One-page tables, CLI examples, decision trees

**For CLI Design** (1 hour):
- **[KGC-PROBE-SPECIFICATION-INDEX.md](/home/user/unrdf/docs/KGC-PROBE-SPECIFICATION-INDEX.md)** - Command definitions, pseudocode, architecture

### Structured Data

**For Automated Processing**:
- **[KGC-PROBE-SPECIFICATION.json](/home/user/unrdf/docs/KGC-PROBE-SPECIFICATION.json)** - Complete spec in JSON format (importable to tools)

### Reference

**Complete Manifest**:
- **[KGC-PROBE-SPECIFICATION-MANIFEST.txt](/home/user/unrdf/docs/KGC-PROBE-SPECIFICATION-MANIFEST.txt)** - Full deliverables list, file structure, checklists

---

## The 10 Probe Domains

| # | Domain | Purpose | Tests |
|---|--------|---------|-------|
| 1 | **Frontmatter** | Parse YAML + validate schema | 25 |
| 2 | **Block Structure** | Validate executable blocks | 30 |
| 3 | **Receipt Validation** | Verify SHA-256 hashes + structure | 20 |
| 4 | **Dynamic Sections** | Hash-match content to receipts | 15 |
| 5 | **Determinism** | Verify reproducibility | 15 |
| 6 | **Resource Bounds** | Enforce usage limits | 10 |
| 7 | **Receipt DAG** | Validate dependency graph | 15 |
| 8 | **Schema Conformance** | Type-safe validation | 20 |
| 9 | **Cross-References** | Check link integrity | 15 |
| 10 | **Error Reporting** | Specific error messages | 20 |

---

## Key Deliverables

### Specification Documents (4)

| File | Size | Content | Use Case |
|------|------|---------|----------|
| `KGC-PROBE-SPECIFICATION.json` | 28 KB | Structured spec, schemas, metrics | Automated tools, test generation |
| `KGC-PROBE-SPECIFICATION.md` | 25 KB | Full narrative, detailed domains | Developer onboarding |
| `KGC-PROBE-QUICK-REFERENCE.md` | 15 KB | Tables, examples, quick lookup | Sprint planning, status updates |
| `KGC-PROBE-SPECIFICATION-SUMMARY.md` | 30 KB | Executive overview, proofs | Leadership reviews |

### Manifest & Index (2)

| File | Content | Use Case |
|------|---------|----------|
| `KGC-PROBE-SPECIFICATION-MANIFEST.txt` | Complete file list, checklist | Project management, tracking |
| `KGC-PROBE-SPECIFICATION-INDEX.md` | CLI commands, pseudocode | Architecture review |

---

## By Role

### Project Manager
1. Read `SPECIFICATION-SUMMARY.md`
2. Use `QUICK-REFERENCE.md` for domains + metrics
3. Track against `SPECIFICATION-MANIFEST.txt` checklist

### Developer
1. Read `SPECIFICATION.md` fully (all 10 domains)
2. Implement domains in order (1→10)
3. Reference `QUICK-REFERENCE.md` for quick lookups
4. Reference `.json` for schema details

### QA / Test Engineer
1. Extract acceptance criteria from `.json`
2. Create test corpus (10 documents)
3. Map tests to each domain
4. Validate metrics (error accuracy, latency)

### Architect
1. Review guard policy (forbidden operations)
2. Validate resource limits
3. Check reproducibility guarantee
4. Approve Zod schemas

---

## Success Metrics at a Glance

| Metric | Target | Evidence |
|--------|--------|----------|
| Document Coverage | 100% receipts found | All declared receipts scanned |
| Error Accuracy | <1% FP, 99%+ recall | 100-document corpus tested |
| Latency | <100ms p99 | Benchmark typical document |
| Hash Collision | 0 collisions | 10,000+ test vectors |
| Reproducibility | 100% identical | 5 runs produce same output |
| Schema Coverage | 100% rules tested | Comprehensive test suite |
| DAG Validity | 0 cycles | DFS check valid |
| Link Integrity | 99%+ accuracy | Corpus with known broken links |
| Error Specificity | 100% context | All errors have location + fix |
| Throughput | 10+ docs/sec | 100-document scan time |

---

## Implementation Timeline

### Sprint 1 (Week 1): MVP
- Frontmatter parser (25 tests)
- Block parser (30 tests)
- Exit: 50 tests PASS, >80% coverage

### Sprint 2 (Week 2): Core
- Receipt validator (20 tests)
- DAG validator (15 tests)
- Dynamic sections (15 tests)
- Exit: 50+ tests PASS, >90% coverage

### Sprint 3 (Week 3): Polish
- Error reporter (20 tests)
- Cross-ref checker (15 tests)
- Schema validator (20 tests)
- Exit: 100% criteria PASS

---

## Core Invariant

**Theorem**: If world state is constant and probe version is fixed, then scan output is deterministic.

```
scan(doc, T0) ≡ scan(doc, T1)  for all T0, T1 with same world state
=> Hash(report_T0) === Hash(report_T1)  (byte-for-byte identical)
```

**Guarantee**: Complete reproducibility with no randomness, no timestamps, no I/O latency affecting output.

---

## File Locations

All specification documents are located in:
```
/home/user/unrdf/docs/
├── KGC-PROBE-SPECIFICATION.json
├── KGC-PROBE-SPECIFICATION.md
├── KGC-PROBE-QUICK-REFERENCE.md
├── KGC-PROBE-SPECIFICATION-SUMMARY.md
├── KGC-PROBE-SPECIFICATION-INDEX.md
├── KGC-PROBE-SPECIFICATION-MANIFEST.txt
└── README-KGC-PROBE-SPEC.md (this file)
```

Total documentation: ~111 KB across 6 files

---

## How to Get Started

### Step 1: Understand the Problem (15 minutes)
Read `SPECIFICATION-SUMMARY.md` for the big picture.

### Step 2: Learn the Domains (1 hour)
Read `SPECIFICATION.md` sections on all 10 domains.

### Step 3: Plan Implementation (30 minutes)
Review `QUICK-REFERENCE.md` implementation roadmap.

### Step 4: Setup Project (1 hour)
Create package structure per file structure template in `QUICK-REFERENCE.md`.

### Step 5: Begin Sprint 1 (Week 1)
Implement frontmatter + block parsers per `SPECIFICATION.md` Domain 1-2.

---

## Frequently Asked Questions

**Q: Where do I start implementing?**  
A: Start with Sprint 1 (Week 1) in `QUICK-REFERENCE.md`. Implement Domain 1 & 2 first.

**Q: How do I know when I'm done?**  
A: Check the success metrics table. All 10 must be achieved.

**Q: What if my test fails?**  
A: Reference the domain acceptance criteria in `SPECIFICATION.md` for that domain.

**Q: How do I validate my implementation?**  
A: Run test checkpoints at end of each sprint (per `SPECIFICATION.md`).

**Q: Can I change the 10 domains?**  
A: No. These are testable claims extracted from the KGC Markdown spec.

**Q: What's the priority order?**  
A: Domains 1, 2, 3, 4 are critical. Complete those first. Domains 5-10 follow.

---

## Related Documents

- **KGC-MARKDOWN-SPECIFICATION.md** - The format being verified
- **KGC-SIDECAR-CLIENT-IMPLEMENTATION.md** - Related gRPC client
- **CLAUDE.md** - Project instructions & adversarial PM principles

---

## Status

- ✅ Specification: COMPLETE
- ⏳ Implementation: READY TO START
- 📅 Timeline: 3 weeks (Sprint 1, 2, 3)
- ✅ Exit Criteria: 100% acceptance criteria PASS

---

**Last Updated**: 2025-12-27  
**Specification Version**: 1.0.0  
**Status**: Implementation Ready
