# KGC Probe Specification - Executive Summary

**Status**: ✅ SPECIFICATION COMPLETE - Implementation Ready
**Date**: 2025-12-27
**Methodology**: SPARC Phase (Specification)
**Agent**: Agent-1 (Specification)

---

## What is the KGC Probe?

A **deterministic verification engine** that scans `.kgcmd` (KGC Markdown) documents and proves:

1. **Frontmatter is valid** - All YAML metadata satisfies schema constraints
2. **Receipts exist & verified** - Every reference resolves with correct SHA-256 hashes
3. **Dynamic sections match outputs** - Content hashes equal receipt.output_hash values
4. **Blocks are deterministic** - Queries ordered, JSON canonicalized, no randomness
5. **Bounds are respected** - Resource usage (queries, runtime, files) within declared limits

**Core Invariant**: `scan(doc, T0) ≡ scan(doc, T1)` → **Byte-for-byte identical audit reports**

---

## Specification Artifacts Delivered

### 1. **KGC-PROBE-SPECIFICATION.json** (28 KB, 602 lines)
**Structured data for automated processing**
- ✅ 15 testable claims with evidence requirements
- ✅ 10 domain acceptance criteria (domain-by-domain checklists)
- ✅ Guard policy summary (10 forbidden operations + 5 resource limits)
- ✅ 10 success metrics with measurement approaches
- ✅ Zod schema mappings for all data types
- ✅ 3-phase implementation roadmap

**Use Case**: Import into project management tools, automated test generation, CI/CD pipeline integration

### 2. **KGC-PROBE-SPECIFICATION.md** (25 KB, 672 lines)
**Narrative specification with examples**
- ✅ Comprehensive problem statement
- ✅ 10 domains with testable claims, evidence requirements, example tests
- ✅ Test checkpoints for 3 sprint phases
- ✅ Guard policy + resource limits explained
- ✅ Success metrics with verification approach
- ✅ Complete implementation checklist

**Use Case**: Developer onboarding, requirements understanding, design discussions

### 3. **KGC-PROBE-QUICK-REFERENCE.md** (15 KB, 384 lines)
**One-page reference for project management**
- ✅ 10 domains at a glance (condensed table)
- ✅ Condensed acceptance criteria
- ✅ Key claims & evidence summary
- ✅ Guard policy (forbidden + limits)
- ✅ Success metrics summary
- ✅ CLI usage examples
- ✅ Test organization + phases
- ✅ File structure template
- ✅ Completion checklist

**Use Case**: Sprint planning, status updates, quick lookups, team synchronization

---

## The 10 Probe Domains

| Domain | Claim | Evidence | Success |
|--------|-------|----------|---------|
| 1. **Frontmatter** | YAML + schema valid | Parse, validate fields | 0 violations |
| 2. **Block Structure** | Metadata parsed correctly | JSON valid, fields present | All blocks valid |
| 3. **Receipt Validation** | SHA-256 hashes correct | Load, hash, verify | Hashes match |
| 4. **Dynamic Sections** | Content matches receipt | Hash sections, compare | 1:1 mapping |
| 5. **Determinism** | strict/lenient reproducible | Verify ordering, canonicalization | Reproducible |
| 6. **Resource Bounds** | Usage within limits | Check metadata vs bounds | 0 violations |
| 7. **Receipt DAG** | No cycles, deps exist | Build graph, check | Valid DAG |
| 8. **Schema Conformance** | All data type-safe | Zod validation | 100% valid |
| 9. **Cross-References** | Links resolvable | Map headings, check links | No broken refs |
| 10. **Error Reporting** | Specific + actionable | Check FP/FN rates | <1% FP, 99%+ recall |

---

## Testable Claims Across All Domains

**15 concrete, measurable claims**:

| # | Claim | Measurement | Acceptance |
|---|-------|-------------|-----------|
| T-001 | Determinism: identical world => identical report | Hash equality | Byte-for-byte match |
| T-002 | Receipt completeness: every ID has receipt | Missing count | 0 missing |
| T-003 | Hash integrity: section matches receipt | Hash comparison | All match |
| T-004 | Frontmatter schema: all fields valid | Validation errors | 0 violations |
| T-005 | Block structure: all valid | Parse errors | All parse ✓ |
| T-006 | Bounds enforcement: usage ≤ limits | Utilization % | 0 violations |
| T-007 | Determinism level: strict verified | ORDER BY check | All verified |
| T-008 | DAG validity: no cycles | Cycle count | 0 cycles |
| T-009 | Universe consistency: same o_hash | Hash mismatch count | 0 mismatches |
| T-010 | Coverage: all sections mapped | Orphaned count | 0 orphans |
| T-011 | Link validity: all resolvable | Broken link count | 0 broken |
| T-012 | Merkle proofs: verify correctly | Proof validity | All valid |
| T-013 | Policy ID: valid UUID v4 | UUID validation | Valid UUID |
| T-014 | Versions: monotonic increase | Downgrade count | No downgrades |
| T-015 | Source files: exist + hash match | Missing/mismatch count | 0 failures |

---

## Success Metrics

| Metric | Target | Verification | Proof |
|--------|--------|---|------|
| **Document Coverage** | 100% receipts found | All declared receipts scanned | receipts_found === declared |
| **Error Accuracy** | <1% FP, 99%+ recall | 100-doc corpus tested | precision ≥ 99%, recall ≥ 99% |
| **Latency** | <100ms p99 | Benchmark typical doc | p50<50ms, p99<100ms |
| **Hash Collision** | 0 collisions | 10,000+ test vectors | collision_count === 0 |
| **Reproducibility** | 100% identical | 5 runs same doc | all hashes equal |
| **Schema Coverage** | 100% rules tested | Complete test suite | coverage === 100% |
| **DAG Validity** | Valid (no cycles) | Synthetic test DAGs | cycles === 0 |
| **Link Integrity** | 99%+ accuracy | Corpus with broken links | FP < 1% |
| **Error Specificity** | 100% context | 50 error samples | all have location + remediation |
| **Throughput** | 10+ docs/sec | 100-doc scan | throughput ≥ 10 docs/sec |

---

## Guard Policy Summary

### Forbidden Operations (10 total)

```
FORBIDDEN:
  ❌ Execute SPARQL queries        (parse-only verification)
  ❌ Network I/O                   (prevent external side effects)
  ❌ File system write             (maintain scan-only property)
  ❌ Child process execution       (prevent code injection)
  ❌ Out-of-bounds file access     (enforce path containment)
  ❌ State mutation                (ensure reproducibility)
  ❌ Non-deterministic operations  (no random, no timestamps)
  ❌ Eval / dynamic code           (prevent injection)
  ❌ Dynamic require               (prevent module loading)
  ❌ Receipt tampering             (maintain integrity)
```

### Resource Limits (5 total)

```
LIMITS:
  Memory:          500MB max per document
  File I/O:        1GB document + 100 receipts
  Parsing:         YAML <10KB, JSON <1MB
  Receipt chain:   Max 1000 receipts, max 10-level depth
  Concurrency:     Single-threaded, 1 doc per instance
```

---

## Implementation Roadmap (3 Weeks)

### Sprint 1: Parser Correctness (Week 1)
**Deliverables**: Frontmatter + block parsing, 50 tests, >80% coverage
```bash
✓ YAML parser + Zod validation     (frontmatter.mjs)
✓ Block parser + metadata check    (blocks.mjs)
✓ CLI: kgc probe scan              (cli.mjs)
✓ 25 frontmatter unit tests
✓ 30 block unit tests
✓ 10 test documents
Goal: EXIT when 50 tests PASS, coverage >80%
```

### Sprint 2: Core Verification (Week 2)
**Deliverables**: Receipts, DAG, dynamic sections, bounds, 50+ new tests, >90% coverage
```bash
✓ Receipt loader + SHA-256 verify  (receipts.mjs)
✓ DAG validator (cycles, order)    (dag.mjs)
✓ Dynamic section mapper + hash    (dynamic.mjs)
✓ Determinism checker              (determinism.mjs)
✓ Bounds enforcer                  (bounds.mjs)
✓ 20 receipt tests, 15 DAG tests, 15 dynamic tests, etc.
Goal: EXIT when 50+ new tests PASS, coverage >90%
```

### Sprint 3: Polish & Validation (Week 3)
**Deliverables**: Error reporting, cross-refs, full coverage, docs, benchmarks, 100% criteria
```bash
✓ Error reporter (all error types)       (error-reporting.mjs)
✓ Link checker (internal + cross-refs)   (cross-refs.mjs)
✓ Schema validator (complete)            (schema.mjs)
✓ Performance benchmarks
✓ Reproducibility verification (5 runs)
✓ Error accuracy corpus (100+ docs)
✓ Documentation + examples
Goal: EXIT when 100% acceptance criteria PASS, all metrics met
```

---

## Evidence Requirements for Each Domain

### Domain 1: Frontmatter
```bash
Test: kgc probe scan docs/api.kgcmd --domain frontmatter
Evidence:
  ✅ o_hash: 64-char hex pattern matches
  ✅ policy_id: valid UUID v4 format
  ✅ receipts: array of valid hashes
  ✅ bounds: numeric ranges validated
  ✅ views: known Diátaxis types
  ✅ version: semver format valid
  ✅ timestamps: ISO 8601 valid + ordering correct
Exit Criteria: 0 validation errors
```

### Domain 2: Blocks
```bash
Test: kgc probe scan docs/api.kgcmd --domain blocks
Evidence:
  ✅ All 4 block types parse correctly
  ✅ Metadata is valid JSON
  ✅ All required fields present
  ✅ receiptId in frontmatter
  ✅ determinismLevel valid
  ✅ Strict blocks have ORDER BY
Exit Criteria: All blocks valid, 0 parse errors
```

### Domain 3: Receipts
```bash
Test: kgc probe verify docs/api.kgcmd --domain receipts
Evidence:
  ✅ Receipt files exist in store
  ✅ SHA-256(receipt) === receipt.id
  ✅ o_hash consistency
  ✅ All metadata fields present
  ✅ Decision is ADMIT
Exit Criteria: All receipts verified, 0 hash failures
```

### Domain 4: Dynamic Sections
```bash
Test: kgc probe scan docs/api.kgcmd --domain dynamic
Evidence:
  ✅ All sections marked with HTML comments
  ✅ receiptId in section matches frontmatter
  ✅ SHA-256(content, RFC 8785) === receipt.output_hash
  ✅ No manual edits detected
  ✅ 1:1 mapping: receipts ↔ sections
Exit Criteria: All sections hash-match, 0 orphans, 0 unmapped receipts
```

### Domain 5: Determinism
```bash
Test: kgc probe scan docs/api.kgcmd --domain determinism
Evidence:
  ✅ Strict blocks reproducible (bit-for-bit)
  ✅ Lenient blocks semantically equivalent
  ✅ ORDER BY present (strict queries)
  ✅ JSON canonicalization (RFC 8785)
  ✅ No timestamps/random in hash
Exit Criteria: All blocks meet determinism level requirement
```

### Domain 6: Resource Bounds
```bash
Test: kgc probe scan docs/api.kgcmd --domain bounds
Evidence:
  ✅ resultCount <= maxQueries
  ✅ executionTime <= maxRuntime
  ✅ filesScanned <= maxFileScans
  ✅ Utilization report (% of limit)
Exit Criteria: 0 bounds violations
```

### Domain 7: DAG
```bash
Test: kgc probe verify docs/api.kgcmd --domain dag
Evidence:
  ✅ Build directed graph from receipt.dependencies
  ✅ DFS check: 0 cycles
  ✅ All dependency IDs exist
  ✅ Timestamp ordering: dependent >= max(dependencies)
  ✅ o_hash consistency across all receipts
Exit Criteria: Valid DAG, 0 cycles, proper ordering
```

### Domain 8: Schema
```bash
Test: kgc probe scan docs/api.kgcmd --domain schema
Evidence:
  ✅ FrontmatterSchema: all field types + constraints
  ✅ BlockMetadataSchema: block metadata valid
  ✅ QueryMetadataSchema: query blocks valid
  ✅ ReceiptSchema: all receipt fields valid
Exit Criteria: 0 schema violations, 100% field coverage
```

### Domain 9: Cross-References
```bash
Test: kgc probe scan docs/api.kgcmd --domain cross-refs
Evidence:
  ✅ Heading map created (H1, H2, H3, H4)
  ✅ All internal links resolvable
  ✅ Source links reference valid indices
  ✅ Receipt links reference frontmatter
  ✅ No heading level skips
  ✅ Heading IDs unique
Exit Criteria: 0 broken links
```

### Domain 10: Error Reporting
```bash
Test: kgc probe scan docs/broken.kgcmd --domain errors
Evidence:
  ✅ Errors classified correctly
  ✅ Error location: file:line:field
  ✅ Severity levels applied
  ✅ Remediation suggestions provided
  ✅ False positive rate <1%
  ✅ False negative rate <1%
Exit Criteria: <1% FP, 99%+ recall, all errors actionable
```

---

## File Locations

All specifications created in `/home/user/unrdf/docs/`:

```
✅ KGC-PROBE-SPECIFICATION.json              (28 KB) - Structured spec
✅ KGC-PROBE-SPECIFICATION.md                (25 KB) - Narrative spec
✅ KGC-PROBE-QUICK-REFERENCE.md              (15 KB) - Quick lookup
✅ KGC-PROBE-SPECIFICATION-SUMMARY.md        (This file) - Executive summary
✅ KGC-PROBE-SPECIFICATION-INDEX.md          (13 KB) - CLI specification index
```

---

## How to Use These Documents

### For Project Managers
1. Read this summary
2. Review Quick Reference for domains + phases
3. Track progress against implementation checklist
4. Report status using metrics table

### For Developers
1. Read the full Specification.md
2. Use Quick Reference for domain details
3. Implement domains in order: 1→10
4. Reference JSON for schema details
5. Check test checkpoints at sprint boundaries

### For QA / Testing
1. Extract acceptance criteria from JSON
2. Map to test cases in each domain
3. Create test data corpus
4. Validate metrics (error accuracy, latency, etc.)
5. Report coverage + pass rates

### For Architecture Review
1. Review guard policy (forbidden operations)
2. Validate resource limits are appropriate
3. Check reproducibility guarantee (determinism)
4. Review Zod schemas
5. Approve implementation roadmap

---

## Key Guarantees

### ✅ Deterministic Reproducibility
**Theorem**: Given same world state and probe version, scan output is bit-for-bit identical.

**Proof**: No randomness, no timestamps, no I/O latency in output; all hashing is deterministic.

### ✅ Cryptographic Integrity
**Theorem**: All data validated with SHA-256 + RFC 8785 canonicalization.

**Proof**: Every receipt hash verified; every dynamic section content hashed; no collisions.

### ✅ Complete Coverage
**Theorem**: Probe identifies all sections, blocks, and receipts without missing any.

**Proof**: Frontmatter declares receipt count; probe scans all; mismatch detected.

### ✅ Error Detection
**Theorem**: Probe detects 99%+ of actual violations with <1% false positive rate.

**Proof**: Comprehensive test corpus + manual verification on diverse documents.

### ✅ Resource Safety
**Theorem**: Probe bounded: no network I/O, no unbounded computation, single-threaded.

**Proof**: Guard policy enforced; resource limits checked; timeout protection.

---

## Next Steps

### Immediate (This Week)
1. **Review this specification** with team
2. **Approve the 10 domains** and success metrics
3. **Setup project structure** (package.json, test framework)
4. **Assign developers** to Sprint 1

### Sprint 1 (Next Week)
1. Implement frontmatter parser + 25 tests
2. Implement block parser + 30 tests
3. Verify: 50 tests PASS, coverage >80%

### Sprint 2 (Week After)
1. Implement receipt verification + 20 tests
2. Implement DAG validation + 15 tests
3. Implement dynamic sections + 15 tests
4. Verify: 50+ new tests PASS, coverage >90%

### Sprint 3 (Week 3)
1. Complete error reporting + cross-refs + schema
2. Benchmarks + reproducibility verification
3. Error accuracy corpus testing (100 docs)
4. **DONE**: 100% acceptance criteria PASS

---

## Success Criteria

**Specification Phase Complete When**:
- ✅ All 15 testable claims understood
- ✅ All 10 domains acceptance criteria clear
- ✅ All 10 success metrics measurable
- ✅ Guard policy and resource limits approved
- ✅ Zod schemas defined
- ✅ Implementation roadmap agreed
- ✅ Team ready to begin Sprint 1

**Implementation Phase Complete When**:
- ✅ All 200+ unit tests PASS
- ✅ Code coverage >95%
- ✅ All 10 success metrics achieved
- ✅ Zero false positives on 100-doc corpus
- ✅ <100ms latency p99
- ✅ Reproducibility verified (5 runs identical)
- ✅ Comprehensive documentation

---

## Questions?

Refer to the detailed specification documents:

1. **"What's the detailed requirement?"** → KGC-PROBE-SPECIFICATION.md
2. **"What's the quick summary?"** → KGC-PROBE-QUICK-REFERENCE.md
3. **"What's the data structure?"** → KGC-PROBE-SPECIFICATION.json
4. **"What's the CLI design?"** → KGC-PROBE-SPECIFICATION-INDEX.md

---

**Specification Status**: ✅ COMPLETE - Ready for implementation
**Date**: 2025-12-27
**Agent**: Agent-1 (Specification)
**Methodology**: SPARC Phase (Specification)
