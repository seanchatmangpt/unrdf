# KGC Probe Quick Reference

## The 10 Domains at a Glance

| # | Domain | Testable Claim | Success Criterion | Key Guard |
|---|--------|----------------|------------------|-----------|
| 1 | **Frontmatter** | YAML parses + all fields valid Zod schema | 0 schema violations | o_hash ∈ [a-f0-9]{64} |
| 2 | **Block Structure** | All 4 block types parse + metadata valid | All blocks parse ✓ | receiptId in frontmatter |
| 3 | **Receipt Validation** | SHA-256(receipt) === receipt.id | Hashes match ✓ | decision=ADMIT only |
| 4 | **Dynamic Sections** | Content between markers matches receipt.output_hash | 1:1 section-receipt mapping | No orphaned receipts |
| 5 | **Determinism** | strict blocks reproducible, lenient semantically equivalent | Bit-for-bit or semantic equivalence | ORDER BY required for strict |
| 6 | **Resource Bounds** | Receipt metadata stays within limits | 0 bounds violations | utilization < 100% |
| 7 | **Receipt DAG** | No cycles, all deps exist, timestamps ordered | Valid DAG ✓ | No cyclic dependencies |
| 8 | **Schema Conformance** | All fields pass Zod validation | 100% schema coverage | Type-safe validation |
| 9 | **Cross-References** | All links resolvable, headings unique | No broken links | Heading hierarchy valid |
| 10 | **Error Reporting** | Errors specific + actionable | <1% FP, 99%+ recall | Exact location + remediation |

---

## Acceptance Criteria (Condensed)

```
✅ Checkpoint 1: Parser (Sprint 1)
  - Frontmatter parsing: 25 tests
  - Block parsing: 30 tests
  - Coverage: >80%

✅ Checkpoint 2: Receipts (Sprint 2)
  - Receipt loading + hashing: 20 tests
  - DAG validation: 15 tests
  - Integration: 10 test documents

✅ Checkpoint 3: Verification (Sprint 3)
  - Reproducibility: T0 === T1 (5 runs)
  - Latency: p99 < 100ms
  - Error accuracy: <1% FP/FN (100-doc corpus)
  - Coverage: >90%
```

---

## Key Claims & Evidence

| # | Claim | Evidence Required | Pass Criterion |
|---|-------|------------------|-----------------|
| T-001 | Identical world + probe version => identical report | Hash(report_T0) === Hash(report_T1) | Byte-for-byte match |
| T-002 | Every receiptId has matching receipt | Load all from store | 0 missing, 0 mismatches |
| T-003 | Dynamic section hash matches receipt | SHA-256(content) === receipt.output_hash | All match |
| T-004 | All frontmatter fields satisfy schema | Zod validation | 0 violations |
| T-005 | Every block has valid metadata | JSON parse + type check | All valid |
| T-006 | Receipt metadata respects bounds | resultCount ≤ maxQueries, etc. | 0 violations |
| T-007 | strict blocks verified deterministic | ORDER BY present, level matches | All verified |
| T-008 | No cyclic dependencies | DAG check (DFS) | 0 cycles |
| T-009 | All receipts use same o_hash | Compare receipt.o_hash to frontmatter | All match |
| T-010 | All dynamic sections have receipts | Check HTML comments + receipts | 100% mapped |
| T-011 | All internal links valid | Compare anchors to headings | 0 broken |
| T-012 | Merkle proofs verify | Recompute siblings → root | All correct |
| T-013 | policy_id valid UUID v4 | Regex + format check | Valid UUID |
| T-014 | Versions monotonically increase | Compare version >= previous | No downgrades |
| T-015 | Source files exist + hash match | Load file, hash, compare | 0 failures |

---

## Guard Policy (Forbidden Operations)

```
FORBIDDEN:
  ❌ Execute SPARQL queries (parse-only)
  ❌ Network I/O (http, fetch, etc.)
  ❌ File write (fs.writeFile, appendFile)
  ❌ Child process (execSync, spawn)
  ❌ Path traversal (no ../ in paths)
  ❌ State mutation (pure functions)
  ❌ Non-deterministic (no random, no timestamps)
  ❌ Eval / dynamic code
  ❌ Dynamic require
  ❌ Receipt tampering (read-only)

RESOURCE LIMITS:
  Memory:          500MB max
  File size:       1GB + 100 receipts
  Parsing:         YAML <10KB, JSON <1MB
  Receipt chain:   Max 1000 receipts, max 10-level depth
  Concurrency:     Single-threaded, 1 doc per instance
```

---

## Success Metrics Summary

| Metric | Target | Evidence | Proof |
|--------|--------|----------|-------|
| **Coverage** | 100% receipts found | receipts_found === receipts_declared | report.coverage=100% |
| **Error Accuracy** | <1% FP, 99%+ recall | 100-doc corpus tested | precision ≥ 99%, recall ≥ 99% |
| **Latency** | <100ms p99 | Benchmark typical doc | p50<50ms, p99<100ms |
| **Hash Collision** | 0 collisions | 10,000+ vectors | collision_count === 0 |
| **Reproducibility** | 100% identical | 5 runs same doc | all hashes equal |
| **Schema Coverage** | 100% rules tested | Zod schema test suite | coverage === 100% |
| **DAG Validity** | Valid (no cycles) | Synthetic DAGs | cycles_found === 0 |
| **Link Integrity** | 99%+ accuracy | Corpus with known broken links | FP < 1% |
| **Error Specificity** | 100% context | 50 sample errors reviewed | all have file:line:field |
| **Throughput** | 10+ docs/sec | 100-doc scan | throughput ≥ 10 docs/sec |

---

## CLI Usage Examples

### Basic Scan
```bash
kgc probe scan docs/api-reference.kgcmd
# Output: JSON report with all 10 domains checked
```

### Targeted Domain Check
```bash
kgc probe scan docs/api-reference.kgcmd --domain frontmatter
kgc probe scan docs/api-reference.kgcmd --domain receipts
kgc probe scan docs/api-reference.kgcmd --domain dynamic
kgc probe scan docs/api-reference.kgcmd --domain determinism
```

### Verbose Error Reporting
```bash
kgc probe scan docs/api-reference.kgcmd --errors detailed
# Shows exact line numbers, field names, remediation steps
```

### Performance Benchmark
```bash
kgc probe benchmark --corpus ./docs --iterations 10
# p50, p95, p99 latencies; throughput; error rates
```

### Reproducibility Test
```bash
kgc probe test-reproducibility docs/api-reference.kgcmd --runs 5
# Hashes output 5 times; all must be identical
```

---

## Test Organization

```
test/probe/
├── frontmatter.test.mjs        (25 tests)
├── blocks.test.mjs              (30 tests)
├── receipts.test.mjs            (20 tests)
├── dag.test.mjs                 (15 tests)
├── dynamic-sections.test.mjs    (15 tests)
├── determinism.test.mjs         (15 tests)
├── bounds.test.mjs              (10 tests)
├── schema.test.mjs              (20 tests)
├── cross-refs.test.mjs          (15 tests)
├── error-reporting.test.mjs     (20 tests)
├── integration.test.mjs         (10 tests)
└── error-accuracy.test.mjs      (100+ corpus tests)

Total: 200+ unit tests + integration tests
Target coverage: >90%
```

---

## Implementation Phases

### Phase 1: MVP (Week 1)
**Deliverables**:
- [x] Probe CLI with `kgc probe scan` command
- [x] Frontmatter parser + Zod validation
- [x] Block parser + metadata validation
- [x] 10 representative test documents
- [ ] **EXIT CRITERIA**: 50 tests PASS, >80% coverage

### Phase 2: Core (Week 2)
**Deliverables**:
- [ ] Receipt loader + SHA-256 verification
- [ ] DAG validator (cycles, ordering)
- [ ] Dynamic section mapper + hasher
- [ ] Determinism checker (ORDER BY, canonicalization)
- [ ] Resource bounds verifier
- [ ] **EXIT CRITERIA**: 50+ new tests, >90% coverage

### Phase 3: Polish (Week 3)
**Deliverables**:
- [ ] Link integrity checker
- [ ] Comprehensive error reporter
- [ ] Full schema validation
- [ ] Performance benchmarks
- [ ] Documentation + examples
- [ ] **EXIT CRITERIA**: 100% acceptance criteria PASS

---

## Zod Schema Reference

### FrontmatterSchema Fields
```
o_hash:           ^[a-f0-9]{64}$
policy_id:        UUID v4
receipts:         Array<64-char hex>, [0,1000]
bounds:
  maxQueries:     [1, 10000]
  maxRuntime:     [100, 60000]
  maxFileScans:   [1, 1000]
views:            ['tutorial'|'how-to'|'reference'|'explanation'], [1,4]
sources:          Array<{path, lineStart, lineEnd, hash}>
version:          semver
createdAt:        ISO 8601
lastProved:       ISO 8601, >= createdAt
```

### BlockMetadataSchema Fields
```
receiptId:           ^[a-f0-9]{64}$
expectedOutputFormat: 'json'|'markdown'|'text'
determinismLevel:    'strict'|'lenient'|'best-effort'
metadata:            JSON object (type-specific)
```

### ReceiptSchema Fields
```
id:               ^[a-f0-9]{64}$ (SHA-256)
timestamp:        ISO 8601
o_hash:           ^[a-f0-9]{64}$
block_type:       'kgc:query'|'kgc:proof'|'kgc:extract'|'kgc:render'
input_hash:       ^[a-f0-9]{64}$
output_hash:      ^[a-f0-9]{64}$
decision:         'ADMIT'|'REJECT'|'PARTIAL'
metadata:         JSON (block-specific)
dependencies:     Array<^[a-f0-9]{64}$> (optional)
merkle_proof:     {siblings, root, index, totalLeaves} (optional)
```

---

## Decision Tree: How Probe Works

```
                    ┌─ Load .kgcmd file
                    │
                    ├─ Parse YAML frontmatter
                    │   └─ Validate with FrontmatterSchema
                    │       └─ Check o_hash, policy_id, bounds, views, etc.
                    │
                    ├─ Scan for executable blocks (kgc:*)
                    │   └─ For each block:
                    │       ├─ Parse metadata (before ---)
                    │       ├─ Validate with BlockMetadataSchema
                    │       ├─ Check body format (JSON, SPARQL, etc.)
                    │       └─ Record receiptId
                    │
                    ├─ Load all receipts from store
                    │   └─ For each receipt:
                    │       ├─ Verify SHA-256(receipt) === receipt.id
                    │       ├─ Check o_hash consistency
                    │       ├─ Validate schema
                    │       └─ Record metadata (resourceCount, runtime, etc.)
                    │
                    ├─ Build receipt dependency DAG
                    │   └─ Check:
                    │       ├─ No cycles (DFS)
                    │       ├─ All dependencies exist
                    │       └─ Timestamp ordering correct
                    │
                    ├─ Scan for dynamic sections (<!-- kgc:dynamic -->)
                    │   └─ For each section:
                    │       ├─ Extract receiptId
                    │       ├─ Hash content (RFC 8785)
                    │       └─ Compare to receipt.output_hash
                    │
                    ├─ Check determinism (strict/lenient)
                    │   └─ Verify ORDER BY, canonicalization, sorting
                    │
                    ├─ Check resource bounds
                    │   └─ resultCount ≤ maxQueries, etc.
                    │
                    ├─ Scan internal links
                    │   └─ Verify all [text](#anchor) are valid
                    │
                    └─ Compile report (JSON)
                        ├─ All 10 domains: PASS/FAIL
                        ├─ Error list (if any)
                        ├─ Coverage %
                        └─ Metrics (latency, counts, etc.)
```

---

## Common Errors & Remediation

| Error | Cause | Fix |
|-------|-------|-----|
| InvalidFrontmatter: o_hash | Too short/wrong format | Use 64-char SHA-256 hash |
| MissingReceipt: ID not in frontmatter | Block references non-existent receipt | Add receipt ID to frontmatter array |
| MismatchedHash: dynamic section | Manual edit after receipt generation | Regenerate receipt or restore content |
| BoundsExceeded: maxQueries | Query returned more results than declared | Increase bounds or optimize query |
| NonDeterministic: query missing ORDER BY | strict block without ordering | Add ORDER BY clause |
| CyclicDependency | Receipt depends on itself (directly/transitively) | Review and remove circular dependencies |
| InvalidBlockStructure | Missing --- separator | Add --- between metadata and body |

---

## Reproducibility Guarantee

**Theorem**: If world_state remains constant and probe_version is fixed, then scan_report is deterministic.

**Proof**:
1. All inputs (frontmatter, blocks, receipts) are deterministic (files are immutable)
2. All parsing is deterministic (YAML, JSON parsers produce same output)
3. All hashing is deterministic (SHA-256 + RFC 8785 canonicalization)
4. All control flow is deterministic (no random, no Date.now(), no I/O latency affecting output)
5. All output is deterministic (JSON serialized with sorted keys, no timestamps)

**Therefore**: `Hash(scan(doc, T0)) === Hash(scan(doc, T1))` ∀T0, T1

---

## File Structure

```
packages/kgc-probe/
├── src/
│   ├── probe.frontmatter.mjs
│   ├── probe.blocks.mjs
│   ├── probe.receipts.mjs
│   ├── probe.dag.mjs
│   ├── probe.dynamic.mjs
│   ├── probe.determinism.mjs
│   ├── probe.bounds.mjs
│   ├── probe.schema.mjs
│   ├── probe.cross-refs.mjs
│   ├── probe.error-reporting.mjs
│   ├── probe.cli.mjs
│   └── index.mjs
├── test/
│   ├── probe/
│   │   └── *.test.mjs (50+ tests)
│   └── fixtures/
│       ├── documents/
│       │   └── *.kgcmd (10 test documents)
│       └── receipts/
│           └── *.json (20 test receipts)
├── docs/
│   ├── KGC-PROBE-SPECIFICATION.md
│   ├── KGC-PROBE-QUICK-REFERENCE.md
│   └── examples/
│       └── *.kgcmd (example documents)
├── package.json
└── README.md
```

---

## Completion Checklist

**By End of Sprint 1**:
- [ ] All 10 domains specified (THIS DOCUMENT)
- [ ] Frontmatter parser implemented + 25 tests PASS
- [ ] Block parser implemented + 30 tests PASS
- [ ] CLI basic command working
- [ ] Code coverage > 80%

**By End of Sprint 2**:
- [ ] Receipt validation working + 20 tests PASS
- [ ] DAG validation working + 15 tests PASS
- [ ] Dynamic section mapping working + 15 tests PASS
- [ ] Determinism checking working + 15 tests PASS
- [ ] Bounds checking working + 10 tests PASS
- [ ] Code coverage > 90%

**By End of Sprint 3**:
- [ ] Cross-reference checking + 15 tests PASS
- [ ] Comprehensive error reporting + 20 tests PASS
- [ ] Full schema validation + 20 tests PASS
- [ ] Performance benchmarks established
- [ ] Reproducibility verified (5 runs, identical hashes)
- [ ] 100-document error accuracy corpus tested
- [ ] Code coverage > 95%
- [ ] All acceptance criteria PASS ✅

---

**Ready to implement?** Start with [Phase 1 Sprint 1](#implementation-phases)
