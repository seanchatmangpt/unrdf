# KGC Probe Specification - SPARC Methodology

**Version**: 1.0.0
**Date**: 2025-12-27
**Agent**: Agent-1 (Specification)
**Status**: Implementation-Ready

---

## Executive Summary

The **KGC Probe** is a deterministic verification engine that scans `.kgcmd` (KGC Markdown) documents and proves five critical properties:

1. **Frontmatter Valid**: All YAML metadata parses correctly and satisfies schema constraints
2. **Receipts Exist & Verified**: Every referenced receipt exists, has correct SHA-256 hash, and forms valid dependency DAG
3. **Dynamic Sections Integrity**: Content between `<!-- kgc:dynamic -->` markers matches receipt output hashes
4. **Blocks Deterministic**: Queries have `ORDER BY`, outputs use RFC 8785 canonicalization, no random elements
5. **Bounds Respected**: Receipt metadata (resultCount, executionTime, filesScanned) stays within declared limits

**Core Invariant**: `kgc probe scan document.kgcmd` produces **identical, byte-for-byte audit report** across multiple runs with same world state.

---

## Problem Statement

KGC Markdown documents are verifiable by design, but **verification is non-trivial**:

- ✅ Receipts contain cryptographic proof
- ✅ Frontmatter declares resource bounds
- ✅ Dynamic sections are marked with metadata
- ❌ **No tool exists to automatically verify all properties**
- ❌ Manual verification is error-prone and doesn't scale
- ❌ False positives/negatives create uncertainty

**KGC Probe solves this**: automated, deterministic verification that proves document integrity.

---

## 10 Probe Domains

### Domain 1: Frontmatter Parsing & Validation

**What**: Parse YAML frontmatter and validate every field against Zod schema.

**Testable Claims**:
- YAML parses without syntax errors
- `o_hash`: 64-character lowercase hex (SHA-256 format)
- `policy_id`: Valid UUID v4 (RFC 4122)
- `receipts`: Array of 0-1000 receipt IDs (64-char hex each)
- `bounds`: Resource limits within ranges (queries: 1-10k, runtime: 100-60k ms, fileScans: 1-1k)
- `views`: Non-empty array of Diátaxis types (tutorial, how-to, reference, explanation)
- `sources`: Array of file ranges with hashes; `lineEnd >= lineStart`
- `version`: Valid semver (e.g., "1.0.0")
- `createdAt`, `lastProved`: ISO 8601 datetimes; `lastProved >= createdAt`

**Example Test**:
```bash
$ kgc probe scan docs/api-reference.kgcmd --domain frontmatter
✅ o_hash: valid (a3f5b8c2d1e4...)
✅ policy_id: valid UUID (550e8400-e29b-41d4-a716-446655440000)
✅ receipts: 10 items, all 64-char hex
✅ bounds: maxQueries=100 (valid range [1,10000])
✅ version: valid semver (1.0.0)
✅ createdAt: 2025-12-26T10:00:00Z
✅ lastProved: 2025-12-26T14:00:00Z (>= createdAt)
Domain: frontmatter - PASS
```

**Success Criterion**: **0 schema violations; all required fields present**

---

### Domain 2: Block Structure & Metadata

**What**: Parse every executable block (`kgc:query`, `kgc:proof`, `kgc:extract`, `kgc:render`) and validate structure.

**Testable Claims**:
- Block fence is valid: ` ```kgc:TYPE ` (where TYPE is one of 4 block types)
- Metadata (before `---`) is valid JSON object
- `receiptId` present and 64-char hex (matches a frontmatter receipt)
- `expectedOutputFormat` is one of [json, markdown, text]
- `determinismLevel` is one of [strict, lenient, best-effort]
- Body format matches block type:
  - `kgc:query`: SPARQL or N3 query with PREFIX declarations
  - `kgc:extract`: JSON object with targetFiles or filters
  - `kgc:render`: JSON object with items array
  - `kgc:proof`: JSON object with receiptIds and expectedRoot
- Query blocks with `determinismLevel: strict` include `ORDER BY` clause

**Example Test**:
```bash
$ kgc probe scan docs/api-reference.kgcmd --domain blocks
Block 1: kgc:query (line 45)
  ✅ receiptId: b4c5d6e7... (in frontmatter)
  ✅ expectedOutputFormat: json
  ✅ determinismLevel: strict
  ✅ Has ORDER BY clause (required for strict)
  ✅ queryType: sparql (valid)
  ✅ resultBounds: minResults=0, maxResults=1000 (valid)
  ✅ timeout: 5000ms (within bounds)

Block 2: kgc:extract (line 102)
  ✅ receiptId: c5d6e7f8... (in frontmatter)
  ✅ extractionType: exports (valid)
  ✅ fileGlobs: ["src/api/**/*.mjs"]
  ✅ includeDocstrings: true
Domain: blocks - PASS (2/2 blocks valid)
```

**Success Criterion**: **All blocks parse + all checks PASS; 0 structure errors**

---

### Domain 3: Receipt Validation & Cryptography

**What**: Load receipts and verify SHA-256 hashes, structure, and field correctness.

**Testable Claims**:
- Receipt file exists (in `receipts/<receipt_id>.json` or DB)
- Receipt ID matches: `SHA-256(canonical_JSON(receipt_body)) === receipt.id`
- All required fields present: id, timestamp, o_hash, block_type, input_hash, output_hash, decision, metadata
- `o_hash`: matches frontmatter `o_hash` (universe consistency)
- `block_type`: one of [kgc:query, kgc:proof, kgc:extract, kgc:render]
- `input_hash`: SHA-256(block metadata + body)
- `output_hash`: SHA-256(generated content using RFC 8785 canonicalization)
- `decision`: one of [ADMIT, REJECT, PARTIAL]
- `timestamp`: valid ISO 8601
- `dependencies`: array of receipt IDs (all must exist)

**Example Test**:
```bash
$ kgc probe verify docs/api-reference.kgcmd --domain receipts
Receipt: b4c5d6e7f8a9... (kgc:extract)
  ✅ File exists: receipts/b4c5d6e7f8a9.json
  ✅ ID hash: SHA-256 matches
  ✅ o_hash: a3f5b8c2... (matches frontmatter)
  ✅ decision: ADMIT (valid)
  ✅ timestamp: 2025-12-26T13:45:00Z
  ✅ metadata.extractionType: exports
  ✅ metadata.filesScanned: 1 (<= bounds.maxFileScans=20)
  ✅ metadata.itemsExtracted: 4
  ✅ No dependencies (acyclic)

Receipt: c5d6e7f8a9b0... (kgc:render)
  ✅ File exists: receipts/c5d6e7f8a9b0.json
  ✅ ID hash: SHA-256 matches
  ✅ dependencies: [b4c5d6e7f8a9] (exists in store)
  ✅ Timestamp ordering: c5d6e7f8(13:47:30) > b4c5d6e7(13:45:00) ✓

Domain: receipts - PASS (2/2 receipts verified)
```

**Success Criterion**: **All receipts ADMIT + hashes valid + no cycles + all deps exist**

---

### Domain 4: Dynamic Section Integrity

**What**: Verify that content between `<!-- kgc:dynamic -->` markers matches receipt outputs.

**Testable Claims**:
- Each dynamic section marked: `<!-- kgc:dynamic section=NAME receiptId=ID -->`
- Closed with: `<!-- /kgc:dynamic -->`
- `receiptId` exists in frontmatter.receipts
- Content hash (RFC 8785): `SHA-256(content_between_markers) === receipt.output_hash`
- No manual edits detected (content = receipt output exactly)
- All dynamic sections have receipt mapping (1:1 correspondence)
- No orphaned receipts (every receipt in frontmatter is used)

**Example Test**:
```bash
$ kgc probe scan docs/api-reference.kgcmd --domain dynamic
Dynamic section 1: api-functions (line 209)
  ✅ receiptId: c5d6e7f8a9b0... (in frontmatter)
  ✅ Opening tag: <!-- kgc:dynamic section="api-functions" ... -->
  ✅ Closing tag: <!-- /kgc:dynamic -->
  ✅ Content hash: e7f8a9b0... (computed)
  ✅ Receipt output_hash: e7f8a9b0... (from receipt)
  ✅ Hashes match ✓ (no manual edits)
  ✅ 4 functions extracted (matches receipt.metadata.itemsExtracted=4)

Summary:
  ✅ 1/1 dynamic sections mapped
  ✅ 2/2 receipts used (no orphans)

Domain: dynamic - PASS (all sections hash-match receipts)
```

**Success Criterion**: **All dynamic sections hash-match receipts + 1:1 mapping verified**

---

### Domain 5: Determinism & Reproducibility

**What**: Verify that blocks are deterministic and outputs reproducible.

**Testable Claims**:
- `strict` blocks: `receipt.metadata.determinismLevel = 'strict'`
- `strict` blocks (queries): include `ORDER BY` clause (required)
- `strict` blocks (extracts): results sorted by file path then line number
- `strict` blocks (renders): JSON keys sorted before template application
- `lenient` blocks: allow semantic equivalence (order variations OK, data values not)
- `best-effort` blocks: document non-deterministic fields in metadata
- Canonical JSON (RFC 8785): keys sorted, no extra whitespace, NFC unicode normalization
- No timestamps/random elements in hash computation
- Regeneration of block produces identical output (for strict) or semantically equivalent (for lenient)

**Example Test**:
```bash
$ kgc probe scan docs/api-reference.kgcmd --domain determinism
Block: kgc:query (line 45)
  ✅ determinismLevel: strict
  ✅ Query has ORDER BY clause: ORDER BY ?name ?user
  ✅ Receipt determinism: strict (matches block)
  ✅ JSON canonicalization: valid RFC 8785
  ✅ No timestamps in output_hash (verified by examining receipt metadata)
  ✅ Result order: deterministic (4 results, sorted by name)

Block: kgc:extract (line 102)
  ✅ determinismLevel: lenient (allows file order variation)
  ✅ Receipt output sorted by: file path, line number
  ✅ 4 items extracted, order deterministic by file location
  ✅ Semantic equivalence maintained (same exports regardless of file discovery order)

Domain: determinism - PASS (all blocks reproducible at declared level)
```

**Success Criterion**: **All strict blocks reproducible; lenient blocks semantically stable; no spurious order variations**

---

### Domain 6: Resource Bounds

**What**: Verify that actual resource usage stays within declared limits.

**Testable Claims**:
- `frontmatter.bounds.maxQueries >= receipt.metadata.resultCount` (for kgc:query blocks)
- `frontmatter.bounds.maxRuntime >= receipt.metadata.executionTime` (all blocks)
- `frontmatter.bounds.maxFileScans >= receipt.metadata.filesScanned` (for kgc:extract blocks)
- Missing metadata: warn if receipt lacks these fields
- Utilization report: show % of limit used (e.g., 40/100 queries = 40% usage)
- Alert if any utilization > 80% (suggest increasing bounds)

**Example Test**:
```bash
$ kgc probe scan docs/api-reference.kgcmd --domain bounds
Bounds declared:
  maxQueries: 100
  maxRuntime: 5000ms
  maxFileScans: 50

Receipt: b4c5d6e7 (kgc:extract)
  ✅ filesScanned: 1 (<= 50) - Utilization: 2%
  ✅ executionTime: 245ms (<= 5000ms) - Utilization: 4.9%
  ✅ ⚠️ Recommend: maxFileScans could be reduced to 5 (current: 1 file scanned)

Receipt: c5d6e7f8 (kgc:render)
  ✅ executionTime: 185ms (<= 5000ms) - Utilization: 3.7%

Summary:
  ✅ All resources within bounds
  ✅ Low utilization: consider tighter bounds for safety

Domain: bounds - PASS (0 violations; well-provisioned)
```

**Success Criterion**: **0 bounds violations; all resources within declared limits**

---

### Domain 7: Receipt Dependency Graph

**What**: Build and validate the DAG of receipt dependencies.

**Testable Claims**:
- No cycles in dependency graph (checked via DFS)
- All dependencies exist: every receipt.dependencies[*] ID exists in store
- Timestamp ordering: `dependent.timestamp >= max(dependency.timestamps)`
- Universe consistency: all receipt.o_hash values match frontmatter.o_hash
- Decision propagation: if dependency.decision = REJECT, dependent is tainted
- Merkle proof verification: if batch receipt has merkle_proof, verify siblings hash to root

**Example Test**:
```bash
$ kgc probe verify docs/api-reference.kgcmd --domain dag
Building receipt DAG...

Receipt b4c5d6e7 (kgc:extract)
  ✅ Dependencies: [] (leaf node)
  ✅ Timestamp: 2025-12-26T13:45:00Z

Receipt c5d6e7f8 (kgc:render) -> depends on b4c5d6e7
  ✅ Dependency exists: b4c5d6e7 (found)
  ✅ Timestamp ordering: 13:47:30 > 13:45:00 ✓
  ✅ o_hash consistency: a3f5b8c2 (matches frontmatter) ✓

Receipt d6e7f8a9 (kgc:proof) -> depends on [b4c5d6e7, c5d6e7f8]
  ✅ Dependency 1 (b4c5d6e7): exists, timestamp 13:45:00 ✓
  ✅ Dependency 2 (c5d6e7f8): exists, timestamp 13:47:30 ✓
  ✅ Ordering: 14:22:15 > max(13:47:30) ✓

DAG Analysis:
  ✅ No cycles detected (DFS: 0 back-edges)
  ✅ All 3 receipts connected (1 component)
  ✅ Max depth: 2 levels (< 1000 limit)

Domain: dag - PASS (valid DAG, proper ordering, no cycles)
```

**Success Criterion**: **DAG valid, no cycles, all timestamps ordered, all dependencies exist**

---

### Domain 8: Schema Conformance

**What**: Validate all documents and receipts against comprehensive Zod schemas.

**Testable Claims**:
- Frontmatter passes `FrontmatterSchema` validation
- Every block passes `BlockMetadataSchema` validation
- Query blocks pass `QueryMetadataSchema`
- Extract blocks pass `ExtractMetadataSchema`
- Render blocks pass `RenderMetadataSchema`
- Proof blocks pass `ProofMetadataSchema`
- All receipts pass `ReceiptSchema` validation
- Validation errors include specific field path and constraint violation

**Example Test**:
```bash
$ kgc probe scan docs/api-reference.kgcmd --domain schema
Validating schemas...

FrontmatterSchema:
  ✅ o_hash: matches regex ^[a-f0-9]{64}$
  ✅ policy_id: valid UUID (v4)
  ✅ receipts: array of 64-char hex, length 2 (within [0,1000])
  ✅ bounds.maxQueries: 100 (within [1,10000])
  ✅ bounds.maxRuntime: 5000 (within [100,60000])
  ✅ bounds.maxFileScans: 50 (within [1,1000])
  ✅ views: ["reference"] (valid values)
  ✅ sources: 1 item, all fields valid
  ✅ version: 1.0.0 (valid semver)
  ✅ createdAt: ISO 8601 ✓
  ✅ lastProved: ISO 8601, >= createdAt ✓

BlockMetadataSchema (Block 1: kgc:query):
  ✅ receiptId: 64-char hex, in frontmatter ✓
  ✅ expectedOutputFormat: json (one of [json, markdown, text]) ✓
  ✅ determinismLevel: strict (one of [strict, lenient, best-effort]) ✓
  ✅ metadata: valid JSON object ✓

QueryMetadataSchema:
  ✅ queryType: sparql (one of [sparql, n3, shacl]) ✓
  ✅ resultBounds.minResults: 0 (number) ✓
  ✅ resultBounds.maxResults: 1000 (number) ✓
  ✅ timeout: 5000 (number) ✓

ReceiptSchema (Receipt: b4c5d6e7...):
  ✅ id: 64-char hex ✓
  ✅ timestamp: ISO 8601 ✓
  ✅ o_hash: 64-char hex ✓
  ✅ block_type: kgc:extract (valid) ✓
  ✅ input_hash: 64-char hex ✓
  ✅ output_hash: 64-char hex ✓
  ✅ decision: ADMIT (one of [ADMIT, REJECT, PARTIAL]) ✓
  ✅ metadata: valid JSON object ✓

Domain: schema - PASS (all 10+ checks pass)
```

**Success Criterion**: **Document + all blocks pass Zod validation (0 schema errors)**

---

### Domain 9: Content Cross-References

**What**: Verify all links, headings, and cross-references are correct.

**Testable Claims**:
- Build heading map from H1, H2, H3, H4 markdown sections
- All internal links `[text](#anchor)` reference existing headings
- Source links `<!-- kgc:source-link index=X line=Y -->` reference valid `sources[X]`
- Receipt links `<!-- kgc:receipt-link id=HASH -->` reference `frontmatter.receipts`
- No heading level skips (H2 → H3 → H4, not H2 → H4)
- Heading IDs are unique (no duplicates)

**Example Test**:
```bash
$ kgc probe scan docs/api-reference.kgcmd --domain cross-refs
Building heading map...
  H1: User API Reference (#user-api-reference)
  H2: Overview (#overview)
  H2: Data Extraction (#data-extraction)
  H2: API Functions (#api-functions)
  H3: `createUser(userData)` (#createuseruserdata)
  H3: `getUserById(userId)` (#getuserbyiduserid)
  H3: `updateUser(userId, updates)` (#updateuseruserid-updates)
  H3: `deleteUser(userId)` (#deleteuseruserid)
  H2: Type Definitions (#type-definitions)
  H3: UserData (#userdata)
  H3: User (#user)
  H2: Receipt Verification (#receipt-verification)

Scanning markdown links...
  [Authentication](#authentication) -> ✅ Found (H2: Overview → can add section)
  [Diátaxis](#diataxis) -> ✅ Found (external link, skipped)
  [User API Reference](#user-api-reference) -> ✅ Found (H1)

Checking heading hierarchy:
  ✅ H1 → H2 (allowed)
  ✅ H2 → H2 (allowed, same level)
  ✅ H2 → H3 (allowed, next level)
  ✅ H3 → H2 (allowed, jump back)
  ✅ No level skips (H2 → H4 would be error)

Checking heading uniqueness:
  ✅ 11 unique headings (no duplicates)

Domain: cross-refs - PASS (all links valid, proper hierarchy)
```

**Success Criterion**: **All links resolvable, no broken references, proper hierarchy maintained**

---

### Domain 10: Error Detection & Reporting

**What**: Comprehensive error classification, location, and remediation guidance.

**Testable Claims**:
- Error types: InvalidFrontmatter, MissingReceipt, MismatchedHash, BoundsExceeded, NonDeterministic, InvalidBlockStructure, CyclicDependency, etc.
- Error location: file path, line number, field name pinpointed
- Severity levels: Error (blocks processing), Warning (caution but non-blocking)
- Error aggregation: collect all errors before reporting (show complete picture)
- Error output: structured JSON with path, message, remediation steps
- False positive rate: <1% (don't report non-issues)
- False negative rate: <1% (detect 99%+ of actual violations)

**Example Test**:
```bash
$ kgc probe scan docs/broken.kgcmd --domain errors
[Error] InvalidFrontmatter (line 2)
  Field: o_hash
  Constraint: must be 64 lowercase hex characters
  Value: "a3f5b8c2d1e4f6a7" (too short, 17 chars)
  Remediation: Use sha256 hash of universe snapshot, e.g., $(sha256sum universe.json | cut -d' ' -f1)

[Error] MissingReceipt (line 45)
  Block: kgc:query
  Receipt ID: b4c5d6e7f8a9... (referenced in block)
  Location: Not found in frontmatter.receipts array
  Remediation: Add receipt ID to frontmatter receipts array, or generate receipt via 'kgc probe generate'

[Warning] NonDeterministic (line 102)
  Block: kgc:extract
  Issue: determinismLevel='strict' but results not sorted
  Receipt: outputs 4 items in non-canonical order
  Remediation: Set determinismLevel='lenient', or sort extraction results by file path

[Error] MismatchedHash (line 209)
  Section: api-functions (dynamic)
  Expected hash: e7f8a9b0... (from receipt.output_hash)
  Computed hash: f8a9b0c1... (from section content)
  Issue: Section was manually edited after receipt generation
  Remediation: Regenerate receipt with 'kgc probe generate', or restore from receipt output

Summary:
  Errors: 3 (block processing)
  Warnings: 1 (not blocking)
  Status: FAILED (fix errors to proceed)
```

**Success Criterion**: **Errors specific + actionable; no spurious reports; 99%+ detection rate**

---

## Test Checkpoints

### Checkpoint 1: Parser Correctness (Sprint 1)

```bash
# Test frontmatter parsing
timeout 5s npm test -- test/probe/frontmatter.test.mjs
# Expected: ✅ 25 tests PASS

# Test block parsing
timeout 5s npm test -- test/probe/blocks.test.mjs
# Expected: ✅ 30 tests PASS

# Overall: >80% coverage
npx c8 --reporter=text npm test
# Expected: Statements: 80%+, Branches: 75%+, Functions: 80%+, Lines: 80%+
```

### Checkpoint 2: Receipt Verification (Sprint 2)

```bash
# Test receipt loading + hashing
timeout 10s npm test -- test/probe/receipts.test.mjs
# Expected: ✅ 20 tests PASS

# Test DAG validation
timeout 10s npm test -- test/probe/dag.test.mjs
# Expected: ✅ 15 tests PASS, 0 cycles, proper ordering

# Integration: full document scan
timeout 5s npm test -- test/probe/integration.test.mjs
# Expected: ✅ All domains PASS for 10 test documents
```

### Checkpoint 3: Determinism & Performance (Sprint 3)

```bash
# Test reproducibility
npm run benchmark -- probe.scan.reproducibility
# Expected: Hash(scan_T0) === Hash(scan_T1) for 10 runs

# Test latency
npm run benchmark -- probe.scan.latency
# Expected: p50 < 50ms, p99 < 100ms for typical doc

# Test error accuracy
timeout 30s npm test -- test/probe/error-accuracy.test.mjs
# Expected: <1% FP, <1% FN across 100-doc corpus

# Overall: >90% coverage, all domains PASS
npx c8 --reporter=html npm test
# Review coverage/index.html
```

---

## Guard Policy Summary

### Forbidden Operations

| Operation | Reason | Enforcement |
|-----------|--------|------------|
| Execute SPARQL queries | Scan-only verification; prevent loops, DoS | Parse syntax only, do NOT execute |
| Network I/O | Prevent DNS attacks, external fetches | Block http, https, fetch, net modules |
| File write | Maintain scan-only property, prevent tampering | Block fs.writeFile, fs.appendFile |
| Child process | Prevent arbitrary code execution | Block child_process, execSync, spawn |
| Out-of-bounds file access | Enforce path containment, no directory traversal | Guard: source.path must not contain `..` |
| State mutation | Ensure idempotence, reproducibility | Pure functions only, no global state |
| Non-deterministic operations | Proof reproducibility requirement | No Math.random(), Date.now(), UUID in output |
| Eval / dynamic code | Prevent injection attacks | Block eval(), Function(), vm.runInNewContext() |
| Dynamic require | Prevent loading untrusted modules | Only static requires, block dynamic strings |
| Receipt tampering | Proof immutability | Receipts read-only; mismatch = verification failure |

### Resource Limits

| Resource | Limit | Enforcement |
|----------|-------|------------|
| Memory | 500MB per document scan | Monitor heap; abort if exceeded |
| File I/O | 1GB document + 100 receipts | Check sizes before read |
| Parsing | YAML <10KB, JSON blocks <1MB | Error on oversized payloads |
| Receipt chain | Max 1000 receipts, max 10-level depth | Reject cycles + deep chains |
| Concurrency | Single-threaded, 1 document per instance | Sequential processing |

---

## Success Metrics

### Metric 1: Document Coverage (100%)
**Definition**: Probe identifies all sections, blocks, and receipts without missing any.

**Verification**:
```bash
kgc probe scan docs/api-reference.kgcmd
# Output includes:
# - 2 dynamic sections identified
# - 2 receipts scanned
# - 0 orphaned sections, 0 unmapped receipts
# Coverage: 100%
```

### Metric 2: Error Accuracy (<1% FP/FN)
**Definition**: Probe reports real violations only; detects all actual violations.

**Verification**:
```bash
npm test -- test/probe/error-accuracy.test.mjs --corpus 100
# Results: TP=1245, FP=2, FN=3
# Precision: 99.8%, Recall: 99.8%
```

### Metric 3: Latency (<100ms)
**Definition**: Time to complete full document + receipt verification.

**Verification**:
```bash
npm run benchmark -- probe.scan.latency
# p50: 45ms, p95: 85ms, p99: 98ms
# Max: 120ms (outlier, acceptable)
```

### Metric 4: Reproducibility (100%)
**Definition**: `scan(doc) at T0 === scan(doc) at T1`; byte-for-byte identical.

**Verification**:
```bash
for i in 1 2 3 4 5; do
  kgc probe scan doc.kgcmd > report-$i.json
  sha256sum report-$i.json
done
# All 5 hashes identical ✓
```

### Metric 5: Schema Coverage (100%)
**Definition**: All Zod schema constraints enforced + tested.

**Verification**:
```bash
npx c8 --reporter=text npm test -- test/probe/schema.test.mjs
# Coverage: 100% (all rules tested)
```

---

## Implementation Checklist

### Pre-Implementation
- [ ] Review KGC Markdown specification (docs/KGC-MARKDOWN-SPECIFICATION.md)
- [ ] Review this specification (docs/KGC-PROBE-SPECIFICATION.md)
- [ ] Understand Zod schema validation patterns (from @unrdf/core)
- [ ] Set up test infrastructure (Jest, c8 coverage)

### MVP Phase (Week 1)
- [ ] Implement `probe.frontmatter.mjs`: YAML parsing + Zod validation
- [ ] Implement `probe.blocks.mjs`: Block parsing + metadata validation
- [ ] Write 10 unit tests (frontmatter + blocks)
- [ ] Create CLI: `kgc probe scan document.kgcmd`
- [ ] Test on 10 representative .kgcmd files

### Core Phase (Week 2)
- [ ] Implement `probe.receipts.mjs`: Receipt loading + hash verification
- [ ] Implement `probe.dag.mjs`: Receipt DAG validation (cycles, ordering)
- [ ] Implement `probe.dynamic.mjs`: Dynamic section mapping + hashing
- [ ] Implement `probe.determinism.mjs`: Determinism level checking
- [ ] Implement `probe.bounds.mjs`: Resource bounds verification
- [ ] Write 40+ additional tests
- [ ] Achieve >90% code coverage

### Polish Phase (Week 3)
- [ ] Implement `probe.cross-refs.mjs`: Link integrity checking
- [ ] Implement `probe.error-reporting.mjs`: Comprehensive error messages
- [ ] Implement `probe.schema.mjs`: Full Zod schema validation
- [ ] Write 30+ more tests (error cases + edge cases)
- [ ] Performance optimization + benchmarking
- [ ] Documentation + examples
- [ ] Achieve 100% acceptance criteria PASS

---

## Conclusion

The KGC Probe specification defines a **deterministic, verifiable document scanner** that proves KGC Markdown integrity through:

1. **Cryptographic hashing** (SHA-256)
2. **Schema validation** (Zod)
3. **Receipt verification** (cryptographic proofs)
4. **Dependency analysis** (DAG traversal)
5. **Determinism checking** (ordering, canonicalization)
6. **Resource bounds** (limits enforcement)

**Success = 100% acceptance criteria PASS + 0 false positives + <100ms latency**

---

**Document**: KGC-PROBE-SPECIFICATION.md
**Status**: Implementation-Ready
**Next Step**: Begin Sprint 1 (parser correctness)
