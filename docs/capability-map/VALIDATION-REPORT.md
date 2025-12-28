# Capability Map Validation Report

**Generated**: 2025-12-28
**Validator**: Automated quality check
**Scope**: All capability maps in `/docs/capability-map/`

---

## Executive Summary

**Total Maps Found**: 3 files (2 capability maps + 1 index)
**Maps Passing All Checks**: 0/2 (0%)
**Maps Passing Core Checks**: 2/2 (100%)
**Overall Quality Score**: 79%

**Status**: PARTIAL PASS - Core structure excellent, cross-references missing

---

## Detailed Results

### 1. core.md - @unrdf/core

| Criterion | Target | Actual | Status |
|-----------|--------|--------|--------|
| Line count | ≥100 | 418 | ✅ PASS |
| Required sections | 5 | 5 | ✅ PASS |
| File:line citations | ≥20 | 36 | ✅ PASS |
| Code blocks | Present | 20 | ✅ PASS |
| Cross-references | ≥1 | 0 | ❌ FAIL |
| Performance table | Present | Yes | ✅ PASS |

**Score**: 5/6 (83%)

**Strengths**:
- Comprehensive evidence: 36 file:line citations (180% of target)
- Well-structured: All 5 required sections present
- Rich examples: 10 code blocks with JavaScript examples
- Performance data: Complete table with O() complexity analysis
- Excellent documentation: Tutorial, How-To, and Explanation sections

**Issues**:
- No markdown links to related capability maps (e.g., oxigraph.md, hooks.md)
- Related Packages section lists packages but doesn't link to their maps

**Package References**: 19 mentions of @unrdf packages (good contextual integration)

---

### 2. oxigraph.md - @unrdf/oxigraph

| Criterion | Target | Actual | Status |
|-----------|--------|--------|--------|
| Line count | ≥100 | 426 | ✅ PASS |
| Required sections | 5 | 5 | ✅ PASS |
| File:line citations | ≥20 | 19 | ⚠️ CLOSE |
| Code blocks | Present | 20 | ✅ PASS |
| Cross-references | ≥1 | 0 | ❌ FAIL |
| Performance table | Present | Yes | ✅ PASS |

**Score**: 4.5/6 (75%)

**Strengths**:
- Excellent structure: All required sections present
- Good examples: 10 code blocks with practical usage
- Performance benchmarks: Detailed table with throughput metrics
- Nearly sufficient evidence: 19 citations (95% of target)
- Clear explanations: "Why Oxigraph is Fast" section provides excellent context

**Issues**:
- File:line citations: 19 (need 1 more for target of 20)
- No markdown links to related capability maps (e.g., core.md, federation.md)
- Related Packages section mentions packages but doesn't hyperlink to maps

**Package References**: 18 mentions of @unrdf packages (good integration)

**Recommendation**: Add 1 more file:line citation to reach target (e.g., for dump() method)

---

### 3. README.md - Index File

| Criterion | Target | Actual | Status |
|-----------|--------|--------|--------|
| Line count | ≥100 | 303 | ✅ PASS |
| Structure | Index | Index | ✅ N/A |
| Cross-references | Many | 26 | ✅ PASS |

**Score**: N/A (index file, different criteria)

**Purpose**: Navigation and overview (not a capability map)

**Strengths**:
- Excellent navigation: 26 markdown links to other documents
- Comprehensive index: All 55 packages listed
- Clear categorization: 7 categories with descriptions
- Good completion tracking: Shows 2/55 maps complete

---

## Quality Metrics Summary

### By Category

| Category | core.md | oxigraph.md | Average |
|----------|---------|-------------|---------|
| Structure | 100% | 100% | 100% |
| Evidence | 100% | 95% | 97.5% |
| Examples | 100% | 100% | 100% |
| Performance | 100% | 100% | 100% |
| Cross-refs | 0% | 0% | 0% |
| **Overall** | **83%** | **75%** | **79%** |

### Evidence Density

| Map | Citations | Lines | Density |
|-----|-----------|-------|---------|
| core.md | 36 | 418 | 8.6% |
| oxigraph.md | 19 | 426 | 4.5% |

**Benchmark**: 5% citation density = strong evidence base
**Status**: core.md excellent (8.6%), oxigraph.md good (4.5%)

### Code Coverage

| Map | Code Blocks | Sections | Coverage |
|-----|-------------|----------|----------|
| core.md | 10 | 9 | 111% |
| oxigraph.md | 10 | 8 | 125% |

**All code blocks use JavaScript syntax** (✅ executable examples)

---

## Issues Found

### Critical (P0)

**None** - All core quality criteria met

### High Priority (P1)

1. **Missing Cross-References Between Maps**
   - **Affected**: core.md, oxigraph.md
   - **Impact**: Users cannot navigate between related capability maps
   - **Evidence**: 0 markdown links to .md files in both maps
   - **Expected**: Each map should link to 3-5 related maps
   - **Example**: core.md mentions @unrdf/oxigraph but doesn't link to oxigraph.md

2. **Citations Below Target (oxigraph.md)**
   - **Affected**: oxigraph.md
   - **Impact**: Slightly lower evidence density
   - **Actual**: 19 citations (target: 20)
   - **Gap**: 1 citation needed
   - **Severity**: Minor (95% of target met)

### Medium Priority (P2)

3. **Related Packages Not Hyperlinked**
   - **Affected**: core.md, oxigraph.md
   - **Impact**: Users must manually search for related package docs
   - **Current**: Lists package names without links
   - **Desired**: Markdown links to capability maps where available

### Low Priority (P3)

**None identified**

---

## Recommended Fixes

### Immediate Actions (P1)

#### Fix 1: Add Cross-References in core.md

**Location**: Line 399-404 (Related Packages section)

**Current**:
```markdown
- **@unrdf/oxigraph** - High-performance RDF store backend
- **@unrdf/hooks** - Add policy validation to operations
```

**Recommended**:
```markdown
- **[@unrdf/oxigraph](./oxigraph.md)** - High-performance RDF store backend
- **[@unrdf/hooks](./hooks.md)** - Add policy validation to operations (Coming Soon)
- **[@unrdf/streaming](./streaming.md)** - Real-time change feeds (Coming Soon)
- **[@unrdf/federation](./federation.md)** - Query across multiple stores (Coming Soon)
```

**Impact**: +4 cross-references, improves navigation

---

#### Fix 2: Add Cross-References in oxigraph.md

**Location**: Line 407-412 (Related Packages section)

**Current**:
```markdown
- **@unrdf/core** - High-level API over Oxigraph
- **@unrdf/hooks** - Add validation policies
```

**Recommended**:
```markdown
- **[@unrdf/core](./core.md)** - High-level API over Oxigraph
- **[@unrdf/hooks](./hooks.md)** - Add validation policies (Coming Soon)
- **[@unrdf/federation](./federation.md)** - Query across Oxigraph stores (Coming Soon)
- **[@unrdf/streaming](./streaming.md)** - Real-time change feeds (Coming Soon)
```

**Impact**: +4 cross-references, improves discoverability

---

#### Fix 3: Add 1 Citation to oxigraph.md

**Location**: Line 35 (dump method in Store Creation & Management table)

**Current**:
```markdown
| `store.dump(format, graph)` | Method | [src/store.mjs:177](file:///home/user/unrdf/packages/oxigraph/src/store.mjs#L177) | Node.js |
```

**Verify**: Ensure citation is accurate (file exists, line number correct)

**Alternative**: Add citation for removeMatches if dump citation doesn't exist

**Impact**: Reaches 20 citations target, 100% evidence criteria

---

### Quality Improvements (P2)

#### Enhancement 1: Add "See Also" Sections

Add to both core.md and oxigraph.md before References section:

```markdown
## See Also

**Related Capability Maps**:
- [@unrdf/core](./core.md) - RDF operations and SPARQL
- [@unrdf/oxigraph](./oxigraph.md) - High-performance store
- [@unrdf/hooks](./hooks.md) - Policy validation (Coming Soon)

**Synthesis Documents**:
- [Capability Basis](../synthesis/CAPABILITY-BASIS.md) - All 47 atoms
- [Composition Lattice](../synthesis/COMPOSITION-LATTICE.md) - Composition patterns
- [Integration Roadmap](../synthesis/INTEGRATION-ROADMAP-80-20.md) - Top 10 integrations
```

**Impact**: Improves discoverability, connects capability maps to synthesis docs

---

#### Enhancement 2: Validate Code Blocks Syntax

**Action**: Run JavaScript syntax validator on all code blocks

**Command**:
```bash
# Extract code blocks and validate
for file in core.md oxigraph.md; do
  awk '/```javascript/,/```/ {print}' $file > temp.js
  node --check temp.js 2>&1 | grep -v "SyntaxError" || echo "$file: PASS"
done
```

**Expected**: All code blocks should parse without syntax errors

**Note**: Basic validation shows code blocks use valid JavaScript (import statements, async/await, etc.)

---

## Validation Checklist

Use this checklist when creating new capability maps:

### Structure (Required)

- [ ] **Line count** ≥100 lines
- [ ] **Overview section** - Package purpose and key capabilities
- [ ] **Capability Atoms section** - Tables with file:line citations
- [ ] **Composition Patterns section** - At least 2 compositions
- [ ] **API Reference section** - Key functions documented
- [ ] **Examples section** - Tutorial or How-To guides
- [ ] **Performance Characteristics section** - Table with metrics

### Evidence (Required)

- [ ] **File:line citations** ≥20 (target: 5-10% of total lines)
- [ ] **Test evidence** - References to test files
- [ ] **Code blocks** - At least 5 executable examples
- [ ] **Benchmarks** - Performance data from measurements

### Integration (Required)

- [ ] **Cross-references** - Links to 3-5 related capability maps
- [ ] **Package mentions** - @unrdf/* packages contextualized
- [ ] **Synthesis links** - Links to CAPABILITY-BASIS.md, COMPOSITION-LATTICE.md

### Quality (Recommended)

- [ ] **Code syntax** - All blocks parse without errors
- [ ] **Link validation** - All .md links resolve correctly
- [ ] **Diataxis structure** - Tutorial, How-To, Explanation, Reference
- [ ] **Production readiness** - Error handling, observability, testing sections

---

## Next Steps

### For Existing Maps (core.md, oxigraph.md)

1. **Add cross-references** (5 minutes each)
   - Update Related Packages sections with markdown links
   - Add "See Also" sections before References

2. **Add 1 citation to oxigraph.md** (2 minutes)
   - Verify dump() method citation or add alternative

3. **Validate changes** (2 minutes)
   - Re-run this validation script
   - Confirm all links resolve

**Total effort**: ~15 minutes to reach 100% quality score

---

### For New Maps (Remaining 53 packages)

1. **Use template**: Follow [CAPABILITY-MAP-TEMPLATE.md](../templates/CAPABILITY-MAP-TEMPLATE.md)
2. **Copy patterns**: Reference core.md and oxigraph.md structure
3. **Extract evidence**: Use 10-agent analysis outputs as source
4. **Validate early**: Run validation after each section
5. **Cross-reference**: Add links as maps are created

**Estimated**: 2-3 hours per map (with agent assistance)

---

## Methodology Notes

### Validation Process

This report was generated using:

1. **Line counting**: `wc -l *.md`
2. **Section detection**: `grep "^## Section Name"`
3. **Citation counting**: `grep -c '\[.*\.mjs:[0-9]\+\]'`
4. **Code block counting**: `grep -c '^\`\`\`'`
5. **Cross-reference detection**: `grep '\]\(\.\/.*\.md\)'`
6. **Manual review**: Read full content for quality assessment

### Quality Criteria Rationale

- **Line count ≥100**: Ensures comprehensive coverage
- **Citations ≥20**: 5% density = strong evidence base
- **Required sections**: Diataxis framework + capability atoms
- **Code blocks**: Executable examples = practical value
- **Cross-references**: Navigation = usability
- **Performance table**: Production-ready = measurable characteristics

### Scoring Method

Each map scored on 6 criteria:
1. Line count (pass/fail)
2. Required sections (pass/fail)
3. File:line citations (pass/warn/fail)
4. Code blocks (pass/fail)
5. Cross-references (pass/fail)
6. Performance table (pass/fail)

**Overall score**: (passing criteria / 6) * 100%

---

## Conclusion

**Status**: STRONG FOUNDATION, MINOR IMPROVEMENTS NEEDED

The existing capability maps (core.md, oxigraph.md) demonstrate excellent quality:

✅ **Strengths**:
- Comprehensive structure (100%)
- Strong evidence base (97.5% avg)
- Rich examples (100%)
- Performance data (100%)

⚠️ **Improvement Areas**:
- Cross-references between maps (0% → target: 100%)
- oxigraph.md citations (95% → target: 100%)

**Recommendation**: Apply 3 recommended fixes (~15 minutes) to reach 100% quality score.

**Template Quality**: Both maps are excellent templates for remaining 53 packages.

---

**Validation Command**:
```bash
# Re-run validation
bash scripts/validate-capability-maps.sh

# Expected after fixes
# - Total maps: 2
# - Maps passing: 2/2 (100%)
# - Quality score: 100%
```

---

**Report Generated**: 2025-12-28
**Next Review**: After applying recommended fixes
**Validator Version**: 1.0.0
