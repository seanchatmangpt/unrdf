# CLI v2 Migration Status

**Last Updated:** 2025-10-01
**Tracking:** CLI v1 → CLI v2 Feature Migration
**Goal:** 100% feature parity + enhanced capabilities

---

## Migration Progress: 2.5%

### Overall Status

| Category | Total Features | Migrated | In Progress | Not Started | % Complete |
|----------|---------------|----------|-------------|-------------|------------|
| **RDF Operations** | 4 | 0 | 0 | 4 | 0% |
| **Hook Management** | 9 | 1 | 0 | 8 | 11% |
| **Query Operations** | 3 | 0 | 0 | 3 | 0% |
| **Validation** | 2 | 0 | 0 | 2 | 0% |
| **Utilities** | 5 | 0 | 0 | 5 | 0% |
| **Sidecar (New)** | 5 | 0 | 0 | 5 | 0% |
| **Context (New)** | 6 | 0 | 0 | 6 | 0% |
| **TOTAL** | **34** | **1** | **0** | **33** | **2.5%** |

---

## P0 Commands (Critical Path)

### 🔴 Priority 1: Core Execution (50% of usage)

| Command | CLI v1 | CLI v2 | Status | Lines | Test Coverage |
|---------|--------|--------|--------|-------|---------------|
| **hook eval** | ✅ Lines 702-731 | ⚠️ Stub (43 lines) | **NEEDS IMPLEMENTATION** | 0/200 | 0% |

**Implementation Plan:**
- File: `src/cli-v2/commands/hook/eval.mjs`
- Dependencies: KnowledgeHookManager, defineHook, useTurtle, useGraph
- Test: `test/cli-v2/commands/hook/eval.test.mjs`
- Estimated Effort: 4 hours
- Blocker: None

**Migration Notes:**
- CLI v1 uses direct file loading + hook executor
- CLI v2 needs KnowledgeHookManager integration
- Add OTEL telemetry
- Support JSON/YAML/table output formats

---

### 🔴 Priority 2: Query Execution (30% of usage)

| Command | CLI v1 | CLI v2 | Status | Lines | Test Coverage |
|---------|--------|--------|--------|-------|---------------|
| **query run** | ✅ Lines 161-246 | ❌ **MISSING** | **NEEDS CREATION** | 0/200 | 0% |

**Implementation Plan:**
- File: `src/cli-v2/commands/query/run.mjs` (NEW)
- Directory: `src/cli-v2/commands/query/` (CREATE)
- Dependencies: useGraph, useTurtle, formatOutput
- Test: `test/cli-v2/commands/query/run.test.mjs`
- Estimated Effort: 3 hours
- Blocker: Directory doesn't exist

**Migration Notes:**
- CLI v1 supports SELECT, ASK, CONSTRUCT
- Add query type detection
- Format results based on query type (SELECT → table, ASK → boolean, CONSTRUCT → Turtle)
- Add CSV output format support

---

### 🔴 Priority 3: RDF Parsing (20% of usage)

| Command | CLI v1 | CLI v2 | Status | Lines | Test Coverage |
|---------|--------|--------|--------|-------|---------------|
| **parse file** | ✅ Lines 89-158 | ❌ **MISSING** | **NEEDS CREATION** | 0/150 | 0% |

**Implementation Plan:**
- File: `src/cli-v2/commands/parse/file.mjs` (NEW)
- Directory: `src/cli-v2/commands/parse/` (CREATE)
- Dependencies: useTurtle, formatOutput
- Test: `test/cli-v2/commands/parse/file.test.mjs`
- Estimated Effort: 2 hours
- Blocker: Directory doesn't exist

**Migration Notes:**
- CLI v1 only supports Turtle
- CLI v2 should support Turtle, N-Triples, JSON-LD
- Add format auto-detection by file extension
- Add syntax validation option

---

### 🟡 Priority 4: Hook Creation

| Command | CLI v1 | CLI v2 | Status | Lines | Test Coverage |
|---------|--------|--------|--------|-------|---------------|
| **hook create** | ✅ Lines 733-752 | ⚠️ Basic (63 lines) | **NEEDS ENHANCEMENT** | 63/150 | 0% |

**Implementation Plan:**
- File: `src/cli-v2/commands/hook/create.mjs`
- Enhancement: Add template system, validation
- Dependencies: defineHook, schemas
- Test: `test/cli-v2/commands/hook/create.test.mjs`
- Estimated Effort: 3 hours
- Blocker: None

**Migration Notes:**
- CLI v1 has scaffold command with templates
- CLI v2 basic implementation exists
- Add template selection (compliance, health, drift)
- Add interactive prompt mode
- Generate SPARQL/SHACL files automatically

---

### 🟡 Priority 5: Hook Validation

| Command | CLI v1 | CLI v2 | Status | Lines | Test Coverage |
|---------|--------|--------|--------|-------|---------------|
| **hook validate** | ✅ Lines 765-779 | ❌ **MISSING** | **NEEDS CREATION** | 0/100 | 0% |

**Implementation Plan:**
- File: `src/cli-v2/commands/hook/validate.mjs` (NEW)
- Dependencies: defineHook, schemas, security-validator
- Test: `test/cli-v2/commands/hook/validate.test.mjs`
- Estimated Effort: 2 hours
- Blocker: None

**Migration Notes:**
- CLI v1 has basic validation
- CLI v2 should use comprehensive Zod validation
- Add security validation using SecurityValidator
- Report all validation errors with clear messages

---

### 🟡 Priority 6: Hook List (Partial)

| Command | CLI v1 | CLI v2 | Status | Lines | Test Coverage |
|---------|--------|--------|--------|-------|---------------|
| **hook list** | ✅ Lines 843-881 | ✅ Working (49 lines) | **NEEDS ENHANCEMENT** | 49/100 | 0% |

**Implementation Plan:**
- File: `src/cli-v2/commands/hook/list.mjs`
- Enhancement: Add filtering, sorting
- Dependencies: KnowledgeHookManager, formatOutput
- Test: `test/cli-v2/commands/hook/list.test.mjs`
- Estimated Effort: 2 hours
- Blocker: None

**Migration Notes:**
- CLI v1 has table output
- CLI v2 basic implementation exists
- Add filtering by policy pack
- Add sorting by name/date/type
- Support all output formats

---

### 🟡 Priority 7: Query Explain

| Command | CLI v1 | CLI v2 | Status | Lines | Test Coverage |
|---------|--------|--------|--------|-------|---------------|
| **query explain** | ❌ None | ❌ **MISSING** | **NEEDS CREATION** | 0/150 | 0% |

**Implementation Plan:**
- File: `src/cli-v2/commands/query/explain.mjs` (NEW)
- Directory: `src/cli-v2/commands/query/` (CREATE)
- Dependencies: useGraph, query-optimizer
- Test: `test/cli-v2/commands/query/explain.test.mjs`
- Estimated Effort: 4 hours
- Blocker: Directory doesn't exist

**Migration Notes:**
- New feature in CLI v2
- Show query execution plan
- Visualize query patterns
- Estimate query cost/performance

---

### 🟡 Priority 8: Parse Validate

| Command | CLI v1 | CLI v2 | Status | Lines | Test Coverage |
|---------|--------|--------|--------|-------|---------------|
| **parse validate** | ⚠️ Partial (validate cmd) | ❌ **MISSING** | **NEEDS CREATION** | 0/100 | 0% |

**Implementation Plan:**
- File: `src/cli-v2/commands/parse/validate.mjs` (NEW)
- Directory: `src/cli-v2/commands/parse/` (CREATE)
- Dependencies: useTurtle, useValidator
- Test: `test/cli-v2/commands/parse/validate.test.mjs`
- Estimated Effort: 2 hours
- Blocker: Directory doesn't exist

**Migration Notes:**
- CLI v1 validates RDF syntax + SHACL
- CLI v2 should separate syntax validation from SHACL
- Add format-specific validators
- Report parse errors with line numbers

---

## P1 Commands (High Priority)

### Hook Management

| Command | CLI v1 Status | CLI v2 Status | Priority |
|---------|--------------|---------------|----------|
| **hook get** | ✅ load command | ⚠️ Stub | P1 |
| **hook update** | ❌ None | ⚠️ Stub | P1 |
| **hook delete** | ✅ Lines 979-1004 | ⚠️ Stub | P1 |
| **hook history** | ✅ Lines 1007-1046 | ⚠️ Stub | P1 |
| **hook describe** | ❌ None | ⚠️ Stub | P1 |

### Graph Operations

| Command | CLI v1 Status | CLI v2 Status | Priority |
|---------|--------------|---------------|----------|
| **graph list** | ❌ None | ⚠️ Stub | P1 |
| **graph get** | ❌ None | ⚠️ Stub | P1 |
| **graph create** | ❌ None | ⚠️ Stub | P1 |
| **graph validate** | ✅ Lines 249-305 | ⚠️ Stub | P1 |
| **graph export** | ⚠️ convert cmd | ⚠️ Stub | P1 |

### Policy Management

| Command | CLI v1 Status | CLI v2 Status | Priority |
|---------|--------------|---------------|----------|
| **policy list** | ❌ None | ⚠️ Stub | P1 |
| **policy get** | ❌ None | ⚠️ Stub | P1 |
| **policy apply** | ❌ None | ⚠️ Stub | P1 |
| **policy validate** | ❌ None | ⚠️ Stub | P1 |
| **policy test** | ❌ None | ⚠️ Stub | P1 |

---

## P2 Commands (Medium Priority)

### Sidecar Operations (New in v2)

| Command | CLI v1 Status | CLI v2 Status | Priority |
|---------|--------------|---------------|----------|
| **sidecar status** | ❌ None | ⚠️ Stub (30 lines) | P2 |
| **sidecar health** | ❌ None | ⚠️ Stub | P2 |
| **sidecar config** | ❌ None | ⚠️ Stub | P2 |
| **sidecar logs** | ❌ None | ⚠️ Stub | P2 |
| **sidecar restart** | ❌ None | ⚠️ Stub | P2 |

**Note:** Sidecar commands require proto definitions (not yet created)

### Store Operations

| Command | CLI v1 Status | CLI v2 Status | Priority |
|---------|--------------|---------------|----------|
| **store import** | ⚠️ parse cmd | ⚠️ Stub | P2 |
| **store export** | ⚠️ convert cmd | ⚠️ Stub | P2 |
| **store query** | ⚠️ query cmd | ⚠️ Stub (50 lines) | P2 |
| **store stats** | ✅ Lines 518-540 | ⚠️ Stub | P2 |
| **store backup** | ❌ None | ⚠️ Stub | P2 |
| **store restore** | ❌ None | ⚠️ Stub | P2 |

### Context Management (New in v2)

| Command | CLI v1 Status | CLI v2 Status | Priority |
|---------|--------------|---------------|----------|
| **context list** | ❌ None | ⚠️ Stub | P2 |
| **context get** | ❌ None | ⚠️ Stub | P2 |
| **context use** | ❌ None | ⚠️ Stub | P2 |
| **context create** | ❌ None | ⚠️ Stub | P2 |
| **context delete** | ❌ None | ⚠️ Stub | P2 |
| **context current** | ❌ None | ⚠️ Stub | P2 |

---

## P3 Commands (Low Priority)

### Utilities (Not Yet Migrated)

| Command | CLI v1 Status | CLI v2 Status | Priority |
|---------|--------------|---------------|----------|
| **init** | ✅ Lines 378-459 | ❌ Missing | P3 |
| **id uuid** | ✅ Lines 468-478 | ❌ Missing | P3 |
| **id hash** | ✅ Lines 480-497 | ❌ Missing | P3 |
| **prefix list** | ✅ Lines 562-575 | ❌ Missing | P3 |
| **prefix expand** | ✅ Lines 577-594 | ❌ Missing | P3 |
| **prefix shrink** | ✅ Lines 596-614 | ❌ Missing | P3 |
| **delta** | ✅ Lines 618-693 | ❌ Missing | P3 |
| **convert** | ✅ Lines 308-375 | ❌ Missing | P3 |
| **scaffold hook** | ✅ Lines 1080-1214 | ❌ Missing | P3 |
| **scaffold policy** | ✅ Lines 1217-1305 | ❌ Missing | P3 |

---

## Implementation Roadmap

### Week 1: P0 Commands (8 commands)
**Goal:** Implement critical path commands

- **Day 1-2: Hook Commands**
  - [ ] Implement `hook/eval.mjs` with KnowledgeHookManager
  - [ ] Enhance `hook/create.mjs` with templates
  - [ ] Create `hook/validate.mjs` with Zod validation
  - [ ] Tests for hook commands

- **Day 3-4: Query Commands**
  - [ ] Create `commands/query/` directory
  - [ ] Implement `query/run.mjs` with useGraph
  - [ ] Implement `query/explain.mjs` with query optimizer
  - [ ] Tests for query commands

- **Day 5: Parse Commands**
  - [ ] Create `commands/parse/` directory
  - [ ] Implement `parse/file.mjs` with useTurtle
  - [ ] Implement `parse/validate.mjs` with syntax validation
  - [ ] Tests for parse commands

**Success Criteria:**
- ✅ 8 P0 commands fully implemented
- ✅ 80%+ test coverage
- ✅ 0 test failures
- ✅ 0 OTEL errors

---

### Week 2: P1 Commands (15 commands)
**Goal:** Complete hook, graph, and policy commands

- **Day 1-2: Hook Commands**
  - [ ] Implement `hook/get.mjs`, `hook/update.mjs`, `hook/delete.mjs`
  - [ ] Implement `hook/history.mjs`, `hook/describe.mjs`
  - [ ] Tests

- **Day 3-4: Graph Commands**
  - [ ] Implement `graph/list.mjs`, `graph/get.mjs`, `graph/create.mjs`
  - [ ] Implement `graph/validate.mjs`, `graph/export.mjs`
  - [ ] Tests

- **Day 5: Policy Commands**
  - [ ] Implement `policy/list.mjs`, `policy/get.mjs`, `policy/apply.mjs`
  - [ ] Implement `policy/validate.mjs`, `policy/test.mjs`
  - [ ] Tests

**Success Criteria:**
- ✅ 15 P1 commands fully implemented
- ✅ 80%+ test coverage
- ✅ Integration tests pass

---

### Week 3: P2 Commands (17 commands)
**Goal:** Complete sidecar, store, and context commands

- **Day 1-2: Sidecar Commands**
  - [ ] Create proto definitions
  - [ ] Implement all 5 sidecar commands
  - [ ] Tests with mock sidecar

- **Day 3-4: Store Commands**
  - [ ] Implement all 6 store commands
  - [ ] Tests

- **Day 5: Context Commands**
  - [ ] Implement all 6 context commands
  - [ ] Tests

**Success Criteria:**
- ✅ 17 P2 commands fully implemented
- ✅ Sidecar proto definitions complete
- ✅ Context management working

---

### Week 4: P3 Commands + Polish (10 commands)
**Goal:** Complete utility commands and final polish

- **Day 1-2: Utility Commands**
  - [ ] Implement init, id, prefix, delta commands
  - [ ] Tests

- **Day 3: Conversion & Scaffolding**
  - [ ] Implement convert, scaffold commands
  - [ ] Tests

- **Day 4-5: Documentation & Polish**
  - [ ] User guide
  - [ ] Migration guide
  - [ ] Final testing

**Success Criteria:**
- ✅ 100% command parity with CLI v1
- ✅ All new CLI v2 features working
- ✅ Documentation complete

---

## Validation Protocol

**CRITICAL: Agent Validation Protocol**

### Before Accepting Any Implementation

1. **Run Tests**
   ```bash
   npm run test:cli-v2
   # MUST show 0 failures
   ```

2. **Check OTEL Metrics**
   ```bash
   grep "Error recorded" test-output.log
   grep "FAIL" test-output.log
   # MUST be empty (no errors)
   ```

3. **Verify Coverage**
   ```bash
   npm run test:coverage
   # MUST show 80%+ for new code
   ```

4. **Manual Testing**
   ```bash
   # Test command
   unrdf <noun> <verb> [args]
   # Should execute successfully
   ```

### Acceptance Criteria

**Command is ONLY accepted when:**
- ✅ Tests pass (0 failures)
- ✅ OTEL shows no errors
- ✅ Coverage > 80%
- ✅ Manual testing passes
- ✅ Documentation complete

**DO NOT TRUST:**
- ❌ Agent reports of success
- ❌ "All tests passing" claims
- ❌ "Production ready" assertions

**ALWAYS VERIFY:**
- ✅ Run actual tests
- ✅ Check actual OTEL logs
- ✅ Read actual source code
- ✅ Execute actual commands

---

## Tracking Metrics

### Code Volume

| Metric | Current | Target | Progress |
|--------|---------|--------|----------|
| **Total Lines** | ~3,500 | 8,000 | 44% |
| **Working Code** | ~500 | 8,000 | 6% |
| **Stub Code** | ~3,000 | 0 | N/A |
| **Test Code** | 0 | 3,000 | 0% |

### Quality Metrics

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| **Test Coverage** | 0% | 80%+ | ❌ **CRITICAL** |
| **Working Commands** | 1/56 | 56/56 | ❌ **CRITICAL** |
| **Documentation** | 20% | 100% | ⚠️ **NEEDS WORK** |
| **OTEL Integration** | 0% | 100% | ❌ **CRITICAL** |

### Performance Targets

| Command | Target Latency | Current | Status |
|---------|---------------|---------|--------|
| `hook eval` | < 500ms | N/A | ⏳ Not implemented |
| `query run` | < 1s | N/A | ⏳ Not implemented |
| `parse file` | < 200ms | N/A | ⏳ Not implemented |

---

## Migration Notes

### CLI v1 → CLI v2 Mapping

**Command Syntax Changes:**
```bash
# CLI v1 (flat)
unrdf parse data.ttl
unrdf query data.ttl --query "SELECT * WHERE { ?s ?p ?o }"
unrdf hook eval hook.json --data data.ttl

# CLI v2 (noun-verb)
unrdf parse file data.ttl
unrdf query run --data data.ttl --query "SELECT * WHERE { ?s ?p ?o }"
unrdf hook eval hook.json --data data.ttl
```

**Configuration Changes:**
```javascript
// CLI v1: unrdf.config.mjs
export default {
  baseIRI: 'http://example.org/',
  prefixes: { ... }
}

// CLI v2: contexts in ~/.unrdf/config.json
{
  "contexts": [
    {
      "name": "default",
      "baseIRI": "http://example.org/",
      "prefixes": { ... }
    }
  ]
}
```

---

## Dependencies

### Required for P0 Implementation

- ✅ KnowledgeHookManager - Available
- ✅ defineHook - Available
- ✅ useGraph - Available
- ✅ useTurtle - Available
- ✅ useValidator - Available
- ✅ formatOutput - Available
- ✅ OTEL observability - Available

### Blocked Dependencies

- ⚠️ Sidecar proto definitions - **NEEDS CREATION**
- ⚠️ Context storage - **NEEDS IMPLEMENTATION**

---

## Risk Assessment

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| **Scope Creep** | High | Medium | Focus on P0 commands first |
| **Breaking Changes** | High | High | Provide v1 compatibility mode |
| **Timeline Slip** | Medium | Low | Weekly checkpoints |
| **Test Coverage** | High | Low | TDD approach, 80% minimum |
| **Performance Regression** | Medium | Low | Benchmarks before/after |

---

## Next Actions

**This Week:**
1. ✅ Complete code quality analysis (DONE)
2. ✅ Create migration status doc (DONE)
3. ⏳ Implement `hook/eval.mjs`
4. ⏳ Create `commands/query/` directory
5. ⏳ Implement `query/run.mjs`
6. ⏳ Create `commands/parse/` directory
7. ⏳ Implement `parse/file.mjs`
8. ⏳ Write tests for P0 commands

**Next Week:**
- Implement remaining P0 commands
- Achieve 80%+ test coverage
- Begin P1 command implementation

---

**Migration Status Complete**
**Ready for Implementation:** YES
**Blocking Issues:** NONE

*"Focus on P0. Tests are truth. Ship when validated."*
