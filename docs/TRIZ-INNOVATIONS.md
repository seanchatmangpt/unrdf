# TRIZ Innovation Analysis: UNRDF Init Pipeline

Using TRIZ principles to identify 10 innovations leveraging existing capabilities.

## TRIZ Principles Applied

**Principle 1: Segmentation** - Break the system into specialized sub-problems
**Principle 10: Prior Action** - Pre-act using existing ontology data
**Principle 6: Universality** - Reuse patterns across all features
**Principle 13: Inversion** - Use reverse processes (drift detection → code generation)
**Principle 3: Local Quality** - Specialized rules per framework/pattern

---

## 10 Innovations Identified

### Innovation 1: Predictive Gap Finder
**Problem:** Developers don't know what to build next
**TRIZ Principle:** Segmentation + Prior Action
**Solution:** Use domain model + drift detection to predict missing files

- Analyze domain entities not yet implemented
- Check which roles (View, Api, Test, Doc) are missing
- Suggest "You have Product entity but no ProductAPI"

**Existing Capability Used:**
- `inferDomainModel()` - Extract entities
- `classifyFiles()` - Know what roles exist
- `computeDrift()` - Detect gaps

**Implementation:** Gap analysis engine that queries ontology

---

### Innovation 2: Auto-Test Generator
**Problem:** Features lack tests (82% coverage is average)
**TRIZ Principle:** Universality + Segmentation
**Solution:** Generate test skeletons from domain + existing tests

- Analyze existing test patterns (template inference)
- Identify which domain entities lack tests
- Generate test file with proper imports + structure

**Existing Capability Used:**
- `inferTemplatesFromProject()` - Learn test patterns
- `inferDomainModel()` - Know which entities exist
- `planMaterialization()` - Plan test file creation

**Implementation:** Template-based test generation

---

### Innovation 3: Documentation Drift Checker
**Problem:** Docs get stale (APIs change, features get renamed)
**TRIZ Principle:** Prior Action + Inversion
**Solution:** Use domain model to validate documentation consistency

- Extract entities/fields from domain model
- Scan markdown docs for references
- Flag outdated entity names, missing fields, renamed features

**Existing Capability Used:**
- `inferDomainModel()` - Source of truth
- `computeDrift()` - Detect misalignment
- RDF queries - Link docs to entities

**Implementation:** Doc-to-domain validator

---

### Innovation 4: Type-Safety Auditor
**Problem:** Zod schemas and TS types drift apart
**TRIZ Principle:** Segmentation + Local Quality
**Solution:** Compare inferred domain model with current implementation

- Parse both Zod and TS types
- Compare field names, types, optionality
- Report: "Zod has 'email' optional, TS has required"

**Existing Capability Used:**
- `inferDomainModel()` - Extract both types
- RDF comparison - Detect differences

**Implementation:** Cross-type validation

---

### Innovation 5: Feature Dependency Graph
**Problem:** Building features in wrong order (B depends on A, but A not done)
**TRIZ Principle:** Universality + Segmentation
**Solution:** Extract dependencies from domain model and templates

- Domain relationships (User → Order → Product)
- API dependencies (POST /orders requires GET /products)
- Service imports

**Existing Capability Used:**
- `inferDomainModel()` - Get relationships
- RDF SPARQL - Query dependency chains

**Implementation:** Dependency analyzer + topological sort

---

### Innovation 6: API Contract Validator
**Problem:** Frontend/backend contracts break (API changed, frontend not updated)
**TRIZ Principle:** Prior Action
**Solution:** Validate API requests/responses against inferred schema

- Extract domain fields from entities
- Generate request/response schemas
- Validate actual API files match schema

**Existing Capability Used:**
- `inferDomainModel()` - Entity schemas
- `classifyFiles()` - Find API files
- Schema comparison

**Implementation:** Contract-driven validation

---

### Innovation 7: Stack-Aware Linter Rules
**Problem:** Same code patterns work differently across frameworks
**TRIZ Principle:** Local Quality
**Solution:** Derive linting rules from stack profile + patterns

- Detect stack (Next.js, Express, etc.)
- Generate framework-specific rules
- E.g., "Next.js API routes must return JSON"

**Existing Capability Used:**
- `detectStackFromFs()` - Know the stack
- `inferTemplatesFromProject()` - Learn patterns
- Hook derivation - Create rules

**Implementation:** Framework-aware rule generator

---

### Innovation 8: Hotspot Analyzer
**Problem:** Don't know which features cause the most bugs/complexity
**TRIZ Principle:** Segmentation + Prior Action
**Solution:** Score features by complexity + change frequency + test coverage

- File count + line count per feature
- Test coverage per feature
- Number of dependencies

**Existing Capability Used:**
- `buildProjectReport()` - Feature analysis
- `classifyFiles()` - Count by role
- Snapshot diffs - Track changes

**Implementation:** Feature complexity scorer

---

### Innovation 9: Automated Refactoring Guide
**Problem:** Refactoring is risky; hard to know if you broke anything
**TRIZ Principle:** Inversion + Prior Action
**Solution:** Plan refactoring using domain model as guide

- Rename entity (e.g., `User` → `Account`)
- Auto-generate refactoring plan:
  - Files to rename/update
  - Imports to fix
  - Tests to update

**Existing Capability Used:**
- `planMaterialization()` - Generate file plans
- Domain relationships - Track dependencies
- Drift detection - Verify after refactor

**Implementation:** Refactoring orchestrator

---

### Innovation 10: Generative Documentation
**Problem:** Docs are written once, become outdated immediately
**TRIZ Principle:** Universality + Segmentation
**Solution:** Generate docs from domain model + code patterns

- Generate entity reference from domain model
- Generate API reference from templates + files
- Generate architecture diagram from features
- Auto-update on domain changes

**Existing Capability Used:**
- `inferDomainModel()` - Entities + fields
- `buildProjectReport()` - Feature structure
- RDF → markdown transformation

**Implementation:** Doc generator from ontology

---

## Implementation Strategy: Chicago School TDD

**Why Chicago School?**
- Test-first with real collaborators (domain model, templates, drift detection)
- Write narrow, focused tests
- Mockable interfaces at system boundaries
- 80/20: Essential tests only, skip edge cases

**Test Pattern:**
```javascript
// Test that uses REAL collaborators (not mocks)
test('gap-finder reports missing ProductAPI', async () => {
  // Real: Inferred domain model with Product entity
  const domainStore = await inferDomainModel(fsStore, stackProfile)

  // Real: Actual project files and their roles
  const projectStore = buildProjectModelFromFs({ fsStore })
  classifyFiles({ fsStore: projectStore })

  // Real: Existing APIs identified by templates
  const templates = inferTemplatesFromProject(projectStore, domainStore)

  // Test the collaborator interaction
  const gaps = findGaps(domainStore, projectStore, templates)

  expect(gaps).toContainEqual({
    entity: 'Product',
    missingRoles: ['Api'],
    suggestion: 'Generate ProductAPI'
  })
})
```

---

## Priority & Timeline

| Priority | Innovation | Complexity | Effort |
|----------|-----------|-----------|--------|
| 🔴 HIGH | 1. Predictive Gap Finder | Low | 1-2h |
| 🔴 HIGH | 2. Auto-Test Generator | Medium | 3-4h |
| 🟠 MED  | 3. Doc Drift Checker | Medium | 3-4h |
| 🟠 MED  | 4. Type-Safety Auditor | Low | 2-3h |
| 🟠 MED  | 5. Feature Dependency Graph | Medium | 3-4h |
| 🟡 LOW  | 6. API Contract Validator | High | 5-6h |
| 🟡 LOW  | 7. Stack-Aware Linter | High | 4-5h |
| 🟡 LOW  | 8. Hotspot Analyzer | Low | 2-3h |
| 🔵 BACKLOG | 9. Refactoring Guide | High | 6-8h |
| 🔵 BACKLOG | 10. Generative Documentation | High | 6-8h |

---

## TRIZ Contradiction Matrix

### Contradiction 1: Coverage vs Speed
**Problem:** More comprehensive analysis (coverage) → slower execution (speed)
**TRIZ Solution:** Principle 14 (Segmentation) - Analyze in phases
**Implementation:** Gap finder runs quick analysis first, detailed only on demand

### Contradiction 2: Flexibility vs Simplicity
**Problem:** Support many frameworks → complex code
**TRIZ Solution:** Principle 6 (Universality) - One pattern, multiple uses
**Implementation:** Template inference learns patterns from each framework

### Contradiction 3: Safety vs Speed
**Problem:** Safe refactoring requires validation → slow execution
**TRIZ Solution:** Principle 10 (Prior Action) - Plan before action
**Implementation:** Generate plan first, apply only when valid

---

## Expected Impact

**For Developers:**
- 🎯 Know what to build next (Gap Finder)
- 🧪 Automatic test scaffolding (Auto-Test Generator)
- 📚 Keep docs in sync (Doc Drift Checker)
- 🔍 Catch type mismatches early (Type Auditor)
- 🔗 Understand dependencies (Dependency Graph)
- 📋 Validate API contracts (Contract Validator)
- ⚡ Follow framework best practices (Stack-Aware Linter)
- 🔥 Focus on high-risk features (Hotspot Analyzer)
- 🔄 Refactor safely (Refactoring Guide)
- 📖 Auto-updated docs (Generative Documentation)

**For UNRDF:**
- Broader applicability (not just init)
- Higher value per project
- More reasons to keep using UNRDF
- Compound effects (gap finder → test generator → docs)

---

## Implementation Order

1. **Week 1:** Innovations 1, 4, 8 (quick wins, low complexity)
2. **Week 2:** Innovations 2, 3, 5 (medium complexity, high value)
3. **Week 3+:** Innovations 6, 7, 9, 10 (complex, backlog-eligible)

Each innovation follows Chicago School TDD:
- Write test using real collaborators
- Implement lean feature (80/20)
- No mocks except at system boundaries
- All tests pass in <2 seconds
