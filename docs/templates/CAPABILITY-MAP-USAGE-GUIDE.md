# Capability Map Template Usage Guide

**Version**: 1.0.0
**Last Updated**: 2025-12-28
**Status**: Production Ready

---

## Overview

This guide explains how to use the Capability Map Template to create comprehensive, evidence-based documentation for @unrdf packages following the Diataxis framework.

**Template Location**: [CAPABILITY-MAP-TEMPLATE.md](./CAPABILITY-MAP-TEMPLATE.md)

**Purpose**: Every @unrdf package should have a capability map that:
- Documents ALL atomic capabilities with evidence
- Organizes documentation by Diataxis quadrants
- Provides concrete, runnable examples
- Shows runtime compatibility and composition patterns
- Enables progressive learning (beginner → expert)

---

## Quick Start

### 1. Generate Initial Scaffold

Use diataxis-kit to auto-generate the initial structure:

```bash
cd packages/diataxis-kit
DETERMINISTIC=1 pnpm run run

# Generated files appear in OUT/@unrdf/[your-package]/
ls -la OUT/@unrdf/your-package/
# tutorials/
# how-to/
# reference/
# explanation/
```

### 2. Copy Template

```bash
# Copy template to your package docs
cp docs/templates/CAPABILITY-MAP-TEMPLATE.md \
   packages/your-package/docs/CAPABILITY-MAP.md
```

### 3. Fill in Package-Specific Details

Replace all placeholders (marked with `[BRACKETS]`):
- `[PACKAGE-NAME]`: e.g., `oxigraph`, `kgc-4d`, `yawl`
- `[VERSION]`: Current package version from package.json
- `[DATE]`: Current date (YYYY-MM-DD)
- `[Runtime]`: Node.js | Browser | BEAM/WASM | Universal

### 4. Document Capabilities

For each capability atom:
1. Add entry to capability table
2. Link to source code (file:line)
3. Link to test proof
4. Add verification command

### 5. Create Diataxis Content

Follow the four-quadrant structure:
- **Tutorials**: Learning-oriented, step-by-step
- **How-To**: Problem-solving, task-oriented
- **Reference**: Information, dry and precise
- **Explanation**: Understanding, conceptual

### 6. Verify Evidence

Run verification commands:
```bash
# All tests must pass
timeout 5s pnpm test

# OTEL validation (if applicable)
timeout 5s node validation/run-all.mjs comprehensive

# Examples must run
timeout 5s node examples/01-basic.mjs
```

---

## Template Structure Explained

### Section 1: Overview

**Purpose**: 30-second summary for decision-makers

**Fill in**:
- Package purpose (2-3 sentences)
- Top 3-5 capabilities
- Import statement
- Dependencies (required + optional)
- Evidence metrics (test coverage, OTEL score)

**Example**:
```markdown
## Overview

Oxigraph-backed RDF store implementation for UNRDF. Provides SPARQL 1.1 query/update support with high performance via Rust WASM.

**Key Capabilities**:
- `createStore()`: Create Oxigraph RDF store
- `store.query()`: Execute SPARQL 1.1 queries
- `store.load()`: Load RDF from Turtle/JSON-LD/etc.
```

**Evidence Checklist**:
- [ ] Test coverage % from test reports
- [ ] Test file count: `find test -name "*.test.mjs" | wc -l`
- [ ] OTEL score from validation/run-all.mjs
- [ ] Example count: `find examples -name "*.mjs" | wc -l`

---

### Section 2: Capability Atoms

**Purpose**: Catalog ALL testable, composable building blocks

**Structure**: Three tiers (following 80/20 principle)
- **Tier 1 (Core)**: 20% of capabilities, 80% of usage
- **Tier 2 (Advanced)**: 15% usage, specialized needs
- **Tier 3 (Experimental)**: 5% usage, cutting edge

**Table Columns**:
- **Atom**: Function/class name with exact signature
- **Type**: Function | Class | Constant | Type
- **Runtime**: Node | Browser | BEAM/WASM | Universal
- **Evidence**: `file://` URL to source code with line number
- **Compositions**: Which composition patterns use this atom

**Example Entry**:
```markdown
| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `createStore(quads?)` | Function | Node, Browser | [src/index.mjs:9](file:///home/user/unrdf/packages/oxigraph/src/index.mjs#L9) | C1, C2, C3, C8 |
```

**How to Get Evidence**:
```bash
# Find function definition
grep -n "export.*createStore" packages/your-package/src/*.mjs

# Output: src/index.mjs:9:export function createStore(quads) {
# Use line 9 in evidence column
```

**Verification Command**:
Every capability group MUST include a verification command:
```bash
timeout 5s node /home/user/unrdf/packages/your-package/test/[test].test.mjs
```

**Run this to verify**:
```bash
# Test exists
test -f packages/your-package/test/capability.test.mjs && echo "✅" || echo "❌"

# Test passes
timeout 5s node packages/your-package/test/capability.test.mjs
echo $?  # Must be 0
```

---

### Section 3: Composition Patterns

**Purpose**: Show how atoms combine to solve real problems

**Format**:
```markdown
**C1**: [Pattern Name] - [Brief description]
```javascript
// Working code showing composition
import { atom1, atom2 } from '@unrdf/package';
const result = atom2(atom1(input));
```

**Example from CAPABILITY-BASIS.md**:
```markdown
**C2**: Time-Travel RDF - Combine RDF CRUD with event sourcing
```javascript
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { freezeUniverse, VectorClock } from '@unrdf/kgc-4d';

const store = createStore();
// ... add triples ...
const snapshot = freezeUniverse(store, new VectorClock());
```

**Guidelines**:
- Each composition should be a complete, runnable example
- Show 2-4 atoms combining
- Include comments explaining WHY this composition is useful
- Reference real use cases

---

### Section 4: Tutorials (📚 Learning-Oriented)

**Purpose**: Help beginners learn by DOING

**Characteristics**:
- ✅ Step-by-step instructions
- ✅ Assumes minimal prior knowledge
- ✅ Builds confidence through small wins
- ✅ Explains WHY as you go
- ❌ Not a reference (doesn't cover everything)
- ❌ Not exploratory (clear path to follow)

**Structure Template**:
```markdown
#### Tutorial 1: Getting Started with [Package Name]

**Goal**: [Concrete outcome - "Build a working X that does Y"]

**Prerequisites**:
- Node.js ≥ 18.0.0
- [Specific knowledge needed]

**Time**: [15-30 minutes]

**Steps**:
1. Installation
2. Basic Setup
3. First Operation
4. Verification

**Success Criteria**:
- [ ] Checklist of what should work
```

**Example Tutorial Outline**:
```markdown
#### Tutorial 1: Your First RDF Graph with Oxigraph

**Goal**: Create an RDF graph, add triples, and query them with SPARQL

**Prerequisites**:
- Node.js ≥ 18.0.0 installed
- Basic understanding of RDF (subject-predicate-object)

**Time**: 15 minutes

**Steps**:

1. **Install Package**
   ```bash
   pnpm add @unrdf/oxigraph
   ```

2. **Create Store**
   ```javascript
   import { createStore, dataFactory } from '@unrdf/oxigraph';

   const store = createStore();
   console.log('Store created!');
   ```

3. **Add Your First Triple**
   ```javascript
   const subject = dataFactory.namedNode('http://example.com/Alice');
   const predicate = dataFactory.namedNode('http://schema.org/name');
   const object = dataFactory.literal('Alice Smith');

   const triple = dataFactory.quad(subject, predicate, object);
   store.add(triple);

   console.log('Triple added!');
   ```

4. **Query with SPARQL**
   ```javascript
   const query = 'SELECT ?name WHERE { ?person <http://schema.org/name> ?name }';
   const results = store.query(query);

   console.log(results); // [{ name: 'Alice Smith' }]
   ```

**Success Criteria**:
- [ ] Store creates without errors
- [ ] Triple exists: `store.has(triple)` returns `true`
- [ ] SPARQL query returns Alice's name

**Next Steps**:
- 📚 Tutorial 2: Loading RDF from Files
- 🛠️ How-To: Query Complex Patterns
```

**Writing Guidelines**:
- Start with the SIMPLEST possible example
- Each step should produce visible output
- Include expected output/results
- Provide success criteria (checkboxes)
- Link to next learning steps

---

### Section 5: How-To Guides (🛠️ Task-Oriented)

**Purpose**: Solve SPECIFIC problems users face in real work

**Characteristics**:
- ✅ Problem-focused (not concept-focused)
- ✅ Assumes basic knowledge from tutorials
- ✅ Shows ONE way that works (not all possible ways)
- ✅ Practical, goal-oriented
- ❌ Not for learning fundamentals
- ❌ Not comprehensive (just solves the problem)

**Structure Template**:
```markdown
#### How-To: [Solve Specific Problem]

**Problem**: [Concrete problem statement]

**Context**: When you [scenario], you need to [solution]

**Prerequisites**:
- Completed [Tutorial X]
- Understanding of [concept]

**Solution**:
[Step-by-step solution with code]

**Verification**:
[How to confirm it works]

**Common Pitfalls**:
- ❌ Don't: [mistake]
- ✅ Do: [correct approach]

**See Also**:
- [Related guides]
```

**Example**:
```markdown
#### How-To: Load RDF Data from Multiple Formats

**Problem**: You have RDF data in Turtle, JSON-LD, and N-Triples files and need to load them into a single store.

**Context**: When integrating data from multiple sources, each may use different RDF serialization formats. You need to load all of them into one queryable store.

**Prerequisites**:
- Completed Tutorial 1 (basic store operations)
- Understanding of RDF serialization formats

**Solution**:

**Step 1: Create a Store**
```javascript
import { createStore } from '@unrdf/oxigraph';
const store = createStore();
```

**Step 2: Load Turtle File**
```javascript
const turtleData = `
  @prefix ex: <http://example.com/> .
  ex:Alice ex:name "Alice" .
`;

store.load(turtleData, {
  format: 'text/turtle',
  baseIri: 'http://example.com/'
});
```

**Why this works**: Oxigraph auto-detects Turtle syntax and parses it into the store's internal representation.

**Step 3: Load JSON-LD**
```javascript
const jsonldData = {
  "@context": { "name": "http://schema.org/name" },
  "@id": "http://example.com/Bob",
  "name": "Bob"
};

store.load(JSON.stringify(jsonldData), {
  format: 'application/ld+json'
});
```

**Step 4: Verify All Data Loaded**
```javascript
const query = 'SELECT ?s ?name WHERE { ?s ?p ?name }';
const results = store.query(query);
console.log(results);
// Expected: [
//   { s: 'http://example.com/Alice', name: 'Alice' },
//   { s: 'http://example.com/Bob', name: 'Bob' }
// ]
```

**Common Pitfalls**:
- ❌ **Don't**: Forget to specify `baseIri` for Turtle
  - **Problem**: Relative IRIs won't resolve correctly
- ✅ **Do**: Always provide `baseIri` when using relative IRIs

**Performance Considerations**:
- For files > 10MB, consider streaming with `@unrdf/streaming`
- For > 100K triples, use batch loading with transactions

**See Also**:
- 🛠️ How-To: Stream Large RDF Files
- 📖 Reference: store.load() API
- 💡 Explanation: RDF Serialization Formats
```

**Writing Guidelines**:
- Start with the problem, not the solution
- Provide ONE working solution (not all alternatives)
- Include verification step
- Document common mistakes
- Link to related resources

---

### Section 6: Reference (📖 Information-Oriented)

**Purpose**: Provide COMPLETE, ACCURATE technical information

**Characteristics**:
- ✅ Comprehensive coverage
- ✅ Dry, precise, factual
- ✅ Structured for lookup (not reading)
- ✅ Example per API
- ❌ Not for learning (use tutorials)
- ❌ Not for problem-solving (use how-tos)

**API Documentation Template**:
```markdown
##### `functionName(param1, param2, options?)`

**Type**: `(param1: Type1, param2: Type2, options?: Options) => ReturnType`

**Purpose**: [One-line what it does]

**Parameters**:
- `param1` (Type1, required): [Description, constraints]
- `param2` (Type2, required): [Description]
- `options` (Options, optional): Configuration
  - `option1` (string, default: 'default'): [Description]

**Returns**: `ReturnType` - [Description]

**Throws**:
- `ErrorType`: When [condition]

**Example**:
[Minimal working example]

**Runtime Compatibility**:
- ✅ Node.js ≥ 18.0.0
- ✅ Browser (ES2020+)

**Performance**:
- Time: O(n)
- Space: O(1)

**Source**: [file:line](file://...)
**Tests**: [test:line](file://...)
```

**How to Auto-Generate Reference**:
```bash
# Extract JSDoc from source
npx jsdoc2md packages/your-package/src/*.mjs > reference-draft.md

# Or use diataxis-kit reference extractor
node packages/diataxis-kit/src/reference-extractor.mjs \
  packages/your-package
```

**Writing Guidelines**:
- Be COMPLETE (document every parameter)
- Be PRECISE (no ambiguity)
- Be FACTUAL (no opinions)
- Include ONE minimal example per API
- Link to source and tests
- Document edge cases and errors

---

### Section 7: Explanation (💡 Understanding-Oriented)

**Purpose**: Deepen understanding of WHY and HOW

**Characteristics**:
- ✅ Conceptual, discursive
- ✅ Connects ideas and context
- ✅ Explores alternatives and trade-offs
- ✅ Answers "why" questions
- ❌ Not task-oriented
- ❌ Not comprehensive reference

**Topic Structure**:
```markdown
#### Why [Package Name] Exists

**The Problem**: [Domain challenge]

**The Solution**: [How this package solves it]

**Benefits**: [Why this approach]

**Trade-offs**: [What you give up]

| Aspect | Traditional | Our Approach | Rationale |
|--------|-------------|--------------|-----------|
```

**ADR (Architecture Decision Record) Template**:
```markdown
##### ADR-001: [Decision Title]

**Context**: [Situation requiring decision]

**Decision**: We chose to [decision].

**Rationale**:
- **Pro**: [Benefit 1]
- **Con**: [Trade-off 1] - Accepted because [reason]

**Alternatives Considered**:
1. **[Alternative]**: Rejected because [reason]

**Consequences**:
- Users must [implication]
- Performance impact: [impact]

**Evidence**: [Link to benchmark/test]
```

**Example ADR**:
```markdown
##### ADR-001: Why Oxigraph Instead of N3.js

**Context**: UNRDF needed a high-performance SPARQL engine. N3.js was the existing choice, but performance became a bottleneck for large datasets (>100K triples).

**Decision**: We migrated to Oxigraph as the primary RDF store implementation.

**Rationale**:
- **Pro**: 40% faster query execution (measured on LUBM benchmark)
- **Pro**: 60% less memory usage (measured on 1M triple dataset)
- **Pro**: Full SPARQL 1.1 support (N3 is partial)
- **Con**: Adds WASM dependency (~2MB) - Accepted because performance gains outweigh size
- **Con**: Breaking API change - Mitigated with v6-compat layer

**Alternatives Considered**:
1. **Stick with N3.js**: Rejected due to performance issues on large datasets
2. **Apache Jena via JVM**: Rejected due to runtime complexity
3. **Build custom engine**: Rejected due to maintenance burden

**Consequences**:
- Users must migrate from `new Store()` to `createStore()`
- Bundle size increases by 2MB (acceptable for web, minimal for Node.js)
- Performance improves 40% for SPARQL queries

**Evidence**:
- Benchmark: [packages/oxigraph/benchmark/results.md](file://...)
- Migration guide: [docs/migrations/n3-to-oxigraph.md](file://...)
```

**Writing Guidelines**:
- Focus on WHY, not HOW (leave HOW to how-tos)
- Discuss alternatives and trade-offs
- Use real data (benchmarks, measurements)
- Connect to broader concepts
- Be reflective, not prescriptive

---

## Integration with Diataxis-Kit

### Auto-Generation Workflow

```bash
# 1. Generate inventory
cd packages/diataxis-kit
pnpm run run

# 2. Review generated classification
cat ARTIFACTS/diataxis/@unrdf/your-package/diataxis.json

# 3. Copy generated scaffolds
cp -r OUT/@unrdf/your-package/* \
      packages/your-package/docs/

# 4. Fill in with template structure
# Use CAPABILITY-MAP-TEMPLATE.md as reference
```

### Confidence Scores

Diataxis-kit generates confidence scores (0-1) for each doc type:
- **0.0-0.3**: Low - Needs manual writing
- **0.4-0.6**: Medium - Has some evidence, needs expansion
- **0.7-1.0**: High - Good foundation, verify and polish

**How to improve confidence**:
- **Tutorials**: Add examples/ directory with working code
- **How-tos**: Add "Usage" and "Configuration" sections to README
- **Reference**: Use JSDoc extensively, export clear APIs
- **Explanation**: Add docs/ directory with conceptual content

### Evidence Collection

Diataxis-kit collects evidence from:
- `README.md`: Headings, content sections
- `examples/`: Code files
- `docs/`: Documentation files
- `src/`: Exported functions/classes
- `package.json`: Keywords, exports, bin entries

**Improve evidence quality**:
```javascript
// Add JSDoc to ALL exports
/**
 * Create a new Oxigraph RDF store
 * @param {Quad[]} [quads] - Initial quads to populate
 * @returns {OxigraphStore} Store instance
 * @example
 * const store = createStore();
 * store.add(quad);
 */
export function createStore(quads) { ... }
```

---

## Quality Checklist

Before publishing capability map documentation:

### Evidence Verification

- [ ] All capability atoms have source file:line references
- [ ] All capabilities have test file references
- [ ] All verification commands run and pass (< 5s)
- [ ] OTEL validation passes (≥80/100) if applicable
- [ ] All examples run without errors

**Run**:
```bash
# Verify all links point to real files
grep -o 'file://[^)]*' CAPABILITY-MAP.md | while read url; do
  file=$(echo $url | sed 's|file://||' | sed 's|#.*||')
  test -f "$file" && echo "✅ $file" || echo "❌ $file"
done

# Verify all test commands
grep 'timeout.*node' CAPABILITY-MAP.md | while read cmd; do
  eval "$cmd" && echo "✅ $cmd" || echo "❌ $cmd"
done
```

### Diataxis Compliance

- [ ] At least 1 tutorial (learning-oriented)
- [ ] At least 2 how-to guides (problem-oriented)
- [ ] Complete API reference (information-oriented)
- [ ] At least 1 explanation topic (understanding-oriented)
- [ ] Cross-links between quadrants (Related: sections)

### Content Quality

- [ ] All placeholders `[BRACKETS]` replaced
- [ ] No "TODO" or "TBD" markers
- [ ] All code examples are runnable
- [ ] All claims backed by evidence (links, benchmarks)
- [ ] Consistent formatting and terminology

### Accessibility

- [ ] Clear headings hierarchy (H2 → H3 → H4)
- [ ] Alt text for images/diagrams
- [ ] Code blocks have language specified
- [ ] Tables have headers
- [ ] Links have descriptive text (not "click here")

---

## Maintenance

### Regular Review Cycle

**Quarterly** (every 90 days):
- [ ] Update version numbers
- [ ] Re-run verification commands
- [ ] Check for new capabilities (new exports)
- [ ] Update benchmarks and metrics
- [ ] Review user feedback and FAQ

**On Breaking Changes**:
- [ ] Add migration guide section
- [ ] Update ADRs if design changed
- [ ] Update all affected examples
- [ ] Bump "Last Updated" date

**On New Features**:
- [ ] Add capability atom entries
- [ ] Add to composition patterns if applicable
- [ ] Write how-to guide for common use case
- [ ] Update runtime compatibility matrix
- [ ] Add example to examples/

### Automation

**Auto-update evidence**:
```bash
#!/bin/bash
# update-evidence.sh

# Re-run tests and capture metrics
pnpm test --coverage > test-output.log

# Extract coverage
coverage=$(grep "All files" test-output.log | awk '{print $10}')

# Update CAPABILITY-MAP.md
sed -i "s/Test Coverage: [0-9]*%/Test Coverage: $coverage/" \
  packages/your-package/docs/CAPABILITY-MAP.md

# Re-run OTEL validation
node validation/run-all.mjs > otel-output.log
score=$(grep "Score:" otel-output.log | awk '{print $2}')

# Update OTEL score
sed -i "s/OTEL Validation: [0-9]*/OTEL Validation: $score/" \
  packages/your-package/docs/CAPABILITY-MAP.md
```

---

## Examples

### Complete Capability Map Examples

**Reference Implementations**:
1. **@unrdf/oxigraph**: [packages/oxigraph/docs/CAPABILITY-MAP.md](file://../oxigraph/docs/)
   - Shows: Complete API reference, benchmarks, runtime matrix
2. **@unrdf/kgc-4d**: [packages/kgc-4d/docs/CAPABILITY-MAP.md](file://../kgc-4d/docs/)
   - Shows: Complex compositions, time-travel patterns
3. **@unrdf/yawl**: [packages/yawl/docs/CAPABILITY-MAP.md](file://../yawl/docs/)
   - Shows: Workflow patterns, extensive how-tos

**Study These For**:
- **Tutorial writing**: @unrdf/oxigraph tutorials/01-getting-started.md
- **How-to guides**: @unrdf/yawl how-to/ (8 guides)
- **API reference**: @unrdf/kgc-4d reference/kgc-store-api.md
- **Explanations**: @unrdf/yawl explanation/workflow-patterns.md

---

## Troubleshooting

### Common Issues

**Issue: Can't find source line numbers**

**Solution**:
```bash
# Search for export
grep -n "export.*yourFunction" packages/your-package/src/*.mjs

# Output: src/index.mjs:42:export function yourFunction() {
# Use line 42 in documentation
```

---

**Issue: Verification commands timing out**

**Solution**:
```bash
# Increase timeout if justified (not default)
timeout 10s node test.mjs  # For integration tests with setup

# Or fix root cause (slow test = code smell)
DEBUG=* node test.mjs  # Find slow operation
```

---

**Issue: Diataxis-kit confidence scores are low**

**Solution**:
```bash
# Check what evidence is missing
cat ARTIFACTS/diataxis/@unrdf/your-package/diataxis.json | \
  jq '.confidence'

# Add missing evidence:
# - Low tutorials? → Add examples/ directory
# - Low how-tos? → Add "Usage" section to README
# - Low reference? → Add JSDoc to exports
# - Low explanation? → Add docs/ conceptual content
```

---

## FAQ

**Q: Do I need to fill in EVERY section of the template?**

A: No. Use what's relevant for your package. Small utility packages may skip Explanations. Large framework packages should have all sections.

**Minimum**:
- Overview
- Capability Atoms (at least Tier 1)
- Reference (API docs)

**Recommended**:
- Add at least 1 Tutorial
- Add 2-3 How-To guides for common tasks

---

**Q: How do I handle capabilities that work differently across runtimes?**

A: Document the differences explicitly in the Runtime Compatibility Matrix and add notes:

```markdown
| Capability | Node.js | Browser | Notes |
|------------|---------|---------|-------|
| `freezeUniverse()` | ✅ Full | ⚠️ In-memory only | Browser lacks Git access |
```

---

**Q: Should I document internal/private functions?**

A: No. Only document exported APIs. Internal functions belong in code comments, not capability maps.

---

**Q: How do I version capability maps?**

A: Capability maps are versioned with the package. Update the version number at the top when you publish a new package version.

Breaking changes should add a Migration section.

---

## Resources

- **Diataxis Framework**: https://diataxis.fr/
- **Template**: [CAPABILITY-MAP-TEMPLATE.md](./CAPABILITY-MAP-TEMPLATE.md)
- **ADR Template**: [ADR-TEMPLATE.md](./ADR-TEMPLATE.md)
- **Evidence Standards**: [EVIDENCE-INDEX.md](../EVIDENCE-INDEX.md)

---

**Last Updated**: 2025-12-28
**Maintainer**: @unrdf/docs-team
**Feedback**: Open issue or PR in main repo
