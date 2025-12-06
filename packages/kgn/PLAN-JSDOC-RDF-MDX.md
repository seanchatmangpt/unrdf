# JSDoc→RDF→MDX Static Site Generator - 80/20 Production Plan

## Executive Summary

**Goal**: Automatically generate Nextra MDX documentation from JSDoc comments using RDF as an intermediate representation.

**Value Proposition**: Single source of truth (code) → comprehensive documentation with semantic links, search, and API references.

**80/20 Focus**: Core 20% features that deliver 80% value:
1. JSDoc parsing from `.mjs` files
2. RDF graph representation of codebase structure
3. MDX generation for Nextra
4. Basic cross-referencing and navigation

## Architecture Overview

```
┌─────────────────┐
│  Source Code    │  ← .mjs files with JSDoc comments
│  (*.mjs, *.js)  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  JSDoc Parser   │  ← Extract docs, params, returns, examples
│  + AST Analysis │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  RDF Graph      │  ← Semantic representation
│  (File System + │     - Files as resources
│   API Structure)│     - Functions as entities
└────────┬────────┘     - Links/relationships
         │
         ▼
┌─────────────────┐
│  KGN Templates  │  ← @unrdf/kgn with RDF filters
│  (*.njk)        │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  MDX Files      │  ← packages/nextra/pages/*.mdx
│  (Nextra Docs)  │
└─────────────────┘
```

## Component Breakdown (80/20)

### 1. **JSDoc Parser** (20% effort, 80% value)

**Input**: Source files (`packages/**/*.mjs`)
**Output**: Structured data (JSON/RDF)

**Core Features**:
- ✅ Parse JSDoc comments using `comment-parser` or `jsdoc`
- ✅ Extract:
  - Function/class names
  - Parameters (`@param`)
  - Return types (`@returns`)
  - Descriptions
  - Examples (`@example`)
  - Module relationships (`@module`, `import`)
- ✅ AST analysis for:
  - Export detection
  - Function signatures
  - Type annotations (JSDoc + inline)

**Libraries**:
- `comment-parser` - Parse JSDoc blocks
- `@babel/parser` - AST parsing
- `@babel/traverse` - AST traversal

**Example Output**:
```json
{
  "file": "packages/kgn/src/filters/rdf.js",
  "exports": [
    {
      "name": "expand",
      "type": "function",
      "params": [
        {"name": "curie", "type": "string"},
        {"name": "prefixes", "type": "Object", "optional": true}
      ],
      "returns": {"type": "string"},
      "description": "Expand CURIE to full URI",
      "examples": ["expand('ex:Person', prefixes)"]
    }
  ]
}
```

---

### 2. **RDF Graph Builder** (20% effort, 80% value)

**Input**: Parsed JSDoc data
**Output**: RDF triples (Turtle/JSON-LD)

**RDF Vocabulary**:
```turtle
@prefix code: <http://unrdf.org/vocab/code#> .
@prefix fs: <http://unrdf.org/vocab/fs#> .
@prefix doc: <http://unrdf.org/vocab/doc#> .

# File System
fs:File a owl:Class .
fs:Directory a owl:Class .
fs:contains rdfs:domain fs:Directory ; rdfs:range fs:File .

# Code Structure
code:Module a owl:Class .
code:Function a owl:Class .
code:Parameter a owl:Class .
code:exports rdfs:domain code:Module ; rdfs:range code:Function .
code:param rdfs:domain code:Function ; rdfs:range code:Parameter .

# Documentation
doc:description rdfs:domain code:Function ; rdfs:range xsd:string .
doc:example rdfs:domain code:Function ; rdfs:range xsd:string .
```

**Example RDF**:
```turtle
@prefix ex: <http://example.org/> .
@prefix code: <http://unrdf.org/vocab/code#> .
@prefix fs: <http://unrdf.org/vocab/fs#> .

ex:packages/kgn/src/filters/rdf.js
  a fs:File , code:Module ;
  fs:path "packages/kgn/src/filters/rdf.js" ;
  code:exports ex:expand .

ex:expand
  a code:Function ;
  code:name "expand" ;
  code:param ex:expand_param_curie , ex:expand_param_prefixes ;
  code:returns "string" ;
  doc:description "Expand CURIE to full URI" ;
  doc:example "expand('ex:Person', prefixes)" .

ex:expand_param_curie
  a code:Parameter ;
  code:name "curie" ;
  code:type "string" .
```

**80/20 Focus**:
- ✅ File paths as URIs
- ✅ Function/class definitions
- ✅ Parameter relationships
- ✅ Cross-file imports
- ❌ Skip: Full type inference, complex OWL reasoning

---

### 3. **MDX Generator** (20% effort, 80% value)

**Input**: RDF graph
**Output**: MDX files for Nextra

**Template Strategy**:
Use @unrdf/kgn templates with RDF filters to generate MDX.

**Template Structure**:
```nunjucks
---
prefixes:
  code: "http://unrdf.org/vocab/code#"
  fs: "http://unrdf.org/vocab/fs#"
module: {
  uri: "ex:packages/kgn/src/filters/rdf.js",
  path: "packages/kgn/src/filters/rdf.js",
  exports: [...]
}
---
# {{ module.path }} API Reference

{% for func in module.exports %}
## `{{ func.name }}`

{{ func.description }}

**Signature:**
```typescript
{{ func.name }}({{ func.params | map('formatParam') | join(', ') }}): {{ func.returns }}
```

{% if func.params %}
**Parameters:**
{% for param in func.params %}
- `{{ param.name }}` (`{{ param.type }}`){{ ' - ' + param.description if param.description }}
{% endfor %}
{% endif %}

{% if func.returns %}
**Returns:** `{{ func.returns }}`
{% endif %}

{% if func.examples %}
**Examples:**
{% for example in func.examples %}
```javascript
{{ example }}
```
{% endfor %}
{% endif %}

---
{% endfor %}
```

**80/20 Focus**:
- ✅ Function documentation
- ✅ Parameter tables
- ✅ Examples
- ✅ Cross-references
- ❌ Skip: Complex diagrams, interactive demos

---

### 4. **File System Integration** (20% effort, 80% value)

**Nextra Structure**:
```
packages/nextra/
  pages/
    api/
      kgn/
        filters.mdx          ← Generated
        engine.mdx           ← Generated
        index.mdx            ← Generated (overview)
    index.mdx
    _meta.json             ← Generated navigation
```

**_meta.json Generation**:
```nunjucks
---
packages:
  - name: "kgn"
    modules:
      - {name: "filters", path: "src/filters/index.js"}
      - {name: "engine", path: "src/engine/index.js"}
---
{
  "api": {
    "title": "API Reference",
    "type": "page"
  },
{% for pkg in packages %}
  "api/{{ pkg.name }}": {
    "title": "{{ pkg.name }}",
    "type": "page"
  }{{ ',' if not loop.last }}
{% endfor %}
}
```

**80/20 Focus**:
- ✅ Auto-generate navigation
- ✅ Module index pages
- ✅ Package overview pages
- ❌ Skip: Custom themes, advanced layout

---

## 80/20 Implementation Plan

### Phase 1: Core Pipeline (Week 1)

**Goal**: End-to-end pipeline working with one module.

**Tasks**:
1. ✅ **JSDoc Parser**: Parse `packages/kgn/src/filters/rdf.js`
   - Extract all functions, params, returns
   - Output JSON
   - ~200 LOC

2. ✅ **RDF Builder**: Convert JSON → RDF
   - Define code vocabulary
   - Generate Turtle output
   - ~150 LOC

3. ✅ **MDX Template**: Create `filter-api.njk`
   - Use @unrdf/kgn
   - Render one module
   - ~100 LOC

4. ✅ **Integration**: Wire components
   - Run parser → RDF → template
   - Output `packages/nextra/pages/api/kgn/filters.mdx`
   - ~100 LOC

**Validation**: Manually review generated MDX, verify it renders in Nextra.

**Evidence**: One fully documented module with examples, params, cross-refs.

---

### Phase 2: Scale to Package (Week 2)

**Goal**: Document entire @unrdf/kgn package.

**Tasks**:
1. ✅ **Batch Processing**: Scan `packages/kgn/src/**/*.mjs`
   - Recursive directory traversal
   - Parse all modules
   - ~100 LOC

2. ✅ **Navigation Generation**: Create `_meta.json`
   - Template for sidebar
   - Auto-detect modules
   - ~50 LOC

3. ✅ **Index Pages**: Generate overview pages
   - Package index (`api/kgn/index.mdx`)
   - Module indexes
   - ~100 LOC

4. ✅ **Cross-References**: Link functions/modules
   - Detect imports
   - Generate MDX links
   - ~100 LOC

**Validation**: Full @unrdf/kgn package documented in Nextra.

**Evidence**: Navigation works, all modules documented, cross-refs functional.

---

### Phase 3: Multi-Package Support (Week 3)

**Goal**: Document all packages in workspace.

**Tasks**:
1. ✅ **Workspace Scanner**: Detect all packages
   - Read `pnpm-workspace.yaml`
   - Filter packages with docs
   - ~50 LOC

2. ✅ **Package-Level Templates**: Overview pages
   - Generate package README.mdx
   - Link to modules
   - ~100 LOC

3. ✅ **Search Integration**: Nextra search
   - Ensure generated MDX is searchable
   - Add metadata
   - ~50 LOC

4. ✅ **CI Integration**: Auto-generate on commit
   - GitHub Actions workflow
   - Generate → commit → deploy
   - ~100 LOC

**Validation**: All packages documented, search works, CI runs on push.

**Evidence**: Full workspace docs, automated generation, deployed to GitHub Pages.

---

## Technical Decisions (80/20)

### JSDoc Parser Choice

**Option 1**: `comment-parser` (lightweight)
- ✅ Simple, fast, minimal deps
- ✅ Parses JSDoc blocks only
- ❌ No AST analysis

**Option 2**: `@microsoft/tsdoc` (TypeScript-focused)
- ✅ Rich metadata
- ❌ TypeScript-centric, complex

**Decision**: Use `comment-parser` + `@babel/parser` for AST.
**Reasoning**: 80/20 - comment-parser handles docs, Babel handles exports.

---

### RDF Store

**Option 1**: In-memory (JSON/JS objects)
- ✅ Fast, simple
- ❌ No SPARQL queries

**Option 2**: @unrdf/oxigraph
- ✅ Full RDF store, SPARQL support
- ❌ Heavier, more setup

**Decision**: Start with in-memory JSON, migrate to Oxigraph in Phase 3 if needed.
**Reasoning**: 80/20 - JSON is sufficient for basic queries.

---

### Template Engine

**Decision**: Use @unrdf/kgn (already built).
**Reasoning**: Perfect fit - RDF data + templates → MDX.

---

## File Structure

```
packages/kgn/
  src/
    doc-generator/
      parser.mjs          ← JSDoc + AST parser
      rdf-builder.mjs     ← JSON → RDF conversion
      mdx-generator.mjs   ← RDF → MDX via templates
      scanner.mjs         ← Workspace/package scanner
      index.mjs           ← Main orchestrator
  templates/
    api/
      module.njk          ← Module API template
      function.njk        ← Function docs template
      package-index.njk   ← Package overview
      meta.njk            ← Navigation JSON
  test/
    doc-generator/
      parser.test.mjs     ← Test JSDoc parsing
      rdf-builder.test.mjs
      end-to-end.test.mjs ← Full pipeline test
```

---

## Success Metrics

### Phase 1 (Week 1)
- ✅ 1 module fully documented
- ✅ All functions have params/returns/examples
- ✅ MDX renders in Nextra

### Phase 2 (Week 2)
- ✅ @unrdf/kgn package 100% documented
- ✅ Navigation works
- ✅ Cross-references functional

### Phase 3 (Week 3)
- ✅ All workspace packages documented
- ✅ CI auto-generates on push
- ✅ Search works
- ✅ Deployed to GitHub Pages

---

## Example Workflow

```bash
# Step 1: Parse codebase
pnpm run docs:parse packages/kgn/src

# Step 2: Generate RDF
pnpm run docs:rdf packages/kgn

# Step 3: Generate MDX
pnpm run docs:mdx packages/kgn

# Step 4: Preview
cd packages/nextra
pnpm dev

# All-in-one
pnpm run docs:generate
```

---

## Comparison with Alternatives

### vs. TypeDoc
- TypeDoc: TypeScript-only, no RDF, limited customization
- Ours: JavaScript, RDF-powered, full template control

### vs. JSDoc CLI
- JSDoc: HTML output, dated UX
- Ours: MDX/Nextra, modern, searchable

### vs. Manual Docs
- Manual: Drift from code, maintenance burden
- Ours: Auto-generated, always in sync

---

## Next Steps (After 80/20)

**Enhancements** (20% effort, 20% value):
- Type inference from usage
- Dependency graphs
- Performance metrics
- Test coverage integration
- Interactive examples (live code)
- Automated PR comments with doc diffs

---

## Adversarial PM Questions

**Q: Did we RUN this?**
A: After implementation, YES. Every phase has validation with evidence.

**Q: Can we PROVE it works?**
A: Each phase has concrete deliverables - generated MDX files that render in Nextra.

**Q: What BREAKS if we're wrong?**
A: Parser fails → no docs. RDF invalid → bad structure. Templates wrong → ugly output.

**Q: What's the EVIDENCE?**
A: Working Nextra site with API docs, deployed to GitHub Pages, auto-updating.

---

## Summary

**80/20 Core**:
1. JSDoc parser → Structured data
2. RDF graph → Semantic representation
3. KGN templates → MDX generation
4. Nextra integration → Beautiful docs

**Timeline**: 3 weeks
**LOC**: ~1,500 lines
**Output**: Full workspace documentation, auto-generated, always up-to-date

**Key Insight**: Use RDF as intermediate representation to enable rich queries, cross-referencing, and semantic search.
