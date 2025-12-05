# Adapting Diataxis: Pattern for Different Package Types

This guide shows how to adapt the @unrdf/core Diataxis pattern to different types of packages.

---

## Package Type Taxonomy

UNRDF packages fall into 3 categories with different documentation needs:

### Type 1: Foundation Packages (@unrdf/core)
- **Purpose:** Core functionality that most users start with
- **Characteristics:** Broadest audience, most basic to complex
- **Examples:** core, oxigraph

**Documentation approach:**
- Tutorials: "Hello world" to advanced
- How-To: General problems (optimize, troubleshoot, format)
- Reference: Comprehensive API, all options
- Explanation: Architecture, concepts, design

**Pattern:** START SIMPLE, BUILD UP

---

### Type 2: Feature Extension Packages (@unrdf/streaming, federation, knowledge-engine)
- **Purpose:** Advanced capabilities building on core
- **Characteristics:** Users already know core, need specific feature
- **Examples:** streaming, federation, knowledge-engine

**Documentation approach:**
- Tutorials: Problem-specific, use-case focused
- How-To: Solutions to feature-specific problems
- Reference: Feature's specific API
- Explanation: Feature architecture, design rationale

**Pattern:** PROBLEM-FOCUSED, NOT "HELLO WORLD"

---

### Type 3: Integration Packages (@unrdf/browser, react, composables, cli)
- **Purpose:** Integration with external system/framework
- **Characteristics:** Users know the external system, need to integrate UNRDF
- **Examples:** browser, react, cli, composables

**Documentation approach:**
- Tutorials: Integration patterns ("UNRDF + React")
- How-To: Integration-specific problems
- Reference: Integration API only (not underlying core)
- Explanation: Why this integration? Design choices

**Pattern:** FRAMEWORK-FIRST, UNRDF-SECOND

---

## Type 1: Foundation (Like @unrdf/core)

### Audience Assumption
- **New users:** Completely unfamiliar
- **Know:** Basic programming
- **Don't know:** RDF, SPARQL, semantic web

### Tutorial Strategy
**Tutorial 1: Hello World**
- Simplest possible example
- "Parse data and query it"
- One concept at a time
- 15-20 minutes

**Example:** "Your First SPARQL Query"
```javascript
const core = await createKnowledgeSubstrateCore();
const store = core.parseRdf(`
  @prefix ex: <http://example.org/> .
  ex:Alice ex:knows ex:Bob .
`);
const results = await core.query(store, 'SELECT * WHERE { ?s ?p ?o }');
```

**Tutorial 2: Common Workflow**
- Real-world patterns
- Multiple concepts together
- Slightly more complex
- 20-25 minutes

**Example:** "Querying Your Data"
- Parse multiple formats
- Write complex queries
- Handle results

**Tutorial 3: Advanced Topics**
- Power user patterns
- Optimization basics
- 25-30 minutes

**Example:** "Complex SPARQL Patterns"
- UNION, GROUP BY
- Aggregates
- Performance tips

### How-To Strategy
Focus on **problems users encounter**:
1. Performance ("My queries are slow")
2. Formats ("I have JSON-LD but need Turtle")
3. Troubleshooting ("No results returned")
4. Tuning ("I have 10 million triples")

### Reference Strategy
**Complete and comprehensive:**
- Every function documented
- Every option explained
- All error codes
- Version migration

### Explanation Strategy
Focus on **foundational understanding:**
- What is RDF? (conceptual)
- Why SPARQL? (alternatives)
- How queries work (execution model)
- Design philosophy

---

## Type 2: Feature Extension (Like @unrdf/streaming)

### Audience Assumption
- **Know:** @unrdf/core well
- **New to:** This specific feature
- **Problem:** Have a specific use case

### Tutorial Strategy
**Assume core knowledge**, jump to feature

**Tutorial 1: Problem Motivation**
- "I have 1 million triples"
- "Processing one file at a time"
- "Memory is 16GB, need to use it wisely"

**Example:** "Processing Large Graphs"
```javascript
// Without streaming: Load everything, then query
const store = await core.parseRdf(gigabytesOfData);
// Runs out of memory!

// With streaming: Load incrementally
const stream = await core.streamRdf(file);
for await (const quad of stream) {
  store.addQuad(quad);
  // Can process immediately
}
```

**Tutorial 2: Feature's Unique Capability**
- "Streaming + backpressure"
- "Handles slow consumers"
- "Real-world example"

**Example:** "Handling Backpressure"
- Producer sends data fast
- Consumer processes slowly
- Streaming handles mismatch
- Code example

**Tutorial 3: Real-World Use Case**
- Full end-to-end example
- Combines with other packages
- Production-ready pattern

**Example:** "Real-Time Data Sync"
- Stream new data as it arrives
- Merge with existing store
- Keep application updated

### How-To Strategy
**Feature-specific problems:**
1. "Optimize memory usage with streaming"
2. "Handle parser errors in stream"
3. "Batch process 1M triples"
4. "Monitor streaming performance"

### Reference Strategy
**Only the feature's API**, not core:
- Streaming functions (parseStream, etc.)
- Stream-specific options
- Stream-specific errors
- Stream-specific configuration

### Explanation Strategy
Focus on **feature design**:
- Why backpressure? (solves what problem?)
- Stream architecture (how does it work?)
- When to use vs when not to use
- Performance implications

---

## Type 3: Integration (Like @unrdf/react)

### Audience Assumption
- **Expert in:** React, Vue, JavaScript
- **New to:** UNRDF
- **Want:** To use UNRDF in my framework

### Tutorial Strategy
**Framework-centric, not feature-centric**

**Tutorial 1: "UNRDF + [Framework]"**
- "Add UNRDF to existing React app"
- Setup pattern first
- Query second

**Example:** "UNRDF in React"
```javascript
// React-first perspective
function MyComponent() {
  // React hook for UNRDF
  const { data, loading, error } = useRdfQuery(`
    SELECT ?name WHERE { ... }
  `);

  return <div>{data.map(...)}</div>;
}
```

NOT:
```javascript
// UNRDF-first perspective
const core = createKnowledgeSubstrateCore();
// Then in component...
```

**Tutorial 2: State Management**
- "Use UNRDF data in React state"
- Binding patterns
- Re-render optimization

**Tutorial 3: Common Pattern**
- Search with filters
- Form integration
- Real-world example

### How-To Strategy
**Framework integration problems:**
1. "Optimize re-renders"
2. "Handle errors in React"
3. "Debug with React DevTools"
4. "Use UNRDF in a form"

**Not:** "How to optimize SPARQL" (that's core's job)

### Reference Strategy
**Only the integration API:**
- useRdfQuery hook
- useRdfStore hook
- Configuration options for React
- React-specific errors

**Link to core API** for query-specific details

### Explanation Strategy
Focus on **integration design**:
- Why hooks? (not class component)
- Why this API shape?
- How does React binding work?
- When to use vs when to drop to core

---

## Comparison Table

| Aspect | Type 1: Foundation | Type 2: Feature | Type 3: Integration |
|--------|------------------|-----------------|-------------------|
| **Tutorial Hook** | "Hello world" | "Problem motivation" | "Framework pattern" |
| **Audience** | New to everything | Knows core | Expert in framework |
| **Tutorial 1** | Basic example | Feature problem | Framework setup |
| **Tutorial 2** | Workflow | Feature capability | Framework state |
| **Tutorial 3** | Advanced topic | Real-world combo | Common pattern |
| **How-To Focus** | General problems | Feature-specific | Integration problems |
| **Reference Scope** | Complete API | Feature API only | Integration API only |
| **Explanation Focus** | Concepts + design | Feature design | Integration design |
| **Code Complexity** | Simple → Complex | Assumes knowledge | Framework-idiomatic |
| **Typical Length** | 16 files, 25K words | 16 files, 20K words | 12-14 files, 15K words |
| **Example Domain** | Social network | Data processing | React components |

---

## Specific Adaptations by Package

### @unrdf/streaming (Feature Extension)

**Key difference from core:** Assume user knows core

**Tutorial 1: "Processing Large RDF Files"**
- NOT: "What is RDF?" (core already taught this)
- YES: "I have 1 billion triples, memory is 16GB"
- Code jumps straight to streaming syntax
- Compares to core approach

**How-To Guides:**
- Not "How to parse RDF" (core's job)
- Yes "How to handle backpressure" (streaming's unique)
- Yes "How to batch process" (streaming pattern)

**Reference:**
- Only streaming functions
- Streaming-specific options
- Link to core for general options

---

### @unrdf/federation (Feature Extension)

**Key difference:** Multi-store, distributed thinking

**Tutorial 1: "Your First Federated Query"**
- "Query 2 SPARQL endpoints simultaneously"
- Assume user knows SPARQL
- Focus on federation syntax

**How-To Guides:**
- "Query across different store types" (federation-specific)
- "Handle remote timeouts" (federation problem)
- "Optimize multi-store queries" (federation-specific)

**Reference:**
- Federation query builder API
- Store registration
- Federation-specific options
- Link to core for SPARQL details

---

### @unrdf/knowledge-engine (Feature Extension)

**Key difference:** Reasoning and inference (most conceptual)

**Tutorial 1: "Your First Inference Rule"**
- "Define a rule that derives new facts"
- Assume SPARQL knowledge
- Focus on rule syntax

**Explanation section (bigger than other packages):**
- Open world vs closed world
- Forward vs backward chaining
- Rule complexity and performance

**How-To Guides:**
- "Write efficient rules" (performance matters more here)
- "Debug infinite loops in reasoning"
- "Explain reasoning chains"

---

### @unrdf/react (Integration)

**Key difference:** React-first, UNRDF is secondary

**Tutorial 1: "Add UNRDF to Your React App"**
- Start with React component structure
- useRdfQuery is the hook
- Integrate UNRDF as data layer

**Example:**
```javascript
// React-centric example
export function PersonList() {
  const { data, loading } = useRdfQuery('SELECT ?name ...');

  if (loading) return <Loading />;
  return <ul>{data.map(p => <li>{p.name}</li>)}</ul>;
}

// Not UNRDF-centric (don't do this)
async function setupRDF() {
  const core = await createKnowledgeSubstrateCore();
  // ... lots of UNRDF setup
  // Then maybe think about React
}
```

**How-To Guides:**
- "Optimize React re-renders with useRdfQuery"
- "Validate forms using SHACL" (React + UNRDF integration)
- "Debug with React DevTools" (React-focused)

---

### @unrdf/cli (Tool/Integration)

**Key difference:** Not a library, it's a command-line tool

**Tutorial 1: "Query RDF Files from Terminal"**
- First use: \`npx @unrdf/cli query data.ttl --sparql "SELECT ..."\`
- Focus on CLI patterns, not code
- Real terminal workflows

**How-To Guides:**
- "Use in bash scripts"
- "Batch process many files"
- "Convert file formats"

**Reference:**
- Command reference (not API reference)
- Flags and options per command
- Example output
- Exit codes

---

## Red Flags: When Pattern Doesn't Fit

❌ **Foundation package without "hello world" tutorial**
- Users need entry point
- Even experts benefit from quick example

❌ **Feature package assuming no core knowledge**
- "What is RDF?" doesn't belong in streaming docs
- Link to core instead

❌ **Integration package teaching the integrated system**
- "Learn React" doesn't belong in react docs
- Assume React knowledge
- Only explain UNRDF integration

❌ **Tool documentation with API reference**
- CLI tools have command reference, not API
- Different structure needed

---

## Checklist: Package Type

Before writing docs, classify the package:

**Is this package the entry point for users?**
- YES → Type 1 (Foundation)
  - Include "hello world" tutorial
  - Comprehensive reference
  - Conceptual explanations

- NO → Ask next question

**Does this package extend core functionality?**
- YES → Type 2 (Feature)
  - Assume core knowledge
  - Start with problem/motivation
  - Feature-specific reference

- NO → Type 3 (Integration)
  - Assume external framework knowledge
  - Framework-centric examples
  - Integration API reference

---

## Conclusion

The same Diataxis structure (Tutorials, How-To, Reference, Explanation) works for all package types. **But the content changes based on audience and package purpose.**

- **Foundation:** "You're new, learn from ground up"
- **Feature:** "You know the foundation, here's something new"
- **Integration:** "You know this framework, here's how to use UNRDF"

The structure is the same. The content adapts.

---

**Next:** For each Phase 2 package, refer to its type and use this guide.
