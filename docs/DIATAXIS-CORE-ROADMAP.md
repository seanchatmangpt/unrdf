# @unrdf/core Diataxis Content Roadmap

**Package:** @unrdf/core (The essential, most-used package)
**Priority:** 1 (Start here)
**Expected completion:** Week 2 of Phase 1
**Effort:** 80-100 hours

This document specifies exactly what content should go in each documentation file for @unrdf/core.

---

## Overview

@unrdf/core is the foundation of UNRDF. Users need to understand:
- How to parse RDF data
- How to query with SPARQL
- How to validate with SHACL
- How to manage transactions
- How to extend with hooks

Diataxis structure serves these needs:

| Type | Purpose | Files |
|------|---------|-------|
| **Tutorials** | Learn core by doing | 3 files, ~60 min total |
| **How-To** | Solve specific problems | 4 files, ~30 min total |
| **Reference** | Look up details | 5 files, comprehensive |
| **Explanation** | Understand concepts | 4 files, ~60 min total |

---

## 1. TUTORIALS (3 files)

Each tutorial teaches a concrete skill through practical examples.

### Tutorial 1: Getting Started (01-getting-started.md)

**Goal:** "In 15 minutes, I'll understand RDF basics and execute my first SPARQL query"

**Learning outcomes:**
- Know what RDF is (high-level)
- Understand triples concept
- Parse RDF from a string
- Execute a basic SPARQL SELECT query
- Read and iterate results

**Content structure:**

1. **Overview** (50 words)
   - "This tutorial teaches you RDF basics and SPARQL"
   - "By the end, you'll query a social network"

2. **What You'll Learn** (50 words, bullet list)
   - Parse RDF data in Turtle format
   - Store triples in a knowledge base
   - Write and execute SPARQL queries
   - Read query results in JavaScript

3. **What You'll Build** (50 words)
   - Simple social network (Alice, Bob, Charlie)
   - Who knows whom relationships
   - Query to find friends

4. **Prerequisites** (bullet list)
   - Node.js 18+
   - Basic JavaScript
   - 15 minutes
   - Text editor

5. **Before You Start** (2-3 code blocks)
   - \`npm init\`
   - \`npm install @unrdf/core\`
   - Verify installation

6. **Step-by-Step** (8-10 steps with code)
   - Create knowledge-base.mjs
   - Define RDF data (Turtle format)
   - Parse into store
   - Verify with console.log
   - Write SELECT query
   - Execute query
   - Iterate results
   - Add more data
   - Write relationship query

7. **Verify It Works** (2-3 code blocks)
   - Expected output shown
   - How to test
   - What success looks like

8. **What You've Learned** (Checkmarks)
   - ✅ Parsed RDF data
   - ✅ Executed SPARQL
   - ✅ Read results
   - ✅ Added data

9. **Next Steps** (3 links)
   - → Tutorial 2: Basic Workflow
   - → How-To: Common Problems
   - → API Reference: Full details

**Code examples needed:**
- Parse Turtle
- Create store
- SELECT query (with prefixes)
- Results iteration
- Basic addQuad

**Key concepts to introduce (but not deeply explain):**
- Triple (subject-predicate-object)
- URI vs literal
- Prefix shorthand
- Variable syntax (\`?variable\`)

**Readability:**
- Active voice ("You'll create...")
- Second person
- Short paragraphs
- Frequent code blocks

**Time to read:** 15-20 minutes
**Time estimate to write:** 6-8 hours

---

### Tutorial 2: Basic Workflow (02-basic-workflow.md)

**Goal:** "I understand common SPARQL patterns and can query real-world data"

**Learning outcomes:**
- Parse multiple RDF formats
- Write queries with FILTERS
- JOIN data from multiple sources
- Use OPTIONAL patterns
- Convert results to different formats

**Content structure:**

1. **Overview** (50 words)
   - "Learn practical SPARQL patterns you'll use every day"
   - "Work with larger, more complex datasets"

2. **What You'll Learn** (bullet list)
   - Parse Turtle, JSON-LD, N-Triples
   - Write queries with FILTER
   - JOIN data (graph patterns)
   - Use OPTIONAL (nullable fields)
   - Export results (CSV, JSON, XML)

3. **Part 1: Multiple Formats** (1000 words)
   - Load sample data in 3 formats
   - Show each format
   - Explain when to use each
   - Parse each into store
   - Code example for each

4. **Part 2: FILTER Queries** (1000 words)
   - Find people over 18
   - Find articles from this year
   - Text search with regex
   - Complex filters (AND, OR, NOT)
   - Code examples

5. **Part 3: JOIN Patterns** (1200 words)
   - Find friends of friends
   - Find articles by specific author
   - Multi-level relationships
   - Graph traversal
   - Code examples

6. **Part 4: OPTIONAL Patterns** (800 words)
   - People with/without email
   - Default values in results
   - Handling missing data
   - Code examples

7. **Part 5: Export Formats** (600 words)
   - Convert to CSV
   - Export as JSON
   - Generate XML
   - Format comparison table
   - Code examples

8. **Real-World Example** (1000 words)
   - Complete workflow combining all concepts
   - Multi-step query
   - Data processing in JavaScript
   - Error handling

9. **Next Steps**
   - → Tutorial 3: Advanced Patterns
   - → How-To: Common Queries
   - → Explanation: SPARQL Concepts

**Code examples needed:**
- Parse Turtle, JSON-LD, N-Triples
- FILTER with comparisons and regex
- Graph patterns with multiple triples
- OPTIONAL pattern syntax
- results.toCSV(), toJSON(), toXML()
- Multi-step workflow

**Key concepts to introduce:**
- Graph patterns (multiple conditions)
- FILTER as server-side filtering
- NULL handling (OPTIONAL)
- Result formatting

**Time to read:** 20-25 minutes
**Time estimate to write:** 8-10 hours

---

### Tutorial 3: Advanced Patterns (03-advanced-patterns.md)

**Goal:** "I can write complex SPARQL queries for advanced use cases"

**Learning outcomes:**
- Use UNION queries
- Write subqueries
- Use aggregate functions
- Handle complex filtering
- Optimize query performance basics

**Content structure:**

1. **Overview** (50 words)
   - "Master advanced SPARQL patterns"
   - "Build complex queries step by step"

2. **What You'll Learn** (bullet list)
   - UNION queries (OR branches)
   - OPTIONAL with conditions
   - Aggregate functions (COUNT, SUM, MAX, MIN)
   - GROUP BY and HAVING
   - LIMIT and OFFSET (pagination)
   - String operations

3. **Part 1: UNION Queries** (900 words)
   - Find people or organizations
   - Find articles or blog posts
   - Multiple conditions with UNION
   - Performance note
   - Code examples

4. **Part 2: Aggregate Functions** (1000 words)
   - Count results: COUNT(?\*)
   - Count distinct: COUNT(DISTINCT ?var)
   - SUM, MAX, MIN
   - GROUP BY usage
   - HAVING clause for filtering groups
   - Code examples

5. **Part 3: Pagination** (600 words)
   - LIMIT clause
   - OFFSET for pagination
   - Practical pagination loop in JavaScript
   - Performance considerations
   - Code examples

6. **Part 4: String Operations** (800 words)
   - String functions (STRLEN, SUBSTR, STRUPCASE, etc.)
   - Regex matching
   - String concatenation
   - Language tags in literals
   - Code examples

7. **Part 5: Nested Complexity** (900 words)
   - Combining all patterns
   - Complex real-world example
   - Breaking down queries
   - Performance tips

8. **Performance Considerations** (600 words)
   - When to use UNION vs FILTER
   - Aggregate cost
   - ORDER BY impact
   - Query complexity indicators
   - Basic optimization tips

9. **Next Steps**
   - → How-To: Performance Optimization
   - → How-To: Troubleshooting Queries
   - → Explanation: Query Execution

**Code examples needed:**
- UNION query
- GROUP BY with COUNT
- HAVING clause
- Pagination loop
- String functions
- Complex combined example

**Key concepts:**
- UNION as logical OR
- Aggregation and grouping
- Pagination patterns
- Query cost/complexity

**Time to read:** 25-30 minutes
**Time estimate to write:** 10-12 hours

---

## 2. HOW-TO GUIDES (4 files)

Each solves a specific problem the reader is having **right now**.

### How-To 1: Optimize SPARQL Queries (optimize-sparql-queries.md)

**Problem:** "My queries are slow. How do I speed them up?"

**Sections:**

1. **Problem Statement** (100 words)
   - Symptoms: Queries taking 100ms+
   - Why it matters
   - Cost of slow queries

2. **Quick Diagnosis** (300 words)
   - How to measure query time
   - \`performance.now()\` example
   - Baseline interpretation
   - < 10ms = good
   - 100ms+ = slow

3. **Solution 1: Simplify Patterns** (500 words, with examples)
   - Problem: Nested patterns slow
   - Before: Complex query
   - After: Simplified query
   - Why it's faster
   - Performance comparison

4. **Solution 2: Add Filters** (400 words, with examples)
   - Problem: Returning too much data
   - Before: No FILTER
   - After: With FILTER
   - Server-side filtering benefit
   - Example filters

5. **Solution 3: Use LIMIT/OFFSET** (400 words, with examples)
   - Problem: Getting all results when need first N
   - Before: No LIMIT
   - After: With LIMIT
   - Pagination code example
   - Memory savings

6. **Solution 4: Avoid Expensive Patterns** (400 words, with examples)
   - Cartesian product example (slow)
   - Fixed version (fast)
   - Why it matters
   - Pattern checklist

7. **Solution 5: Use Typed Data** (300 words, with examples)
   - Problem: String vs numeric comparison
   - Why typed data is faster
   - Before: Untyped
   - After: Typed with \`xsd:integer\`
   - RDF in turtle

8. **Performance Comparison Table**
   - Technique | Impact | Difficulty
   - All 5+ solutions ranked

9. **Benchmarking Code** (300 words)
   - Complete benchmark function
   - How to measure improvements
   - Iterations, timing, averaging

10. **When to Add Indexes** (200 words)
    - When basic optimizations aren't enough
    - Link to Reference for index details

11. **See Also** (links)
    - API Reference: Query Options
    - Explanation: Query Execution
    - How-To: Large Graphs

**Code blocks needed:**
- 10+ before/after query pairs
- Benchmark function
- Each optimization technique
- Real-world examples

**Tone:** Direct, solution-focused

**Time estimate to write:** 6-8 hours

---

### How-To 2: Working with Different RDF Formats (working-with-formats.md)

**Problem:** "I have data in JSON-LD but need to work with Turtle. How do I convert?"

**Sections:**

1. **Quick Reference Table**
   - Format | Use When | Pros | Cons
   - Turtle | Most readable
   - JSON-LD | Web data
   - N-Triples | Streaming
   - RDF/XML | Legacy systems

2. **Parsing Each Format** (800 words)
   - Each format with example
   - When to use each
   - Code example for parsing
   - Error handling

3. **Converting Between Formats** (600 words)
   - Parse as one format
   - Serialize as another
   - Round-trip example
   - Code examples

4. **Handling Format Errors** (400 words)
   - Common parse errors
   - Debugging checklist
   - Try-catch pattern
   - Error messages explained

5. **Large File Handling** (400 words)
   - Streaming parser
   - Memory-efficient approach
   - Batch processing
   - Code example

6. **Validation** (300 words)
   - Check format compliance
   - SHACL validation
   - Link to SHACL guide

7. **See Also**

**Code blocks needed:**
- Parse example for each format
- Round-trip conversion
- Error handling
- Stream parsing
- Large file example

**Time estimate to write:** 5-7 hours

---

### How-To 3: Troubleshooting Common Issues (troubleshooting.md)

**Problem:** "Something isn't working. How do I fix it?"

**Sections (Problem → Cause → Solution):**

1. **Query Returns No Results**
   - Cause: Prefix mismatch
   - Cause: Wrong property names
   - Cause: Case sensitivity
   - Cause: Data not loaded
   - Debug checklist
   - Code to test

2. **Getting Parse Errors**
   - Cause: Malformed Turtle syntax
   - Cause: Invalid URI
   - Cause: Encoding issue
   - Specific error messages
   - How to fix each

3. **Type Mismatches**
   - Cause: String vs number
   - Cause: Literal vs URI
   - Cause: Language tags
   - How to identify
   - Fixes

4. **Performance Issues**
   - Cause: Large dataset
   - Cause: Complex query
   - Cause: No indexes
   - Quick wins
   - When to optimize

5. **Memory Leaks**
   - Cause: Holding references
   - Cause: Infinite loops
   - Detection method
   - Code patterns to avoid

6. **Concurrency Issues**
   - Cause: Concurrent updates
   - Cause: Race conditions
   - Solutions (transactions)
   - Code examples

7. **Common Error Codes Table**
   - Error message | Cause | Solution
   - 10+ common errors

8. **FAQ Section**
   - Q: Can I use store.clear() in loop?
   - Q: How big can a store be?
   - Q: Is it thread-safe?
   - Q: Can I nest transactions?
   - Etc.

**Format:** Problem → Diagnosis → Solution

**Code blocks needed:**
- Debug code for each problem
- Detection patterns
- Fixes/workarounds
- Recommended patterns

**Time estimate to write:** 5-7 hours

---

### How-To 4: Performance Tuning for Large Graphs (performance-tuning.md)

**Problem:** "I have 10 million triples. How do I keep performance acceptable?"

**Sections:**

1. **Understanding Bottlenecks** (500 words)
   - Query time vs storage time
   - Memory vs disk
   - Profiling queries
   - Benchmarking
   - Code examples

2. **Memory Optimization** (600 words)
   - When to use Oxigraph
   - In-memory limits
   - Streaming strategy
   - Batch operations
   - Code examples

3. **Query Optimization** (800 words)
   - Earlier: Basic optimization
   - Now: Advanced patterns
   - Selective loading
   - Filtering early
   - Index usage
   - Code examples

4. **Batch Operations** (500 words)
   - Add many triples efficiently
   - Transaction batching
   - Memory management
   - Performance comparison
   - Code examples

5. **Indexing Strategy** (600 words)
   - When to add indexes
   - Which properties to index
   - Index types
   - Trade-offs
   - Code examples

6. **Monitoring** (400 words)
   - Track query times
   - Monitor memory usage
   - Set up alerts
   - Continuous optimization
   - Code examples

7. **Real-World Example** (800 words)
   - Large social network
   - Initial performance: bad
   - Optimizations applied step by step
   - Final performance: good
   - What made the difference

8. **See Also**

**Code blocks needed:**
- Profiling code
- Memory measurement
- Batch operation patterns
- Index creation
- Monitoring setup
- Complete example

**Time estimate to write:** 6-8 hours

---

## 3. REFERENCE (5 files)

Complete, authoritative information for looking things up.

### Reference 1: API.md

**Content:** Every public function

**For each function:**
- Function signature
- Description (1-2 sentences)
- Parameters table
  - Name | Type | Default | Description
- Returns description
- Throws (error types)
- 2-3 code examples
- Links to how-to for usage

**Functions to document:**

**createKnowledgeSubstrateCore(options?)**
- Options: backend, enableValidation, enableTransactions, enableHooks, maxTriples, enableObservability
- Returns: Promise<KnowledgeSubstrate>
- Examples: Basic, with Oxigraph, performance-optimized

**Store Interface (returned by parseRdf)**
- parseRdf(data, format?, options?)
- query(sparql, options?)
- validateShacl(shapesStore)
- addQuad(subject, predicate, object, graph?)
- removeQuad(subject, predicate, object, graph?)
- match(subject?, predicate?, object?, graph?)
- has(subject, predicate, object, graph?)
- clear()
- size property

**Result Utilities**
- results.toJSON()
- results.toCSV()
- results.toTSV()
- results.toXML()

**Data Model**
- namedNode(uri)
- literal(value, languageOrDatatype?)
- blankNode(id?)
- variable(name)

**Exports**
- Table of all exports
- Categorized by use case
- Links to relevant how-tos

**Total length:** ~100-150 lines per function × 15 functions = ~2000 words

**Time estimate to write:** 10-12 hours

---

### Reference 2: TYPES.md

**Content:** All types and interfaces

**Sections:**

**Store**
- Properties: size
- Methods: parseRdf, query, addQuad, removeQuad, etc.
- Full interface definition

**QueryResults**
- Extends Array<Binding>
- Methods: toJSON, toCSV, toXML, etc.
- Indexing: results[0], results.length

**Binding**
- Methods: get(name), getAll(), has(name)
- Example: binding.get('name').value

**RDFTerm Types**
- NamedNode: { termType: 'NamedNode', value: string }
- Literal: { termType: 'Literal', value, language?, datatype? }
- BlankNode: { termType: 'BlankNode', value }
- Variable: { termType: 'Variable', value }

**Options Interfaces**
- CoreOptions
- QueryOptions
- ParseOptions
- StoreOptions

**Error Types**
- ParseError
- QueryError
- ValidationError
- TransactionError
- ConfigError

**Total length:** ~1500 words

**Time estimate to write:** 4-6 hours

---

### Reference 3: CONFIGURATION.md

**Content:** All configurable options

**createKnowledgeSubstrateCore() options table**
- backend | string | 'memory' | Storage backend
- enableValidation | boolean | true | Enable SHACL
- enableTransactions | boolean | true | ACID transactions
- enableHooks | boolean | true | Knowledge hooks
- enableStreaming | boolean | false | Stream processing
- enableObservability | boolean | false | OTEL tracing
- maxTriples | number | Infinity | Memory limit
- logLevel | string | 'info' | Logging level
- [format-specific options]

**Query options**
- timeout | number | 30000ms | Query timeout
- maxResults | number | Infinity | Result limit
- profile | boolean | false | Enable profiling
- baseUri | string | undefined | Base for relative IRIs
- useIndexes | boolean | true | Use indexes if available

**Parse options**
- format | string | auto | Explicit format
- streaming | boolean | false | Stream parsing
- validateSyntax | boolean | true | Check syntax
- charset | string | 'utf-8' | Encoding

**Store options (Oxigraph)**
- path | string | required | Database file
- autoVacuum | boolean | true | Optimize DB
- readonly | boolean | false | Read-only mode
- journal | boolean | true | Transaction log

**Examples**
- Memory store config
- Oxigraph persistent config
- High-performance config
- Development config

**Total length:** ~1000 words

**Time estimate to write:** 3-4 hours

---

### Reference 4: ERRORS.md

**Content:** Error reference table

**Table: Error Code | When | Message | Solution**

- ParseError: Unexpected EOF | Malformed RDF | Check syntax
- ParseError: Undefined namespace | Missing PREFIX | Add PREFIX clause
- QueryError: Syntax error | Invalid SPARQL | Check query syntax
- QueryError: Variable not in SELECT | ORDER BY undefined | Add to SELECT
- QueryError: Undefined variable | Reference unknown var | Define variable
- ValidationError: Shape constraint violated | SHACL validation failed | Fix data per shape
- TransactionError: Nested transaction | Transaction within transaction | Don't nest
- TransactionError: Rollback failed | Rollback error | Check backend
- ConfigError: Invalid option | Bad config value | Check options
- ConfigError: Missing dependency | Required package missing | npm install
- MemoryError: Out of memory | Heap exceeded | Use Oxigraph
- QueryTimeoutError: Query timeout | Query too slow | Simplify query
- BackendError: Database locked | Another process using DB | Wait or restart
- TypeMismatchError: Type mismatch | Literal vs URI | Check types

**For each error:**
- Full error message example
- When it occurs (conditions)
- Root cause explanation
- Step-by-step solution
- Prevention tips
- Code example

**Total length:** ~1500 words

**Time estimate to write:** 4-6 hours

---

### Reference 5: MIGRATION.md

**Content:** Version migration guide

**Sections:**

**3.x → 4.x Breaking Changes**
- API changes
- Deprecated functions
- Removed features
- Code migration examples

**4.x → 5.x (Current)**
- Current version features
- New APIs introduced
- Removed APIs
- Migration guide

**For each change:**
- What changed
- Why it changed
- Before code
- After code
- Migration checklist

**Total length:** ~1000 words

**Time estimate to write:** 4-6 hours

---

## 4. EXPLANATION (4 files)

Conceptual understanding and design rationale.

### Explanation 1: Architecture (architecture.md)

**Content:** How @unrdf/core is organized

**Based on:** DIATAXIS-EXAMPLES.md (already provided)

**Sections:**
1. Big picture (layered diagram)
2. Each layer's responsibility
3. Why layered design
4. Backend abstraction
5. Optional features
6. Data flow example
7. Performance implications
8. Future evolution

**Length:** ~2000 words
**Time estimate to write:** 6-8 hours

---

### Explanation 2: Design Decisions (design-decisions.md)

**Content:** Why these choices?

**Decisions to explain:**
1. Why layered architecture?
2. Why backend abstraction?
3. Why optional features?
4. Why this API design?
5. Why SPARQL not custom query language?
6. Why separate SHACL validation?
7. Why in-memory default?

**For each decision:**
- Alternative approach
- Why we chose our approach
- Trade-offs
- When the other approach might be better

**Length:** ~1500 words
**Time estimate to write:** 5-7 hours

---

### Explanation 3: SPARQL Concepts (sparql-concepts.md)

**Content:** Understanding SPARQL basics

**Sections:**
1. What is SPARQL? (vs SQL, Cypher, etc.)
2. RDF as a graph
3. Triple-based thinking
4. Graph patterns (matching)
5. Variables and binding
6. Query structure
7. Result types
8. Query execution order

**Length:** ~1500 words
**Time estimate to write:** 5-7 hours

---

### Explanation 4: Query Execution Model (query-execution.md)

**Content:** How queries actually run

**Sections:**
1. Query compilation
2. Planning (optimization)
3. Pattern evaluation
4. Result joining
5. Filtering application
6. Ordering and limiting
7. Bottlenecks
8. Optimization opportunities

**Length:** ~1500 words
**Time estimate to write:** 5-7 hours

---

## Summary by Diataxis Type

| Type | Files | Total Words | Effort Hours |
|------|-------|-------------|--------------|
| **Tutorials** | 3 | 5000-6000 | 24-30 |
| **How-To** | 4 | 4000-5000 | 22-30 |
| **Reference** | 5 | 7000-8000 | 26-34 |
| **Explanation** | 4 | 6000-7000 | 21-28 |
| **TOTAL** | **16 files** | **22000-26000** | **93-122 hours** |

---

## Sequencing & Dependencies

**No dependencies** between sections — can be written in parallel.

**Recommended sequence:**
1. Tutorials first (learn-by-doing establishes patterns)
2. References next (fill in details)
3. How-To guides (build on tutorials)
4. Explanations (provide deeper understanding)

**Quality gates:**
- [ ] Tutorials: All code examples tested
- [ ] References: Complete (no placeholders)
- [ ] How-To: Real problems solved
- [ ] Explanation: No code, pure concepts

---

## Writing Assignments (for team)

| Role | Files | Effort |
|------|-------|--------|
| **Core Domain Expert** | All reference files | 26-34 hours |
| **API Author** | API.md, TYPES.md | 14-18 hours |
| **Tutorial Author** | All tutorials | 24-30 hours |
| **How-To Writer** | All how-to guides | 22-30 hours |
| **Architect** | Explanation files | 21-28 hours |
| **Editor** | All files (polish) | 10-15 hours |

---

## Quality Checklist

Before each file is "done":

- [ ] **Content complete** - No TODO/FIXME placeholders
- [ ] **Code examples tested** - Run in fresh project
- [ ] **Links valid** - All cross-references work
- [ ] **Tone consistent** - Matches Diataxis type
- [ ] **Length appropriate** - Not too long/short
- [ ] **Accessibility** - No jargon without explanation
- [ ] **Grammar** - Spell-checked, edited
- [ ] **Formatting** - Clear headings, good spacing
- [ ] **Practical** - Real problems/examples
- [ ] **Audience appropriate** - Right level for readers

---

## Success Definition

@unrdf/core documentation is complete when:
1. ✅ All 16 files written
2. ✅ Validation score: 100%
3. ✅ All code examples tested
4. ✅ No TODO/FIXME remaining
5. ✅ Links all working
6. ✅ Peer reviewed
7. ✅ Deployed to docs site

---

**Ready to implement?** Start with tutorials, they establish the foundation pattern.
