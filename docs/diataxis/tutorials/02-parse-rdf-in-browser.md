# Tutorial 02: Parse RDF in Browser

**Objective:** Learn how to parse and query RDF data in browser environments using UNRDF's cross-runtime capabilities.

**Audience:** Beginners - some JavaScript experience required

**Prerequisites:**
- Completed [Tutorial 01: Create and Freeze Universe](./01-create-and-freeze-universe.md) (recommended)
- Basic understanding of browser JavaScript
- Familiarity with ES modules
- **Capability Atoms:** `@unrdf/core` (parse), `@unrdf/browser` (runtime bridge)

**Estimated Time:** 15-20 minutes

---

## What You'll Build

By the end of this tutorial, you will:
1. Set up UNRDF in a browser environment
2. Parse Turtle and JSON-LD in the browser
3. Execute SPARQL queries client-side
4. Understand cross-runtime bridging patterns

**Final Result:** A working browser application that parses and queries RDF data locally.

---

## Step 1: Browser Setup

**[Placeholder - Content to be filled]**

```html
<!-- HTML setup -->
<script type="module">
  import { createStore, parseTurtle } from '@unrdf/core';
</script>
```

**Evidence:** Browser setup in `/home/user/unrdf/packages/browser/src/setup.mjs`

---

## Step 2: Parse Turtle in Browser

**[Placeholder - Content to be filled]**

```javascript
// Browser parsing code
const turtle = `
  @prefix ex: <http://example.org/> .
  ex:Alice ex:knows ex:Bob .
`;
const store = await parseTurtle(turtle);
```

**Evidence:** Parse implementation at `/home/user/unrdf/packages/core/src/parse.mjs`

---

## Step 3: Execute SPARQL Queries

**[Placeholder - Content to be filled]**

```javascript
// SPARQL in browser
const results = await store.query(`
  SELECT ?person WHERE {
    ?person <http://example.org/knows> ?friend .
  }
`);
```

**Evidence:** Query engine at `/home/user/unrdf/packages/core/src/query.mjs`

---

## Step 4: Handle Browser Constraints

**[Placeholder - Content to be filled]**

```javascript
// Memory management and performance
// Browser-specific optimizations
```

**Evidence:** Browser optimizations in `/home/user/unrdf/packages/browser/src/optimizations.mjs`

---

## Example Code

**Complete Working Example:**

[Link to proof artifact - browser demo app]

**Evidence:** Full example at `/home/user/unrdf/examples/browser-parsing.html`

---

## Common Issues

**[Placeholder - Troubleshooting tips]**

- Issue: "Module not found in browser"
- Issue: "CORS errors when loading remote RDF"
- Issue: "Memory limits exceeded"

---

## What You've Learned

- ✅ How to set up UNRDF in browser environments
- ✅ How to parse RDF formats client-side
- ✅ How to execute SPARQL queries in the browser
- ✅ Understanding of cross-runtime bridging
- ✅ Browser-specific performance considerations

---

## Next Steps

**Continue Learning:**
- **[Tutorial 03: Generate and Verify Receipts](./03-generate-and-verify-receipts.md)** - Receipt generation

**Solve Specific Problems:**
- **[How-To 03: Measure Query Performance](../how-to/03-measure-query-performance.md)** - Optimize queries

**Understand the Design:**
- **[Explanation 03: Cross-Runtime Bridging](../explanation/cross-runtime-bridging.md)** - Deep dive into runtime strategies

**API Reference:**
- **[Package Exports Reference](../reference/package-exports.md)** - All available imports
- **[RDF Format Notes](../reference/rdf-format-notes.md)** - Format specifications

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
