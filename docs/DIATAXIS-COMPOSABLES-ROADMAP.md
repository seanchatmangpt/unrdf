# @unrdf/composables Documentation Roadmap

**Package:** @unrdf/composables (Type 3: Integration - Vue)
**Effort:** 50-64 hours
**Files:** 16
**Words:** 14,000-18,000
**Phase:** 3 (Week 6)
**Pattern:** Follow React integration pattern from Phase 2

---

## Overview

`@unrdf/composables` provides Vue 3 composition functions for reactive RDF data access. It's an integration package, so documentation assumes Vue.js expertise and introduces UNRDF concepts.

### Key Assumption
> Reader is expert in Vue 3 composition API. Introducing RDF queries as a new concept.

This means:
- Don't explain `ref()`, `reactive()`, `watch()`, `computed()`
- DO explain how they apply to RDF data
- Show Vue patterns first, UNRDF second

---

## Tutorials (3 files, 12-16 hours total)

### Tutorial 1: Getting Started with Vue Compositions

**File:** `docs/tutorials/01-getting-started-with-vue.md`
**Effort:** 5-6 hours
**Words:** 3,500-4,000

**Goal:** New user loads RDF data into a Vue component

**Structure:**

1. **Hook (Problem statement, 500 words)**
   - Scenario: Building Vue component that displays RDF data
   - Current approach: Fetch data, manually manage state
   - Better approach: Reactive composition function
   - By end: One function call gives reactive data

2. **Compose Function Basics (1,000 words)**
   - Vue composition API review (syntax, not concepts)
   - Add store to component: `const store = useRdfStore()`
   - Initialize with file or endpoint
   - Component structure with setup()

3. **Executing Your First Query (1,200 words)**
   - SPARQL basics (just enough to understand example)
   - Query for all people: `SPARQL SELECT ?name`
   - Execute with `useQuery()`: `const results = useQuery(sparql)`
   - Explain results object (array of bindings)

4. **Binding to Template (800 words)**
   - Show results are reactive refs
   - Bind to template: `v-for="item in results"`
   - Display person name: `{{ item.name }}`
   - Live update when data changes

5. **Verify and Troubleshoot (1,000 words)**
   - Expected output (screenshot/code)
   - Common mistakes:
     - Forgetting `await useRdfStore()`
     - Incorrect SPARQL syntax
     - Missing ref unwrapping
   - Testing: Open DevTools, inspect results
   - Next: Try modifying the query

**Code Examples:**

```javascript
// Example 1: Basic setup (copy-paste ready)
import { useRdfStore, useQuery } from '@unrdf/composables';
import { defineComponent } from 'vue';

export default defineComponent({
  setup() {
    const store = useRdfStore();

    const results = useQuery(`
      SELECT ?name WHERE {
        ?person a :Person ;
                :name ?name .
      }
    `);

    return { results };
  },
  template: `
    <div>
      <h2>People</h2>
      <ul>
        <li v-for="person in results" :key="person.name">
          {{ person.name }}
        </li>
      </ul>
    </div>
  `
});
```

```javascript
// Example 2: With data loading
import { onMounted } from 'vue';

setup() {
  const store = useRdfStore();

  onMounted(async () => {
    await store.loadFile('people.ttl');
  });

  const results = useQuery(`...`);

  return { results };
}
```

```javascript
// Example 3: Error handling
const results = useQuery(sparql, {
  onError: (error) => {
    console.error('Query failed:', error.message);
  }
});
```

**Learning Outcomes:**
- [ ] Can create Vue component with RDF data
- [ ] Understand useRdfStore() and useQuery()
- [ ] Can execute basic SPARQL in Vue
- [ ] Know how to debug basic issues

---

### Tutorial 2: Reactive Query Updates

**File:** `docs/tutorials/02-reactive-query-updates.md`
**Effort:** 4-5 hours
**Words:** 2,500-3,000

**Goal:** Component updates automatically when RDF data changes

**Structure:**

1. **Problem Statement (400 words)**
   - Scenario: Data is being updated externally
   - Current approach: Polling or manual refresh
   - Better approach: Real-time subscription
   - Use case: Dashboard with live data

2. **Understanding Reactivity in RDF (800 words)**
   - Vue's reactivity model (refs, computed)
   - RDF subscription model (quad streams)
   - How they work together
   - Analogy: RDF subscriptions like Vue watch()

3. **Using useSubscription (1,200 words)**
   - Subscribe to RDF changes: `const sub = useSubscription()`
   - Handler function receives new quads
   - Update component state from handler
   - Unsubscribe on component destroy

4. **Building a Live Update Component (1,000 words)**
   - Real example: Person status updates
   - Subscribe to person changes
   - Update reactive data when subscription fires
   - Show status in template immediately

**Code Examples:**

```javascript
// Example 1: Basic subscription
import { useRdfStore, useSubscription } from '@unrdf/composables';
import { ref } from 'vue';

setup() {
  const store = useRdfStore();
  const lastUpdate = ref(null);

  useSubscription((quad) => {
    console.log('RDF updated:', quad);
    lastUpdate.value = new Date();
  });

  return { lastUpdate };
}
```

```javascript
// Example 2: Live dashboard
const statuses = ref([]);

useSubscription((quad) => {
  if (quad.predicate.value === 'http://example.com/status') {
    const person = quad.subject.value;
    const status = quad.object.value;

    // Update or add status
    const existing = statuses.value.find(s => s.person === person);
    if (existing) {
      existing.status = status;
    } else {
      statuses.value.push({ person, status });
    }
  }
});
```

**Learning Outcomes:**
- [ ] Understand RDF subscription model
- [ ] Can implement useSubscription()
- [ ] Know how to update component from RDF changes
- [ ] Can build reactive dashboard

---

### Tutorial 3: Adding and Modifying Data

**File:** `docs/tutorials/03-adding-and-modifying-data.md`
**Effort:** 3-5 hours
**Words:** 2,000-2,500

**Goal:** Create and modify RDF data from Vue form

**Structure:**

1. **From Form to RDF (600 words)**
   - Typical form: Text input, submit button
   - Goal: Convert form data to RDF
   - Pattern: Form → Validation → RDF quads → Store

2. **Creating Quads (800 words)**
   - Quad structure (subject, predicate, object, graph)
   - Creating a quad manually
   - `dataFactory` for proper RDF objects
   - Type safety (URIs, literals, blank nodes)

3. **Using useMutation (1,000 words)**
   - Mutation basics: add, update, delete
   - `useMutation()` composition function
   - Adding quads: `await mutation.add(quads)`
   - Error handling and validation

4. **Real Example: Blog Post Form (600 words)**
   - Form with title and content
   - Create post RDF
   - Add to store with mutation
   - Show confirmation to user

**Code Examples:**

```javascript
// Example 1: Simple mutation
import { useMutation, useRdfStore } from '@unrdf/composables';

setup() {
  const store = useRdfStore();
  const mutation = useMutation(store);

  const addPerson = async (name) => {
    const quads = [
      {
        subject: { type: 'BlankNode', value: '_:person1' },
        predicate: { type: 'NamedNode', value: 'http://example.com/name' },
        object: { type: 'Literal', value: name }
      }
    ];

    await mutation.add(quads);
  };

  return { addPerson };
}
```

```javascript
// Example 2: Form submission
const name = ref('');
const error = ref(null);

const handleSubmit = async () => {
  try {
    await addPerson(name.value);
    name.value = '';
  } catch (e) {
    error.value = e.message;
  }
};
```

**Learning Outcomes:**
- [ ] Understand quad structure
- [ ] Can create RDF quads from form data
- [ ] Use useMutation() to add data
- [ ] Handle errors from mutations

---

## How-To Guides (4 files, 12-16 hours total)

### How-To 1: Optimize Vue Reactivity

**File:** `docs/how-to/01-optimize-vue-reactivity.md`
**Effort:** 3-4 hours
**Words:** 1,500-2,000

**Problem:** Component re-renders too frequently, performance degrades

**Solutions:**

1. **Use `computed()` for stable results (400 words)**
   - Problem: Query runs on every component change
   - Solution: Computed property caches results
   - Code: `const filtered = computed(() => results.value.filter(...))`

2. **Debounce subscription callbacks (400 words)**
   - Problem: Many updates = many renders
   - Solution: Debounce handler
   - Code: `useSubscription(debounce(handler, 200ms))`

3. **Batch mutations into transactions (400 words)**
   - Problem: 100 quads = 100 separate mutations = 100 renders
   - Solution: Batch into one transaction
   - Code: `mutation.transaction(() => { add all 100 })`

4. **Configure result caching (300 words)**
   - Problem: Same query re-executes
   - Solution: Cache configuration
   - Code: `useQuery(sparql, { cache: true })`

**Before/After Metrics:**
- Table showing renders/second before and after
- Memory usage before/after
- Real numbers from profiling

---

### How-To 2: Handle Large Datasets

**File:** `docs/how-to/02-handle-large-datasets.md`
**Effort:** 3-4 hours
**Words:** 1,500-2,000

**Problem:** Component slows down with millions of RDF triples

**Solutions:**

1. **Pagination (400 words)**
   - Use LIMIT/OFFSET in query
   - Fetch 20 at a time
   - Code: `SELECT ?item LIMIT 20 OFFSET 0`

2. **Virtual scrolling (400 words)**
   - Load visible items only
   - Vue component library recommendation
   - Code pattern with virtual scroller

3. **Lazy load related data (400 words)**
   - Load person, don't load their 1000 posts
   - Load on demand when user clicks
   - Code: Click → Execute query → Update data

4. **Filter at query time (300 words)**
   - Don't filter in JavaScript
   - Push filters to query
   - Code: `FILTER (?age > 18)`

---

### How-To 3: Debug Queries in Vue

**File:** `docs/how-to/03-debug-queries-in-vue.md`
**Effort:** 3-4 hours
**Words:** 1,500-2,000

**Problem:** Query returns empty, unexpected data, or errors

**Diagnosis Process:**

1. **Verify query syntax (400 words)**
   - Test query in isolation (standalone SPARQL tool)
   - Check predicate URIs match data
   - Common mistakes (missing ?variables, wrong prefixes)

2. **Inspect store contents (400 words)**
   - View store in DevTools
   - Log all quads: `store.match(null, null, null)`
   - Verify data was loaded

3. **Enable debug logging (400 words)**
   - Configuration: `{ debug: true }`
   - Log query execution
   - Log results transformation
   - View in browser console

4. **Test query in isolation (300 words)**
   - Create minimal example outside Vue
   - Execute same query and store
   - Helps isolate Vue vs RDF issues

---

### How-To 4: Integrate with Pinia or Vuex

**File:** `docs/how-to/04-integrate-with-state-management.md`
**Effort:** 3-4 hours
**Words:** 1,500-2,000

**Problem:** Need RDF store in multiple components

**Solutions:**

1. **Store composable pattern (400 words)**
   - Create single useRdfStore instance
   - Export from composables/store.js
   - Import in any component
   - Share across app

2. **Pinia integration (400 words)**
   - Create Pinia store that wraps useRdfStore
   - Actions for queries/mutations
   - State for results
   - Code: Pinia store example

3. **Vuex integration (400 words)**
   - Similar pattern for Vuex
   - Actions for async operations
   - Mutations for state updates
   - Code: Vuex module example

4. **Testing with stores (300 words)**
   - Mock store for testing
   - Fixtures for test data
   - Unit test example

---

## Reference (5 files, 12-16 hours total)

### Reference 1: API Documentation

**File:** `docs/reference/01-api.md`
**Effort:** 4-5 hours
**Words:** 3,000-3,500

Functions documented:

1. **useRdfStore(options?)**
   - Signature, parameters, returns
   - Options table (format, persistence, indexing)
   - Error conditions
   - 3 examples (basic, with file, with persistence)

2. **useQuery(sparql, options?)**
   - Signature, parameters, returns
   - Returns QueryResult<T> (typed)
   - Options table (timeout, caching, batchSize)
   - 3 examples (simple, with filters, typed)

3. **useMutation(store)**
   - Signature, parameters, returns
   - Methods: add(), update(), delete()
   - Options per method
   - 3 examples (single add, batch, transaction)

4. **useSubscription(handler, options?)**
   - Signature, parameters, returns
   - Handler: (quad: Quad) => void
   - Options table (immediate, buffer)
   - 3 examples (basic, with filter, with batching)

5. **useIndexing(store)**
   - Signature, parameters, returns
   - Methods: createIndex(), dropIndex()
   - Options for index type (predicate, object, combined)
   - 3 examples

---

### Reference 2: Types Documentation

**File:** `docs/reference/02-types.md`
**Effort:** 2-3 hours
**Words:** 1,500-2,000

**Interfaces:**

```javascript
// ComposableStore
interface ComposableStore {
  // Properties
  quads: Ref<Quad[]>;
  size: Ref<number>;

  // Methods
  loadFile(path: string): Promise<void>;
  addQuads(quads: Quad[]): Promise<void>;
  query(sparql: string): Promise<Binding[]>;
}

// QueryResult<T>
interface QueryResult<T = Binding> {
  value: Ref<T[]>;
  loading: Ref<boolean>;
  error: Ref<Error | null>;
  refresh(): Promise<void>;
}

// MutationOptions
interface MutationOptions {
  transaction?: boolean;
  validate?: boolean;
  onError?: (error: Error) => void;
}
```

---

### Reference 3: Configuration

**File:** `docs/reference/03-configuration.md`
**Effort:** 2-3 hours
**Words:** 1,000-1,500

Configuration table format:

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| format | 'turtle' \| 'nquads' | 'turtle' | RDF format |
| persistence | boolean | false | Save to disk |
| indexing | 'predicate' \| 'object' | 'predicate' | Index strategy |
| caching | boolean | true | Cache queries |
| batchSize | number | 100 | Mutation batch size |

---

### Reference 4: Error Reference

**File:** `docs/reference/04-errors.md`
**Effort:** 2-3 hours
**Words:** 1,000-1,500

Error reference table:

| Code | Message | Cause | Solution |
|------|---------|-------|----------|
| STORE_NOT_INIT | Store not initialized | useRdfStore not called | Call useRdfStore first |
| QUERY_SYNTAX | Invalid SPARQL | Malformed SPARQL | Validate syntax |
| QUERY_TIMEOUT | Query timeout | Large dataset | Add LIMIT or FILTER |

---

### Reference 5: Performance Guide

**File:** `docs/reference/05-performance.md`
**Effort:** 2-3 hours
**Words:** 1,000-1,500

Performance characteristics:

- **Query execution:** O(n) where n = matching quads
- **Memory:** ~1KB per triple in memory
- **Update latency:** <50ms for <1000 updates
- **Subscription overhead:** <1ms per update in handler

Benchmarks table comparing different operations.

---

## Explanation (4 files, 10-14 hours total)

### Explanation 1: Vue Reactivity Model

**File:** `docs/explanation/01-vue-reactivity-model.md`
**Effort:** 3-4 hours
**Words:** 2,000-2,500

**Big Picture:** How Vue 3 reactivity enables reactive RDF

**Definition:** Vue wraps data in Proxy objects that intercept changes

**How It Works:**
1. `ref()` creates reactive wrapper
2. Changes trigger watchers/computed
3. Components re-render

**Why:** Automatic synchronization between data and UI

**When to Use:** Building interactive UIs with frequent updates

**Relevance to Composables:**
- useRdfStore returns reactive refs
- useQuery returns reactive results
- useSubscription triggers reactivity updates
- Composables leverage Vue's reactivity system

---

### Explanation 2: Composition Design Decisions

**File:** `docs/explanation/02-composition-design-decisions.md`
**Effort:** 3-4 hours
**Words:** 2,000-2,500

**Why composition API?**
- Organize code by feature, not type
- Better code reuse
- Cleaner dependency injection

**Why reactive refs?**
- Automatic Vue integration
- No manual update logic needed
- Works with existing Vue ecosystem

**Why batching mutations?**
- Performance: fewer renders
- Consistency: atomic updates
- Simplicity: one transaction vs many

**Why debounce subscriptions?**
- Prevent update storms
- Reduce render thrashing
- Improve frame rate

**Trade-offs:**
- Batching: Complexity vs performance
- Caching: Memory vs speed
- Subscriptions: Latency vs simplicity

---

### Explanation 3: RDF in Vue Ecosystem

**File:** `docs/explanation/03-rdf-in-vue-ecosystem.md`
**Effort:** 2-3 hours
**Words:** 1,500-2,000

**RDF's role:** Semantic data format, graph relationships

**Vue's role:** Reactive UI rendering

**How they fit:** UNRDF data + Vue rendering = interactive RDF apps

**When to use:** Knowledge-heavy UIs, linked data, semantic queries

**Examples:**
- Recipe browser (links between ingredients, recipes, chefs)
- Knowledge graph explorer (navigate entities and relationships)
- Person database with relationships (family tree, organizational chart)

---

## Summary

**Total Effort:** 50-64 hours
**Total Files:** 16
**Total Words:** 14,000-18,000

**Content Breakdown:**
- Tutorials: 3 files, 12-16 hours (learn by doing)
- How-To: 4 files, 12-16 hours (solve problems)
- Reference: 5 files, 12-16 hours (look it up)
- Explanation: 4 files, 10-14 hours (understand why)

**Quality Gates:**
- ✅ All code examples tested
- ✅ No TODO/FIXME placeholders
- ✅ Peer reviewed
- ✅ 100% validation score

---

## Next Phase

Phase 4 (Week 9+) covers private packages and root documentation.

This package provides the integration pattern for Phase 4's similar packages (composables for Vue, similar to React).
