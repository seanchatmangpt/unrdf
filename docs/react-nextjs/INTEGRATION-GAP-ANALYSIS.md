# Integration Gap Analysis: React Hooks vs Next.js Apps

**Date:** 2025-12-05
**Status:** 🔴 CRITICAL GAP IDENTIFIED
**Severity:** High - Documentation describes ideal state, not actual implementation

---

## Executive Summary

The UNRDF React hooks (101 total) are **properly implemented and exported** but are **NOT being used** in the three Next.js applications. The apps use either:
1. **Static hardcoded data** (packages/react)
2. **Mock implementations** (playground/hooks-showcase)
3. **Custom KGC components** (packages/kgc-4d/playground)

**Impact:** Documentation created describes how hooks SHOULD work, but there's no live demonstration with real RDF data.

---

## Gap Analysis by Application

### 1. packages/react (μ(O) Calculus Dashboard)

**Current State:**
```jsx
// packages/react/app/page.jsx
const benchmarkData = useMemo(() => ({
  singleOperator: { latency: 0.853, /* hardcoded static data */ },
  // ... more static data
}), []);

// Uses visualization components
<PerformanceMetricsChart data={benchmarkData} />
<KnowledgeGraph data={benchmarkData} />
```

**Missing:**
- ❌ No `useKnowledgeEngine()` for RDF data
- ❌ No `KnowledgeEngineProvider` in layout
- ❌ No real RDF triples from Oxigraph
- ❌ Visualization components receive static objects, not RDF data

**Should Be:**
```jsx
'use client';

import { useKnowledgeEngine, useTriples } from 'unrdf-react';
import { KnowledgeEngineProvider } from 'unrdf-react/context';

// In layout.jsx
export default function RootLayout({ children }) {
  return (
    <KnowledgeEngineProvider backend="indexeddb">
      {children}
    </KnowledgeEngineProvider>
  );
}

// In page.jsx
function Dashboard() {
  const { engine, ready } = useKnowledgeEngine();
  const { data: benchmarkTriples } = useTriples({
    predicate: 'http://unrdf.org/benchmark/metric'
  });

  return ready ? (
    <PerformanceMetricsChart data={benchmarkTriples} />
  ) : <div>Loading...</div>;
}
```

**Gap Type:** Implementation - Hooks exist but aren't integrated

---

### 2. playground/hooks-showcase

**Current State:**
```jsx
// playground/hooks-showcase/components/demos/core-demo.jsx

// Mock the hooks for demo (in production, import from unrdf)
function useKnowledgeEngineMock() {
  const [data, setData] = useState([]);
  const query = async (sparql) => {
    await new Promise(r => setTimeout(r, 500)); // Fake delay
    setData([ /* fake data */ ]);
  };
  return { query, insert, delete: del, data, loading, error };
}

// Uses mock
const { query, insert, delete: del, data, loading } = useKnowledgeEngineMock();
```

**Missing:**
- ❌ Uses `useKnowledgeEngineMock()` instead of real `useKnowledgeEngine()`
- ❌ No actual RDF store connection
- ❌ All 10 demo components use mocks
- ❌ No `KnowledgeEngineProvider`

**Should Be:**
```jsx
'use client';

import { useKnowledgeEngine } from 'unrdf-react';

function CoreDemo() {
  const { query, insert, delete: del, data, loading } = useKnowledgeEngine();

  // Now using REAL hooks with REAL RDF data!
  return (/* ... */);
}
```

**Gap Type:** Critical - "Hooks showcase" doesn't actually showcase the hooks!

---

### 3. packages/kgc-4d/playground

**Current State:**
```jsx
// packages/kgc-4d/playground/app/page.jsx
import {
  UniverseExplorer,
  EventTimeline,
  EntityEditor,
} from '../components/index.mjs';

// Uses custom KGC components, not UNRDF React hooks
<UniverseExplorer />
<EntityEditor entityUri={selectedEntity} />
```

**Missing:**
- ❌ No UNRDF React hooks imported or used
- ❌ Uses KGC-specific components (separate system)
- ❌ No integration with `useKnowledgeEngine`, `useTriples`, etc.

**Should Be:**
```jsx
'use client';

import { useKnowledgeEngine, useChangeFeed, useTriples } from 'unrdf-react';

function PlaygroundPage() {
  const { engine, ready } = useKnowledgeEngine();
  const changes = useChangeFeed({
    filter: (change) => change.triple.subject.startsWith('http://example.org/')
  });

  const { data: entities } = useTriples({
    predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    object: 'http://kgc.io/ontology/Project'
  });

  return (/* Use entities from real RDF store */);
}
```

**Gap Type:** Architectural - KGC-4D uses different abstraction (Shard/Universe)

---

## Root Cause Analysis

### Why This Gap Exists

1. **Parallel Development:**
   - Hooks library developed separately
   - Next.js apps built as visualization demos
   - No integration step completed

2. **Different Purposes:**
   - `packages/react/app` - Benchmark visualization (static data is sufficient)
   - `playground/hooks-showcase` - UI demo (mocks avoid complexity)
   - `packages/kgc-4d` - Different architectural pattern (Shard-based, not hooks-based)

3. **Documentation Before Integration:**
   - Documentation describes ideal/intended usage
   - Reality: Integration work not yet done

---

## Verification Commands

### Hooks ARE Exported
```bash
grep -A 5 "export {" /home/user/unrdf/packages/react/src/index.mjs
# ✅ Shows: export { useKnowledgeEngine, useChangeFeed, ... }
```

### Hooks NOT Imported in Apps
```bash
grep -r "from 'unrdf-react'" /home/user/unrdf/packages/react/app/
# ❌ Returns: (nothing - zero imports)

grep -r "from 'unrdf-react'" /home/user/unrdf/playground/hooks-showcase/components/demos/
# ❌ Returns: (nothing - only mocks used)
```

### Mocks Used Instead
```bash
grep "Mock" /home/user/unrdf/playground/hooks-showcase/components/demos/core-demo.jsx
# ❌ Shows: useKnowledgeEngineMock, useChangeFeedMock, etc.
```

---

## Impact Assessment

### Documentation Impact
- ✅ **Good:** Documentation accurately describes HOW hooks should be used
- ❌ **Bad:** No live working example to validate documentation
- ❌ **Bad:** Users can't see hooks working with real data
- ❌ **Bad:** Can't verify hook behavior matches documentation

### User Impact
- ❌ Users clone repo expecting working demos
- ❌ "Hooks showcase" shows UI mockups, not real functionality
- ❌ No reference implementation to learn from
- ❌ Can't validate that hooks actually work as documented

### Development Impact
- ❌ Can't test hooks in realistic scenarios
- ❌ No integration testing coverage
- ❌ Documentation may drift from reality
- ❌ Performance claims unverified

---

## Recommended Actions

### Priority 1: Fix Hooks Showcase (Critical)

**Why:** This is literally called "hooks showcase" but doesn't showcase the hooks!

**Changes needed:**
1. Add `KnowledgeEngineProvider` to `playground/hooks-showcase/app/layout.jsx`
2. Replace all mock hooks with real hooks in demo components
3. Add sample RDF data on mount
4. Update demo components to use real SPARQL queries

**Estimated effort:** 4-6 hours
**Files to modify:** 11 (1 layout + 10 demo components)

---

### Priority 2: Integrate Hooks in Benchmark Dashboard (Medium)

**Why:** Demonstrates hooks with real performance data

**Changes needed:**
1. Add `KnowledgeEngineProvider` to `packages/react/app/layout.jsx`
2. Convert static `benchmarkData` to RDF triples
3. Use `useTriples()` or `useSPARQLQuery()` to fetch data
4. Update visualization components to accept RDF data

**Estimated effort:** 6-8 hours
**Files to modify:** 3 (layout + page + data conversion)

---

### Priority 3: Document KGC-4D Separation (Low)

**Why:** KGC-4D uses different pattern (intentional, not a gap)

**Changes needed:**
1. Add explanation in docs that KGC-4D uses Shard architecture
2. Note that it's complementary to hooks, not replacement
3. Show how hooks COULD be used with KGC-4D if desired

**Estimated effort:** 1-2 hours
**Files to modify:** 1 (documentation only)

---

## Decision Matrix

| Option | Pros | Cons | Recommendation |
|--------|------|------|----------------|
| **Fix Now** | • Validates documentation<br>• Working demos<br>• Integration tested | • Requires dev work<br>• May uncover bugs | ✅ **YES for hooks-showcase** |
| **Document Gap** | • Fast<br>• Honest about status | • Users confused<br>• No validation | ✅ **YES (this document)** |
| **Leave As-Is** | • No work | • Documentation lies<br>• No working examples | ❌ **NO** |

---

## Proposed Integration Plan

### Phase 1: Hooks Showcase (Week 1)
- [ ] Add KnowledgeEngineProvider to layout
- [ ] Replace useKnowledgeEngineMock with useKnowledgeEngine
- [ ] Replace useChangeFeedMock with useChangeFeed
- [ ] Add sample RDF data (people, projects)
- [ ] Update all 10 demo components
- [ ] Test in browser
- [ ] Update README with "Now with REAL hooks!" note

### Phase 2: Benchmark Dashboard (Week 2)
- [ ] Design RDF schema for benchmark data
- [ ] Convert static data to RDF triples
- [ ] Add KnowledgeEngineProvider
- [ ] Use useTriples() for data fetching
- [ ] Update visualization components
- [ ] Test performance
- [ ] Document integration

### Phase 3: Documentation Update (Week 3)
- [ ] Add "Live Examples" section to README
- [ ] Link to working demos
- [ ] Add integration guide
- [ ] Document KGC-4D architecture difference
- [ ] Update tutorials with real code from apps

---

## Success Criteria

**Hooks Showcase:**
- ✅ Zero mock hooks remaining
- ✅ All demos use real UNRDF React hooks
- ✅ RDF data persists in IndexedDB
- ✅ Real-time updates work via useChangeFeed

**Benchmark Dashboard:**
- ✅ Benchmark data stored as RDF triples
- ✅ Uses useKnowledgeEngine for data access
- ✅ Performance metrics queryable via SPARQL
- ✅ No hardcoded static data

**Documentation:**
- ✅ All examples reference actual working code
- ✅ README links to live demos
- ✅ Integration guide shows real implementation
- ✅ No disclaimer about "this is just documentation"

---

## Conclusion

**The hooks are real, the documentation is accurate, but the integration is missing.**

This is a **classic 80/20 situation:**
- 80% of the work is done (hooks implemented, documentation written)
- 20% of work remains (integration into Next.js apps)
- But that 20% is what makes it REAL

**Recommendation:** Fix the "hooks-showcase" app FIRST (highest value, lowest effort), then consider benchmark dashboard integration.

---

## Next Steps

1. **Acknowledge the gap** - This document serves as evidence
2. **Prioritize integration** - Start with hooks-showcase
3. **Update documentation** - Add "Integration Status" section
4. **Track progress** - Use GitHub issue for integration work

**Owner:** Development team
**Timeline:** 2-3 weeks for full integration
**Dependencies:** None (hooks are ready, apps are ready)
