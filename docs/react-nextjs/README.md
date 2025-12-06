# UNRDF React & Next.js Documentation

**Complete Diataxis-structured documentation for all React and Next.js code in the UNRDF monorepo.**

---

## What is This?

This documentation covers **36 JSX components**, **101 React hooks**, and **3 Next.js applications** in the UNRDF monorepo. It follows the [Diataxis framework](https://diataxis.fr/) to provide four types of documentation:

- **Tutorials** - Learning-oriented, hands-on lessons
- **How-to Guides** - Task-oriented, problem-solving guides
- **Reference** - Information-oriented, technical descriptions
- **Explanation** - Understanding-oriented, theoretical knowledge

---

## Quick Start

### I want to learn UNRDF React

Start with tutorials:
1. [Getting Started](./tutorials/01-getting-started.md) - Your first React app with UNRDF
2. [Knowledge Graph Explorer](./tutorials/02-knowledge-graph-explorer.md) - Build an interactive graph viewer
3. [Real-time Streaming](./tutorials/03-real-time-streaming.md) - Add collaboration features

**Time:** ~60 minutes total

---

### I want to solve a specific problem

Browse how-to guides:
- [Query with SPARQL](./how-to/query-with-sparql.md) - Execute SPARQL queries
- [Customize Graph Layouts](./how-to/customize-graph-layouts.md) - Configure visualizations
- [Implement Offline Sync](./how-to/implement-offline-sync.md) - Build offline-capable apps

**Time:** 5-20 minutes per guide

---

### I need API documentation

Check references:
- [Core Hooks API](./reference/core-hooks.md) - useKnowledgeEngine, useTriples, etc.
- [Visualization Components](./reference/components.md) - KnowledgeGraph, charts, maps
- [Streaming Hooks API](./reference/streaming-hooks.md) - useChangeFeed, useSubscription

**Format:** Complete API signatures, parameters, examples

---

### I want to understand the design

Read explanations:
- [Architecture](./explanation/architecture.md) - System design and principles
- [Hooks Organization](./explanation/hooks-organization.md) - 101 hooks categorized

**Purpose:** Concepts, rationale, tradeoffs

---

## Documentation Structure

```
docs/react-nextjs/
├── README.md (this file)
│
├── tutorials/                    # Learning-oriented
│   ├── 01-getting-started.md
│   ├── 02-knowledge-graph-explorer.md
│   └── 03-real-time-streaming.md
│
├── how-to/                       # Task-oriented
│   ├── query-with-sparql.md
│   ├── customize-graph-layouts.md
│   └── implement-offline-sync.md
│
├── reference/                    # Information-oriented
│   ├── core-hooks.md
│   ├── components.md
│   └── streaming-hooks.md
│
└── explanation/                  # Understanding-oriented
    ├── architecture.md
    └── hooks-organization.md
```

---

## What's Covered

### React Hooks (101 total)

**Core categories:**
- **Core (7)** - useKnowledgeEngine, useTriples, useStore, useGraphs
- **Streaming (6)** - useChangeFeed, useSubscription, useDeltaTracking
- **Query (5)** - useSPARQLQuery, useSPARQLConstruct, useSPARQLAsk
- **Dark-Matter (5)** - useQueryAnalyzer, useQueryOptimizer
- **AI-Semantic (9)** - useEmbeddings, useSemanticSearch, useNLPExtraction
- **Federation (6)** - useFederatedQuery, useConsensus, useReplication
- **HTF (12)** - Hyper-Thesis Framework operators (Γ, Λ, Π, τ)
- **Advanced-Utility (7)** - useGraphDiff, useGraphMerge, useIsomorphism
- **Form-UI (6)** - useQueryBuilder, useSPARQLEditor, useVisualizer
- **Error-Recovery (4)** - useErrorBoundary, useRetry, useRecovery
- **Policy-Security (4)** - useSecurityPolicy, useAccessControl, useValidation
- **Storage (4)** - useStorage, useIndexedDB, useTransactions
- **Context (4)** - useEngineContext, useConfigContext, useThemeContext
- **Knowledge-Hooks (4)** - useKnowledgeHooks, useHookExecution
- **Utils (4)** - useDebounce, useThrottle, useAsync
- **Effects (3)** - useListener, useObserver, useSideEffect
- **Composition (3)** - useComposedQuery, usePipeline, useWorkflow
- **Cache (3)** - useQueryCache, useMemoizedTriples
- **Batch (2)** - useBatchInsert, useBatchDelete

**See:** [Hooks Organization](./explanation/hooks-organization.md) for detailed breakdown

---

### Components (10 JSX)

Visualization components in `packages/react/components/`:

- **KnowledgeGraph** - Interactive graph visualization (Cytoscape)
- **EntropyCascadeVisualization** - Entropy cascade charts
- **EntropyVisualization3D** - 3D entropy visualization
- **GeospatialVisualization** - Map-based visualization (deck.gl)
- **PerformanceMetricsChart** - Performance charts (Chart.js)
- **PlantUMLViewer** - PlantUML diagram rendering
- **FMEADashboard** - Failure Mode analysis dashboard
- **InformationFlowAnalysis** - Information flow visualization
- **JTBDScenarios** - Jobs-to-be-Done scenarios
- **OperatorCardinality** - Operator cardinality visualization

**See:** [Components Reference](./reference/components.md)

---

### Next.js Applications (3)

1. **packages/react** - μ(O) Calculus Benchmark Dashboard
   - Next.js 16.0.7, React 18.3.1
   - Static export mode
   - Visualization components

2. **playground/hooks-showcase** - Interactive Hooks Demo
   - Next.js 14.2.0, React 18.3.0
   - 10 demo components showcasing hooks
   - Radix UI components

3. **packages/kgc-4d/playground** - Shard-Based Architecture Demo
   - Next.js 15.1.0, React 19.0.0
   - WebSocket Tether protocol
   - Framer Motion animations

**See:** [Architecture](./explanation/architecture.md) for integration patterns

---

### Examples (6 JSX)

Usage examples in `examples/`:
- `browser-react.jsx` - Browser-based React usage
- `react-hooks/basic-usage.jsx` - Basic hooks example
- `react-hooks/graph-explorer.jsx` - Graph explorer example
- `react-hooks/htf-thesis-builder.jsx` - HTF framework example
- `react-hooks/knowledge-hooks-editor.jsx` - Knowledge hooks editor

---

## Technology Stack

### Core Technologies

| Package | Version | Purpose |
|---------|---------|---------|
| **React** | 18.3.1 / 19.0.0 | UI framework |
| **Next.js** | 14.2.0 - 16.0.7 | React framework |
| **@unrdf/oxigraph** | Workspace | RDF store (WASM) |
| **@unrdf/core** | Workspace | Knowledge engine |

### Visualization Libraries

| Library | Purpose |
|---------|---------|
| **Cytoscape** | Graph visualization |
| **Three.js** | 3D graphics |
| **deck.gl** | Geospatial visualization |
| **Chart.js** | Charts and metrics |

### UI Libraries

| Library | Purpose |
|---------|---------|
| **Radix UI** | Accessible components |
| **Tailwind CSS** | Styling |
| **Framer Motion** | Animations |
| **lucide-react** | Icons |

---

## Diataxis Framework

This documentation follows [Diataxis](https://diataxis.fr/) principles:

| Type | Orientation | Purpose | Example |
|------|-------------|---------|---------|
| **Tutorials** | Learning | Acquire skills | "Build your first app" |
| **How-to** | Task | Solve problems | "How to query with SPARQL" |
| **Reference** | Information | Describe precisely | "useTriples API" |
| **Explanation** | Understanding | Clarify concepts | "Why hooks are organized this way" |

**Benefits:**
- Find what you need quickly
- Appropriate detail level
- Clear purpose for each document
- Predictable structure

---

## Contributing to Docs

### Adding Tutorials

Follow this structure:
```markdown
# Tutorial: [Title]

**Learning Objectives:**
- Bullet points

**Prerequisites:**
- Requirements
- Time estimate

## Step 1: [Action]
Clear instructions with code examples

## Step 2: [Action]
...

## What You Learned
Summary checklist

## Try It Yourself
Extension ideas

## What's Next?
Links to related docs
```

---

### Adding How-to Guides

Follow this structure:
```markdown
# How-to: [Task]

**Goal:** One sentence

**Time:** Estimate

## [Solution Step 1]
Code + explanation

## [Solution Step 2]
...

## Related
- Links to reference/explanation
```

---

### Adding Reference Docs

Follow this structure:
```markdown
# Reference: [API Name]

## [Function/Hook Name]

### Signature
TypeScript signature

### Parameters
Table with types

### Returns
Type definition

### Example
Working code

---

## Next item...
```

---

### Adding Explanations

Follow this structure:
```markdown
# Explanation: [Concept]

**Purpose:** Why this doc exists

## Overview
High-level summary

## [Section 1]
Concept explanation with diagrams

## [Section 2]
...

## Related
- Links to how-to/reference
```

---

## Verification

All file counts are verifiable:

```bash
# Total JSX files
find /home/user/unrdf -name "*.jsx" | wc -l
# Expected: 36

# React hooks
find /home/user/unrdf/packages/react/src -name "*.mjs" -type f | wc -l
# Expected: 101

# Next.js config files
find /home/user/unrdf -name "next.config.*" | wc -l
# Expected: 3

# Visualization components
find /home/user/unrdf/packages/react/components -name "*.jsx" | wc -l
# Expected: 10
```

---

## Related Documentation

- [UNRDF Core Documentation](../../packages/core/README.md)
- [Oxigraph Integration](../../packages/oxigraph/README.md)
- [KGC-4D Documentation](../../packages/kgc-4d/README.md)
- [CLAUDE.md](../../CLAUDE.md) - Development guidelines

---

## License

Same as parent project - see [LICENSE](../../LICENSE)

---

## Feedback

Found an issue or have a suggestion?
- Open an issue: https://github.com/seanchatmangpt/unrdf/issues
- See [CLAUDE.md](../../CLAUDE.md) for development workflow

---

**Last Updated:** 2025-12-05
**Documentation Version:** 1.0.0
**Covers:** 36 JSX files, 101 hooks, 3 Next.js apps
