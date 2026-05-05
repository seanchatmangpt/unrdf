# Vitest Architecture for UNRDF

## 🏗️ System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                      UNRDF Monorepo                             │
│                                                                  │
│  ┌────────────────────────────────────────────────────────┐   │
│  │              Root vitest.config.mjs                      │   │
│  │  • 80/20 pruned test suite                              │   │
│  │  • Single-threaded execution                            │   │
│  │  • AI agent compatibility                               │   │
│  │  • Global coverage: 95%                                 │   │
│  └─────────┬───────────────────────────────────────────────┘   │
│            │                                                     │
│  ┌─────────┴────────────────────────────────────────────────┐  │
│  │                  11 Core Packages                         │  │
│  │                                                            │  │
│  │  ┌───────────────┐  ┌───────────────┐  ┌──────────────┐ │  │
│  │  │  @unrdf/core  │  │@unrdf/browser │  │@unrdf/hooks  │ │  │
│  │  │  Node.js env  │  │  jsdom env    │  │ Node.js env  │ │  │
│  │  │  Coverage: 80%│  │  Coverage: 80%│  │Coverage: 80% │ │  │
│  │  └───────┬───────┘  └───────┬───────┘  └──────┬───────┘ │  │
│  │          │                   │                 │          │  │
│  │  ┌───────▼───────┐  ┌───────▼───────┐  ┌─────▼────────┐ │  │
│  │  │@unrdf/streaming│ │@unrdf/composables│ │@unrdf/cli   │ │  │
│  │  │  Node.js env   │ │   jsdom env    │  │ Node.js env │ │  │
│  │  │  Coverage: 80% │ │  Coverage: 80% │  │Coverage: 80%│ │  │
│  │  └────────────────┘ └────────────────┘  └──────────────┘ │  │
│  │                                                            │  │
│  │  ┌───────────────┐  ┌───────────────┐  ┌──────────────┐ │  │
│  │  │@unrdf/federation│ │@unrdf/knowledge│ │@unrdf/dark   │ │  │
│  │  │               │  │    -engine     │  │  -matter     │ │  │
│  │  │  Node.js env  │  │  Node.js env   │  │ Node.js env  │ │  │
│  │  │  Coverage: 80%│  │  Coverage: 80% │  │Coverage: 80% │ │  │
│  │  └───────────────┘  └────────────────┘  └──────────────┘ │  │
│  │                                                            │  │
│  │  ┌───────────────┐  ┌───────────────┐                    │  │
│  │  │@unrdf/project │  │               │                    │  │
│  │  │   -engine     │  │               │                    │  │
│  │  │  Node.js env  │  │               │                    │  │
│  │  │  Coverage: 80%│  │               │                    │  │
│  │  └───────────────┘  └───────────────┘                    │  │
│  └────────────────────────────────────────────────────────────┘  │
│                                                                  │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │              26 Example Subprojects                         │ │
│  │                                                             │ │
│  │  Each package has 2-3 examples:                            │ │
│  │  • basic-store, sparql-queries, rdf-parsing                │ │
│  │  • indexed-db, offline-support                             │ │
│  │  • hook-chains, policy-hooks                               │ │
│  │  • change-feeds, real-time-sync                            │ │
│  │  • query-integration, reactive-graphs                      │ │
│  │  • peer-discovery, distributed-queries                     │ │
│  │  • basic-inference, sparql-rules                           │ │
│  │  • index-advisor, query-optimization                       │ │
│  │  • format-conversion, graph-commands                       │ │
│  │  • And more...                                             │ │
│  │                                                             │ │
│  │  Coverage threshold: 70% (lower for examples)              │ │
│  └────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

## 📊 Test Execution Flow

```
┌─────────────────────────────────────────────────────────────┐
│                    1. Test Discovery                         │
│  vitest run --coverage                                       │
│  ↓                                                           │
│  Scans: test/**/*.test.mjs                                   │
│  Excludes: node_modules, dist, fixtures                     │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                 2. Environment Setup                         │
│  • Node.js: Standard Node.js runtime                        │
│  • jsdom: Browser simulation (window, document, etc.)       │
│  • Forks pool: Single-threaded execution                    │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                 3. Test Execution                            │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  beforeAll() hooks                                    │  │
│  │    ↓                                                  │  │
│  │  beforeEach() hooks                                   │  │
│  │    ↓                                                  │  │
│  │  Test 1: it('should...', () => { ... })              │  │
│  │    ↓                                                  │  │
│  │  afterEach() hooks                                    │  │
│  │    ↓                                                  │  │
│  │  beforeEach() hooks                                   │  │
│  │    ↓                                                  │  │
│  │  Test 2: it('should...', () => { ... })              │  │
│  │    ↓                                                  │  │
│  │  afterEach() hooks                                    │  │
│  │    ↓                                                  │  │
│  │  afterAll() hooks                                     │  │
│  └──────────────────────────────────────────────────────┘  │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│              4. Coverage Collection (v8)                     │
│  • Instruments source code                                   │
│  • Tracks line/function/branch coverage                     │
│  • Generates coverage/coverage-final.json                   │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│              5. Coverage Analysis                            │
│  • Compares against thresholds                              │
│  • Lines: 80%+ (core), 70%+ (examples)                      │
│  • Functions: 80%+ (core), 70%+ (examples)                  │
│  • Branches: 75%+ (core), 60%+ (examples)                   │
│  • Statements: 80%+ (core), 70%+ (examples)                 │
│  • Fails if below threshold                                 │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│               6. Report Generation                           │
│  • Text report: Console output                              │
│  • JSON report: coverage/coverage-final.json                │
│  • HTML report: coverage/index.html                         │
│  • Test results: coverage/test-results.json                 │
└─────────────────────────────────────────────────────────────┘
```

## 🔄 Test Lifecycle

```
┌─────────────────────────────────────────────────────────────┐
│                      Test Suite                              │
│                                                              │
│  describe('Feature', () => {                                │
│    ┌────────────────────────────────────────────────────┐  │
│    │  beforeAll(() => {                                  │  │
│    │    // Setup once for entire suite                   │  │
│    │    connectDatabase()                                │  │
│    │  })                                                 │  │
│    └─────────────────┬──────────────────────────────────┘  │
│                      │                                      │
│    ┌─────────────────▼──────────────────────────────────┐  │
│    │  beforeEach(() => {                                 │  │
│    │    // Setup before each test                        │  │
│    │    store = createStore()                            │  │
│    │  })                                                 │  │
│    └─────────────────┬──────────────────────────────────┘  │
│                      │                                      │
│    ┌─────────────────▼──────────────────────────────────┐  │
│    │  it('should do something', () => {                  │  │
│    │    // Arrange                                       │  │
│    │    const input = 'test'                             │  │
│    │                                                      │  │
│    │    // Act                                           │  │
│    │    const result = functionUnderTest(input)          │  │
│    │                                                      │  │
│    │    // Assert                                        │  │
│    │    expect(result).toBe('expected')                  │  │
│    │  })                                                 │  │
│    └─────────────────┬──────────────────────────────────┘  │
│                      │                                      │
│    ┌─────────────────▼──────────────────────────────────┐  │
│    │  afterEach(() => {                                  │  │
│    │    // Cleanup after each test                       │  │
│    │    store.clear()                                    │  │
│    │  })                                                 │  │
│    └─────────────────┬──────────────────────────────────┘  │
│                      │                                      │
│    │  ... more tests ...                                   │
│    │                                                       │
│    ┌─────────────────▼──────────────────────────────────┐  │
│    │  afterAll(() => {                                   │  │
│    │    // Cleanup once after entire suite               │  │
│    │    disconnectDatabase()                             │  │
│    │  })                                                 │  │
│    └────────────────────────────────────────────────────┘  │
│  })                                                         │
└─────────────────────────────────────────────────────────────┘
```

## 🎯 Coverage Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   Coverage Layers                            │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Layer 1: Source Code Instrumentation                  │ │
│  │  • v8 coverage provider                                │ │
│  │  • Adds instrumentation to src/**/*.mjs                │ │
│  │  • Tracks execution during tests                       │ │
│  └────────────────────┬───────────────────────────────────┘ │
│                       │                                      │
│  ┌────────────────────▼───────────────────────────────────┐ │
│  │  Layer 2: Execution Tracking                           │ │
│  │  • Lines: Which lines were executed?                   │ │
│  │  • Functions: Which functions were called?             │ │
│  │  • Branches: Which branches were taken?                │ │
│  │  • Statements: Which statements ran?                   │ │
│  └────────────────────┬───────────────────────────────────┘ │
│                       │                                      │
│  ┌────────────────────▼───────────────────────────────────┐ │
│  │  Layer 3: Threshold Validation                         │ │
│  │  ┌───────────────────────────────────────────────────┐│ │
│  │  │  Core Packages:                                   ││ │
│  │  │  • Lines: 80%    Functions: 80%                   ││ │
│  │  │  • Branches: 75% Statements: 80%                  ││ │
│  │  └───────────────────────────────────────────────────┘│ │
│  │  ┌───────────────────────────────────────────────────┐│ │
│  │  │  Examples:                                        ││ │
│  │  │  • Lines: 70%    Functions: 70%                   ││ │
│  │  │  • Branches: 60% Statements: 70%                  ││ │
│  │  └───────────────────────────────────────────────────┘│ │
│  └────────────────────┬───────────────────────────────────┘ │
│                       │                                      │
│  ┌────────────────────▼───────────────────────────────────┐ │
│  │  Layer 4: Report Generation                            │ │
│  │  • Text: Console output with summary                   │ │
│  │  • JSON: Machine-readable coverage data               │ │
│  │  • HTML: Interactive coverage browser                  │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

## 🏛️ Package Hierarchy

```
unrdf/
├── Root Level (vitest.config.mjs)
│   ├── 80/20 pruned test suite
│   ├── Global coverage: 95%
│   └── Single-threaded execution
│
├── Core Packages (11 packages)
│   ├── @unrdf/core
│   │   ├── vitest.config.mjs (node)
│   │   ├── test/core.test.mjs
│   │   ├── test/adversarial.test.mjs
│   │   └── Coverage: 80/80/75/80
│   │
│   ├── @unrdf/browser
│   │   ├── vitest.config.mjs (jsdom)
│   │   ├── test/browser.test.mjs
│   │   ├── test/adversarial.test.mjs
│   │   └── Coverage: 80/80/75/80
│   │
│   ├── @unrdf/hooks
│   │   ├── vitest.config.mjs (node)
│   │   ├── test/hooks.test.mjs
│   │   └── Coverage: 80/80/75/80
│   │
│   ├── @unrdf/streaming
│   │   ├── vitest.config.mjs (node)
│   │   ├── test/streaming.test.mjs
│   │   └── Coverage: 80/80/75/80
│   │
│   ├── @unrdf/composables
│   │   ├── vitest.config.mjs (jsdom)
│   │   ├── test/composables.test.mjs
│   │   └── Coverage: 80/80/75/80
│   │
│   ├── @unrdf/federation
│   │   ├── vitest.config.mjs (node)
│   │   ├── test/federation.test.mjs
│   │   └── Coverage: 80/80/75/80
│   │
│   ├── @unrdf/knowledge-engine
│   │   ├── vitest.config.mjs (node, 60s timeout)
│   │   ├── test/knowledge-engine.test.mjs
│   │   └── Coverage: 80/80/75/80
│   │
│   ├── @unrdf/dark-matter
│   │   ├── vitest.config.mjs (node)
│   │   ├── test/dark-matter.test.mjs
│   │   └── Coverage: 80/80/75/80
│   │
│   ├── @unrdf/cli
│   │   ├── vitest.config.mjs (node)
│   │   ├── test/cli.test.mjs
│   │   └── Coverage: 80/80/75/80
│   │
│   ├── @unrdf/project-engine
│   │   ├── vitest.config.mjs (node)
│   │   ├── test/project-engine.test.mjs
│   │   └── Coverage: 80/80/75/80
│   │
│   └── Each package has:
│       • Independent vitest.config.mjs
│       • Own test/ directory
│       • Package-specific test scripts
│       • Adversarial test suite
│
└── Example Subprojects (26 examples)
    ├── packages/core/examples/
    │   ├── basic-store/
    │   │   ├── vitest.config.mjs (node)
    │   │   ├── test/example.test.mjs
    │   │   └── Coverage: 70/70/60/70
    │   ├── sparql-queries/
    │   │   ├── vitest.config.mjs (node)
    │   │   ├── test/example.test.mjs
    │   │   └── Coverage: 70/70/60/70
    │   └── rdf-parsing/
    │       ├── vitest.config.mjs (node)
    │       ├── test/example.test.mjs
    │       └── Coverage: 70/70/60/70
    │
    ├── packages/browser/examples/
    │   ├── indexed-db/
    │   │   ├── vitest.config.mjs (jsdom)
    │   │   ├── test/example.test.mjs
    │   │   └── Coverage: 70/70/60/70
    │   └── offline-support/
    │       ├── vitest.config.mjs (jsdom)
    │       ├── test/example.test.mjs
    │       └── Coverage: 70/70/60/70
    │
    └── ... (21 more examples)
```

## 🔧 Configuration Inheritance

```
┌─────────────────────────────────────────────────────────────┐
│           Root vitest.config.mjs                             │
│  • Single-threaded execution                                 │
│  • 80/20 pruned suite                                        │
│  • Global coverage: 95%                                      │
│  • Shared optimizeDeps                                       │
└────────────────────────┬────────────────────────────────────┘
                         │
        ┌────────────────┴────────────────┐
        │                                  │
        ▼                                  ▼
┌───────────────────────┐      ┌──────────────────────────┐
│  Package Configs      │      │  Example Configs         │
│  • Inherits root      │      │  • Minimal config        │
│  • Environment: node  │      │  • Lower thresholds      │
│  • Coverage: 80%      │      │  • Coverage: 70%         │
│  • Package-specific   │      │  • Demonstration focus   │
└───────────────────────┘      └──────────────────────────┘
```

## 📊 Reporting Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   Coverage Reports                           │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Text Report (Console)                                  │ │
│  │  ┌────────────────────────────────────────────────────┐│ │
│  │  │  File      | % Stmts | % Branch | % Funcs | % Lines││ │
│  │  │  ----------|---------|----------|---------|--------││ │
│  │  │  All files |   latest |    latest |   latest |  latest││ │
│  │  │  core.mjs  |   latest |    latest |   latest |  latest││ │
│  │  └────────────────────────────────────────────────────┘│ │
│  └────────────────────────────────────────────────────────┘ │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  JSON Report (coverage/coverage-final.json)             │ │
│  │  {                                                      │ │
│  │    "path/to/file.mjs": {                               │ │
│  │      "lines": { "1": 5, "2": 3, ... },                 │ │
│  │      "functions": { "functionName": { ... } },         │ │
│  │      "branches": { ... }                               │ │
│  │    }                                                    │ │
│  │  }                                                      │ │
│  └────────────────────────────────────────────────────────┘ │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  HTML Report (coverage/index.html)                      │ │
│  │  ┌────────────────────────────────────────────────────┐│ │
│  │  │  Interactive file browser                           ││ │
│  │  │  • Navigate source files                            ││ │
│  │  │  • See line-by-line coverage                        ││ │
│  │  │  • Highlight uncovered code                         ││ │
│  │  │  • Filter by coverage threshold                     ││ │
│  │  └────────────────────────────────────────────────────┘│ │
│  └────────────────────────────────────────────────────────┘ │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Test Results (coverage/test-results.json)              │ │
│  │  {                                                      │ │
│  │    "numTotalTests": 330,                               │ │
│  │    "numPassedTests": 330,                              │ │
│  │    "numFailedTests": 0,                                │ │
│  │    "success": true,                                    │ │
│  │    "testResults": [...]                                │ │
│  │  }                                                      │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

## 🚀 Execution Optimization

```
┌─────────────────────────────────────────────────────────────┐
│              AI Agent Compatibility                          │
│                                                              │
│  Single-Threaded Execution:                                  │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  pool: 'forks'                                          │ │
│  │  poolOptions: {                                         │ │
│  │    forks: { singleFork: true }                          │ │
│  │  }                                                      │ │
│  │  concurrent: false                                      │ │
│  │  maxConcurrency: 1                                      │ │
│  └────────────────────────────────────────────────────────┘ │
│                                                              │
│  Benefits:                                                   │
│  • Prevents race conditions                                  │
│  • Ensures deterministic execution                           │
│  • Compatible with AI agent coordination                     │
│  • Easier debugging                                          │
│  • Consistent performance metrics                            │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│            Dependency Optimization                           │
│                                                              │
│  optimizeDeps: {                                             │
│    include: ['n3', 'zod', '@comunica/query-sparql']         │
│  }                                                           │
│                                                              │
│  Benefits:                                                   │
│  • Pre-bundles large dependencies                            │
│  • Faster test startup                                       │
│  • Reduced memory usage                                      │
│  • Improved CI/CD performance                                │
└─────────────────────────────────────────────────────────────┘
```

## 📈 Scaling Strategy

```
┌─────────────────────────────────────────────────────────────┐
│                  Package Growth Plan                         │
│                                                              │
│  Current: 11 packages + 26 examples = 37 subprojects        │
│                                                              │
│  Growth Phases:                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Phase 1: Foundation (Current)                          │ │
│  │  • 11 core packages                                     │ │
│  │  • 26 examples                                          │ │
│  │  • Standard vitest configs                              │ │
│  │  • 80%+ coverage                                        │ │
│  └────────────────────────────────────────────────────────┘ │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Phase 2: Expansion                                     │ │
│  │  • Add new packages as needed                           │ │
│  │  • Copy vitest.config.mjs template                      │ │
│  │  • Create test/ directory                               │ │
│  │  • Maintain standards                                   │ │
│  └────────────────────────────────────────────────────────┘ │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  Phase 3: Optimization                                  │ │
│  │  • Parallel package testing                             │ │
│  │  • Shared test fixtures                                 │ │
│  │  • CI/CD optimization                                   │ │
│  │  • Performance monitoring                               │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

---

**Architecture Version:** latest
**Last Updated:** 2025-12-04
**Maintained By:** UNRDF Architecture Team
