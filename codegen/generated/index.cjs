/**
 * UNRDF Package Registry
 * Auto-generated from package.json files
 * Generated: 2026-09-28T23:33:20.839Z
 */

const PACKAGES = {
  "@unrdf/ai-ml-innovations": {
    "name": "@unrdf/ai-ml-innovations",
    "version": "0.0.0-agnostic",
    "description": "Novel AI/ML integration patterns for UNRDF knowledge graphs",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./temporal-gnn": "./src/temporal-gnn.mjs",
      "./neural-symbolic": "./src/neural-symbolic-reasoner.mjs",
      "./federated": "./src/federated-embeddings.mjs"
    },
    "path": "ai-ml-innovations",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/knowledge-engine",
      "@unrdf/semantic-search",
      "@unrdf/ml-inference",
      "@unrdf/kgc-4d",
      "@unrdf/v6-core",
      "@opentelemetry/api",
      "zod"
    ],
    "devDependencies": [
      "vitest",
      "eslint"
    ]
  },
  "@unrdf/atomvm": {
    "name": "@unrdf/atomvm",
    "version": "0.0.0-agnostic",
    "description": "AtomVM runtimes, OTP patterns, and receipted swarm control planes for browser and Node.js",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./service-worker-manager": "./src/service-worker-manager.mjs",
      "./assets": "./src/assets.mjs",
      "./avm-packer": "./src/avm-packer.mjs",
      "./continuum": "./src/continuum/index.mjs",
      "./continuum/browser": "./src/continuum/browser-client.mjs"
    },
    "path": "atomvm",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/oxigraph",
      "@unrdf/receipts",
      "@unrdf/streaming",
      "@unrdf/v6-core",
      "coi-serviceworker",
      "zod"
    ],
    "devDependencies": [
      "@playwright/test",
      "@vitest/browser",
      "jsdom",
      "vite",
      "vitest"
    ]
  },
  "@unrdf/blockchain": {
    "name": "@unrdf/blockchain",
    "version": "0.0.0-agnostic",
    "description": "Blockchain integration for UNRDF - Cryptographic receipt anchoring and audit trails",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./anchoring": "./src/anchoring/receipt-anchorer.mjs",
      "./contracts": "./src/contracts/workflow-verifier.mjs",
      "./merkle": "./src/merkle/merkle-proof-generator.mjs"
    },
    "path": "blockchain",
    "dependencies": [
      "@noble/hashes",
      "@unrdf/kgc-4d",
      "ethers",
      "merkletreejs",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/caching": {
    "name": "@unrdf/caching",
    "version": "0.0.0-agnostic",
    "description": "Multi-layer caching system for RDF queries with Redis and LRU",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./layers": "./src/layers/multi-layer-cache.mjs",
      "./invalidation": "./src/invalidation/dependency-tracker.mjs",
      "./query": "./src/query/sparql-cache.mjs"
    },
    "path": "caching",
    "dependencies": [
      "@unrdf/oxigraph",
      "ioredis",
      "lru-cache",
      "msgpackr",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/chatman-equation": {
    "name": "@unrdf/chatman-equation",
    "version": "0.0.0-agnostic",
    "description": "Chatman Equation documentation generation using Tera-compatible templates",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./template-engine": "./src/template-engine.mjs",
      "./config": "./src/config-loader.mjs",
      "./filters": "./src/filters.mjs"
    },
    "path": "chatman-equation",
    "dependencies": [
      "@iarna/toml",
      "@unrdf/core",
      "@unrdf/kgn",
      "@unrdf/oxigraph",
      "glob",
      "nunjucks",
      "smol-toml",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/cli": {
    "name": "@unrdf/cli",
    "version": "0.0.0-agnostic",
    "description": "UNRDF CLI - Command-line Tools for Graph Operations and Context Management",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./commands": "./src/commands/index.mjs"
    },
    "path": "cli",
    "dependencies": [
      "@iarna/toml",
      "archiver",
      "@unrdf/core",
      "@unrdf/daemon",
      "@unrdf/federation",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/kgc-swarm",
      "@unrdf/knowledge-engine",
      "@unrdf/oxigraph",
      "@unrdf/project-engine",
      "@unrdf/receipts",
      "@unrdf/streaming",
      "citty",
      "glob",
      "gray-matter",
      "js-yaml",
      "nunjucks",
      "table",
      "yaml",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "citty-test-utils",
      "vitest"
    ]
  },
  "@unrdf/codegen": {
    "name": "@unrdf/codegen",
    "version": "0.0.0-agnostic",
    "description": "Code generation and metaprogramming tools for UNRDF",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./sparql-types": "./src/sparql-type-generator.mjs",
      "./meta-templates": "./src/meta-template-engine.mjs",
      "./property-tests": "./src/property-test-generator.mjs"
    },
    "path": "codegen",
    "dependencies": [
      "fast-check",
      "zod"
    ],
    "devDependencies": [
      "nunjucks",
      "vitest"
    ]
  },
  "@unrdf/collab": {
    "name": "@unrdf/collab",
    "version": "0.0.0-agnostic",
    "description": "Real-time collaborative RDF editing using CRDTs (Yjs) with offline-first architecture",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./crdt": "./src/crdt/index.mjs",
      "./sync": "./src/sync/index.mjs",
      "./composables": "./src/composables/index.mjs"
    },
    "path": "collab",
    "dependencies": [
      "@unrdf/core",
      "yjs",
      "y-websocket",
      "y-indexeddb",
      "lib0",
      "zod",
      "ws"
    ],
    "devDependencies": [
      "@types/node",
      "@types/ws",
      "vitest",
      "vue"
    ]
  },
  "@unrdf/composables": {
    "name": "@unrdf/composables",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Composables - Vue 3 Composables for Reactive RDF State (Optional Extension)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./graph": "./src/graph.mjs",
      "./delta": "./src/delta.mjs"
    },
    "path": "composables",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/streaming",
      "rdf-canonize",
      "unctx",
      "vue"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/consensus": {
    "name": "@unrdf/consensus",
    "version": "0.0.0-agnostic",
    "description": "Production-grade Raft consensus for distributed workflow coordination",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./raft": "./src/raft/raft-coordinator.mjs",
      "./cluster": "./src/membership/cluster-manager.mjs",
      "./state": "./src/state/distributed-state-machine.mjs",
      "./transport": "./src/transport/websocket-transport.mjs"
    },
    "path": "consensus",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/federation",
      "msgpackr",
      "ws",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "@types/ws",
      "eslint",
      "prettier",
      "vitest"
    ]
  },
  "@unrdf/core": {
    "name": "@unrdf/core",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./rdf": "./src/rdf/index.mjs",
      "./rdf/minimal-n3-integration": "./src/rdf/minimal-n3-integration.mjs",
      "./rdf/n3-justified-only": "./src/rdf/n3-justified-only.mjs",
      "./sparql": "./src/sparql/index.mjs",
      "./sparql/executor-sync": "./src/sparql/executor-sync.mjs",
      "./sparql/embeddings": "./src/sparql/embeddings.mjs",
      "./sparql/semantic-executor": "./src/sparql/semantic-executor.mjs",
      "./index/hnsw": "./src/index/hnsw.mjs",
      "./types": "./src/types.mjs",
      "./constants": "./src/constants.mjs",
      "./validation": "./src/validation/index.mjs",
      "./health": "./src/health.mjs",
      "./logger": "./src/logger.mjs",
      "./metrics": "./src/metrics.mjs",
      "./security": "./src/security.mjs",
      "./security-schemas": "./src/security-schemas.mjs",
      "./utils/sparql-utils": "./src/utils/sparql-utils.mjs",
      "./utils/semantic-bridge": "./src/utils/semantic-bridge.mjs",
      "./utils/lockchain-writer": "./src/utils/lockchain-writer.mjs",
      "./viz/graph-visualizer": "./src/viz/graph-visualizer.mjs",
      "./viz/query-explainer": "./src/viz/query-explainer.mjs",
      "./debug/rdf-inspector": "./src/debug/rdf-inspector.mjs",
      "./capabilities": "./src/capability-ledger.mjs",
      "./capability-graph": "./src/capability-graph.mjs",
      "./evidence": "./src/evidence-store.mjs",
      "./receipts": "./src/receipt-chain.mjs",
      "./execution-plan": "./src/execution-plan.mjs",
      "./admission": "./src/admission-boundary.mjs",
      "./command-verifier": "./src/command-verifier.mjs",
      "./replay": "./src/replay-runner.mjs",
      "./transaction-core": "./src/utils/transaction-core.mjs"
    },
    "path": "core",
    "dependencies": [
      "@noble/hashes",
      "@opentelemetry/api",
      "@rdfjs/data-model",
      "@rdfjs/namespace",
      "@rdfjs/serializer-jsonld",
      "@rdfjs/serializer-turtle",
      "@rdfjs/to-ntriples",
      "@unrdf/oxigraph",
      "async-mutex",
      "hnswlib-node",
      "jsonld",
      "n3",
      "onnxruntime-node",
      "oxigraph",
      "rdf-canonize",
      "rdf-ext",
      "rdf-validate-shacl",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  "@unrdf/daemon": {
    "name": "@unrdf/daemon",
    "version": "0.0.0-agnostic",
    "description": "Background daemon for managing scheduled tasks and event-driven operations",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./daemon": "./src/daemon.mjs",
      "./mcp": "./src/mcp/index.mjs",
      "./schemas": "./src/schemas.mjs",
      "./trigger-evaluator": "./src/trigger-evaluator.mjs",
      "./v6-deltagate": "./src/integrations/v6-deltagate.mjs",
      "./middleware/rate-limiter": "./src/middleware/rate-limiter.mjs",
      "./middleware/rate-limiter-schema": "./src/middleware/rate-limiter.schema.mjs",
      "./integrations/nitro-tasks": "./src/integrations/nitro-tasks.mjs",
      "./integrations/kgc-4d-sourcing": "./src/integrations/kgc-4d-sourcing.mjs",
      "./integrations/kgc-4d-merkle": "./src/integrations/kgc-4d-merkle.mjs"
    },
    "path": "daemon",
    "dependencies": [
      "@ai-sdk/groq",
      "@grpc/grpc-js",
      "@modelcontextprotocol/sdk",
      "@opentelemetry/api",
      "@opentelemetry/exporter-metrics-otlp-grpc",
      "@opentelemetry/exporter-trace-otlp-grpc",
      "@opentelemetry/resources",
      "@opentelemetry/sdk-metrics",
      "@opentelemetry/sdk-node",
      "@opentelemetry/sdk-trace-node",
      "@opentelemetry/semantic-conventions",
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/otel",
      "ai",
      "cron-parser",
      "hash-wasm",
      "nunjucks",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/dark-matter": {
    "name": "@unrdf/dark-matter",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Dark Matter - Query Optimization and Performance Analysis (Optional Extension)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./analyzer": "./src/dark-matter/query-analyzer.mjs"
    },
    "path": "dark-matter",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "typhonjs-escomplex",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/decision-fabric": {
    "name": "@unrdf/decision-fabric",
    "version": "0.0.0-agnostic",
    "description": "Hyperdimensional Decision Fabric - Intent-to-Outcome transformation engine using μ-operators",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./engine": "./src/engine.mjs",
      "./operators": "./src/operators.mjs",
      "./socratic": "./src/socratic-agent.mjs",
      "./pareto": "./src/pareto-analyzer.mjs"
    },
    "path": "decision-fabric",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/knowledge-engine",
      "@unrdf/oxigraph",
      "@unrdf/project-engine",
      "@unrdf/streaming",
      "@unrdf/validation",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "eslint",
      "jest"
    ]
  },
  "@unrdf/diataxis-kit": {
    "name": "@unrdf/diataxis-kit",
    "version": "0.0.0-agnostic",
    "description": "Diátaxis documentation kit for monorepo package inventory and deterministic doc scaffold generation",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./inventory": "./src/inventory.mjs",
      "./evidence": "./src/evidence.mjs",
      "./classify": "./src/classify.mjs",
      "./scaffold": "./src/scaffold.mjs",
      "./stable-json": "./src/stable-json.mjs",
      "./hash": "./src/hash.mjs"
    },
    "path": "diataxis-kit",
    "dependencies": [],
    "devDependencies": []
  },
  "@unrdf/domain": {
    "name": "@unrdf/domain",
    "version": "0.0.0-agnostic",
    "description": "Domain models and types for UNRDF",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "domain",
    "dependencies": [],
    "devDependencies": []
  },
  "@unrdf/engine-gateway": {
    "name": "@unrdf/engine-gateway",
    "version": "0.0.0-agnostic",
    "description": "μ(O) Engine Gateway - Enforcement layer for Oxigraph-first, N3-minimal RDF processing",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./gateway": "./src/gateway.mjs",
      "./operation-detector": "./src/operation-detector.mjs",
      "./validators": "./src/validators.mjs"
    },
    "path": "engine-gateway",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/event-automation": {
    "name": "@unrdf/event-automation",
    "version": "0.0.0-agnostic",
    "description": "Event-driven automation for v6.1.0 - automatic delta processing with receipts",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./engine": "./src/event-automation-engine.mjs",
      "./delta-processor": "./src/delta-processor.mjs",
      "./receipt-tracker": "./src/receipt-tracker.mjs",
      "./policy-enforcer": "./src/policy-enforcer.mjs",
      "./schemas": "./src/schemas.mjs"
    },
    "path": "event-automation",
    "dependencies": [
      "@unrdf/daemon",
      "@unrdf/v6-core",
      "@unrdf/hooks",
      "@unrdf/receipts",
      "@opentelemetry/api",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest",
      "eslint"
    ]
  },
  "@unrdf/federation": {
    "name": "@unrdf/federation",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Federation - Distributed RDF Query with RAFT Consensus and Multi-Master Replication",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./coordinator": "./src/federation/coordinator.mjs",
      "./advanced-sparql": "./src/advanced-sparql-federation.mjs",
      "./ml/predictor": "./src/ml/predictor.mjs",
      "./query-planner-core": "./src/federation/query-planner-core.mjs"
    },
    "path": "federation",
    "dependencies": [
      "@comunica/query-sparql",
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/hooks",
      "prom-client",
      "zod"
    ],
    "devDependencies": [
      "@opentelemetry/sdk-trace-base",
      "@types/node",
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  "@unrdf/fusion": {
    "name": "@unrdf/fusion",
    "version": "0.0.0-agnostic",
    "description": "Unified integration layer for 7-day UNRDF innovation - KGC-4D, blockchain, hooks, caching",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "fusion",
    "dependencies": [
      "@unrdf/blockchain",
      "@unrdf/caching",
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "graphql",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/geosparql": {
    "name": "@unrdf/geosparql",
    "version": "0.0.0-agnostic",
    "description": "OGC GeoSPARQL standard compliance for spatial RDF queries",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./geometry": "./src/geometry.mjs",
      "./spatial-relations": "./src/spatial-relations.mjs",
      "./distance": "./src/distance.mjs",
      "./rtree-index": "./src/rtree-index.mjs",
      "./crs": "./src/crs.mjs",
      "./query-functions": "./src/query-functions.mjs"
    },
    "path": "geosparql",
    "dependencies": [
      "@turf/turf",
      "rbush",
      "@unrdf/oxigraph",
      "@opentelemetry/api",
      "zod"
    ],
    "devDependencies": [
      "vitest",
      "eslint"
    ]
  },
  "@unrdf/graph-analytics": {
    "name": "@unrdf/graph-analytics",
    "version": "0.0.0-agnostic",
    "description": "Advanced graph analytics for RDF knowledge graphs using graphlib",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./converter": "./src/converter/rdf-to-graph.mjs",
      "./centrality": "./src/centrality/pagerank-analyzer.mjs",
      "./paths": "./src/paths/relationship-finder.mjs",
      "./clustering": "./src/clustering/community-detector.mjs"
    },
    "path": "graph-analytics",
    "dependencies": [
      "@dagrejs/graphlib",
      "graphlib",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/hooks": {
    "name": "@unrdf/hooks",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Knowledge Hooks - Policy Definition and Execution Framework",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./define": "./src/define.mjs",
      "./executor": "./src/executor.mjs",
      "./parallel-executor": "./src/hooks/parallel-executor.mjs",
      "./dependency-graph": "./src/hooks/dependency-graph.mjs",
      "./worker-pool": "./src/hooks/worker-pool.mjs"
    },
    "path": "hooks",
    "dependencies": [
      "@noble/hashes",
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/otel",
      "@unrdf/oxigraph",
      "citty",
      "eyereasoner",
      "oxigraph",
      "rdf-validate-shacl",
      "sparqljs",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  "@unrdf/integration-tests": {
    "name": "@unrdf/integration-tests",
    "version": "0.0.0-agnostic",
    "description": "Phase 5: Comprehensive Integration & Adversarial Tests (75 tests)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {},
    "path": "integration-tests",
    "dependencies": [
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/kgc-multiverse",
      "@unrdf/federation",
      "@unrdf/streaming",
      "@unrdf/oxigraph",
      "@unrdf/receipts",
      "@unrdf/core",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@vitest/coverage-v8",
      "vitest"
    ]
  },
  "@unrdf/kgc-4d": {
    "name": "@unrdf/kgc-4d",
    "version": "0.0.0-agnostic",
    "description": "KGC 4D Datum & Universe Freeze Engine - Nanosecond-precision event logging with Git-backed snapshots",
    "tier": "essential",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./client": "./src/client.mjs",
      "./hdit": "./src/hdit/index.mjs"
    },
    "path": "kgc-4d",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "async-mutex",
      "hash-wasm",
      "isomorphic-git",
      "zod"
    ],
    "devDependencies": [
      "comment-parser",
      "simple-statistics",
      "tinybench",
      "vitest"
    ]
  },
  "@unrdf/kgc-4d-playground": {
    "name": "@unrdf/kgc-4d-playground",
    "version": "0.0.0-agnostic",
    "description": "KGC-4D Playground - Shard-Based Architecture Demo with Perfect Client/Server Relationship",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {},
    "path": "kgc-4d-playground",
    "dependencies": [
      "@monaco-editor/react",
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "@unrdf/validation",
      "@xyflow/react",
      "clsx",
      "d3-scale",
      "elkjs",
      "framer-motion",
      "lucide-react",
      "next",
      "react",
      "react-dom",
      "react-force-graph-3d",
      "tailwind-merge",
      "three",
      "ws",
      "zod"
    ],
    "devDependencies": [
      "@playwright/test",
      "@types/node",
      "@types/react",
      "@types/ws",
      "autoprefixer",
      "eslint",
      "eslint-config-next",
      "postcss",
      "tailwindcss",
      "vitest"
    ]
  },
  "@unrdf/kgc-cli": {
    "name": "@unrdf/kgc-cli",
    "version": "0.0.0-agnostic",
    "description": "KGC CLI - Deterministic extension registry for ~40 workspace packages",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./registry": "./src/lib/registry.mjs",
      "./manifest": "./src/manifest/extensions.mjs",
      "./latex": "./src/lib/latex/index.mjs",
      "./latex/schemas": "./src/lib/latex/schemas.mjs"
    },
    "path": "kgc-cli",
    "dependencies": [
      "citty",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/kgc-docs": {
    "name": "@unrdf/kgc-docs",
    "version": "0.0.0-agnostic",
    "description": "KGC Markdown parser and dynamic documentation generator with proof anchoring",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/kgc-markdown.mjs",
      "./parser": "./src/parser.mjs",
      "./renderer": "./src/renderer.mjs",
      "./proof": "./src/proof.mjs",
      "./reference-validator": "./src/reference-validator.mjs",
      "./changelog-generator": "./src/changelog-generator.mjs",
      "./executor": "./src/executor.mjs"
    },
    "path": "kgc-docs",
    "dependencies": [
      "@unrdf/kgc-runtime",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/kgc-multiverse": {
    "name": "@unrdf/kgc-multiverse",
    "version": "0.0.0-agnostic",
    "description": "KGC Multiverse - Universe branching, forking, and morphism algebra for knowledge graphs",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./universe-manager": "./src/universe-manager.mjs",
      "./morphism": "./src/morphism.mjs",
      "./guards": "./src/guards.mjs",
      "./q-star": "./src/q-star.mjs",
      "./composition": "./src/composition.mjs",
      "./parallel-executor": "./src/parallel-executor.mjs",
      "./worker-task": "./src/worker-task.mjs",
      "./cli-10k": "./src/cli-10k.mjs"
    },
    "path": "kgc-multiverse",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/kgc-4d",
      "@unrdf/receipts",
      "hash-wasm",
      "piscina",
      "zod"
    ],
    "devDependencies": [
      "@vitest/coverage-v8",
      "vitest",
      "eslint",
      "unbuild"
    ]
  },
  "@unrdf/kgc-probe": {
    "name": "@unrdf/kgc-probe",
    "version": "0.0.0-agnostic",
    "description": "KGC Probe - Automated knowledge graph integrity scanning with 10 agents and artifact validation",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./orchestrator": "./src/orchestrator.mjs",
      "./guards": "./src/guards.mjs",
      "./agents": "./src/agents/index.mjs",
      "./storage": "./src/storage/index.mjs",
      "./types": "./src/types.mjs",
      "./artifact": "./src/artifact.mjs",
      "./cli": "./src/cli.mjs",
      "./utils": "./src/utils/index.mjs",
      "./utils/logger": "./src/utils/logger.mjs",
      "./utils/errors": "./src/utils/errors.mjs",
      "./orchestration-core": "./src/orchestration-core.mjs"
    },
    "path": "kgc-probe",
    "dependencies": [
      "@noble/hashes",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/kgc-substrate",
      "@unrdf/oxigraph",
      "@unrdf/v6-core",
      "hash-wasm",
      "n3",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest",
      "@vitest/coverage-v8"
    ]
  },
  "@unrdf/kgc-runtime": {
    "name": "@unrdf/kgc-runtime",
    "version": "0.0.0-agnostic",
    "description": "KGC governance runtime with comprehensive Zod schemas and work item system",
    "tier": "extended",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./schemas": "./src/schemas.mjs",
      "./work-item": "./src/work-item.mjs",
      "./plugin-manager": "./src/plugin-manager.mjs",
      "./plugin-isolation": "./src/plugin-isolation.mjs",
      "./api-version": "./src/api-version.mjs"
    },
    "path": "kgc-runtime",
    "dependencies": [
      "@unrdf/oxigraph",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/kgc-substrate": {
    "name": "@unrdf/kgc-substrate",
    "version": "0.0.0-agnostic",
    "description": "KGC Substrate - Deterministic, hash-stable KnowledgeStore with immutable append-only log",
    "tier": "extended",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./types": "./src/types.mjs",
      "./KnowledgeStore": "./src/KnowledgeStore.mjs"
    },
    "path": "kgc-substrate",
    "dependencies": [
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "@unrdf/core",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest",
      "@vitest/coverage-v8"
    ]
  },
  "@unrdf/kgc-swarm": {
    "name": "@unrdf/kgc-swarm",
    "version": "0.0.0-agnostic",
    "description": "Multi-agent template orchestration with cryptographic receipts - KGC planning meets kgn rendering",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./guards": "./src/guards.mjs",
      "./orchestrator": "./src/orchestrator.mjs",
      "./token-generator": "./src/token-generator.mjs",
      "./compressor": "./src/compressor.mjs",
      "./tracker": "./src/tracker.mjs",
      "./guardian": "./src/guardian.mjs",
      "./transport": "./src/transport/hypercore-transport.mjs"
    },
    "path": "kgc-swarm",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/kgc-substrate",
      "@unrdf/kgn",
      "@unrdf/knowledge-engine",
      "@unrdf/kgc-4d",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest",
      "eslint",
      "typescript",
      "fast-check"
    ]
  },
  "@unrdf/kgc-tools": {
    "name": "@unrdf/kgc-tools",
    "version": "0.0.0-agnostic",
    "description": "KGC Tools - Verification, freeze, and replay utilities for KGC capsules",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./verify": "./src/verify.mjs",
      "./freeze": "./src/freeze.mjs",
      "./replay": "./src/replay.mjs",
      "./list": "./src/list.mjs",
      "./tool-wrapper": "./src/tool-wrapper.mjs"
    },
    "path": "kgc-tools",
    "dependencies": [
      "@unrdf/kgc-4d",
      "@unrdf/kgc-runtime",
      "@unrdf/core",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/kgn": {
    "name": "@unrdf/kgn",
    "version": "0.0.0-agnostic",
    "description": "Deterministic Nunjucks template system with custom filters and frontmatter support",
    "tier": "optional",
    "main": "dist/index.mjs",
    "exports": {
      ".": {
        "import": "./dist/index.mjs",
        "types": "./dist/index.d.ts"
      },
      "./engine": {
        "import": "./src/engine/index.js"
      },
      "./filters": {
        "import": "./src/filters/index.js"
      },
      "./renderer": {
        "import": "./src/renderer/index.js"
      },
      "./linter": {
        "import": "./src/linter/index.js"
      },
      "./templates/*": "./src/templates/*"
    },
    "path": "kgn",
    "dependencies": [
      "@unrdf/core",
      "consola",
      "fs-extra",
      "glob",
      "gray-matter",
      "nunjucks",
      "yaml",
      "zod"
    ],
    "devDependencies": [
      "@amiceli/vitest-cucumber",
      "@babel/parser",
      "@babel/traverse",
      "comment-parser",
      "cors",
      "eslint",
      "nodemon",
      "vitest"
    ]
  },
  "@unrdf/knowledge-engine": {
    "name": "@unrdf/knowledge-engine",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Knowledge Engine - Rule Engine, Inference, and Pattern Matching (Optional Extension)",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./query": "./src/query.mjs",
      "./canonicalize": "./src/canonicalize.mjs",
      "./parse": "./src/parse.mjs",
      "./ai-search": "./src/ai-enhanced-search.mjs"
    },
    "path": "knowledge-engine",
    "dependencies": [
      "@comunica/query-sparql",
      "@iarna/toml",
      "@noble/hashes",
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/streaming",
      "@xenova/transformers",
      "eyereasoner",
      "lru-cache",
      "rdf-canonize",
      "rdf-ext",
      "rdf-validate-shacl",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/manufacturing": {
    "name": "@unrdf/manufacturing",
    "version": "0.0.0-agnostic",
    "description": "μ(O) Manufacturing Operator Runtime — composable operators for deterministic artifact manufacturing",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./operators": "./src/operators/index.mjs",
      "./pipeline": "./src/pipeline/index.mjs",
      "./gate": "./src/gate/index.mjs",
      "./causality": "./src/causality/index.mjs",
      "./artifact": "./src/artifact/index.mjs",
      "./repository-fact-accounting": "./src/repository-fact-accounting.mjs",
      "./git-repository-facts": "./src/git-repository-facts.mjs"
    },
    "path": "manufacturing",
    "dependencies": [
      "@unrdf/core",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/ml-inference": {
    "name": "@unrdf/ml-inference",
    "version": "0.0.0-agnostic",
    "description": "UNRDF ML Inference - High-performance ONNX model inference pipeline for RDF streams",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./runtime": "./src/runtime/onnx-runner.mjs",
      "./pipeline": "./src/pipeline/streaming-inference.mjs",
      "./registry": "./src/registry/model-registry.mjs"
    },
    "path": "ml-inference",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/streaming",
      "@unrdf/oxigraph",
      "onnxruntime-node",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/ml-versioning": {
    "name": "@unrdf/ml-versioning",
    "version": "0.0.0-agnostic",
    "description": "ML Model Versioning System using TensorFlow.js and UNRDF KGC-4D time-travel capabilities",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./version-store": "./src/version-store.mjs",
      "./tf": "./src/tf.mjs",
      "./examples/image-classifier": "./src/examples/image-classifier.mjs"
    },
    "path": "ml-versioning",
    "dependencies": [
      "@tensorflow/tfjs",
      "@tensorflow/tfjs-backend-cpu",
      "@tensorflow/tfjs-node",
      "@unrdf/core",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/observability": {
    "name": "@unrdf/observability",
    "version": "0.0.0-agnostic",
    "description": "Innovative Prometheus/Grafana observability dashboard for UNRDF distributed workflows",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./metrics": "./src/metrics/workflow-metrics.mjs",
      "./exporters": "./src/exporters/grafana-exporter.mjs",
      "./alerts": "./src/alerts/alert-manager.mjs"
    },
    "path": "observability",
    "dependencies": [
      "@opentelemetry/api",
      "@opentelemetry/exporter-prometheus",
      "@opentelemetry/sdk-metrics",
      "express",
      "hash-wasm",
      "prom-client",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/otel": {
    "name": "@unrdf/otel",
    "version": "0.0.0-agnostic",
    "description": "OpenTelemetry integration for UNRDF using pm4py-rust telemetry infrastructure",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./attributes": "./src/generated/attributes.mjs",
      "./metrics": "./src/generated/metrics.mjs",
      "./pm4py": "./src/pm4py.mjs",
      "./monitoring": "./src/monitoring.mjs",
      "./validation": "./src/validation/index.mjs",
      "./collector-config": "./deploy/otel-collector-config.yaml",
      "./ocel": "./src/ocel/index.mjs",
      "./conformance": "./src/conformance/index.mjs"
    },
    "path": "otel",
    "dependencies": [
      "@opentelemetry/api",
      "@opentelemetry/semantic-conventions",
      "@unrdf/manufacturing"
    ],
    "devDependencies": []
  },
  "@unrdf/oxigraph": {
    "name": "@unrdf/oxigraph",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./store": "./src/store.mjs",
      "./types": "./src/types.mjs",
      "./query-cache": "./src/query-cache.mjs",
      "./sparql-star": "./src/sparql-star.mjs",
      "./store-receipts": "./src/store-receipts.mjs"
    },
    "path": "oxigraph",
    "dependencies": [
      "oxigraph",
      "zod",
      "@unrdf/v6-core"
    ],
    "devDependencies": [
      "@types/node",
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  "@unrdf/pictl-algorithms": {
    "name": "@unrdf/pictl-algorithms",
    "version": "0.0.0-agnostic",
    "description": "PICTL Process Mining Algorithms for UNRDF Federation - OCEL discovery, conformance, and prediction via WASM",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./pictl-wrapper": "./src/pictl-wrapper.mjs"
    },
    "path": "pictl-algorithms",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/pictl-semantics",
      "zod"
    ],
    "devDependencies": [
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  "@unrdf/pictl-semantics": {
    "name": "@unrdf/pictl-semantics",
    "version": "0.0.0-agnostic",
    "description": "PICTL Semantics Integration with @unrdf Federation - Ontology-driven process mining with cryptographic quorum consensus",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./quorum": "./src/quorum.mjs",
      "./ontology-loader": "./src/ontology-loader.mjs",
      "./result-validator": "./src/result-validator.mjs"
    },
    "path": "pictl-semantics",
    "dependencies": [
      "@comunica/query-sparql",
      "@opentelemetry/api",
      "@rdfjs/data-model",
      "@unrdf/core",
      "@unrdf/federation",
      "hash-wasm",
      "n3",
      "rdf-canonize",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "@unrdf/oxigraph",
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  "@unrdf/privacy": {
    "name": "@unrdf/privacy",
    "version": "0.0.0-agnostic",
    "description": "Differential privacy for SPARQL queries: budget accounting, Laplace/Gaussian/exponential mechanisms",
    "tier": "optional",
    "main": "./src/differential-privacy-sparql.mjs",
    "exports": {
      ".": "./src/differential-privacy-sparql.mjs",
      "./differential-privacy-sparql": "./src/differential-privacy-sparql.mjs"
    },
    "path": "privacy",
    "dependencies": [
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/project-engine": {
    "name": "@unrdf/project-engine",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Project Engine - Self-hosting Tools and Infrastructure (Development Only)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "project-engine",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/knowledge-engine",
      "@unrdf/oxigraph",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/rdf-graphql": {
    "name": "@unrdf/rdf-graphql",
    "version": "0.0.0-agnostic",
    "description": "Type-safe GraphQL interface for RDF knowledge graphs with automatic schema generation",
    "tier": "optional",
    "main": "src/adapter.mjs",
    "exports": {
      ".": "./src/adapter.mjs",
      "./schema": "./src/schema-generator.mjs",
      "./query": "./src/query-builder.mjs",
      "./resolver": "./src/resolver.mjs"
    },
    "path": "rdf-graphql",
    "dependencies": [
      "graphql",
      "@graphql-tools/schema",
      "@unrdf/oxigraph",
      "zod"
    ],
    "devDependencies": []
  },
  "@unrdf/react": {
    "name": "@unrdf/react",
    "version": "0.0.0-agnostic",
    "description": "UNRDF React - AI Semantic Analysis Tools for RDF Knowledge Graphs (Optional Extension)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./ai-semantic": "./src/ai-semantic/index.mjs",
      "./semantic-analyzer": "./src/ai-semantic/semantic-analyzer.mjs"
    },
    "path": "react",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/oxigraph",
      "lru-cache",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/receipts": {
    "name": "@unrdf/receipts",
    "version": "0.0.0-agnostic",
    "description": "KGC Receipts - Batch receipt generation with Merkle tree verification and post-quantum cryptography for knowledge graph operations",
    "tier": "extended",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./batch-receipt-generator": "./src/batch-receipt-generator.mjs",
      "./merkle-batcher": "./src/merkle-batcher.mjs",
      "./dilithium3": "./src/dilithium3.mjs",
      "./hybrid-signature": "./src/hybrid-signature.mjs",
      "./pq-signer": "./src/pq-signer.mjs",
      "./pq-merkle": "./src/pq-merkle.mjs",
      "./verifier": "./src/receipt-verifier.mjs"
    },
    "path": "receipts",
    "dependencies": [
      "@noble/curves",
      "@noble/hashes",
      "@unrdf/core",
      "@unrdf/kgc-4d",
      "@unrdf/kgc-multiverse",
      "@unrdf/oxigraph",
      "dilithium-crystals",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@vitest/coverage-v8",
      "eslint",
      "unbuild",
      "vitest"
    ]
  },
  "@unrdf/self-healing-workflows": {
    "name": "@unrdf/self-healing-workflows",
    "version": "0.0.0-agnostic",
    "description": "Automatic error recovery system with 85-95% success rate using YAWL + Daemon + Hooks",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./engine": "./src/self-healing-engine.mjs",
      "./retry": "./src/retry-strategy.mjs",
      "./circuit-breaker": "./src/circuit-breaker.mjs",
      "./recovery": "./src/recovery-actions.mjs",
      "./classifier": "./src/error-classifier.mjs",
      "./health": "./src/health-monitor.mjs",
      "./schemas": "./src/schemas.mjs"
    },
    "path": "self-healing-workflows",
    "dependencies": [
      "@opentelemetry/api",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/semantic-parts": {
    "name": "@unrdf/semantic-parts",
    "version": "0.0.0-agnostic",
    "description": "Evidence-bounded semantic software-parts graph and cross-language substitution discovery",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "semantic-parts",
    "dependencies": [],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/semantic-search": {
    "name": "@unrdf/semantic-search",
    "version": "0.0.0-agnostic",
    "description": "AI-powered semantic search over RDF knowledge graphs using vector embeddings",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./embeddings": "./src/embeddings/index.mjs",
      "./search": "./src/search/index.mjs",
      "./discovery": "./src/discovery/index.mjs"
    },
    "path": "semantic-search",
    "dependencies": [
      "@unrdf/oxigraph",
      "@xenova/transformers",
      "sharp",
      "vectra",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/serverless": {
    "name": "@unrdf/serverless",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Serverless - One-click AWS deployment for RDF applications",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./cdk": "./src/cdk/index.mjs",
      "./deploy": "./src/deploy/index.mjs",
      "./api": "./src/api/index.mjs",
      "./storage": "./src/storage/index.mjs",
      "./storage/dynamodb-core": "./src/storage/dynamodb-core.mjs",
      "./storage/dynamodb-adapter": "./src/storage/dynamodb-adapter.mjs"
    },
    "path": "serverless",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "aws-cdk-lib",
      "constructs",
      "esbuild",
      "zod"
    ],
    "devDependencies": [
      "@aws-sdk/client-dynamodb",
      "@aws-sdk/client-lambda",
      "@aws-sdk/lib-dynamodb",
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/spatial-kg": {
    "name": "@unrdf/spatial-kg",
    "version": "0.0.0-agnostic",
    "description": "Spatial Knowledge Graphs - WebXR-enabled 3D visualization and navigation of RDF knowledge graphs",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./engine": "./src/spatial-kg-engine.mjs",
      "./layout": "./src/layout-3d.mjs",
      "./renderer": "./src/webxr-renderer.mjs",
      "./query": "./src/spatial-query.mjs",
      "./gestures": "./src/gesture-controller.mjs",
      "./collaboration": "./src/collaboration.mjs",
      "./lod": "./src/lod-manager.mjs",
      "./schemas": "./src/schemas.mjs"
    },
    "path": "spatial-kg",
    "dependencies": [
      "@unrdf/core",
      "@opentelemetry/api",
      "three",
      "d3-force-3d",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "@types/three",
      "vitest"
    ]
  },
  "@unrdf/streaming": {
    "name": "@unrdf/streaming",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Streaming - Change Feeds and Real-time Synchronization",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./processor": "./src/processor.mjs",
      "./shacl-core": "./src/shacl-core.mjs",
      "./validate": "./src/validate.mjs",
      "./checkpointed-pipeline": "./src/checkpointed-pipeline.mjs"
    },
    "path": "streaming",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/oxigraph",
      "citty",
      "hash-wasm",
      "lru-cache",
      "ws",
      "zod"
    ],
    "devDependencies": [
      "@rdfjs/data-model",
      "@types/node",
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  "@unrdf/temporal-discovery": {
    "name": "@unrdf/temporal-discovery",
    "version": "0.0.0-agnostic",
    "description": "Temporal knowledge discovery for RDF graphs - pattern mining, anomaly detection, trend analysis",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./pattern-miner": "./src/pattern-miner.mjs",
      "./anomaly-detector": "./src/anomaly-detector.mjs",
      "./trend-analyzer": "./src/trend-analyzer.mjs",
      "./correlation-finder": "./src/correlation-finder.mjs",
      "./changepoint-detector": "./src/changepoint-detector.mjs",
      "./engine": "./src/temporal-discovery-engine.mjs"
    },
    "path": "temporal-discovery",
    "dependencies": [
      "@unrdf/kgc-4d",
      "@unrdf/semantic-search",
      "@unrdf/graph-analytics",
      "@opentelemetry/api",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/test-utils": {
    "name": "@unrdf/test-utils",
    "version": "0.0.0-agnostic",
    "description": "Shared test utilities and fixtures for unrdf packages",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "test-utils",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/oxigraph"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/v6-compat": {
    "name": "@unrdf/v6-compat",
    "version": "0.0.0-agnostic",
    "description": "UNRDF v6 Compatibility Layer - v5 to v6 migration bridge with adapters and lint rules",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./adapters": "./src/adapters.mjs",
      "./lint-rules": "./src/lint-rules.mjs",
      "./schema-generator": "./src/schema-generator.mjs",
      "./schema-codec": "./src/schema-codec.mjs"
    },
    "path": "v6-compat",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "@unrdf/v6-core",
      "glob",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "eslint",
      "vitest"
    ]
  },
  "@unrdf/v6-core": {
    "name": "@unrdf/v6-core",
    "version": "0.0.0-agnostic",
    "description": "UNRDF v6 Core - ΔGate control plane, unified receipts, and delta contracts",
    "tier": "essential",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./browser": "./src/browser.mjs",
      "./browser/receipt-store": "./src/browser/receipt-store.mjs",
      "./deltagate": "./src/deltagate.mjs",
      "./schemas": "./src/schemas.mjs",
      "./receipts": "./src/receipts.mjs",
      "./receipts/base-receipt": "./src/receipts/base-receipt.mjs",
      "./receipts/merkle": "./src/receipts/merkle/tree.mjs",
      "./receipt-pattern": "./src/receipt-pattern.mjs",
      "./delta": "./src/delta/index.mjs",
      "./delta/schema": "./src/delta/schema.mjs",
      "./delta/gate": "./src/delta/gate.mjs",
      "./grammar": "./src/grammar/index.mjs",
      "./cli": "./src/cli/index.mjs",
      "./cli/nouns": "./src/cli/nouns.mjs",
      "./cli/verbs": "./src/cli/verbs.mjs",
      "./cli/spine": "./src/cli/spine.mjs",
      "./cli/commands/receipt": "./src/cli/commands/receipt.mjs",
      "./cli/commands/delta": "./src/cli/commands/delta.mjs"
    },
    "path": "v6-core",
    "dependencies": [
      "@unrdf/kgc-substrate",
      "@unrdf/kgc-cli",
      "@unrdf/kgc-4d",
      "@unrdf/hooks",
      "@unrdf/oxigraph",
      "@unrdf/blockchain",
      "citty",
      "zod",
      "hash-wasm",
      "mustache"
    ],
    "devDependencies": [
      "@types/node",
      "eslint",
      "typescript"
    ]
  },
  "@unrdf/validation": {
    "name": "@unrdf/validation",
    "version": "0.0.0-agnostic",
    "description": "OTEL validation framework for UNRDF development",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "validation",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/knowledge-engine",
      "zod"
    ],
    "devDependencies": []
  },
  "@unrdf/zkp": {
    "name": "@unrdf/zkp",
    "version": "0.0.0-agnostic",
    "description": "Zero-Knowledge SPARQL - Privacy-preserving query proofs using zk-SNARKs",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./prover": "./src/sparql-zkp-prover.mjs",
      "./circuit": "./src/circuit-compiler.mjs",
      "./groth16": "./src/groth16-prover.mjs",
      "./verifier": "./src/groth16-verifier.mjs",
      "./schemas": "./src/schemas.mjs"
    },
    "path": "zkp",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@opentelemetry/api",
      "zod",
      "hash-wasm",
      "snarkjs",
      "circomlibjs",
      "sparqljs"
    ],
    "devDependencies": [
      "@types/node",
      "eslint",
      "prettier",
      "vitest",
      "@vitest/coverage-v8"
    ]
  }
};

const REGISTRY = {
  packages: Object.values(PACKAGES),
  essential: [
  {
    "name": "@unrdf/core",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./rdf": "./src/rdf/index.mjs",
      "./rdf/minimal-n3-integration": "./src/rdf/minimal-n3-integration.mjs",
      "./rdf/n3-justified-only": "./src/rdf/n3-justified-only.mjs",
      "./sparql": "./src/sparql/index.mjs",
      "./sparql/executor-sync": "./src/sparql/executor-sync.mjs",
      "./sparql/embeddings": "./src/sparql/embeddings.mjs",
      "./sparql/semantic-executor": "./src/sparql/semantic-executor.mjs",
      "./index/hnsw": "./src/index/hnsw.mjs",
      "./types": "./src/types.mjs",
      "./constants": "./src/constants.mjs",
      "./validation": "./src/validation/index.mjs",
      "./health": "./src/health.mjs",
      "./logger": "./src/logger.mjs",
      "./metrics": "./src/metrics.mjs",
      "./security": "./src/security.mjs",
      "./security-schemas": "./src/security-schemas.mjs",
      "./utils/sparql-utils": "./src/utils/sparql-utils.mjs",
      "./utils/semantic-bridge": "./src/utils/semantic-bridge.mjs",
      "./utils/lockchain-writer": "./src/utils/lockchain-writer.mjs",
      "./viz/graph-visualizer": "./src/viz/graph-visualizer.mjs",
      "./viz/query-explainer": "./src/viz/query-explainer.mjs",
      "./debug/rdf-inspector": "./src/debug/rdf-inspector.mjs",
      "./capabilities": "./src/capability-ledger.mjs",
      "./capability-graph": "./src/capability-graph.mjs",
      "./evidence": "./src/evidence-store.mjs",
      "./receipts": "./src/receipt-chain.mjs",
      "./execution-plan": "./src/execution-plan.mjs",
      "./admission": "./src/admission-boundary.mjs",
      "./command-verifier": "./src/command-verifier.mjs",
      "./replay": "./src/replay-runner.mjs",
      "./transaction-core": "./src/utils/transaction-core.mjs"
    },
    "path": "core",
    "dependencies": [
      "@noble/hashes",
      "@opentelemetry/api",
      "@rdfjs/data-model",
      "@rdfjs/namespace",
      "@rdfjs/serializer-jsonld",
      "@rdfjs/serializer-turtle",
      "@rdfjs/to-ntriples",
      "@unrdf/oxigraph",
      "async-mutex",
      "hnswlib-node",
      "jsonld",
      "n3",
      "onnxruntime-node",
      "oxigraph",
      "rdf-canonize",
      "rdf-ext",
      "rdf-validate-shacl",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/hooks",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Knowledge Hooks - Policy Definition and Execution Framework",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./define": "./src/define.mjs",
      "./executor": "./src/executor.mjs",
      "./parallel-executor": "./src/hooks/parallel-executor.mjs",
      "./dependency-graph": "./src/hooks/dependency-graph.mjs",
      "./worker-pool": "./src/hooks/worker-pool.mjs"
    },
    "path": "hooks",
    "dependencies": [
      "@noble/hashes",
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/otel",
      "@unrdf/oxigraph",
      "citty",
      "eyereasoner",
      "oxigraph",
      "rdf-validate-shacl",
      "sparqljs",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/kgc-4d",
    "version": "0.0.0-agnostic",
    "description": "KGC 4D Datum & Universe Freeze Engine - Nanosecond-precision event logging with Git-backed snapshots",
    "tier": "essential",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./client": "./src/client.mjs",
      "./hdit": "./src/hdit/index.mjs"
    },
    "path": "kgc-4d",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "async-mutex",
      "hash-wasm",
      "isomorphic-git",
      "zod"
    ],
    "devDependencies": [
      "comment-parser",
      "simple-statistics",
      "tinybench",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/oxigraph",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./store": "./src/store.mjs",
      "./types": "./src/types.mjs",
      "./query-cache": "./src/query-cache.mjs",
      "./sparql-star": "./src/sparql-star.mjs",
      "./store-receipts": "./src/store-receipts.mjs"
    },
    "path": "oxigraph",
    "dependencies": [
      "oxigraph",
      "zod",
      "@unrdf/v6-core"
    ],
    "devDependencies": [
      "@types/node",
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/streaming",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Streaming - Change Feeds and Real-time Synchronization",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./processor": "./src/processor.mjs",
      "./shacl-core": "./src/shacl-core.mjs",
      "./validate": "./src/validate.mjs",
      "./checkpointed-pipeline": "./src/checkpointed-pipeline.mjs"
    },
    "path": "streaming",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/oxigraph",
      "citty",
      "hash-wasm",
      "lru-cache",
      "ws",
      "zod"
    ],
    "devDependencies": [
      "@rdfjs/data-model",
      "@types/node",
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/v6-core",
    "version": "0.0.0-agnostic",
    "description": "UNRDF v6 Core - ΔGate control plane, unified receipts, and delta contracts",
    "tier": "essential",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./browser": "./src/browser.mjs",
      "./browser/receipt-store": "./src/browser/receipt-store.mjs",
      "./deltagate": "./src/deltagate.mjs",
      "./schemas": "./src/schemas.mjs",
      "./receipts": "./src/receipts.mjs",
      "./receipts/base-receipt": "./src/receipts/base-receipt.mjs",
      "./receipts/merkle": "./src/receipts/merkle/tree.mjs",
      "./receipt-pattern": "./src/receipt-pattern.mjs",
      "./delta": "./src/delta/index.mjs",
      "./delta/schema": "./src/delta/schema.mjs",
      "./delta/gate": "./src/delta/gate.mjs",
      "./grammar": "./src/grammar/index.mjs",
      "./cli": "./src/cli/index.mjs",
      "./cli/nouns": "./src/cli/nouns.mjs",
      "./cli/verbs": "./src/cli/verbs.mjs",
      "./cli/spine": "./src/cli/spine.mjs",
      "./cli/commands/receipt": "./src/cli/commands/receipt.mjs",
      "./cli/commands/delta": "./src/cli/commands/delta.mjs"
    },
    "path": "v6-core",
    "dependencies": [
      "@unrdf/kgc-substrate",
      "@unrdf/kgc-cli",
      "@unrdf/kgc-4d",
      "@unrdf/hooks",
      "@unrdf/oxigraph",
      "@unrdf/blockchain",
      "citty",
      "zod",
      "hash-wasm",
      "mustache"
    ],
    "devDependencies": [
      "@types/node",
      "eslint",
      "typescript"
    ]
  }
],
  extended: [
  {
    "name": "@unrdf/cli",
    "version": "0.0.0-agnostic",
    "description": "UNRDF CLI - Command-line Tools for Graph Operations and Context Management",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./commands": "./src/commands/index.mjs"
    },
    "path": "cli",
    "dependencies": [
      "@iarna/toml",
      "archiver",
      "@unrdf/core",
      "@unrdf/daemon",
      "@unrdf/federation",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/kgc-swarm",
      "@unrdf/knowledge-engine",
      "@unrdf/oxigraph",
      "@unrdf/project-engine",
      "@unrdf/receipts",
      "@unrdf/streaming",
      "citty",
      "glob",
      "gray-matter",
      "js-yaml",
      "nunjucks",
      "table",
      "yaml",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "citty-test-utils",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/consensus",
    "version": "0.0.0-agnostic",
    "description": "Production-grade Raft consensus for distributed workflow coordination",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./raft": "./src/raft/raft-coordinator.mjs",
      "./cluster": "./src/membership/cluster-manager.mjs",
      "./state": "./src/state/distributed-state-machine.mjs",
      "./transport": "./src/transport/websocket-transport.mjs"
    },
    "path": "consensus",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/federation",
      "msgpackr",
      "ws",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "@types/ws",
      "eslint",
      "prettier",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/federation",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Federation - Distributed RDF Query with RAFT Consensus and Multi-Master Replication",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./coordinator": "./src/federation/coordinator.mjs",
      "./advanced-sparql": "./src/advanced-sparql-federation.mjs",
      "./ml/predictor": "./src/ml/predictor.mjs",
      "./query-planner-core": "./src/federation/query-planner-core.mjs"
    },
    "path": "federation",
    "dependencies": [
      "@comunica/query-sparql",
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/hooks",
      "prom-client",
      "zod"
    ],
    "devDependencies": [
      "@opentelemetry/sdk-trace-base",
      "@types/node",
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/kgc-runtime",
    "version": "0.0.0-agnostic",
    "description": "KGC governance runtime with comprehensive Zod schemas and work item system",
    "tier": "extended",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./schemas": "./src/schemas.mjs",
      "./work-item": "./src/work-item.mjs",
      "./plugin-manager": "./src/plugin-manager.mjs",
      "./plugin-isolation": "./src/plugin-isolation.mjs",
      "./api-version": "./src/api-version.mjs"
    },
    "path": "kgc-runtime",
    "dependencies": [
      "@unrdf/oxigraph",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/kgc-substrate",
    "version": "0.0.0-agnostic",
    "description": "KGC Substrate - Deterministic, hash-stable KnowledgeStore with immutable append-only log",
    "tier": "extended",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./types": "./src/types.mjs",
      "./KnowledgeStore": "./src/KnowledgeStore.mjs"
    },
    "path": "kgc-substrate",
    "dependencies": [
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "@unrdf/core",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest",
      "@vitest/coverage-v8"
    ]
  },
  {
    "name": "@unrdf/knowledge-engine",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Knowledge Engine - Rule Engine, Inference, and Pattern Matching (Optional Extension)",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./query": "./src/query.mjs",
      "./canonicalize": "./src/canonicalize.mjs",
      "./parse": "./src/parse.mjs",
      "./ai-search": "./src/ai-enhanced-search.mjs"
    },
    "path": "knowledge-engine",
    "dependencies": [
      "@comunica/query-sparql",
      "@iarna/toml",
      "@noble/hashes",
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/streaming",
      "@xenova/transformers",
      "eyereasoner",
      "lru-cache",
      "rdf-canonize",
      "rdf-ext",
      "rdf-validate-shacl",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/receipts",
    "version": "0.0.0-agnostic",
    "description": "KGC Receipts - Batch receipt generation with Merkle tree verification and post-quantum cryptography for knowledge graph operations",
    "tier": "extended",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./batch-receipt-generator": "./src/batch-receipt-generator.mjs",
      "./merkle-batcher": "./src/merkle-batcher.mjs",
      "./dilithium3": "./src/dilithium3.mjs",
      "./hybrid-signature": "./src/hybrid-signature.mjs",
      "./pq-signer": "./src/pq-signer.mjs",
      "./pq-merkle": "./src/pq-merkle.mjs",
      "./verifier": "./src/receipt-verifier.mjs"
    },
    "path": "receipts",
    "dependencies": [
      "@noble/curves",
      "@noble/hashes",
      "@unrdf/core",
      "@unrdf/kgc-4d",
      "@unrdf/kgc-multiverse",
      "@unrdf/oxigraph",
      "dilithium-crystals",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@vitest/coverage-v8",
      "eslint",
      "unbuild",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/v6-compat",
    "version": "0.0.0-agnostic",
    "description": "UNRDF v6 Compatibility Layer - v5 to v6 migration bridge with adapters and lint rules",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./adapters": "./src/adapters.mjs",
      "./lint-rules": "./src/lint-rules.mjs",
      "./schema-generator": "./src/schema-generator.mjs",
      "./schema-codec": "./src/schema-codec.mjs"
    },
    "path": "v6-compat",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "@unrdf/v6-core",
      "glob",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "eslint",
      "vitest"
    ]
  }
],
  optional: [
  {
    "name": "@unrdf/ai-ml-innovations",
    "version": "0.0.0-agnostic",
    "description": "Novel AI/ML integration patterns for UNRDF knowledge graphs",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./temporal-gnn": "./src/temporal-gnn.mjs",
      "./neural-symbolic": "./src/neural-symbolic-reasoner.mjs",
      "./federated": "./src/federated-embeddings.mjs"
    },
    "path": "ai-ml-innovations",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/knowledge-engine",
      "@unrdf/semantic-search",
      "@unrdf/ml-inference",
      "@unrdf/kgc-4d",
      "@unrdf/v6-core",
      "@opentelemetry/api",
      "zod"
    ],
    "devDependencies": [
      "vitest",
      "eslint"
    ]
  },
  {
    "name": "@unrdf/atomvm",
    "version": "0.0.0-agnostic",
    "description": "AtomVM runtimes, OTP patterns, and receipted swarm control planes for browser and Node.js",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./service-worker-manager": "./src/service-worker-manager.mjs",
      "./assets": "./src/assets.mjs",
      "./avm-packer": "./src/avm-packer.mjs",
      "./continuum": "./src/continuum/index.mjs",
      "./continuum/browser": "./src/continuum/browser-client.mjs"
    },
    "path": "atomvm",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/oxigraph",
      "@unrdf/receipts",
      "@unrdf/streaming",
      "@unrdf/v6-core",
      "coi-serviceworker",
      "zod"
    ],
    "devDependencies": [
      "@playwright/test",
      "@vitest/browser",
      "jsdom",
      "vite",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/blockchain",
    "version": "0.0.0-agnostic",
    "description": "Blockchain integration for UNRDF - Cryptographic receipt anchoring and audit trails",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./anchoring": "./src/anchoring/receipt-anchorer.mjs",
      "./contracts": "./src/contracts/workflow-verifier.mjs",
      "./merkle": "./src/merkle/merkle-proof-generator.mjs"
    },
    "path": "blockchain",
    "dependencies": [
      "@noble/hashes",
      "@unrdf/kgc-4d",
      "ethers",
      "merkletreejs",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/caching",
    "version": "0.0.0-agnostic",
    "description": "Multi-layer caching system for RDF queries with Redis and LRU",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./layers": "./src/layers/multi-layer-cache.mjs",
      "./invalidation": "./src/invalidation/dependency-tracker.mjs",
      "./query": "./src/query/sparql-cache.mjs"
    },
    "path": "caching",
    "dependencies": [
      "@unrdf/oxigraph",
      "ioredis",
      "lru-cache",
      "msgpackr",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/chatman-equation",
    "version": "0.0.0-agnostic",
    "description": "Chatman Equation documentation generation using Tera-compatible templates",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./template-engine": "./src/template-engine.mjs",
      "./config": "./src/config-loader.mjs",
      "./filters": "./src/filters.mjs"
    },
    "path": "chatman-equation",
    "dependencies": [
      "@iarna/toml",
      "@unrdf/core",
      "@unrdf/kgn",
      "@unrdf/oxigraph",
      "glob",
      "nunjucks",
      "smol-toml",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/codegen",
    "version": "0.0.0-agnostic",
    "description": "Code generation and metaprogramming tools for UNRDF",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./sparql-types": "./src/sparql-type-generator.mjs",
      "./meta-templates": "./src/meta-template-engine.mjs",
      "./property-tests": "./src/property-test-generator.mjs"
    },
    "path": "codegen",
    "dependencies": [
      "fast-check",
      "zod"
    ],
    "devDependencies": [
      "nunjucks",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/collab",
    "version": "0.0.0-agnostic",
    "description": "Real-time collaborative RDF editing using CRDTs (Yjs) with offline-first architecture",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./crdt": "./src/crdt/index.mjs",
      "./sync": "./src/sync/index.mjs",
      "./composables": "./src/composables/index.mjs"
    },
    "path": "collab",
    "dependencies": [
      "@unrdf/core",
      "yjs",
      "y-websocket",
      "y-indexeddb",
      "lib0",
      "zod",
      "ws"
    ],
    "devDependencies": [
      "@types/node",
      "@types/ws",
      "vitest",
      "vue"
    ]
  },
  {
    "name": "@unrdf/composables",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Composables - Vue 3 Composables for Reactive RDF State (Optional Extension)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./graph": "./src/graph.mjs",
      "./delta": "./src/delta.mjs"
    },
    "path": "composables",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/streaming",
      "rdf-canonize",
      "unctx",
      "vue"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/daemon",
    "version": "0.0.0-agnostic",
    "description": "Background daemon for managing scheduled tasks and event-driven operations",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./daemon": "./src/daemon.mjs",
      "./mcp": "./src/mcp/index.mjs",
      "./schemas": "./src/schemas.mjs",
      "./trigger-evaluator": "./src/trigger-evaluator.mjs",
      "./v6-deltagate": "./src/integrations/v6-deltagate.mjs",
      "./middleware/rate-limiter": "./src/middleware/rate-limiter.mjs",
      "./middleware/rate-limiter-schema": "./src/middleware/rate-limiter.schema.mjs",
      "./integrations/nitro-tasks": "./src/integrations/nitro-tasks.mjs",
      "./integrations/kgc-4d-sourcing": "./src/integrations/kgc-4d-sourcing.mjs",
      "./integrations/kgc-4d-merkle": "./src/integrations/kgc-4d-merkle.mjs"
    },
    "path": "daemon",
    "dependencies": [
      "@ai-sdk/groq",
      "@grpc/grpc-js",
      "@modelcontextprotocol/sdk",
      "@opentelemetry/api",
      "@opentelemetry/exporter-metrics-otlp-grpc",
      "@opentelemetry/exporter-trace-otlp-grpc",
      "@opentelemetry/resources",
      "@opentelemetry/sdk-metrics",
      "@opentelemetry/sdk-node",
      "@opentelemetry/sdk-trace-node",
      "@opentelemetry/semantic-conventions",
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/otel",
      "ai",
      "cron-parser",
      "hash-wasm",
      "nunjucks",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/dark-matter",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Dark Matter - Query Optimization and Performance Analysis (Optional Extension)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./analyzer": "./src/dark-matter/query-analyzer.mjs"
    },
    "path": "dark-matter",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "typhonjs-escomplex",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/decision-fabric",
    "version": "0.0.0-agnostic",
    "description": "Hyperdimensional Decision Fabric - Intent-to-Outcome transformation engine using μ-operators",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./engine": "./src/engine.mjs",
      "./operators": "./src/operators.mjs",
      "./socratic": "./src/socratic-agent.mjs",
      "./pareto": "./src/pareto-analyzer.mjs"
    },
    "path": "decision-fabric",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/knowledge-engine",
      "@unrdf/oxigraph",
      "@unrdf/project-engine",
      "@unrdf/streaming",
      "@unrdf/validation",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "eslint",
      "jest"
    ]
  },
  {
    "name": "@unrdf/diataxis-kit",
    "version": "0.0.0-agnostic",
    "description": "Diátaxis documentation kit for monorepo package inventory and deterministic doc scaffold generation",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./inventory": "./src/inventory.mjs",
      "./evidence": "./src/evidence.mjs",
      "./classify": "./src/classify.mjs",
      "./scaffold": "./src/scaffold.mjs",
      "./stable-json": "./src/stable-json.mjs",
      "./hash": "./src/hash.mjs"
    },
    "path": "diataxis-kit",
    "dependencies": [],
    "devDependencies": []
  },
  {
    "name": "@unrdf/domain",
    "version": "0.0.0-agnostic",
    "description": "Domain models and types for UNRDF",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "domain",
    "dependencies": [],
    "devDependencies": []
  },
  {
    "name": "@unrdf/engine-gateway",
    "version": "0.0.0-agnostic",
    "description": "μ(O) Engine Gateway - Enforcement layer for Oxigraph-first, N3-minimal RDF processing",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./gateway": "./src/gateway.mjs",
      "./operation-detector": "./src/operation-detector.mjs",
      "./validators": "./src/validators.mjs"
    },
    "path": "engine-gateway",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/event-automation",
    "version": "0.0.0-agnostic",
    "description": "Event-driven automation for v6.1.0 - automatic delta processing with receipts",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./engine": "./src/event-automation-engine.mjs",
      "./delta-processor": "./src/delta-processor.mjs",
      "./receipt-tracker": "./src/receipt-tracker.mjs",
      "./policy-enforcer": "./src/policy-enforcer.mjs",
      "./schemas": "./src/schemas.mjs"
    },
    "path": "event-automation",
    "dependencies": [
      "@unrdf/daemon",
      "@unrdf/v6-core",
      "@unrdf/hooks",
      "@unrdf/receipts",
      "@opentelemetry/api",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest",
      "eslint"
    ]
  },
  {
    "name": "@unrdf/fusion",
    "version": "0.0.0-agnostic",
    "description": "Unified integration layer for 7-day UNRDF innovation - KGC-4D, blockchain, hooks, caching",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "fusion",
    "dependencies": [
      "@unrdf/blockchain",
      "@unrdf/caching",
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "graphql",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/geosparql",
    "version": "0.0.0-agnostic",
    "description": "OGC GeoSPARQL standard compliance for spatial RDF queries",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./geometry": "./src/geometry.mjs",
      "./spatial-relations": "./src/spatial-relations.mjs",
      "./distance": "./src/distance.mjs",
      "./rtree-index": "./src/rtree-index.mjs",
      "./crs": "./src/crs.mjs",
      "./query-functions": "./src/query-functions.mjs"
    },
    "path": "geosparql",
    "dependencies": [
      "@turf/turf",
      "rbush",
      "@unrdf/oxigraph",
      "@opentelemetry/api",
      "zod"
    ],
    "devDependencies": [
      "vitest",
      "eslint"
    ]
  },
  {
    "name": "@unrdf/graph-analytics",
    "version": "0.0.0-agnostic",
    "description": "Advanced graph analytics for RDF knowledge graphs using graphlib",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./converter": "./src/converter/rdf-to-graph.mjs",
      "./centrality": "./src/centrality/pagerank-analyzer.mjs",
      "./paths": "./src/paths/relationship-finder.mjs",
      "./clustering": "./src/clustering/community-detector.mjs"
    },
    "path": "graph-analytics",
    "dependencies": [
      "@dagrejs/graphlib",
      "graphlib",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/integration-tests",
    "version": "0.0.0-agnostic",
    "description": "Phase 5: Comprehensive Integration & Adversarial Tests (75 tests)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {},
    "path": "integration-tests",
    "dependencies": [
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/kgc-multiverse",
      "@unrdf/federation",
      "@unrdf/streaming",
      "@unrdf/oxigraph",
      "@unrdf/receipts",
      "@unrdf/core",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@vitest/coverage-v8",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/kgc-4d-playground",
    "version": "0.0.0-agnostic",
    "description": "KGC-4D Playground - Shard-Based Architecture Demo with Perfect Client/Server Relationship",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {},
    "path": "kgc-4d-playground",
    "dependencies": [
      "@monaco-editor/react",
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "@unrdf/validation",
      "@xyflow/react",
      "clsx",
      "d3-scale",
      "elkjs",
      "framer-motion",
      "lucide-react",
      "next",
      "react",
      "react-dom",
      "react-force-graph-3d",
      "tailwind-merge",
      "three",
      "ws",
      "zod"
    ],
    "devDependencies": [
      "@playwright/test",
      "@types/node",
      "@types/react",
      "@types/ws",
      "autoprefixer",
      "eslint",
      "eslint-config-next",
      "postcss",
      "tailwindcss",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/kgc-cli",
    "version": "0.0.0-agnostic",
    "description": "KGC CLI - Deterministic extension registry for ~40 workspace packages",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./registry": "./src/lib/registry.mjs",
      "./manifest": "./src/manifest/extensions.mjs",
      "./latex": "./src/lib/latex/index.mjs",
      "./latex/schemas": "./src/lib/latex/schemas.mjs"
    },
    "path": "kgc-cli",
    "dependencies": [
      "citty",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/kgc-docs",
    "version": "0.0.0-agnostic",
    "description": "KGC Markdown parser and dynamic documentation generator with proof anchoring",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/kgc-markdown.mjs",
      "./parser": "./src/parser.mjs",
      "./renderer": "./src/renderer.mjs",
      "./proof": "./src/proof.mjs",
      "./reference-validator": "./src/reference-validator.mjs",
      "./changelog-generator": "./src/changelog-generator.mjs",
      "./executor": "./src/executor.mjs"
    },
    "path": "kgc-docs",
    "dependencies": [
      "@unrdf/kgc-runtime",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/kgc-multiverse",
    "version": "0.0.0-agnostic",
    "description": "KGC Multiverse - Universe branching, forking, and morphism algebra for knowledge graphs",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./universe-manager": "./src/universe-manager.mjs",
      "./morphism": "./src/morphism.mjs",
      "./guards": "./src/guards.mjs",
      "./q-star": "./src/q-star.mjs",
      "./composition": "./src/composition.mjs",
      "./parallel-executor": "./src/parallel-executor.mjs",
      "./worker-task": "./src/worker-task.mjs",
      "./cli-10k": "./src/cli-10k.mjs"
    },
    "path": "kgc-multiverse",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/kgc-4d",
      "@unrdf/receipts",
      "hash-wasm",
      "piscina",
      "zod"
    ],
    "devDependencies": [
      "@vitest/coverage-v8",
      "vitest",
      "eslint",
      "unbuild"
    ]
  },
  {
    "name": "@unrdf/kgc-probe",
    "version": "0.0.0-agnostic",
    "description": "KGC Probe - Automated knowledge graph integrity scanning with 10 agents and artifact validation",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./orchestrator": "./src/orchestrator.mjs",
      "./guards": "./src/guards.mjs",
      "./agents": "./src/agents/index.mjs",
      "./storage": "./src/storage/index.mjs",
      "./types": "./src/types.mjs",
      "./artifact": "./src/artifact.mjs",
      "./cli": "./src/cli.mjs",
      "./utils": "./src/utils/index.mjs",
      "./utils/logger": "./src/utils/logger.mjs",
      "./utils/errors": "./src/utils/errors.mjs",
      "./orchestration-core": "./src/orchestration-core.mjs"
    },
    "path": "kgc-probe",
    "dependencies": [
      "@noble/hashes",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/kgc-substrate",
      "@unrdf/oxigraph",
      "@unrdf/v6-core",
      "hash-wasm",
      "n3",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest",
      "@vitest/coverage-v8"
    ]
  },
  {
    "name": "@unrdf/kgc-swarm",
    "version": "0.0.0-agnostic",
    "description": "Multi-agent template orchestration with cryptographic receipts - KGC planning meets kgn rendering",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./guards": "./src/guards.mjs",
      "./orchestrator": "./src/orchestrator.mjs",
      "./token-generator": "./src/token-generator.mjs",
      "./compressor": "./src/compressor.mjs",
      "./tracker": "./src/tracker.mjs",
      "./guardian": "./src/guardian.mjs",
      "./transport": "./src/transport/hypercore-transport.mjs"
    },
    "path": "kgc-swarm",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/kgc-substrate",
      "@unrdf/kgn",
      "@unrdf/knowledge-engine",
      "@unrdf/kgc-4d",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest",
      "eslint",
      "typescript",
      "fast-check"
    ]
  },
  {
    "name": "@unrdf/kgc-tools",
    "version": "0.0.0-agnostic",
    "description": "KGC Tools - Verification, freeze, and replay utilities for KGC capsules",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./verify": "./src/verify.mjs",
      "./freeze": "./src/freeze.mjs",
      "./replay": "./src/replay.mjs",
      "./list": "./src/list.mjs",
      "./tool-wrapper": "./src/tool-wrapper.mjs"
    },
    "path": "kgc-tools",
    "dependencies": [
      "@unrdf/kgc-4d",
      "@unrdf/kgc-runtime",
      "@unrdf/core",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/kgn",
    "version": "0.0.0-agnostic",
    "description": "Deterministic Nunjucks template system with custom filters and frontmatter support",
    "tier": "optional",
    "main": "dist/index.mjs",
    "exports": {
      ".": {
        "import": "./dist/index.mjs",
        "types": "./dist/index.d.ts"
      },
      "./engine": {
        "import": "./src/engine/index.js"
      },
      "./filters": {
        "import": "./src/filters/index.js"
      },
      "./renderer": {
        "import": "./src/renderer/index.js"
      },
      "./linter": {
        "import": "./src/linter/index.js"
      },
      "./templates/*": "./src/templates/*"
    },
    "path": "kgn",
    "dependencies": [
      "@unrdf/core",
      "consola",
      "fs-extra",
      "glob",
      "gray-matter",
      "nunjucks",
      "yaml",
      "zod"
    ],
    "devDependencies": [
      "@amiceli/vitest-cucumber",
      "@babel/parser",
      "@babel/traverse",
      "comment-parser",
      "cors",
      "eslint",
      "nodemon",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/manufacturing",
    "version": "0.0.0-agnostic",
    "description": "μ(O) Manufacturing Operator Runtime — composable operators for deterministic artifact manufacturing",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./operators": "./src/operators/index.mjs",
      "./pipeline": "./src/pipeline/index.mjs",
      "./gate": "./src/gate/index.mjs",
      "./causality": "./src/causality/index.mjs",
      "./artifact": "./src/artifact/index.mjs",
      "./repository-fact-accounting": "./src/repository-fact-accounting.mjs",
      "./git-repository-facts": "./src/git-repository-facts.mjs"
    },
    "path": "manufacturing",
    "dependencies": [
      "@unrdf/core",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/ml-inference",
    "version": "0.0.0-agnostic",
    "description": "UNRDF ML Inference - High-performance ONNX model inference pipeline for RDF streams",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./runtime": "./src/runtime/onnx-runner.mjs",
      "./pipeline": "./src/pipeline/streaming-inference.mjs",
      "./registry": "./src/registry/model-registry.mjs"
    },
    "path": "ml-inference",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/streaming",
      "@unrdf/oxigraph",
      "onnxruntime-node",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/ml-versioning",
    "version": "0.0.0-agnostic",
    "description": "ML Model Versioning System using TensorFlow.js and UNRDF KGC-4D time-travel capabilities",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./version-store": "./src/version-store.mjs",
      "./tf": "./src/tf.mjs",
      "./examples/image-classifier": "./src/examples/image-classifier.mjs"
    },
    "path": "ml-versioning",
    "dependencies": [
      "@tensorflow/tfjs",
      "@tensorflow/tfjs-backend-cpu",
      "@tensorflow/tfjs-node",
      "@unrdf/core",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/observability",
    "version": "0.0.0-agnostic",
    "description": "Innovative Prometheus/Grafana observability dashboard for UNRDF distributed workflows",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./metrics": "./src/metrics/workflow-metrics.mjs",
      "./exporters": "./src/exporters/grafana-exporter.mjs",
      "./alerts": "./src/alerts/alert-manager.mjs"
    },
    "path": "observability",
    "dependencies": [
      "@opentelemetry/api",
      "@opentelemetry/exporter-prometheus",
      "@opentelemetry/sdk-metrics",
      "express",
      "hash-wasm",
      "prom-client",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/otel",
    "version": "0.0.0-agnostic",
    "description": "OpenTelemetry integration for UNRDF using pm4py-rust telemetry infrastructure",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./attributes": "./src/generated/attributes.mjs",
      "./metrics": "./src/generated/metrics.mjs",
      "./pm4py": "./src/pm4py.mjs",
      "./monitoring": "./src/monitoring.mjs",
      "./validation": "./src/validation/index.mjs",
      "./collector-config": "./deploy/otel-collector-config.yaml",
      "./ocel": "./src/ocel/index.mjs",
      "./conformance": "./src/conformance/index.mjs"
    },
    "path": "otel",
    "dependencies": [
      "@opentelemetry/api",
      "@opentelemetry/semantic-conventions",
      "@unrdf/manufacturing"
    ],
    "devDependencies": []
  },
  {
    "name": "@unrdf/pictl-algorithms",
    "version": "0.0.0-agnostic",
    "description": "PICTL Process Mining Algorithms for UNRDF Federation - OCEL discovery, conformance, and prediction via WASM",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./pictl-wrapper": "./src/pictl-wrapper.mjs"
    },
    "path": "pictl-algorithms",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/pictl-semantics",
      "zod"
    ],
    "devDependencies": [
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/pictl-semantics",
    "version": "0.0.0-agnostic",
    "description": "PICTL Semantics Integration with @unrdf Federation - Ontology-driven process mining with cryptographic quorum consensus",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./quorum": "./src/quorum.mjs",
      "./ontology-loader": "./src/ontology-loader.mjs",
      "./result-validator": "./src/result-validator.mjs"
    },
    "path": "pictl-semantics",
    "dependencies": [
      "@comunica/query-sparql",
      "@opentelemetry/api",
      "@rdfjs/data-model",
      "@unrdf/core",
      "@unrdf/federation",
      "hash-wasm",
      "n3",
      "rdf-canonize",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "@unrdf/oxigraph",
      "@unrdf/test-utils",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/privacy",
    "version": "0.0.0-agnostic",
    "description": "Differential privacy for SPARQL queries: budget accounting, Laplace/Gaussian/exponential mechanisms",
    "tier": "optional",
    "main": "./src/differential-privacy-sparql.mjs",
    "exports": {
      ".": "./src/differential-privacy-sparql.mjs",
      "./differential-privacy-sparql": "./src/differential-privacy-sparql.mjs"
    },
    "path": "privacy",
    "dependencies": [
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/project-engine",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Project Engine - Self-hosting Tools and Infrastructure (Development Only)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "project-engine",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/knowledge-engine",
      "@unrdf/oxigraph",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/rdf-graphql",
    "version": "0.0.0-agnostic",
    "description": "Type-safe GraphQL interface for RDF knowledge graphs with automatic schema generation",
    "tier": "optional",
    "main": "src/adapter.mjs",
    "exports": {
      ".": "./src/adapter.mjs",
      "./schema": "./src/schema-generator.mjs",
      "./query": "./src/query-builder.mjs",
      "./resolver": "./src/resolver.mjs"
    },
    "path": "rdf-graphql",
    "dependencies": [
      "graphql",
      "@graphql-tools/schema",
      "@unrdf/oxigraph",
      "zod"
    ],
    "devDependencies": []
  },
  {
    "name": "@unrdf/react",
    "version": "0.0.0-agnostic",
    "description": "UNRDF React - AI Semantic Analysis Tools for RDF Knowledge Graphs (Optional Extension)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./ai-semantic": "./src/ai-semantic/index.mjs",
      "./semantic-analyzer": "./src/ai-semantic/semantic-analyzer.mjs"
    },
    "path": "react",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/oxigraph",
      "lru-cache",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/self-healing-workflows",
    "version": "0.0.0-agnostic",
    "description": "Automatic error recovery system with 85-95% success rate using YAWL + Daemon + Hooks",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./engine": "./src/self-healing-engine.mjs",
      "./retry": "./src/retry-strategy.mjs",
      "./circuit-breaker": "./src/circuit-breaker.mjs",
      "./recovery": "./src/recovery-actions.mjs",
      "./classifier": "./src/error-classifier.mjs",
      "./health": "./src/health-monitor.mjs",
      "./schemas": "./src/schemas.mjs"
    },
    "path": "self-healing-workflows",
    "dependencies": [
      "@opentelemetry/api",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/semantic-parts",
    "version": "0.0.0-agnostic",
    "description": "Evidence-bounded semantic software-parts graph and cross-language substitution discovery",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "semantic-parts",
    "dependencies": [],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/semantic-search",
    "version": "0.0.0-agnostic",
    "description": "AI-powered semantic search over RDF knowledge graphs using vector embeddings",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./embeddings": "./src/embeddings/index.mjs",
      "./search": "./src/search/index.mjs",
      "./discovery": "./src/discovery/index.mjs"
    },
    "path": "semantic-search",
    "dependencies": [
      "@unrdf/oxigraph",
      "@xenova/transformers",
      "sharp",
      "vectra",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/serverless",
    "version": "0.0.0-agnostic",
    "description": "UNRDF Serverless - One-click AWS deployment for RDF applications",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./cdk": "./src/cdk/index.mjs",
      "./deploy": "./src/deploy/index.mjs",
      "./api": "./src/api/index.mjs",
      "./storage": "./src/storage/index.mjs",
      "./storage/dynamodb-core": "./src/storage/dynamodb-core.mjs",
      "./storage/dynamodb-adapter": "./src/storage/dynamodb-adapter.mjs"
    },
    "path": "serverless",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "aws-cdk-lib",
      "constructs",
      "esbuild",
      "zod"
    ],
    "devDependencies": [
      "@aws-sdk/client-dynamodb",
      "@aws-sdk/client-lambda",
      "@aws-sdk/lib-dynamodb",
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/spatial-kg",
    "version": "0.0.0-agnostic",
    "description": "Spatial Knowledge Graphs - WebXR-enabled 3D visualization and navigation of RDF knowledge graphs",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./engine": "./src/spatial-kg-engine.mjs",
      "./layout": "./src/layout-3d.mjs",
      "./renderer": "./src/webxr-renderer.mjs",
      "./query": "./src/spatial-query.mjs",
      "./gestures": "./src/gesture-controller.mjs",
      "./collaboration": "./src/collaboration.mjs",
      "./lod": "./src/lod-manager.mjs",
      "./schemas": "./src/schemas.mjs"
    },
    "path": "spatial-kg",
    "dependencies": [
      "@unrdf/core",
      "@opentelemetry/api",
      "three",
      "d3-force-3d",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "@types/three",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/temporal-discovery",
    "version": "0.0.0-agnostic",
    "description": "Temporal knowledge discovery for RDF graphs - pattern mining, anomaly detection, trend analysis",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./pattern-miner": "./src/pattern-miner.mjs",
      "./anomaly-detector": "./src/anomaly-detector.mjs",
      "./trend-analyzer": "./src/trend-analyzer.mjs",
      "./correlation-finder": "./src/correlation-finder.mjs",
      "./changepoint-detector": "./src/changepoint-detector.mjs",
      "./engine": "./src/temporal-discovery-engine.mjs"
    },
    "path": "temporal-discovery",
    "dependencies": [
      "@unrdf/kgc-4d",
      "@unrdf/semantic-search",
      "@unrdf/graph-analytics",
      "@opentelemetry/api",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/test-utils",
    "version": "0.0.0-agnostic",
    "description": "Shared test utilities and fixtures for unrdf packages",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "test-utils",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/oxigraph"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/validation",
    "version": "0.0.0-agnostic",
    "description": "OTEL validation framework for UNRDF development",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "validation",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/knowledge-engine",
      "zod"
    ],
    "devDependencies": []
  },
  {
    "name": "@unrdf/zkp",
    "version": "0.0.0-agnostic",
    "description": "Zero-Knowledge SPARQL - Privacy-preserving query proofs using zk-SNARKs",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./prover": "./src/sparql-zkp-prover.mjs",
      "./circuit": "./src/circuit-compiler.mjs",
      "./groth16": "./src/groth16-prover.mjs",
      "./verifier": "./src/groth16-verifier.mjs",
      "./schemas": "./src/schemas.mjs"
    },
    "path": "zkp",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@opentelemetry/api",
      "zod",
      "hash-wasm",
      "snarkjs",
      "circomlibjs",
      "sparqljs"
    ],
    "devDependencies": [
      "@types/node",
      "eslint",
      "prettier",
      "vitest",
      "@vitest/coverage-v8"
    ]
  }
],
  total: Object.keys(PACKAGES).length
};

module.exports = {
  PACKAGES,
  REGISTRY,
  getPackage: (name) => PACKAGES[name],
  findByTier: (tier) => REGISTRY[tier] || [],
  getAll: () => Object.values(PACKAGES),
  getTier: (name) => PACKAGES[name]?.tier
};
