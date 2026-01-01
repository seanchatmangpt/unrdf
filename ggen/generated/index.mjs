/**
 * UNRDF Package Registry
 * Auto-generated from package.json files
 * Generated: 2026-01-01T03:09:45.019Z
 */

export const PACKAGES = {
  "@unrdf/atomvm": {
    "name": "@unrdf/atomvm",
    "version": "5.0.1",
    "description": "Run AtomVM (Erlang/BEAM VM) in browser and Node.js using WebAssembly",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./service-worker-manager": "./src/service-worker-manager.mjs"
    },
    "path": "atomvm",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/streaming",
      "coi-serviceworker"
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
    "version": "1.0.0",
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
      "@unrdf/yawl",
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
    "version": "1.0.0",
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
  "@unrdf/cli": {
    "name": "@unrdf/cli",
    "version": "5.0.1",
    "description": "UNRDF CLI - Command-line Tools for Graph Operations and Context Management",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./commands": "./src/commands/index.mjs"
    },
    "path": "cli",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/decision-fabric",
      "@unrdf/federation",
      "@unrdf/hooks",
      "@unrdf/streaming",
      "citty",
      "table",
      "yaml"
    ],
    "devDependencies": [
      "@types/node",
      "citty-test-utils",
      "vitest"
    ]
  },
  "@unrdf/collab": {
    "name": "@unrdf/collab",
    "version": "1.0.0",
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
      "vitest"
    ]
  },
  "@unrdf/composables": {
    "name": "@unrdf/composables",
    "version": "5.0.1",
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
      "@unrdf/streaming",
      "vue"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/consensus": {
    "name": "@unrdf/consensus",
    "version": "1.0.0",
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
    "version": "6.0.0-alpha.1",
    "description": "UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./rdf": "./src/rdf/index.mjs",
      "./rdf/minimal-n3-integration": "./src/rdf/minimal-n3-integration.mjs",
      "./rdf/n3-justified-only": "./src/rdf/n3-justified-only.mjs",
      "./sparql": "./src/sparql/index.mjs",
      "./types": "./src/types.mjs",
      "./constants": "./src/constants.mjs",
      "./validation": "./src/validation/index.mjs",
      "./health": "./src/health.mjs",
      "./logger": "./src/logger.mjs",
      "./metrics": "./src/metrics.mjs",
      "./security": "./src/security.mjs",
      "./security-schemas": "./src/security-schemas.mjs",
      "./utils/sparql-utils": "./src/utils/sparql-utils.mjs"
    },
    "path": "core",
    "dependencies": [
      "@rdfjs/data-model",
      "@rdfjs/namespace",
      "@rdfjs/serializer-jsonld",
      "@rdfjs/serializer-turtle",
      "@rdfjs/to-ntriples",
      "@unrdf/oxigraph",
      "jsonld",
      "n3",
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
  "@unrdf/dark-matter": {
    "name": "@unrdf/dark-matter",
    "version": "5.0.1",
    "description": "UNRDF Dark Matter - Query Optimization and Performance Analysis (Optional Extension)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./optimizer": "./src/optimizer.mjs",
      "./analyzer": "./src/analyzer.mjs"
    },
    "path": "dark-matter",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "typhonjs-escomplex"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/decision-fabric": {
    "name": "@unrdf/decision-fabric",
    "version": "0.1.0",
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
      "@unrdf/streaming",
      "@unrdf/validation"
    ],
    "devDependencies": [
      "@types/node",
      "eslint",
      "jest"
    ]
  },
  "@unrdf/diataxis-kit": {
    "name": "@unrdf/diataxis-kit",
    "version": "1.0.0",
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
  "docs": {
    "name": "docs",
    "version": "5.0.1",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {},
    "path": "docs",
    "dependencies": [
      "@ai-sdk/gateway",
      "@ai-sdk/vue",
      "@electric-sql/pglite",
      "@iconify-json/logos",
      "@iconify-json/lucide",
      "@iconify-json/simple-icons",
      "@iconify-json/vscode-icons",
      "@nuxt/content",
      "@nuxt/image",
      "@nuxt/ui",
      "@nuxtjs/mdc",
      "ai",
      "better-sqlite3",
      "date-fns",
      "drizzle-orm",
      "nuxt",
      "nuxt-auth-utils",
      "nuxt-charts",
      "nuxt-llms",
      "nuxt-og-image",
      "shiki-stream"
    ],
    "devDependencies": [
      "@nuxt/eslint",
      "@playwright/test",
      "@types/node",
      "@vitejs/plugin-vue",
      "@vitest/ui",
      "@vue/test-utils",
      "drizzle-kit",
      "eslint",
      "happy-dom",
      "msw",
      "typescript",
      "vitest",
      "vue-tsc"
    ]
  },
  "@unrdf/domain": {
    "name": "@unrdf/domain",
    "version": "5.0.1",
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
    "version": "5.0.1",
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
  "@unrdf/federation": {
    "name": "@unrdf/federation",
    "version": "6.0.0",
    "description": "UNRDF Federation - Distributed RDF Query with RAFT Consensus and Multi-Master Replication",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./coordinator": "./src/coordinator.mjs",
      "./advanced-sparql": "./src/advanced-sparql-federation.mjs"
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
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/fusion": {
    "name": "@unrdf/fusion",
    "version": "1.0.0",
    "description": "Unified integration layer for 7-day UNRDF innovation - KGC-4D, blockchain, hooks, caching",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "fusion",
    "dependencies": [
      "@unrdf/oxigraph",
      "@unrdf/kgc-4d",
      "@unrdf/blockchain",
      "@unrdf/hooks",
      "@unrdf/caching",
      "@unrdf/yawl",
      "graphql",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/graph-analytics": {
    "name": "@unrdf/graph-analytics",
    "version": "1.0.0",
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
    "version": "5.0.1",
    "description": "UNRDF Knowledge Hooks - Policy Definition and Execution Framework",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./define": "./src/define.mjs",
      "./executor": "./src/executor.mjs"
    },
    "path": "hooks",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "citty",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/integration-tests": {
    "name": "@unrdf/integration-tests",
    "version": "5.1.0",
    "description": "Phase 5: Comprehensive Integration & Adversarial Tests (75 tests)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {},
    "path": "integration-tests",
    "dependencies": [
      "@unrdf/yawl",
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
    "version": "5.0.1",
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
      "hash-wasm",
      "isomorphic-git"
    ],
    "devDependencies": [
      "comment-parser",
      "simple-statistics",
      "tinybench",
      "vitest"
    ]
  },
  "@unrdf/kgc-claude": {
    "name": "@unrdf/kgc-claude",
    "version": "5.0.0",
    "description": "KGC-Claude Substrate - Deterministic run objects, universal checkpoints, bounded autonomy, and multi-agent concurrency for Claude integration",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./run-capsule": "./src/run-capsule.mjs",
      "./checkpoint": "./src/checkpoint.mjs",
      "./autonomy-guard": "./src/autonomy-guard.mjs",
      "./shard-merge": "./src/shard-merge.mjs",
      "./async-workflow": "./src/async-workflow.mjs",
      "./projection": "./src/projection.mjs",
      "./swarm-orchestrator": "./src/swarm-orchestrator.mjs",
      "./poka-yoke-guards": "./src/poka-yoke-guards.mjs",
      "./observable-io": "./src/observable-io.mjs",
      "./info-scheduler": "./src/info-scheduler.mjs",
      "./drift-detector": "./src/drift-detector.mjs",
      "./budget-enforcer": "./src/budget-enforcer.mjs",
      "./agent-harness": "./src/agent-harness.mjs",
      "./receipt-compositor": "./src/receipt-compositor.mjs",
      "./mcp-server-builder": "./src/mcp-server-builder.mjs",
      "./mcp-federation": "./src/mcp-federation.mjs",
      "./mcp-bridge": "./src/mcp-bridge.mjs",
      "./capabilities/ide-integration": "./src/capabilities/ide-integration.mjs",
      "./capabilities/ui-components": "./src/capabilities/ui-components.mjs",
      "./capabilities/editor-commands": "./src/capabilities/editor-commands.mjs"
    },
    "path": "kgc-claude",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/kgc-4d",
      "@unrdf/yawl",
      "@unrdf/hooks",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/kgc-cli": {
    "name": "@unrdf/kgc-cli",
    "version": "5.0.1",
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
    "version": "1.0.0",
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
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/kgc-multiverse": {
    "name": "@unrdf/kgc-multiverse",
    "version": "1.0.0",
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
    "version": "1.0.0",
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
      "./utils/errors": "./src/utils/errors.mjs"
    },
    "path": "kgc-probe",
    "dependencies": [
      "@unrdf/kgc-substrate",
      "@unrdf/kgc-4d",
      "@unrdf/v6-core",
      "@unrdf/oxigraph",
      "@unrdf/hooks",
      "@unrdf/yawl",
      "hash-wasm",
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
    "version": "1.0.0",
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
    "version": "1.0.0",
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
    "version": "1.0.0",
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
      "./guardian": "./src/guardian.mjs"
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
    "version": "1.0.0",
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
    "version": "5.0.1",
    "description": "Deterministic Nunjucks template system with custom filters and frontmatter support",
    "tier": "optional",
    "main": "src/index.js",
    "exports": {
      ".": {
        "import": "./src/index.js"
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
      "@unrdf/test-utils",
      "fs-extra",
      "gray-matter",
      "nunjucks",
      "yaml"
    ],
    "devDependencies": [
      "@amiceli/vitest-cucumber",
      "@babel/parser",
      "@babel/traverse",
      "comment-parser",
      "eslint",
      "nodemon",
      "vitest"
    ]
  },
  "@unrdf/knowledge-engine": {
    "name": "@unrdf/knowledge-engine",
    "version": "5.0.1",
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
      "@noble/hashes",
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/streaming",
      "@xenova/transformers",
      "eyereasoner"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/ml-inference": {
    "name": "@unrdf/ml-inference",
    "version": "5.0.1",
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
    "version": "1.0.0",
    "description": "ML Model Versioning System using TensorFlow.js and UNRDF KGC-4D time-travel capabilities",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./version-store": "./src/version-store.mjs",
      "./examples/image-classifier": "./src/examples/image-classifier.mjs"
    },
    "path": "ml-versioning",
    "dependencies": [
      "@tensorflow/tfjs-node",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "@unrdf/core",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/nextra-docs": {
    "name": "@unrdf/nextra-docs",
    "version": "5.0.1",
    "description": "UNRDF documentation with Nextra 4 - Developer-focused Next.js documentation",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {},
    "path": "nextra",
    "dependencies": [
      "katex",
      "next",
      "nextra",
      "nextra-theme-docs",
      "react",
      "react-dom",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "@types/react",
      "@types/react-dom",
      "typescript"
    ]
  },
  "@unrdf/observability": {
    "name": "@unrdf/observability",
    "version": "1.0.0",
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
      "prom-client",
      "@opentelemetry/api",
      "@opentelemetry/exporter-prometheus",
      "@opentelemetry/sdk-metrics",
      "express",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/oxigraph": {
    "name": "@unrdf/oxigraph",
    "version": "5.0.1",
    "description": "UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./store": "./src/store.mjs",
      "./types": "./src/types.mjs"
    },
    "path": "oxigraph",
    "dependencies": [
      "oxigraph",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/project-engine": {
    "name": "@unrdf/project-engine",
    "version": "5.0.1",
    "description": "UNRDF Project Engine - Self-hosting Tools and Infrastructure (Development Only)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "project-engine",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/knowledge-engine"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/rdf-graphql": {
    "name": "@unrdf/rdf-graphql",
    "version": "1.0.0",
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
    "version": "5.0.0",
    "description": "UNRDF React - AI Semantic Analysis Tools for RDF Knowledge Graphs (Optional Extension)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./ai-semantic": "./src/ai-semantic/index.mjs",
      "./semantic-analyzer": "./src/ai-semantic/semantic-analyzer.mjs",
      "./embeddings-manager": "./src/ai-semantic/embeddings-manager.mjs",
      "./nlp-query-builder": "./src/ai-semantic/nlp-query-builder.mjs",
      "./anomaly-detector": "./src/ai-semantic/anomaly-detector.mjs"
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
    "version": "1.0.0",
    "description": "KGC Receipts - Batch receipt generation with Merkle tree verification for knowledge graph operations",
    "tier": "extended",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./batch-receipt-generator": "./src/batch-receipt-generator.mjs",
      "./merkle-batcher": "./src/merkle-batcher.mjs"
    },
    "path": "receipts",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/kgc-4d",
      "@unrdf/kgc-multiverse",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@vitest/coverage-v8",
      "vitest",
      "eslint",
      "unbuild"
    ]
  },
  "@unrdf/semantic-search": {
    "name": "@unrdf/semantic-search",
    "version": "1.0.0",
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
    "version": "1.0.0",
    "description": "UNRDF Serverless - One-click AWS deployment for RDF applications",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./cdk": "./src/cdk/index.mjs",
      "./deploy": "./src/deploy/index.mjs",
      "./api": "./src/api/index.mjs",
      "./storage": "./src/storage/index.mjs"
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
  "@unrdf/streaming": {
    "name": "@unrdf/streaming",
    "version": "5.0.1",
    "description": "UNRDF Streaming - Change Feeds and Real-time Synchronization",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./processor": "./src/processor.mjs"
    },
    "path": "streaming",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/oxigraph",
      "citty",
      "lru-cache",
      "ws",
      "zod"
    ],
    "devDependencies": [
      "@rdfjs/data-model",
      "@types/node",
      "vitest"
    ]
  },
  "@unrdf/test-utils": {
    "name": "@unrdf/test-utils",
    "version": "5.0.1",
    "description": "Testing utilities for UNRDF development",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "test-utils",
    "dependencies": [
      "@unrdf/oxigraph",
      "@opentelemetry/api",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/v6-compat": {
    "name": "@unrdf/v6-compat",
    "version": "6.0.0-rc.1",
    "description": "UNRDF v6 Compatibility Layer - v5 to v6 migration bridge with adapters and lint rules",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./adapters": "./src/adapters.mjs",
      "./lint-rules": "./src/lint-rules.mjs",
      "./schema-generator": "./src/schema-generator.mjs"
    },
    "path": "v6-compat",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "@unrdf/v6-core",
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
    "version": "6.0.0-rc.1",
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
      "@unrdf/yawl",
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
    "version": "5.0.1",
    "description": "OTEL validation framework for UNRDF development",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "validation",
    "dependencies": [
      "@unrdf/knowledge-engine"
    ],
    "devDependencies": []
  },
  "@unrdf/yawl": {
    "name": "@unrdf/yawl",
    "version": "6.0.0",
    "description": "YAWL (Yet Another Workflow Language) engine with KGC-4D time-travel and receipt verification",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./api": "./src/api/workflow-api.mjs",
      "./graphql-api": "./src/api/graphql-api.mjs",
      "./ontology": "./src/ontology/yawl-ontology.mjs",
      "./store": "./src/store/yawl-store.mjs",
      "./types": "./src/types/yawl-types.mjs",
      "./schemas": "./src/types/yawl-schemas.mjs",
      "./hooks": "./src/hooks/yawl-hooks.mjs",
      "./resources": "./src/resources/yawl-resources.mjs",
      "./cancellation": "./src/cancellation/index.mjs",
      "./receipt": "./src/receipt.mjs",
      "./blockchain-receipts": "./src/blockchain-receipts.mjs",
      "./visualization": "./src/visualization/live-workflow-viz.mjs"
    },
    "path": "yawl",
    "dependencies": [
      "@graphql-tools/schema",
      "@noble/ed25519",
      "@observablehq/plot",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "d3",
      "graphql",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "eslint",
      "vitest"
    ]
  },
  "@unrdf/yawl-ai": {
    "name": "@unrdf/yawl-ai",
    "version": "1.0.0",
    "description": "AI-powered workflow optimization using TensorFlow.js and YAWL patterns",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./predictor": "./src/ml/workflow-predictor.mjs",
      "./optimizer": "./src/ml/performance-optimizer.mjs",
      "./anomaly": "./src/ml/anomaly-detector.mjs",
      "./adapter": "./src/integration/yawl-adapter.mjs"
    },
    "path": "yawl-ai",
    "dependencies": [
      "@tensorflow/tfjs-node",
      "@tensorflow/tfjs-layers",
      "ml-matrix",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  "@unrdf/yawl-api": {
    "name": "@unrdf/yawl-api",
    "version": "1.0.0",
    "description": "High-performance REST API framework that exposes YAWL workflows as RESTful APIs with OpenAPI documentation",
    "tier": "optional",
    "main": "src/server.mjs",
    "exports": {
      ".": "./src/server.mjs",
      "./server": "./src/server.mjs"
    },
    "path": "yawl-api",
    "dependencies": [
      "@unrdf/yawl",
      "@unrdf/kgc-4d",
      "fastify",
      "@fastify/swagger",
      "@fastify/swagger-ui",
      "@fastify/cors",
      "zod",
      "zod-to-json-schema"
    ],
    "devDependencies": [
      "eslint",
      "vitest"
    ]
  },
  "@unrdf/yawl-durable": {
    "name": "@unrdf/yawl-durable",
    "version": "0.1.0",
    "description": "Durable execution framework inspired by Temporal.io using YAWL and KGC-4D",
    "tier": "optional",
    "main": "src/engine.mjs",
    "exports": {
      ".": "./src/engine.mjs",
      "./saga": "./src/saga.mjs",
      "./activity": "./src/activity.mjs",
      "./replay": "./src/replay.mjs"
    },
    "path": "yawl-durable",
    "dependencies": [
      "@unrdf/yawl",
      "@unrdf/kgc-4d",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@jest/globals",
      "eslint",
      "jest"
    ]
  },
  "@unrdf/yawl-kafka": {
    "name": "@unrdf/yawl-kafka",
    "version": "1.0.0",
    "description": "Apache Kafka event streaming integration for YAWL workflows with Avro serialization",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./producer": "./src/producer.mjs",
      "./consumer": "./src/consumer.mjs",
      "./schemas": "./src/schemas.mjs"
    },
    "path": "yawl-kafka",
    "dependencies": [
      "@unrdf/core",
      "avsc",
      "kafkajs",
      "zod"
    ],
    "devDependencies": [
      "eslint",
      "vitest"
    ]
  },
  "@unrdf/yawl-langchain": {
    "name": "@unrdf/yawl-langchain",
    "version": "1.0.0",
    "description": "LangChain integration for YAWL workflow engine - AI-powered workflow orchestration with RDF context",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./adapter": "./src/adapter.mjs",
      "./examples": "./examples/code-review-workflow.mjs"
    },
    "path": "yawl-langchain",
    "dependencies": [
      "@langchain/core",
      "@langchain/openai",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "@unrdf/yawl",
      "langchain",
      "zod"
    ],
    "devDependencies": [
      "eslint",
      "vitest"
    ]
  },
  "@unrdf/yawl-observability": {
    "name": "@unrdf/yawl-observability",
    "version": "1.0.0",
    "description": "Workflow observability framework with Prometheus metrics and OpenTelemetry tracing for YAWL",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./metrics": "./src/metrics.mjs",
      "./tracing": "./src/tracing.mjs",
      "./sli": "./src/sli.mjs"
    },
    "path": "yawl-observability",
    "dependencies": [
      "@opentelemetry/api",
      "@opentelemetry/sdk-node",
      "@opentelemetry/sdk-metrics",
      "@unrdf/yawl",
      "prom-client",
      "zod"
    ],
    "devDependencies": [
      "eslint",
      "vitest"
    ]
  },
  "@unrdf/yawl-queue": {
    "name": "@unrdf/yawl-queue",
    "version": "1.0.0",
    "description": "Distributed YAWL workflow execution using BullMQ and Redis",
    "tier": "optional",
    "main": "src/adapter.mjs",
    "exports": {
      ".": "./src/adapter.mjs",
      "./adapter": "./src/adapter.mjs",
      "./examples/data-pipeline": "./src/examples/data-pipeline.mjs"
    },
    "path": "yawl-queue",
    "dependencies": [
      "@unrdf/yawl",
      "@unrdf/kgc-4d",
      "bullmq",
      "ioredis",
      "zod"
    ],
    "devDependencies": [
      "eslint",
      "vitest"
    ]
  },
  "@unrdf/yawl-realtime": {
    "name": "@unrdf/yawl-realtime",
    "version": "1.0.0",
    "description": "Real-time collaboration framework for YAWL workflows using Socket.io",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./server": "./src/server.mjs",
      "./client": "./src/client.mjs"
    },
    "path": "yawl-realtime",
    "dependencies": [
      "@unrdf/yawl",
      "socket.io",
      "socket.io-client",
      "zod"
    ],
    "devDependencies": [
      "vitest",
      "eslint"
    ]
  },
  "@unrdf/yawl-viz": {
    "name": "@unrdf/yawl-viz",
    "version": "1.0.0",
    "description": "Real-time D3.js visualization for YAWL workflows with Van der Aalst pattern rendering",
    "tier": "optional",
    "main": "src/visualizer.mjs",
    "exports": {
      ".": "./src/visualizer.mjs"
    },
    "path": "yawl-viz",
    "dependencies": [
      "@unrdf/yawl",
      "d3",
      "d3-graphviz",
      "d3-selection",
      "d3-zoom",
      "d3-drag",
      "d3-force",
      "d3-hierarchy"
    ],
    "devDependencies": [
      "eslint",
      "vitest",
      "vite",
      "jsdom"
    ]
  }
};

export const REGISTRY = {
  packages: Object.values(PACKAGES),
  essential: [
  {
    "name": "@unrdf/core",
    "version": "6.0.0-alpha.1",
    "description": "UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./rdf": "./src/rdf/index.mjs",
      "./rdf/minimal-n3-integration": "./src/rdf/minimal-n3-integration.mjs",
      "./rdf/n3-justified-only": "./src/rdf/n3-justified-only.mjs",
      "./sparql": "./src/sparql/index.mjs",
      "./types": "./src/types.mjs",
      "./constants": "./src/constants.mjs",
      "./validation": "./src/validation/index.mjs",
      "./health": "./src/health.mjs",
      "./logger": "./src/logger.mjs",
      "./metrics": "./src/metrics.mjs",
      "./security": "./src/security.mjs",
      "./security-schemas": "./src/security-schemas.mjs",
      "./utils/sparql-utils": "./src/utils/sparql-utils.mjs"
    },
    "path": "core",
    "dependencies": [
      "@rdfjs/data-model",
      "@rdfjs/namespace",
      "@rdfjs/serializer-jsonld",
      "@rdfjs/serializer-turtle",
      "@rdfjs/to-ntriples",
      "@unrdf/oxigraph",
      "jsonld",
      "n3",
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
    "name": "@unrdf/hooks",
    "version": "5.0.1",
    "description": "UNRDF Knowledge Hooks - Policy Definition and Execution Framework",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./define": "./src/define.mjs",
      "./executor": "./src/executor.mjs"
    },
    "path": "hooks",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "citty",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/kgc-4d",
    "version": "5.0.1",
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
      "hash-wasm",
      "isomorphic-git"
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
    "version": "5.0.1",
    "description": "UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./store": "./src/store.mjs",
      "./types": "./src/types.mjs"
    },
    "path": "oxigraph",
    "dependencies": [
      "oxigraph",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/streaming",
    "version": "5.0.1",
    "description": "UNRDF Streaming - Change Feeds and Real-time Synchronization",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./processor": "./src/processor.mjs"
    },
    "path": "streaming",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/hooks",
      "@unrdf/oxigraph",
      "citty",
      "lru-cache",
      "ws",
      "zod"
    ],
    "devDependencies": [
      "@rdfjs/data-model",
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/v6-core",
    "version": "6.0.0-rc.1",
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
      "@unrdf/yawl",
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
  {
    "name": "@unrdf/yawl",
    "version": "6.0.0",
    "description": "YAWL (Yet Another Workflow Language) engine with KGC-4D time-travel and receipt verification",
    "tier": "essential",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./api": "./src/api/workflow-api.mjs",
      "./graphql-api": "./src/api/graphql-api.mjs",
      "./ontology": "./src/ontology/yawl-ontology.mjs",
      "./store": "./src/store/yawl-store.mjs",
      "./types": "./src/types/yawl-types.mjs",
      "./schemas": "./src/types/yawl-schemas.mjs",
      "./hooks": "./src/hooks/yawl-hooks.mjs",
      "./resources": "./src/resources/yawl-resources.mjs",
      "./cancellation": "./src/cancellation/index.mjs",
      "./receipt": "./src/receipt.mjs",
      "./blockchain-receipts": "./src/blockchain-receipts.mjs",
      "./visualization": "./src/visualization/live-workflow-viz.mjs"
    },
    "path": "yawl",
    "dependencies": [
      "@graphql-tools/schema",
      "@noble/ed25519",
      "@observablehq/plot",
      "@unrdf/hooks",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "d3",
      "graphql",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "eslint",
      "vitest"
    ]
  }
],
  extended: [
  {
    "name": "@unrdf/cli",
    "version": "5.0.1",
    "description": "UNRDF CLI - Command-line Tools for Graph Operations and Context Management",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./commands": "./src/commands/index.mjs"
    },
    "path": "cli",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/decision-fabric",
      "@unrdf/federation",
      "@unrdf/hooks",
      "@unrdf/streaming",
      "citty",
      "table",
      "yaml"
    ],
    "devDependencies": [
      "@types/node",
      "citty-test-utils",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/consensus",
    "version": "1.0.0",
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
    "version": "6.0.0",
    "description": "UNRDF Federation - Distributed RDF Query with RAFT Consensus and Multi-Master Replication",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./coordinator": "./src/coordinator.mjs",
      "./advanced-sparql": "./src/advanced-sparql-federation.mjs"
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
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/kgc-runtime",
    "version": "1.0.0",
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
    "version": "1.0.0",
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
    "version": "5.0.1",
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
      "@noble/hashes",
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/streaming",
      "@xenova/transformers",
      "eyereasoner"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/receipts",
    "version": "1.0.0",
    "description": "KGC Receipts - Batch receipt generation with Merkle tree verification for knowledge graph operations",
    "tier": "extended",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./batch-receipt-generator": "./src/batch-receipt-generator.mjs",
      "./merkle-batcher": "./src/merkle-batcher.mjs"
    },
    "path": "receipts",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/kgc-4d",
      "@unrdf/kgc-multiverse",
      "hash-wasm",
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
    "name": "@unrdf/v6-compat",
    "version": "6.0.0-rc.1",
    "description": "UNRDF v6 Compatibility Layer - v5 to v6 migration bridge with adapters and lint rules",
    "tier": "extended",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./adapters": "./src/adapters.mjs",
      "./lint-rules": "./src/lint-rules.mjs",
      "./schema-generator": "./src/schema-generator.mjs"
    },
    "path": "v6-compat",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "@unrdf/v6-core",
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
    "name": "@unrdf/atomvm",
    "version": "5.0.1",
    "description": "Run AtomVM (Erlang/BEAM VM) in browser and Node.js using WebAssembly",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./service-worker-manager": "./src/service-worker-manager.mjs"
    },
    "path": "atomvm",
    "dependencies": [
      "@opentelemetry/api",
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/streaming",
      "coi-serviceworker"
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
    "version": "1.0.0",
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
      "@unrdf/yawl",
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
    "version": "1.0.0",
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
    "name": "@unrdf/collab",
    "version": "1.0.0",
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
      "vitest"
    ]
  },
  {
    "name": "@unrdf/composables",
    "version": "5.0.1",
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
      "@unrdf/streaming",
      "vue"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/dark-matter",
    "version": "5.0.1",
    "description": "UNRDF Dark Matter - Query Optimization and Performance Analysis (Optional Extension)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./optimizer": "./src/optimizer.mjs",
      "./analyzer": "./src/analyzer.mjs"
    },
    "path": "dark-matter",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "typhonjs-escomplex"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/decision-fabric",
    "version": "0.1.0",
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
      "@unrdf/streaming",
      "@unrdf/validation"
    ],
    "devDependencies": [
      "@types/node",
      "eslint",
      "jest"
    ]
  },
  {
    "name": "@unrdf/diataxis-kit",
    "version": "1.0.0",
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
    "name": "docs",
    "version": "5.0.1",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {},
    "path": "docs",
    "dependencies": [
      "@ai-sdk/gateway",
      "@ai-sdk/vue",
      "@electric-sql/pglite",
      "@iconify-json/logos",
      "@iconify-json/lucide",
      "@iconify-json/simple-icons",
      "@iconify-json/vscode-icons",
      "@nuxt/content",
      "@nuxt/image",
      "@nuxt/ui",
      "@nuxtjs/mdc",
      "ai",
      "better-sqlite3",
      "date-fns",
      "drizzle-orm",
      "nuxt",
      "nuxt-auth-utils",
      "nuxt-charts",
      "nuxt-llms",
      "nuxt-og-image",
      "shiki-stream"
    ],
    "devDependencies": [
      "@nuxt/eslint",
      "@playwright/test",
      "@types/node",
      "@vitejs/plugin-vue",
      "@vitest/ui",
      "@vue/test-utils",
      "drizzle-kit",
      "eslint",
      "happy-dom",
      "msw",
      "typescript",
      "vitest",
      "vue-tsc"
    ]
  },
  {
    "name": "@unrdf/domain",
    "version": "5.0.1",
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
    "version": "5.0.1",
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
    "name": "@unrdf/fusion",
    "version": "1.0.0",
    "description": "Unified integration layer for 7-day UNRDF innovation - KGC-4D, blockchain, hooks, caching",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "fusion",
    "dependencies": [
      "@unrdf/oxigraph",
      "@unrdf/kgc-4d",
      "@unrdf/blockchain",
      "@unrdf/hooks",
      "@unrdf/caching",
      "@unrdf/yawl",
      "graphql",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/graph-analytics",
    "version": "1.0.0",
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
    "version": "5.1.0",
    "description": "Phase 5: Comprehensive Integration & Adversarial Tests (75 tests)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {},
    "path": "integration-tests",
    "dependencies": [
      "@unrdf/yawl",
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
    "name": "@unrdf/kgc-claude",
    "version": "5.0.0",
    "description": "KGC-Claude Substrate - Deterministic run objects, universal checkpoints, bounded autonomy, and multi-agent concurrency for Claude integration",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./run-capsule": "./src/run-capsule.mjs",
      "./checkpoint": "./src/checkpoint.mjs",
      "./autonomy-guard": "./src/autonomy-guard.mjs",
      "./shard-merge": "./src/shard-merge.mjs",
      "./async-workflow": "./src/async-workflow.mjs",
      "./projection": "./src/projection.mjs",
      "./swarm-orchestrator": "./src/swarm-orchestrator.mjs",
      "./poka-yoke-guards": "./src/poka-yoke-guards.mjs",
      "./observable-io": "./src/observable-io.mjs",
      "./info-scheduler": "./src/info-scheduler.mjs",
      "./drift-detector": "./src/drift-detector.mjs",
      "./budget-enforcer": "./src/budget-enforcer.mjs",
      "./agent-harness": "./src/agent-harness.mjs",
      "./receipt-compositor": "./src/receipt-compositor.mjs",
      "./mcp-server-builder": "./src/mcp-server-builder.mjs",
      "./mcp-federation": "./src/mcp-federation.mjs",
      "./mcp-bridge": "./src/mcp-bridge.mjs",
      "./capabilities/ide-integration": "./src/capabilities/ide-integration.mjs",
      "./capabilities/ui-components": "./src/capabilities/ui-components.mjs",
      "./capabilities/editor-commands": "./src/capabilities/editor-commands.mjs"
    },
    "path": "kgc-claude",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/oxigraph",
      "@unrdf/kgc-4d",
      "@unrdf/yawl",
      "@unrdf/hooks",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/kgc-cli",
    "version": "5.0.1",
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
    "version": "1.0.0",
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
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/kgc-multiverse",
    "version": "1.0.0",
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
    "version": "1.0.0",
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
      "./utils/errors": "./src/utils/errors.mjs"
    },
    "path": "kgc-probe",
    "dependencies": [
      "@unrdf/kgc-substrate",
      "@unrdf/kgc-4d",
      "@unrdf/v6-core",
      "@unrdf/oxigraph",
      "@unrdf/hooks",
      "@unrdf/yawl",
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
    "name": "@unrdf/kgc-swarm",
    "version": "1.0.0",
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
      "./guardian": "./src/guardian.mjs"
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
    "version": "1.0.0",
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
    "version": "5.0.1",
    "description": "Deterministic Nunjucks template system with custom filters and frontmatter support",
    "tier": "optional",
    "main": "src/index.js",
    "exports": {
      ".": {
        "import": "./src/index.js"
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
      "@unrdf/test-utils",
      "fs-extra",
      "gray-matter",
      "nunjucks",
      "yaml"
    ],
    "devDependencies": [
      "@amiceli/vitest-cucumber",
      "@babel/parser",
      "@babel/traverse",
      "comment-parser",
      "eslint",
      "nodemon",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/ml-inference",
    "version": "5.0.1",
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
    "version": "1.0.0",
    "description": "ML Model Versioning System using TensorFlow.js and UNRDF KGC-4D time-travel capabilities",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./version-store": "./src/version-store.mjs",
      "./examples/image-classifier": "./src/examples/image-classifier.mjs"
    },
    "path": "ml-versioning",
    "dependencies": [
      "@tensorflow/tfjs-node",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "@unrdf/core",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/nextra-docs",
    "version": "5.0.1",
    "description": "UNRDF documentation with Nextra 4 - Developer-focused Next.js documentation",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {},
    "path": "nextra",
    "dependencies": [
      "katex",
      "next",
      "nextra",
      "nextra-theme-docs",
      "react",
      "react-dom",
      "zod"
    ],
    "devDependencies": [
      "@types/node",
      "@types/react",
      "@types/react-dom",
      "typescript"
    ]
  },
  {
    "name": "@unrdf/observability",
    "version": "1.0.0",
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
      "prom-client",
      "@opentelemetry/api",
      "@opentelemetry/exporter-prometheus",
      "@opentelemetry/sdk-metrics",
      "express",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/project-engine",
    "version": "5.0.1",
    "description": "UNRDF Project Engine - Self-hosting Tools and Infrastructure (Development Only)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "project-engine",
    "dependencies": [
      "@unrdf/core",
      "@unrdf/knowledge-engine"
    ],
    "devDependencies": [
      "@types/node",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/rdf-graphql",
    "version": "1.0.0",
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
    "version": "5.0.0",
    "description": "UNRDF React - AI Semantic Analysis Tools for RDF Knowledge Graphs (Optional Extension)",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./ai-semantic": "./src/ai-semantic/index.mjs",
      "./semantic-analyzer": "./src/ai-semantic/semantic-analyzer.mjs",
      "./embeddings-manager": "./src/ai-semantic/embeddings-manager.mjs",
      "./nlp-query-builder": "./src/ai-semantic/nlp-query-builder.mjs",
      "./anomaly-detector": "./src/ai-semantic/anomaly-detector.mjs"
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
    "name": "@unrdf/semantic-search",
    "version": "1.0.0",
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
    "version": "1.0.0",
    "description": "UNRDF Serverless - One-click AWS deployment for RDF applications",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./cdk": "./src/cdk/index.mjs",
      "./deploy": "./src/deploy/index.mjs",
      "./api": "./src/api/index.mjs",
      "./storage": "./src/storage/index.mjs"
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
    "name": "@unrdf/test-utils",
    "version": "5.0.1",
    "description": "Testing utilities for UNRDF development",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "test-utils",
    "dependencies": [
      "@unrdf/oxigraph",
      "@opentelemetry/api",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/validation",
    "version": "5.0.1",
    "description": "OTEL validation framework for UNRDF development",
    "tier": "optional",
    "main": "./src/index.mjs",
    "exports": {
      ".": "./src/index.mjs"
    },
    "path": "validation",
    "dependencies": [
      "@unrdf/knowledge-engine"
    ],
    "devDependencies": []
  },
  {
    "name": "@unrdf/yawl-ai",
    "version": "1.0.0",
    "description": "AI-powered workflow optimization using TensorFlow.js and YAWL patterns",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./predictor": "./src/ml/workflow-predictor.mjs",
      "./optimizer": "./src/ml/performance-optimizer.mjs",
      "./anomaly": "./src/ml/anomaly-detector.mjs",
      "./adapter": "./src/integration/yawl-adapter.mjs"
    },
    "path": "yawl-ai",
    "dependencies": [
      "@tensorflow/tfjs-node",
      "@tensorflow/tfjs-layers",
      "ml-matrix",
      "zod"
    ],
    "devDependencies": [
      "vitest"
    ]
  },
  {
    "name": "@unrdf/yawl-api",
    "version": "1.0.0",
    "description": "High-performance REST API framework that exposes YAWL workflows as RESTful APIs with OpenAPI documentation",
    "tier": "optional",
    "main": "src/server.mjs",
    "exports": {
      ".": "./src/server.mjs",
      "./server": "./src/server.mjs"
    },
    "path": "yawl-api",
    "dependencies": [
      "@unrdf/yawl",
      "@unrdf/kgc-4d",
      "fastify",
      "@fastify/swagger",
      "@fastify/swagger-ui",
      "@fastify/cors",
      "zod",
      "zod-to-json-schema"
    ],
    "devDependencies": [
      "eslint",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/yawl-durable",
    "version": "0.1.0",
    "description": "Durable execution framework inspired by Temporal.io using YAWL and KGC-4D",
    "tier": "optional",
    "main": "src/engine.mjs",
    "exports": {
      ".": "./src/engine.mjs",
      "./saga": "./src/saga.mjs",
      "./activity": "./src/activity.mjs",
      "./replay": "./src/replay.mjs"
    },
    "path": "yawl-durable",
    "dependencies": [
      "@unrdf/yawl",
      "@unrdf/kgc-4d",
      "hash-wasm",
      "zod"
    ],
    "devDependencies": [
      "@jest/globals",
      "eslint",
      "jest"
    ]
  },
  {
    "name": "@unrdf/yawl-kafka",
    "version": "1.0.0",
    "description": "Apache Kafka event streaming integration for YAWL workflows with Avro serialization",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./producer": "./src/producer.mjs",
      "./consumer": "./src/consumer.mjs",
      "./schemas": "./src/schemas.mjs"
    },
    "path": "yawl-kafka",
    "dependencies": [
      "@unrdf/core",
      "avsc",
      "kafkajs",
      "zod"
    ],
    "devDependencies": [
      "eslint",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/yawl-langchain",
    "version": "1.0.0",
    "description": "LangChain integration for YAWL workflow engine - AI-powered workflow orchestration with RDF context",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./adapter": "./src/adapter.mjs",
      "./examples": "./examples/code-review-workflow.mjs"
    },
    "path": "yawl-langchain",
    "dependencies": [
      "@langchain/core",
      "@langchain/openai",
      "@unrdf/kgc-4d",
      "@unrdf/oxigraph",
      "@unrdf/yawl",
      "langchain",
      "zod"
    ],
    "devDependencies": [
      "eslint",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/yawl-observability",
    "version": "1.0.0",
    "description": "Workflow observability framework with Prometheus metrics and OpenTelemetry tracing for YAWL",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./metrics": "./src/metrics.mjs",
      "./tracing": "./src/tracing.mjs",
      "./sli": "./src/sli.mjs"
    },
    "path": "yawl-observability",
    "dependencies": [
      "@opentelemetry/api",
      "@opentelemetry/sdk-node",
      "@opentelemetry/sdk-metrics",
      "@unrdf/yawl",
      "prom-client",
      "zod"
    ],
    "devDependencies": [
      "eslint",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/yawl-queue",
    "version": "1.0.0",
    "description": "Distributed YAWL workflow execution using BullMQ and Redis",
    "tier": "optional",
    "main": "src/adapter.mjs",
    "exports": {
      ".": "./src/adapter.mjs",
      "./adapter": "./src/adapter.mjs",
      "./examples/data-pipeline": "./src/examples/data-pipeline.mjs"
    },
    "path": "yawl-queue",
    "dependencies": [
      "@unrdf/yawl",
      "@unrdf/kgc-4d",
      "bullmq",
      "ioredis",
      "zod"
    ],
    "devDependencies": [
      "eslint",
      "vitest"
    ]
  },
  {
    "name": "@unrdf/yawl-realtime",
    "version": "1.0.0",
    "description": "Real-time collaboration framework for YAWL workflows using Socket.io",
    "tier": "optional",
    "main": "src/index.mjs",
    "exports": {
      ".": "./src/index.mjs",
      "./server": "./src/server.mjs",
      "./client": "./src/client.mjs"
    },
    "path": "yawl-realtime",
    "dependencies": [
      "@unrdf/yawl",
      "socket.io",
      "socket.io-client",
      "zod"
    ],
    "devDependencies": [
      "vitest",
      "eslint"
    ]
  },
  {
    "name": "@unrdf/yawl-viz",
    "version": "1.0.0",
    "description": "Real-time D3.js visualization for YAWL workflows with Van der Aalst pattern rendering",
    "tier": "optional",
    "main": "src/visualizer.mjs",
    "exports": {
      ".": "./src/visualizer.mjs"
    },
    "path": "yawl-viz",
    "dependencies": [
      "@unrdf/yawl",
      "d3",
      "d3-graphviz",
      "d3-selection",
      "d3-zoom",
      "d3-drag",
      "d3-force",
      "d3-hierarchy"
    ],
    "devDependencies": [
      "eslint",
      "vitest",
      "vite",
      "jsdom"
    ]
  }
],
  total: Object.keys(PACKAGES).length
};

export function getPackage(name) {
  return PACKAGES[name];
}

export function findByTier(tier) {
  return REGISTRY[tier] || [];
}

export function getAll() {
  return Object.values(PACKAGES);
}

export function getTier(name) {
  return PACKAGES[name]?.tier;
}

export function getEssential() {
  return REGISTRY.essential;
}

export function getExtended() {
  return REGISTRY.extended;
}

export function getOptional() {
  return REGISTRY.optional;
}

export function stats() {
  return {
    total: REGISTRY.total,
    essential: REGISTRY.essential.length,
    extended: REGISTRY.extended.length,
    optional: REGISTRY.optional.length,
    versions: Object.values(PACKAGES).map(p => p.version).join(', ')
  };
}
