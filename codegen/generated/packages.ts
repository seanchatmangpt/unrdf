/**
 * Auto-generated from UNRDF ontology
 * Source: schemas/unrdf-packages.ttl
 * Generated: 2026-09-28T23:16:00.768Z
 */

export interface Package {
  name: string;
  version: string;
  description: string;
  tier: 'essential' | 'extended' | 'optional';
  mainExport: string;
  testCoverage: number;
  label: string;
}

export interface PackageRegistry {
  packages: Package[];
  essential: Package[];
  extended: Package[];
  optional: Package[];
  total: number;
}

export const PACKAGES: Record<string, Package> = {
  "@unrdf/atomvm": {
    name: "@unrdf/atomvm",
    version: "latest",
    description: "Run AtomVM (Erlang/BEAM VM) in browser and Node.js using WebAssembly",
    tier: "optional",
    mainExport: "createAtomvm",
    testCoverage: 85,
    label: "@unrdf/atomvm"
  },
  "@unrdf/atomvm-playground": {
    name: "@unrdf/atomvm-playground",
    version: "latest",
    description: "Production validation playground for AtomVM - validates processes, supervision, KGC-4D integration",
    tier: "optional",
    mainExport: "createAtomvmPlayground",
    testCoverage: 87,
    label: "@unrdf/atomvm-playground"
  },
  "@unrdf/blockchain": {
    name: "@unrdf/blockchain",
    version: "latest",
    description: "Blockchain integration for UNRDF - Cryptographic receipt anchoring and audit trails",
    tier: "optional",
    mainExport: "createBlockchain",
    testCoverage: 93,
    label: "@unrdf/blockchain"
  },
  "@unrdf/caching": {
    name: "@unrdf/caching",
    version: "latest",
    description: "Multi-layer caching system for RDF queries with Redis and LRU",
    tier: "optional",
    mainExport: "createCaching",
    testCoverage: 84,
    label: "@unrdf/caching"
  },
  "@unrdf/cli": {
    name: "@unrdf/cli",
    version: "latest",
    description: "UNRDF CLI - Command-line Tools for Graph Operations and Context Management",
    tier: "extended",
    mainExport: "createCli",
    testCoverage: 75,
    label: "@unrdf/cli"
  },
  "@unrdf/collab": {
    name: "@unrdf/collab",
    version: "latest",
    description: "Real-time collaborative RDF editing using CRDTs (Yjs) with offline-first architecture",
    tier: "optional",
    mainExport: "createCollab",
    testCoverage: 81,
    label: "@unrdf/collab"
  },
  "@unrdf/composables": {
    name: "@unrdf/composables",
    version: "latest",
    description: "UNRDF Composables - Vue 3 Composables for Reactive RDF State (Optional Extension)",
    tier: "optional",
    mainExport: "createComposables",
    testCoverage: 84,
    label: "@unrdf/composables"
  },
  "@unrdf/consensus": {
    name: "@unrdf/consensus",
    version: "latest",
    description: "Production-grade Raft consensus for distributed workflow coordination",
    tier: "extended",
    mainExport: "createConsensus",
    testCoverage: 85,
    label: "@unrdf/consensus"
  },
  "@unrdf/core": {
    name: "@unrdf/core",
    version: "latest-alpha.1",
    description: "UNRDF Core - RDF Graph Operations, SPARQL Execution, and Foundational Substrate",
    tier: "essential",
    mainExport: "createCore",
    testCoverage: 76,
    label: "@unrdf/core"
  },
  "@unrdf/dark-matter": {
    name: "@unrdf/dark-matter",
    version: "latest",
    description: "UNRDF Dark Matter - Query Optimization and Performance Analysis (Optional Extension)",
    tier: "optional",
    mainExport: "createDarkMatter",
    testCoverage: 80,
    label: "@unrdf/dark-matter"
  },
  "@unrdf/decision-fabric": {
    name: "@unrdf/decision-fabric",
    version: "latest",
    description: "Hyperdimensional Decision Fabric - Intent-to-Outcome transformation engine using μ-operators",
    tier: "optional",
    mainExport: "createDecisionFabric",
    testCoverage: 90,
    label: "@unrdf/decision-fabric"
  },
  "@unrdf/diataxis-kit": {
    name: "@unrdf/diataxis-kit",
    version: "latest",
    description: "Diátaxis documentation kit for monorepo package inventory and deterministic doc scaffold generation",
    tier: "optional",
    mainExport: "createDiataxisKit",
    testCoverage: 78,
    label: "@unrdf/diataxis-kit"
  },
  "docs": {
    name: "docs",
    version: "latest",
    description: "",
    tier: "optional",
    mainExport: "createdocs",
    testCoverage: 87,
    label: "docs"
  },
  "@unrdf/domain": {
    name: "@unrdf/domain",
    version: "latest",
    description: "Domain models and types for UNRDF",
    tier: "optional",
    mainExport: "createDomain",
    testCoverage: 79,
    label: "@unrdf/domain"
  },
  "@unrdf/engine-gateway": {
    name: "@unrdf/engine-gateway",
    version: "latest",
    description: "μ(O) Engine Gateway - Enforcement layer for Oxigraph-first, N3-minimal RDF processing",
    tier: "optional",
    mainExport: "createEngineGateway",
    testCoverage: 77,
    label: "@unrdf/engine-gateway"
  },
  "@unrdf/federation": {
    name: "@unrdf/federation",
    version: "latest",
    description: "UNRDF Federation - Distributed RDF Query with RAFT Consensus and Multi-Master Replication",
    tier: "extended",
    mainExport: "createFederation",
    testCoverage: 80,
    label: "@unrdf/federation"
  },
  "@unrdf/fusion": {
    name: "@unrdf/fusion",
    version: "latest",
    description: "Unified integration layer for 7-day UNRDF innovation - KGC-4D, blockchain, hooks, caching",
    tier: "optional",
    mainExport: "createFusion",
    testCoverage: 88,
    label: "@unrdf/fusion"
  },
  "@unrdf/graph-analytics": {
    name: "@unrdf/graph-analytics",
    version: "latest",
    description: "Advanced graph analytics for RDF knowledge graphs using graphlib",
    tier: "optional",
    mainExport: "createGraphAnalytics",
    testCoverage: 85,
    label: "@unrdf/graph-analytics"
  },
  "@unrdf/hooks": {
    name: "@unrdf/hooks",
    version: "latest",
    description: "UNRDF Knowledge Hooks - Policy Definition and Execution Framework",
    tier: "essential",
    mainExport: "createHooks",
    testCoverage: 82,
    label: "@unrdf/hooks"
  },
  "@unrdf/integration-tests": {
    name: "@unrdf/integration-tests",
    version: "latest",
    description: "Phase 5: Comprehensive Integration & Adversarial Tests (75 tests)",
    tier: "optional",
    mainExport: "createIntegrationTests",
    testCoverage: 82,
    label: "@unrdf/integration-tests"
  },
  "@unrdf/kgc-4d": {
    name: "@unrdf/kgc-4d",
    version: "latest",
    description: "KGC 4D Datum & Universe Freeze Engine - Nanosecond-precision event logging with Git-backed snapshots",
    tier: "essential",
    mainExport: "createKgc4d",
    testCoverage: 86,
    label: "@unrdf/kgc-4d"
  },
  "@unrdf/kgc-4d-playground": {
    name: "@unrdf/kgc-4d-playground",
    version: "latest",
    description: "KGC-4D Playground - Shard-Based Architecture Demo with Perfect Client/Server Relationship",
    tier: "optional",
    mainExport: "createKgc4dPlayground",
    testCoverage: 89,
    label: "@unrdf/kgc-4d-playground"
  },
  "@unrdf/kgc-claude": {
    name: "@unrdf/kgc-claude",
    version: "latest",
    description: "KGC-Claude Substrate - Deterministic run objects, universal checkpoints, bounded autonomy, and multi-agent concurrency for Claude integration",
    tier: "optional",
    mainExport: "createKgcClaude",
    testCoverage: 76,
    label: "@unrdf/kgc-claude"
  },
  "@unrdf/kgc-cli": {
    name: "@unrdf/kgc-cli",
    version: "latest",
    description: "KGC CLI - Deterministic extension registry for ~40 workspace packages",
    tier: "optional",
    mainExport: "createKgcCli",
    testCoverage: 88,
    label: "@unrdf/kgc-cli"
  },
  "@unrdf/kgc-docs": {
    name: "@unrdf/kgc-docs",
    version: "latest",
    description: "KGC Markdown parser and dynamic documentation generator with proof anchoring",
    tier: "optional",
    mainExport: "createKgcDocs",
    testCoverage: 92,
    label: "@unrdf/kgc-docs"
  },
  "@unrdf/kgc-multiverse": {
    name: "@unrdf/kgc-multiverse",
    version: "latest",
    description: "KGC Multiverse - Universe branching, forking, and morphism algebra for knowledge graphs",
    tier: "optional",
    mainExport: "createKgcMultiverse",
    testCoverage: 91,
    label: "@unrdf/kgc-multiverse"
  },
  "@unrdf/kgc-probe": {
    name: "@unrdf/kgc-probe",
    version: "latest",
    description: "KGC Probe - Automated knowledge graph integrity scanning with 10 agents and artifact validation",
    tier: "optional",
    mainExport: "createKgcProbe",
    testCoverage: 78,
    label: "@unrdf/kgc-probe"
  },
  "@unrdf/kgc-runtime": {
    name: "@unrdf/kgc-runtime",
    version: "latest",
    description: "KGC governance runtime with comprehensive Zod schemas and work item system",
    tier: "extended",
    mainExport: "createKgcRuntime",
    testCoverage: 87,
    label: "@unrdf/kgc-runtime"
  },
  "@unrdf/kgc-substrate": {
    name: "@unrdf/kgc-substrate",
    version: "latest",
    description: "KGC Substrate - Deterministic, hash-stable KnowledgeStore with immutable append-only log",
    tier: "extended",
    mainExport: "createKgcSubstrate",
    testCoverage: 94,
    label: "@unrdf/kgc-substrate"
  },
  "@unrdf/kgc-swarm": {
    name: "@unrdf/kgc-swarm",
    version: "latest",
    description: "Multi-agent template orchestration with cryptographic receipts - KGC planning meets kgn rendering",
    tier: "optional",
    mainExport: "createKgcSwarm",
    testCoverage: 76,
    label: "@unrdf/kgc-swarm"
  },
  "@unrdf/kgc-tools": {
    name: "@unrdf/kgc-tools",
    version: "latest",
    description: "KGC Tools - Verification, freeze, and replay utilities for KGC capsules",
    tier: "optional",
    mainExport: "createKgcTools",
    testCoverage: 78,
    label: "@unrdf/kgc-tools"
  },
  "@unrdf/kgn": {
    name: "@unrdf/kgn",
    version: "latest",
    description: "Deterministic Nunjucks template system with custom filters and frontmatter support",
    tier: "optional",
    mainExport: "createKgn",
    testCoverage: 88,
    label: "@unrdf/kgn"
  },
  "@unrdf/knowledge-engine": {
    name: "@unrdf/knowledge-engine",
    version: "latest",
    description: "UNRDF Knowledge Engine - Rule Engine, Inference, and Pattern Matching (Optional Extension)",
    tier: "extended",
    mainExport: "createKnowledgeEngine",
    testCoverage: 91,
    label: "@unrdf/knowledge-engine"
  },
  "@unrdf/ml-inference": {
    name: "@unrdf/ml-inference",
    version: "latest",
    description: "UNRDF ML Inference - High-performance ONNX model inference pipeline for RDF streams",
    tier: "optional",
    mainExport: "createMlInference",
    testCoverage: 91,
    label: "@unrdf/ml-inference"
  },
  "@unrdf/ml-versioning": {
    name: "@unrdf/ml-versioning",
    version: "latest",
    description: "ML Model Versioning System using TensorFlow.js and UNRDF KGC-4D time-travel capabilities",
    tier: "optional",
    mainExport: "createMlVersioning",
    testCoverage: 88,
    label: "@unrdf/ml-versioning"
  },
  "@unrdf/nextra-docs": {
    name: "@unrdf/nextra-docs",
    version: "latest",
    description: "UNRDF documentation with Nextra 4 - Developer-focused Next.js documentation",
    tier: "optional",
    mainExport: "createNextraDocs",
    testCoverage: 94,
    label: "@unrdf/nextra-docs"
  },
  "@unrdf/observability": {
    name: "@unrdf/observability",
    version: "latest",
    description: "Innovative Prometheus/Grafana observability dashboard for UNRDF distributed workflows",
    tier: "optional",
    mainExport: "createObservability",
    testCoverage: 82,
    label: "@unrdf/observability"
  },
  "@unrdf/oxigraph": {
    name: "@unrdf/oxigraph",
    version: "latest",
    description: "UNRDF Oxigraph - Graph database benchmarking implementation using Oxigraph SPARQL engine",
    tier: "essential",
    mainExport: "createOxigraph",
    testCoverage: 85,
    label: "@unrdf/oxigraph"
  },
  "@unrdf/project-engine": {
    name: "@unrdf/project-engine",
    version: "latest",
    description: "UNRDF Project Engine - Self-hosting Tools and Infrastructure (Development Only)",
    tier: "optional",
    mainExport: "createProjectEngine",
    testCoverage: 92,
    label: "@unrdf/project-engine"
  },
  "@unrdf/rdf-graphql": {
    name: "@unrdf/rdf-graphql",
    version: "latest",
    description: "Type-safe GraphQL interface for RDF knowledge graphs with automatic schema generation",
    tier: "optional",
    mainExport: "createRdfGraphql",
    testCoverage: 88,
    label: "@unrdf/rdf-graphql"
  },
  "@unrdf/react": {
    name: "@unrdf/react",
    version: "latest",
    description: "UNRDF React - AI Semantic Analysis Tools for RDF Knowledge Graphs (Optional Extension)",
    tier: "optional",
    mainExport: "createReact",
    testCoverage: 83,
    label: "@unrdf/react"
  },
  "@unrdf/receipts": {
    name: "@unrdf/receipts",
    version: "latest",
    description: "KGC Receipts - Batch receipt generation with Merkle tree verification for knowledge graph operations",
    tier: "extended",
    mainExport: "createReceipts",
    testCoverage: 91,
    label: "@unrdf/receipts"
  },
  "@unrdf/semantic-search": {
    name: "@unrdf/semantic-search",
    version: "latest",
    description: "AI-powered semantic search over RDF knowledge graphs using vector embeddings",
    tier: "optional",
    mainExport: "createSemanticSearch",
    testCoverage: 86,
    label: "@unrdf/semantic-search"
  },
  "@unrdf/serverless": {
    name: "@unrdf/serverless",
    version: "latest",
    description: "UNRDF Serverless - One-click AWS deployment for RDF applications",
    tier: "optional",
    mainExport: "createServerless",
    testCoverage: 81,
    label: "@unrdf/serverless"
  },
  "@unrdf/streaming": {
    name: "@unrdf/streaming",
    version: "latest",
    description: "UNRDF Streaming - Change Feeds and Real-time Synchronization",
    tier: "essential",
    mainExport: "createStreaming",
    testCoverage: 81,
    label: "@unrdf/streaming"
  },
  "@unrdf/test-utils": {
    name: "@unrdf/test-utils",
    version: "latest",
    description: "Testing utilities for UNRDF development",
    tier: "optional",
    mainExport: "createTestUtils",
    testCoverage: 78,
    label: "@unrdf/test-utils"
  },
  "@unrdf/v6-compat": {
    name: "@unrdf/v6-compat",
    version: "latest-rc.1",
    description: "UNRDF v6 Compatibility Layer - v5 to v6 migration bridge with adapters and lint rules",
    tier: "extended",
    mainExport: "createV6Compat",
    testCoverage: 78,
    label: "@unrdf/v6-compat"
  },
  "@unrdf/v6-core": {
    name: "@unrdf/v6-core",
    version: "latest-rc.1",
    description: "UNRDF v6 Core - ΔGate control plane, unified receipts, and delta contracts",
    tier: "essential",
    mainExport: "createV6Core",
    testCoverage: 80,
    label: "@unrdf/v6-core"
  },
  "@unrdf/validation": {
    name: "@unrdf/validation",
    version: "latest",
    description: "OTEL validation framework for UNRDF development",
    tier: "optional",
    mainExport: "createValidation",
    testCoverage: 77,
    label: "@unrdf/validation"
  },
  "@unrdf/yawl": {
    name: "@unrdf/yawl",
    version: "latest",
    description: "YAWL (Yet Another Workflow Language) engine with KGC-4D time-travel and receipt verification",
    tier: "essential",
    mainExport: "createYawl",
    testCoverage: 80,
    label: "@unrdf/yawl"
  },
  "@unrdf/yawl-ai": {
    name: "@unrdf/yawl-ai",
    version: "latest",
    description: "AI-powered workflow optimization using TensorFlow.js and YAWL patterns",
    tier: "optional",
    mainExport: "createYawlAi",
    testCoverage: 92,
    label: "@unrdf/yawl-ai"
  },
  "@unrdf/yawl-api": {
    name: "@unrdf/yawl-api",
    version: "latest",
    description: "High-performance REST API framework that exposes YAWL workflows as RESTful APIs with OpenAPI documentation",
    tier: "optional",
    mainExport: "createYawlApi",
    testCoverage: 77,
    label: "@unrdf/yawl-api"
  },
  "@unrdf/yawl-durable": {
    name: "@unrdf/yawl-durable",
    version: "latest",
    description: "Durable execution framework inspired by Temporal.io using YAWL and KGC-4D",
    tier: "optional",
    mainExport: "createYawlDurable",
    testCoverage: 79,
    label: "@unrdf/yawl-durable"
  },
  "@unrdf/yawl-kafka": {
    name: "@unrdf/yawl-kafka",
    version: "latest",
    description: "Apache Kafka event streaming integration for YAWL workflows with Avro serialization",
    tier: "optional",
    mainExport: "createYawlKafka",
    testCoverage: 87,
    label: "@unrdf/yawl-kafka"
  },
  "@unrdf/yawl-langchain": {
    name: "@unrdf/yawl-langchain",
    version: "latest",
    description: "LangChain integration for YAWL workflow engine - AI-powered workflow orchestration with RDF context",
    tier: "optional",
    mainExport: "createYawlLangchain",
    testCoverage: 75,
    label: "@unrdf/yawl-langchain"
  },
  "@unrdf/yawl-observability": {
    name: "@unrdf/yawl-observability",
    version: "latest",
    description: "Workflow observability framework with Prometheus metrics and OpenTelemetry tracing for YAWL",
    tier: "optional",
    mainExport: "createYawlObservability",
    testCoverage: 91,
    label: "@unrdf/yawl-observability"
  },
  "@unrdf/yawl-queue": {
    name: "@unrdf/yawl-queue",
    version: "latest",
    description: "Distributed YAWL workflow execution using BullMQ and Redis",
    tier: "optional",
    mainExport: "createYawlQueue",
    testCoverage: 92,
    label: "@unrdf/yawl-queue"
  },
  "@unrdf/yawl-realtime": {
    name: "@unrdf/yawl-realtime",
    version: "latest",
    description: "Real-time collaboration framework for YAWL workflows using Socket.io",
    tier: "optional",
    mainExport: "createYawlRealtime",
    testCoverage: 86,
    label: "@unrdf/yawl-realtime"
  },
  "@unrdf/yawl-viz": {
    name: "@unrdf/yawl-viz",
    version: "latest",
    description: "Real-time D3.js visualization for YAWL workflows with Van der Aalst pattern rendering",
    tier: "optional",
    mainExport: "createYawlViz",
    testCoverage: 84,
    label: "@unrdf/yawl-viz"
  }
};

export function getRegistry(): PackageRegistry {
  return {
    packages: Object.values(PACKAGES),
    essential: Object.values(PACKAGES).filter(p => p.tier === 'essential'),
    extended: Object.values(PACKAGES).filter(p => p.tier === 'extended'),
    optional: Object.values(PACKAGES).filter(p => p.tier === 'optional'),
    total: Object.keys(PACKAGES).length
  };
}

export function getPackage(name: string): Package | undefined {
  return PACKAGES[name];
}

export function findByTier(tier: 'essential' | 'extended' | 'optional'): Package[] {
  return Object.values(PACKAGES).filter(p => p.tier === tier);
}
