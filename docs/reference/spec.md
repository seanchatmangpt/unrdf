# KGC Runtime Schema Specification

**Version:** 1.0.0
**Status:** Draft
**Last Updated:** 2024-12-26

## Overview

This document specifies the comprehensive Zod schemas for the KGC (Knowledge Graph Computation) governance runtime substrate. These schemas provide type-safe validation for all core runtime components with strict versioning, immutability guarantees, and cryptographic anchoring.

## Design Principles

### 1. Versioning

- All schemas use semantic versioning (semver)
- Default version: `1.0.0`
- Forward compatibility through version-aware parsing
- Breaking changes increment major version

### 2. Immutability

- Receipts are append-only, never modified
- Content-addressable storage via SHA-256 hashing
- Cryptographic signatures for authenticity
- Merkle proofs for batch verification

### 3. Validation

- Strict type checking with Zod
- Range constraints on all numeric fields
- Format validation (UUIDs, SHA-256, semver, URLs)
- Comprehensive error messages

### 4. Governance

- Actor attribution (who/what/when/why)
- Resource bounds enforcement
- Policy-driven access control
- Audit trail completeness

---

## Schema Definitions

### 1. Receipt Schema

**Purpose:** Versioned, immutable audit records for all runtime events.

**Use Cases:**

- Execution audit trails
- Cryptographic verification
- Compliance/regulatory requirements
- Debugging/forensics

**Structure:**

```typescript
{
  version: string;           // Semantic version (default: "1.0.0")
  id: string;                // UUID v4
  timestamp: number;         // Unix epoch milliseconds
  runId: string;             // Associated run identifier
  actor: string;             // Actor ID (format: "type:identifier")
  action: enum;              // Action performed
  payload: object;           // Action-specific data
  result?: {                 // Execution result
    success: boolean;
    output?: any;
    error?: string;
    duration?: number;
  };
  contentHash?: string;      // SHA-256 of receipt content
  previousHash?: string;     // SHA-256 of previous receipt (blockchain-style)
  signature?: {              // Cryptographic signature
    algorithm: enum;
    publicKey: string;
    value: string;
  };
  merkleProof?: {            // Merkle tree proof
    root: string;
    path: string[];
    index: number;
  };
  anchors?: Array<{          // External anchoring
    type: enum;
    reference: string;
    timestamp?: number;
  }>;
  metadata?: object;
}
```

**Actions:**

- `execute` - Run a workflow/task
- `validate` - Validate input/output
- `commit` - Commit changes to store
- `rollback` - Revert changes
- `checkpoint` - Save intermediate state
- `snapshot` - Create point-in-time snapshot
- `merge` - Merge branches/forks
- `fork` - Create new branch

**Actor Format:**

- `agent:<name>` - AI agent
- `user:<id>` - Human user
- `system:<component>` - System component

**Example:**

```javascript
{
  version: "1.0.0",
  id: "550e8400-e29b-41d4-a716-446655440000",
  timestamp: 1703001600000,
  runId: "run-2024-001",
  actor: "agent:orchestrator",
  action: "execute",
  payload: {
    workflowId: "wf-001",
    input: { x: 42 }
  },
  result: {
    success: true,
    output: { y: 84 },
    duration: 2314
  },
  contentHash: "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
  signature: {
    algorithm: "ed25519",
    publicKey: "0x1234...",
    value: "0xabcd..."
  }
}
```

---

### 2. RunCapsule Schema

**Purpose:** Complete execution snapshot (Δ_run) capturing input/output/trace/artifacts.

**Use Cases:**

- Reproducible execution
- Debugging/replay
- Result caching
- Provenance tracking

**Structure:**

```typescript
{
  id: string;                // Unique run identifier
  version: string;           // Schema version (default: "1.0.0")
  startTime: number;         // Unix epoch milliseconds
  endTime?: number;          // Unix epoch milliseconds (null if running)
  status: enum;              // Run status
  input: {
    task: string;            // Task description
    parameters?: object;     // Execution parameters
    context?: object;        // Initial context
    artifacts?: Array<{      // Input artifacts
      type: enum;
      path?: string;
      content?: string;
      hash?: string;
      metadata?: object;
    }>;
  };
  output?: {
    success: boolean;
    results?: object;
    artifacts?: Array<{      // Generated artifacts
      type: enum;
      path?: string;
      content?: string;
      hash?: string;
      size?: number;
      metadata?: object;
    }>;
    error?: {
      message: string;
      code?: string;
      stack?: string;
      recoverable?: boolean;
    };
  };
  trace?: ToolTraceEntry[];  // Ordered tool calls
  bounds?: Bounds;           // Resource limits
  actor: string;             // Actor ID
  receipt?: Receipt;         // Associated receipt
  provenance?: {             // Provenance chain
    parentRunId?: string;
    dependencies?: string[];
    derivedFrom?: string[];
  };
  checkpoints?: Array<{      // Long-running task checkpoints
    id: string;
    timestamp: number;
    state: object;
    progress?: number;       // 0.0-1.0
  }>;
  metadata?: object;
}
```

**Status Values:**

- `pending` - Not yet started
- `running` - Currently executing
- `completed` - Finished successfully
- `failed` - Failed with error
- `cancelled` - User/system cancelled
- `timeout` - Exceeded time limit

**Artifact Types:**

- `file` - File on disk
- `directory` - Directory
- `url` - Remote resource
- `inline` - Embedded content
- `proof` - Cryptographic proof
- `receipt` - Execution receipt

**Example:**

```javascript
{
  id: "run-2024-12-26-001",
  version: "1.0.0",
  startTime: 1703001600000,
  endTime: 1703001620000,
  status: "completed",
  input: {
    task: "Implement feature X",
    parameters: { timeout: 5000 },
    context: { workingDir: "/home/user/project" }
  },
  output: {
    success: true,
    results: { filesChanged: 5, testsAdded: 12 },
    artifacts: [
      {
        type: "file",
        path: "/home/user/project/src/feature.mjs",
        hash: "abc123...",
        size: 4567
      }
    ]
  },
  trace: [ /* ToolTraceEntry[] */ ],
  actor: "agent:backend-dev"
}
```

---

### 3. ToolTraceEntry Schema

**Purpose:** Atomic tool call records for deterministic replay.

**Use Cases:**

- Execution replay
- Debugging
- Performance profiling
- Causality tracking

**Structure:**

```typescript
{
  id: string;                // UUID v4
  timestamp: number;         // Unix epoch milliseconds
  toolName: string;          // Tool name (Bash, Read, Write, etc.)
  input: object;             // Tool input parameters
  output?: any;              // Tool output/result
  duration: number;          // Milliseconds
  status: enum;              // Execution status
  error?: {
    message: string;
    code?: string;
    stack?: string;
  };
  parentId?: string;         // Parent trace ID (for nested calls)
  dependencies?: string[];   // Dependent trace IDs
  resources?: {              // Resource usage
    cpuTime?: number;
    memoryPeak?: number;
    ioBytes?: number;
  };
  metadata?: object;
}
```

**Status Values:**

- `success` - Completed successfully
- `error` - Failed with error
- `timeout` - Exceeded time limit
- `cancelled` - User/system cancelled

**Example:**

```javascript
{
  id: "123e4567-e89b-12d3-a456-426614174000",
  timestamp: 1703001600000,
  toolName: "Bash",
  input: { command: "npm test", timeout: 5000 },
  output: { stdout: "✅ All tests passed", stderr: "", exitCode: 0 },
  duration: 2314,
  status: "success",
  resources: {
    cpuTime: 1200,
    memoryPeak: 45678901
  }
}
```

---

### 4. Bounds Schema

**Purpose:** Resource capacity limits enforcing governance constraints.

**Use Cases:**

- Resource exhaustion prevention
- Quota management
- Performance guarantees
- Cost control

**Structure:**

```typescript
{
  maxFiles: number;          // Max files created/modified (default: 100)
  maxBytes: number;          // Max bytes written (default: 10MB)
  maxOps: number;            // Max operations (default: 1000)
  maxRuntime: number;        // Max runtime ms (default: 5 min)
  maxGraphRewrites: number;  // Max graph changes (default: 50)
  enforcementPolicy: enum;   // Enforcement mode (default: "strict")
  warnings?: {               // Warning thresholds (0.0-1.0)
    filesThreshold: number;
    bytesThreshold: number;
    opsThreshold: number;
    runtimeThreshold: number;
    graphRewritesThreshold: number;
  };
  currentUsage?: {           // Current usage
    files: number;
    bytes: number;
    ops: number;
    runtime: number;
    graphRewrites: number;
  };
  metadata?: object;
}
```

**Enforcement Policies:**

- `strict` - Hard limits, reject on violation
- `soft` - Soft limits, log warnings but allow
- `monitor` - Monitor only, no enforcement

**Example:**

```javascript
{
  maxFiles: 100,
  maxBytes: 10485760,        // 10 MB
  maxOps: 1000,
  maxRuntime: 300000,        // 5 minutes
  maxGraphRewrites: 50,
  enforcementPolicy: "strict",
  warnings: {
    filesThreshold: 0.8,     // Warn at 80%
    bytesThreshold: 0.9,     // Warn at 90%
    opsThreshold: 0.75,
    runtimeThreshold: 0.9
  }
}
```

---

### 5. WorkItem Schema

**Purpose:** Async task node states for distributed execution.

**Use Cases:**

- Task queue management
- Distributed execution
- Dependency resolution
- Retry handling

**Structure:**

```typescript
{
  id: string;                // UUID v4
  type: string;              // Work item type
  state: enum;               // Current state
  priority: number;          // 0-100 (default: 50)
  createdAt: number;         // Unix epoch milliseconds
  startedAt?: number;        // When state became 'running'
  completedAt?: number;      // When state became terminal
  payload: object;           // Task-specific data
  result?: any;              // Populated on completion
  error?: {
    message: string;
    code?: string;
    stack?: string;
    retryable: boolean;
  };
  dependencies?: string[];   // Must complete before this
  retries?: {
    max: number;             // Max retries (default: 3)
    current: number;         // Current retry count
    backoff: enum;           // Backoff strategy
    delay: number;           // Base delay ms (default: 1000)
  };
  timeout: number;           // Execution timeout ms (default: 30000)
  assignedTo?: string;       // Agent/worker assigned
  policyEvaluation?: {       // Governance evaluation
    allowed: boolean;
    reason?: string;
    constraints?: object;
  };
  progress?: number;         // 0.0-1.0
  metadata?: object;
}
```

**State Machine:**

- `queued` → `running` → `succeeded`
- `queued` → `running` → `failed` (may retry)
- `queued` → `denied` (policy rejection)
- Any state → `cancelled` (user/system abort)

**Backoff Strategies:**

- `constant` - Fixed delay
- `linear` - Linearly increasing delay
- `exponential` - Exponentially increasing delay

**Example:**

```javascript
{
  id: "123e4567-e89b-12d3-a456-426614174000",
  type: "file_operation",
  state: "running",
  priority: 75,
  createdAt: 1703001600000,
  startedAt: 1703001605000,
  payload: {
    operation: "write",
    path: "/home/user/file.txt",
    content: "Hello World"
  },
  dependencies: ["223e4567-e89b-12d3-a456-426614174000"],
  retries: {
    max: 3,
    current: 0,
    backoff: "exponential",
    delay: 1000
  },
  timeout: 30000,
  assignedTo: "agent:worker-01"
}
```

---

### 6. ProjectionManifest Schema

**Purpose:** Surface definitions for CLI/docs/IDE/API/UI integration.

**Use Cases:**

- Multi-surface UX consistency
- Auto-generated integrations
- Policy-driven access control
- Version-aware compatibility

**Structure:**

```typescript
{
  version: string;           // Schema version (default: "1.0.0")
  surfaces: {
    cli?: {                  // CLI configuration
      commands?: Array<{
        name: string;
        description?: string;
        aliases?: string[];
        options?: Array<{
          name: string;
          type: enum;
          required: boolean;
          default?: any;
          description?: string;
        }>;
        examples?: string[];
      }>;
      globalOptions?: object;
    };
    docs?: {                 // Documentation configuration
      generator?: enum;
      outputDir: string;
      includes?: string[];
      excludes?: string[];
      theme?: string;
      navigation?: Array<{
        title: string;
        path: string;
        children?: any[];
      }>;
    };
    ide?: {                  // IDE integration
      lsp?: {
        enabled: boolean;
        port: number;
        features?: enum[];
      };
      snippets?: Array<{
        prefix: string;
        body: string;
        description?: string;
        scope?: string;
      }>;
      schemas?: Array<{
        fileMatch: string[];
        schema: object;
      }>;
    };
    api?: {                  // API configuration
      type: enum;
      baseUrl?: string;
      endpoints?: Array<{
        path: string;
        method?: enum;
        schema?: object;
        auth?: enum;
      }>;
      versioning?: {
        strategy: enum;
        current: string;
      };
    };
    ui?: {                   // UI configuration
      type: enum;
      framework?: string;
      routes?: Array<{
        path: string;
        component: string;
        title?: string;
      }>;
      theme?: object;
    };
  };
  accessControl?: {          // Access control
    default: enum;
    rules?: Array<{
      surface: string;
      action: string;
      allowed: boolean;
      roles?: string[];
    }>;
  };
  metadata?: object;
}
```

**Example:**

```javascript
{
  version: "1.0.0",
  surfaces: {
    cli: {
      commands: [
        {
          name: "run",
          description: "Execute a workflow",
          options: [
            { name: "file", type: "string", required: true }
          ],
          examples: ["kgc run workflow.yml"]
        }
      ]
    },
    docs: {
      generator: "typedoc",
      outputDir: "./docs",
      includes: ["**/*.mjs"]
    },
    ide: {
      lsp: { enabled: true, port: 9000 },
      snippets: [
        {
          prefix: "run",
          body: "RunCapsuleSchema.parse({ ... })"
        }
      ]
    }
  }
}
```

---

### 7. KGCMarkdown AST Schema

**Purpose:** Front matter + fenced block structured documents.

**Use Cases:**

- Literate programming
- Executable notebooks
- Semantic documentation
- Multi-format export

**Structure:**

```typescript
{
  type: "document";
  frontMatter?: {            // YAML metadata
    title?: string;
    version?: string;
    author?: string;
    date?: Date;
    ontology?: string[];     // URIs
    tags?: string[];
    custom?: object;
  };
  children: Array<           // AST nodes
    | HeadingNode
    | ParagraphNode
    | FencedBlockNode
    | ListNode
    | TableNode
    | LinkNode
    | ImageNode
    | BlockquoteNode
  >;
  metadata?: object;
}
```

**Node Types:**

**Heading:**

```typescript
{
  type: "heading";
  level: 1-6;
  content: string;
  id?: string;
  metadata?: object;
}
```

**Paragraph:**

```typescript
{
  type: "paragraph";
  content: string;
  semanticAnnotations?: Array<{
    predicate: string;       // URI
    object: string;
    datatype?: string;
  }>;
  metadata?: object;
}
```

**Fenced Block (Code):**

```typescript
{
  type: "fenced-block";
  language?: string;
  attributes?: object;
  content: string;
  output?: string;           // Execution output
  executable: boolean;
  metadata?: object;
}
```

**List:**

```typescript
{
  type: "list";
  ordered: boolean;
  items: string[];
  metadata?: object;
}
```

**Table:**

```typescript
{
  type: "table";
  headers?: string[];
  rows: string[][];
  alignment?: ("left" | "center" | "right")[];
  metadata?: object;
}
```

**Example:**

```javascript
{
  type: "document",
  frontMatter: {
    title: "KGC Example",
    version: "1.0.0",
    ontology: ["http://schema.org/"]
  },
  children: [
    {
      type: "heading",
      level: 1,
      content: "Introduction"
    },
    {
      type: "fenced-block",
      language: "javascript",
      executable: true,
      content: "console.log('Hello KGC');"
    },
    {
      type: "paragraph",
      content: "This is a paragraph."
    }
  ]
}
```

---

## Validation

All schemas provide validation functions:

```javascript
import { validateReceipt, validateRunCapsule, ... } from '@unrdf/kgc-runtime/schemas';

const result = validateReceipt(data);
if (result.success) {
  console.log('Valid:', result.data);
} else {
  console.error('Errors:', result.errors);
}
```

**Validation Result:**

```typescript
{
  success: boolean;
  data: T | null; // Validated data (if success)
  errors: Array<{
    // Validation errors (if failure)
    path: string; // Field path
    message: string; // Error message
    code: string; // Error code
  }>;
}
```

---

## Rationale

### Why Zod?

1. **Runtime validation** - TypeScript types are erased at compile time
2. **Schema inference** - Auto-generate TypeScript types from schemas
3. **Composability** - Complex schemas from simple primitives
4. **Developer experience** - Clear error messages, auto-completion
5. **Ecosystem** - Wide adoption, active maintenance

### Why SHA-256?

1. **Collision resistance** - Cryptographically secure
2. **Performance** - Fast computation
3. **Tooling** - Ubiquitous support across languages/platforms
4. **Standards** - NIST/FIPS approved

### Why UUID v4?

1. **Uniqueness** - Globally unique without coordination
2. **Randomness** - No predictability (security)
3. **Standards** - RFC 4122 compliant
4. **Tooling** - Native support in most environments

### Why Actor Prefixes?

1. **Type safety** - Parse failures on invalid format
2. **Self-documenting** - Clear attribution
3. **Access control** - Policy rules by actor type
4. **Auditing** - Easy filtering/grouping

---

## Migration Guide

### From v0.x to v1.0

1. Add `version: "1.0.0"` to all schemas
2. Migrate `actor` strings to `type:identifier` format
3. Replace custom hashes with SHA-256 (64 hex chars)
4. Update UUIDs to v4 format
5. Add `metadata` fields for extensibility

### Compatibility

- **Forward compatible**: v1.x parsers can read v1.y (y > x)
- **Breaking changes**: Major version increment required
- **Deprecation**: 2 minor versions notice before removal

---

## References

- [Zod Documentation](https://zod.dev/)
- [RFC 4122 - UUID](https://tools.ietf.org/html/rfc4122)
- [FIPS 180-4 - SHA-256](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf)
- [Semantic Versioning](https://semver.org/)
- [JSON Schema](https://json-schema.org/)

---

## Appendix A: Complete Example

```javascript
import {
  ReceiptSchema,
  RunCapsuleSchema,
  ToolTraceEntrySchema,
  BoundsSchema,
  WorkItemSchema,
  ProjectionManifestSchema,
  KGCMarkdownSchema,
} from '@unrdf/kgc-runtime/schemas';

// Create a complete run with all components
const trace = ToolTraceEntrySchema.parse({
  id: crypto.randomUUID(),
  timestamp: Date.now(),
  toolName: 'Bash',
  input: { command: 'npm test' },
  duration: 2000,
  status: 'success',
});

const receipt = ReceiptSchema.parse({
  id: crypto.randomUUID(),
  timestamp: Date.now(),
  runId: 'run-001',
  actor: 'agent:test',
  action: 'execute',
  payload: {},
});

const capsule = RunCapsuleSchema.parse({
  id: 'run-001',
  startTime: Date.now(),
  endTime: Date.now() + 5000,
  status: 'completed',
  input: { task: 'Test task' },
  output: { success: true, results: {} },
  trace: [trace],
  actor: 'agent:test',
  receipt,
});

console.log('✅ Complete run capsule:', capsule);
```

---

**Document Status:** Draft
**Next Review:** 2025-01-26
**Maintainer:** KGC Runtime Team
