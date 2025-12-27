# KGC Probe Data Schemas & Examples

**Purpose**: Concrete JSON/YAML specifications for shard, artifact, receipt, and configuration structures.

**Audience**: Implementation team (Node.js developers)

---

## 1. Shard Schema

### 1.1 Shard File Format

Each agent produces one JSON file in `shards/` directory named `<agent-name>.json`:

```jsonschema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "Agent Shard",
  "description": "Output from single agent during probe scan",
  "type": "object",
  "properties": {
    "agentId": {
      "type": "string",
      "pattern": "^[0-9]{2}$",
      "description": "Agent ID 01-10"
    },
    "agentName": {
      "type": "string",
      "enum": [
        "orchestrator",
        "runtime",
        "cache",
        "federation",
        "consensus",
        "hooks",
        "domain",
        "claude",
        "substrate",
        "knowledge-engine"
      ]
    },
    "timestamp": {
      "type": "string",
      "format": "date-time",
      "description": "ISO 8601 timestamp when agent completed"
    },
    "duration_ms": {
      "type": "integer",
      "minimum": 0,
      "description": "Execution time in milliseconds"
    },
    "hash": {
      "type": "string",
      "pattern": "^[a-f0-9]{64}$",
      "description": "SHA256 hash of entire shard output"
    },
    "output": {
      "type": "object",
      "description": "Agent-specific output structure",
      "properties": {
        "claims": {
          "type": "array",
          "description": "List of capability/knowledge claims",
          "items": {
            "type": "object",
            "properties": {
              "id": {
                "type": "string",
                "pattern": "^[a-z0-9-]+$",
                "description": "Unique claim identifier (e.g., 'cap-snapshot-create')"
              },
              "name": {
                "type": "string",
                "description": "Human-readable claim name"
              },
              "description": {
                "type": "string",
                "description": "Detailed explanation"
              },
              "value": {
                "description": "Claim value (string, object, or array)"
              },
              "confidence": {
                "type": "number",
                "minimum": 0,
                "maximum": 1,
                "description": "Confidence level (0-1)"
              },
              "tags": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Semantic tags (e.g., ['async', 'rdf', 'deterministic'])"
              },
              "example": {
                "type": "string",
                "description": "Code or usage example"
              },
              "references": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Related claim IDs"
              }
            },
            "required": ["id", "name", "value"]
          }
        },
        "metadata": {
          "type": "object",
          "properties": {
            "apiVersion": {"type": "string"},
            "rdfGraph": {"type": "boolean"},
            "statementCount": {"type": "integer"}
          }
        }
      },
      "required": ["claims"]
    }
  },
  "required": ["agentId", "agentName", "timestamp", "output"]
}
```

### 1.2 Example Shard (Orchestrator)

```json
{
  "agentId": "01",
  "agentName": "orchestrator",
  "timestamp": "2025-01-01T10:00:01.234Z",
  "duration_ms": 1234,
  "hash": "a3f4b8c2d9e1f6a8b7c5d3e2f1a9b8c7d6e5f4a3b2c1d0e9f8a7b6c5d4e3",
  "output": {
    "claims": [
      {
        "id": "cap-snapshot-create",
        "name": "Snapshot Creation",
        "description": "Create immutable universe snapshot with timestamp and metadata",
        "value": {
          "inputType": "object",
          "outputType": "snapshot",
          "async": true,
          "deterministic": true
        },
        "confidence": 0.99,
        "tags": ["snapshot", "universe", "write"],
        "example": "kgc snapshot create --args '{\"universe\":\"my-universe\",\"message\":\"v1.0\"}'",
        "references": ["cap-snapshot-restore", "cap-snapshot-list"]
      },
      {
        "id": "cap-snapshot-restore",
        "name": "Snapshot Restore",
        "description": "Restore universe to previously saved snapshot",
        "value": {
          "inputType": "snapshotId",
          "outputType": "universe",
          "async": true,
          "deterministic": false
        },
        "confidence": 0.95,
        "tags": ["snapshot", "universe", "read"],
        "example": "kgc snapshot restore --args '{\"snapshotId\":\"snap_1\"}'"
      }
    ],
    "metadata": {
      "apiVersion": "5.0.1",
      "rdfGraph": true,
      "statementCount": 24
    }
  }
}
```

---

## 2. Merged Artifact Schema

### 2.1 Artifact File Format

Primary artifact file: `merged/index.json`

```jsonschema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "Merged Artifact",
  "description": "Consolidated knowledge from all agents",
  "type": "object",
  "properties": {
    "metadata": {
      "type": "object",
      "properties": {
        "runId": {
          "type": "string",
          "pattern": "^[0-9]{8}-[0-9]{6}-[a-z0-9]{6}$",
          "description": "Unique run identifier"
        },
        "mergedAt": {
          "type": "string",
          "format": "date-time"
        },
        "version": {
          "type": "string",
          "pattern": "^[0-9]+\\.[0-9]+\\.[0-9]+$"
        },
        "shardCount": {
          "type": "integer",
          "minimum": 1
        }
      },
      "required": ["runId", "mergedAt", "version", "shardCount"]
    },
    "agents": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "id": {
            "type": "string",
            "pattern": "^[0-9]{2}$"
          },
          "name": {"type": "string"},
          "timestamp": {"type": "string", "format": "date-time"},
          "shardHash": {
            "type": "string",
            "pattern": "^[a-f0-9]{64}$"
          },
          "claimCount": {"type": "integer", "minimum": 0}
        },
        "required": ["id", "name", "timestamp", "shardHash", "claimCount"]
      }
    },
    "claims": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "id": {"type": "string"},
          "name": {"type": "string"},
          "description": {"type": "string"},
          "value": {},
          "confidence": {"type": "number", "minimum": 0, "maximum": 1},
          "tags": {"type": "array", "items": {"type": "string"}},
          "example": {"type": "string"},
          "source": {
            "type": "string",
            "description": "Agent that provided this claim"
          },
          "timestamp": {"type": "string", "format": "date-time"},
          "shardHash": {
            "type": "string",
            "pattern": "^[a-f0-9]{64}$",
            "description": "Hash of original shard"
          },
          "sources": {
            "type": "array",
            "items": {"type": "string"},
            "description": "For merged claims, list of all contributing agents"
          },
          "mergeNote": {
            "type": "string",
            "description": "Explanation if claim was merged from multiple sources"
          }
        },
        "required": ["id", "name", "value", "source", "timestamp"]
      }
    },
    "statistics": {
      "type": "object",
      "properties": {
        "totalClaims": {"type": "integer"},
        "totalAgents": {"type": "integer"},
        "claimsByAgent": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "agent": {"type": "string"},
              "count": {"type": "integer"}
            }
          }
        },
        "lastUpdated": {"type": "string", "format": "date-time"},
        "coverage": {
          "type": "number",
          "minimum": 0,
          "maximum": 1,
          "description": "Estimated capability coverage (0-1)"
        }
      }
    }
  },
  "required": ["metadata", "agents", "claims", "statistics"]
}
```

### 2.2 Example Merged Artifact

```json
{
  "metadata": {
    "runId": "20250101-120000-abc123",
    "mergedAt": "2025-01-01T12:00:05.678Z",
    "version": "1.0.0",
    "shardCount": 10
  },
  "agents": [
    {
      "id": "01",
      "name": "orchestrator",
      "timestamp": "2025-01-01T10:00:01.234Z",
      "shardHash": "a3f4b8c2d9e1f6a8b7c5d3e2f1a9b8c7d6e5f4a3b2c1d0e9f8a7b6c5d4e3",
      "claimCount": 12
    },
    {
      "id": "02",
      "name": "runtime",
      "timestamp": "2025-01-01T10:00:02.345Z",
      "shardHash": "f4a3b2c1d0e9f8a7b6c5d4e3f2a1b0c9d8e7f6a5b4c3d2e1f0a9b8c7d6e5",
      "claimCount": 8
    }
  ],
  "claims": [
    {
      "id": "cap-snapshot-create",
      "name": "Snapshot Creation",
      "description": "Create immutable universe snapshot",
      "value": {
        "inputType": "object",
        "outputType": "snapshot",
        "async": true
      },
      "confidence": 0.99,
      "tags": ["snapshot", "write"],
      "example": "kgc snapshot create",
      "source": "orchestrator",
      "timestamp": "2025-01-01T10:00:01.234Z",
      "shardHash": "a3f4b8c2d9e1f6a8b7c5d3e2f1a9b8c7d6e5f4a3b2c1d0e9f8a7b6c5d4e3"
    }
  ],
  "statistics": {
    "totalClaims": 345,
    "totalAgents": 10,
    "claimsByAgent": [
      {"agent": "orchestrator", "count": 42},
      {"agent": "runtime", "count": 38}
    ],
    "lastUpdated": "2025-01-01T10:00:05.000Z",
    "coverage": 0.94
  }
}
```

---

## 3. Receipt Schemas

### 3.1 Hash Chain Receipt

File: `receipts/chain.json`

```jsonschema
{
  "type": "object",
  "properties": {
    "chain": {
      "type": "array",
      "description": "Sequential hash chain entries",
      "items": {
        "type": "object",
        "properties": {
          "agentName": {"type": "string"},
          "shardHash": {
            "type": "string",
            "pattern": "^[a-f0-9]{64}$",
            "description": "SHA256 of original shard"
          },
          "previousHash": {
            "type": ["string", "null"],
            "pattern": "^[a-f0-9]{64}$|^null$",
            "description": "Hash from previous entry in chain"
          },
          "timestamp": {"type": "string", "format": "date-time"},
          "hash": {
            "type": "string",
            "pattern": "^[a-f0-9]{64}$",
            "description": "SHA256 of this entry (including previousHash)"
          },
          "signature": {
            "type": "string",
            "description": "Optional RSA/ECDSA signature of entry.hash"
          }
        },
        "required": ["agentName", "shardHash", "timestamp", "hash"]
      }
    },
    "root": {
      "type": "string",
      "pattern": "^[a-f0-9]{64}$",
      "description": "Hash of final entry (merkle-like root)"
    },
    "agentCount": {"type": "integer"}
  },
  "required": ["chain", "root", "agentCount"]
}
```

### 3.2 Example Hash Chain

```json
{
  "chain": [
    {
      "agentName": "cache",
      "shardHash": "c1a2b3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0",
      "previousHash": null,
      "timestamp": "2025-01-01T10:00:00.100Z",
      "hash": "d9e8f7a6b5c4d3e2f1a0b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f3a2b1c0"
    },
    {
      "agentName": "claude",
      "shardHash": "e2a1b0c9d8e7f6a5b4c3d2e1f0a9b8c7d6e5f4a3b2c1d0e9f8a7b6c5d4e3",
      "previousHash": "d9e8f7a6b5c4d3e2f1a0b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f3a2b1c0",
      "timestamp": "2025-01-01T10:00:00.200Z",
      "hash": "f0e9d8c7b6a5c4d3e2f1a0b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f3a2b1"
    }
  ],
  "root": "f0e9d8c7b6a5c4d3e2f1a0b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f3a2b1",
  "agentCount": 2
}
```

### 3.3 Merkle Tree Receipt

File: `receipts/merkle.json`

```jsonschema
{
  "type": "object",
  "properties": {
    "root": {
      "type": "string",
      "pattern": "^[a-f0-9]{64}$",
      "description": "Merkle tree root hash"
    },
    "height": {
      "type": "integer",
      "description": "Tree height (log2 of leaf count, rounded up)"
    },
    "leafCount": {
      "type": "integer",
      "description": "Number of shard leaves"
    },
    "tree": {
      "type": "object",
      "description": "Tree structure for visualization",
      "properties": {
        "levels": {
          "type": "array",
          "description": "Merkle tree levels (bottom-up)"
        }
      }
    },
    "proofs": {
      "type": "array",
      "description": "Membership proofs for each shard",
      "items": {
        "type": "object",
        "properties": {
          "agentName": {"type": "string"},
          "leafHash": {
            "type": "string",
            "pattern": "^[a-f0-9]{64}$"
          },
          "path": {
            "type": "array",
            "description": "Path from leaf to root (hash/direction pairs)",
            "items": {
              "type": "object",
              "properties": {
                "hash": {"type": "string", "pattern": "^[a-f0-9]{64}$"},
                "direction": {"type": "string", "enum": ["left", "right"]}
              }
            }
          },
          "verified": {"type": "boolean"}
        },
        "required": ["agentName", "leafHash", "path"]
      }
    }
  },
  "required": ["root", "height", "leafCount", "proofs"]
}
```

### 3.4 Example Merkle Tree

```json
{
  "root": "abc123def456abc123def456abc123def456abc123def456abc123def456abc1",
  "height": 4,
  "leafCount": 10,
  "proofs": [
    {
      "agentName": "orchestrator",
      "leafHash": "d9e8f7a6b5c4d3e2f1a0b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f3a2b1c0",
      "path": [
        {
          "hash": "e2a1b0c9d8e7f6a5b4c3d2e1f0a9b8c7d6e5f4a3b2c1d0e9f8a7b6c5d4e3",
          "direction": "right"
        },
        {
          "hash": "f0e9d8c7b6a5c4d3e2f1a0b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f3a2b1",
          "direction": "left"
        }
      ],
      "verified": true
    }
  ]
}
```

---

## 4. Configuration Schema

### 4.1 .kgc-probe.json

```jsonschema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "version": {
      "type": "string",
      "pattern": "^[0-9]+\\.[0-9]+\\.[0-9]+$"
    },
    "allowlist": {
      "type": "array",
      "items": {"type": "string"},
      "description": "Agent names to include in probe"
    },
    "guards": {
      "type": "object",
      "description": "Per-agent execution guards",
      "additionalProperties": {
        "type": "object",
        "properties": {
          "timeLimit": {"type": "integer", "minimum": 1000},
          "memoryLimit": {"type": "string", "pattern": "^[0-9]+(MB|GB)$"},
          "refusals": {"type": "array", "items": {"type": "string"}},
          "preconditions": {"type": "array", "items": {"type": "string"}}
        }
      }
    },
    "mergeRules": {
      "type": "object",
      "description": "Conflict resolution rules per claim ID",
      "properties": {
        "default": {
          "type": "string",
          "enum": ["keep-first", "keep-last", "merge-both", "fail"]
        }
      },
      "additionalProperties": {
        "type": "string",
        "enum": ["keep-first", "keep-last", "merge-both", "fail"]
      }
    },
    "schema": {
      "type": "object",
      "description": "Zod/JSON-Schema for shard validation"
    },
    "ontology": {
      "type": "object",
      "properties": {
        "namespace": {"type": "string"},
        "prefixes": {
          "type": "object",
          "additionalProperties": {"type": "string"}
        }
      }
    },
    "reports": {
      "type": "object",
      "properties": {
        "diataxisStructure": {"type": "boolean"},
        "includeExamples": {"type": "boolean"},
        "maxDepth": {"type": "integer", "minimum": 1}
      }
    }
  },
  "required": ["version", "allowlist"]
}
```

### 4.2 Example Configuration

```json
{
  "version": "1.0.0",
  "allowlist": [
    "orchestrator",
    "runtime",
    "cache",
    "federation",
    "consensus",
    "hooks",
    "domain",
    "claude",
    "substrate",
    "knowledge-engine"
  ],
  "guards": {
    "orchestrator": {
      "timeLimit": 30000,
      "memoryLimit": "512MB",
      "refusals": ["external-network", "filesystem-write"]
    },
    "runtime": {
      "timeLimit": 20000,
      "memoryLimit": "256MB"
    },
    "*": {
      "timeLimit": 30000,
      "memoryLimit": "512MB"
    }
  },
  "mergeRules": {
    "default": "keep-first",
    "capability.version": "keep-last",
    "metadata.timestamp": "keep-last",
    "claims.#schema": "merge-both"
  },
  "schema": {
    "type": "object",
    "properties": {
      "agentId": {"type": "string"},
      "agentName": {"type": "string"},
      "claims": {
        "type": "array",
        "items": {
          "type": "object",
          "required": ["id", "name"]
        }
      }
    },
    "required": ["agentId", "claims"]
  },
  "ontology": {
    "namespace": "https://unrdf.io/kgc/probe/",
    "prefixes": {
      "kgc": "https://unrdf.io/kgc/",
      "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
      "xsd": "http://www.w3.org/2001/XMLSchema#"
    }
  },
  "reports": {
    "diataxisStructure": true,
    "includeExamples": true,
    "maxDepth": 3
  }
}
```

---

## 5. Diff Schema

### 5.1 Delta File Format

File: `diff/delta.json`

```jsonschema
{
  "type": "object",
  "properties": {
    "delta": {
      "type": "object",
      "properties": {
        "added": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "claim": {"type": "string"},
              "agent": {"type": "string"},
              "timestamp": {"type": "string", "format": "date-time"},
              "source_line": {"type": "integer"}
            }
          }
        },
        "removed": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "claim": {"type": "string"},
              "agent": {"type": "string"},
              "timestamp": {"type": "string", "format": "date-time"}
            }
          }
        },
        "modified": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "claim": {"type": "string"},
              "oldValue": {},
              "newValue": {},
              "agent": {"type": "string"}
            }
          }
        }
      }
    },
    "summary": {
      "type": "object",
      "properties": {
        "addedCount": {"type": "integer"},
        "removedCount": {"type": "integer"},
        "modifiedCount": {"type": "integer"},
        "changesetSize": {"type": "integer"},
        "timestamp": {"type": "string", "format": "date-time"},
        "oldSize": {"type": "integer"},
        "newSize": {"type": "integer"}
      }
    }
  }
}
```

### 5.2 Example Delta

```json
{
  "delta": {
    "added": [
      {
        "claim": "cap-advanced-query",
        "agent": "knowledge-engine",
        "timestamp": "2025-01-01T11:00:00Z",
        "source_line": 42
      }
    ],
    "removed": [
      {
        "claim": "cap-deprecated-v1",
        "agent": "runtime",
        "timestamp": "2024-12-01T10:00:00Z"
      }
    ],
    "modified": [
      {
        "claim": "cap-snapshot-create",
        "oldValue": {"async": false},
        "newValue": {"async": true},
        "agent": "orchestrator"
      }
    ]
  },
  "summary": {
    "addedCount": 15,
    "removedCount": 3,
    "modifiedCount": 2,
    "changesetSize": 20,
    "timestamp": "2025-01-01T12:00:00Z",
    "oldSize": 345,
    "newSize": 357
  }
}
```

---

## 6. Output Directory Structure Example

```
probe/out/20250101-120000-abc123/
├── shards/
│   ├── orchestrator.json                    (1.2 MB)
│   ├── runtime.json                         (0.8 MB)
│   ├── cache.json                           (0.5 MB)
│   ├── federation.json                      (0.7 MB)
│   ├── consensus.json                       (0.6 MB)
│   ├── hooks.json                           (0.9 MB)
│   ├── domain.json                          (0.4 MB)
│   ├── claude.json                          (2.1 MB)
│   ├── substrate.json                       (1.8 MB)
│   └── knowledge-engine.json                (1.5 MB)
│
├── merged/
│   ├── world.ttl                            (12 MB RDF Turtle)
│   ├── index.json                           (8.5 MB JSON)
│   └── report.md                            (2.3 MB Markdown)
│
├── receipts/
│   ├── chain.json                           (45 KB hash chain)
│   ├── merkle.json                          (120 KB merkle tree)
│   ├── verification-log.txt                 (8 KB verification results)
│   └── keys.json                            (optional, for crypto)
│
├── diff/
│   └── delta.json                           (380 KB, if comparing)
│
└── meta/
    ├── config.json                          (2.5 KB config used)
    ├── manifest.json                        (1.8 KB agent metadata)
    └── metrics.json                         (3.2 KB timing/sizes)

Total size: ~35-45 MB for full run
```

---

## 7. Implementation Notes

### Zod Validation Example (Node.js)

```javascript
import { z } from 'zod';

const ShardSchema = z.object({
  agentId: z.string().regex(/^\d{2}$/),
  agentName: z.enum([
    'orchestrator', 'runtime', 'cache', 'federation',
    'consensus', 'hooks', 'domain', 'claude',
    'substrate', 'knowledge-engine'
  ]),
  timestamp: z.string().datetime(),
  duration_ms: z.number().int().nonnegative().optional(),
  hash: z.string().regex(/^[a-f0-9]{64}$/),
  output: z.object({
    claims: z.array(z.object({
      id: z.string().regex(/^[a-z0-9-]+$/),
      name: z.string(),
      value: z.any(),
      confidence: z.number().min(0).max(1).optional(),
      tags: z.array(z.string()).optional(),
      example: z.string().optional(),
      references: z.array(z.string()).optional()
    })),
    metadata: z.object({
      apiVersion: z.string().optional(),
      rdfGraph: z.boolean().optional()
    }).optional()
  })
});

// Usage
const shard = await loadShard('shards/orchestrator.json');
const validated = ShardSchema.parse(shard);
```

### RDF Output Example (Turtle)

```turtle
@prefix kgc: <https://unrdf.io/kgc/probe/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

kgc:report-20250101-120000-abc123
    a kgc:ProbeReport ;
    kgc:runId "20250101-120000-abc123" ;
    kgc:timestamp "2025-01-01T12:00:05Z"^^xsd:dateTime ;
    kgc:hasAgent kgc:agent-orchestrator, kgc:agent-runtime, ... ;
    kgc:hasCapability kgc:cap-snapshot-create, kgc:cap-snapshot-restore, ... ;
    kgc:totalClaims 345 ;
    kgc:totalAgents 10 .

kgc:cap-snapshot-create
    a kgc:Capability ;
    rdfs:label "Snapshot Creation" ;
    rdfs:comment "Create immutable universe snapshot with timestamp and metadata" ;
    kgc:providedBy kgc:agent-orchestrator ;
    kgc:confidence 0.99 ;
    kgc:tag "snapshot", "write", "async" ;
    kgc:example "kgc snapshot create --args '{...}'" ;
    kgc:relatedTo kgc:cap-snapshot-restore .

kgc:agent-orchestrator
    a kgc:Agent ;
    rdfs:label "Orchestrator" ;
    kgc:agentId "01" ;
    kgc:processingTime 1234 ;
    kgc:providesClaims 12 ;
    kgc:shardHash "a3f4b8c2..."^^xsd:hexBinary .
```

---

## Conclusion

These schemas provide:

1. **Strict validation** via JSON-Schema and Zod
2. **Concrete examples** for all data structures
3. **Type safety** for implementation
4. **Deterministic formatting** (alphabetical keys, consistent hashing)
5. **Future extensibility** (additionalProperties: true where appropriate)

Implementers should:
- Use Zod for runtime validation in Node.js
- Generate Turtle RDF using @unrdf/oxigraph
- Validate all shards before merge
- Sign hash chains if crypto verification is needed

