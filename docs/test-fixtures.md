# Test Fixtures Definition

## Overview

This document specifies all test fixtures required for the KGC probe test suite. Fixtures provide reproducible, deterministic test data without depending on external systems.

**Directory Structure:**
```
tests/
├── fixtures/
│   ├── frozen-environment.json          (Determinism setup)
│   ├── shards/
│   │   ├── runtime_shard_1.json
│   │   ├── filesystem_shard_1.json
│   │   ├── wasm_shard_1.json
│   │   ├── environment_shard_1.json
│   │   ├── network_shard_1.json
│   │   ├── custom_1_shard_1.json
│   │   ├── custom_2_shard_1.json
│   │   ├── custom_3_shard_1.json
│   │   ├── logging_shard_1.json
│   │   └── metrics_shard_1.json
│   ├── guard-test-cases.json            (Guard enforcement)
│   ├── receipt-chain.json               (Receipt verification)
│   ├── test-projects/
│   │   └── real-project-1/              (E2E testing)
│   └── baselines/
│       └── e2e-baseline.json            (E2E validation)
└── helpers/
    ├── fixture-loader.mjs
    ├── environment-freezer.mjs
    └── hash-validator.mjs
```

---

## Fixture 1: Frozen Environment (Determinism)

**File:** `tests/fixtures/frozen-environment.json`

**Purpose:** Capture complete environment state for reproducible determinism testing

**Schema:**

```javascript
{
  // Snapshot metadata
  "id": "frozen-env-001",
  "createdAt": "2025-12-27T10:00:00.000Z",
  "description": "Determinism test environment snapshot",

  // File system snapshot
  "projectPath": "/tmp/test-project-det-001",
  "files": [
    {
      "path": "package.json",
      "content": "{\"name\": \"test-app\", ...}",
      "size": 1024,
      "mode": "0644",
      "mtime": "2025-12-27T10:00:00.000Z",
      // Pre-computed hash for quick validation
      "hash": "abc123def456abc123def456abc123de"
    },
    {
      "path": "src/index.js",
      "content": "console.log('test');",
      "size": 256,
      "mode": "0644",
      "mtime": "2025-12-27T10:00:00.000Z",
      "hash": "def456abc123def456abc123def456ab"
    },
    {
      "path": "src/utils.js",
      "content": "function add(a,b){return a+b;}",
      "size": 128,
      "mode": "0644",
      "mtime": "2025-12-27T10:00:00.000Z",
      "hash": "ghi789jkl012ghi789jkl012ghi789gh"
    },
    {
      "path": ".env.test",
      "content": "NODE_ENV=test\\nDEBUG=false",
      "size": 64,
      "mode": "0600",
      "mtime": "2025-12-27T10:00:00.000Z",
      "hash": "jkl012ghi789jkl012ghi789jkl012jk"
    },
    {
      "path": "config/database.js",
      "content": "module.exports = {host: 'localhost', ...}",
      "size": 512,
      "mode": "0644",
      "mtime": "2025-12-27T10:00:00.000Z",
      "hash": "mno345pqr678mno345pqr678mno345mn"
    }
  ],

  // Environment variables (frozen state)
  "environment": {
    "NODE_ENV": "test",
    "DEBUG": "false",
    "CUSTOM_VAR_1": "value1",
    "CUSTOM_VAR_2": "value2",
    "PORT": "3000",
    "LOG_LEVEL": "info"
  },

  // System time (fixed for all runs)
  "systemTime": {
    "iso": "2025-12-27T10:00:00.000Z",
    "unix": 1735308000000,
    "timezone": "UTC"
  },

  // Network simulation (frozen responses)
  "networkResponses": [
    {
      "url": "http://localhost:3000/api/health",
      "method": "GET",
      "statusCode": 200,
      "responseBody": "{\"status\": \"ok\"}",
      "latency": 10
    },
    {
      "url": "http://localhost:5432/",
      "method": "CONNECT",
      "statusCode": 0,
      "responseBody": null,
      "latency": 50,
      "error": "Connection refused"
    }
  ],

  // Process information (simulated)
  "process": {
    "pid": 12345,
    "ppid": 1,
    "cwd": "/tmp/test-project-det-001",
    "version": "18.0.0",
    "platform": "linux",
    "arch": "x64"
  },

  // Metadata for validation
  "metadata": {
    "seed": 42,                         // Random seed for reproducibility
    "version": "1.0",                   // Fixture version
    "checksum": "total_hash_of_all_files"
  }
}
```

**Usage in Tests:**

```javascript
// Load frozen environment
const frozenEnv = loadFixture("frozen-environment.json")

// Setup test (creates files, sets env vars, freezes time)
beforeEach(() => {
  setupFrozenEnvironment(frozenEnv)
})

// Teardown test
afterEach(() => {
  cleanupFrozenEnvironment(frozenEnv)
})

// Run determinism test 10 times with identical state
for (let run = 1; run <= 10; run++) {
  setupFrozenEnvironment(frozenEnv)
  const output = runProbeScan()
  const hash = SHA256(JSON.stringify(output))
  hashes.push(hash)
  cleanupFrozenEnvironment(frozenEnv)
}

// All hashes should be identical
expect(new Set(hashes).size).toBe(1)  // Only 1 unique hash
```

---

## Fixture 2: Precalculated Shards (Merge Testing)

**Files:** `tests/fixtures/shards/[domain]_shard_1.json` (10 files)

**Purpose:** Known shard outputs for testing merge logic without running actual agents

**Schema (Example: runtime_shard_1.json):**

```javascript
{
  // Metadata
  "domain": "runtime",
  "shardId": "runtime-shard-001",
  "version": "1.0",
  "generatedAt": "2025-12-27T10:00:00.000Z",

  // Observations produced by this shard
  "observationCount": 7,
  "observations": [
    {
      "id": "runtime-001",
      "domain": "runtime",
      "type": "function_execution",
      "functionName": "processData",
      "duration": 45,
      "status": "success",
      "timestamp": "2025-12-27T10:00:00.000Z",
      "metadata": {
        "callStack": ["main", "processData"]
      }
    },
    {
      "id": "runtime-002",
      "domain": "runtime",
      "type": "function_execution",
      "functionName": "validateInput",
      "duration": 12,
      "status": "success",
      "timestamp": "2025-12-27T10:00:00.100Z",
      "metadata": {
        "callStack": ["main", "validateInput"]
      }
    },
    {
      "id": "runtime-003",
      "domain": "runtime",
      "type": "error_thrown",
      "errorType": "TypeError",
      "errorMessage": "Cannot read property 'x' of undefined",
      "timestamp": "2025-12-27T10:00:00.200Z"
    },
    {
      "id": "runtime-004",
      "domain": "runtime",
      "type": "function_execution",
      "functionName": "cleanup",
      "duration": 5,
      "status": "success",
      "timestamp": "2025-12-27T10:00:00.300Z"
    },
    {
      "id": "runtime-005",
      "domain": "runtime",
      "type": "memory_usage",
      "heapUsed": 52428800,
      "heapTotal": 67108864,
      "external": 1048576,
      "timestamp": "2025-12-27T10:00:00.400Z"
    },
    {
      "id": "runtime-006",
      "domain": "runtime",
      "type": "async_operation",
      "operationType": "setTimeout",
      "delay": 100,
      "timestamp": "2025-12-27T10:00:00.500Z"
    },
    {
      "id": "runtime-007",
      "domain": "runtime",
      "type": "module_load",
      "moduleName": "express",
      "loadTime": 250,
      "timestamp": "2025-12-27T10:00:00.600Z"
    }
  ],

  // Guard status for each observation
  "guardStatuses": [
    { "observationId": "runtime-001", "status": "ALLOW" },
    { "observationId": "runtime-002", "status": "ALLOW" },
    { "observationId": "runtime-003", "status": "ALLOW" },
    { "observationId": "runtime-004", "status": "ALLOW" },
    { "observationId": "runtime-005", "status": "ALLOW" },
    { "observationId": "runtime-006", "status": "ALLOW" },
    { "observationId": "runtime-007", "status": "ALLOW" }
  ],

  // Receipts for this shard
  "receiptCount": 7,
  "receipts": [
    {
      "id": "runtime-receipt-001",
      "observationId": "runtime-001",
      "observationHash": "hash_of_runtime_001_content",
      "previousReceiptHash": null,
      "timestamp": "2025-12-27T10:00:00.000Z"
    },
    {
      "id": "runtime-receipt-002",
      "observationId": "runtime-002",
      "observationHash": "hash_of_runtime_002_content",
      "previousReceiptHash": "hash_of_runtime_receipt_001",
      "timestamp": "2025-12-27T10:00:00.100Z"
    }
    // ... remaining receipts
  ],

  // Hash of this entire shard
  "shardHash": "complete_hash_of_all_shard_data",

  // Validation checksums
  "checksums": {
    "observationsHash": "hash_of_observations_array",
    "receiptsHash": "hash_of_receipts_array"
  }
}
```

**For other domains (filesystem_shard_1.json):**

```javascript
{
  "domain": "filesystem",
  "observationCount": 10,
  "observations": [
    {
      "id": "fs-001",
      "domain": "filesystem",
      "type": "file_read",
      "path": "/home/user/unrdf/package.json",
      "size": 1024,
      "permissions": "0644",
      "timestamp": "2025-12-27T10:00:00.000Z"
    },
    {
      "id": "fs-002",
      "domain": "filesystem",
      "type": "file_write",
      "path": "/tmp/output.log",
      "size": 512,
      "permissions": "0644",
      "timestamp": "2025-12-27T10:00:00.100Z"
    },
    {
      "id": "fs-003",
      "domain": "filesystem",
      "type": "directory_scan",
      "path": "/home/user/unrdf/src",
      "fileCount": 15,
      "totalSize": 45000,
      "timestamp": "2025-12-27T10:00:00.200Z"
    },
    // ... more observations
  ],
  // ... same structure as runtime
}
```

**Usage in Tests:**

```javascript
// Load all precalculated shards
const shards = loadAllShards()

// Test merge with known inputs
const merged = mergeShardsWithVerification(shards)

// Verify observation count
const expectedTotal = Object.values(shards)
  .reduce((sum, shard) => sum + shard.observationCount, 0)
expect(merged.observations.length).toBe(expectedTotal)

// Verify Merkle root matches precalculated value
expect(merged.merkleProof.root).toBe(expectedMerkleRootHash)
```

---

## Fixture 3: Guard Test Cases

**File:** `tests/fixtures/guard-test-cases.json`

**Purpose:** Test scenarios with forbidden observations

**Schema:**

```javascript
{
  "version": "1.0",
  "testCases": [
    // CASE 1: AWS Secrets (Environment)
    {
      "name": "AWS_SECRET_ACCESS_KEY exposure",
      "category": "environment",
      "guardRuleId": "deny_aws_secrets",
      "shouldDeny": true,
      "observation": {
        "domain": "environment",
        "variable": "AWS_SECRET_ACCESS_KEY",
        "value": "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
      },
      "expectedDenyReason": "AWS credentials must not be observed"
    },

    // CASE 2: System Password File (FileSystem)
    {
      "name": "System /etc/passwd access",
      "category": "filesystem",
      "guardRuleId": "deny_system_files",
      "shouldDeny": true,
      "observation": {
        "domain": "filesystem",
        "type": "file_read",
        "path": "/etc/passwd",
        "content": "root:x:0:0:root:/root:/bin/bash"
      },
      "expectedDenyReason": "System configuration files must not be observed"
    },

    // CASE 3: SSH Keys (FileSystem)
    {
      "name": "SSH private key access",
      "category": "filesystem",
      "guardRuleId": "deny_ssh_keys",
      "shouldDeny": true,
      "observation": {
        "domain": "filesystem",
        "type": "file_read",
        "path": "/home/user/.ssh/id_rsa",
        "content": "-----BEGIN RSA PRIVATE KEY-----\\nMIIEpAIBAAKCAQEA..."
      },
      "expectedDenyReason": "SSH keys must not be observed"
    },

    // CASE 4: Database Credentials (Environment)
    {
      "name": "Database password in environment",
      "category": "environment",
      "guardRuleId": "deny_db_credentials",
      "shouldDeny": true,
      "observation": {
        "domain": "environment",
        "variable": "DATABASE_PASSWORD",
        "value": "super_secret_password_123"
      },
      "expectedDenyReason": "Database credentials must not be observed"
    },

    // CASE 5: Application Secrets (FileSystem)
    {
      "name": "Application secrets.json file",
      "category": "filesystem",
      "guardRuleId": "deny_secrets_files",
      "shouldDeny": true,
      "observation": {
        "domain": "filesystem",
        "type": "file_read",
        "path": "/home/user/unrdf/src/secrets.json",
        "content": "{\"apiKey\": \"secret123\", \"token\": \"...\"}"
      },
      "expectedDenyReason": "Secrets files must not be observed"
    },

    // CASE 6: Internal Network (Network)
    {
      "name": "Connection to internal service",
      "category": "network",
      "guardRuleId": "deny_internal_network",
      "shouldDeny": true,
      "observation": {
        "domain": "network",
        "type": "connection_attempt",
        "destination": "internal.example.com",
        "port": 5432,
        "protocol": "tcp"
      },
      "expectedDenyReason": "Internal network connections must not be observed"
    },

    // CASE 7: Valid Environment Variable (Should allow)
    {
      "name": "Valid NODE_ENV variable",
      "category": "environment",
      "guardRuleId": null,
      "shouldDeny": false,
      "observation": {
        "domain": "environment",
        "variable": "NODE_ENV",
        "value": "production"
      },
      "expectedDenyReason": null
    },

    // CASE 8: Public File (Should allow)
    {
      "name": "Public package.json file",
      "category": "filesystem",
      "guardRuleId": null,
      "shouldDeny": false,
      "observation": {
        "domain": "filesystem",
        "type": "file_read",
        "path": "/home/user/unrdf/package.json",
        "content": "{\"name\": \"unrdf\", ...}"
      },
      "expectedDenyReason": null
    },

    // CASE 9: Public Network (Should allow)
    {
      "name": "Connection to public service",
      "category": "network",
      "guardRuleId": null,
      "shouldDeny": false,
      "observation": {
        "domain": "network",
        "type": "connection_attempt",
        "destination": "api.github.com",
        "port": 443,
        "protocol": "https"
      },
      "expectedDenyReason": null
    }
  ]
}
```

**Usage in Tests:**

```javascript
const guardTestCases = loadFixture("guard-test-cases.json")

for (const testCase of guardTestCases.testCases) {
  const result = applyGuards(testCase.observation, GUARD_RULES)

  if (testCase.shouldDeny) {
    expect(result.status).toBe("DENY")
    expect(result.receipt.denyReason).toBe(testCase.expectedDenyReason)
    expect(result.receipt.guardRuleId).toBe(testCase.guardRuleId)
  } else {
    expect(result.status).toBe("ALLOW")
  }
}
```

---

## Fixture 4: Receipt Chain Data

**File:** `tests/fixtures/receipt-chain.json`

**Purpose:** Pre-computed receipt chains and Merkle proofs for verification testing

**Schema:**

```javascript
{
  "version": "1.0",
  "testData": {
    // Observations in order
    "observations": [
      {
        "id": "obs-001",
        "domain": "runtime",
        "data": "function processData executed",
        "timestamp": "2025-12-27T10:00:00.000Z"
      },
      {
        "id": "obs-002",
        "domain": "filesystem",
        "data": "file /home/user/unrdf/package.json read",
        "timestamp": "2025-12-27T10:00:00.100Z"
      },
      {
        "id": "obs-003",
        "domain": "environment",
        "data": "environment variable NODE_ENV=production",
        "timestamp": "2025-12-27T10:00:00.200Z"
      },
      {
        "id": "obs-004",
        "domain": "network",
        "data": "connection to api.github.com:443",
        "timestamp": "2025-12-27T10:00:00.300Z"
      },
      {
        "id": "obs-005",
        "domain": "metrics",
        "data": "memory usage 52MB heap used",
        "timestamp": "2025-12-27T10:00:00.400Z"
      }
    ],

    // Receipts with chaining
    "receipts": [
      {
        "id": "receipt-001",
        "observationId": "obs-001",
        "observationHash": "abc123...",  // SHA256(obs-001.data)
        "previousReceiptHash": null,
        "timestamp": "2025-12-27T10:00:00.000Z",
        "receiptHash": "receipt_hash_001"
      },
      {
        "id": "receipt-002",
        "observationId": "obs-002",
        "observationHash": "def456...",  // SHA256(obs-002.data)
        "previousReceiptHash": "receipt_hash_001",
        "timestamp": "2025-12-27T10:00:00.100Z",
        "receiptHash": "receipt_hash_002"
      },
      {
        "id": "receipt-003",
        "observationId": "obs-003",
        "observationHash": "ghi789...",  // SHA256(obs-003.data)
        "previousReceiptHash": "receipt_hash_002",
        "timestamp": "2025-12-27T10:00:00.200Z",
        "receiptHash": "receipt_hash_003"
      },
      {
        "id": "receipt-004",
        "observationId": "obs-004",
        "observationHash": "jkl012...",  // SHA256(obs-004.data)
        "previousReceiptHash": "receipt_hash_003",
        "timestamp": "2025-12-27T10:00:00.300Z",
        "receiptHash": "receipt_hash_004"
      },
      {
        "id": "receipt-005",
        "observationId": "obs-005",
        "observationHash": "mno345...",  // SHA256(obs-005.data)
        "previousReceiptHash": "receipt_hash_004",
        "timestamp": "2025-12-27T10:00:00.400Z",
        "receiptHash": "receipt_hash_005"
      }
    ],

    // Merkle tree proof
    "merkleProof": {
      "root": "merkle_root_hash_xyz...",

      // Path for each observation to verify
      "paths": [
        {
          "observationIndex": 0,
          "observationHash": "abc123...",
          "siblings": [
            { "hash": "def456...", "direction": "RIGHT" },
            { "hash": "ghi789jkl012...", "direction": "RIGHT" }
          ],
          "isValid": true
        },
        {
          "observationIndex": 1,
          "observationHash": "def456...",
          "siblings": [
            { "hash": "abc123...", "direction": "LEFT" },
            { "hash": "ghi789jkl012...", "direction": "RIGHT" }
          ],
          "isValid": true
        },
        {
          "observationIndex": 2,
          "observationHash": "ghi789...",
          "siblings": [
            { "hash": "jkl012...", "direction": "RIGHT" },
            { "hash": "merkle_left_subtree...", "direction": "LEFT" }
          ],
          "isValid": true
        },
        {
          "observationIndex": 3,
          "observationHash": "jkl012...",
          "siblings": [
            { "hash": "ghi789...", "direction": "LEFT" },
            { "hash": "merkle_left_subtree...", "direction": "LEFT" }
          ],
          "isValid": true
        },
        {
          "observationIndex": 4,
          "observationHash": "mno345...",
          "siblings": [
            { "hash": "merkle_left_subtree...", "direction": "LEFT" }
          ],
          "isValid": true
        }
      ]
    },

    // Pre-computed hashes
    "checksums": {
      "allReceiptsHash": "complete_hash_of_all_receipts",
      "merkleRootHash": "merkle_root_hash_xyz...",
      "completeDataHash": "hash_of_entire_chain"
    }
  }
}
```

**Usage in Tests:**

```javascript
const chainData = loadFixture("receipt-chain.json")

// Verify all receipt hashes
for (const receipt of chainData.receipts) {
  const isValid = verifyReceiptHash(receipt, chainData.observations)
  expect(isValid).toBe(true)
}

// Verify all Merkle paths
for (const path of chainData.merkleProof.paths) {
  const reconstructed = reconstructMerkleRoot(path)
  expect(reconstructed).toBe(chainData.merkleProof.root)
}

// Verify receipt chaining
for (let i = 1; i < chainData.receipts.length; i++) {
  const current = chainData.receipts[i]
  const previous = chainData.receipts[i-1]
  expect(current.previousReceiptHash).toBe(previous.receiptHash)
}
```

---

## Fixture 5: Real Project Snapshot (E2E)

**Directory:** `tests/fixtures/test-projects/real-project-1/`

**Purpose:** Realistic project structure for end-to-end CLI testing

**Structure:**

```
real-project-1/
├── package.json              # Real package metadata
├── package-lock.json         # Lock file (small stub)
├── .env.test                 # Test environment file
├── .env.production          # Production environment (guarded)
├── .gitignore               # Standard gitignore
├── README.md                # Project documentation
│
├── src/
│   ├── index.js             # Entry point
│   ├── utils.js             # Utility functions
│   ├── config.js            # Configuration (no secrets)
│   ├── secrets.json         # Secrets file (will be guarded)
│   └── modules/
│       ├── auth.js          # Authentication logic
│       ├── database.js      # Database logic
│       └── api.js           # API logic
│
├── config/
│   ├── database.js          # Database config (some secrets)
│   ├── logger.js            # Logger config
│   └── environment.js       # Env handling
│
├── tests/
│   ├── unit.test.js         # Unit tests
│   └── integration.test.js  # Integration tests
│
├── dist/                    # Compiled output (after build)
│   └── index.js
│
├── .github/
│   └── workflows/
│       ├── test.yml         # Test workflow
│       └── build.yml        # Build workflow
│
└── node_modules/            # Symlink or stub (not real packages)
    └── .bin/
        └── npm              # Stub executable
```

**Key Files:**

```javascript
// package.json
{
  "name": "test-app",
  "version": "1.0.0",
  "description": "Test application for probe",
  "main": "dist/index.js",
  "scripts": {
    "start": "node src/index.js",
    "test": "jest",
    "build": "esbuild src/index.js --bundle --outfile=dist/index.js"
  },
  "dependencies": {
    "express": "4.18.2",
    "dotenv": "16.0.3"
  },
  "devDependencies": {
    "jest": "29.0.0",
    "esbuild": "0.15.0"
  }
}
```

```javascript
// src/index.js
const express = require('express')
const config = require('./config')

const app = express()

app.get('/health', (req, res) => {
  res.json({ status: 'ok' })
})

module.exports = app
```

```bash
# .env.test
NODE_ENV=test
DEBUG=false
LOG_LEVEL=info
PORT=3000
```

**Usage in Tests:**

```javascript
const testProjectPath = "./tests/fixtures/test-projects/real-project-1"

// Run CLI commands against fixture project
const result = execSync(`kgc probe scan --project ${testProjectPath}`)

// Verify all expected files were scanned
expect(result.stdout).toContain("package.json")
expect(result.stdout).toContain("src/index.js")

// Verify secrets file was found and guarded
expect(result.stdout).toContain("secrets.json")
expect(result.stdout).toMatch(/DENY.*secrets\.json/)
```

---

## Fixture 6: E2E Baseline

**File:** `tests/fixtures/baselines/e2e-baseline.json`

**Purpose:** Expected results for E2E test validation

**Schema:**

```javascript
{
  "version": "1.0",
  "description": "Baseline expectations for E2E workflow",

  // Expected artifact counts
  "expectedCounts": {
    "observations": 65,           // Minimum expected
    "receipts": 68,               // Allow for deny receipts
    "denyReceipts": 8,            // Guard denies
    "domains": 10,                // All domains covered
    "agentShards": 10             // One per domain
  },

  // Expected domain representation
  "expectedDomains": [
    "runtime",
    "filesystem",
    "wasm",
    "environment",
    "network",
    "custom_1",
    "custom_2",
    "custom_3",
    "logging",
    "metrics"
  ],

  // Expected domain observation minimums
  "domainMinimums": {
    "runtime": 5,
    "filesystem": 10,
    "wasm": 3,
    "environment": 15,
    "network": 5,
    "custom_1": 2,
    "custom_2": 2,
    "custom_3": 2,
    "logging": 10,
    "metrics": 10
  },

  // Expected Merkle root (pre-computed)
  "expectedMerkleRoot": "baseline_merkle_root_hash_here",

  // Performance targets
  "performanceTargets": {
    "scanDuration": {
      "target": 30000,  // 30 seconds
      "max": 35000      // With margin
    },
    "mergeDuration": {
      "target": 5000,   // 5 seconds
      "max": 7000
    },
    "verifyDuration": {
      "target": 10000,  // 10 seconds
      "max": 15000
    },
    "totalWorkflow": {
      "target": 45000,  // 45 seconds total
      "max": 60000
    }
  },

  // Expected guard rules enforced
  "expectedGuardRules": [
    "deny_aws_secrets",
    "deny_system_files",
    "deny_ssh_keys",
    "deny_db_credentials",
    "deny_secrets_files",
    "deny_internal_network",
    "deny_private_keys"
  ],

  // Forbidden patterns should NOT appear
  "forbiddenPatterns": [
    "AKIA[0-9A-Z]{16}",           // AWS access key
    "-----BEGIN RSA PRIVATE KEY-----",  // SSH key
    "/etc/passwd",
    "DATABASE_PASSWORD",
    "API_KEY"
  ],

  // Validation checksums
  "checksums": {
    "observationsHash": "baseline_observations_hash",
    "receiptsHash": "baseline_receipts_hash",
    "dataIntegrity": "baseline_complete_hash"
  }
}
```

**Usage in Tests:**

```javascript
const baseline = loadFixture("baselines/e2e-baseline.json")

// Verify observation count
expect(result.observations.length).toBeGreaterThanOrEqual(baseline.expectedCounts.observations)

// Verify all domains present
const foundDomains = new Set(result.observations.map(o => o.domain))
for (const domain of baseline.expectedDomains) {
  expect(foundDomains).toContain(domain)
}

// Verify Merkle root matches (if deterministic run)
expect(result.merkleProof.root).toBe(baseline.expectedMerkleRoot)

// Verify performance within SLA
expect(result.metrics.scanDuration).toBeLessThan(baseline.performanceTargets.scanDuration.max)
```

---

## Fixture Helpers

### fixture-loader.mjs

```javascript
/**
 * Load fixtures with validation
 * @param {string} fixtureName - Name of fixture file
 * @returns {object} Parsed fixture data
 */
export function loadFixture(fixtureName) {
  const path = `./tests/fixtures/${fixtureName}`
  const content = readFileSync(path, 'utf-8')
  return JSON.parse(content)
}

export function loadAllShards() {
  const domains = [
    "runtime", "filesystem", "wasm", "environment",
    "network", "custom_1", "custom_2", "custom_3",
    "logging", "metrics"
  ]

  const shards = {}
  for (const domain of domains) {
    shards[domain] = loadFixture(`shards/${domain}_shard_1.json`)
  }
  return shards
}
```

### environment-freezer.mjs

```javascript
/**
 * Setup frozen environment for determinism testing
 */
export function setupFrozenEnvironment(frozenEnv) {
  // Create temp directory
  const tmpDir = mkdtempSync(path.join(tmpdir(), 'test-'))

  // Write all files
  for (const file of frozenEnv.files) {
    const filePath = path.join(tmpDir, file.path)
    mkdirSync(path.dirname(filePath), { recursive: true })
    writeFileSync(filePath, file.content)
  }

  // Set environment variables
  for (const [key, value] of Object.entries(frozenEnv.environment)) {
    process.env[key] = value
  }

  // Freeze time (if available)
  // Use "useFakeTimers" in Jest or similar

  return tmpDir
}

export function cleanupFrozenEnvironment(projectPath) {
  // Remove temp directory
  rmSync(projectPath, { recursive: true, force: true })

  // Restore environment
  // Reset time
}
```

---

## Validation Checklist

Before committing fixtures:

- [ ] All fixture files are valid JSON
- [ ] All observations have required fields (id, domain, type, timestamp)
- [ ] All receipts have observation hash precomputed
- [ ] Merkle proofs are mathematically valid
- [ ] Guard test cases cover all rule types
- [ ] Test project structure is realistic
- [ ] Baseline expectations match actual capability
- [ ] No real secrets in any fixture
- [ ] All fixture hashes computed correctly
- [ ] Fixture documentation complete

---

## Maintenance

**When to update fixtures:**

1. **After schema changes**: Recompute all fixture hashes
2. **After guard rule changes**: Update guard test cases
3. **After performance improvements**: Update baseline SLAs
4. **Quarterly review**: Verify fixtures still valid and realistic

**Updating process:**

```bash
# Regenerate from live system
npm run generate:fixtures

# Validate all fixtures
npm run validate:fixtures

# Test suite verification
npm test -- --fixtures
```

---

## References

- Test Strategy: `/home/user/unrdf/docs/test-validation-strategy.md`
- OTEL Validation: `/home/user/unrdf/docs/otel-validation-harness.md`
- Fixture tests: `/home/user/unrdf/tests/fixtures.test.js`
