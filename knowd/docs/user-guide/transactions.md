# Transactions

Knowd provides ACID-compliant transactions for managing RDF data changes. This guide covers transaction handling, receipt generation, and cryptographic proofs.

## Transaction Overview

Transactions in Knowd are **atomic operations** that ensure all changes succeed or fail together. Each transaction generates a **cryptographically signed receipt** that provides proof of all changes made.

## Transaction Lifecycle

```
1. Validation (optional) → 2. Execution → 3. Hooks → 4. Receipt → 5. Response
```

### 1. Validation Phase
- **Input validation** - Verify request format and permissions
- **SHACL validation** - Validate data against shapes (optional)
- **Pre-transaction hooks** - Run validation hooks before changes

### 2. Execution Phase
- **Lock acquisition** - Acquire necessary locks for consistency
- **Change application** - Apply additions and removals atomically
- **WAL logging** - Write to Write-Ahead Log for durability

### 3. Hooks Phase
- **Event processing** - Trigger registered hooks based on changes
- **Asynchronous execution** - Hooks run independently of transaction

### 4. Receipt Generation
- **Canonicalization** - Convert changes to URDNA2015 canonical form
- **Merkle tree** - Generate SHA3-256 merkle root for integrity
- **Digital signature** - Sign receipt with Ed25519 private key

### 5. Response
- **Return receipt** - Provide receipt ID and results to client
- **Confirmation** - Confirm successful transaction completion

## Transaction API

### Submit Transaction

**Endpoint:** `POST /v1/tx`

**Request Body:**
```json
{
  "delta": {
    "add": [
      {
        "subject": "<http://example.org/person1>",
        "predicate": "<http://example.org/name>",
        "object": "\"Alice\"",
        "graph": "default"
      }
    ],
    "remove": [
      {
        "subject": "<http://example.org/person2>",
        "predicate": "<http://example.org/age>",
        "object": "\"25\"",
        "graph": "default"
      }
    ]
  },
  "actor": "user@example.com",
  "metadata": {
    "reason": "User registration",
    "ip": "192.168.1.1"
  }
}
```

**Response:**
```json
{
  "receiptId": "tx-uuid-12345",
  "added": 1,
  "removed": 1,
  "timestamp": "2025-01-01T12:00:00Z",
  "merkleRoot": "abc123...",
  "signature": "base64-signature..."
}
```

### Transaction Parameters

**delta** (required) - The changes to apply:
- **add** - Array of quads to add
- **remove** - Array of quads to remove

**actor** (required) - Identifier of who performed the transaction

**metadata** (optional) - Additional context information

## Receipt Verification

Every transaction generates a receipt with cryptographic proof of the changes made.

### Retrieve Receipt

**Endpoint:** `GET /v1/receipts/{receiptId}`

**Response:**
```json
{
  "id": "tx-uuid-12345",
  "version": "1.0",
  "actor": "user@example.com",
  "timestamp": "2025-01-01T12:00:00Z",
  "delta": {
    "add": [...],
    "remove": [...]
  },
  "canonical": "Canonical N-Quads format...",
  "merkleRoot": "sha3-256-hash",
  "signature": "base64-ed25519-signature",
  "pubKey": "base64-public-key",
  "git": {
    "commit": "abc123...",
    "repo": "https://github.com/unrdf/knowd"
  }
}
```

### Verify Receipt

**Endpoint:** `GET /v1/receipts/{receiptId}/verify`

**Response:**
```json
{
  "ok": true,
  "signatureVerified": true,
  "merkleRootVerified": true,
  "canonicalForm": "Verified canonical form...",
  "verifiedAt": "2025-01-01T12:05:00Z"
}
```

**Verification Checks:**
- ✅ **Digital signature** - Ed25519 signature verification
- ✅ **Data integrity** - SHA3-256 Merkle root verification
- ✅ **Canonicalization** - URDNA2015 canonical form validation
- ✅ **Temporal validity** - Timestamp and sequence validation

## Lockchain (Receipt Chain)

Knowd implements a **lockchain** - a chain of cryptographically linked receipts that provides an immutable audit trail.

### Chain Properties

- **Immutable** - Receipts cannot be modified after creation
- **Verifiable** - Each receipt can be independently verified
- **Traceable** - Complete chain of custody for all changes
- **Tamper-evident** - Any modification breaks cryptographic chain

### Chain Verification

```bash
# Verify entire chain integrity
curl http://localhost:8090/v1/receipts/verify-chain

# Response shows chain health
{
  "chainLength": 15420,
  "totalTransactions": 15420,
  "verifiedReceipts": 15420,
  "brokenLinks": 0,
  "earliestReceipt": "2025-01-01T00:00:00Z",
  "latestReceipt": "2025-01-01T12:00:00Z",
  "chainStatus": "healthy"
}
```

## Transaction Examples

### Basic Data Addition

```bash
# Add person data
curl -X POST http://localhost:8090/v1/tx \
  -H "Content-Type: application/json" \
  -d '{
    "delta": {
      "add": [
        {
          "subject": "<http://example.org/alice>",
          "predicate": "<http://xmlns.com/foaf/0.1/name>",
          "object": "\"Alice Smith\"",
          "graph": "default"
        },
        {
          "subject": "<http://example.org/alice>",
          "predicate": "<http://xmlns.com/foaf/0.1/knows>",
          "object": "<http://example.org/bob>",
          "graph": "default"
        }
      ]
    },
    "actor": "admin@example.com"
  }'
```

### Data Update Transaction

```bash
# Update age (remove old, add new)
curl -X POST http://localhost:8090/v1/tx \
  -H "Content-Type: application/json" \
  -d '{
    "delta": {
      "remove": [
        {
          "subject": "<(http://example.org/bob",
          "predicate": "<http://xmlns.com/foaf/0.1/age>",
          "object": "\"24\"",
          "graph": "default"
        }
      ],
      "add": [
        {
          "subject": "<http://example.org/bob>",
          "predicate": "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
          "object": "\"25\"",
          "graph": "default"
        }
      ]
    },
    "actor": "user@example.com",
    "metadata": {
      "reason": "Birthday update"
    }
  }'
```

### Bulk Data Import

```bash
# Import large dataset
curl -X POST http://localhost:8090/v1/tx \
  -H "Content-Type: application/json" \
  -d '{
    "delta": {
      "add": [
        // Large array of quads...
      ]
    },
    "actor": "importer@example.com",
    "metadata": {
      "source": "legacy-database-export",
      "batch_id": "batch-001"
    }
  }'
```

## Transaction Error Handling

### Validation Errors

**Error Response:**
```json
{
  "error": "ValidationError",
  "message": "Invalid quad format in delta.add[5]",
  "code": 400,
  "details": {
    "field": "delta.add.5.object",
    "issue": "Missing angle brackets for IRI"
  }
}
```

### Constraint Violations

**SHACL Validation Error:**
```json
{
  "error": "ConstraintViolation",
  "message": "SHACL validation failed",
  "code": 400,
  "details": {
    "violations": [
      {
        "focusNode": "<http://example.org/person>",
        "property": "<http://example.org/name>",
        "constraint": "sh:minCount",
        "message": "Required name property missing"
      }
    ]
  }
}
```

### Concurrency Errors

**Conflict Error:**
```json
{
  "error": "TransactionConflict",
  "message": "Transaction conflicts with concurrent modification",
  "code": 409,
  "details": {
    "conflictingTx": "tx-uuid-54321",
    "retryAfter": 60
  }
}
```

## Transaction Best Practices

### 1. Batching Changes

**✅ Good:** Batch related changes in single transaction
```json
{
  "delta": {
    "add": [
      { "subject": "<http://example.org/person>", "predicate": "<http://example.org/name>", "object": "\"Alice\"" },
      { "subject": "<http://example.org/person>", "predicate": "<http://example.org/age>", "object": "\"25\"" },
      { "subject": "<http://example.org/person>", "predicate": "<http://example.org/email>", "object": "\"alice@example.com\"" }
    ]
  }
}
```

**❌ Avoid:** Multiple small transactions
```bash
# Don't do this - creates unnecessary overhead
curl -X POST /v1/tx -d '{"delta": {"add": [{"s": "person", "p": "name", "o": "Alice"}]}}'
curl -X POST /v1/tx -d '{"delta": {"add": [{"s": "person", "p": "age", "o": "25"}]}}'
```

### 2. Meaningful Metadata

**✅ Include:** Relevant context
```json
{
  "actor": "user@example.com",
  "metadata": {
    "reason": "Profile update",
    "ip": "192.168.1.1",
    "userAgent": "MobileApp/1.0"
  }
}
```

### 3. Error Handling

**✅ Implement:** Proper retry logic
```javascript
async function submitTransaction(delta, maxRetries = 3) {
  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      const response = await fetch('/v1/tx', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(delta)
      });
      
      if (response.ok) {
        return await response.json();
      }
      
      const error = await response.json();
      if (error.code === 409) {
        // Transaction conflict - retry with backoff
        await new Promise(resolve => setTimeout(resolve, 1000 * attempt));
        continue;
      }
      
      throw new Error(error.message);
    } catch (error) {
      if (attempt === maxRetries) throw error;
      await new Promise(resolve => setTimeout(resolve, 1000 * attempt));
    }
  }
}
```

### 4. Receipt Storage

**✅ Store:** Receipt IDs for audit trails
```javascript
// Always store receipt ID for future verification
const receipt = await submitTransaction({
  delta: { add: [...] },
  actor: "user@example.com"
});

console.log(`Transaction completed: ${receipt.receiptId}`);
localStorage.setItem('lastTransaction', receipt.receiptId);
```

## Monitoring Transactions

### Transaction Metrics

```bash
# Get transaction statistics
curl http://localhost:8090/v1/stats/transactions

# Response includes performance metrics
{
  "totalTransactions": 15420,
  "averageTransactionSize": 5.2,
  "averageLatencyMs": 23,
  "successRate": 0.998,
  "errorRate": 0.002,
  "lastHour": {
    "count": 342,
    "avgSize": 4.8,
    "avgLatency": 19
  }
}
```

### Performance Monitoring

- **Transaction throughput** - Transactions per second
- **Average latency** - Time from request to receipt
- **Error rates** - Failed transaction percentage
- **Queue depth** - Pending transactions

This comprehensive transaction system ensures data integrity while providing the audit trails needed for compliance and debugging.
