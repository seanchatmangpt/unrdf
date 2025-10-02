# Knowledge Hooks API Guide

## Table of Contents
- [Introduction](#introduction)
- [Getting Started](#getting-started)
- [Authentication](#authentication)
- [Core Concepts](#core-concepts)
- [API Endpoints](#api-endpoints)
- [Common Use Cases](#common-use-cases)
- [Best Practices](#best-practices)
- [Rate Limits](#rate-limits)
- [Error Handling](#error-handling)
- [Code Examples](#code-examples)

## Introduction

The Knowledge Hooks API enables reactive semantic reasoning on RDF graphs. Knowledge Hooks are predicates that trigger actions based on:
- SPARQL query results (ASK, SELECT)
- SHACL shape validation
- Threshold conditions
- Change detection (deltas)
- Windowed aggregations

### Key Features

- **Multi-Phase Execution**: Pre-operation, post-operation, and invariant validation
- **Cryptographic Provenance**: SHA-256 integrity verification for queries
- **Deterministic Execution**: Seeded randomness for reproducible results
- **Receipt Anchoring**: Blockchain/Merkle tree integration for audit trails
- **Circuit Breaker Protection**: Automatic failover for high availability
- **OpenTelemetry Integration**: Distributed tracing and observability

## Getting Started

### Base URL

```
Development: http://localhost:3000
Production:  https://api.unrdf.example.com
```

### Quick Start Example

```javascript
// 1. Register a hook
const response = await fetch('http://localhost:3000/api/hooks/register', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Authorization': 'Bearer YOUR_JWT_TOKEN'
  },
  body: JSON.stringify({
    id: 'validate-budget-constraints',
    select: `
      SELECT ?allocation ?amount ?limit WHERE {
        ?allocation rdf:type :BudgetAllocation ;
                    :amount ?amount ;
                    :category ?category .
        ?category :limit ?limit .
      }
    `,
    predicates: [
      {
        kind: 'ASK',
        query: `
          ASK WHERE {
            ?allocation :amount ?amount .
            ?allocation :category ?category .
            ?category :limit ?limit .
            FILTER(?amount <= ?limit)
          }
        `
      }
    ],
    combine: 'AND',
    phase: 'pre'
  })
})

const { data } = await response.json()
console.log(`Hook registered: ${data.hookId}`)

// 2. Evaluate the hook
const evalResponse = await fetch('http://localhost:3000/api/hooks/evaluate', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Authorization': 'Bearer YOUR_JWT_TOKEN'
  },
  body: JSON.stringify({
    hookId: 'validate-budget-constraints',
    data: `
      @prefix : <https://example.org/> .
      @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

      :allocation1 a :BudgetAllocation ;
        :amount 8500 ;
        :category :marketing .

      :marketing :limit 10000 .
    `
  })
})

const evalResult = await evalResponse.json()
console.log(`Hook fired: ${evalResult.data.fired}`)
```

## Authentication

All API endpoints require JWT Bearer authentication.

### Obtaining a Token

```bash
# Request JWT token (implementation-specific)
curl -X POST http://localhost:3000/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username": "your-username", "password": "your-password"}'
```

### Using the Token

Include the JWT token in the `Authorization` header:

```bash
curl -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..." \
  http://localhost:3000/api/hooks/list
```

### Token Expiration

- Tokens expire after 24 hours
- Refresh tokens before expiration to avoid interruptions
- 401 Unauthorized responses indicate expired or invalid tokens

## Core Concepts

### Hook Phases

Knowledge Hooks execute in three phases:

1. **Pre-operation (`pre`)**: Execute before a graph operation
   - Validation before write
   - Authorization checks
   - Constraint enforcement

2. **Post-operation (`post`)**: Execute after a graph operation
   - Derived data computation
   - Notification triggers
   - Audit logging

3. **Invariant (`invariant`)**: Continuous validation
   - Graph integrity checks
   - Consistency monitoring
   - Real-time alerts

### Predicate Types

#### 1. ASK Predicate

SPARQL ASK query that returns boolean (true/false).

```json
{
  "kind": "ASK",
  "query": "ASK WHERE { ?s ?p ?o . FILTER(?o > 100) }"
}
```

**Use Cases:**
- Existence checks
- Boolean conditions
- Simple validations

#### 2. SHACL Predicate

Validates RDF data against SHACL shapes.

```json
{
  "kind": "SHACL",
  "shapes": "@prefix sh: <http://www.w3.org/ns/shacl#> .\n:ProjectShape a sh:NodeShape ."
}
```

**Use Cases:**
- Schema validation
- Data quality checks
- Structural constraints

#### 3. THRESHOLD Predicate

Compares a numeric variable against a threshold.

```json
{
  "kind": "THRESHOLD",
  "variable": "totalSpending",
  "operator": ">",
  "value": 10000
}
```

**Operators:** `>`, `<`, `>=`, `<=`, `==`, `!=`

**Use Cases:**
- Budget limits
- Performance thresholds
- Alerting conditions

#### 4. DELTA Predicate

Detects changes between graph states.

```json
{
  "kind": "DELTA"
}
```

**Use Cases:**
- Change detection
- Audit trails
- Incremental processing

#### 5. COUNT Predicate

Counts query results and compares to threshold.

```json
{
  "kind": "COUNT",
  "countVariable": "numProjects"
}
```

**Use Cases:**
- Result set size validation
- Cardinality constraints
- Quota enforcement

#### 6. WINDOW Predicate

Aggregates data over time windows.

```json
{
  "kind": "WINDOW",
  "windowSize": 3600
}
```

**Use Cases:**
- Time-series analysis
- Moving averages
- Rate limiting

### Combining Predicates

Multiple predicates can be combined with `AND` or `OR`:

- **AND**: All predicates must pass (strict validation)
- **OR**: At least one predicate must pass (flexible validation)

```json
{
  "predicates": [
    { "kind": "ASK", "query": "..." },
    { "kind": "THRESHOLD", "variable": "amount", "operator": "<", "value": 1000 }
  ],
  "combine": "AND"
}
```

## API Endpoints

### 1. List Hooks

**GET** `/api/hooks/list`

Retrieve all registered hooks with pagination and filtering.

**Query Parameters:**
- `limit` (integer, 1-100): Maximum hooks to return (default: 20)
- `offset` (integer): Pagination offset (default: 0)
- `phase` (string): Filter by phase (`pre`, `post`, `invariant`)
- `disabled` (boolean): Filter by disabled status
- `tags` (string): Comma-separated tags

**Example:**

```bash
curl -H "Authorization: Bearer TOKEN" \
  "http://localhost:3000/api/hooks/list?limit=10&phase=pre"
```

**Response:**

```json
{
  "success": true,
  "data": {
    "hooks": [
      {
        "id": "validate-budget-constraints",
        "name": "Budget Constraint Validator",
        "description": "Ensures budget allocations don't exceed limits",
        "phase": "pre",
        "predicateCount": 2,
        "ontology": ["https://example.org/ontology/budget"],
        "channel": { "kind": "stdio" },
        "createdAt": "2025-10-01T12:00:00.000Z"
      }
    ],
    "total": 1
  }
}
```

### 2. Register Hook

**POST** `/api/hooks/register`

Create and register a new knowledge hook.

**Request Body:**

```json
{
  "id": "validate-budget-constraints",
  "select": "SELECT ?allocation ?amount WHERE { ... }",
  "predicates": [
    {
      "kind": "ASK",
      "query": "ASK WHERE { ... }"
    }
  ],
  "combine": "AND",
  "phase": "pre"
}
```

**Response (201 Created):**

```json
{
  "success": true,
  "data": {
    "hookId": "validate-budget-constraints",
    "phase": "pre",
    "predicateCount": 1
  }
}
```

### 3. Get Hook by ID

**GET** `/api/hooks/{id}`

Retrieve detailed information about a specific hook.

**Example:**

```bash
curl -H "Authorization: Bearer TOKEN" \
  http://localhost:3000/api/hooks/validate-budget-constraints
```

**Response:**

```json
{
  "success": true,
  "data": {
    "id": "validate-budget-constraints",
    "meta": {
      "name": "Budget Constraint Validator",
      "description": "Ensures budget allocations don't exceed limits"
    },
    "phase": "pre",
    "predicates": [...],
    "source": "defineHook({ ... })",
    "createdAt": "2025-10-01T12:00:00.000Z"
  }
}
```

### 4. Update Hook

**PUT** `/api/hooks/{id}`

Update an existing hook's predicates or configuration.

**Request Body:** Same as register hook

**Response:**

```json
{
  "success": true,
  "data": {
    "hookId": "validate-budget-constraints",
    "phase": "pre",
    "predicateCount": 2,
    "updated": true,
    "timestamp": "2025-10-01T12:30:00.000Z"
  }
}
```

### 5. Delete Hook

**DELETE** `/api/hooks/{id}`

Permanently delete a knowledge hook.

**Example:**

```bash
curl -X DELETE \
  -H "Authorization: Bearer TOKEN" \
  http://localhost:3000/api/hooks/validate-budget-constraints
```

**Response:**

```json
{
  "success": true,
  "data": {
    "hookId": "validate-budget-constraints",
    "deleted": true,
    "timestamp": "2025-10-01T13:00:00.000Z"
  }
}
```

### 6. Evaluate Hook

**POST** `/api/hooks/evaluate`

Execute a knowledge hook against RDF data.

**Request Body:**

```json
{
  "hookId": "validate-budget-constraints",
  "data": "@prefix : <https://example.org/> .\n:allocation1 :amount 8500 .",
  "context": {
    "traceId": "a7ffc6f8bf1ed766",
    "metadata": {
      "userId": "user-123",
      "operation": "budget-update"
    }
  }
}
```

**Response:**

```json
{
  "success": true,
  "data": {
    "hookId": "validate-budget-constraints",
    "fired": true,
    "timestamp": "2025-10-01T12:00:00.000Z",
    "predicates": [
      {
        "kind": "ASK",
        "passed": true,
        "bindings": []
      }
    ],
    "duration": {
      "queryMs": 12.5,
      "evaluationMs": 8.3,
      "totalMs": 20.8
    }
  }
}
```

## Common Use Cases

### 1. Budget Validation

Prevent budget allocations from exceeding category limits.

```javascript
await fetch('/api/hooks/register', {
  method: 'POST',
  headers: { 'Authorization': `Bearer ${token}` },
  body: JSON.stringify({
    id: 'budget-validator',
    select: `
      SELECT ?allocation ?amount ?limit WHERE {
        ?allocation :amount ?amount ; :category ?cat .
        ?cat :limit ?limit .
      }
    `,
    predicates: [
      {
        kind: 'THRESHOLD',
        variable: 'amount',
        operator: '<=',
        value: 'limit' // Reference to another variable
      }
    ],
    combine: 'AND',
    phase: 'pre'
  })
})
```

### 2. Data Quality Validation

Ensure all projects have required fields using SHACL.

```javascript
await fetch('/api/hooks/register', {
  method: 'POST',
  headers: { 'Authorization': `Bearer ${token}` },
  body: JSON.stringify({
    id: 'project-quality-check',
    predicates: [
      {
        kind: 'SHACL',
        shapes: `
          @prefix sh: <http://www.w3.org/ns/shacl#> .
          @prefix : <https://example.org/> .

          :ProjectShape a sh:NodeShape ;
            sh:targetClass :Project ;
            sh:property [
              sh:path :name ;
              sh:minCount 1 ;
              sh:datatype xsd:string
            ] ;
            sh:property [
              sh:path :budget ;
              sh:minCount 1 ;
              sh:datatype xsd:decimal
            ] .
        `
      }
    ],
    combine: 'AND',
    phase: 'post'
  })
})
```

### 3. High-Value Transaction Alerts

Trigger notifications when spending exceeds threshold.

```javascript
await fetch('/api/hooks/register', {
  method: 'POST',
  headers: { 'Authorization': `Bearer ${token}` },
  body: JSON.stringify({
    id: 'high-value-alert',
    select: `
      SELECT (SUM(?amount) AS ?totalSpending) WHERE {
        ?txn a :Transaction ; :amount ?amount .
      }
    `,
    predicates: [
      {
        kind: 'THRESHOLD',
        variable: 'totalSpending',
        operator: '>',
        value: 10000
      }
    ],
    combine: 'AND',
    phase: 'post'
  })
})
```

### 4. Change Detection

Monitor graph changes and trigger downstream processing.

```javascript
await fetch('/api/hooks/register', {
  method: 'POST',
  headers: { 'Authorization': `Bearer ${token}` },
  body: JSON.stringify({
    id: 'change-detector',
    predicates: [
      {
        kind: 'DELTA'
      }
    ],
    combine: 'AND',
    phase: 'post'
  })
})
```

## Best Practices

### 1. Use Meaningful Hook IDs

Choose descriptive, lowercase-kebab-case IDs:

**Good:**
- `validate-budget-constraints`
- `ensure-project-has-owner`
- `alert-high-spending`

**Bad:**
- `hook1`
- `MyHook`
- `validateBudget`

### 2. Optimize SPARQL Queries

- Use specific triple patterns
- Avoid `SELECT *`
- Add FILTERs early in the query
- Use LIMIT for large result sets

**Optimized:**
```sparql
SELECT ?allocation ?amount WHERE {
  ?allocation a :BudgetAllocation ;
              :amount ?amount .
  FILTER(?amount > 1000)
}
LIMIT 100
```

### 3. Combine Predicates Wisely

- Use `AND` for strict validation (all must pass)
- Use `OR` for flexible validation (any can pass)
- Group related predicates together

### 4. Set Appropriate Phases

- **Pre-operation**: Validation, authorization
- **Post-operation**: Notifications, derived data
- **Invariant**: Continuous monitoring

### 5. Handle Errors Gracefully

Always check response status and handle errors:

```javascript
const response = await fetch('/api/hooks/evaluate', {
  method: 'POST',
  headers: { 'Authorization': `Bearer ${token}` },
  body: JSON.stringify({ hookId: 'my-hook', data: rdfData })
})

if (!response.ok) {
  const error = await response.json()
  console.error(`Error: ${error.error.message}`)
  // Handle specific error codes
  if (error.error.code === 'SERVICE_UNAVAILABLE') {
    // Retry with exponential backoff
  }
}
```

### 6. Use Trace Context

Include trace IDs for distributed tracing:

```javascript
await fetch('/api/hooks/evaluate', {
  method: 'POST',
  body: JSON.stringify({
    hookId: 'my-hook',
    data: rdfData,
    context: {
      traceId: generateTraceId(),
      metadata: {
        userId: currentUser.id,
        operation: 'budget-update'
      }
    }
  })
})
```

### 7. Verify Query Integrity

Use SHA-256 hashes to ensure query integrity:

```javascript
import crypto from 'crypto'

const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }'
const hash = crypto.createHash('sha256').update(query).digest('hex')

await fetch('/api/hooks/register', {
  method: 'POST',
  body: JSON.stringify({
    id: 'my-hook',
    selectQuery: query,
    selectQuerySha256: hash,
    // ...
  })
})
```

## Rate Limits

The API enforces the following rate limits:

- **100 requests per minute** per API key
- **1000 hook evaluations per hour**

### Rate Limit Headers

Responses include rate limit information:

```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 87
X-RateLimit-Reset: 1696176000
```

### Handling Rate Limits

When rate limited, you'll receive a `429 Too Many Requests` response:

```json
{
  "success": false,
  "error": {
    "code": "RATE_LIMIT_EXCEEDED",
    "message": "Rate limit exceeded. Retry after 60 seconds.",
    "statusCode": 429,
    "retryAfter": 60
  }
}
```

**Best Practice:** Implement exponential backoff:

```javascript
async function evaluateWithRetry(hookId, data, maxRetries = 3) {
  for (let i = 0; i < maxRetries; i++) {
    const response = await fetch('/api/hooks/evaluate', {
      method: 'POST',
      body: JSON.stringify({ hookId, data })
    })

    if (response.status !== 429) {
      return response.json()
    }

    const retryAfter = response.headers.get('Retry-After') || Math.pow(2, i)
    await new Promise(resolve => setTimeout(resolve, retryAfter * 1000))
  }

  throw new Error('Max retries exceeded')
}
```

## Error Handling

### Standard Error Response

All errors follow this structure:

```json
{
  "success": false,
  "error": {
    "code": "ERROR_CODE",
    "message": "Human-readable error message",
    "statusCode": 400,
    "details": {},
    "issues": []
  }
}
```

### Common Error Codes

| Code | Status | Description |
|------|--------|-------------|
| `VALIDATION_ERROR` | 400 | Request validation failed |
| `NOT_FOUND` | 404 | Hook not found |
| `UNAUTHORIZED` | 401 | Authentication required |
| `CONFLICT` | 409 | Hook ID already exists |
| `RATE_LIMIT_EXCEEDED` | 429 | Too many requests |
| `SERVICE_UNAVAILABLE` | 503 | Circuit breaker open |
| `INTERNAL_SERVER_ERROR` | 500 | Unexpected server error |

### Validation Errors

Validation errors include detailed issues:

```json
{
  "success": false,
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Request validation failed",
    "statusCode": 400,
    "issues": [
      {
        "path": "predicates.0.query",
        "message": "SPARQL query cannot be empty",
        "code": "too_small"
      }
    ]
  }
}
```

## Code Examples

### JavaScript/Node.js

```javascript
import fetch from 'node-fetch'

const API_BASE = 'http://localhost:3000'
const TOKEN = process.env.API_TOKEN

// Register a hook
async function registerHook() {
  const response = await fetch(`${API_BASE}/api/hooks/register`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${TOKEN}`
    },
    body: JSON.stringify({
      id: 'validate-budget',
      select: `
        SELECT ?allocation ?amount WHERE {
          ?allocation :amount ?amount .
        }
      `,
      predicates: [
        {
          kind: 'THRESHOLD',
          variable: 'amount',
          operator: '<=',
          value: 10000
        }
      ],
      combine: 'AND',
      phase: 'pre'
    })
  })

  return response.json()
}

// Evaluate a hook
async function evaluateHook(hookId, rdfData) {
  const response = await fetch(`${API_BASE}/api/hooks/evaluate`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${TOKEN}`
    },
    body: JSON.stringify({
      hookId,
      data: rdfData
    })
  })

  return response.json()
}

// Usage
const result = await registerHook()
console.log(`Hook registered: ${result.data.hookId}`)

const evaluation = await evaluateHook('validate-budget', `
  @prefix : <https://example.org/> .
  :allocation1 :amount 8500 .
`)
console.log(`Hook fired: ${evaluation.data.fired}`)
```

### Python

```python
import requests
import os

API_BASE = 'http://localhost:3000'
TOKEN = os.getenv('API_TOKEN')

headers = {
    'Content-Type': 'application/json',
    'Authorization': f'Bearer {TOKEN}'
}

# Register a hook
def register_hook():
    response = requests.post(
        f'{API_BASE}/api/hooks/register',
        headers=headers,
        json={
            'id': 'validate-budget',
            'select': '''
                SELECT ?allocation ?amount WHERE {
                    ?allocation :amount ?amount .
                }
            ''',
            'predicates': [
                {
                    'kind': 'THRESHOLD',
                    'variable': 'amount',
                    'operator': '<=',
                    'value': 10000
                }
            ],
            'combine': 'AND',
            'phase': 'pre'
        }
    )
    return response.json()

# Evaluate a hook
def evaluate_hook(hook_id, rdf_data):
    response = requests.post(
        f'{API_BASE}/api/hooks/evaluate',
        headers=headers,
        json={
            'hookId': hook_id,
            'data': rdf_data
        }
    )
    return response.json()

# Usage
result = register_hook()
print(f"Hook registered: {result['data']['hookId']}")

evaluation = evaluate_hook('validate-budget', '''
    @prefix : <https://example.org/> .
    :allocation1 :amount 8500 .
''')
print(f"Hook fired: {evaluation['data']['fired']}")
```

### cURL

```bash
# Register a hook
curl -X POST http://localhost:3000/api/hooks/register \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $API_TOKEN" \
  -d '{
    "id": "validate-budget",
    "select": "SELECT ?allocation ?amount WHERE { ?allocation :amount ?amount . }",
    "predicates": [
      {
        "kind": "THRESHOLD",
        "variable": "amount",
        "operator": "<=",
        "value": 10000
      }
    ],
    "combine": "AND",
    "phase": "pre"
  }'

# Evaluate a hook
curl -X POST http://localhost:3000/api/hooks/evaluate \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $API_TOKEN" \
  -d '{
    "hookId": "validate-budget",
    "data": "@prefix : <https://example.org/> .\n:allocation1 :amount 8500 ."
  }'

# List all hooks
curl -H "Authorization: Bearer $API_TOKEN" \
  "http://localhost:3000/api/hooks/list?limit=10"

# Get hook by ID
curl -H "Authorization: Bearer $API_TOKEN" \
  http://localhost:3000/api/hooks/validate-budget

# Delete a hook
curl -X DELETE \
  -H "Authorization: Bearer $API_TOKEN" \
  http://localhost:3000/api/hooks/validate-budget
```

## Additional Resources

- [OpenAPI Specification](./openapi.yaml)
- [Swagger UI Documentation](http://localhost:3000/api-docs)
- [GitHub Repository](https://github.com/yourusername/unrdf)
- [Knowledge Hooks Whitepaper](../WHITEPAPER.md)

## Support

For questions or issues:
- Open an issue on GitHub
- Email: support@unrdf.example.com
- Discord: https://discord.gg/unrdf
