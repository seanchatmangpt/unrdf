# KGC Markdown (.kgcmd) Format Specification

> **Version**: 1.0.0
> **Date**: 2025-12-26
> **Status**: Implementation-Ready
> **Type**: Formal Specification

---

## Table of Contents

1. [Overview](#1-overview)
2. [Frontmatter Schema](#2-frontmatter-schema)
3. [Executable Block Grammar](#3-executable-block-grammar)
4. [Content Sections](#4-content-sections)
5. [Determinism Guarantees](#5-determinism-guarantees)
6. [Receipt Integration](#6-receipt-integration)
7. [Validation Rules](#7-validation-rules)
8. [Error Model](#8-error-model)
9. [Complete Examples](#9-complete-examples)
10. [Implementation Notes](#10-implementation-notes)

---

## 1. Overview

### 1.1 Purpose

KGC Markdown (.kgcmd) is a **verifiable documentation format** that combines:

- Static prose (manually authored content)
- Dynamic content (generated from executable blocks)
- Cryptographic receipts (proof of correct execution)
- Bounded computation (resource limits enforced)

**Core Invariant**: Every dynamic section must be backed by a receipt proving it was generated deterministically within declared bounds.

### 1.2 Design Goals

1. **Deterministic Reproduction**: Same inputs → same outputs → same hashes
2. **Cryptographic Verification**: Every claim backed by receipt chain
3. **Resource Bounds**: Queries/extractions limited (prevent runaway computation)
4. **Multi-View Support**: Same content, multiple presentations (Diátaxis framework)
5. **Source Traceability**: All content linked to source code/data ranges

### 1.3 File Structure

```
filename.kgcmd
├── YAML Frontmatter (o_hash, policy_id, receipts, bounds, etc.)
├── Markdown Content
│   ├── Static Sections (prose)
│   ├── Dynamic Sections (from executable blocks)
│   └── Executable Blocks (kgc:query, kgc:proof, kgc:extract, kgc:render)
└── Receipt Appendix (cryptographic proof chain)
```

---

## 2. Frontmatter Schema

### 2.1 Complete Schema Definition

The frontmatter is a YAML block delimited by `---` at the start of the file. All fields use camelCase naming.

```yaml
---
# Universe snapshot identifier (SHA-256 hex, 64 chars)
o_hash: 'a3f5b8c2d1e4f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1'

# Policy pack UUID that admitted this document
policy_id: '550e8400-e29b-41d4-a716-446655440000'

# Receipt hashes justifying content (SHA-256 hex, 64 chars each)
receipts:
  - 'b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5'
  - 'c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6'

# Resource bounds for executable blocks
bounds:
  maxQueries: 100 # Max SPARQL/query operations
  maxRuntime: 5000 # Max milliseconds per block
  maxFileScans: 50 # Max files to scan in kgc:extract

# Diátaxis views this document supports
views:
  - 'reference' # API documentation
  - 'explanation' # Conceptual overview

# Source code ranges this doc was extracted from
sources:
  - path: 'src/api/user.mjs'
    lineStart: 1
    lineEnd: 250
    hash: 'd6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7'
  - path: 'src/api/auth.mjs'
    lineStart: 10
    lineEnd: 85
    hash: 'e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8'

# Document version (semver)
version: '1.2.0'

# Timestamps (ISO 8601 with timezone)
createdAt: '2025-12-26T10:30:00Z'
lastProved: '2025-12-26T14:22:15Z'

# Optional metadata
tags:
  - 'api'
  - 'authentication'
  - 'user-management'

authors:
  - name: 'System'
    role: 'Automated Extractor'
---
```

### 2.2 Field Definitions

#### 2.2.1 `o_hash` (required)

- **Type**: `string`
- **Format**: SHA-256 hex digest (exactly 64 hexadecimal characters)
- **Purpose**: Identifies the universe snapshot (knowledge graph state) at proof time
- **Example**: `"a3f5b8c2d1e4f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1"`
- **Validation**: Must match `^[a-f0-9]{64}$`

#### 2.2.2 `policy_id` (required)

- **Type**: `string`
- **Format**: UUID v4 (RFC 4122)
- **Purpose**: Which policy pack validated and admitted this document
- **Example**: `"550e8400-e29b-41d4-a716-446655440000"`
- **Validation**: Must match UUID regex `^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$`

#### 2.2.3 `receipts` (required)

- **Type**: `array<string>`
- **Format**: Array of SHA-256 hex digests (64 chars each)
- **Purpose**: List of receipt hashes that justify dynamic content in this document
- **Constraints**:
  - Minimum 0 receipts (static-only docs)
  - Maximum 1000 receipts (prevent unbounded growth)
  - Each receipt must exist in receipt store
  - Each dynamic section must reference at least one receipt
- **Example**:
  ```yaml
  receipts:
    - 'b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5'
    - 'c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6'
  ```

#### 2.2.4 `bounds` (required)

- **Type**: `object`
- **Purpose**: Resource limits for executable blocks
- **Fields**:
  - `maxQueries` (number): Maximum SPARQL/query operations (1-10000)
  - `maxRuntime` (number): Maximum milliseconds per block (100-60000)
  - `maxFileScans` (number): Maximum files to scan (1-1000)
- **Example**:
  ```yaml
  bounds:
    maxQueries: 100
    maxRuntime: 5000
    maxFileScans: 50
  ```
- **Validation**: All fields required, must be positive integers within ranges

#### 2.2.5 `views` (required)

- **Type**: `array<string>`
- **Format**: Array of Diátaxis view types
- **Allowed Values**: `["tutorial", "how-to", "reference", "explanation"]`
- **Purpose**: Declares which documentation views this file supports
- **Constraints**: Minimum 1 view, maximum 4 views
- **Example**:
  ```yaml
  views:
    - 'reference'
    - 'explanation'
  ```

#### 2.2.6 `sources` (required)

- **Type**: `array<object>`
- **Purpose**: Source code/data ranges this doc was extracted from
- **Object Schema**:
  - `path` (string, required): Relative file path from repo root
  - `lineStart` (number, required): First line (1-indexed)
  - `lineEnd` (number, required): Last line (1-indexed, inclusive)
  - `hash` (string, required): SHA-256 of extracted range
- **Constraints**:
  - Minimum 0 sources (pure prose docs)
  - Maximum 100 sources
  - `lineEnd` ≥ `lineStart`
  - `path` must not contain `..` (no directory traversal)
- **Example**:
  ```yaml
  sources:
    - path: 'src/api/user.mjs'
      lineStart: 1
      lineEnd: 250
      hash: 'd6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7'
  ```

#### 2.2.7 `version` (required)

- **Type**: `string`
- **Format**: Semantic versioning (semver 2.0.0)
- **Purpose**: Document version for change tracking
- **Example**: `"1.2.0"`
- **Validation**: Must match `^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$`

#### 2.2.8 `createdAt` (required)

- **Type**: `string`
- **Format**: ISO 8601 datetime with timezone
- **Purpose**: When document was first created
- **Example**: `"2025-12-26T10:30:00Z"`
- **Validation**: Must be valid ISO 8601 datetime

#### 2.2.9 `lastProved` (required)

- **Type**: `string`
- **Format**: ISO 8601 datetime with timezone
- **Purpose**: When receipts were last verified
- **Example**: `"2025-12-26T14:22:15Z"`
- **Validation**: Must be valid ISO 8601, must be ≥ `createdAt`

#### 2.2.10 `tags` (optional)

- **Type**: `array<string>`
- **Purpose**: Categorization and search
- **Constraints**: Maximum 20 tags, each 1-50 chars
- **Example**:
  ```yaml
  tags:
    - 'api'
    - 'authentication'
  ```

#### 2.2.11 `authors` (optional)

- **Type**: `array<object>`
- **Object Schema**:
  - `name` (string, required): Author name
  - `role` (string, optional): Author role
- **Example**:
  ```yaml
  authors:
    - name: 'System'
      role: 'Automated Extractor'
  ```

---

## 3. Executable Block Grammar

### 3.1 Block Structure

Executable blocks are fenced code blocks with special type identifiers. General structure:

````markdown
```kgc:BLOCK_TYPE
{
  "receiptId": "b4c5d6e7...",
  "expectedOutputFormat": "json|markdown|text",
  "determinismLevel": "strict|lenient|best-effort",
  "metadata": { /* block-specific metadata */ }
}
---
BLOCK BODY (query, code, or JSON)
```
````

**Separator**: `---` divides metadata (JSON header) from body
**Metadata**: Always valid JSON object
**Body**: Format depends on block type

### 3.2 Block Type: `kgc:query`

**Purpose**: Execute SPARQL/N3/SHACL queries with cardinality bounds

**Structure**:

````markdown
```kgc:query
{
  "receiptId": "b4c5d6e7f8a9...",
  "expectedOutputFormat": "json",
  "determinismLevel": "strict",
  "metadata": {
    "queryType": "sparql",           // "sparql" | "n3" | "shacl"
    "resultBounds": {
      "minResults": 0,
      "maxResults": 1000
    },
    "timeout": 5000,                 // milliseconds
    "description": "Fetch all users with admin role"
  }
}
---
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/>

SELECT ?user ?name WHERE {
  ?user a foaf:Person ;
        foaf:name ?name ;
        ex:role ex:Admin .
}
ORDER BY ?name
```
````

**Metadata Fields**:

- `queryType` (string, required): `"sparql"`, `"n3"`, or `"shacl"`
- `resultBounds` (object, required):
  - `minResults` (number): Minimum expected results
  - `maxResults` (number): Maximum allowed results
- `timeout` (number, required): Max execution time in ms
- `description` (string, optional): Human-readable purpose

**Determinism Requirements**:

- `ORDER BY` clause required if `determinismLevel: "strict"`
- Result serialization must be canonical JSON (sorted keys)
- Timestamps excluded from hashing unless explicitly marked

**Validation**:

- Query must parse correctly for declared `queryType`
- Result count must satisfy `minResults ≤ count ≤ maxResults`
- Execution must complete within `timeout`
- Receipt must exist and match `receiptId`

### 3.3 Block Type: `kgc:proof`

**Purpose**: Verify receipt chain and validate cryptographic proofs

**Structure**:

````markdown
```kgc:proof
{
  "receiptId": "c5d6e7f8a9b0...",
  "expectedOutputFormat": "json",
  "determinismLevel": "strict",
  "metadata": {
    "proofType": "merkle",           // "merkle" | "sequential" | "batch"
    "verifyChain": true,             // Check full receipt chain
    "validateSignatures": false,     // Check cryptographic signatures
    "description": "Verify API extraction receipt chain"
  }
}
---
{
  "receiptIds": [
    "b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5",
    "c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6"
  ],
  "expectedRoot": "d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7"
}
```
````

**Metadata Fields**:

- `proofType` (string, required): `"merkle"`, `"sequential"`, or `"batch"`
- `verifyChain` (boolean, required): Check full receipt dependency chain
- `validateSignatures` (boolean, required): Verify cryptographic signatures
- `description` (string, optional)

**Body Schema** (JSON):

- `receiptIds` (array<string>, required): Receipt hashes to verify
- `expectedRoot` (string, required): Expected Merkle root hash

**Output**:

- JSON object with verification results:
  ```json
  {
    "valid": true,
    "verifiedCount": 2,
    "merkleRoot": "d6e7f8a9...",
    "chainDepth": 5,
    "invalidReceipts": []
  }
  ```

### 3.4 Block Type: `kgc:extract`

**Purpose**: Extract code exports, API surfaces, type signatures from source files

**Structure**:

````markdown
```kgc:extract
{
  "receiptId": "d6e7f8a9b0c1...",
  "expectedOutputFormat": "json",
  "determinismLevel": "lenient",
  "metadata": {
    "extractionType": "exports",     // "exports" | "types" | "functions" | "classes"
    "fileGlobs": ["src/api/**/*.mjs"],
    "includePrivate": false,
    "includeDocstrings": true,
    "description": "Extract public API surface from user module"
  }
}
---
{
  "targetFiles": [
    "src/api/user.mjs",
    "src/api/auth.mjs"
  ],
  "filters": {
    "visibility": "public",
    "documented": true
  }
}
```
````

**Metadata Fields**:

- `extractionType` (string, required): What to extract
  - `"exports"`: Exported functions, classes, constants
  - `"types"`: Type definitions (JSDoc, TypeScript)
  - `"functions"`: Function signatures
  - `"classes"`: Class definitions
- `fileGlobs` (array<string>, required): Glob patterns for files to scan
- `includePrivate` (boolean, required): Include private/internal APIs
- `includeDocstrings` (boolean, required): Extract JSDoc comments
- `description` (string, optional)

**Body Schema** (JSON):

- `targetFiles` (array<string>, optional): Specific files (overrides globs)
- `filters` (object, optional): Additional filtering criteria
  - `visibility` (string): `"public"` | `"private"` | `"all"`
  - `documented` (boolean): Only include documented items

**Output**:

- JSON array of extracted items:
  ```json
  [
    {
      "name": "createUser",
      "type": "function",
      "file": "src/api/user.mjs",
      "line": 45,
      "signature": "(userData: UserData) => Promise<User>",
      "docstring": "Creates a new user with validation"
    }
  ]
  ```

**Determinism**:

- `determinismLevel: "lenient"` allows file order variations
- Output sorted by file path, then line number
- Excludes timestamps, absolute paths

### 3.5 Block Type: `kgc:render`

**Purpose**: Transform JSON input into formatted markdown sections

**Structure**:

````markdown
```kgc:render
{
  "receiptId": "e7f8a9b0c1d2...",
  "expectedOutputFormat": "markdown",
  "determinismLevel": "strict",
  "metadata": {
    "templateName": "api-reference",
    "sectionTitle": "User API Reference",
    "includeTableOfContents": true,
    "description": "Render extracted API as reference docs"
  }
}
---
{
  "items": [
    {
      "name": "createUser",
      "type": "function",
      "signature": "(userData: UserData) => Promise<User>",
      "docstring": "Creates a new user with validation",
      "parameters": [
        {
          "name": "userData",
          "type": "UserData",
          "description": "User creation payload"
        }
      ],
      "returns": {
        "type": "Promise<User>",
        "description": "Newly created user object"
      }
    }
  ]
}
```
````

**Metadata Fields**:

- `templateName` (string, required): Rendering template identifier
  - `"api-reference"`: API documentation format
  - `"tutorial"`: Tutorial-style format
  - `"how-to"`: Task-oriented format
- `sectionTitle` (string, required): Heading for generated section
- `includeTableOfContents` (boolean, required): Generate TOC
- `description` (string, optional)

**Body Schema** (JSON):

- Template-specific JSON structure
- For `"api-reference"`:
  - `items` (array<object>, required): API items to render
  - Each item has `name`, `type`, `signature`, `docstring`, etc.

**Output**:

- Markdown text conforming to template
- Example:

  ```markdown
  ## User API Reference

  ### `createUser(userData)`

  Creates a new user with validation.

  **Parameters:**

  - `userData` (`UserData`): User creation payload

  **Returns:**

  - `Promise<User>`: Newly created user object
  ```

**Determinism**:

- Template must produce identical output for identical input JSON
- JSON keys sorted alphabetically before rendering
- No timestamps or random IDs in output

---

## 4. Content Sections

### 4.1 Section Types

KGC Markdown documents contain three types of sections:

#### 4.1.1 Static Sections

**Definition**: Manually authored prose with no dynamic generation

**Characteristics**:

- Pure markdown (headings, paragraphs, lists, links)
- No executable blocks
- Content hash computed directly from markdown text
- No receipt required

**Example**:

```markdown
## Introduction

This document describes the User API for managing user accounts.
The API provides CRUD operations with role-based access control.
```

**Hashing**: SHA-256 of normalized markdown (trailing whitespace removed, consistent line endings)

#### 4.1.2 Dynamic Sections

**Definition**: Content generated entirely from executable blocks

**Characteristics**:

- Generated by `kgc:query`, `kgc:extract`, or `kgc:render` blocks
- Must reference `receiptId` in block metadata
- Receipt validates input hash → output hash
- Cannot be manually edited (regenerated on proof)

**Example**:

```markdown
## API Reference

<!-- kgc:dynamic section="api-reference" receiptId="e7f8a9b0c1d2..." -->

### `createUser(userData)`

Creates a new user with validation.

**Parameters:**

- `userData` (`UserData`): User creation payload

**Returns:**

- `Promise<User>`: Newly created user object

<!-- /kgc:dynamic -->
```

**Validation**:

- HTML comments mark dynamic section boundaries
- `receiptId` must exist in frontmatter `receipts` array
- Receipt output hash must match section content hash
- Regeneration must produce identical output

#### 4.1.3 Hybrid Sections

**Definition**: Mix of static prose and dynamic content

**Characteristics**:

- Static markdown + embedded dynamic blocks
- Each dynamic portion references separate receipt
- Static portions hashed separately

**Example**:

```markdown
## User Management

The following users have admin privileges:

<!-- kgc:dynamic section="admin-users" receiptId="f8a9b0c1d2e3..." -->

- Alice (alice@example.com)
- Bob (bob@example.com)
<!-- /kgc:dynamic -->

Admins can perform the following operations:

<!-- kgc:dynamic section="admin-operations" receiptId="a9b0c1d2e3f4..." -->

- Create users
- Delete users
- Modify roles
<!-- /kgc:dynamic -->
```

**Hashing**:

- Static prose: Hash of markdown text
- Dynamic portions: Hash from receipt
- Section hash: Merkle tree of sub-hashes

### 4.2 Section Ordering Rules

1. **Frontmatter**: Always first
2. **Title** (H1): Required, immediately after frontmatter
3. **Table of Contents**: Optional, after title
4. **Body Sections**: Any order, consistent with `views`
5. **Executable Blocks**: Can appear anywhere in body
6. **Receipt Appendix**: Always last (if present)

### 4.3 Heading Level Constraints

- **H1 (`#`)**: Document title only (exactly 1)
- **H2 (`##`)**: Major sections (e.g., "API Reference", "Tutorial")
- **H3 (`###`)**: Subsections (e.g., function names)
- **H4 (`####`)**: Details (e.g., parameters, examples)
- **H5-H6**: Rarely used, for deep nesting

**Rule**: No heading level skips (e.g., H2 → H4 without H3)

### 4.4 Cross-References

**Internal Links**:

```markdown
See [Authentication](#authentication) for details.
```

**External Links**:

```markdown
Refer to [SPARQL 1.1 Spec](https://www.w3.org/TR/sparql11-query/).
```

**Source Links** (to frontmatter sources):

```markdown
<!-- kgc:source-link index="0" line="45" -->

Defined in `src/api/user.mjs:45`
```

**Receipt Links**:

```markdown
<!-- kgc:receipt-link id="e7f8a9b0c1d2..." -->

[Verified by receipt e7f8a9...](#receipt-e7f8a9b0c1d2)
```

---

## 5. Determinism Guarantees

### 5.1 Determinism Levels

#### 5.1.1 `strict`

**Definition**: Bit-for-bit reproducible output

**Requirements**:

- Queries must have `ORDER BY` clause
- JSON output uses canonical serialization (RFC 8785)
- No timestamps in output (unless explicitly part of data)
- No random IDs or UUIDs
- No floating-point precision issues
- File order deterministic (sorted paths)

**Use Cases**:

- API reference generation
- Query results for documentation
- Proof verification

**Example**:

```json
{
  "determinismLevel": "strict",
  "queryType": "sparql"
}
```

Query must include:

```sparql
SELECT ?user ?name WHERE { ... }
ORDER BY ?name ?user
```

#### 5.1.2 `lenient`

**Definition**: Semantically equivalent output, minor variations allowed

**Allowed Variations**:

- List item order (unordered lists)
- JSON key order (non-canonical)
- Whitespace differences (indentation)
- Timestamp precision (second vs. millisecond)

**Prohibited Variations**:

- Different data values
- Missing/extra items
- Type changes

**Use Cases**:

- File extraction (file discovery order varies)
- Best-effort query results

**Example**:

```json
{
  "determinismLevel": "lenient",
  "extractionType": "exports"
}
```

#### 5.1.3 `best-effort`

**Definition**: Non-deterministic, documented variations

**Characteristics**:

- Output may vary between runs
- Variations documented in receipt metadata
- Hash computed over normalized form
- Receipts note "non-deterministic" flag

**Use Cases**:

- Interactive examples
- Live query results
- Timestamp-dependent content

**Example**:

```json
{
  "determinismLevel": "best-effort",
  "metadata": {
    "nonDeterministicFields": ["timestamp", "executionTime"]
  }
}
```

### 5.2 JSON Canonicalization

**Standard**: RFC 8785 (JSON Canonicalization Scheme)

**Rules**:

1. Object keys sorted lexicographically
2. No insignificant whitespace
3. Unicode normalization (NFC)
4. Number representation canonical
5. No trailing zeros in decimals

**Example**:

Non-canonical:

```json
{ "name": "Alice", "id": 42, "active": true }
```

Canonical:

```json
{ "active": true, "id": 42, "name": "Alice" }
```

**Implementation**:

```javascript
import { canonicalize } from 'json-canonicalize';
const canonical = canonicalize(obj);
const hash = sha256(canonical);
```

### 5.3 Handling Non-Deterministic Elements

**Strategy 1: Exclusion**

Mark fields excluded from hashing:

```json
{
  "user": "Alice",
  "createdAt": "2025-12-26T10:30:00Z", // excluded from hash
  "_meta": {
    "excludeFromHash": ["createdAt"]
  }
}
```

**Strategy 2: Normalization**

Convert to deterministic form:

```javascript
// Timestamp → ISO 8601 string, truncated to day
"2025-12-26T10:30:45.123Z" → "2025-12-26T00:00:00Z"

// Floating point → fixed precision
3.141592653589793 → "3.14159265"
```

**Strategy 3: Documentation**

Receipt metadata notes non-determinism:

```json
{
  "receiptId": "...",
  "determinismLevel": "best-effort",
  "nonDeterministicFields": ["timestamp", "processingTime"],
  "hashStrategy": "exclude-timestamps"
}
```

---

## 6. Receipt Integration

### 6.1 Receipt Structure

**Receipt**: Cryptographic proof of execution

**Schema**:

```json
{
  "id": "b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5",
  "timestamp": "2025-12-26T14:22:15Z",
  "o_hash": "a3f5b8c2d1e4f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1",
  "block_type": "kgc:query",
  "input_hash": "c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6",
  "output_hash": "d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7",
  "decision": "ADMIT",
  "metadata": {
    "queryType": "sparql",
    "executionTime": 245,
    "resultCount": 12,
    "determinismLevel": "strict"
  },
  "dependencies": ["a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0"],
  "merkle_proof": {
    "siblings": [
      "e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8",
      "f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9"
    ],
    "root": "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2"
  }
}
```

### 6.2 Receipt Fields

- **id** (string, required): Receipt hash (SHA-256)
- **timestamp** (string, required): ISO 8601 execution time
- **o_hash** (string, required): Universe snapshot hash
- **block_type** (string, required): Executable block type
- **input_hash** (string, required): Hash of input (query/config)
- **output_hash** (string, required): Hash of output
- **decision** (string, required): `"ADMIT"` | `"REJECT"` | `"PARTIAL"`
- **metadata** (object, optional): Block-specific data
- **dependencies** (array<string>, optional): Dependent receipt IDs
- **merkle_proof** (object, optional): Merkle tree proof

### 6.3 Receipt Chain Validation

**Chain**: Directed acyclic graph (DAG) of receipts

**Validation Steps**:

1. **Existence Check**: All `receiptId` references exist in store
2. **Hash Verification**: Receipt `id` matches SHA-256 of receipt body
3. **Dependency Resolution**: All `dependencies` receipts exist
4. **Merkle Proof**: Verify siblings hash to root
5. **Universe Consistency**: All receipts use same `o_hash`
6. **Timestamp Order**: Dependencies have earlier timestamps
7. **Decision Check**: All receipts have `decision: "ADMIT"`

**Example Chain**:

```
Receipt A (extract API)
    ↓
Receipt B (render markdown) → depends on A
    ↓
Receipt C (proof verification) → depends on B
```

Validation:

```javascript
// Pseudo-code
function validateChain(receiptIds, store) {
  const receipts = receiptIds.map(id => store.get(id));

  // Check existence
  if (receipts.some(r => !r)) throw new MissingReceipt();

  // Verify hashes
  for (const r of receipts) {
    const computed = sha256(canonicalize(r));
    if (computed !== r.id) throw new MismatchedHash();
  }

  // Check DAG (no cycles)
  if (hasCycle(receipts)) throw new CyclicDependency();

  // Verify Merkle proofs
  for (const r of receipts) {
    if (r.merkle_proof && !verifyMerkleProof(r)) {
      throw new InvalidMerkleProof();
    }
  }

  return true;
}
```

### 6.4 Merkle Proof Structure

**Purpose**: Prove receipt is part of batch without revealing all receipts

**Structure**:

```json
{
  "merkle_proof": {
    "siblings": [
      "e7f8a9b0c1d2...", // Sibling hash at level 0
      "f8a9b0c1d2e3..." // Sibling hash at level 1
    ],
    "root": "a1b2c3d4e5f6...", // Merkle root
    "index": 3, // Leaf index in tree
    "totalLeaves": 8 // Total receipts in batch
  }
}
```

**Verification**:

```javascript
function verifyMerkleProof(receipt) {
  let hash = receipt.id;
  const { siblings, root, index } = receipt.merkle_proof;

  let position = index;
  for (const sibling of siblings) {
    hash = position % 2 === 0 ? sha256(hash + sibling) : sha256(sibling + hash);
    position = Math.floor(position / 2);
  }

  return hash === root;
}
```

---

## 7. Validation Rules

### 7.1 Document-Level Validation

**Rule V1**: Frontmatter must parse as valid YAML
**Rule V2**: All required frontmatter fields present
**Rule V3**: Exactly one H1 heading (document title)
**Rule V4**: All `receiptId` references exist in `receipts` array
**Rule V5**: All `receipts` array items used at least once
**Rule V6**: No heading level skips
**Rule V7**: Executable blocks have valid JSON metadata
**Rule V8**: Dynamic sections have opening/closing comments

### 7.2 Frontmatter Validation

**Rule F1**: `o_hash` matches regex `^[a-f0-9]{64}$`
**Rule F2**: `policy_id` is valid UUID v4
**Rule F3**: `receipts` is array of 64-char hex strings
**Rule F4**: `bounds.maxQueries` ∈ [1, 10000]
**Rule F5**: `bounds.maxRuntime` ∈ [100, 60000]
**Rule F6**: `bounds.maxFileScans` ∈ [1, 1000]
**Rule F7**: `views` non-empty subset of `["tutorial", "how-to", "reference", "explanation"]`
**Rule F8**: `sources` array items have `lineEnd ≥ lineStart`
**Rule F9**: `version` matches semver regex
**Rule F10**: `lastProved ≥ createdAt`

### 7.3 Executable Block Validation

**Rule B1**: Block metadata is valid JSON object
**Rule B2**: `receiptId` field present and 64-char hex
**Rule B3**: `expectedOutputFormat` ∈ `["json", "markdown", "text"]`
**Rule B4**: `determinismLevel` ∈ `["strict", "lenient", "best-effort"]`
**Rule B5**: Metadata separator `---` present
**Rule B6**: Body format matches block type requirements
**Rule B7**: Query blocks with `determinismLevel: "strict"` have `ORDER BY`

### 7.4 Receipt Validation

**Rule R1**: Receipt `id` matches SHA-256 of canonical receipt JSON
**Rule R2**: Receipt `o_hash` matches document frontmatter `o_hash`
**Rule R3**: All `dependencies` receipt IDs exist
**Rule R4**: No cyclic dependencies
**Rule R5**: `timestamp` is valid ISO 8601
**Rule R6**: `decision` ∈ `["ADMIT", "REJECT", "PARTIAL"]`
**Rule R7**: `input_hash` matches hash of block metadata + body
**Rule R8**: `output_hash` matches hash of generated content
**Rule R9**: Merkle proof verifies if present

### 7.5 Content Validation

**Rule C1**: Dynamic section content matches `output_hash` from receipt
**Rule C2**: Static section content hashes are stable (no spurious changes)
**Rule C3**: Cross-references point to existing sections/receipts
**Rule C4**: Source links reference valid `sources` array indices

---

## 8. Error Model

### 8.1 Error Types

#### 8.1.1 `InvalidFrontmatter`

**Severity**: Error (blocks document loading)

**Causes**:

- YAML parse failure
- Missing required field
- Invalid field format (wrong regex, type)
- Out-of-range values

**Message Format**:

```
InvalidFrontmatter: <field> <issue>
Examples:
- InvalidFrontmatter: o_hash must be 64 hex characters, got 32
- InvalidFrontmatter: missing required field 'policy_id'
- InvalidFrontmatter: bounds.maxQueries must be 1-10000, got 50000
```

**Remediation**:

1. Check frontmatter YAML syntax
2. Verify all required fields present
3. Validate field formats against spec
4. Ensure numeric ranges correct

#### 8.1.2 `MissingReceipt`

**Severity**: Error (blocks verification)

**Causes**:

- `receiptId` referenced but not in frontmatter `receipts` array
- Receipt ID in frontmatter but not in receipt store
- Broken dependency chain

**Message Format**:

```
MissingReceipt: <receiptId> referenced in <location> but not found
Examples:
- MissingReceipt: b4c5d6e7... referenced in kgc:query block but not in frontmatter
- MissingReceipt: c5d6e7f8... in frontmatter but not in receipt store
- MissingReceipt: dependency a9b0c1d2... not found for receipt b4c5d6e7...
```

**Remediation**:

1. Add receipt ID to frontmatter `receipts` array
2. Ensure receipt exists in store (regenerate if needed)
3. Resolve dependency chain issues

#### 8.1.3 `MismatchedHash`

**Severity**: Error (blocks verification)

**Causes**:

- Receipt `id` doesn't match computed hash
- Receipt `input_hash` doesn't match block input
- Receipt `output_hash` doesn't match generated content
- Source `hash` doesn't match file content

**Message Format**:

```
MismatchedHash: <entity> hash mismatch
Expected: <expected_hash>
Got: <actual_hash>

Examples:
- MismatchedHash: receipt id
  Expected: b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5
  Got: c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6

- MismatchedHash: dynamic section output
  Expected: d6e7f8a9... (from receipt)
  Got: e7f8a9b0... (from content)
```

**Remediation**:

1. Regenerate receipt from scratch
2. Verify input hasn't changed
3. Check for non-deterministic content
4. Ensure canonical JSON serialization

#### 8.1.4 `BoundsExceeded`

**Severity**: Error (blocks execution)

**Causes**:

- Query returned more results than `bounds.maxQueries`
- Execution exceeded `bounds.maxRuntime`
- File scan exceeded `bounds.maxFileScans`

**Message Format**:

```
BoundsExceeded: <bound_type> exceeded
Limit: <limit>
Actual: <actual>

Examples:
- BoundsExceeded: maxQueries
  Limit: 100
  Actual: 1245

- BoundsExceeded: maxRuntime
  Limit: 5000ms
  Actual: 8750ms
```

**Remediation**:

1. Increase bounds in frontmatter (if justified)
2. Optimize query (add LIMIT clause)
3. Reduce file scan scope (tighter globs)
4. Split into multiple blocks

#### 8.1.5 `NonDeterministic`

**Severity**: Warning (for `lenient`), Error (for `strict`)

**Causes**:

- Query lacks `ORDER BY` with `determinismLevel: strict`
- Output varies between runs
- Non-canonical JSON serialization
- Floating-point precision issues

**Message Format**:

```
NonDeterministic: <issue> in <block_type> block
Determinism Level: <level>

Examples:
- NonDeterministic: query missing ORDER BY clause
  Determinism Level: strict
  Block: kgc:query at line 45

- NonDeterministic: output hash varies between runs
  Determinism Level: strict
  Previous: d6e7f8a9...
  Current: e7f8a9b0...
```

**Remediation**:

1. Add `ORDER BY` to queries
2. Use canonical JSON serialization
3. Exclude non-deterministic fields from hash
4. Change `determinismLevel` to `lenient` if acceptable

#### 8.1.6 `InvalidBlockStructure`

**Severity**: Error (blocks execution)

**Causes**:

- Missing metadata separator `---`
- Invalid JSON in metadata
- Missing required metadata fields
- Body doesn't match block type format

**Message Format**:

```
InvalidBlockStructure: <issue> in <block_type> block at line <line>

Examples:
- InvalidBlockStructure: missing metadata separator '---'
  Block: kgc:query at line 67

- InvalidBlockStructure: invalid JSON in metadata
  Block: kgc:extract at line 102
  Error: Unexpected token '}' at position 45
```

**Remediation**:

1. Add `---` separator between metadata and body
2. Validate JSON syntax
3. Ensure all required metadata fields present
4. Check body format matches block type

#### 8.1.7 `CyclicDependency`

**Severity**: Error (blocks verification)

**Causes**:

- Receipt dependency chain contains cycle
- Self-referential receipt

**Message Format**:

```
CyclicDependency: cycle detected in receipt chain
Cycle: <receipt_id_1> → <receipt_id_2> → ... → <receipt_id_1>

Example:
- CyclicDependency: cycle detected
  Cycle: b4c5d6e7... → c5d6e7f8... → d6e7f8a9... → b4c5d6e7...
```

**Remediation**:

1. Review receipt dependencies
2. Remove circular references
3. Regenerate receipts in correct order

### 8.2 Error Handling Strategy

**Fail-Fast**: Stop processing on first error (severity: Error)
**Collect-Warnings**: Continue processing, report warnings at end
**Contextual**: Include file path, line number, field name in errors

**Example Error Output**:

```
Error in document: docs/api-reference.kgcmd

[Error] InvalidFrontmatter (line 5)
  Field: o_hash
  Issue: must be 64 hex characters, got 32
  Value: "a3f5b8c2d1e4f6a7b8c9d0e1f2a3b4c5"

[Error] MissingReceipt (line 67)
  Receipt ID: b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5
  Referenced in: kgc:query block
  Not found in: frontmatter receipts array

[Warning] NonDeterministic (line 67)
  Block: kgc:query
  Issue: query missing ORDER BY clause
  Determinism Level: strict
  Recommendation: Add ORDER BY ?name to query

Total: 2 errors, 1 warning
Status: FAILED (fix errors to proceed)
```

---

## 9. Complete Examples

### 9.1 Example 1: API Reference Document

**File**: `examples/api-reference.kgcmd`

````markdown
---
o_hash: 'a3f5b8c2d1e4f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1'
policy_id: '550e8400-e29b-41d4-a716-446655440000'
receipts:
  - 'b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5'
  - 'c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6'
bounds:
  maxQueries: 50
  maxRuntime: 3000
  maxFileScans: 20
views:
  - 'reference'
sources:
  - path: 'src/api/user.mjs'
    lineStart: 1
    lineEnd: 250
    hash: 'd6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7'
version: '1.0.0'
createdAt: '2025-12-26T10:00:00Z'
lastProved: '2025-12-26T14:00:00Z'
tags:
  - 'api'
  - 'user-management'
---

# User API Reference

## Overview

This document provides complete API reference for the User Management module.
All APIs require authentication via JWT token.

## Data Extraction

```kgc:extract
{
  "receiptId": "b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5",
  "expectedOutputFormat": "json",
  "determinismLevel": "lenient",
  "metadata": {
    "extractionType": "exports",
    "fileGlobs": ["src/api/user.mjs"],
    "includePrivate": false,
    "includeDocstrings": true,
    "description": "Extract public user API exports"
  }
}
---
{
  "targetFiles": ["src/api/user.mjs"],
  "filters": {
    "visibility": "public",
    "documented": true
  }
}
```

## API Functions

<!-- kgc:dynamic section="api-functions" receiptId="c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6" -->

### `createUser(userData)`

Creates a new user account with validation.

**Parameters:**

- `userData` (`UserData`): User creation payload
  - `email` (string, required): User email address
  - `name` (string, required): Full name
  - `role` (string, optional): User role (default: "user")

**Returns:**

- `Promise<User>`: Newly created user object

**Throws:**

- `ValidationError`: Invalid user data
- `ConflictError`: Email already exists

**Example:**

```javascript
const user = await createUser({
  email: 'alice@example.com',
  name: 'Alice Smith',
  role: 'admin',
});
```

### `getUserById(userId)`

Retrieves user by ID.

**Parameters:**

- `userId` (`string`): User UUID

**Returns:**

- `Promise<User | null>`: User object or null if not found

**Example:**

```javascript
const user = await getUserById('550e8400-e29b-41d4-a716-446655440000');
```

### `updateUser(userId, updates)`

Updates user fields.

**Parameters:**

- `userId` (`string`): User UUID
- `updates` (`Partial<UserData>`): Fields to update

**Returns:**

- `Promise<User>`: Updated user object

**Throws:**

- `NotFoundError`: User not found
- `ValidationError`: Invalid update data

### `deleteUser(userId)`

Deletes user account.

**Parameters:**

- `userId` (`string`): User UUID

**Returns:**

- `Promise<void>`

**Throws:**

- `NotFoundError`: User not found
- `PermissionError`: Insufficient permissions

<!-- /kgc:dynamic -->

## Type Definitions

### `UserData`

```typescript
interface UserData {
  email: string;
  name: string;
  role?: 'user' | 'admin' | 'moderator';
}
```

### `User`

```typescript
interface User extends UserData {
  id: string;
  createdAt: string;
  updatedAt: string;
}
```

## Receipt Verification

```kgc:proof
{
  "receiptId": "d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7",
  "expectedOutputFormat": "json",
  "determinismLevel": "strict",
  "metadata": {
    "proofType": "sequential",
    "verifyChain": true,
    "validateSignatures": false,
    "description": "Verify extraction and rendering receipts"
  }
}
---
{
  "receiptIds": [
    "b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5",
    "c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6"
  ],
  "expectedRoot": "e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8"
}
```

---

## Appendix: Receipts

### Receipt b4c5d6e7... (kgc:extract)

```json
{
  "id": "b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5",
  "timestamp": "2025-12-26T13:45:00Z",
  "o_hash": "a3f5b8c2d1e4f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1",
  "block_type": "kgc:extract",
  "input_hash": "c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6",
  "output_hash": "d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7",
  "decision": "ADMIT",
  "metadata": {
    "extractionType": "exports",
    "filesScanned": 1,
    "itemsExtracted": 4,
    "determinismLevel": "lenient"
  }
}
```

### Receipt c5d6e7f8... (kgc:render)

```json
{
  "id": "c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6",
  "timestamp": "2025-12-26T13:47:30Z",
  "o_hash": "a3f5b8c2d1e4f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1",
  "block_type": "kgc:render",
  "input_hash": "d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7",
  "output_hash": "e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8",
  "decision": "ADMIT",
  "metadata": {
    "templateName": "api-reference",
    "itemsRendered": 4,
    "determinismLevel": "strict"
  },
  "dependencies": ["b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5"]
}
```
````

### 9.2 Example 2: Tutorial with Queries

**File**: `examples/tutorial-query.kgcmd`

````markdown
---
o_hash: 'f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9'
policy_id: '660e8400-e29b-41d4-a716-446655440001'
receipts:
  - 'a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0'
  - 'b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1'
bounds:
  maxQueries: 200
  maxRuntime: 10000
  maxFileScans: 10
views:
  - 'tutorial'
  - 'how-to'
sources:
  - path: 'test-data/sample-graph.ttl'
    lineStart: 1
    lineEnd: 500
    hash: 'c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2'
version: '1.0.0'
createdAt: '2025-12-26T11:00:00Z'
lastProved: '2025-12-26T15:30:00Z'
tags:
  - 'tutorial'
  - 'sparql'
  - 'knowledge-graph'
---

# SPARQL Query Tutorial

## Introduction

This tutorial teaches you how to query RDF knowledge graphs using SPARQL.
We'll use a sample social network graph with people and relationships.

## Sample Data

Our knowledge graph contains:

- 50 people (using FOAF vocabulary)
- 120 friendship relationships
- Employment information for 35 people

## Example 1: Finding Friends

Let's find all of Alice's friends:

```kgc:query
{
  "receiptId": "a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0",
  "expectedOutputFormat": "json",
  "determinismLevel": "strict",
  "metadata": {
    "queryType": "sparql",
    "resultBounds": {
      "minResults": 0,
      "maxResults": 100
    },
    "timeout": 5000,
    "description": "Find all friends of Alice"
  }
}
---
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/people/>

SELECT ?friendName WHERE {
  ex:alice foaf:knows ?friend .
  ?friend foaf:name ?friendName .
}
ORDER BY ?friendName
```

**Expected Results:**

<!-- kgc:dynamic section="alice-friends" receiptId="a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0" -->

```json
[
  { "friendName": "Bob Smith" },
  { "friendName": "Carol Jones" },
  { "friendName": "Dave Wilson" },
  { "friendName": "Eve Brown" }
]
```

Alice has **4 friends** in the graph.

<!-- /kgc:dynamic -->

## Example 2: Finding Common Friends

Now let's find people who are friends with both Alice and Bob:

```kgc:query
{
  "receiptId": "b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1",
  "expectedOutputFormat": "json",
  "determinismLevel": "strict",
  "metadata": {
    "queryType": "sparql",
    "resultBounds": {
      "minResults": 0,
      "maxResults": 50
    },
    "timeout": 5000,
    "description": "Find mutual friends of Alice and Bob"
  }
}
---
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX ex: <http://example.org/people/>

SELECT ?mutualFriendName WHERE {
  ex:alice foaf:knows ?mutualFriend .
  ex:bob foaf:knows ?mutualFriend .
  ?mutualFriend foaf:name ?mutualFriendName .
}
ORDER BY ?mutualFriendName
```

**Expected Results:**

<!-- kgc:dynamic section="mutual-friends" receiptId="b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1" -->

```json
[{ "mutualFriendName": "Carol Jones" }, { "mutualFriendName": "Dave Wilson" }]
```

Alice and Bob have **2 mutual friends**.

<!-- /kgc:dynamic -->

## Key Takeaways

1. **Use prefixes** to make queries readable
2. **ORDER BY** ensures deterministic results
3. **Triple patterns** match graph structure
4. **Variables** (e.g., `?friend`) bind to values

## Next Steps

- Try modifying queries to find different relationships
- Explore FILTER clauses for conditional matching
- Learn about OPTIONAL patterns for partial matches

## Proof of Correctness

All queries in this tutorial have been verified:

```kgc:proof
{
  "receiptId": "c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2",
  "expectedOutputFormat": "json",
  "determinismLevel": "strict",
  "metadata": {
    "proofType": "sequential",
    "verifyChain": true,
    "validateSignatures": false,
    "description": "Verify all tutorial query receipts"
  }
}
---
{
  "receiptIds": [
    "a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0",
    "b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1"
  ],
  "expectedRoot": "d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3"
}
```

---

## Appendix: Query Receipts

### Receipt a9b0c1d2... (Alice's Friends Query)

```json
{
  "id": "a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0",
  "timestamp": "2025-12-26T15:15:00Z",
  "o_hash": "f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9",
  "block_type": "kgc:query",
  "input_hash": "b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1",
  "output_hash": "c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2",
  "decision": "ADMIT",
  "metadata": {
    "queryType": "sparql",
    "executionTime": 125,
    "resultCount": 4,
    "determinismLevel": "strict"
  }
}
```

### Receipt b0c1d2e3... (Mutual Friends Query)

```json
{
  "id": "b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1",
  "timestamp": "2025-12-26T15:20:00Z",
  "o_hash": "f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9",
  "block_type": "kgc:query",
  "input_hash": "c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2",
  "output_hash": "d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3",
  "decision": "ADMIT",
  "metadata": {
    "queryType": "sparql",
    "executionTime": 185,
    "resultCount": 2,
    "determinismLevel": "strict"
  }
}
```
````

---

## 10. Implementation Notes

### 10.1 Parser Architecture

**Recommended Approach**: Multi-stage pipeline

1. **YAML Parsing**: Parse frontmatter with `js-yaml`
2. **Schema Validation**: Validate frontmatter with zod schema
3. **Markdown Parsing**: Parse content with `remark` or `markdown-it`
4. **Block Extraction**: Extract `kgc:*` blocks via regex/AST
5. **Block Validation**: Validate each block metadata + body
6. **Receipt Loading**: Load receipts from store
7. **Hash Verification**: Verify all receipt hashes
8. **Dynamic Section Matching**: Match sections to receipt outputs

### 10.2 Execution Engine

**Components**:

- **Query Executor**: Runs SPARQL/N3/SHACL queries
- **Extract Engine**: Parses source files, extracts exports
- **Render Engine**: Applies templates to JSON data
- **Proof Verifier**: Checks receipt chains and Merkle proofs

**Execution Flow**:

1. Parse block metadata
2. Check resource bounds
3. Execute block (with timeout)
4. Canonicalize output
5. Hash output
6. Create receipt
7. Store receipt

### 10.3 Receipt Storage

**Options**:

- **File-based**: `receipts/<receipt_id>.json`
- **Database**: SQLite, PostgreSQL (indexed by id, o_hash, timestamp)
- **Content-addressed**: IPFS, Git blob storage

**Required Queries**:

- Get receipt by ID
- List receipts by `o_hash`
- Find dependent receipts (reverse dependency lookup)

### 10.4 Hashing Strategy

**Library**: `hash-wasm` (fast, WASM-based)

```javascript
import { sha256 } from 'hash-wasm';
import { canonicalize } from 'json-canonicalize';

async function hashJSON(obj) {
  const canonical = canonicalize(obj);
  return await sha256(canonical);
}

async function hashMarkdown(text) {
  // Normalize line endings, trim trailing whitespace
  const normalized = text
    .replace(/\r\n/g, '\n')
    .split('\n')
    .map(line => line.trimEnd())
    .join('\n');
  return await sha256(normalized);
}
```

### 10.5 Testing Strategy

**Unit Tests**:

- Frontmatter parsing and validation
- Each block type parser
- Hash computation (canonical JSON)
- Receipt verification logic

**Integration Tests**:

- End-to-end document processing
- Receipt chain validation
- Merkle proof verification
- Error handling scenarios

**Property-Based Tests**:

- Determinism: same input → same output → same hash
- Idempotence: validate(validate(doc)) = validate(doc)
- Commutativity: receipt order doesn't affect validation

### 10.6 Performance Targets

- **Parse frontmatter**: <10ms
- **Parse full document**: <100ms (5000 line doc)
- **Execute kgc:query**: <5000ms (configurable via bounds)
- **Execute kgc:extract**: <3000ms (50 files)
- **Verify receipt chain**: <50ms (10 receipts)
- **Merkle proof verification**: <5ms

### 10.7 Security Considerations

1. **SPARQL Injection**: Sanitize query inputs, use parameterized queries
2. **Path Traversal**: Validate file paths, prevent `../` in sources
3. **DoS via Bounds**: Enforce strict resource limits
4. **Receipt Tampering**: Verify cryptographic hashes
5. **Dependency Confusion**: Validate receipt dependency chains

### 10.8 Extensibility

**Future Block Types**:

- `kgc:validate`: Run SHACL validation, emit violation reports
- `kgc:transform`: Apply N3 rules, generate new triples
- `kgc:benchmark`: Run performance tests, emit metrics
- `kgc:diagram`: Generate Mermaid/PlantUML diagrams

**Plugin System**:

- Custom templates for `kgc:render`
- Custom extractors for different languages (Python, Rust)
- Custom proof types (ZK-SNARKs, threshold signatures)

---

## Conclusion

This specification defines a complete, implementation-ready format for **verifiable documentation** combining:

1. **Static content** (prose)
2. **Dynamic content** (queries, extractions, renderings)
3. **Cryptographic receipts** (proof of correctness)
4. **Resource bounds** (prevent runaway computation)
5. **Multi-view support** (Diátaxis framework)

**Key Properties**:

- ✅ Deterministic reproduction
- ✅ Cryptographic verification
- ✅ Resource-bounded execution
- ✅ Source traceability
- ✅ Error transparency

**Next Steps**:

1. Implement zod schemas (see `schemas/kgc-markdown.mjs`)
2. Build parser pipeline
3. Create execution engine for block types
4. Implement receipt storage and verification
5. Write comprehensive test suite
6. Document plugin API for extensions

---

**Document Hash**: `<to be computed after finalization>`
**License**: CC-BY-SA 4.0
**Maintainer**: UNRDF Project
