# KGC Markdown Hooks - Poka-Yoke Error-Proofing System

## Overview

This directory contains the hooks implementation for Dynamic KGC Markdown that enforces "poka-yoke" (error-proofing) rules for documentation governance. The system ensures that all documentation maintains cryptographic receipts, universe state hashes, and computational bounds.

## Architecture

```
.claude/hooks/
├── hooks-config.yaml           # Hook registry and configuration
├── hooks-shared.mjs            # Shared utilities (300 lines)
├── on-write-docs-hook.mjs      # Write validation (300 lines)
├── on-edit-docs-hook.mjs       # Edit validation (250 lines)
├── on-bounds-exceeded-hook.mjs # Bounds checking (350 lines)
├── on-non-determinism-hook.mjs # Determinism validation (300 lines)
├── hooks.test.mjs              # Comprehensive test suite
└── README.md                   # This file
```

**Total: ~1,500 lines of production-ready code**

## Hooks

### 1. on-write-docs-hook.mjs

**Trigger**: Write tool attempts to write to `docs/**/*.md` or `docs/**/*.kgcmd`

**Rules**:

- Document must have valid frontmatter with `o_hash`, `policy_id`, and `receipts` array
- Each receipt ID in frontmatter must exist in `receipts/` directory
- DENY if missing frontmatter/receipts; suggest `/kgc:prove`

**Example Frontmatter**:

```yaml
---
o_hash: 1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef
policy_id: kgc-policy-v1
receipts: [receipt-123, receipt-456]
---
```

**Denial Receipt**:

```json
{
  "id": "uuid-v4",
  "timestamp": "2025-12-26T10:30:45Z",
  "operation": "write",
  "targetPath": "/home/user/unrdf/docs/example.md",
  "reasonCode": "MISSING_FRONTMATTER",
  "message": "Document write denied: Missing frontmatter block",
  "remediation": {
    "command": "/kgc:prove",
    "steps": [
      "Add valid frontmatter to the document",
      "Or use /kgc:prove to generate receipts automatically"
    ],
    "documentation": "docs/chapter-5-receipts-chaining-verification.md"
  }
}
```

### 2. on-edit-docs-hook.mjs

**Trigger**: Edit tool modifies content (not frontmatter) in `docs/**`

**Rules**:

- If content changes, `o_hash` must be updated to reflect new universe snapshot
- Validate new `o_hash` matches declared universe state (via receipts)
- DENY if `o_hash` unchanged after content edit; suggest `/kgc:refresh`

**Example**:

```markdown
## <!-- BEFORE -->

## o_hash: old-hash-value

Old content

## <!-- AFTER (DENIED) -->

## o_hash: old-hash-value # ❌ Unchanged!

New content

## <!-- AFTER (ALLOWED) -->

## o_hash: new-hash-value # ✅ Updated!

New content
```

### 3. on-bounds-exceeded-hook.mjs

**Trigger**: `kgc:query`, `kgc:proof`, or `kgc:render` about to execute

**Rules**:

- Bounds in frontmatter (`maxQueries`, `maxRuntime`, `maxFileScans`) must not be exceeded
- Predict cost before execution (query cardinality, file count, estimated runtime)
- DENY if bounds exceeded; suggest increasing bounds with `/kgc:refresh`

**Default Bounds** (Andon Principle):

```yaml
maxQueries: 100
maxRuntime: 5000 # 5 seconds
maxFileScans: 1000
```

**Cost Estimation**:

- **ASK queries**: 1 result, ~10ms
- **COUNT queries**: 1 result, ~100ms
- **SELECT with LIMIT**: Limited cardinality, ~50ms + 20ms per join
- **SELECT \***: High cardinality (~1000), ~100ms + filters

**Example Denial**:

```
Computational bounds exceeded: maxRuntime: 250ms > 10ms (+240ms)

Options:
1. Increase bounds in frontmatter
2. Optimize query (add LIMIT, reduce joins)
3. Use /kgc:refresh to regenerate with updated bounds
```

### 4. on-non-determinism-hook.mjs

**Trigger**: After `kgc:query`, `kgc:proof`, `kgc:render`, or `kgc:dynamic` execution

**Rules**:

- Output must be deterministic (stable ordering, no timestamps, no randomness)
- Compare current output hash with expected hash from receipt
- WARN if hash mismatch; suggest debugging steps

**Non-Deterministic Patterns Detected**:

- ISO timestamps: `2025-12-26T10:30:45Z`
- Unix timestamps: `1735210245123`
- Random numbers: `Math.random()`, `crypto.randomUUID()`
- Process IDs: `process.pid`
- Unstable ordering: `Object.keys()`, `for...in`

**Example Warning**:

```
Warning: Non-deterministic output detected

Output contains non-deterministic patterns:
  - Pattern: \d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2} at position 234

Output hash mismatch:
  Expected: aaaaaaa...
  Actual:   bbbbbbb...

Debugging steps:
1. Review output for timestamp patterns
2. Check for Math.random() or crypto.randomUUID()
3. Ensure SPARQL results are sorted (ORDER BY)
4. Verify JSON key ordering is stable
```

## Shared Utilities (hooks-shared.mjs)

### Frontmatter Parsing

```javascript
import { extractFrontmatter, validateFrontmatter } from './hooks-shared.mjs';

const { frontmatter, content } = extractFrontmatter(markdown);
const validation = validateFrontmatter(frontmatter);

if (!validation.valid) {
  console.error(validation.error);
}
```

### Receipt Validation

```javascript
import { checkReceiptExists, validateReceipts } from './hooks-shared.mjs';

const { exists, path } = await checkReceiptExists('receipt-123');
const validation = await validateReceipts(['receipt-123', 'receipt-456']);

if (!validation.valid) {
  console.error('Missing:', validation.missing);
  console.error('Invalid:', validation.invalid);
}
```

### Denial Receipt Generation

```javascript
import { generateDenialReceipt } from './hooks-shared.mjs';

const receipt = await generateDenialReceipt({
  operation: 'write',
  targetPath: '/home/user/unrdf/docs/example.md',
  reasonCode: 'MISSING_FRONTMATTER',
  message: 'Document write denied',
  details: { errors: [...] },
  remediation: {
    command: '/kgc:prove',
    steps: ['Step 1', 'Step 2'],
    documentation: 'docs/chapter-5.md',
  },
});

// Receipt written to: receipts/denials/{uuid}-{reasonCode}.json
```

### Content Hashing

```javascript
import { computeContentHash } from './hooks-shared.mjs';

const hash = await computeContentHash('content');
// Returns: 64-character BLAKE3 hex hash
```

## Configuration

See `hooks-config.yaml` for complete configuration options.

### Enable/Disable Hooks

```yaml
hooks:
  - name: on-write-docs
    enabled: true # Set to false to disable
```

### Adjust Bounds

```yaml
hooks:
  - name: on-bounds-exceeded
    config:
      defaultBounds:
        maxQueries: 100
        maxRuntime: 5000
        maxFileScans: 1000
```

### Warn-Only Mode

```yaml
hooks:
  - name: on-non-determinism
    config:
      warnOnly: true # Only warn, don't deny
```

## Testing

Run the comprehensive test suite:

```bash
# Run all tests
node --test .claude/hooks/hooks.test.mjs

# Run specific test suite
node --test --test-name-pattern="on-write-docs" .claude/hooks/hooks.test.mjs
```

**Test Coverage**:

- ✅ on-write-docs: 7 tests (happy path + 6 error cases)
- ✅ on-edit-docs: 5 tests (happy path + 4 error cases)
- ✅ on-bounds-exceeded: 5 tests (happy path + 4 error cases)
- ✅ on-non-determinism: 6 tests (happy path + 5 error cases)
- ✅ Shared utilities: 3 tests

**Total: 26 tests**

## Integration with Claude Code

Hooks are automatically executed by Claude Code when configured in `.claude/hooks/`.

### Hook Execution Flow

```
1. User action (Write/Edit tool, kgc:query, etc.)
   ↓
2. Claude Code detects matching hook trigger
   ↓
3. Hook execute() function called with context
   ↓
4. Hook validates operation
   ↓
5a. If allowed: proceed with operation
5b. If denied: generate denial receipt, show remediation
```

### Context Structure

```javascript
{
  tool: 'Write',
  params: {
    file_path: '/home/user/unrdf/docs/example.md',
    content: '...'
  }
}
```

### Return Values

```javascript
// Allow
{ proceed: true }

// Deny
{
  proceed: false,
  message: 'Validation failed: ...',
  receipt: { id: '...', ... }
}

// Warn
{
  proceed: true,
  message: 'Warning: ...',
  receipt: { id: '...', ... }
}
```

## Denial Receipts

All denials are logged as receipts in `receipts/denials/`:

```
receipts/denials/
├── uuid-1-MISSING_FRONTMATTER.json
├── uuid-2-O_HASH_UNCHANGED.json
├── uuid-3-BOUNDS_EXCEEDED.json
└── uuid-4-HASH_MISMATCH.json
```

**Receipt Structure** (see `DenialReceiptSchema` in hooks-shared.mjs):

- `id`: UUID v4
- `timestamp`: ISO 8601
- `operation`: write, edit, query, proof, render
- `targetPath`: File path
- `reasonCode`: Structured error code
- `message`: Human-readable message
- `details`: Additional context
- `remediation`: Steps to fix + documentation link
- `gitContext`: Branch, commit, uncommitted changes

## Remediation Commands

| Hook               | Command        | Purpose                           |
| ------------------ | -------------- | --------------------------------- |
| on-write-docs      | `/kgc:prove`   | Generate receipts and frontmatter |
| on-edit-docs       | `/kgc:refresh` | Update o_hash after content edit  |
| on-bounds-exceeded | `/kgc:refresh` | Regenerate with increased bounds  |
| on-non-determinism | N/A            | Debug output for determinism      |

## Performance

All hooks follow the **Andon Principle**:

- **Max hook execution time**: 1 second
- **Timeout action**: warn
- **Default maxRuntime**: 5 seconds (SLA)

## Reason Codes

```javascript
MISSING_FRONTMATTER; // No frontmatter block
INVALID_FRONTMATTER; // Schema validation failed
MISSING_O_HASH; // o_hash field missing
MISSING_POLICY_ID; // policy_id field missing
MISSING_RECEIPTS; // receipts field missing
RECEIPT_NOT_FOUND; // Receipt ID not found
INVALID_RECEIPT; // Receipt malformed
O_HASH_UNCHANGED; // Content edit without hash update
CONTENT_CHANGED_WITHOUT_HASH; // Alias for O_HASH_UNCHANGED
BOUNDS_EXCEEDED; // Any bound exceeded
MAX_QUERIES_EXCEEDED; // Query count exceeded
MAX_RUNTIME_EXCEEDED; // Runtime exceeded
MAX_FILE_SCANS_EXCEEDED; // File scan count exceeded
NON_DETERMINISTIC_OUTPUT; // Output not deterministic
HASH_MISMATCH; // Output hash doesn't match expected
```

## Documentation References

- **Receipt Theory**: `docs/chapter-5-receipts-chaining-verification.md`
- **Andon Signals**: `docs/ANDON-SIGNALS-INDEX.md`
- **Big Bang 80/20**: `docs/bb80-20-methodology.md`

## Implementation Notes

### Why BLAKE3?

- **Collision-resistant**: Cryptographically secure
- **Fast**: ~3 GB/s on modern hardware
- **Deterministic**: Same input = same hash
- **Fixed length**: 64-character hex (256 bits)

### Why Poka-Yoke?

"Mistake-proofing" from lean manufacturing. Design systems that prevent errors rather than detect them.

**Example**: o_hash validation prevents content changes without universe state updates, maintaining cryptographic chain integrity.

### Why Receipts?

Receipts provide **tamper-evident audit trails** without exposing internal state. External verifiers can validate governance without database access.

## Contributing

When adding new hooks:

1. Implement in new file: `on-{trigger}-hook.mjs`
2. Add configuration to `hooks-config.yaml`
3. Add tests to `hooks.test.mjs`
4. Update this README
5. Ensure <500 lines per hook
6. Follow denial receipt schema

## License

Part of UNRDF KGC-4D project. See LICENSE in repository root.
