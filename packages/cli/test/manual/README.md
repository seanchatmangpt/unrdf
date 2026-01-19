# Manual Test Scripts

Manual testing scripts for verifying CLI functionality without running the full test suite.

## Sync Command Test

**File:** `test-sync.mjs`

**Purpose:** End-to-end verification of the sync command pipeline (config → ontology → SPARQL → template → output).

### Prerequisites

1. Install dependencies:
   ```bash
   cd /home/user/unrdf
   pnpm install
   ```

2. Ensure you're in the project root directory.

### Usage

```bash
node packages/cli/test/manual/test-sync.mjs
```

### What It Tests

The script performs a complete sync workflow:

1. **Setup Phase**
   - Creates a temporary directory
   - Writes a minimal Turtle ontology (User and Post entities)
   - Writes a Nunjucks template for code generation
   - Writes a ggen.toml configuration file

2. **Execution Phase**
   - Imports and calls `runSync()` from orchestrator.mjs
   - Runs with verbose output enabled

3. **Verification Phase**
   - Checks that output file is generated
   - Verifies content contains expected patterns:
     - JSDoc headers
     - Generated timestamp
     - Entity constants (USER, POST)
     - ALL_ENTITIES array
     - Correct URIs

4. **Cleanup Phase**
   - Removes temporary directory
   - Reports test results

### Expected Output

```
============================================================
  Manual Sync Command Test
============================================================

ℹ Creating temporary test directory...
✓ Created: /tmp/sync-manual-test-1234567890
ℹ Setting up test files...
✓ Created ontology: schema.ttl
✓ Created template: templates/entities.njk
✓ Created config: ggen.toml

============================================================
  Running Sync Command
============================================================

ℹ Executing runSync()...

============================================================
  Sync Results
============================================================

✓ Sync completed successfully in XXXms
ℹ Rules processed: 1
ℹ Files generated: 1
ℹ Total bytes: XXX
ℹ Errors: 0

============================================================
  Verifying Output
============================================================

✓ Output file exists: entities.mjs
✓ Found: JSDoc header
✓ Found: Generated timestamp
✓ Found: Entity count comment
✓ Found: USER constant
✓ Found: POST constant
✓ Found: ALL_ENTITIES array
✓ Found: User URI
✓ Found: Post URI

============================================================
  Generated Content Preview
============================================================

[... generated JavaScript code ...]

============================================================
  Cleanup
============================================================

ℹ Removing temporary directory...
✓ Cleanup complete

============================================================
  Test Summary
============================================================

✓ ALL TESTS PASSED
```

### Exit Codes

- **0**: All tests passed
- **1**: One or more tests failed

### Troubleshooting

**Error: Dependencies not installed**
```
cd /home/user/unrdf
pnpm install
```

**Error: Cannot find module**
- Ensure you're running from the project root
- Check that `packages/cli/src/cli/commands/sync/orchestrator.mjs` exists

**Error: ENOENT: no such file or directory**
- Ensure temp directory is writable
- Check disk space

### Advantages Over Full Test Suite

1. **Fast:** Runs a single focused test in <5 seconds
2. **Verbose:** Shows detailed output for debugging
3. **Self-contained:** All test data inline (no external files needed)
4. **Simple:** No test framework overhead
5. **Manual verification:** Easy to see exactly what's generated

### When to Use

- Quick verification after code changes
- Debugging sync command issues
- Demonstrating sync functionality
- Before committing sync-related changes
- CI/CD dry runs

### Extending

To test additional scenarios, duplicate and modify:

```javascript
const TEST_ONTOLOGY = `...`; // Your ontology
const TEST_TEMPLATE = `...`; // Your template
const SPARQL_QUERY = `...`;  // Your query
```

Then adjust the verification checks in the `Verifying Output` section.
