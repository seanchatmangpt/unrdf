# Quick Start - Manual Sync Test

## TL;DR

```bash
# 1. Install dependencies (first time only)
cd /home/user/unrdf
pnpm install

# 2. Run the test
node packages/cli/test/manual/test-sync.mjs

# Expected: ✓ ALL TESTS PASSED (exit code 0)
```

## What It Does

Tests the complete sync pipeline: **Config → Ontology → SPARQL → Template → Output**

## Test Data (Inline)

- **Ontology**: User + Post entities (Turtle format)
- **Template**: Entity constants generator (Nunjucks)
- **Config**: ggen.toml with one generation rule
- **Query**: Extract owl:Class instances

## Verification (8 Checks)

1. Output file exists
2. JSDoc header
3. Generated timestamp
4. Entity count
5. USER constant
6. POST constant
7. ALL_ENTITIES array
8. Correct URIs

## Success Output

```
✓ Sync completed successfully in XXXms
✓ ALL TESTS PASSED
```

## Failure Output

```
✗ [specific check that failed]
✗ SOME TESTS FAILED
```

## Files

| File | Purpose |
|------|---------|
| `test-sync.mjs` | Main test script (run this) |
| `README.md` | Full documentation |
| `SUMMARY.md` | Detailed creation summary |
| `QUICKSTART.md` | This file |

## Troubleshooting

**"Error: Dependencies not installed"**
```bash
cd /home/user/unrdf && pnpm install
```

**Test fails**
- Check `packages/cli/src/cli/commands/sync/orchestrator.mjs` exists
- Verify `@unrdf/core` is installed
- Ensure temp directory is writable

## Advanced

**Run with trace**
```bash
NODE_OPTIONS='--trace-warnings' node packages/cli/test/manual/test-sync.mjs
```

**Keep temp files (for debugging)**
Edit test-sync.mjs, comment out cleanup section:
```javascript
// await rm(testDir, { recursive: true, force: true });
console.log(`Temp dir preserved: ${testDir}`);
```

**Custom test data**
Edit these constants in test-sync.mjs:
- `TEST_ONTOLOGY` - Your Turtle ontology
- `TEST_TEMPLATE` - Your Nunjucks template
- `SPARQL_QUERY` - Your SPARQL query

---

**Runtime**: < 5 seconds
**Dependencies**: pnpm install (one-time)
**Exit codes**: 0 = pass, 1 = fail
**Temp files**: Auto-cleaned
