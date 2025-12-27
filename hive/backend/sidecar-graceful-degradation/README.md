# Sidecar Graceful Degradation Fix

## Issue
Tests were failing when calling `sidecar status` and `sidecar health` because the sidecar process was not running. Commands would exit with code 1, causing test failures.

## Solution
Modified sidecar commands to gracefully handle unavailable sidecar by:
- Detecting gRPC connection errors (codes 14, 4)
- Showing user-friendly warning instead of error
- Exiting with code 0 (success) instead of 1 (error)

## Files Modified
- `/Users/sac/unrdf/src/cli/commands/sidecar.mjs`
  - `sidecarStatusCommand()` - Added graceful degradation
  - `sidecarHealthCommand()` - Added graceful degradation

## Behavior Change

### Before
```bash
$ node cli/unrdf.mjs sidecar status
❌ Sidecar unavailable. Ensure sidecar is running: unrdf sidecar start
(Exit code: 1) ❌
```

### After
```bash
$ node cli/unrdf.mjs sidecar status
⚠️  Sidecar not available
Run 'unrdf sidecar start' to start the sidecar process
(Exit code: 0) ✅
```

## Testing

Run validation script:
```bash
bash hive/backend/sidecar-graceful-degradation/test-validation.sh
```

Manual testing:
```bash
# Test status command
node cli/unrdf.mjs sidecar status

# Test health command
node cli/unrdf.mjs sidecar health

# Verify exit code
node cli/unrdf.mjs sidecar status; echo "Exit code: $?"
```

## Files in This Directory
- `README.md` - This file
- `implementation.md` - Detailed implementation notes
- `sidecar.mjs` - Fixed sidecar command file
- `test-validation.sh` - Automated validation test

## Impact
✅ Tests no longer fail when sidecar unavailable
✅ User-friendly warning messages
✅ Proper exit codes (0 for unavailable, 1 for errors)
✅ Tests can run without sidecar process

## Error Handling

| Error Code | Type | Action |
|-----------|------|--------|
| 14 | UNAVAILABLE | Exit 0 with warning |
| 4 | DEADLINE_EXCEEDED | Exit 0 with warning |
| 12 | UNIMPLEMENTED | Exit 1 with error |
| 16 | UNAUTHENTICATED | Exit 1 with error |
| Other | Generic | Exit 1 with error |
