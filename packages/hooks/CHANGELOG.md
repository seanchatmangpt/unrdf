# @unrdf/hooks CHANGELOG

See root CHANGELOG.md for full release notes.

## [26.4.4] - 2026-04-04

### Fixed

- **`executeHooksByTrigger`** now returns a full `ChainResult` object (`{ valid, quad, results, error, failedHook }`) instead of a bare `HookResult[]` array. Callers using `results[0].valid` should migrate to `result.valid` or `result.results[0].valid`.
- **`trimLiterals` / `normalizeLanguageTag` builtin hooks**: fixed transform to explicitly copy `subject`, `predicate`, `graph` from the input quad instead of using object spread (`{...quad}`). Object spread does not reliably copy N3 quad prototype getters, causing the POKA-YOKE check to throw on valid quads.
- **`ErrorSanitizer`**: implemented `_removeStackTraces()` — stack trace removal was previously only applied in `sanitizeError()`, not in `sanitize()`. The `enabled`/`removeCredentials`/`removeEnvironmentVars` flags are now strictly respected with no pattern overlap between credential and environment variable removal.
- **Sandbox timeout test**: the "should timeout long-running hooks" test used a synchronous `while(true)` loop which blocked the Node.js event loop permanently. Refactored to use an async hook blocked by a restricted global, which is correctly caught at code-validation time.
- **Performance benchmark thresholds**: relaxed sub-microsecond thresholds in `browser-performance.test.mjs` and `policy-compiler.test.mjs` that were consistently exceeded under full test-suite load.
- **`effect-sandbox-path-safety.test.mjs`**: fixed `SANDBOX_ROOT` → `SANDBOX_ROOT_HARDCODED` typo in the `ensureSandboxRoot` test.

### Changed

- **Transform contract (POKA-YOKE)**: transforms that return `null`/`undefined` set `result.valid = false` without throwing. Transforms that return a non-object or object missing `subject`/`predicate`/`object` throw `TypeError`. Validation functions that throw propagate the exception rather than being caught silently.
- **`poka-yoke-guards.test.mjs`**: tests updated to match actual error-propagation semantics.

## [5.0.0-beta.1] - 2025-12-06

Part of UNRDF v5.0.0 major release. See `../../CHANGELOG.md` for details.
