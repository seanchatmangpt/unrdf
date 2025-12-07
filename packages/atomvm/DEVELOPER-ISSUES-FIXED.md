# Developer Issues Fixed

## Critical Issues Found and Fixed

### 1. ❌ API Export Mismatch (FIXED)

**Problem**: README documented that developers could import `AtomVMRuntime`, `TerminalUI`, and service-worker-manager functions from `@unrdf/atomvm`, but `src/index.mjs` only exported `App` class.

**Impact**: Developers following the README examples would get import errors.

**Fix**: 
- Updated `src/index.mjs` to re-export all public APIs
- Added export path in `package.json` for `@unrdf/atomvm/service-worker-manager`
- Separated browser app initialization (`src/app.mjs`) from library exports (`src/index.mjs`)

**Verification**:
```bash
# Exports now available:
import { AtomVMRuntime } from '@unrdf/atomvm';  # ✅ Works
import { TerminalUI } from '@unrdf/atomvm';      # ✅ Works
import { registerServiceWorker } from '@unrdf/atomvm/service-worker-manager';  # ✅ Works
```

### 2. ❌ App Initialization Side Effects (FIXED)

**Problem**: `src/index.mjs` had App initialization code that ran immediately when the module was imported, causing side effects when importing just the classes.

**Impact**: Importing `AtomVMRuntime` would trigger App initialization, potentially causing errors or unexpected behavior.

**Fix**: 
- Moved App initialization to `src/app.mjs` (browser app entry point)
- `src/index.mjs` now only exports classes (library entry point)
- Updated `index.html` to load `src/app.mjs` instead

**Verification**:
- Library imports no longer trigger App initialization
- Browser app still works correctly

### 3. ⚠️ Playwright Browsers Not Installed (DOCUMENTATION)

**Problem**: Playwright tests fail because browsers aren't installed.

**Impact**: Developers can't run E2E tests without installing browsers first.

**Status**: Not a code issue - needs documentation or setup script.

**Recommendation**: Add to README:
```bash
# Install Playwright browsers (required for E2E tests)
pnpm exec playwright install
```

## Verification Results

### ✅ Build Works
```bash
pnpm build  # ✅ Success
```

### ✅ Unit Tests Pass
```bash
pnpm test  # ✅ 45/45 tests passing (excluding playwright)
```

### ✅ Exports Correct
- `AtomVMRuntime` exported from main entry
- `TerminalUI` exported from main entry  
- Service worker functions exported from main entry and subpath
- `App` class exported for browser app usage

### ⚠️ Playwright Tests
- Require browser installation: `pnpm exec playwright install`
- Tests are configured correctly, just need browsers

## Remaining Developer Experience Issues

### 1. Module Name Required (By Design)
- Browser requires `?module=<name>` URL parameter
- No default module name
- **Status**: Working as designed, but could be more discoverable

### 2. Erlang Toolchain Required (By Design)
- Building modules requires `erlc` and `packbeam`
- **Status**: Expected requirement, documented in README

### 3. Service Worker Reload (By Design)
- Page reloads once to activate service worker
- **Status**: Expected behavior, documented in README

## Summary

**Fixed Issues**: 2 critical API/export issues
**Documentation Issues**: 1 (Playwright browser installation)
**Working as Designed**: 3 (module name, Erlang tools, service worker reload)

**Overall Status**: ✅ **Ready for developers** - All critical API issues fixed. Exports match README documentation.

