# KGC Runtime API Stability and Deprecation Policy

## Current Version

**API Version**: 5.0.1 (Beta)

**Status**: Beta - API is stable but may have minor changes before 5.1.0 stable release

## Semantic Versioning

KGC Runtime follows [Semantic Versioning 2.0.0](https://semver.org/):

```
MAJOR.MINOR.PATCH

5.0.1
│ │ └─ Patch: Backward compatible bug fixes
│ └─── Minor: Backward compatible new features
└───── Major: Breaking changes
```

### Version Guarantees

- **Patch (x.x.PATCH)**: Bug fixes, no API changes
- **Minor (x.MINOR.x)**: New features, fully backward compatible
- **Major (MAJOR.x.x)**: Breaking changes, may require migration

## Deprecation Policy

### Overview

To ensure smooth transitions and maintain stability, we follow a strict deprecation policy:

**Rule**: APIs are deprecated for **minimum 2 releases** before removal

**Minimum Deprecation Period**: 90 days

### Deprecation Process

#### 1. Announcement (Release N)

API feature is marked as deprecated:

```javascript
/**
 * @deprecated Since v5.0.0. Use newFunction() instead.
 * Will be removed in v7.0.0 (March 2025)
 */
export function oldFunction() {
  console.warn('[DEPRECATION] oldFunction is deprecated. Use newFunction instead.');
  // ... implementation continues to work
}
```

#### 2. Warning Period (Releases N+1, N+2)

Deprecated APIs continue to function but emit warnings:

```
[DEPRECATION WARNING] oldFunction is deprecated since v5.0.0.
Use newFunction instead. Removal scheduled for v7.0.0 (March 2025).
```

#### 3. Removal (Release N+3 or later)

After minimum 90 days AND 2 releases, API is removed:

```javascript
// v7.0.0 - Function removed
// Import will fail
// Migration guide: docs/migration/v5-to-v7.md
```

## API Stability Levels

### Stable

- **Guarantee**: No breaking changes in minor/patch versions
- **Deprecation**: Minimum 2 releases + 90 days notice
- **Examples**: Core schemas, receipt generation, work items

```javascript
// Stable API - Won't change in 5.x.x
import { ReceiptSchema } from '@unrdf/kgc-runtime/schemas';
```

### Beta

- **Guarantee**: API shape is stable but may have additions
- **Changes**: New fields/methods can be added in minor versions
- **Deprecation**: Same as stable (2 releases + 90 days)
- **Examples**: Plugin system (v5.0.1), API versioning

```javascript
// Beta API - Stable but may expand
import { PluginManager } from '@unrdf/kgc-runtime/plugin-manager';
```

### Experimental

- **Guarantee**: None - can change or be removed at any time
- **Usage**: Not recommended for production
- **Warning**: Clearly marked in documentation

```javascript
// Experimental API - Use at your own risk
// @experimental
import { ExperimentalFeature } from '@unrdf/kgc-runtime/experimental';
```

## Version Compatibility

### Plugin API Version Checking

Plugins must declare their required API version:

```json
{
  "name": "my-plugin",
  "api_version": "5.0.1"
}
```

Runtime validates compatibility:

```javascript
import { validatePluginVersion } from '@unrdf/kgc-runtime/api-version';

try {
  validatePluginVersion('5.0.0'); // OK - compatible
  validatePluginVersion('4.0.0'); // WARNING - deprecated
  validatePluginVersion('3.0.0'); // ERROR - removed
} catch (error) {
  console.error('Incompatible plugin version:', error.message);
}
```

### Compatibility Rules

| Plugin API | Runtime 5.0.1 | Status |
|-----------|---------------|---------|
| 5.0.1 | ✅ Compatible | Exact match |
| 5.0.0 | ✅ Compatible | Patch difference OK |
| 4.9.9 | ⚠️ Deprecated | Works but warns |
| 3.x.x | ❌ Removed | Error thrown |

## Migration Guides

When breaking changes occur, we provide comprehensive migration guides:

### Example: v4 to v5 Migration

**Location**: `docs/migration/v4-to-v5.md`

**Contents**:
1. Breaking changes summary
2. Step-by-step migration instructions
3. Code examples (before/after)
4. Automated migration scripts
5. Testing recommendations

```javascript
// v4 (deprecated)
const receipt = generateReceipt(op, data);

// v5 (current)
const receipt = await generateReceipt(op, inputs, outputs, parentHash);
```

## Deprecation Timeline

### Current Deprecations (v5.0.1)

| Feature | Deprecated | Removal | Alternative |
|---------|-----------|---------|-------------|
| `WorkItem.metadata` direct mutation | v5.0.0 | v7.0.0 | Use `updateMetadata()` method |
| `Receipt.hash` MD5 | v4.0.0 | v6.0.0 | Now using BLAKE3 (automatic) |

### Upcoming Changes (Planned)

| Feature | Target Version | Type | Description |
|---------|---------------|------|-------------|
| Plugin hot-reload | v5.1.0 | Addition | Reload plugins without restart |
| WASM plugin support | v5.2.0 | Addition | Run plugins in WASM sandbox |
| GraphQL API | v6.0.0 | Addition | Query runtime via GraphQL |

## Stability Guarantees by Module

### Core Runtime (Stable)

- ✅ `schemas.mjs` - All core schemas
- ✅ `receipt.mjs` - Receipt generation/validation
- ✅ `work-item.mjs` - Work item executor
- ✅ `bounds.mjs` - Bounds checking
- ✅ `capsule.mjs` - Run capsule management

### Plugin System (Beta)

- 🔶 `plugin-manager.mjs` - Plugin lifecycle
- 🔶 `plugin-isolation.mjs` - Capability security
- 🔶 `api-version.mjs` - Version management

### Governance (Beta)

- 🔶 `admission-gate.mjs` - Policy enforcement
- 🔶 `freeze-restore.mjs` - State snapshots
- 🔶 `merge.mjs` - State merging

## Breaking Change Policy

### What Constitutes a Breaking Change?

**Breaking** (requires major version):
- Removing exported function/class
- Changing function signature (params, return type)
- Changing schema validation rules (stricter)
- Removing schema fields
- Changing default behavior

**Not Breaking** (minor/patch version):
- Adding new function/class
- Adding optional parameters
- Adding schema fields (optional)
- Relaxing validation rules
- Internal implementation changes
- Performance improvements

### Example Scenarios

```javascript
// ❌ BREAKING - Requires v6.0.0
// v5: generateReceipt(op, data)
// v6: generateReceipt(op, inputs, outputs) // Changed signature

// ✅ NOT BREAKING - Can be v5.1.0
// v5.0: generateReceipt(op, inputs, outputs)
// v5.1: generateReceipt(op, inputs, outputs, options = {}) // Added optional param
```

## Staying Updated

### Subscribe to Changes

1. **GitHub Releases**: Watch the repository
2. **Changelog**: Check `CHANGELOG.md` for each release
3. **Migration Alerts**: Run `npm outdated` for deprecation warnings

### Automated Checks

Add to your CI/CD:

```bash
# Check for deprecated API usage
npm run check-deprecations

# Validate plugin compatibility
npm run validate-plugin-version
```

## Questions?

- **Deprecation Questions**: Open an issue with tag `deprecation`
- **Migration Help**: Check `docs/migration/` or ask in Discussions
- **Breaking Change Proposals**: RFC process in GitHub Discussions

---

**Last Updated**: 2024-12-27
**Next Review**: 2025-03-01 (with v5.1.0 release)
