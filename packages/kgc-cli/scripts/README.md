# Build and Utility Scripts

**Purpose**: Development and deployment scripts for LaTeX pipeline.

## Scripts

Future scripts:
- `setup-swiftlatex.mjs` - Download and verify SwiftLaTeX WASM binaries
- `verify-integration.mjs` - End-to-end pipeline verification
- `benchmark-compilation.mjs` - Performance benchmarking
- `generate-fixtures.mjs` - Auto-generate test fixtures
- `cache-warmup.mjs` - Pre-populate cache with common packages

## Example: setup-swiftlatex.mjs

```javascript
// Download SwiftLaTeX WASM binaries and verify integrity
import { promises as fs } from 'node:fs';
import { createHash } from 'node:crypto';

const SWIFTLATEX_VERSION = '1.0.0';
const VENDOR_DIR = '../vendor/swiftlatex';

async function downloadEngine(engine, url, expectedHash) {
  // 1. Download WASM file
  // 2. Verify SHA-256 hash
  // 3. Save to vendor/swiftlatex/
  // 4. Update LICENSES/
}

async function main() {
  await downloadEngine('xetex', 'https://...', '...');
  await downloadEngine('pdftex', 'https://...', '...');
  console.log('✅ SwiftLaTeX engines installed');
}

main();
```

## Running Scripts

```bash
# From packages/kgc-cli/
node scripts/setup-swiftlatex.mjs
node scripts/verify-integration.mjs
node scripts/benchmark-compilation.mjs --fixture=thesis
```

## Integration with package.json

Scripts can be added to `package.json`:

```json
{
  "scripts": {
    "setup:wasm": "node scripts/setup-swiftlatex.mjs",
    "verify:pipeline": "node scripts/verify-integration.mjs"
  }
}
```
