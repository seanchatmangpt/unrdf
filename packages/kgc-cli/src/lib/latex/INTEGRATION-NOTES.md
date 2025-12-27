# LaTeX Pipeline Integration Notes

**Agent 1 (Orchestrator/Integrator) - Complete**
**Date**: 2025-12-27

## Quick Start

### Import the main API
```javascript
import { compileLatexToPdf } from '@unrdf/kgc-cli/latex';

const pdfBytes = await compileLatexToPdf({
  inputTexPath: '/path/to/main.tex',
  projectDir: '/path/to/project',
  engine: 'xetex',
  passes: 2
});
```

### Validate inputs with schemas
```javascript
import { CompileOptionsSchema } from '@unrdf/kgc-cli/latex/schemas';

const validated = CompileOptionsSchema.parse({
  inputTexPath: '/path/to/main.tex',
  projectDir: '/path/to/project'
});
```

### Use CLI commands
```bash
# Build LaTeX document
npm run latex:build -- --input=fixtures/minimal/main.tex --output=dist/output.pdf

# Diagnose compilation issues
npm run latex:diagnose -- --input=fixtures/minimal/main.tex

# Verify WASM engines
npm run validate:wasm
```

## File Locations

### Core Modules
- **Schemas**: `/home/user/unrdf/packages/kgc-cli/src/lib/latex/schemas.mjs`
- **Main Export**: `/home/user/unrdf/packages/kgc-cli/src/lib/latex/index.mjs`
- **Orchestrator**: `/home/user/unrdf/packages/kgc-cli/src/lib/latex/compile.mjs`
- **Engine**: `/home/user/unrdf/packages/kgc-cli/src/lib/latex/swiftlatex-engine.mjs`

### Agent Modules
- **Agent 2 (VFS)**: `src/lib/latex/vfs/` - Already implemented with 62 tests passing
- **Agent 3 (Engine)**: `src/lib/latex/engine/` - Needs WASM loader implementation
- **Agent 4 (Resolver)**: `src/lib/latex/ctan-resolver.mjs` - Already implemented
- **Agent 5 (Lockfile)**: `src/lib/latex/latex-lock.mjs` - Already implemented
- **Agent 6 (Diagnostics)**: `src/lib/latex/diagnostics/` - Already implemented with 21 tests passing

### Supporting
- **Fixtures**: `/home/user/unrdf/packages/kgc-cli/fixtures/`
- **Scripts**: `/home/user/unrdf/packages/kgc-cli/scripts/`
- **WASM Vendor**: `/home/user/unrdf/packages/kgc-cli/vendor/swiftlatex/`

## Integration Status

| Agent | Module | Status | Files | Tests |
|-------|--------|--------|-------|-------|
| 1 | Schemas/Integration | ✅ Complete | schemas.mjs, index.mjs | 56 exports validated |
| 2 | VFS Collection | ✅ Complete | vfs/ | 62/62 passing |
| 3 | Engine Runner | ⚠️ Mock | swiftlatex-engine.mjs | Needs WASM binaries |
| 4 | CTAN Resolver | ✅ Complete | ctan-resolver.mjs | Implemented |
| 5 | Lockfile Manager | ✅ Complete | latex-lock.mjs | 13 tests passing |
| 6 | Diagnostics | ✅ Complete | diagnostics/ | 21/21 passing |
| 7-9 | CLI/Test/Docs | ⚠️ Partial | extensions/latex.mjs | CLI wired, needs tests |
| 10 | Orchestrator | ✅ Complete | compile.mjs | Implemented |

## What Works Now

✅ Schema validation
✅ VFS collection from project directory
✅ Path normalization and validation
✅ Lockfile creation and management
✅ Diagnostic log parsing
✅ Missing input detection
✅ CTAN package resolution
✅ CLI argument parsing
✅ Module exports (56 items)

## What Needs Implementation

### Critical (Blocks compilation)

**Agent 3 - WASM Loader**
- File: `src/lib/latex/engine/loader.mjs`
- Task: Load SwiftLaTeX WASM modules
- Required: Download real WASM binaries to `vendor/swiftlatex/`
- Current: Mock throws error at line 153-162 of `swiftlatex-engine.mjs`

### High Priority (Improves UX)

**CLI Commands**
- File: `src/extensions/latex.mjs`
- Task: Implement `validate` and `clean` commands
- Current: Placeholder returns "not yet implemented"

**Test Fixtures**
- Directory: `fixtures/`
- Task: Add article/, thesis/, errors/ examples
- Current: Only minimal/ exists

### Medium Priority (Enhancement)

**Cache Management**
- Directory: `src/lib/latex/cache/`
- Task: Implement manager.mjs, cleanup.mjs, integrity.mjs
- Current: README documentation only

**Engine Utilities**
- Directory: `src/lib/latex/engine/`
- Task: Implement loader.mjs, instance-pool.mjs, memory-manager.mjs
- Current: README documentation only

**Utility Scripts**
- Directory: `scripts/`
- Task: Implement setup-swiftlatex.mjs, verify-integration.mjs
- Current: README documentation only

## For Other Agents

### Agent 3 (Engine) - Next Steps

1. Download SwiftLaTeX WASM:
   ```bash
   # From packages/kgc-cli/
   node scripts/setup-swiftlatex.mjs
   ```

2. Implement loader:
   ```javascript
   // src/lib/latex/engine/loader.mjs
   export async function loadEngine(engine, wasmPath) {
     // Load Emscripten module
     // Initialize WASM
     // Return engine instance
   }
   ```

3. Replace mock in `swiftlatex-engine.mjs`:
   ```javascript
   // Line 146: Replace throw with:
   const engineInstance = await loadEngineFromWASM(engine, wasmPath);
   return engineInstance;
   ```

### Agent 7-9 (CLI/Test/Docs) - Next Steps

1. Implement CLI commands in `src/extensions/latex.mjs`:
   - `validate` handler (dry-run compilation)
   - `clean` handler (clear cache)

2. Add integration tests:
   ```javascript
   // test/latex-integration.test.mjs
   import { describe, it, expect } from 'vitest';
   import { compileLatexToPdf } from '../src/lib/latex/index.mjs';

   describe('LaTeX Compilation', () => {
     it('compiles minimal example', async () => {
       const pdf = await compileLatexToPdf({
         inputTexPath: 'fixtures/minimal/main.tex',
         projectDir: 'fixtures/minimal'
       });
       expect(pdf).toBeInstanceOf(Uint8Array);
       expect(pdf.length).toBeGreaterThan(1000);
     });
   });
   ```

3. Create test fixtures:
   ```bash
   fixtures/
   ├── minimal/       ✅ Done
   ├── article/       📝 TODO: Article with \cite, \ref
   ├── thesis/        📝 TODO: Chapters, ToC, bibliography
   └── errors/        📝 TODO: Missing package, undefined command
   ```

## Validation Commands

```bash
# Validate schemas load
node -e "import('./src/lib/latex/schemas.mjs').then(() => console.log('✅ Schemas OK'))"

# Validate exports
node -e "import('./src/lib/latex/index.mjs').then(m => console.log('✅', Object.keys(m).length, 'exports'))"

# Validate WASM (currently fails - expected)
npm run validate:wasm

# Run existing tests
npm test -- latex
```

## Troubleshooting

### "WASM engine not found"
**Cause**: SwiftLaTeX binaries are placeholders
**Fix**: Implement Agent 3's WASM loader and download real binaries

### "Module not found"
**Cause**: Import path incorrect
**Fix**: Use package exports:
```javascript
// ✅ Correct
import { ... } from '@unrdf/kgc-cli/latex';

// ❌ Wrong
import { ... } from '@unrdf/kgc-cli/src/lib/latex/index.mjs';
```

### "Validation failed"
**Cause**: Input doesn't match schema
**Fix**: Check schema requirements:
```javascript
import { CompileOptionsSchema } from '@unrdf/kgc-cli/latex/schemas';
const result = CompileOptionsSchema.safeParse(input);
console.log(result.error); // See what's wrong
```

## Performance Expectations

Once WASM is integrated:
- **VFS Collection**: ~5-20ms for typical project (< 100 files)
- **Compilation**: ~50-500ms per pass (depends on document size)
- **Caching**: ~1-5ms for cache hit
- **Resolution**: ~100-500ms per CTAN package (network dependent)

## Memory Usage

- **VFS**: Entire project loaded into memory (Map<string, Uint8Array>)
- **WASM**: ~10-50MB per engine instance
- **Cache**: Disk-based, no memory limit

## Next Actions

**Immediate**: Agent 3 implements WASM loader
**Parallel**: Agent 7-9 add test fixtures and CLI tests
**Future**: Implement cache management utilities

---

**Agent 1 Integration Complete**
All foundational schemas, exports, and directory structure ready for pipeline assembly.
