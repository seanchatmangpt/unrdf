# Agent 7: Convention-Preserving Generator - Design Plan

## 🎯 Mission

Generate façade modules that are **byte-for-byte deterministic** and **indistinguishable from target organization's native code**.

**Core Principle**: Same input → identical output. Every time. Measurably.

---

## 🏗️ Architecture Overview

```
Input                    Generator                 Output
┌─────────────────┐     ┌──────────────────┐     ┌────────────────────┐
│ CompiledProfile │────→│ Template Engine  │────→│ customer-facade.mjs│
│ CompiledLens    │     │   ↓              │     │ (deterministic)    │
│ ServiceSpec     │     │ AST Builder      │     └────────────────────┘
└─────────────────┘     │   ↓              │              ↓
                        │ Formatter        │     ┌────────────────────┐
                        │   ↓              │     │ Validation         │
                        │ Validator        │     │ OTEL spans         │
                        └──────────────────┘     │ Golden test ✅     │
                                                 └────────────────────┘
```

**Pipeline**:
1. **Template Expansion** - Fill service spec into templates
2. **AST Construction** - Build consistent syntax tree
3. **Deterministic Formatting** - Apply ordering/spacing rules
4. **Validation** - Verify against profile + golden files

---

## 📁 Files to Create

### 1. `./src/generator.mjs` (Core Engine)

**Exports**:
```javascript
/**
 * Generate façade module matching target conventions
 * @param {CompiledProfile} profile - Target organization conventions
 * @param {CompiledLens} lens - Field mapping transformations
 * @param {ServiceSpec} service - Service to wrap
 * @returns {string} Generated source code (deterministic)
 */
export function generateFacade(profile, lens, service) {
  // STEP 1: Build AST
  const ast = buildFacadeAST(profile, lens, service);

  // STEP 2: Format deterministically
  const code = formatCode(ast, profile);

  // STEP 3: Validate
  const validation = validateGeneratedCode(code, profile);
  if (!validation.ok) throw new Error(`Validation failed: ${validation.violations}`);

  return code;
}

/**
 * Build AST from service spec
 * @internal
 */
function buildFacadeAST(profile, lens, service) {
  return {
    imports: buildImports(profile, service),
    constants: buildConstants(profile),
    functions: service.operations.map(op => buildFunction(profile, lens, op)),
    exports: buildExports(service.operations)
  };
}
```

**Key Algorithm**:
```javascript
// Determinism enforcement points:
// 1. Sort imports alphabetically
// 2. Sort operations by name
// 3. Sort JSDoc fields by profile.fieldOrder
// 4. Use stable iteration (for...of, not forEach with object keys)
// 5. No Date.now(), Math.random(), process.hrtime()
```

### 2. `./src/templates.mjs` (Code Templates)

**Template Structure**:
```javascript
/**
 * Function template builder
 * @param {Operation} op - Service operation
 * @param {CompiledProfile} profile - Conventions
 * @param {CompiledLens} lens - Transformations
 * @returns {FunctionNode}
 */
export function buildFunctionTemplate(op, profile, lens) {
  return {
    jsdoc: buildJSDoc(op, profile),
    signature: buildSignature(op, profile),
    body: buildBody(op, profile, lens),
    errorHandling: buildErrorHandler(profile)
  };
}

/**
 * JSDoc template matching target conventions
 */
function buildJSDoc(op, profile) {
  // Order fields per profile.jsdocFieldOrder
  const fields = [
    { tag: 'description', value: op.description },
    ...op.params.map(p => ({ tag: 'param', value: `{${p.type}} ${p.name} - ${p.desc}` })),
    { tag: 'returns', value: `{${op.returns.type}} ${op.returns.desc}` }
  ];

  // Add profile-specific fields (e.g., @throws, @example)
  if (profile.conventions.jsdoc.includeThrows) {
    fields.push({ tag: 'throws', value: `{${profile.conventions.errorClass}} ${profile.conventions.errorMessage}` });
  }

  // Sort by profile.jsdocFieldOrder
  return sortFields(fields, profile.conventions.jsdoc.fieldOrder);
}

/**
 * Error handling template
 */
function buildErrorHandler(profile) {
  const style = profile.conventions.errorHandling.style;

  if (style === 'try-catch-zod') {
    return {
      wrapper: 'try { ... } catch (err) { ... }',
      validation: 'zod.parse(input)',
      throw: `throw new ${profile.conventions.errorClass}(message, { cause: err })`
    };
  }

  if (style === 'result-monad') {
    return {
      wrapper: 'return Result.try(() => { ... })',
      validation: 'validateOrError(input)',
      return: 'Result.ok(value) / Result.err(error)'
    };
  }

  throw new Error(`Unknown error handling style: ${style}`);
}

/**
 * Logging template
 */
function buildLoggingTemplate(profile, lens) {
  // Use lens to transform canonical fields → target fields
  const fieldMap = lens.compile();

  return {
    level: profile.conventions.logging.defaultLevel,
    fields: Object.entries(fieldMap).map(([canonical, target]) =>
      `${target}: context.${canonical}`
    ),
    format: profile.conventions.logging.format // 'json' | 'pretty' | 'structured'
  };
}
```

**Templates Provided**:
- Function wrapper (sync/async)
- JSDoc block (field ordering)
- Error handling (try-catch, Result, Option)
- Logging statement (JSON, structured)
- Import statement (sorted, grouped)
- Export statement (named, default)

### 3. `./src/formatter.mjs` (Deterministic Formatting)

**Core Function**:
```javascript
/**
 * Format AST to deterministic source code
 * @param {AST} ast - Abstract syntax tree
 * @param {CompiledProfile} profile - Formatting rules
 * @returns {string} Formatted code (byte-for-byte stable)
 */
export function formatCode(ast, profile) {
  const config = {
    indent: '  ', // 2 spaces (no tabs)
    lineLength: 100,
    sortImports: true,
    sortExports: true,
    jsdocFieldOrder: profile.conventions.jsdoc.fieldOrder,
    trailingComma: profile.conventions.style.trailingComma ?? 'es5',
    quotes: profile.conventions.style.quotes ?? 'single',
    semicolons: profile.conventions.style.semicolons ?? true
  };

  const sections = [
    formatFileHeader(profile),
    formatImports(ast.imports, config),
    formatConstants(ast.constants, config),
    formatFunctions(ast.functions, config),
    formatExports(ast.exports, config)
  ];

  return sections.filter(s => s).join('\n\n') + '\n'; // Always end with newline
}

/**
 * Deterministic import sorting
 */
function formatImports(imports, config) {
  if (!imports.length) return '';

  // Group: node built-ins, external, internal
  const groups = {
    builtin: imports.filter(i => isNodeBuiltin(i.from)),
    external: imports.filter(i => !isNodeBuiltin(i.from) && !i.from.startsWith('.')),
    internal: imports.filter(i => i.from.startsWith('.'))
  };

  // Sort each group alphabetically
  Object.values(groups).forEach(group =>
    group.sort((a, b) => a.from.localeCompare(b.from))
  );

  // Format each import
  const formatted = [
    ...groups.builtin.map(formatImport),
    groups.external.length && '', // blank line between groups
    ...groups.external.map(formatImport),
    groups.internal.length && '',
    ...groups.internal.map(formatImport)
  ].filter(line => line !== false);

  return formatted.join('\n');
}

function formatImport(imp) {
  const { names, from } = imp;
  const quote = config.quotes === 'single' ? "'" : '"';

  if (names.length === 1) {
    return `import { ${names[0]} } from ${quote}${from}${quote};`;
  }

  // Multi-line if >3 names or exceeds line length
  if (names.length > 3 || calculateLength(names, from) > config.lineLength) {
    const sortedNames = [...names].sort();
    return `import {\n${sortedNames.map(n => `  ${n}`).join(',\n')}\n} from ${quote}${from}${quote};`;
  }

  return `import { ${names.join(', ')} } from ${quote}${from}${quote};`;
}

/**
 * Deterministic JSDoc formatting
 */
function formatJSDoc(jsdoc, config) {
  const order = config.jsdocFieldOrder || ['description', 'param', 'returns', 'throws', 'example'];

  // Sort fields by order
  const sorted = jsdoc.fields.sort((a, b) =>
    order.indexOf(a.tag) - order.indexOf(b.tag)
  );

  const lines = [
    '/**',
    ` * ${sorted.find(f => f.tag === 'description')?.value || ''}`,
    ...sorted.filter(f => f.tag !== 'description').map(f => ` * @${f.tag} ${f.value}`),
    ' */'
  ];

  return lines.join('\n');
}
```

**Formatting Rules** (Enforced):
1. **Indentation**: 2 spaces, no tabs
2. **Line length**: 100 characters (wrap consistently)
3. **Import sorting**: builtin → external → internal, alphabetical
4. **JSDoc order**: Per profile.jsdocFieldOrder
5. **Trailing commas**: Consistent (es5, all, none)
6. **Quotes**: Single or double (profile-defined)
7. **Semicolons**: Always or never (profile-defined)
8. **Blank lines**: 1 between sections, 2 between functions
9. **File ending**: Always single newline

### 4. `./src/validator.mjs` (Code Validation)

**Exports**:
```javascript
/**
 * Validate generated code against profile
 * @param {string} code - Generated source code
 * @param {CompiledProfile} profile - Target conventions
 * @returns {{ ok: boolean, violations: string[] }}
 */
export function validateGeneratedCode(code, profile) {
  const violations = [];

  // Static analysis
  violations.push(...checkImportOrder(code));
  violations.push(...checkJSDocFormat(code, profile));
  violations.push(...checkLineLength(code, 100));
  violations.push(...checkIndentation(code));
  violations.push(...checkTrailingNewline(code));

  // Convention compliance
  violations.push(...checkErrorHandling(code, profile));
  violations.push(...checkLoggingFields(code, profile));

  // Determinism check
  violations.push(...checkNonDeterministicAPIs(code));

  return {
    ok: violations.length === 0,
    violations
  };
}

/**
 * Check for non-deterministic API usage
 */
function checkNonDeterministicAPIs(code) {
  const forbidden = [
    /Date\.now\(\)/,
    /Math\.random\(\)/,
    /process\.hrtime/,
    /crypto\.randomUUID/,
    /performance\.now/
  ];

  const violations = [];
  forbidden.forEach(pattern => {
    if (pattern.test(code)) {
      violations.push(`Non-deterministic API detected: ${pattern.source}`);
    }
  });

  return violations;
}

/**
 * Check JSDoc field order matches profile
 */
function checkJSDocFormat(code, profile) {
  // Parse JSDoc blocks
  const jsdocBlocks = extractJSDocBlocks(code);
  const expectedOrder = profile.conventions.jsdoc.fieldOrder;

  const violations = [];
  jsdocBlocks.forEach((block, idx) => {
    const actualOrder = block.fields.map(f => f.tag);

    // Check order
    for (let i = 0; i < actualOrder.length - 1; i++) {
      const current = expectedOrder.indexOf(actualOrder[i]);
      const next = expectedOrder.indexOf(actualOrder[i + 1]);
      if (current > next) {
        violations.push(`JSDoc block ${idx}: fields out of order (${actualOrder[i]} > ${actualOrder[i+1]})`);
      }
    }
  });

  return violations;
}
```

### 5. `./test/generator.test.mjs` (Golden Tests)

**Test Strategy**:
```javascript
import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { generateFacade, formatCode, validateGeneratedCode } from '../src/generator.mjs';

describe('Convention-Preserving Generator', () => {

  it('generates customer service façade (golden test)', async () => {
    // GIVEN: Profile, lens, service spec
    const profile = await loadProfile('target-org');
    const lens = await loadLens('canonical-to-target');
    const service = {
      name: 'CustomerService',
      description: 'Customer management operations',
      operations: [
        {
          name: 'createCustomer',
          description: 'Create a new customer',
          params: [
            { name: 'data', type: 'CustomerData', desc: 'Customer information' }
          ],
          returns: { type: 'Customer', desc: 'Created customer' }
        },
        {
          name: 'getCustomer',
          description: 'Retrieve customer by ID',
          params: [
            { name: 'id', type: 'string', desc: 'Customer ID' }
          ],
          returns: { type: 'Customer', desc: 'Customer record' }
        }
      ]
    };

    // WHEN: Generate façade
    const generated = generateFacade(profile, lens, service);

    // THEN: Match golden file byte-for-byte
    const golden = await readFile('./examples/customer-service-facade.mjs', 'utf-8');
    assert.equal(generated, golden, 'Generated code must match golden file exactly');
  });

  it('generates identical output on repeated calls (determinism)', async () => {
    const profile = await loadProfile('target-org');
    const lens = await loadLens('canonical-to-target');
    const service = getMinimalServiceSpec();

    // Generate 10 times
    const outputs = Array.from({ length: 10 }, () =>
      generateFacade(profile, lens, service)
    );

    // All must be identical
    const first = outputs[0];
    outputs.forEach((output, idx) => {
      assert.equal(output, first, `Output ${idx} differs from first`);
    });
  });

  it('validates generated code passes all checks', async () => {
    const profile = await loadProfile('target-org');
    const lens = await loadLens('canonical-to-target');
    const service = getMinimalServiceSpec();

    const generated = generateFacade(profile, lens, service);
    const validation = validateGeneratedCode(generated, profile);

    assert.equal(validation.ok, true, `Violations: ${validation.violations.join(', ')}`);
    assert.equal(validation.violations.length, 0);
  });

  it('formats imports in deterministic order', () => {
    const ast = {
      imports: [
        { names: ['createStore'], from: '@unrdf/oxigraph' },
        { names: ['readFile'], from: 'node:fs/promises' },
        { names: ['helper'], from: './utils.mjs' },
        { names: ['zod'], from: 'zod' }
      ]
    };

    const formatted = formatImports(ast.imports, defaultConfig);

    // Expect: node built-ins, external, internal
    const lines = formatted.split('\n').filter(l => l.trim());
    assert.match(lines[0], /node:fs/);
    assert.match(lines[1], /@unrdf/);
    assert.match(lines[2], /zod/);
    assert.match(lines[3], /\.\/utils/);
  });

  it('formats JSDoc fields in profile order', () => {
    const profile = {
      conventions: {
        jsdoc: {
          fieldOrder: ['description', 'param', 'returns', 'throws', 'example']
        }
      }
    };

    const jsdoc = {
      fields: [
        { tag: 'returns', value: '{Customer}' },
        { tag: 'param', value: '{string} id - Customer ID' },
        { tag: 'description', value: 'Get customer by ID' },
        { tag: 'throws', value: '{Error} If not found' }
      ]
    };

    const formatted = formatJSDoc(jsdoc, profile);

    // Check order
    const lines = formatted.split('\n');
    assert.match(lines[1], /Get customer by ID/); // description first
    assert.match(lines[2], /@param/);
    assert.match(lines[3], /@returns/);
    assert.match(lines[4], /@throws/);
  });

});
```

**Golden File Requirements**:
- Store in `./examples/customer-service-facade.mjs`
- Generated exactly once (manual review)
- Committed to git (reference truth)
- Regenerated only on profile changes
- Diff reviewed in PR

### 6. `./examples/customer-service-facade.mjs` (Golden File)

**Example Output**:
```javascript
/**
 * Customer Service Façade
 * Auto-generated from CustomerService spec
 * Conventions: target-org profile v1.0.0
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';
import { logOperation } from './logging.mjs';
import { transformFields } from './lens.mjs';

const CustomerDataSchema = z.object({
  name: z.string(),
  email: z.string().email()
});

/**
 * Create a new customer
 * @param {CustomerData} data - Customer information
 * @returns {Customer} Created customer
 * @throws {ValidationError} If data is invalid
 */
export async function createCustomer(data) {
  try {
    const validated = CustomerDataSchema.parse(data);

    logOperation({
      operation: 'createCustomer',
      customer_name: validated.name,
      customer_email: validated.email
    });

    const transformed = transformFields(validated);
    const customer = await store.create('Customer', transformed);

    return customer;
  } catch (err) {
    throw new ValidationError('Failed to create customer', { cause: err });
  }
}

/**
 * Retrieve customer by ID
 * @param {string} id - Customer ID
 * @returns {Customer} Customer record
 * @throws {NotFoundError} If customer does not exist
 */
export async function getCustomer(id) {
  try {
    logOperation({
      operation: 'getCustomer',
      customer_id: id
    });

    const customer = await store.get('Customer', id);

    if (!customer) {
      throw new NotFoundError(`Customer ${id} not found`);
    }

    return customer;
  } catch (err) {
    if (err instanceof NotFoundError) throw err;
    throw new OperationError('Failed to retrieve customer', { cause: err });
  }
}
```

---

## 🔧 Generation Algorithm

### Step-by-Step Process

```javascript
function generateFacade(profile, lens, service) {
  // PHASE 1: Pre-processing
  const normalized = normalizeServiceSpec(service);
  const sorted = sortOperations(normalized.operations); // Alphabetical

  // PHASE 2: AST Construction
  const ast = {
    header: buildFileHeader(profile, service),
    imports: buildImports(profile, service, sorted),
    constants: buildConstants(profile, sorted),
    functions: sorted.map(op => buildFunction(profile, lens, op)),
    exports: buildExports(sorted)
  };

  // PHASE 3: Formatting
  const formatted = formatCode(ast, profile);

  // PHASE 4: Validation
  const validation = validateGeneratedCode(formatted, profile);
  if (!validation.ok) {
    throw new GenerationError('Validation failed', { violations: validation.violations });
  }

  // PHASE 5: Return
  return formatted;
}
```

### Determinism Guarantees

**Sources of Non-Determinism** (Eliminated):
1. ❌ Object.keys() iteration order → ✅ Array.sort()
2. ❌ Set iteration order → ✅ Sorted array
3. ❌ Date.now() timestamps → ✅ Static version string
4. ❌ Random IDs → ✅ Deterministic naming
5. ❌ File system order → ✅ Explicit sorting
6. ❌ Async race conditions → ✅ Sequential processing

**Verification**:
```bash
# Generate 10 times, all outputs identical
for i in {1..10}; do
  node src/generator.mjs > "output-$i.mjs"
done

# Check all are identical
diff output-{1,2,3,4,5,6,7,8,9,10}.mjs
# Expected: No differences
```

---

## 🧪 Testing Strategy

### Test Pyramid

```
       Golden Tests (1)
      /              \
  Determinism (1)  Validation (1)
  /        |        \
Format   Template   AST
 (5)       (3)      (2)
```

**Minimum Tests** (Total: 13):

1. **Golden Test** (1):
   - Generate customer-service-facade.mjs
   - Compare to golden file (byte-for-byte)

2. **Determinism Test** (1):
   - Generate 10 times
   - All outputs identical

3. **Validation Test** (1):
   - Generated code passes all checks
   - Zero violations

4. **Format Tests** (5):
   - Import sorting (builtin, external, internal)
   - JSDoc field ordering
   - Line length wrapping
   - Trailing comma consistency
   - Indentation correctness

5. **Template Tests** (3):
   - Function template expansion
   - Error handling template
   - Logging template

6. **AST Tests** (2):
   - AST construction from service spec
   - Export statement generation

**Test Execution**:
```bash
timeout 5s npm test 2>&1 | tee test-output.log

# Verify
grep "✅" test-output.log  # All passing
grep "13 passed" test-output.log  # Total count
```

---

## 📊 Success Criteria

### Measurable Outcomes

| Metric | Target | Verification |
|--------|--------|--------------|
| **Determinism** | 100% | 10 runs → identical output |
| **Golden match** | Byte-for-byte | `diff generated.mjs golden.mjs` = 0 lines |
| **Validation** | 0 violations | `validateGeneratedCode().violations.length === 0` |
| **Test coverage** | 13/13 passing | `timeout 5s npm test` |
| **Generation time** | <100ms | `time node generator.mjs` |
| **Code size** | <500 lines | `wc -l src/generator.mjs` |

### Adversarial PM Questions

**Before declaring "done"**:

1. ❓ **Did you RUN the generator or just write it?**
   - Show: `node src/generator.mjs > output.mjs && cat output.mjs`

2. ❓ **Can you PROVE determinism?**
   - Show: `for i in {1..10}; do node generator.mjs > out-$i.mjs; done && diff out-*.mjs`

3. ❓ **Does generated code match golden file?**
   - Show: `diff examples/customer-service-facade.mjs <(node generator.mjs)` (0 lines)

4. ❓ **What BREAKS if formatting is wrong?**
   - Answer: Target org rejects PR (linter fails), breaks trust, manual fixes needed

5. ❓ **How do you verify JSDoc field order?**
   - Show: `grep '@param\|@returns\|@throws' output.mjs` (order matches profile)

6. ❓ **What happens on non-deterministic input?**
   - Answer: Validator catches it (`checkNonDeterministicAPIs` fails)

---

## 🔗 Dependencies

### Input Dependencies

1. **Agent 3** (`@unrdf/autonomic-lens`):
   ```javascript
   import { compileLens } from '@unrdf/autonomic-lens';
   const lens = compileLens(lensAST);
   const fieldMap = lens.getFieldMapping(); // canonical → target
   ```

2. **Agent 6** (`@unrdf/autonomic-profile`):
   ```javascript
   import { compileProfile } from '@unrdf/autonomic-profile';
   const profile = compileProfile(profileAST);
   const conventions = profile.conventions; // jsdoc, errorHandling, logging, style
   ```

3. **Zod** (validation):
   ```javascript
   import { z } from 'zod';
   const ServiceSpecSchema = z.object({
     name: z.string(),
     operations: z.array(OperationSchema)
   });
   ```

### Output Dependencies

**Consumers**:
- Agent 8: Multi-Façade Validator (validates generated code)
- Agent 9: Contract Composer (uses façades to build contracts)
- End users: Import façade in their applications

**Exports**:
```javascript
export {
  generateFacade,      // Main API
  formatCode,          // Formatting utility
  validateGeneratedCode // Validation utility
};
```

---

## 🚀 Implementation Phases

### Phase 1: Core Generator (1 unit)
- [ ] Implement `buildFacadeAST()`
- [ ] Implement `buildFunction()`
- [ ] Implement `buildImports()`
- [ ] Test AST construction

### Phase 2: Templates (1 unit)
- [ ] Create function templates
- [ ] Create JSDoc templates
- [ ] Create error handling templates
- [ ] Test template expansion

### Phase 3: Formatter (1 unit)
- [ ] Implement `formatCode()`
- [ ] Implement import sorting
- [ ] Implement JSDoc formatting
- [ ] Test formatting rules

### Phase 4: Validator (1 unit)
- [ ] Implement `validateGeneratedCode()`
- [ ] Implement static checks
- [ ] Implement convention checks
- [ ] Test validation

### Phase 5: Golden Tests (1 unit)
- [ ] Create golden file
- [ ] Implement golden test
- [ ] Implement determinism test
- [ ] Verify all tests pass

**Total**: 5 units (~2-3 hours estimated)

---

## 🎯 SPARC Alignment

### Specification Phase
- **Input**: CompiledProfile, CompiledLens, ServiceSpec
- **Output**: Deterministic façade source code
- **Success**: Byte-for-byte identical on repeated generation

### Pseudocode Phase
```
FUNCTION generateFacade(profile, lens, service):
  normalized = sortOperations(service.operations)
  ast = buildAST(profile, lens, normalized)
  formatted = formatCode(ast, profile)
  validation = validate(formatted, profile)
  IF validation.ok THEN RETURN formatted
  ELSE THROW validation.violations
```

### Architecture Phase
- **Modules**: generator, templates, formatter, validator
- **Data Flow**: ServiceSpec → AST → Formatted Code → Validated Code
- **Testing**: Golden files + determinism + unit tests

### Refinement Phase
- **TDD**: Write tests first (golden, determinism, validation)
- **Iteration**: Adjust templates based on test failures
- **Optimization**: Cache template compilation if needed

### Completion Phase
- **Integration**: Works with Agent 3 (lens) + Agent 6 (profile)
- **Documentation**: Examples + usage guide
- **Validation**: All 13 tests passing, 0 violations

---

## 📝 Notes

### Design Decisions

1. **Why AST over String Templates?**
   - Easier to enforce deterministic ordering
   - Simpler to validate structure
   - Better for complex formatting rules

2. **Why Golden Tests?**
   - Prevent regressions (changes are visible in diff)
   - Single source of truth (manual review once)
   - Fast feedback (simple file comparison)

3. **Why Separate Validator?**
   - Reusable by other agents (e.g., Agent 8)
   - Testable independently
   - Clear separation of concerns

### Known Limitations

1. **Line wrapping**: Fixed at 100 chars (not configurable per-profile yet)
2. **Comment preservation**: Generated code has no inline comments (only JSDoc)
3. **Style variations**: Supports 2-3 error handling styles (expand as needed)

### Future Enhancements

1. **Multi-language support**: TypeScript, Python, Go templates
2. **Custom templates**: Allow profile to override default templates
3. **Incremental generation**: Update only changed operations
4. **Performance**: Parallelize operation generation (if >100 operations)

---

## ✅ Definition of Done

**Agent 7 is complete when**:

1. ✅ `timeout 5s npm test` passes 13/13 tests
2. ✅ Generated code matches golden file (byte-for-byte)
3. ✅ 10 consecutive generations produce identical output
4. ✅ `validateGeneratedCode()` returns `{ ok: true, violations: [] }`
5. ✅ `wc -l src/*.mjs` < 500 lines per file
6. ✅ Integration test with Agent 3 + Agent 6 passes
7. ✅ PLAN.md reviewed and approved

**Evidence Required**:
- Test output showing 13/13 passing
- `diff` command showing 0 differences (golden vs generated)
- Timing output showing <100ms generation
- Validation output showing 0 violations

---

## 🔍 Adversarial PM Final Check

**If challenged, can you prove**:

1. ✅ **Determinism**: `for i in {1..10}; do node generator.mjs > out-$i.mjs; done && diff out-*.mjs` (no output)
2. ✅ **Correctness**: `diff examples/golden.mjs <(node generator.mjs)` (0 lines)
3. ✅ **Performance**: `time node generator.mjs` (<100ms)
4. ✅ **Coverage**: `npm test 2>&1 | grep "13 passed"` (exact match)
5. ✅ **Integration**: `node test-integration.mjs` (Agent 3 + 6 work together)

**What breaks if you're wrong**:
- Generated code fails target org's linter → PR rejected
- Non-deterministic output → diffs pollute git history
- Wrong conventions → human must manually fix → defeats purpose
- Slow generation → blocks development workflow

**Trust model**: OTEL validation required (Agent 8 will verify).

---

**End of Plan**
