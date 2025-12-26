# Agent 7: Convention-Preserving Code Generator

## Mission
Generate service façade code that matches organizational conventions exactly. Deterministic output guarantees same spec + profile → same code always.

## Core Principles

### 1. Template-Based Generation
- **Service Class Template**: Skeleton with CRUD operations (create, read, update, delete, list)
- **Error Handler Template**: Matches profile error model exactly
- **Test Template**: Matches profile testing framework and patterns
- **Deterministic**: No timestamps, GUIDs, or random elements in generated code

### 2. Convention Preservation
Generated code MUST match profile in:
- **Naming**: camelCase vs snake_case, prefixes, suffixes
- **Error Handling**: Error class structure, field names, codes
- **Logging**: Field names (timestamp vs time, level vs severity), format
- **Testing**: Framework (Jest, Mocha, Vitest), assertion style, coverage

### 3. Determinism Guarantees
- **Input Hash**: SHA-256(spec + profile + lens) → unique input signature
- **Canonicalization**: Remove comments, normalize whitespace, sort imports
- **Output Hash**: SHA-256(canonicalized code) → deterministic fingerprint
- **Reproducibility**: Same inputs → identical outputs across runs, machines, time

## Architecture

```
Input:
  serviceSpec: { name, entities, operations }
  compiledProfile: { naming, errorModel, logging, testing }
  compiledLens: { transform: fn }

Processing:
  1. Apply lens transform to spec
  2. Generate class skeleton
  3. Generate methods (CRUD + custom operations)
  4. Generate error handlers
  5. Generate logging statements
  6. Generate test file
  7. Canonicalize all code
  8. Hash output

Output:
  {
    code: string,           // Service class implementation
    testCode: string,       // Test file
    filename: string,       // e.g., "customer-service.mjs"
    hash: string,          // SHA-256 of canonicalized code
    metadata: {
      inputHash: string,   // Hash of inputs for reproducibility
      generatedAt: string, // ISO timestamp (NOT in code)
      profile: string,     // Profile name used
      lens: string         // Lens name used
    }
  }
```

## Template Structure

### Service Class Template
```javascript
/**
 * @fileoverview {ServiceName} - Generated service façade
 * @generated Convention-preserving code generator (Agent 7)
 */

import { dataFactory, createStore } from '@unrdf/oxigraph';
import { {ErrorClass} } from './{error-file}.mjs';
import { logger } from './{logger-file}.mjs';

/**
 * {ServiceName} service
 * @class
 */
export class {ServiceName} {
  /**
   * @param {{StoreType}} store - RDF store instance
   */
  constructor(store) {
    this.store = store;
  }

  /**
   * Create a new {entity}
   * @param {{EntityDTO}} dto - Data transfer object
   * @returns {Promise<{EntityResult}>}
   * @throws {{ErrorClass}}
   */
  async create{Entity}(dto) {
    // Validation
    // Logging (profile.logging.fields)
    // Implementation
    // Error handling (profile.errorModel)
  }

  // ... read, update, delete, list methods
}
```

### Error Handler Template
```javascript
/**
 * Application error matching profile conventions
 * @generated
 */
export class {ErrorClass} extends Error {
  /**
   * @param {string} code - Error code
   * @param {string} message - Error message
   * @param {Object} [details] - Additional details
   */
  constructor(code, message, details = {}) {
    super(message);
    this.{codeField} = code;      // from profile.errorModel.codeField
    this.{messageField} = message; // from profile.errorModel.messageField
    this.{detailsField} = details; // from profile.errorModel.detailsField
    this.name = this.constructor.name;
  }
}
```

### Test Template
```javascript
/**
 * @fileoverview Tests for {ServiceName}
 * @generated
 */

import { {testFrameworkImport} } from '{testFramework}';
import { {ServiceName} } from './{service-file}.mjs';

{testFrameworkDescribe}('{ServiceName}', () => {
  let service;

  {testFrameworkBeforeEach}(() => {
    // Setup
  });

  {testFrameworkIt}('should create {entity}', async () => {
    // Arrange
    // Act
    // Assert (profile.testing.assertionStyle)
  });

  // ... more tests for read, update, delete, list
});
```

## Determinism Strategy

### 1. Canonicalization Rules
```javascript
canonicalizeGenerated(code) {
  1. Remove all comments (/* */ and //)
  2. Remove all JSDoc (@param, @returns, etc.)
  3. Normalize whitespace:
     - Convert tabs to 2 spaces
     - Remove trailing whitespace
     - Normalize newlines to single \n
     - Remove blank lines >1 consecutive
  4. Sort imports alphabetically
  5. Normalize string quotes (all single quotes)
  6. Return deterministic string
}
```

### 2. Hash Generation
```javascript
hashGeneratedCode(code) {
  1. Canonicalize code
  2. SHA-256 hash
  3. Return hex string
}

inputHash(spec, profile, lens) {
  1. JSON.stringify with sorted keys
  2. SHA-256 hash
  3. Return hex string
}
```

### 3. Reproducibility Testing
- **Test 1**: Generate same service 100x → all hashes identical
- **Test 2**: Change 1 char in spec → hash changes
- **Test 3**: Different profile → different hash
- **Test 4**: Same spec on different machines → same hash

## Code Quality Guarantees

### 1. Valid JavaScript
- All generated code must parse without errors
- Can be imported as ES module
- Can be instantiated and executed

### 2. Convention Compliance
- Naming matches profile exactly (verified by parser)
- Error handling uses profile error model
- Logging includes all profile.logging.fields
- Tests use profile.testing.framework

### 3. Golden Test
- Check in golden file: `agent-7/golden/customer-service.mjs`
- Compare generated vs golden
- If different, fail with clear diff
- Update golden only when conventions change

## Integration Points

### With Agent 6 (Profile Compiler)
```javascript
import { compileProfile } from '../agent-6/index.mjs';
const compiledProfile = await compileProfile(profileAST);
```

### With Agent 3 (Lens Compiler)
```javascript
import { compileLens } from '../agent-3/index.mjs';
const compiledLens = await compileLens(lensAST);
```

### With Agent 8 (Drift Detector)
```javascript
// Agent 7 provides hash for Agent 8 to detect changes
const { code, hash } = generateFacade(spec, profile, lens);
// Agent 8 can compare hash(generated) vs hash(existing)
```

## Performance Targets

- **Generation Speed**: <50ms for typical service (5 CRUD operations)
- **Canonicalization**: <10ms for typical file (500 LoC)
- **Hash Calculation**: <5ms
- **Total**: <100ms end-to-end

## Error Handling

### Generation Errors
```javascript
class GenerationError extends Error {
  constructor(phase, reason, context) {
    super(`Code generation failed at ${phase}: ${reason}`);
    this.phase = phase;  // 'template', 'method', 'test', etc.
    this.reason = reason;
    this.context = context;
  }
}
```

### Validation Errors
- Invalid spec → throw with details
- Missing profile fields → throw with required fields
- Invalid lens → throw with validation errors

## Example Usage

```javascript
import { generateFacade } from './agent-7/index.mjs';
import { compileProfile } from '../agent-6/index.mjs';
import { compileLens } from '../agent-3/index.mjs';

const serviceSpec = {
  name: 'CustomerService',
  entities: ['Customer'],
  operations: ['create', 'read', 'update', 'delete', 'list']
};

const compiledProfile = await compileProfile(profileAST);
const compiledLens = await compileLens(lensAST);

const { code, testCode, filename, hash } = await generateFacade(
  serviceSpec,
  compiledProfile,
  compiledLens
);

// code is ready to use, matches conventions exactly
// hash proves determinism
```

## Success Criteria

- [ ] 100 consecutive runs → identical hashes
- [ ] Generated code is valid JavaScript (imports, executes)
- [ ] Matches profile conventions (verified by parser)
- [ ] Golden test passes
- [ ] Performance: <100ms total generation time
- [ ] Test coverage: 100% of generator logic

## OTEL Validation

```bash
# Generate code
node agent-7/demo-facade-customer.mjs

# Verify determinism (100 runs)
for i in {1..100}; do
  node agent-7/demo-facade-customer.mjs | grep "Hash:" | sort -u | wc -l
done
# MUST output: 1 (single unique hash)

# Run tests
timeout 5s node agent-7/test.mjs
# MUST show: ✅ All tests pass

# Verify generated code validity
node -c agent-7/generated/customer-service.mjs
# MUST exit 0 (syntax valid)
```

## Adversarial PM Questions

**Q**: Did you RUN the generated code?
**A**: Yes - import it, instantiate, execute methods. Show output.

**Q**: Can you PROVE determinism?
**A**: Yes - 100 runs, all hashes identical. Show logs.

**Q**: What BREAKS if hash differs?
**A**: Migration system thinks code changed when it didn't → unnecessary rewrites, drift false positives.

**Q**: What's the EVIDENCE it matches conventions?
**A**: Parse generated code, extract naming/error/logging patterns, compare to profile. Show diff (must be empty).

---

**Core Truth**: Generated code is indistinguishable from hand-written code following conventions. Determinism means migration is idempotent.
