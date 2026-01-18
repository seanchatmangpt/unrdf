# Agent 6: Conventions Profile Primitive

## Mission
Encode organizational engineering conventions as a machine-checkable specification that serves as a compilation target for code generation and validation.

## Architecture

### 1. Profile Schema (`profile-schema.mjs`)
**Purpose**: Define the structure of a ConventionsProfile using Zod.

**Schema Hierarchy**:
```
ConventionsProfile
├── name: string
├── description: string
├── fileLayout
│   ├── serviceModules: string[] (glob patterns)
│   ├── testModules: string[] (glob patterns)
│   └── naming
│       ├── serviceClass: "PascalCase" | "camelCase" | "snake_case"
│       ├── method: "camelCase" | "snake_case"
│       └── constant: "UPPER_SNAKE_CASE" | "SCREAMING_SNAKE_CASE"
├── errorModel
│   ├── errorClass: string (class name)
│   └── fields: { code, message, details }
├── logging
│   ├── fields: string[] (required log fields)
│   └── format: "json" | "structured" | "plaintext"
├── testing
│   ├── framework: "vitest" | "jest" | "mocha"
│   ├── minCoverage: number (0-100)
│   └── requiredPatterns: string[] (validation rules)
└── dataContracts
    ├── dtoFormat: "object" | "class"
    ├── validation: "zod" | "joi" | "manual"
    └── requiredFields: string[]
```

### 2. Compiler (`compiler.mjs`)
**Purpose**: Compile and validate profiles against source code.

**Functions**:

#### `compileProfile(profileObj)`
- Input: Raw profile object
- Validation: Check against ConventionsProfileSchema
- Expansion: Resolve glob patterns to file lists
- Output: `{ schema, violations: [], compiled: true }`
- Error: Throws if profile invalid

#### `validateAgainstProfile(compiledProfile, targetFiles)`
- Input: Compiled profile + array of file paths
- Checks:
  - File naming conventions (serviceClass, method, constant)
  - Error model structure (errorClass, fields)
  - Logging format (fields, format)
  - Test patterns (framework, coverage, patterns)
  - Data contract compliance (dtoFormat, validation)
- Output: `{ valid: boolean, violations: [] }`
- Violation format: `{ file, rule, message, suggestion }`

#### `diagnosticReport(violations)`
- Input: Array of violations
- Output: Human-readable report with:
  - Summary: "Found X violations across Y files"
  - Grouped by rule type
  - Suggested fixes for each violation
  - Example: "NAMING_VIOLATION: src/foo.mjs uses snake_case for method 'get_user', expected camelCase → Rename to 'getUser'"

### 3. Validation Rules

#### Naming Rules
- **serviceClass**: Extract class/function names, check casing
- **method**: Extract method/function names, check casing
- **constant**: Extract const declarations, check casing

#### Error Model Rules
- Search for error class definitions
- Verify presence of required fields (code, message, details)
- Check field types match profile

#### Logging Rules
- Search for log statements
- Verify all required fields present
- Check format matches (JSON, structured, plaintext)

#### Testing Rules
- Match test files against glob patterns
- Check framework imports (vitest/jest/mocha)
- Verify coverage reports against minCoverage
- Validate test file naming patterns

#### Data Contract Rules
- Check DTO definitions (object literals vs classes)
- Verify validation library usage (zod/joi/manual)
- Ensure required fields present in all DTOs

### 4. Demo Profile (`demo-profile.mjs`)
**Example**: Enterprise-style Node.js service conventions

```javascript
{
  name: "Enterprise Service Standards",
  description: "Standard conventions for internal microservices",
  fileLayout: {
    serviceModules: ["src/services/*/index.mjs"],
    testModules: ["test/services/*.test.mjs"],
    naming: {
      serviceClass: "PascalCase",
      method: "camelCase",
      constant: "UPPER_SNAKE_CASE"
    }
  },
  errorModel: {
    errorClass: "AppError",
    fields: {
      code: "string",
      message: "string",
      details: "object"
    }
  },
  logging: {
    fields: ["timestamp", "level", "service", "traceId", "userId"],
    format: "json"
  },
  testing: {
    framework: "vitest",
    minCoverage: 80,
    requiredPatterns: [
      "Test files MUST use .test.mjs extension",
      "Each service MUST have corresponding test file"
    ]
  },
  dataContracts: {
    dtoFormat: "object",
    validation: "zod",
    requiredFields: ["id", "createdAt", "updatedAt"]
  }
}
```

### 5. Test Coverage (`test.mjs`)

#### Test Suite
1. **Schema Validation**
   - Valid profile passes
   - Missing required field fails
   - Invalid enum value fails
   - Extra fields allowed (forward compatibility)

2. **Profile Compilation**
   - Valid profile compiles successfully
   - Invalid profile throws with clear error
   - Deterministic: Same input → same output (100 iterations)

3. **Code Validation**
   - Naming violations detected (wrong casing)
   - Error model violations detected (missing fields)
   - Logging violations detected (missing required fields)
   - Test violations detected (wrong extension, missing coverage)
   - Data contract violations detected (no validation, missing fields)

4. **Diagnostic Reporting**
   - Multiple violations grouped by type
   - Clear suggestions for fixes
   - File paths normalized
   - Readable format

5. **Edge Cases**
   - Empty file list
   - No violations (100% compliant)
   - Malformed source code (graceful failure)
   - Circular dependencies (detected and reported)

## Success Criteria

✅ **Compilation**:
- Profile validates against schema
- Glob patterns expand correctly
- Compilation is deterministic

✅ **Validation**:
- All rule types detect violations
- False positive rate < 1%
- False negative rate < 1%

✅ **Diagnostics**:
- Violations clearly describe issue
- Suggestions actionable
- Report format machine-parseable

✅ **Tests**:
- 100% pass rate
- All edge cases covered
- Determinism verified (100 iterations)

## Information-Theoretic Guarantees

**Profile Entropy**: H(Profile) ≤ 12 bits
- 7 top-level keys
- ~16 configuration options
- Bounded complexity

**Validation Precision**: P(correct classification) ≥ 99%
- AST-based parsing (not regex)
- Zod schema validation
- Deterministic rule evaluation

**Compilation Invariant**: ∀p ∈ ValidProfiles, compile(compile(p)) = compile(p)
- Idempotent compilation
- No state leakage
- Pure functions only

## Non-Goals

❌ **Auto-fixing**: Only report violations, don't modify code
❌ **Runtime enforcement**: Static analysis only
❌ **Full language parsing**: Focus on conventions, not semantics
❌ **Performance optimization**: Correctness over speed

## Implementation Notes

- Use `acorn` for JavaScript AST parsing
- Use `minimatch` for glob pattern matching
- Pure functions throughout (no side effects)
- No OTEL in core logic (validation module only)
- Zod for all schema validation
- JSDoc type hints 100%
