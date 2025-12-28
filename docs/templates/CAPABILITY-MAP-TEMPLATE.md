# @unrdf/[PACKAGE-NAME] Capability Map

**Version**: [VERSION]
**Status**: [Production Ready | Beta | Experimental]
**Runtime**: [Node.js | Browser | BEAM/WASM | Universal]
**Last Updated**: [DATE]

---

## Overview

[2-3 sentence description of the package's primary purpose and value proposition]

**Key Capabilities**:
- [Capability 1]: [One-line description]
- [Capability 2]: [One-line description]
- [Capability 3]: [One-line description]

**Package Exports**:
```javascript
import { capability1, capability2 } from '@unrdf/[package-name]';
```

**Dependencies**:
- Required: `@unrdf/core` (≥X.Y.Z)
- Optional: `@unrdf/other-package` (for specific features)

**Evidence**:
- Test Coverage: [XX]%
- Test Files: [N] files, [N] passing
- OTEL Validation: [XX]/100
- Example Files: [N]

---

## Capability Atoms

### Core Capabilities (Tier 1)

| Atom | Type | Runtime | Evidence | Compositions |
|------|------|---------|----------|--------------|
| `functionName()` | Function | Node, Browser | [src/file.mjs:L](file://path#L) | C1, C2, C3 |
| `ClassName` | Class | Node | [src/file.mjs:L](file://path#L) | C1, C4 |

**Verification**:
```bash
timeout 5s node /home/user/unrdf/packages/[name]/test/[test].test.mjs
```

### Advanced Capabilities (Tier 2)

[Same table structure for advanced/optional capabilities]

### Experimental Capabilities (Tier 3)

[Same table structure for experimental features]

---

## Composition Patterns

**C1**: [Pattern Name] - [Brief description]
```javascript
// Example composition
import { atom1, atom2 } from '@unrdf/[package]';
// Usage pattern
```

**C2**: [Pattern Name] - [Brief description]
```javascript
// Example composition
```

---

## Diataxis Documentation

### 📚 Tutorials (Learning-Oriented)

#### Tutorial 1: Getting Started with [Package Name]

**Goal**: [What learners will achieve by the end - specific, measurable outcome]

**Prerequisites**:
- Node.js ≥ 18.0.0 installed
- Basic understanding of [concept]
- Familiarity with [technology]

**Time**: [15-30 minutes]

**What You'll Build**: [Concrete deliverable - "A working X that does Y"]

**Steps**:

1. **Installation**
   ```bash
   pnpm install @unrdf/[package-name]
   ```

2. **Basic Setup**
   ```javascript
   import { capability1 } from '@unrdf/[package-name]';

   // Step-by-step code with explanations
   const instance = capability1({
     option1: 'value1',
     option2: 'value2'
   });
   ```

3. **First Operation**
   ```javascript
   // Clear, executable code
   const result = instance.operation();
   console.log(result); // Expected output: [specific value]
   ```

4. **Verification**
   ```bash
   # Command to verify it works
   node tutorial-example.mjs
   # Expected output: [specific output]
   ```

**Success Criteria**:
- [ ] Code runs without errors
- [ ] Output matches expected result
- [ ] You understand [key concept]

**Next Steps**:
- 📚 Try [Tutorial 2]
- 🛠️ Read [How-To Guide: Solve Problem X]
- 💡 Understand [Explanation: Why This Design]

---

#### Tutorial 2: [Next Level Topic]

[Same structure as Tutorial 1]

---

### 🛠️ How-To Guides (Task-Oriented)

#### How-To: [Solve Specific Problem]

**Problem**: [Concrete problem statement - "You need to..."]

**Context**: When you encounter [scenario], you need to [solution]

**Prerequisites**:
- Completed [Tutorial 1] or equivalent knowledge
- Understanding of [concept]

**Solution**:

**Step 1: [Action]**
```javascript
// Focused, goal-oriented code
import { capability1, capability2 } from '@unrdf/[package-name]';

const solution = capability1(params);
```

**Why this works**: [Brief explanation of the approach]

**Step 2: [Next Action]**
```javascript
// Next part of solution
const result = capability2(solution, {
  option: 'value'
});
```

**Step 3: [Final Action]**
```javascript
// Complete the solution
return result;
```

**Verification**:
```bash
# How to verify the solution works
timeout 5s node solution-example.mjs
# Expected: [specific output]
```

**Common Pitfalls**:
- ❌ **Don't**: [Common mistake]
  - **Problem**: [Why it fails]
- ✅ **Do**: [Correct approach]
  - **Reason**: [Why it works]

**Alternatives**:
- If you need [different outcome], use [alternative approach]
- For [edge case], consider [different pattern]

**Performance Considerations**:
- For datasets < 1K items: [approach A]
- For datasets > 1K items: [approach B with caching]

**See Also**:
- 🛠️ [Related How-To Guide]
- 📖 [API Reference: capability1]
- 💡 [Explanation: Design Decision]

---

#### How-To: [Another Common Task]

[Same structure]

---

### 📖 Reference (Information-Oriented)

#### API Reference

##### `functionName(param1, param2, options?)`

**Type**: `(param1: Type1, param2: Type2, options?: Options) => ReturnType`

**Purpose**: [One-line description of what it does]

**Parameters**:
- `param1` (Type1, required): [Description, valid values, constraints]
- `param2` (Type2, required): [Description]
- `options` (Options, optional): Configuration object
  - `option1` (string, default: `'default'`): [Description]
  - `option2` (boolean, default: `false`): [Description]

**Returns**: `ReturnType` - [Description of return value]

**Throws**:
- `TypeError`: When [condition]
- `ValidationError`: When [condition]

**Example**:
```javascript
import { functionName } from '@unrdf/[package-name]';

const result = functionName('value1', 42, {
  option1: 'custom',
  option2: true
});
// result = { ... }
```

**Runtime Compatibility**:
- ✅ Node.js ≥ 18.0.0
- ✅ Browser (ES2020+)
- ❌ BEAM/WASM (use [alternative])

**Performance**:
- Time Complexity: O([complexity])
- Space Complexity: O([complexity])
- Typical execution: [X]ms for [N] items

**Source**: [src/file.mjs:L](file://path#L)

**Tests**: [test/file.test.mjs](file://path)

---

##### `ClassName`

**Type**: Class

**Purpose**: [What this class represents and does]

**Constructor**:
```typescript
constructor(config: ClassConfig)
```

**Parameters**:
- `config` (ClassConfig): Configuration object
  - `property1` (string, required): [Description]
  - `property2` (number, optional): [Description]

**Properties**:
- `property` (Type, readonly): [Description]

**Methods**:

###### `methodName(param)`

**Type**: `(param: Type) => ReturnType`

**Description**: [What this method does]

**Parameters**: [Same format as function parameters]

**Returns**: [Return value description]

**Example**:
```javascript
const instance = new ClassName({ property1: 'value' });
const result = instance.methodName('param');
```

**Source**: [src/file.mjs:L](file://path#L)

---

#### Configuration Reference

**Environment Variables**:
| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `UNRDF_[SETTING]` | string | `'default'` | [What it controls] |
| `UNRDF_[OPTION]` | number | `1000` | [What it controls] |

**Configuration Object**:
```typescript
interface Config {
  setting1: string;        // Description
  setting2?: number;       // Optional: Description (default: 100)
  setting3: boolean;       // Description
}
```

**Example Configuration**:
```javascript
const config = {
  setting1: 'value',
  setting2: 500,
  setting3: true
};
```

---

#### Error Codes Reference

| Code | Name | Cause | Solution |
|------|------|-------|----------|
| `E001` | `ValidationError` | Invalid input parameter | Check parameter types and constraints |
| `E002` | `RuntimeError` | [Specific cause] | [Specific solution] |

**Example Error Handling**:
```javascript
try {
  const result = functionName(param);
} catch (error) {
  if (error.code === 'E001') {
    // Handle validation error
  }
}
```

---

#### Type Definitions

**Full TypeScript Definitions**:
```typescript
// Complete type definitions exported by the package
export interface Type1 {
  property1: string;
  property2: number;
}

export type ReturnType = {
  result: Type1;
  metadata: Metadata;
};
```

**JSDoc Annotations**:
```javascript
/**
 * @typedef {Object} Type1
 * @property {string} property1 - Description
 * @property {number} property2 - Description
 */
```

---

### 💡 Explanation (Understanding-Oriented)

#### Why [Package Name] Exists

**The Problem**: [What problem does this package solve?]

In traditional [domain], developers face [specific challenges]:
- Challenge 1: [Description]
- Challenge 2: [Description]
- Challenge 3: [Description]

**The Solution**: [How this package addresses the problem]

[Package Name] takes a different approach based on [principle/theory]:
1. [Key insight 1]
2. [Key insight 2]
3. [Key insight 3]

**Real-World Example**:
Consider a scenario where [concrete example]...

**Benefits**:
- ✅ [Benefit 1]: [Why this matters]
- ✅ [Benefit 2]: [Why this matters]
- ✅ [Benefit 3]: [Why this matters]

**Trade-offs**:
| Aspect | Traditional Approach | Our Approach | Why We Chose This |
|--------|---------------------|--------------|-------------------|
| [Aspect 1] | [Description] | [Description] | [Rationale] |
| [Aspect 2] | [Description] | [Description] | [Rationale] |

---

#### Architecture & Design Decisions

**System Architecture**:

```
┌─────────────────────────────────────────┐
│          Public API Layer               │
│  (functionName, ClassName, etc.)        │
└──────────────┬──────────────────────────┘
               │
┌──────────────▼──────────────────────────┐
│       Core Processing Layer             │
│  (Internal algorithms, state mgmt)      │
└──────────────┬──────────────────────────┘
               │
┌──────────────▼──────────────────────────┐
│      Data/Storage Layer                 │
│  (Store, persistence, caching)          │
└─────────────────────────────────────────┘
```

**Component Interactions**:
1. [Component A] receives input and validates via [validation strategy]
2. [Component B] processes data using [algorithm/pattern]
3. [Component C] stores results in [storage mechanism]

**Design Decisions**:

##### ADR-001: [Decision Title]

**Context**: [What situation led to needing this decision?]

**Decision**: We chose to [decision made].

**Rationale**:
- **Pro**: [Benefit 1]
- **Pro**: [Benefit 2]
- **Con**: [Trade-off 1] - We accepted this because [reason]
- **Con**: [Trade-off 2] - Mitigated by [mitigation]

**Alternatives Considered**:
1. **[Alternative 1]**: Rejected because [reason]
2. **[Alternative 2]**: Rejected because [reason]

**Consequences**:
- Users must [implication 1]
- Performance is [impact]
- Complexity is [impact]

**Evidence**: [Link to benchmark, test, or proof]

---

##### ADR-002: [Another Key Decision]

[Same structure]

---

#### Core Concepts

##### Concept: [Important Concept 1]

**Definition**: [Clear, precise definition]

**Why It Matters**: [Practical significance]

**How It Works**:
[Detailed explanation with diagrams if needed]

```
[ASCII diagram or pseudocode if helpful]
```

**Example**:
```javascript
// Concrete example demonstrating the concept
const example = demonstrateConcept();
```

**Common Misconceptions**:
- ❌ **Myth**: [Common misunderstanding]
  - **Reality**: [Correct understanding]
- ❌ **Myth**: [Another misconception]
  - **Reality**: [Correct understanding]

**Related Concepts**:
- [Concept 2]: [How they relate]
- [Concept 3]: [How they relate]

---

##### Concept: [Important Concept 2]

[Same structure]

---

#### Performance Model

**Theoretical Performance**:

Based on [algorithm/data structure], the package exhibits:
- Time Complexity: O([complexity]) for [operation]
- Space Complexity: O([complexity])
- Scalability: [Description]

**Empirical Benchmarks**:

| Operation | Dataset Size | Execution Time | Memory |
|-----------|--------------|----------------|--------|
| [Op 1] | 1K items | [X]ms | [Y]MB |
| [Op 1] | 10K items | [X]ms | [Y]MB |
| [Op 1] | 100K items | [X]ms | [Y]MB |

**Performance Characteristics**:
- 40% faster than [alternative] for [workload]
- 60% less memory than [alternative] for [workload]
- Scales linearly up to [N] items, then [behavior]

**Optimization Strategies**:
1. **Caching**: [What is cached, eviction policy]
2. **Indexing**: [What is indexed, update strategy]
3. **Batching**: [When batching is used, batch sizes]

**Verification**:
```bash
# Run benchmarks
timeout 30s node packages/[name]/benchmark/run.mjs
```

**Source**: [benchmark/results.md](file://path)

---

#### Integration Patterns

**Pattern 1: [Integration Scenario]**

**Use Case**: When you need to [specific use case]

**Architecture**:
```
[Diagram showing how components integrate]
```

**Implementation**:
```javascript
// Complete working example
import { capability } from '@unrdf/[package]';
import { other } from '@unrdf/other-package';

const integrated = capability(other.data());
```

**Best Practices**:
- ✅ Do: [Recommendation]
- ❌ Don't: [Anti-pattern]

**Example Projects**:
- [Project Name]: [Link to real-world usage]

---

#### When to Use vs Alternatives

**Use [Package Name] when**:
- ✅ You need [specific requirement]
- ✅ Your use case involves [scenario]
- ✅ You value [quality attribute] over [trade-off]

**Consider alternatives when**:
- 🤔 [Alternative Package]: Better if you need [different requirement]
- 🤔 [Another Alternative]: Better if you prioritize [different quality]

**Comparison Matrix**:

| Feature | [This Package] | [Alternative 1] | [Alternative 2] |
|---------|----------------|-----------------|-----------------|
| [Feature 1] | ✅ Excellent | ⚠️ Partial | ❌ Not supported |
| [Feature 2] | ✅ Built-in | ❌ Requires plugin | ✅ Built-in |
| Performance | [Metric] | [Metric] | [Metric] |
| Ease of Use | [Rating] | [Rating] | [Rating] |

**Migration Guides**:
- From [Alternative]: See [migration-from-X.md]
- To [Alternative]: See [migration-to-Y.md]

---

## Runtime Compatibility Matrix

| Capability | Node.js | Browser | BEAM/WASM | Notes |
|------------|---------|---------|-----------|-------|
| [Capability 1] | ✅ ≥18.0 | ✅ ES2020+ | ⏳ Planned | [Specific notes] |
| [Capability 2] | ✅ ≥18.0 | ❌ Not supported | ❌ Not supported | Requires filesystem |
| [Capability 3] | ✅ ≥18.0 | ✅ ES2020+ | ✅ Supported | Universal |

**Legend**:
- ✅ Fully supported
- ⏳ Planned/In progress
- ❌ Not supported
- ⚠️ Partial support (see notes)

**Browser Considerations**:
- Requires: [Specific browser features]
- Polyfills: [Required polyfills if any]
- Bundle size: [Size impact]

**Node.js Considerations**:
- Native modules: [Any native dependencies]
- ESM-only: Requires `"type": "module"` in package.json

---

## Examples Index

### Basic Examples

**Example 1: [Simple Use Case]**
- **File**: [examples/01-basic.mjs](file://path)
- **What it demonstrates**: [Description]
- **Run**: `node examples/01-basic.mjs`
- **Expected output**: [Description]

**Example 2: [Another Use Case]**
- **File**: [examples/02-intermediate.mjs](file://path)
- **What it demonstrates**: [Description]
- **Run**: `node examples/02-intermediate.mjs`

### Advanced Examples

**Example 3: [Complex Integration]**
- **File**: [examples/03-advanced.mjs](file://path)
- **What it demonstrates**: [Description]
- **Prerequisites**: [Required setup]
- **Run**: `node examples/03-advanced.mjs`

### Production Examples

**Example 4: [Production Pattern]**
- **File**: [examples/04-production.mjs](file://path)
- **What it demonstrates**: [Description]
- **Notes**: [Production considerations]

---

## Testing & Verification

### Test Suite

**Test Coverage**:
- Lines: [XX]%
- Branches: [XX]%
- Functions: [XX]%
- Statements: [XX]%

**Test Structure**:
```
test/
├── unit/                # Unit tests (fast)
│   ├── capability1.test.mjs
│   └── capability2.test.mjs
├── integration/         # Integration tests
│   └── full-flow.test.mjs
└── benchmark/           # Performance tests
    └── performance.test.mjs
```

**Run Tests**:
```bash
# All tests
timeout 5s pnpm test

# Specific test file
timeout 5s node test/unit/capability1.test.mjs

# Integration tests
timeout 10s pnpm test:integration

# Benchmarks
timeout 30s pnpm test:bench
```

### OTEL Validation

**Observability Coverage**:
- OTEL Score: [XX]/100
- Spans: [N] spans covering [operations]
- Metrics: [N] metrics tracked

**Run Validation**:
```bash
timeout 5s node validation/run-all.mjs comprehensive
grep "Score:" validation-output.log  # Must be ≥80/100
```

**Validation Evidence**: [validation/results.md](file://path)

---

## Evidence & Verification

### Source Code References

All capability atoms are traceable to source:
- [Capability 1]: [src/file.mjs:L](file://path#L)
- [Capability 2]: [src/file.mjs:L](file://path#L)

### Test Evidence

All claims verified by tests:
- [Feature 1]: [test/file.test.mjs:L](file://path#L) - Passing
- [Feature 2]: [test/file.test.mjs:L](file://path#L) - Passing

### Benchmark Evidence

Performance claims verified:
- [Claim 1]: [benchmark/results.md](file://path) - [Measured result]
- [Claim 2]: [benchmark/results.md](file://path) - [Measured result]

### Verification Commands

**Quick Verification** (< 5 seconds):
```bash
# Run all tests
timeout 5s pnpm test

# Check exports
node -e "import('@unrdf/[package]').then(m => console.log(Object.keys(m)))"

# Verify examples
timeout 5s node examples/01-basic.mjs
```

**Full Verification** (< 30 seconds):
```bash
# Tests + OTEL validation + benchmarks
timeout 30s pnpm run verify:all
```

---

## Migration & Upgrade Guides

### From v[X] to v[Y]

**Breaking Changes**:
1. **[Change 1]**: [What changed]
   - **Before**: `oldAPI()`
   - **After**: `newAPI()`
   - **Migration**: [Step-by-step migration]

2. **[Change 2]**: [What changed]
   - **Before**: [Old pattern]
   - **After**: [New pattern]
   - **Migration**: [Step-by-step migration]

**Deprecations**:
- `oldFunction()`: Use `newFunction()` instead (removed in v[Z])

**New Features**:
- [Feature 1]: [Description and usage]
- [Feature 2]: [Description and usage]

**Migration Checklist**:
- [ ] Update package version: `pnpm add @unrdf/[package]@[version]`
- [ ] Replace [deprecated API 1]
- [ ] Replace [deprecated API 2]
- [ ] Run tests: `pnpm test`
- [ ] Verify no runtime warnings

---

## Troubleshooting

### Common Issues

#### Issue: [Common Problem 1]

**Symptoms**: [How it manifests]

**Cause**: [Root cause]

**Solution**:
```javascript
// Fixed code
```

**Prevention**: [How to avoid this issue]

---

#### Issue: [Common Problem 2]

[Same structure]

---

### Debug Mode

Enable debug logging:
```bash
DEBUG=unrdf:[package]* node your-script.mjs
```

Output interpretation:
- `[INFO]`: [What these messages mean]
- `[WARN]`: [When to investigate]
- `[ERROR]`: [Action required]

---

## FAQ

**Q: [Common question 1]?**

A: [Clear, actionable answer with code example if relevant]

---

**Q: [Common question 2]?**

A: [Answer]

---

**Q: When should I use [this] vs [alternative]?**

A: Use [this] when [criteria]. Use [alternative] when [different criteria]. See [Explanation: When to Use vs Alternatives](#when-to-use-vs-alternatives) for detailed comparison.

---

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for:
- Code style guidelines
- Test requirements
- PR process
- Development setup

**Package-Specific Guidelines**:
- [Specific guideline 1]
- [Specific guideline 2]

---

## Cross-References

### Related Packages
- **@unrdf/[related-package-1]**: [How it relates]
- **@unrdf/[related-package-2]**: [How it relates]

### External Resources
- [Official Spec]: [URL]
- [Research Paper]: [Citation and URL]
- [Community Examples]: [URL]

### Internal Documentation
- 📚 Tutorial: [Link to related tutorial]
- 🛠️ How-To: [Link to related guide]
- 📖 Reference: [Link to API docs]
- 💡 Explanation: [Link to concept docs]

---

## Changelog

See [CHANGELOG.md](./CHANGELOG.md) for detailed version history.

**Recent Changes** (v[VERSION]):
- [Feature]: [Description]
- [Fix]: [Description]
- [Breaking]: [Description]

---

## License

[LICENSE] - See [LICENSE.md](../../LICENSE.md)

---

## Appendix

### Glossary

**[Term 1]**: [Definition]

**[Term 2]**: [Definition]

### Notation Conventions

- `code`: Inline code or identifier
- **Bold**: Important concepts
- *Italic*: Emphasis
- ✅/❌/⚠️: Status indicators

### Evidence Standards

All documentation claims must include:
1. **Code reference**: File and line number
2. **Test proof**: Link to passing test
3. **Verification command**: Reproducible command

---

**Document Metadata**:
- **Template Version**: 1.0.0
- **Generated**: [DATE]
- **Maintainer**: [@unrdf/core-team]
- **Last Review**: [DATE]
- **Next Review**: [DATE + 90 days]
