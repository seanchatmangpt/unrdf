# Hook Chains Example

This example demonstrates how to use **@unrdf/hooks** for creating sequential hook chains that validate and transform RDF quads through multiple stages.

## Features

- **Sequential Execution**: Hooks execute in defined order
- **Progressive Transformation**: Quads transform through pipeline stages
- **Early Termination**: Chain stops on first validation failure
- **Result Aggregation**: Complete execution history and results
- **Composable Patterns**: Mix and match hooks for different pipelines

## Installation

```bash
pnpm install
```

## Usage

Run the example:

```bash
pnpm start
```

Run tests:

```bash
pnpm test
```

## Example Output

```
🔗 Hook Chains Example

============================================================

🧹 Data Cleaning Chain
────────────────────────────────────────────────────────────
Input:   Alice   Smith
       "  Alice   Smith  "
Status: ✅ SUCCESS
Steps: 3/3 passed
Output: Alice Smith
        "Alice Smith"

Chain execution:
  1. ✅ 🔄 trim-literals
  2. ✅ 🔄 normalize-whitespace
  3. ✅    validate-literal-length

🔍 Quality Assurance Chain
────────────────────────────────────────────────────────────
Input: Literal with 1500 characters
Status: ❌ FAILED
Steps: 3/4 passed
Failed at: validate-literal-length
Reason: validation failed

Chain execution (early termination):
  1. ✅ standard-validation
  2. ✅ validate-iris
  3. ✅ validate-iri-format
  4. ❌ validate-literal-length

⚙️  Complete Processing Chain
────────────────────────────────────────────────────────────
Input:
  Object: "  Chuck  "
  Graph: DefaultGraph

Status: ✅ SUCCESS
Steps: 5/5 passed

Output:
  Object: "Chuck"
  Graph: NamedNode
  Graph IRI: http://example.org/provenance/1733288888888

Chain execution:
  1. ✅    validate-iris
  2. ✅ 🔄 normalize-whitespace
      └─ Transformation applied
  3. ✅    validate-literal-length
  4. ✅ 🔄 add-provenance
      └─ Transformation applied
  5. ✅    final-validation

============================================================
✨ Hook Chains Example Complete
```

## Hook Chain Patterns

### 1. Data Cleaning Chain

Clean and normalize messy data:

```javascript
const dataCleaningChain = [
  builtinHooks.trimLiterals,       // Remove leading/trailing whitespace
  normalizeWhitespace,              // Collapse multiple spaces
  validateLiteralLength,            // Ensure valid length
];

const result = executeHookChain(dataCleaningChain, store, quad);
```

### 2. Quality Assurance Chain

Comprehensive validation pipeline:

```javascript
const qualityAssuranceChain = [
  builtinHooks.standardValidation,  // RDF structure
  validateIRIs,                      // IRI well-formedness
  builtinHooks.validateIRIFormat,   // URL validation
  validateLiteralLength,             // Length constraints
];

const result = executeHookChain(qualityAssuranceChain, store, quad);
```

### 3. Complete Processing Chain

Full pipeline with validation and transformation:

```javascript
const completeProcessingChain = [
  validateIRIs,                      // 1. Validate structure
  normalizeWhitespace,               // 2. Clean data
  validateLiteralLength,             // 3. Check constraints
  addProvenance,                     // 4. Add metadata
  finalValidation,                   // 5. Final checks
];

const result = executeHookChain(completeProcessingChain, store, quad);
```

## Custom Hooks

### Validation Hook

```javascript
const validateIRIs = defineHook({
  name: 'validate-iris',
  trigger: 'before-add',
  validate: quad => {
    const validateIRI = term => {
      if (term.termType !== 'NamedNode') return true;
      try {
        new URL(term.value);
        return true;
      } catch {
        return false;
      }
    };

    return validateIRI(quad.subject) &&
           validateIRI(quad.predicate) &&
           validateIRI(quad.object);
  },
});
```

### Transformation Hook

```javascript
const normalizeWhitespace = defineHook({
  name: 'normalize-whitespace',
  trigger: 'before-add',
  transform: quad => {
    if (quad.object.termType !== 'Literal') {
      return quad;
    }

    const normalized = quad.object.value
      .trim()
      .replace(/\s+/g, ' ');

    return DataFactory.quad(
      quad.subject,
      quad.predicate,
      DataFactory.literal(
        normalized,
        quad.object.language || quad.object.datatype
      ),
      quad.graph
    );
  },
});
```

## Chain Execution

### Success Case

```javascript
const result = executeHookChain(chain, store, quad);

if (result.success) {
  console.log('All hooks passed!');
  console.log('Transformed quad:', result.quad);
  console.log('Steps executed:', result.results.length);
}
```

### Failure Case (Early Termination)

```javascript
const result = executeHookChain(chain, store, invalidQuad);

if (!result.success) {
  const failedHook = result.results.find(r => !r.passed);
  console.log('Failed at:', failedHook.hookName);
  console.log('Reason:', failedHook.reason);
  console.log('Steps before failure:', result.results.length);
}
```

### Tracking Transformations

```javascript
const result = executeHookChain(chain, store, quad);

result.results.forEach((r, i) => {
  console.log(`Step ${i + 1}: ${r.hookName}`);
  if (r.transformed) {
    console.log('  → Transformation applied');
  }
  if (!r.passed) {
    console.log('  → Validation failed:', r.reason);
  }
});
```

## Testing

The example includes comprehensive tests:

- Individual hook behavior
- Chain execution order
- Early termination on failures
- Progressive transformations
- Result aggregation

Run tests with:

```bash
pnpm test
```

## Learn More

- [@unrdf/hooks Documentation](../../README.md)
- [Policy Hooks Example](../policy-hooks/)
- [UNRDF Core](../../../core/)

## License

MIT
