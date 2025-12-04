# Policy Hooks Example

This example demonstrates how to use **@unrdf/hooks** for implementing RDF data policies including access control, data validation, privacy, and provenance tracking.

## Features

- **ACL Policy**: Access control lists for trusted namespaces
- **Data Type Policy**: Strict type validation for RDF literals
- **Privacy Policy**: Automatic data redaction and transformation
- **Provenance Policy**: Require metadata for audit trails

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
🔒 Policy Hooks Example

============================================================

✅ Test 1: Trusted namespace
   Passed: 4/4
   Failed: 0/4

❌ Test 2: Untrusted namespace
   Passed: 2/4
   Failed: 2/4
   ❌ acl-policy: validation failed
   ❌ provenance-policy: validation failed

✅ Test 3: Valid age constraint
   Passed: 4/4
   Failed: 0/4

❌ Test 4: Invalid age constraint
   Passed: 3/4
   Failed: 1/4
   ❌ data-type-policy: validation failed

🔐 Test 5: Privacy policy transformation
   Status: PASSED
   Original: alice@example.org
   Transformed: [REDACTED]

❌ Test 6: Missing provenance
   Passed: 3/4
   Failed: 1/4
   ❌ provenance-policy: validation failed

============================================================
✨ Policy Hooks Example Complete
```

## Policy Hooks

### 1. ACL Policy

Validates that RDF quads come from trusted namespaces:

```javascript
const aclPolicy = defineHook({
  name: 'acl-policy',
  trigger: 'before-add',
  validate: quad => {
    const trustedNamespaces = [
      'http://example.org/',
      'http://xmlns.com/foaf/0.1/',
    ];
    // Check subject and predicate IRIs
    return trustedNamespaces.some(ns =>
      quad.subject.value.startsWith(ns) ||
      quad.predicate.value.startsWith(ns)
    );
  },
});
```

### 2. Data Type Policy

Enforces type constraints on specific predicates:

```javascript
const dataTypePolicy = defineHook({
  name: 'data-type-policy',
  trigger: 'before-add',
  validate: quad => {
    if (quad.predicate.value === 'http://xmlns.com/foaf/0.1/age') {
      const value = parseInt(quad.object.value, 10);
      return !isNaN(value) && value >= 0 && value <= 150;
    }
    return true;
  },
});
```

### 3. Privacy Policy

Transforms sensitive data automatically:

```javascript
const privacyPolicy = defineHook({
  name: 'privacy-policy',
  trigger: 'before-add',
  transform: quad => {
    if (quad.predicate.value === 'http://xmlns.com/foaf/0.1/mbox') {
      return DataFactory.quad(
        quad.subject,
        quad.predicate,
        DataFactory.literal('[REDACTED]'),
        quad.graph
      );
    }
    return quad;
  },
});
```

### 4. Provenance Policy

Requires named graphs for audit trails:

```javascript
const provenancePolicy = defineHook({
  name: 'provenance-policy',
  trigger: 'before-add',
  validate: quad => {
    return quad.graph && quad.graph.termType === 'NamedNode';
  },
});
```

## Registry Usage

Create and configure a policy registry:

```javascript
import { createHookRegistry, registerHook } from '@unrdf/hooks';

const registry = createHookRegistry();

registerHook(registry, aclPolicy);
registerHook(registry, dataTypePolicy);
registerHook(registry, privacyPolicy);
registerHook(registry, provenancePolicy);

// Execute all policies
const results = executeHooksByTrigger(registry, store, 'before-add', quad);
console.log(`Passed: ${results.passed.length}/${results.total}`);
```

## Testing

The example includes comprehensive tests:

- ACL policy validation
- Data type constraints
- Privacy transformations
- Provenance requirements
- Registry execution

Run tests with:

```bash
pnpm test
```

## Learn More

- [@unrdf/hooks Documentation](../../README.md)
- [Hook Chains Example](../hook-chains/)
- [UNRDF Core](../../../core/)

## License

MIT
