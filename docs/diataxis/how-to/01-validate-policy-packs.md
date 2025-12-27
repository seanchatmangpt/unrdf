# How-To 01: Validate Policy Packs

**Objective:** Validate policy pack definitions before deployment to ensure correctness and avoid runtime errors.

**Audience:** Intermediate - developers implementing policy systems

**Prerequisites:**
- Understanding of SPARQL syntax
- Familiarity with SHACL validation
- **Capability Atoms:** `@unrdf/validation` (policy validation), `@unrdf/hooks` (policy hooks)

**Estimated Time:** 15 minutes

---

## Problem

You need to validate that your policy pack definitions are:
- Syntactically correct
- Semantically valid
- Have all required fields
- Reference existing resources
- Don't create circular dependencies

---

## Solution

Use UNRDF's built-in policy pack validator with comprehensive checks.

---

## Step-by-Step Guide

### 1. Install Validation Tools

**[Placeholder - Content to be filled]**

```bash
pnpm add @unrdf/validation
```

**Evidence:** Package at `/home/user/unrdf/packages/validation/`

---

### 2. Define Your Policy Pack

**[Placeholder - Content to be filled]**

```javascript
// policy-pack.mjs
export const policyPack = {
  name: 'approval-policies',
  version: '1.0.0',
  policies: [
    {
      id: 'require-approval',
      predicate: `ASK { ... }`
    }
  ]
};
```

**Evidence:** Policy pack schema at `/home/user/unrdf/packages/validation/src/policy-pack-schema.mjs`

---

### 3. Run Validation

**[Placeholder - Content to be filled]**

```javascript
import { validatePolicyPack } from '@unrdf/validation';

const result = await validatePolicyPack(policyPack, {
  strict: true,
  checkReferences: true,
  validateSparql: true
});

if (!result.valid) {
  console.error('Validation errors:', result.errors);
}
```

**Evidence:** Validation at `/home/user/unrdf/packages/validation/src/validate-policy-pack.mjs`

---

### 4. Fix Common Errors

**[Placeholder - Troubleshooting common validation errors]**

**Evidence:** Error messages at `/home/user/unrdf/packages/validation/src/error-messages.mjs`

---

### 5. Automate with CI

**[Placeholder - CI integration examples]**

```yaml
# .github/workflows/validate-policies.yml
name: Validate Policies
on: [push]
jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - run: pnpm validate:policies
```

**Evidence:** CI scripts at `/home/user/unrdf/scripts/validate-policies.sh`

---

## Complete Example

**[Placeholder - Link to complete example]**

**Evidence:** Full example at `/home/user/unrdf/examples/policy-pack-validation.mjs`

---

## Common Issues

**[Placeholder - Troubleshooting]**

- Issue: "SPARQL syntax error"
- Issue: "Circular dependency detected"
- Issue: "Missing required field"

---

## Related Guides

- **[How-To 02: Audit Decision Trail](./02-audit-decision-trail.md)** - Track policy decisions
- **[Tutorial 04: Implement Policy Gates](../tutorials/04-implement-policy-gates.md)** - Learn policy gates
- **[Reference: Policy Predicate Syntax](../reference/policy-predicate-syntax.md)** - Syntax reference

---

**Questions?** Check [TROUBLESHOOTING.md](/home/user/unrdf/docs/TROUBLESHOOTING.md) or file an issue.
