---
title: "How to [Task]"
description: "Step-by-step guide to [accomplish specific task] with UNRDF"
category: "how-to"
audience: "intermediate"
version: "6.0.0-alpha.1"
last_updated: "YYYY-MM-DD"
estimated_time: "X minutes"
proof:
  source:
    - "src/path/to/implementation.mjs"
  hash: "sha256-hash-placeholder"
  confidence: 0.90
related:
  - path: "/docs/reference/api/relevant-api.md"
    title: "API Reference"
  - path: "/docs/how-to/related-guide.md"
    title: "Related Guide"
---

# How to [Task]

## Problem

[Clear description of the problem this guide solves]

**Use this guide when you need to:**
- [Scenario 1]
- [Scenario 2]
- [Scenario 3]

---

## Solution

[Brief overview of the approach - 1-2 sentences]

---

## Prerequisites

- UNRDF 6.0.0-alpha.1 or later
- [Other requirement]
- [Other requirement]

---

## Steps

### 1. [Step title]

[Explanation of what this step does]

```javascript
// Code example
import { something } from 'unrdf';

const result = something();
```

### 2. [Step title]

[Explanation]

```javascript
// Code example
```

### 3. [Step title]

[Explanation]

```javascript
// Code example
```

---

## Complete Example

Here's a complete, working example you can copy and use:

```javascript
/**
 * Complete example: [Task description]
 *
 * Run with: node example.mjs
 */

import { something } from 'unrdf';

async function main() {
  // Step 1
  const step1 = something();

  // Step 2
  const step2 = await step1.doSomething();

  // Step 3
  console.log('Result:', step2);
}

main().catch(console.error);
```

**Expected output:**
```
Result: [expected output]
```

---

## Variations

### Variation A: [Different scenario]

If you need to [different requirement]:

```javascript
// Modified approach
```

### Variation B: [Another scenario]

For [another use case]:

```javascript
// Alternative approach
```

---

## Troubleshooting

### Error: "[Error message]"

**Cause:** [Why this happens]

**Fix:** [How to resolve]

```javascript
// Fix example
```

---

## Best Practices

- **Do:** [Recommendation]
- **Do:** [Recommendation]
- **Avoid:** [Anti-pattern]
- **Avoid:** [Anti-pattern]

---

## Proof

This guide was validated against:

- src/path/to/implementation.mjs

```json
{
  "sources": ["src/path/to/implementation.mjs"],
  "hash": "sha256-hash-placeholder",
  "confidence": 0.90,
  "generatedAt": "YYYY-MM-DD"
}
```

