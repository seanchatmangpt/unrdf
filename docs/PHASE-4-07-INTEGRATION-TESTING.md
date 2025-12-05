# Phase 4.7: Integration Testing - Dual Runtime Validation

## Overview

Integration testing ensures your system behaves identically on both runtimes. This gives confidence that development in the browser will work in production.

Three testing strategies:

1. **Golden Tests** - Identical test vectors run on both runtimes
2. **Fault Injection** - Inject failures, verify recovery is identical
3. **Real Node Gating** - Final validation against actual OTP node

---

## 1. Golden Tests

### Concept

Create a set of "golden" test vectors - sequences of commands and expected responses:

```json
{
  "name": "Query with LIMIT",
  "steps": [
    {
      "send": {
        "type": "command",
        "payload": {
          "action": "executeQuery",
          "params": {
            "query": "SELECT ?s WHERE { ?s ?p ?o } LIMIT 10"
          }
        }
      },
      "expect": {
        "type": "response",
        "payload": {
          "success": true,
          "count": 10
        }
      }
    }
  ]
}
```

### Test Suite Setup

```typescript
// test/integration.test.ts
import { ProtocolClient } from '../client';
import * as fs from 'fs';

interface TestVector {
  name: string;
  setup?: any;
  steps: {
    send: Command;
    expect: Response | ResponseMatcher;
  }[];
}

async function runGoldenTest(client: ProtocolClient, test: TestVector) {
  console.log(`Running: ${test.name}`);

  for (const step of test.steps) {
    const response = await client.send(step.send);

    if (!matchesExpected(response, step.expect)) {
      throw new Error(
        `Test failed: ${test.name}\n` +
        `Expected: ${JSON.stringify(step.expect, null, 2)}\n` +
        `Got: ${JSON.stringify(response, null, 2)}`
      );
    }
  }

  console.log(`✓ Passed: ${test.name}`);
}

// Load all golden tests
const testVectors: TestVector[] = JSON.parse(
  fs.readFileSync('test/golden-vectors.json', 'utf-8')
);

// Run against both runtimes
describe('Golden Tests - Real Backend', () => {
  let client: ProtocolClient;

  beforeAll(async () => {
    client = new ProtocolClient('production');
    await client.connect();
  });

  afterAll(async () => {
    await client.disconnect();
  });

  testVectors.forEach(test => {
    it(test.name, async () => {
      await runGoldenTest(client, test);
    });
  });
});

describe('Golden Tests - AtomVM Simulation', () => {
  let client: ProtocolClient;

  beforeAll(async () => {
    client = new ProtocolClient('development');
    await client.connect();
  });

  afterAll(async () => {
    await client.disconnect();
  });

  testVectors.forEach(test => {
    it(test.name, async () => {
      await runGoldenTest(client, test);
    });
  });
});
```

### Golden Test Vectors

Create `test/golden-vectors.json`:

```json
[
  {
    "name": "Empty query",
    "steps": [
      {
        "send": {
          "type": "command",
          "id": "test-1",
          "payload": {
            "action": "executeQuery",
            "params": { "query": "SELECT * WHERE { ?s ?p ?o }" }
          }
        },
        "expect": {
          "type": "response",
          "id": "test-1",
          "payload": {
            "success": true,
            "data": [],
            "count": 0
          }
        }
      }
    ]
  },
  {
    "name": "Invalid query returns error",
    "steps": [
      {
        "send": {
          "type": "command",
          "id": "test-2",
          "payload": {
            "action": "executeQuery",
            "params": { "query": "INVALID SPARQL" }
          }
        },
        "expect": {
          "type": "error",
          "id": "test-2",
          "payload": {
            "error_code": "VALIDATION_ERROR"
          }
        }
      }
    ]
  },
  {
    "name": "Add triple and query it",
    "steps": [
      {
        "send": {
          "type": "command",
          "id": "test-3a",
          "payload": {
            "action": "addTriple",
            "params": {
              "subject": "http://example.com/s",
              "predicate": "http://example.com/p",
              "object": "http://example.com/o"
            }
          }
        },
        "expect": {
          "type": "response",
          "id": "test-3a",
          "payload": { "success": true }
        }
      },
      {
        "send": {
          "type": "command",
          "id": "test-3b",
          "payload": {
            "action": "executeQuery",
            "params": { "query": "SELECT * WHERE { ?s ?p ?o }" }
          }
        },
        "expect": {
          "type": "response",
          "id": "test-3b",
          "payload": {
            "success": true,
            "count": 1
          }
        }
      }
    ]
  }
]
```

---

## 2. Fault Injection

### Purpose

Verify that both runtimes handle failures identically.

### Fault Framework

```erlang
% lib/unrdf_fault_injection/src/fault_injector.erl
-module(fault_injector).

% Enable/disable faults at runtime
-record(fault_config, {
    enabled = false :: boolean(),
    fault_type = none :: timeout | error | unavailable,
    probability = 100 :: 0..100  % Percentage
}).

% Inject a fault
inject_fault(FaultType) ->
    case should_trigger_fault(FaultType) of
        true ->
            trigger_fault(FaultType);
        false ->
            ok
    end.

trigger_fault(timeout) ->
    throw({error, query_timeout});

trigger_fault(error) ->
    throw({error, internal_error});

trigger_fault(unavailable) ->
    throw({error, service_unavailable}).

should_trigger_fault(FaultType) ->
    case get_fault_config(FaultType) of
        #{enabled := true, probability := P} ->
            random:uniform(100) =< P;
        _ ->
            false
    end.
```

### JavaScript Wrapper

```typescript
// test/fault-injector.ts
export class FaultInjectorClient {
  constructor(private client: ProtocolClient) {}

  // Inject timeout fault
  async withTimeoutFault(fn: () => Promise<any>) {
    await this.client.send({
      type: 'command',
      payload: {
        action: 'injectFault',
        params: { faultType: 'timeout', probability: 100 }
      }
    });

    try {
      return await fn();
    } finally {
      await this.client.send({
        type: 'command',
        payload: {
          action: 'injectFault',
          params: { faultType: 'timeout', probability: 0 }
        }
      });
    }
  }

  // Inject error fault
  async withErrorFault(fn: () => Promise<any>) {
    // Similar pattern...
  }
}
```

### Fault Injection Tests

```typescript
// test/fault-injection.test.ts
describe('Fault Injection - Both Runtimes', () => {
  let client: ProtocolClient;
  let faultClient: FaultInjectorClient;

  beforeEach(async () => {
    client = new ProtocolClient();
    await client.connect();
    faultClient = new FaultInjectorClient(client);
  });

  it('handles timeout gracefully', async () => {
    await expect(
      faultClient.withTimeoutFault(() =>
        client.send({
          type: 'command',
          payload: { action: 'slowQuery' },
          meta: { timeout_ms: 1000 }
        })
      )
    ).rejects.toThrow('QUERY_TIMEOUT');
  });

  it('retries on transient errors', async () => {
    let attempts = 0;

    const result = await faultClient.withErrorFault(async () => {
      attempts++;
      return client.send({
        type: 'command',
        payload: { action: 'query' }
      });
    });

    // Both runtimes should retry identically
    expect(result.payload.success).toBe(true);
  });

  it('recovers from connection loss', async () => {
    // Simulate disconnect
    await client.disconnect();

    // Should reconnect automatically
    await new Promise(resolve => setTimeout(resolve, 2000));

    const response = await client.send({
      type: 'command',
      payload: { action: 'health' }
    });

    expect(response.payload.success).toBe(true);
  });
});
```

---

## 3. Real Node Gating in CI

### Purpose

Before deploying, validate against an actual OTP node to catch subtle incompatibilities.

### CI/CD Pipeline

```yaml
# .github/workflows/integration-test.yml
name: Integration Tests

on: [push, pull_request]

jobs:
  test-both-runtimes:
    runs-on: ubuntu-latest

    services:
      erlang-otp:
        image: erlang:latest
        ports:
          - 8080:8080
        options: >-
          --health-cmd "curl http://localhost:8080/health"
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5

    steps:
      - uses: actions/checkout@v3

      - name: Install Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'

      - name: Install dependencies
        run: npm install

      - name: Build Erlang
        run: |
          cd erlang
          rebar3 compile

      - name: Start OTP Node
        run: |
          cd erlang
          rebar3 release
          _build/default/rel/unrdf_otp/bin/unrdf_otp daemon

      - name: Golden Tests Against OTP
        run: npm run test:integration:otp
        env:
          TARGET_RUNTIME: otp
          ERLANG_SERVER_URL: ws://localhost:8080/rdf-protocol

      - name: Golden Tests Against AtomVM
        run: npm run test:integration:atomvm
        env:
          TARGET_RUNTIME: atomvm

      - name: Fault Injection Tests
        run: npm run test:faults

      - name: Real Node Gate
        if: github.event_name == 'push' && github.ref == 'refs/heads/main'
        run: npm run test:real-node-gate

  deploy:
    needs: test-both-runtimes
    if: github.ref == 'refs/heads/main'
    runs-on: ubuntu-latest

    steps:
      - name: Deploy to Production
        run: |
          # Deploy only after all tests pass
          npm run deploy:production
```

### Real Node Gate Test

```typescript
// test/real-node-gate.test.ts
// Only runs when TARGET_RUNTIME === 'real-otp'

describe('Real OTP Node Gate', () => {
  let client: ProtocolClient;

  beforeAll(async () => {
    if (process.env.TARGET_RUNTIME !== 'real-otp') {
      skip();
    }

    client = new ProtocolClient('production');
    await client.connect();
  });

  afterAll(async () => {
    await client.disconnect();
  });

  it('executes queries against real OTP node', async () => {
    const response = await client.executeQuery(
      'SELECT ?s WHERE { ?s ?p ?o }'
    );
    expect(Array.isArray(response)).toBe(true);
  });

  it('all golden tests pass against real node', async () => {
    // Run all golden test vectors
    const testVectors = JSON.parse(fs.readFileSync('test/golden-vectors.json'));

    for (const test of testVectors) {
      await runGoldenTest(client, test);
    }
  });

  it('handles load gracefully', async () => {
    const promises = [];

    for (let i = 0; i < 100; i++) {
      promises.push(
        client.executeQuery('SELECT * WHERE { ?s ?p ?o }')
      );
    }

    const results = await Promise.all(promises);
    expect(results.length).toBe(100);
  });
});
```

---

## 4. Reporting Test Results

### Test Matrix

Create a matrix showing which tests passed on which runtimes:

```
Test Name                           | AtomVM | OTP | Status
─────────────────────────────────────────────────────────────
Query with LIMIT                    | ✓      | ✓   | PASS
Invalid query returns error         | ✓      | ✓   | PASS
Add triple and query it             | ✓      | ✓   | PASS
Timeout fault injection             | ✓      | ✓   | PASS
Error recovery                      | ✓      | ✓   | PASS
Load test (100 concurrent)          | ✓      | ✓   | PASS
─────────────────────────────────────────────────────────────
Total: 6 passed, 0 failed           | 100%   | 100%| READY
```

### Generate Report

```typescript
// test/report-generator.ts
export async function generateTestReport() {
  const results = {
    timestamp: new Date().toISOString(),
    atomvm: await runTests('development'),
    otp: await runTests('production'),
    faults: await runFaultTests(),
  };

  const markdown = `
# Integration Test Report

**Date:** ${results.timestamp}

## Summary

- AtomVM Tests: ${results.atomvm.passed}/${results.atomvm.total} ✓
- OTP Tests: ${results.otp.passed}/${results.otp.total} ✓
- Fault Injection: ${results.faults.passed}/${results.faults.total} ✓

## Results by Runtime

### AtomVM (Browser Simulation)
\`\`\`
${results.atomvm.details}
\`\`\`

### OTP (Production)
\`\`\`
${results.otp.details}
\`\`\`

## Verdict

${results.atomvm.passed === results.atomvm.total &&
  results.otp.passed === results.otp.total ?
  '✅ **Ready for deployment**' :
  '❌ **Tests failed - do not deploy**'}
  `;

  return markdown;
}
```

---

## Best Practices

### 1. Keep Tests Independent

```typescript
// ✓ Good: Each test is independent
it('creates a triple', async () => {
  const client = new ProtocolClient();
  await client.connect();
  const response = await client.send(addTripleCommand);
  expect(response.success).toBe(true);
  await client.disconnect();
});

// ✗ Bad: Test depends on previous test
let client: ProtocolClient;

it('creates a triple', () => {
  client = new ProtocolClient();
  client.connect();
});

it('queries the triple', () => {
  // Depends on previous test!
});
```

### 2. Use Same Random Seed

```typescript
// Both runtimes should use same random seed for determinism
const RANDOM_SEED = 12345;

function generateTestData() {
  const rng = seededRandom(RANDOM_SEED);
  // Generate identical data on both runtimes
}
```

### 3. Mock Time

```typescript
// Use a controllable clock instead of Date.now()
class TestClock {
  private time = 0;

  advance(ms: number) {
    this.time += ms;
  }

  now(): number {
    return this.time;
  }
}

// Both runtimes use same time progression
```

---

## See Also

- **01-PROTOCOL-DESIGN.md** - Protocol layer being tested
- **04-DUAL-ADAPTERS.md** - Adapter implementations
- **07-INTEGRATION-TESTING.md** - This document
