# Phase 4.6 (CORRECTED): JavaScript Setup - MJS + JSDoc

## Project Structure

```
my-app/
├── package.json
├── src/
│   ├── client.mjs                    # Protocol client
│   ├── app.mjs                       # Main application
│   └── adapters/
│       ├── real-adapter.mjs          # OTP adapter
│       └── simulated-adapter.mjs     # AtomVM adapter
├── test/
│   ├── adapters.test.mjs             # Adapter tests
│   └── golden.test.mjs               # Golden tests
├── docs/
│   └── PROTOCOL.md                   # Protocol documentation
└── .env                              # Configuration
```

---

## Step 1: Project Setup

### package.json

```json
{
  "name": "my-rdf-app",
  "version": "1.0.0",
  "type": "module",
  "description": "UNRDF + Erlang/OTP app with dual-runtime support",
  "main": "src/app.mjs",
  "scripts": {
    "dev": "MODE=development node src/app.mjs",
    "prod": "MODE=production node src/app.mjs",
    "test": "node --test test/**/*.test.mjs",
    "test:otp": "MODE=production npm test",
    "test:atomvm": "MODE=development npm test",
    "lint": "eslint src/ test/",
    "serve": "http-server"
  },
  "dependencies": {},
  "devDependencies": {
    "eslint": "^8.0.0"
  }
}
```

### .env Configuration

```bash
# Production (OTP node)
NODE_ENV=production
ERLANG_SERVER_URL=ws://localhost:8080/rdf-protocol

# Development (AtomVM browser)
# NODE_ENV=development
```

---

## Step 2: Create Adapter Interfaces (JSDoc)

### types.mjs

```javascript
// src/types.mjs
// All type definitions using JSDoc

/**
 * @typedef {Object} Command
 * @property {'command'} type
 * @property {string | number} id
 * @property {string} [timestamp]
 * @property {string} [version]
 * @property {Object} payload
 * @property {string} payload.action
 * @property {Object} [payload.params]
 * @property {Object} [meta]
 * @property {number} [meta.timeout_ms]
 * @property {string} [meta.correlation_id]
 */

/**
 * @typedef {Object} Response
 * @property {'response'} type
 * @property {string | number} id
 * @property {string} [timestamp]
 * @property {string} [version]
 * @property {Object} payload
 * @property {boolean} payload.success
 * @property {any} [payload.data]
 * @property {string} [payload.error]
 * @property {Array} [payload.errors]
 * @property {Object} [meta]
 * @property {number} [meta.duration_ms]
 * @property {string} [meta.execution_path]
 */

/**
 * @typedef {Object} Event
 * @property {'event'} type
 * @property {null} id
 * @property {string} [timestamp]
 * @property {Object} payload
 * @property {string} payload.event_type
 * @property {Object} payload.details
 * @property {Object} [meta]
 * @property {string} [meta.source]
 */

/**
 * @typedef {Object} ErrorMessage
 * @property {'error'} type
 * @property {string | number} id
 * @property {string} [timestamp]
 * @property {Object} payload
 * @property {string} payload.error_code
 * @property {string} payload.message
 * @property {Object} [payload.details]
 * @property {Object} [meta]
 * @property {boolean} [meta.retryable]
 * @property {number} [meta.retry_after_ms]
 * @property {string} [meta.error_id]
 */

/**
 * @typedef {Command | Response | Event | ErrorMessage} ProtocolMessage
 */

/**
 * @typedef {Object} HealthStatus
 * @property {'ok' | 'degraded' | 'down'} status
 * @property {string} [message]
 * @property {Object} [details]
 */

/**
 * @callback EventHandler
 * @param {Event} event
 * @returns {void}
 */

/**
 * @callback Unsubscribe
 * @returns {void}
 */

/**
 * @typedef {Object} ProtocolAdapter
 * @property {() => Promise<void>} connect
 * @property {() => Promise<void>} disconnect
 * @property {() => boolean} isConnected
 * @property {(command: Command) => Promise<Response>} send
 * @property {(topic: string, handler: EventHandler) => Unsubscribe} subscribe
 * @property {() => Promise<HealthStatus>} getHealth
 */

export {};
```

---

## Step 3: Implement Real Adapter

See **PHASE-4-04-DUAL-ADAPTERS-CORRECTED.mjs.md** for full implementation.

---

## Step 4: Implement Simulated Adapter

See **PHASE-4-04-DUAL-ADAPTERS-CORRECTED.mjs.md** for full implementation.

---

## Step 5: Create Protocol Client

See **PHASE-4-04-DUAL-ADAPTERS-CORRECTED.mjs.md** for full implementation.

---

## Step 6: Application Code

### Basic Usage

```javascript
// src/app.mjs

import { ProtocolClient } from './client.mjs';

/**
 * Main application function
 * Demonstrates basic RDF operations
 */
async function main() {
  const client = new ProtocolClient();

  try {
    // Connect (automatically selects real or simulated adapter)
    await client.connect();
    console.log('Connected to backend');

    // Execute query
    const results = await client.executeQuery(
      'SELECT ?s ?p WHERE { ?s ?p ?o } LIMIT 10'
    );
    console.log(`Found ${results.length} results`);

    // Add triple
    await client.addTriple(
      'http://example.com/subject',
      'http://example.com/property',
      'http://example.com/object'
    );
    console.log('Triple added');

    // Check health
    const health = await client.getHealth();
    console.log(`System status: ${health.status}`);

  } catch (error) {
    console.error('Error:', error.message);
  } finally {
    await client.disconnect();
  }
}

main();
```

### Advanced Usage

```javascript
// src/advanced.mjs

import { ProtocolClient } from './client.mjs';

/**
 * Advanced example with subscriptions
 */
async function advancedExample() {
  const client = new ProtocolClient();
  await client.connect();

  // Subscribe to data changes
  const unsubscribe = client.onDataChanged((event) => {
    console.log('Data changed event:');
    console.log('  Event type:', event.payload.event_type);
    console.log('  Details:', event.payload.details);
  });

  try {
    // Perform operations that trigger events
    await client.addTriple(
      'http://example.com/person',
      'http://example.com/name',
      'John Doe'
    );

    // Wait for events
    await new Promise(resolve => setTimeout(resolve, 5000));

  } finally {
    unsubscribe();
    await client.disconnect();
  }
}

advancedExample();
```

---

## Step 7: Unit Tests

### Adapter Tests

```javascript
// test/adapters.test.mjs

import { ProtocolClient } from '../src/client.mjs';

/**
 * Test protocol client with both adapters
 */
export async function testAdapters() {
  console.log('\n=== Testing Adapters ===\n');

  // Test both runtime modes
  const modes = ['production', 'development'];

  for (const mode of modes) {
    console.log(`Testing ${mode} mode...`);

    const client = new ProtocolClient(mode);

    try {
      await client.connect();

      // Test 1: Health check
      const health = await client.getHealth();
      if (health.status === 'ok' || health.status === 'degraded') {
        console.log(`  ✓ Health check passed`);
      } else {
        throw new Error('Health check failed');
      }

      // Test 2: Query execution
      try {
        const results = await client.executeQuery(
          'SELECT * WHERE { ?s ?p ?o } LIMIT 10'
        );
        console.log(`  ✓ Query execution passed (${results.length} results)`);
      } catch (error) {
        console.log(`  ! Query execution: ${error.message}`);
      }

      // Test 3: Error handling
      try {
        await client.executeQuery('INVALID SPARQL QUERY');
        throw new Error('Should have thrown error');
      } catch (error) {
        if (error.message.includes('VALIDATION_ERROR') ||
            error.message.includes('Syntax error')) {
          console.log(`  ✓ Error handling passed`);
        } else {
          console.log(`  ! Unexpected error: ${error.message}`);
        }
      }

      console.log(`${mode} mode: PASSED\n`);

    } catch (error) {
      console.error(`${mode} mode: FAILED - ${error.message}\n`);
    } finally {
      await client.disconnect();
    }
  }
}

// Run tests if this is main module
if (import.meta.url === `file://${process.argv[1]}`) {
  testAdapters().catch(console.error);
}

export default testAdapters;
```

### Golden Tests

```javascript
// test/golden.test.mjs

import { ProtocolClient } from '../src/client.mjs';

/**
 * Golden test: query with empty result
 */
async function testEmptyQuery(client) {
  const results = await client.executeQuery(
    'SELECT ?s WHERE { ?s <http://example.com/nonexistent> ?o }'
  );

  if (Array.isArray(results) && results.length === 0) {
    return true;
  }
  throw new Error(`Expected empty results, got ${results.length}`);
}

/**
 * Golden test: invalid query error
 */
async function testInvalidQuery(client) {
  try {
    await client.executeQuery('INVALID SPARQL');
    throw new Error('Should have thrown error');
  } catch (error) {
    if (error.message.includes('Syntax') ||
        error.message.includes('VALIDATION')) {
      return true;
    }
    throw error;
  }
}

/**
 * Run golden tests against both adapters
 */
export async function runGoldenTests() {
  console.log('\n=== Golden Tests (Both Adapters) ===\n');

  const tests = [
    ['Empty query', testEmptyQuery],
    ['Invalid query error', testInvalidQuery]
  ];

  const modes = ['production', 'development'];

  for (const mode of modes) {
    console.log(`${mode.toUpperCase()}`);

    const client = new ProtocolClient(mode);
    await client.connect();

    try {
      for (const [name, testFn] of tests) {
        try {
          await testFn(client);
          console.log(`  ✓ ${name}`);
        } catch (error) {
          console.error(`  ✗ ${name}: ${error.message}`);
        }
      }
    } finally {
      await client.disconnect();
    }

    console.log();
  }
}

// Run tests if this is main module
if (import.meta.url === `file://${process.argv[1]}`) {
  runGoldenTests().catch(console.error);
}

export default runGoldenTests;
```

---

## Step 8: Integration Testing

### Test Runner

```javascript
// test/runner.mjs

import testAdapters from './adapters.test.mjs';
import runGoldenTests from './golden.test.mjs';

/**
 * Run all integration tests
 */
async function runAllTests() {
  console.log('='.repeat(60));
  console.log('INTEGRATION TEST SUITE');
  console.log('='.repeat(60));

  try {
    // Run adapter tests
    await testAdapters();

    // Run golden tests
    await runGoldenTests();

    console.log('='.repeat(60));
    console.log('All tests completed');
    console.log('='.repeat(60));

  } catch (error) {
    console.error('Test suite failed:', error);
    process.exit(1);
  }
}

runAllTests();
```

---

## Step 9: Development Workflow

### Local Development (Browser + AtomVM)

```bash
# Terminal 1: Serve web app
npm run serve

# Terminal 2: Run with AtomVM simulation
MODE=development npm run dev
```

Access at `http://localhost:8080` - app uses simulated adapter.

### Production Testing (OTP Node)

```bash
# Terminal 1: Start OTP node
cd erlang && rebar3 release
_build/default/rel/unrdf_otp/bin/unrdf_otp console

# Terminal 2: Run against real OTP
MODE=production npm run test:otp
```

### Testing Both Simultaneously

```bash
# Run golden tests against both
npm run test:otp && npm run test:atomvm
```

---

## Key Design Decisions

### 1. No TypeScript

All code is standard JavaScript with JSDoc. Benefits:
- No build step required
- Simpler tooling
- Direct runtime execution
- Smaller bundle size

### 2. Pure Functions

All adapter methods are pure:
- No side effects in module scope
- Everything is a function
- Easy to test and reason about

### 3. Error Handling

Simple try-catch approach:
- No defensive code
- Errors propagate clearly
- Easy to understand failure modes

### 4. Private Fields

Use `#` prefix for private methods:
```javascript
#handleMessage(message) {
  // Private implementation
}
```

### 5. Module Exports

Each module exports only what's needed:
```javascript
export { ProtocolClient };
export { RealAdapter };
export { SimulatedAdapter };
```

---

## Troubleshooting

### Issue: "Module not found"

**Solution:** Ensure all imports use `.mjs` extension:
```javascript
// ✓ Correct
import { Client } from './client.mjs';

// ✗ Wrong
import { Client } from './client';
```

### Issue: "Cannot find module in production"

**Solution:** Verify `ERLANG_SERVER_URL` environment variable:
```bash
export ERLANG_SERVER_URL=ws://localhost:8080/rdf-protocol
```

### Issue: "AtomVM not loaded"

**Solution:** Ensure HTML loads AtomVM before app:
```html
<script src="atomvm.js"></script>
<script type="module" src="app.mjs"></script>
```

---

## See Also

- **04-DUAL-ADAPTERS-CORRECTED.mjs.md** - Full adapter implementations
- **01-PROTOCOL-DESIGN.md** - Protocol specification
- **07-INTEGRATION-TESTING.md** - Testing strategies
