/**
 * Basic Knowledge Hook Example
 *
 * Demonstrates a simple health check hook that monitors system status.
 * This is the minimal example to get started with Knowledge Hooks.
 */

import { defineHook } from '@unrdf/hooks';
import { writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';

// Step 1: Create SPARQL query file
const queryContent = `
PREFIX ex: <http://example.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

ASK {
  ?service rdf:type ex:Service .
  ?service ex:status ?status .
  FILTER(?status != "healthy")
}
`;

// Save query to file
const queryPath = resolve(process.cwd(), 'hooks', 'health-check.ask.rq');
await writeFile(queryPath, queryContent, 'utf-8');

console.log(`✓ Created query file: ${queryPath}`);

// Step 2: Compute SHA256 hash (in production, use actual hash)
const mockHash = 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855';

// Step 3: Define the hook
const healthCheckHook = defineHook({
  meta: {
    name: 'system:health-check',
    description: 'Monitor system health and alert on issues',
    ontology: ['ex'],
  },

  channel: {
    graphs: ['urn:graph:system'],
    view: 'delta', // Watch for changes
  },

  when: {
    kind: 'sparql-ask',
    ref: {
      uri: 'file://hooks/health-check.ask.rq',
      sha256: mockHash,
      mediaType: 'application/sparql-query',
    },
  },

  determinism: {
    seed: 42, // Ensure deterministic execution
  },

  receipt: {
    anchor: 'none', // Use 'git-notes' for audit trails
  },

  // Pre-execution validation
  async before({ payload }) {
    console.log('[BEFORE] Validating payload...');

    if (!payload || typeof payload !== 'object') {
      return {
        cancel: true,
        reason: 'Invalid payload: must be an object',
      };
    }

    // Normalize payload
    return {
      ...payload,
      timestamp: Date.now(),
      validated: true,
    };
  },

  // Main execution
  async run({ payload, context: _context }) {
    console.log('[RUN] Health check triggered!');
    console.log('Payload:', JSON.stringify(payload, null, 2));

    // Simulate health check logic
    const healthStatus = {
      status: 'healthy',
      checks: {
        database: 'ok',
        cache: 'ok',
        api: 'ok',
      },
      timestamp: payload.timestamp,
    };

    // Return result
    return {
      result: healthStatus,
      assertions: [], // Could add RDF assertions here
    };
  },

  // Post-execution cleanup
  async after({ result, cancelled, reason }) {
    if (cancelled) {
      console.log(`[AFTER] Hook cancelled: ${reason}`);
      return {
        result: {
          finalStatus: 'cancelled',
          reason,
        },
      };
    }

    console.log('[AFTER] Health check completed');
    console.log('Result:', JSON.stringify(result, null, 2));

    // Audit logging
    return {
      result: {
        ...result,
        finalStatus: 'completed',
        audited: true,
      },
    };
  },
});

console.log('\n✓ Hook defined successfully!');
console.log('Hook name:', healthCheckHook.meta.name);
console.log('Condition type:', healthCheckHook.when.kind);

// Example: Execute hook manually
console.log('\n--- Testing Hook Execution ---\n');

// Simulate hook event
const testEvent = {
  name: 'manual-trigger',
  payload: {
    source: 'test',
    data: 'sample',
  },
  context: {
    env: {
      environment: 'development',
    },
  },
};

// Execute before
const beforeResult = await healthCheckHook.before(testEvent);
console.log('Before result:', beforeResult);

if (!beforeResult.cancel) {
  // Execute run
  testEvent.payload = beforeResult;
  const runResult = await healthCheckHook.run(testEvent);
  console.log('Run result:', runResult);

  // Execute after
  const afterResult = await healthCheckHook.after({
    ...testEvent,
    result: runResult.result,
    cancelled: false,
  });
  console.log('After result:', afterResult);
}

console.log('\n✓ Example complete!\n');

/**
 * Expected Output:
 *
 * ✓ Created query file: /path/to/hooks/health-check.ask.rq
 * ✓ Hook defined successfully!
 * Hook name: system:health-check
 * Condition type: sparql-ask
 *
 * --- Testing Hook Execution ---
 *
 * [BEFORE] Validating payload...
 * Before result: { source: 'test', data: 'sample', timestamp: 1696284567890, validated: true }
 * [RUN] Health check triggered!
 * Payload: {
 *   "source": "test",
 *   "data": "sample",
 *   "timestamp": 1696284567890,
 *   "validated": true
 * }
 * Run result: {
 *   result: {
 *     status: 'healthy',
 *     checks: { database: 'ok', cache: 'ok', api: 'ok' },
 *     timestamp: 1696284567890
 *   },
 *   assertions: []
 * }
 * [AFTER] Health check completed
 * Result: { status: 'healthy', checks: { database: 'ok', cache: 'ok', api: 'ok' }, timestamp: 1696284567890 }
 * After result: {
 *   result: {
 *     status: 'healthy',
 *     checks: { database: 'ok', cache: 'ok', api: 'ok' },
 *     timestamp: 1696284567890,
 *     finalStatus: 'completed',
 *     audited: true
 *   }
 * }
 *
 * ✓ Example complete!
 */
