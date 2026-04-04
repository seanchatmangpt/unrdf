/**
 * @unrdf/hooks - Window Condition Example
 *
 * Demonstrates time-windowed aggregation evaluation.
 * Triggers rules when aggregated metrics within time windows meet thresholds.
 *
 * Use cases:
 * - Rate limiting: "more than 100 requests/minute"
 * - Anomaly detection: "average temperature exceeds threshold"
 * - Traffic analysis: "peak traffic detection"
 * - Quota enforcement: "daily transaction limit"
 */

import { createStore } from '@unrdf/oxigraph';
import { evaluateCondition } from '../src/hooks/condition-evaluator.mjs';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';

const { namedNode, literal, quad } = DataFactory;

console.log('=== Window Condition Example ===\n');

// Utility: create window condition
function createWindowCondition(aggregate, windowMs, query) {
  return {
    kind: 'window',
    spec: {
      size: windowMs,
      aggregate: aggregate,
      query: query
    }
  };
}

// Example 1: Count-based window
console.log('1. Count Window (Rate Limiting)\n');

const countWindowCondition = createWindowCondition(
  'count',
  60000,  // 1 minute
  `SELECT ?s WHERE {
    ?s a ex:Request ;
       ex:timestamp ?t .
    FILTER (?t > NOW() - PT1M)
  }`
);

console.log('Condition:');
console.log(JSON.stringify(countWindowCondition, null, 2));
console.log('');
console.log('Purpose: Count matches in last 60 seconds');
console.log('Triggers when: count >= configured threshold');
console.log('Use for: Rate limiting, quota enforcement');
console.log('');

console.log('Example usage in hook:');
console.log(`
{
  condition: ${JSON.stringify(countWindowCondition, null, 4)},
  effects: [{
    kind: 'sparql-construct',
    query: \`CONSTRUCT { ?s ex:rateLimited true } WHERE { ?s a ex:Request }\`
  }]
}

Flow:
1. Query finds all requests in last 60 seconds
2. Count results
3. If count > 100, hook triggers
4. Affected requests marked as rate-limited
`);

// Example 2: Sum aggregation
console.log('\n2. Sum Window (Traffic Volume)\n');

const sumWindowCondition = createWindowCondition(
  'sum',
  300000,  // 5 minutes
  `SELECT ?bytes WHERE {
    ?transfer ex:transferredBytes ?bytes ;
              ex:timestamp ?t .
    FILTER (?t > NOW() - PT5M)
  }`
);

console.log('Condition:');
console.log(JSON.stringify(sumWindowCondition, null, 2));
console.log('');
console.log('Purpose: Sum bytes transferred in last 5 minutes');
console.log('Triggers when: sum > threshold (e.g., 1GB)');
console.log('Use for: Bandwidth management, peak detection\n');

// Example 3: Average aggregation
console.log('3. Average Window (Anomaly Detection)\n');

const avgWindowCondition = createWindowCondition(
  'avg',
  3600000,  // 1 hour
  `SELECT ?temp WHERE {
    ?sensor ex:reading ?temp ;
            ex:timestamp ?t .
    FILTER (?t > NOW() - PT1H)
  }`
);

console.log('Condition:');
console.log(JSON.stringify(avgWindowCondition, null, 2));
console.log('');
console.log('Purpose: Average temperature over last hour');
console.log('Triggers when: avg > 85 degrees (anomaly)');
console.log('Use for: Anomaly detection, SLA monitoring\n');

// Example 4: Min/Max windows
console.log('4. Min/Max Windows (Range Monitoring)\n');

const minWindowCondition = createWindowCondition(
  'min',
  86400000,  // 24 hours
  `SELECT ?value WHERE {
    ?metric ex:dailyValue ?value ;
            ex:timestamp ?t .
    FILTER (?t > NOW() - P1D)
  }`
);

const maxWindowCondition = createWindowCondition(
  'max',
  86400000,  // 24 hours
  `SELECT ?value WHERE {
    ?metric ex:dailyValue ?value ;
            ex:timestamp ?t .
    FILTER (?t > NOW() - P1D)
  }`
);

console.log('Min window (find lowest value in window):');
console.log(JSON.stringify(minWindowCondition, null, 2));
console.log('');
console.log('Max window (find highest value in window):');
console.log(JSON.stringify(maxWindowCondition, null, 2));
console.log('');
console.log('Use for: Performance tracking, threshold detection\n');

// Real-world scenarios
console.log('5. Real-World Scenarios\n');

console.log('Scenario A: API Rate Limiting');
console.log('-'.repeat(40));
console.log(`
Condition: Window count in 1 minute > 1000
{
  kind: 'window',
  spec: {
    size: 60000,
    aggregate: 'count',
    query: 'SELECT ?req WHERE { ?req a ex:APICall ; ex:timestamp ?t }'
  }
}

Hook flow:
1. Client makes API requests
2. Each request added as RDF triple with timestamp
3. Window counts requests in last minute
4. If > 1000, trigger rate limiting effect
5. Mark client as throttled, return 429 Too Many Requests
`);

console.log('\nScenario B: Data Quality Monitoring');
console.log('-'.repeat(40));
console.log(`
Condition: Window average error rate in 1 hour > 5%
{
  kind: 'window',
  spec: {
    size: 3600000,
    aggregate: 'avg',
    query: 'SELECT ?rate WHERE { ?batch ex:errorRate ?rate }'
  }
}

Hook flow:
1. Data processing batches complete
2. Each batch records error rate
3. Window averages error rates from last hour
4. If average > 5%, alert operations
5. Trigger investigation and mitigation effect
`);

console.log('\nScenario C: Quota Enforcement');
console.log('-'.repeat(40));
console.log(`
Condition: Window sum of usage in 24 hours > quota
{
  kind: 'window',
  spec: {
    size: 86400000,
    aggregate: 'sum',
    query: 'SELECT ?usage WHERE { ?tx ex:usage ?usage }'
  }
}

Hook flow:
1. Transactions recorded with usage amount
2. Window sums usage over rolling 24 hours
3. If sum > daily quota, enforce limit
4. Mark user as over-quota
5. Require billing upgrade or wait for next period
`);

// Aggregate functions explained
console.log('\n6. Aggregate Functions Explained\n');

console.log('count   - Number of matches in window');
console.log('         Example: How many API calls in last minute?');
console.log('');
console.log('sum     - Total of numeric values in window');
console.log('         Example: Total bytes transferred in 5 minutes?');
console.log('');
console.log('avg     - Average of numeric values in window');
console.log('         Example: Average response time over 1 hour?');
console.log('');
console.log('min     - Minimum value in window');
console.log('         Example: Lowest temperature recorded today?');
console.log('');
console.log('max     - Maximum value in window');
console.log('         Example: Peak memory usage in last hour?\n');

// Window sizing
console.log('7. Window Size Best Practices\n');

console.log('Use Case             Window Size    Frequency        Aggregate');
console.log('-'.repeat(60));
console.log('API rate limiting    1 minute      Per request      count');
console.log('Traffic analysis     5 minutes     Per measurement  sum/avg');
console.log('SLA monitoring       1 hour        Per measurement  avg/max');
console.log('Daily quota          24 hours      Per transaction  sum');
console.log('Anomaly detection    1 hour        Per event        avg/stddev\n');

// Combining with effects
console.log('8. Full Hook Example: Rate Limiting\n');

console.log(`
const rateLimitHook = {
  name: 'enforce-api-rate-limit',

  condition: {
    kind: 'window',
    spec: {
      size: 60000,      // 1 minute window
      aggregate: 'count',
      query: \`
        SELECT ?request WHERE {
          ?request a ex:APIRequest ;
                   ex:clientId ?client ;
                   ex:timestamp ?t .
          FILTER (?t > NOW() - PT1M)
        }
      \`
    }
  },

  effects: [{
    kind: 'sparql-construct',
    query: \`
      CONSTRUCT {
        ?client ex:rateLimited true ;
                ex:retryAfter "60"^^xsd:integer ;
                ex:error "Rate limit exceeded" .
      }
      WHERE {
        ?request a ex:APIRequest ;
                 ex:clientId ?client .
      }
    \`
  }]
};
`);

console.log('\nExecution:');
console.log('1. Request arrives, added to store with timestamp');
console.log('2. Window condition counts requests from same client in 1min');
console.log('3. If count > 1000, condition triggers');
console.log('4. Effect adds rate limit response triples');
console.log('5. API returns 429 with retry-after header\n');

console.log('=== Example Complete ===');
