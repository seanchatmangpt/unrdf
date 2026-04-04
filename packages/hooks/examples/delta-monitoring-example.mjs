/**
 * @unrdf/hooks - Delta Monitoring Example
 *
 * Demonstrates change detection using Delta conditions.
 * Triggers rules when data grows, shrinks, or changes significantly.
 *
 * Use cases:
 * - Validation on large imports (increase > 10%)
 * - Anomaly detection on suspicious deletes
 * - Rate limiting on rapid changes
 * - Archive triggers on data growth
 */

import { createStore } from '@unrdf/oxigraph';
import { evaluateCondition } from '../src/hooks/condition-evaluator.mjs';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';

const { namedNode, literal, quad } = DataFactory;

console.log('=== Delta Monitoring Example ===\n');

// Utility: create a hook condition with delta spec
function createDeltaCondition(changeType, threshold = 0.1) {
  return {
    kind: 'delta',
    spec: {
      change: changeType,
      threshold: threshold
    }
  };
}

// Example 1: Detect any change
console.log('1. Detect Any Change\n');
const anyChangeCondition = createDeltaCondition('any');

console.log('Condition:');
console.log(JSON.stringify(anyChangeCondition, null, 2));
console.log('');
console.log('Triggers when: any quad is added or deleted');
console.log('Use for: audit trails, change notifications');
console.log('');

// Example 2: Detect data growth
console.log('2. Detect Data Growth (Increase)\n');
const growthCondition = createDeltaCondition('increase', 0.1);

console.log('Condition:');
console.log(JSON.stringify(growthCondition, null, 2));
console.log('');
console.log('Triggers when: net adds > 10% of total quads');
console.log('Example: If store has 100 quads, triggers if 11+ net adds');
console.log('Use for: validation after imports, reindexing\n');

// Example 3: Detect data loss
console.log('3. Detect Data Loss (Decrease)\n');
const shrinkCondition = createDeltaCondition('decrease', 0.05);

console.log('Condition:');
console.log(JSON.stringify(shrinkCondition, null, 2));
console.log('');
console.log('Triggers when: net deletes > 5% of total quads');
console.log('Example: If store has 100 quads, triggers if 6+ net deletes');
console.log('Use for: anomaly detection, suspicious deletion alerts\n');

// Example 4: Detect significant modifications
console.log('4. Detect Significant Changes (Modify)\n');
const modifyCondition = createDeltaCondition('modify', 0.15);

console.log('Condition:');
console.log(JSON.stringify(modifyCondition, null, 2));
console.log('');
console.log('Triggers when: |adds - deletes| > 15% of total quads');
console.log('Example: 20 adds + 5 deletes = 25 net, triggers if > 15 quads');
console.log('Use for: detecting bulk replace/sync operations\n');

// Practical scenarios
console.log('5. Real-World Scenarios\n');

console.log('Scenario A: Data Import Validation');
console.log('-'.repeat(40));
console.log(`
Hook: {
  condition: {
    kind: 'delta',
    spec: { change: 'increase', threshold: 0.2 }  // 20% growth
  },
  effects: [{
    kind: 'sparql-construct',
    query: 'CONSTRUCT { ?s ex:imported true } WHERE { ?s a ex:NewData }'
  }]
}

Trigger: When bulk data import adds > 20% new quads
Action: Mark imported entities, run compliance checks
`);

console.log('Scenario B: Suspicious Deletion Alert');
console.log('-'.repeat(40));
console.log(`
Hook: {
  condition: {
    kind: 'delta',
    spec: { change: 'decrease', threshold: 0.02 }  // 2% loss
  },
  effects: [{
    kind: 'sparql-construct',
    query: 'CONSTRUCT { ?s ex:audit ex:DeleteAlert } WHERE { ?s ?p ?o }'
  }]
}

Trigger: When deletions exceed 2% (unusual for stable data)
Action: Create audit record for investigation
`);

console.log('Scenario C: Archive on Growth');
console.log('-'.repeat(40));
console.log(`
Hook: {
  condition: {
    kind: 'delta',
    spec: { change: 'increase', threshold: 0.5 }  // 50% growth
  },
  effects: [{
    kind: 'function',
    inline: async (store) => {
      // Archive cold data to external storage
      return { archived: true, count: store.size };
    }
  }]
}

Trigger: When data doubles (50% growth)
Action: Archive historical data to long-term storage
`);

// Change calculation logic
console.log('\n6. Delta Calculation Details\n');

console.log('Formula for change magnitude:');
console.log('  changeMagnitude = (additions - deletions) / totalQuads\n');

console.log('Example calculation:');
console.log('  Store size:    1000 quads');
console.log('  Additions:     250 quads');
console.log('  Deletions:     50 quads');
console.log('  Net change:    250 - 50 = 200');
console.log('  Magnitude:     200 / 1000 = 0.2 (20%)\n');

console.log('  Condition evaluation:');
console.log('  - increase (threshold 0.1):  0.2 > 0.1 ✓ TRIGGERS');
console.log('  - increase (threshold 0.3):  0.2 > 0.3 ✗ no trigger');
console.log('  - decrease (threshold 0.1): -0.2 < -0.1 ✗ no trigger');
console.log('  - modify (threshold 0.15):  |0.2| > 0.15 ✓ TRIGGERS\n');

// Threshold recommendations
console.log('7. Recommended Thresholds\n');

console.log('Data Size          Increase   Decrease   Modify   Use Case');
console.log('-' * 60);
console.log('< 100 quads        0.20       0.20       0.20     Development');
console.log('100-1K quads       0.15       0.10       0.15     Small datasets');
console.log('1K-100K quads      0.10       0.05       0.10     Medium datasets');
console.log('> 100K quads       0.05       0.02       0.05     Large graphs\n');

// Combining with effects
console.log('8. Combined Example: Monitor + React\n');

console.log(`
const importValidationHook = {
  name: 'validate-large-import',

  condition: {
    kind: 'delta',
    spec: { change: 'increase', threshold: 0.1 }  // 10% growth trigger
  },

  effects: [
    // Effect 1: Mark newly added entities
    {
      kind: 'sparql-construct',
      query: \`CONSTRUCT {
        ?s ex:addedAt ?now ;
           ex:source ex:Import .
      } WHERE {
        ?s a ex:Trade .
        BIND (NOW() as ?now)
      }\`
    },

    // Effect 2: Run compliance check
    {
      kind: 'sparql-construct',
      query: \`CONSTRUCT {
        ?s ex:complianceStatus ex:PendingReview .
      } WHERE {
        ?s a ex:Trade ;
           ex:source ex:Import .
      }\`
    }
  ]
};
`);

console.log('\nExecution flow:');
console.log('1. New data added to store (e.g., 250 of 1000 quads)');
console.log('2. Delta condition evaluates: 0.25 > 0.10 ✓');
console.log('3. First effect executes: mark new trades with timestamp');
console.log('4. Second effect executes: set compliance status');
console.log('5. Hook completes successfully');
console.log('6. Compliance team notified for batch review\n');

console.log('=== Example Complete ===');
