/**
 * @unrdf/hooks - SHACL Repair Mode Example
 *
 * Demonstrates SHACL validation with automatic repair.
 * When validation fails, repair queries fix data, then re-validate.
 *
 * Use cases:
 * - Auto-filling missing required properties with defaults
 * - Normalizing invalid references
 * - Fixing data quality issues automatically
 */

import { createStore } from '@unrdf/oxigraph';
import { evaluateCondition } from '../src/hooks/condition-evaluator.mjs';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';

const { namedNode, literal, quad } = DataFactory;

console.log('=== SHACL Repair Mode Example ===\n');

// Create a test store
const store = createStore();

// Add test data (some valid, some missing required properties)
console.log('1. Loading test data...');
store.add(
  quad(
    namedNode('http://example.org/trade/t1'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/Trade')
  )
);
store.add(
  quad(
    namedNode('http://example.org/trade/t1'),
    namedNode('http://example.org/amount'),
    literal('150000')
  )
);
// Note: missing riskScore

store.add(
  quad(
    namedNode('http://example.org/trade/t2'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/Trade')
  )
);
// Note: missing both amount and riskScore

console.log(`   Loaded ${store.size} quads\n`);

// Define SHACL condition with repair query
console.log('2. Defining SHACL repair condition...\n');

const shaclRepairCondition = {
  kind: 'shacl',
  ref: {
    uri: 'file:///shapes/trade-shape.ttl',
    // Note: in production, provide actual sha256 hash
    // sha256: 'abc123def456...'
  },
  enforcementMode: 'repair',
  repairConstruct: `
    # Fill in missing required properties with defaults
    CONSTRUCT {
      ?trade ex:riskScore ?defaultRisk ;
             ex:amount ?defaultAmount ;
             ex:status ex:Pending .
    }
    WHERE {
      ?trade a ex:Trade .
      OPTIONAL { ?trade ex:riskScore ?existing . }
      OPTIONAL { ?trade ex:amount ?existingAmount . }
      FILTER (!BOUND(?existing) || !BOUND(?existingAmount))
      BIND (50 as ?defaultRisk)
      BIND (0 as ?defaultAmount)
    }
  `
};

console.log('Repair CONSTRUCT query:');
console.log(shaclRepairCondition.repairConstruct);
console.log('');

// Example 1: Block mode (strictest - no repairs)
console.log('3. Example: Block Mode (no repairs)\n');
const blockCondition = {
  ...shaclRepairCondition,
  enforcementMode: 'block'
};

console.log('With block mode:');
console.log('- Validation failure = hook fails (false)');
console.log('- No automatic repair attempted');
console.log('- Application must handle fix separately\n');

// Example 2: Annotate mode (audit trail)
console.log('4. Example: Annotate Mode (logging)\n');
const annotateCondition = {
  ...shaclRepairCondition,
  enforcementMode: 'annotate'
};

console.log('With annotate mode:');
console.log('- Validation failure = add sh:ValidationResult triples');
console.log('- Returns true (permits operation)');
console.log('- Violations recorded in RDF for audit trail');
console.log('- Good for compliance tracking\n');

// Example 3: Repair mode (automatic fixing)
console.log('5. Example: Repair Mode (automatic fix)\n');
const repairCondition = {
  ...shaclRepairCondition,
  enforcementMode: 'repair'
};

console.log('With repair mode:');
console.log('- Validation failure = execute repairConstruct');
console.log('- Repair adds missing properties with defaults');
console.log('- Re-validates after repair');
console.log('- Returns success/failure of final validation\n');

// Example 4: Multi-phase repair strategy
console.log('6. Complex Example: Multi-Phase Repair\n');

const complexRepairCondition = {
  kind: 'shacl',
  ref: { uri: 'file:///shapes/comprehensive.ttl' },
  enforcementMode: 'repair',
  repairConstruct: `
    PREFIX ex: <http://example.org/>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

    CONSTRUCT {
      ?entity ex:status ex:Reviewed ;
              ex:lastChecked ?now ;
              ex:quality ?quality .
    }
    WHERE {
      ?entity a ex:Entity .
      BIND (NOW() as ?now)

      # Quality assessment: calculate based on completeness
      OPTIONAL { ?entity ex:description ?desc . }
      OPTIONAL { ?entity ex:classification ?class . }
      OPTIONAL { ?entity ex:owner ?owner . }

      BIND (
        IF(BOUND(?desc) && BOUND(?class) && BOUND(?owner),
           "high",
           "medium")
        as ?quality
      )
    }
  `
};

console.log('Multi-phase repair includes:');
console.log('1. Add status field if missing');
console.log('2. Record timestamp of check');
console.log('3. Calculate quality score based on available properties\n');

// Practical workflow
console.log('7. Practical Workflow\n');
console.log('Step 1: Data arrives (potentially with gaps)');
console.log('Step 2: evaluateCondition() with shacl/repair');
console.log('Step 3: If validation fails:');
console.log('        - Auto repair executes');
console.log('        - Missing properties filled with defaults');
console.log('        - Re-validation checks if repair succeeded');
console.log('Step 4: Result indicates success/failure');
console.log('Step 5: Application continues based on result\n');

// Best practices
console.log('8. Best Practices for Repair Mode\n');
console.log('✓ Use repair for:');
console.log('  - Filling optional properties with sensible defaults');
console.log('  - Normalizing data format (URI standardization)');
console.log('  - Adding audit/lifecycle properties');
console.log('');
console.log('✗ Avoid repair for:');
console.log('  - Changing core business data semantics');
console.log('  - Complex transformations (use effects instead)');
console.log('  - Expensive computations (will run on every violation)');
console.log('');
console.log('💡 Combine with effects:');
console.log('  1. Condition: shacl/repair → auto-fix violations');
console.log('  2. Effect: sparql-construct → apply business logic\n');

console.log('=== Example Complete ===');
