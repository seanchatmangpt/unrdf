/**
 * @file Hook Chains Example
 *
 * Demonstrates:
 * - Creating sequential hook chains
 * - Hook composition and ordering
 * - Early termination on failures
 * - Progressive quad transformations
 * - Chain result aggregation
 *
 * @module hooks-example-chains
 */

import { namedNode, literal, quad, createStore } from '@unrdf/core';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  defineHook,
  createHookRegistry,
  registerHook,
  executeHookChain,
  builtinHooks,
} from '@unrdf/hooks';
/* ========================================================================= */
/* Custom Hook Definitions for Chaining                                     */
/* ========================================================================= */

/**
 * Step 1: Validate IRI format
 */
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

    return (
      validateIRI(quad.subject) &&
      validateIRI(quad.predicate) &&
      validateIRI(quad.object)
    );
  },
  metadata: {
    description: 'Step 1: Validate all IRIs are well-formed',
    chain: { order: 1 },
  },
});

/**
 * Step 2: Normalize whitespace in literals
 */
const normalizeWhitespace = defineHook({
  name: 'normalize-whitespace',
  trigger: 'before-add',
  transform: quad => {
    if (quad.object.termType !== 'Literal') {
      return quad;
    }

    // Normalize whitespace: trim and collapse multiple spaces
    const normalized = quad.object.value.trim().replace(/\s+/g, ' ');

    return dataFactory.quad(
      quad.subject,
      quad.predicate,
      dataFactory.literal(
        normalized,
        quad.object.language || quad.object.datatype
      ),
      quad.graph
    );
  },
  metadata: {
    description: 'Step 2: Normalize whitespace in literals',
    chain: { order: 2 },
  },
});

/**
 * Step 3: Validate literal length constraints
 */
const validateLiteralLength = defineHook({
  name: 'validate-literal-length',
  trigger: 'before-add',
  validate: quad => {
    if (quad.object.termType !== 'Literal') {
      return true;
    }

    // Check literal length constraints
    const value = quad.object.value;
    return value.length > 0 && value.length <= 1000;
  },
  metadata: {
    description: 'Step 3: Validate literal length (1-1000 chars)',
    chain: { order: 3 },
  },
});

/**
 * Step 4: Add provenance metadata
 */
const addProvenance = defineHook({
  name: 'add-provenance',
  trigger: 'before-add',
  transform: quad => {
    // If no graph specified, add provenance graph
    if (!quad.graph || quad.graph.termType === 'DefaultGraph') {
      return dataFactory.quad(
        quad.subject,
        quad.predicate,
        quad.object,
        namedNode(`http://example.org/provenance/${Date.now()}`)
      );
    }
    return quad;
  },
  metadata: {
    description: 'Step 4: Add provenance metadata',
    chain: { order: 4 },
  },
});

/**
 * Step 5: Final validation check
 */
const finalValidation = defineHook({
  name: 'final-validation',
  trigger: 'before-add',
  validate: quad => {
    // Ensure quad has all required components
    return (
      quad.subject &&
      quad.predicate &&
      quad.object &&
      quad.predicate.termType === 'NamedNode'
    );
  },
  metadata: {
    description: 'Step 5: Final validation check',
    chain: { order: 5 },
  },
});

/* ========================================================================= */
/* Chain Configurations                                                      */
/* ========================================================================= */

/**
 * Data Cleaning Chain
 *
 * Progressive transformation: trim → normalize → validate
 */
const dataCleaningChain = [
  builtinHooks.trimLiterals,
  normalizeWhitespace,
  validateLiteralLength,
];

/**
 * Quality Assurance Chain
 *
 * Comprehensive validation: structure → format → constraints
 */
const qualityAssuranceChain = [
  builtinHooks.standardValidation,
  validateIRIs,
  builtinHooks.validateIRIFormat,
  validateLiteralLength,
];

/**
 * Complete Processing Chain
 *
 * Full pipeline: validate → clean → transform → finalize
 */
const completeProcessingChain = [
  validateIRIs,
  normalizeWhitespace,
  validateLiteralLength,
  addProvenance,
  finalValidation,
];

/* ========================================================================= */
/* Example Usage                                                             */
/* ========================================================================= */

/**
 * Demonstrate data cleaning chain.
 */
function demonstrateDataCleaning() {
  console.log('\n🧹 Data Cleaning Chain');
  console.log('─'.repeat(60));

  const store = createStore();
  const dirtyQuad = quad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('  Alice   Smith  ') // Messy whitespace
  );

  console.log('Input:', dirtyQuad.object.value);
  console.log('      ', JSON.stringify(dirtyQuad.object.value));

  const result = executeHookChain(dataCleaningChain, dirtyQuad);

  console.log('Status:', result.valid ? '✅ SUCCESS' : '❌ FAILED');
  console.log('Steps:', `${result.results.filter(r => r.valid).length}/${result.results.length} passed`);

  if (result.valid) {
    console.log('Output:', result.quad.object.value);
    console.log('       ', JSON.stringify(result.quad.object.value));
  }

  if (result.results.length > 0) {
    console.log('\nChain execution:');
    result.results.forEach((r, i) => {
      const status = r.valid ? '✅' : '❌';
      const transformed = r.quad && r.quad !== dirtyQuad ? '🔄' : '  ';
      console.log(`  ${i + 1}. ${status} ${transformed} ${r.hookName}`);
    });
  }
}

/**
 * Demonstrate quality assurance chain with failure.
 */
function demonstrateQualityAssurance() {
  console.log('\n🔍 Quality Assurance Chain');
  console.log('─'.repeat(60));

  const store = createStore();
  const invalidQuad = quad(
    namedNode('http://example.org/bob'),
    namedNode('http://xmlns.com/foaf/0.1/description'),
    literal('x'.repeat(1500)) // Too long
  );

  console.log('Input: Literal with', invalidQuad.object.value.length, 'characters');

  const result = executeHookChain(qualityAssuranceChain, invalidQuad);

  console.log('Status:', result.valid ? '✅ SUCCESS' : '❌ FAILED');
  console.log('Steps:', `${result.results.filter(r => r.valid).length}/${result.results.length} passed`);

  if (!result.valid) {
    const failedHook = result.results.find(r => !r.valid);
    console.log('Failed at:', failedHook?.hookName);
    console.log('Reason:', failedHook?.error);
  }

  console.log('\nChain execution (early termination):');
  result.results.forEach((r, i) => {
    const status = r.valid ? '✅' : '❌';
    console.log(`  ${i + 1}. ${status} ${r.hookName}`);
  });
}

/**
 * Demonstrate complete processing chain.
 */
function demonstrateCompleteProcessing() {
  console.log('\n⚙️  Complete Processing Chain');
  console.log('─'.repeat(60));

  const store = createStore();
  const rawQuad = quad(
    namedNode('http://example.org/charlie'),
    namedNode('http://xmlns.com/foaf/0.1/nick'),
    literal('  Chuck  ')
    // No graph - will be added by provenance hook
  );

  console.log('Input:');
  console.log('  Object:', JSON.stringify(rawQuad.object.value));
  console.log('  Graph:', rawQuad.graph.termType);

  const result = executeHookChain(completeProcessingChain, rawQuad);

  console.log('\nStatus:', result.valid ? '✅ SUCCESS' : '❌ FAILED');
  console.log('Steps:', `${result.results.filter(r => r.valid).length}/${result.results.length} passed`);

  if (result.valid) {
    console.log('\nOutput:');
    console.log('  Object:', JSON.stringify(result.quad.object.value));
    console.log('  Graph:', result.quad.graph.termType);
    if (result.quad.graph.termType === 'NamedNode') {
      console.log('  Graph IRI:', result.quad.graph.value);
    }
  }

  console.log('\nChain execution:');
  result.results.forEach((r, i) => {
    const status = r.valid ? '✅' : '❌';
    const transformed = r.quad && r.quad !== rawQuad ? '🔄' : '  ';
    console.log(`  ${i + 1}. ${status} ${transformed} ${r.hookName}`);
    if (transformed === '🔄') {
      console.log(`      └─ Transformation applied`);
    }
  });
}

/**
 * Run all chain demonstrations.
 */
function demonstrateHookChains() {
  console.log('🔗 Hook Chains Example\n');
  console.log('='.repeat(60));

  demonstrateDataCleaning();
  demonstrateQualityAssurance();
  demonstrateCompleteProcessing();

  console.log('\n' + '='.repeat(60));
  console.log('✨ Hook Chains Example Complete\n');
}

/* ========================================================================= */
/* Export API                                                                */
/* ========================================================================= */

export {
  validateIRIs,
  normalizeWhitespace,
  validateLiteralLength,
  addProvenance,
  finalValidation,
  dataCleaningChain,
  qualityAssuranceChain,
  completeProcessingChain,
  demonstrateDataCleaning,
  demonstrateQualityAssurance,
  demonstrateCompleteProcessing,
  demonstrateHookChains,
};

// Run example if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  demonstrateHookChains();
}
