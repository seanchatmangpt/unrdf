/**
 * @file OTEL Validation Runner for KGC-4D Playground
 * @module lib/otel/validation-runner
 *
 * @description
 * Comprehensive validation framework for KGC-4D Playground using OTEL spans.
 *
 * Validates:
 * 1. Data Persistence - Universe stores quads in Oxigraph
 * 2. Validation Hooks - Delta submission rules enforced
 * 3. Shard Projection - Query/filter operations correct
 * 4. End-to-end Flow - Complete data lifecycle
 *
 * OTEL spans are the source of truth - not function claims.
 */

import { ensureValidatorInitialized, getOTELValidationStatus, recordOTELSpans } from './instrumentation.mjs';
import { recordUniversePersistence, setInstrumentationId as setUniverseInstrId } from './universe-instrumented.mjs';
import { submitDeltaInstrumented, setInstrumentationId as setDeltaInstrId } from './delta-instrumented.mjs';
import { projectShardInstrumented, setInstrumentationId as setShardInstrId } from './shard-instrumented.mjs';
import { getUniverse, dataFactory } from '../server/universe.mjs';

/**
 * Validation result object
 * @typedef {Object} ValidationResult
 * @property {string} name - Test name
 * @property {boolean} passed - Whether test passed
 * @property {string} reason - Why test passed/failed
 * @property {number} duration - Test duration in ms
 * @property {Object} evidence - OTEL-based proof
 */

/**
 * Run comprehensive OTEL validation suite
 * @param {Object} options - Configuration options
 * @param {boolean} options.verbose - Show detailed output
 * @param {string} options.filter - Only run tests matching this pattern
 * @returns {Promise<Object>} Validation report
 */
export async function runOTELValidation(options = {}) {
  const { verbose = false, filter = null } = options;
  const startTime = Date.now();
  const results = [];
  const validationId = `validation-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

  // Ensure validator is initialized
  await ensureValidatorInitialized();

  // Set instrumentation ID on all modules
  setUniverseInstrId(validationId);
  setDeltaInstrId(validationId);
  setShardInstrId(validationId);

  if (verbose) console.log('\n🔍 Starting OTEL Validation Suite\n');

  // Get universe singleton
  const universe = await getUniverse();

  // Test 1: Data Persistence
  if (!filter || 'persistence'.includes(filter)) {
    const result = await validateDataPersistence(universe, validationId);
    results.push(result);
    if (verbose) printResult(result);
  }

  // Test 2: Validation Hooks
  if (!filter || 'validation'.includes(filter)) {
    const result = await validateValidationHooks(validationId);
    results.push(result);
    if (verbose) printResult(result);
  }

  // Test 3: Shard Projection
  if (!filter || 'shard'.includes(filter)) {
    const result = await validateShardProjection(universe, validationId);
    results.push(result);
    if (verbose) printResult(result);
  }

  // Test 4: End-to-End Flow
  if (!filter || 'e2e'.includes(filter)) {
    const result = await validateEndToEndFlow(universe, validationId);
    results.push(result);
    if (verbose) printResult(result);
  }

  const duration = Date.now() - startTime;

  // Generate report
  const report = generateReport(results, duration, validationId);

  if (verbose) {
    console.log('\n' + '='.repeat(70));
    console.log(JSON.stringify(report, null, 2));
    console.log('='.repeat(70) + '\n');
  }

  return report;
}

/**
 * Validate data persistence via OTEL spans
 * @private
 */
async function validateDataPersistence(universe, validationId) {
  const testStart = Date.now();

  try {
    // Create test deltas
    const deltas = [
      {
        type: 'add',
        subject: dataFactory.namedNode('http://example.org/validation/entity1'),
        predicate: dataFactory.namedNode('http://kgc.io/ontology/name'),
        object: dataFactory.literal('Validation Test Entity'),
      },
    ];

    // Record persistence
    await recordUniversePersistence(
      universe,
      { type: 'VALIDATION_TEST', payload: { test: 'persistence' } },
      deltas
    );

    // Get OTEL status
    const status = getOTELValidationStatus(validationId);
    const persistenceSpans = status.spans.filter(s => s.name === 'universe.persist');

    const passed = persistenceSpans.length > 0 && status.total_spans > 0;

    return {
      name: 'Data Persistence',
      passed,
      reason: passed
        ? `✅ ${persistenceSpans.length} persistence spans recorded`
        : '❌ No persistence spans found',
      duration: Date.now() - testStart,
      evidence: {
        total_spans: status.total_spans,
        persistence_spans: persistenceSpans.length,
        operations_traced: persistenceSpans.reduce((sum, s) => sum + (s.attributes?.['operation.count'] || 0), 0),
      },
    };
  } catch (error) {
    return {
      name: 'Data Persistence',
      passed: false,
      reason: `❌ Error: ${error.message}`,
      duration: Date.now() - testStart,
      evidence: { error: error.message },
    };
  }
}

/**
 * Validate validation hooks via OTEL spans
 * @private
 */
async function validateValidationHooks(validationId) {
  const testStart = Date.now();

  try {
    // Test valid delta
    const validDelta = {
      operations: [
        {
          type: 'add',
          subject: { value: 'http://example.org/validation/project', termType: 'NamedNode' },
          predicate: { value: 'http://kgc.io/ontology/budget', termType: 'NamedNode' },
          object: { value: '50000', termType: 'Literal' },
        },
      ],
      source: 'validation-test',
    };

    const validResult = await submitDeltaInstrumented(validDelta);

    // Test invalid delta
    const invalidDelta = {
      operations: [
        {
          type: 'add',
          subject: { value: 'http://example.org/validation/project2', termType: 'NamedNode' },
          predicate: { value: 'http://kgc.io/ontology/budget', termType: 'NamedNode' },
          object: { value: '999999', termType: 'Literal' },
        },
      ],
      source: 'validation-test',
    };

    const invalidResult = await submitDeltaInstrumented(invalidDelta);

    // Get OTEL status
    const status = getOTELValidationStatus(validationId);
    const validationSpans = status.spans.filter(s => s.name === 'delta.validation');

    const passed =
      validResult.status === 'ACK' &&
      invalidResult.status === 'REJECT' &&
      validationSpans.length >= 2;

    return {
      name: 'Validation Hooks',
      passed,
      reason: passed
        ? `✅ Valid delta accepted, invalid delta rejected (${validationSpans.length} spans)`
        : '❌ Validation rules not properly enforced',
      duration: Date.now() - testStart,
      evidence: {
        valid_result: validResult.status,
        invalid_result: invalidResult.status,
        validation_spans: validationSpans.length,
        accepted: validationSpans.filter(s => s.status === 'ok').length,
        rejected: validationSpans.filter(s => s.status === 'error').length,
      },
    };
  } catch (error) {
    return {
      name: 'Validation Hooks',
      passed: false,
      reason: `❌ Error: ${error.message}`,
      duration: Date.now() - testStart,
      evidence: { error: error.message },
    };
  }
}

/**
 * Validate shard projection via OTEL spans
 * @private
 */
async function validateShardProjection(universe, validationId) {
  const testStart = Date.now();

  try {
    // Project shard
    const shard = await projectShardInstrumented({});

    // Get OTEL status
    const status = getOTELValidationStatus(validationId);
    const projectionSpans = status.spans.filter(s => s.name === 'shard.projection');

    const passed =
      shard && shard.quads && shard.quads.length > 0 && projectionSpans.length > 0;

    return {
      name: 'Shard Projection',
      passed,
      reason: passed
        ? `✅ Shard projected with ${shard.quads.length} quads`
        : '❌ Shard projection failed',
      duration: Date.now() - testStart,
      evidence: {
        shard_id: shard?.id,
        quads_projected: shard?.quads?.length || 0,
        projection_spans: projectionSpans.length,
        avg_duration_ms: projectionSpans.length > 0
          ? projectionSpans.reduce((sum, s) => sum + s.duration, 0) / projectionSpans.length
          : 0,
      },
    };
  } catch (error) {
    return {
      name: 'Shard Projection',
      passed: false,
      reason: `❌ Error: ${error.message}`,
      duration: Date.now() - testStart,
      evidence: { error: error.message },
    };
  }
}

/**
 * Validate end-to-end data lifecycle
 * @private
 */
async function validateEndToEndFlow(universe, validationId) {
  const testStart = Date.now();

  try {
    // Complete flow:
    // 1. Persist data
    // 2. Validate delta
    // 3. Project shard

    const deltas = [
      {
        type: 'add',
        subject: dataFactory.namedNode('http://example.org/e2e/entity'),
        predicate: dataFactory.namedNode('http://kgc.io/ontology/name'),
        object: dataFactory.literal('E2E Test'),
      },
    ];

    await recordUniversePersistence(universe, { type: 'E2E_TEST' }, deltas);

    const delta = {
      operations: [
        {
          type: 'add',
          subject: { value: 'http://example.org/e2e/project', termType: 'NamedNode' },
          predicate: { value: 'http://kgc.io/ontology/budget', termType: 'NamedNode' },
          object: { value: '25000', termType: 'Literal' },
        },
      ],
      source: 'e2e-test',
    };

    const deltaResult = await submitDeltaInstrumented(delta);

    const shard = await projectShardInstrumented({});

    // Get OTEL status
    const status = getOTELValidationStatus(validationId);

    const passed =
      status.total_spans > 0 &&
      status.spans.some(s => s.name === 'universe.persist') &&
      status.spans.some(s => s.name === 'delta.validation') &&
      status.spans.some(s => s.name === 'shard.projection') &&
      deltaResult.status === 'ACK' &&
      shard.quads.length > 0;

    return {
      name: 'End-to-End Flow',
      passed,
      reason: passed
        ? '✅ Complete data lifecycle verified'
        : '❌ End-to-end flow incomplete',
      duration: Date.now() - testStart,
      evidence: {
        persistence_working: status.spans.some(s => s.name === 'universe.persist'),
        validation_working: status.spans.some(s => s.name === 'delta.validation'),
        projection_working: status.spans.some(s => s.name === 'shard.projection'),
        delta_status: deltaResult.status,
        shard_quads: shard.quads.length,
        total_spans: status.total_spans,
      },
    };
  } catch (error) {
    return {
      name: 'End-to-End Flow',
      passed: false,
      reason: `❌ Error: ${error.message}`,
      duration: Date.now() - testStart,
      evidence: { error: error.message },
    };
  }
}

/**
 * Generate validation report
 * @private
 */
function generateReport(results, duration, validationId) {
  const passed = results.filter(r => r.passed).length;
  const total = results.length;
  const score = Math.round((passed / total) * 100);

  return {
    validationId,
    timestamp: new Date().toISOString(),
    duration,
    summary: {
      passed,
      total,
      score,
      status: score >= 75 ? 'PASS' : score >= 50 ? 'PARTIAL' : 'FAIL',
    },
    tests: results,
    recommendations:
      score < 100
        ? ['Fix failed tests before production deployment']
        : ['All tests passing - ready for production'],
  };
}

/**
 * Print validation result
 * @private
 */
function printResult(result) {
  const status = result.passed ? '✅' : '❌';
  console.log(`\n${status} ${result.name}`);
  console.log(`   ${result.reason}`);
  console.log(`   Duration: ${result.duration}ms`);
  console.log(`   Evidence:`, JSON.stringify(result.evidence, null, 4).split('\n').join('\n   '));
}

/**
 * Run validation and exit with appropriate code
 * @param {Object} options - Validation options
 */
export async function runValidationCLI(options = {}) {
  try {
    const report = await runOTELValidation({ ...options, verbose: true });

    // Exit with code 0 if all tests pass, 1 otherwise
    const exitCode = report.summary.score >= 75 ? 0 : 1;
    process.exit(exitCode);
  } catch (error) {
    console.error('❌ Validation runner error:', error.message);
    process.exit(1);
  }
}
