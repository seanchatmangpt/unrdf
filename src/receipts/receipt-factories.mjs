/**
 * @fileoverview Receipt Factories - Type-specific receipt creation functions
 *
 * @module receipts/receipt-factories
 */

import { ReceiptBuilder } from './receipt-builder.mjs';

// ============================================================================
// Type-Specific Receipt Factories
// ============================================================================

/**
 * Create an admission receipt
 *
 * @param {object} options - Receipt options
 * @returns {Promise<import('./receipt-schemas.mjs').UniversalReceipt>}
 */
export async function createAdmissionReceipt(options) {
  const {
    pkg,
    capsuleId,
    decision,
    reason,
    partition,
    guards = [],
    invariants = [],
    quadCount,
    beforeHash,
    afterHash,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const builder = new ReceiptBuilder('admission', pkg, { timestamp });

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'admission-engine', ...provenance })
    .input({ hashes: { capsule: capsuleId, before: beforeHash || '' } })
    .output({ hash: afterHash || beforeHash || '' })
    .chain(beforeReceiptHash)
    .extension({
      type: 'admission',
      data: {
        capsuleId,
        partition,
        guards,
        invariants,
        quadCount,
        beforeHash,
        afterHash,
      },
    })
    .build();
}

/**
 * Create a test execution receipt
 *
 * @param {object} options - Receipt options
 * @returns {Promise<import('./receipt-schemas.mjs').UniversalReceipt>}
 */
export async function createTestReceipt(options) {
  const {
    pkg,
    suite,
    file,
    total,
    passed,
    failed,
    skipped = 0,
    coverage,
    failures = [],
    duration,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const decision = failed === 0 ? 'ALLOW' : 'DENY';
  const reason = failed === 0
    ? `All ${total} tests passed`
    : `${failed} of ${total} tests failed`;

  const builder = new ReceiptBuilder('test', pkg, { timestamp });
  const startTime = timestamp || new Date();
  const endTime = new Date(startTime.getTime() + (duration || 0));

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'test-runner', ...provenance })
    .input({ hashes: { testFile: file || 'unknown' } })
    .output({ hash: `test-result-${total}-${passed}-${failed}` })
    .metrics({
      duration: duration || 0,
      startTime: startTime.toISOString(),
      endTime: endTime.toISOString(),
    })
    .chain(beforeReceiptHash)
    .extension({
      type: 'test',
      data: {
        suite,
        file,
        total,
        passed,
        failed,
        skipped,
        coverage,
        failures,
      },
    })
    .build();
}

/**
 * Create a build receipt
 *
 * @param {object} options - Receipt options
 * @returns {Promise<import('./receipt-schemas.mjs').UniversalReceipt>}
 */
export async function createBuildReceipt(options) {
  const {
    pkg,
    target,
    mode = 'production',
    success,
    bundleSize,
    chunks = [],
    warnings = [],
    errors = [],
    duration,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const decision = success ? (warnings.length > 0 ? 'WARN' : 'ALLOW') : 'DENY';
  const reason = success
    ? `Build completed${warnings.length > 0 ? ` with ${warnings.length} warning(s)` : ''}`
    : `Build failed: ${errors.join(', ')}`;

  const builder = new ReceiptBuilder('build', pkg, { timestamp });
  const startTime = timestamp || new Date();
  const endTime = new Date(startTime.getTime() + (duration || 0));

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'build-system', ...provenance })
    .input({ hashes: { source: 'src-hash' } })
    .output({ hash: `build-${bundleSize || 0}` })
    .metrics({
      duration: duration || 0,
      startTime: startTime.toISOString(),
      endTime: endTime.toISOString(),
    })
    .chain(beforeReceiptHash)
    .extension({
      type: 'build',
      data: {
        target,
        mode,
        bundleSize,
        chunks,
        warnings,
        errors,
      },
    })
    .build();
}

/**
 * Create a deployment receipt
 *
 * @param {object} options - Receipt options
 * @returns {Promise<import('./receipt-schemas.mjs').UniversalReceipt>}
 */
export async function createDeploymentReceipt(options) {
  const {
    pkg,
    environment,
    version,
    rollbackVersion,
    success,
    instances,
    region,
    url,
    healthCheck,
    duration,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const decision = success ? 'ALLOW' : 'DENY';
  const reason = success
    ? `Deployed v${version} to ${environment}`
    : `Deployment to ${environment} failed`;

  const builder = new ReceiptBuilder('deployment', pkg, { timestamp });
  const startTime = timestamp || new Date();
  const endTime = new Date(startTime.getTime() + (duration || 0));

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'deploy-system', ...provenance })
    .input({ hashes: { artifact: version } })
    .output({ hash: `deploy-${environment}-${version}` })
    .metrics({
      duration: duration || 0,
      startTime: startTime.toISOString(),
      endTime: endTime.toISOString(),
    })
    .chain(beforeReceiptHash)
    .extension({
      type: 'deployment',
      data: {
        environment,
        version,
        rollbackVersion,
        instances,
        region,
        url,
        healthCheck,
      },
    })
    .build();
}

/**
 * Create a projection receipt (documentation generation)
 *
 * @param {object} options - Receipt options
 * @returns {Promise<import('./receipt-schemas.mjs').UniversalReceipt>}
 */
export async function createProjectionReceipt(options) {
  const {
    pkg,
    format,
    template,
    pages,
    files = [],
    wordCount,
    success,
    duration,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const decision = success ? 'ALLOW' : 'DENY';
  const reason = success
    ? `Generated ${pages || files.length} ${format} page(s)`
    : `Projection failed`;

  const builder = new ReceiptBuilder('projection', pkg, { timestamp });
  const startTime = timestamp || new Date();
  const endTime = new Date(startTime.getTime() + (duration || 0));

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'projection-engine', ...provenance })
    .input({ hashes: { template: template || 'default' } })
    .output({ hash: `projection-${format}-${pages || files.length}`, artifacts: files })
    .metrics({
      duration: duration || 0,
      startTime: startTime.toISOString(),
      endTime: endTime.toISOString(),
    })
    .chain(beforeReceiptHash)
    .extension({
      type: 'projection',
      data: {
        format,
        template,
        pages,
        files,
        wordCount,
      },
    })
    .build();
}

/**
 * Create a query receipt (SPARQL, etc.)
 *
 * @param {object} options - Receipt options
 * @returns {Promise<import('./receipt-schemas.mjs').UniversalReceipt>}
 */
export async function createQueryReceipt(options) {
  const {
    pkg,
    queryType,
    queryHash,
    resultCount,
    bindings,
    graphsAccessed = [],
    success,
    duration,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const decision = success ? 'ALLOW' : 'DENY';
  const reason = success
    ? `${queryType} returned ${resultCount} result(s)`
    : `Query execution failed`;

  const builder = new ReceiptBuilder('query', pkg, { timestamp });
  const startTime = timestamp || new Date();
  const endTime = new Date(startTime.getTime() + (duration || 0));

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'query-engine', ...provenance })
    .input({ hashes: { query: queryHash || 'unknown' } })
    .output({ hash: `query-result-${resultCount || 0}` })
    .metrics({
      duration: duration || 0,
      startTime: startTime.toISOString(),
      endTime: endTime.toISOString(),
    })
    .chain(beforeReceiptHash)
    .extension({
      type: 'query',
      data: {
        queryType,
        queryHash,
        resultCount,
        bindings,
        graphsAccessed,
      },
    })
    .build();
}

/**
 * Create a workflow receipt
 *
 * @param {object} options - Receipt options
 * @returns {Promise<import('./receipt-schemas.mjs').UniversalReceipt>}
 */
export async function createWorkflowReceipt(options) {
  const {
    pkg,
    workflowId,
    workflowName,
    taskCount,
    completedTasks,
    failedTasks = 0,
    steps = [],
    success,
    duration,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const decision = success ? 'ALLOW' : 'DENY';
  const reason = success
    ? `Workflow ${workflowName || workflowId} completed ${completedTasks}/${taskCount} tasks`
    : `Workflow failed: ${failedTasks} task(s) failed`;

  const builder = new ReceiptBuilder('workflow', pkg, { timestamp });
  const startTime = timestamp || new Date();
  const endTime = new Date(startTime.getTime() + (duration || 0));

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'workflow-engine', ...provenance })
    .input({ hashes: { workflow: workflowId } })
    .output({ hash: `workflow-${workflowId}-${completedTasks}` })
    .metrics({
      duration: duration || 0,
      startTime: startTime.toISOString(),
      endTime: endTime.toISOString(),
    })
    .chain(beforeReceiptHash)
    .extension({
      type: 'workflow',
      data: {
        workflowId,
        workflowName,
        taskCount,
        completedTasks,
        failedTasks,
        steps,
      },
    })
    .build();
}
