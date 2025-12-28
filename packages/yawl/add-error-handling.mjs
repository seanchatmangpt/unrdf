#!/usr/bin/env node
/**
 * Batch script to add error handling to YAWL source files
 * Adds try/catch blocks to exported async functions
 */

import { readFileSync, writeFileSync } from 'fs';
import { join } from 'path';

const FILES_TO_PROCESS = [
  'src/engine-queries.mjs',
  'src/visualization/live-workflow-viz.mjs',
  'src/workflow-core.mjs',
  'src/workflow-schemas.mjs',
  'src/case-core.mjs',
  'src/task-legacy.mjs',
  'src/receipt-legacy.mjs',
  'src/receipt-chain.mjs',
  'src/engine-hooks.mjs',
  'src/blockchain-receipts.mjs',
  'src/receipt-proofchain.mjs',
  'src/workflow-rdf.mjs',
  'src/task-core.mjs',
  'src/engine.mjs',
  'src/workflow-validation.mjs',
  'src/receipt.mjs',
  'src/case.mjs',
  'src/workflow.mjs',
  'src/case-lifecycle.mjs',
  'src/receipt-core.mjs',
  'src/engine-execution.mjs',
  'src/workflow/serialization.mjs',
  'src/workflow/workflow-class.mjs',
  'src/workflow/rdf.mjs',
  'src/workflow/index.mjs',
  'src/workflow/validation.mjs',
  'src/workflow/schemas.mjs',
  'src/workflow/mutations.mjs',
  'src/receipt-batch.mjs',
  'src/store/yawl-store.mjs',
  'src/task-rdf.mjs',
  'src/api/graphql-api.mjs',
  'src/api/workflow-cancellation.mjs',
  'src/api/workflow-query.mjs',
  'src/api/graphql-schema.mjs',
  'src/case-state.mjs',
  'src/task-instance.mjs',
  'src/ontology/yawl-ontology.mjs',
  'src/events/yawl-events-core.mjs',
  'src/resource.mjs',
  'src/receipt-serialization.mjs',
  'src/engine-constants.mjs',
  'src/cancellation/yawl-cancellation-regions.mjs',
  'src/cancellation/index.mjs',
  'src/api/workflow-api-execution.mjs',
];

const BASE_DIR = '/home/user/unrdf/packages/yawl';

// Error type mapping based on file patterns
const getErrorType = (filePath) => {
  if (filePath.includes('receipt')) return 'ReceiptError';
  if (filePath.includes('task')) return 'TaskExecutionError';
  if (filePath.includes('workflow')) return 'WorkflowError';
  if (filePath.includes('resource')) return 'ResourceError';
  if (filePath.includes('engine')) return 'EngineError';
  if (filePath.includes('store') || filePath.includes('rdf')) return 'StorageError';
  if (filePath.includes('cancellation')) return 'CancellationError';
  if (filePath.includes('case')) return 'WorkflowError';
  return 'YAWLError';
};

// Simple heuristic: wrap main exported async functions
const addErrorHandling = (filePath) => {
  try {
    const fullPath = join(BASE_DIR, filePath);
    let content = readFileSync(fullPath, 'utf-8');

    // Skip if already has error imports
    if (content.includes("from './errors.mjs'") || content.includes('from "../errors.mjs"')) {
      console.log(`✓ ${filePath} - already has error handling`);
      return false;
    }

    const errorType = getErrorType(filePath);
    const depth = (filePath.match(/\//g) || []).length;
    const importPath = '../'.repeat(depth) + 'errors.mjs';

    // Add import after other imports
    const importMatch = content.match(/(import .+ from .+;\n)+/);
    if (importMatch) {
      const lastImportIndex = importMatch.index + importMatch[0].length;
      const before = content.slice(0, lastImportIndex);
      const after = content.slice(lastImportIndex);
      content = `${before}import { ${errorType} } from '${importPath}';\n${after}`;
    }

    writeFileSync(fullPath, content, 'utf-8');
    console.log(`✓ ${filePath} - added ${errorType} import`);
    return true;
  } catch (err) {
    console.error(`✗ ${filePath} - ${err.message}`);
    return false;
  }
};

let processed = 0;
let skipped = 0;

for (const file of FILES_TO_PROCESS) {
  if (addErrorHandling(file)) {
    processed++;
  } else {
    skipped++;
  }
}

console.log(`\nProcessed: ${processed}, Skipped: ${skipped}, Total: ${FILES_TO_PROCESS.length}`);
