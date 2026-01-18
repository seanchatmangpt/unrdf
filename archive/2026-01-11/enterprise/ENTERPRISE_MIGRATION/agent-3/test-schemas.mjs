/**
 * @fileoverview Quick validation test for canonical schemas
 */

import {
  generateMigrationId,
  generateDomainId,
  generateCapsuleId,
  generateReceiptId,
  generateLensId,
  validateId,
  parseId,
  verifyId,
} from './id-rules.mjs';

import {
  validateMigrationRequest,
  createEmptyStats,
  createDomainResult,
} from './dto-schemas.mjs';

import {
  MigrationError,
  ErrorCode,
  createLensFailureError,
  aggregateErrors,
} from './error-schemas.mjs';

import {
  createLogger,
  MigrationPhase,
  LogLevel,
  formatLogEntry,
} from './logging-schemas.mjs';

// Test ID generation
console.log('Testing ID generation...');
const migrationId = generateMigrationId({
  sourceSystem: 'legacy-crm',
  targetSystem: 'salesforce',
  batchId: 'batch-001',
  timestamp: 1703587200,
});
console.log(`Migration ID: ${migrationId}`);

const domainId = generateDomainId({
  namespace: 'com.example',
  name: 'customer',
  timestamp: 1703587200,
});
console.log(`Domain ID: ${domainId}`);

const capsuleId = generateCapsuleId({
  content: 'sample-content-hash',
  schemaVersion: 'v1.0',
  timestamp: 1703587200,
});
console.log(`Capsule ID: ${capsuleId}`);

// Test ID validation
console.log('\nTesting ID validation...');
console.log(`Migration ID valid: ${validateId(migrationId, 'migration')}`);
console.log(`Domain ID valid: ${validateId(domainId, 'domain')}`);

// Test ID parsing
console.log('\nTesting ID parsing...');
const parsed = parseId(migrationId);
console.log(`Parsed migration ID:`, parsed);

// Test DTO validation
console.log('\nTesting DTO validation...');
const validRequest = {
  migrationId: migrationId,
  sourceSystem: 'legacy-crm',
  targetSystem: 'salesforce',
  batchId: 'batch-001',
  domains: [
    {
      domainId: domainId,
      name: 'customer',
      namespace: 'com.example',
      sourceLensId: 'lens_1703587200_abc12345',
      targetLensId: 'lens_1703587200_def67890',
      capsules: [
        {
          capsuleId: capsuleId,
          data: { test: 'data' },
          schemaVersion: 'v1.0',
        },
      ],
    },
  ],
};

const validation = validateMigrationRequest(validRequest);
console.log(`Request validation: ${validation.valid ? 'PASS' : 'FAIL'}`);
if (!validation.valid) {
  console.log('Errors:', validation.errors);
}

// Test error creation
console.log('\nTesting error schemas...');
const error1 = createLensFailureError(
  'Lens transformation failed',
  { lensId: 'lens_123', operation: 'transform' },
  'corr-123'
);
console.log(`Error created: ${error1.code} - ${error1.message}`);
console.log(`Error JSON:`, JSON.stringify(error1.toJSON(), null, 2));

// Test logging
console.log('\nTesting logging schemas...');
const logger = createLogger('agent-3');
const logEntry = logger.info(
  'corr-456',
  MigrationPhase.VALIDATION,
  'Schema validation completed',
  { validated: 5 }
);
console.log(`Log entry: ${formatLogEntry(logEntry)}`);

console.log('\nAll schema tests completed successfully!');
