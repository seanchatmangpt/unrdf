/**
 * @fileoverview DTO Schemas for Enterprise Migration
 * Request/Response data transfer objects with validation
 */

/**
 * @typedef {object} MigrationRequest
 * @property {string} migrationId - Unique migration identifier
 * @property {string} sourceSystem - Source system identifier
 * @property {string} targetSystem - Target system identifier
 * @property {string} batchId - Batch identifier
 * @property {DomainMigration[]} domains - Domains to migrate
 * @property {object} [metadata] - Optional metadata
 * @property {string} [metadata.initiator] - User/system that initiated migration
 * @property {number} [metadata.priority] - Priority level (1-10)
 * @property {string[]} [metadata.tags] - Classification tags
 */

/**
 * @typedef {object} DomainMigration
 * @property {string} domainId - Domain identifier
 * @property {string} name - Domain name
 * @property {string} namespace - Domain namespace
 * @property {string} sourceLensId - Source lens identifier
 * @property {string} targetLensId - Target lens identifier
 * @property {object[]} capsules - Data capsules to migrate
 * @property {string} capsules[].capsuleId - Capsule identifier
 * @property {object} capsules[].data - Capsule data payload
 * @property {string} capsules[].schemaVersion - Schema version
 */

/**
 * @typedef {object} MigrationResponse
 * @property {string} migrationId - Migration identifier
 * @property {'pending' | 'in_progress' | 'completed' | 'failed' | 'partial'} status - Migration status
 * @property {number} timestamp - Response timestamp (Unix)
 * @property {MigrationStats} stats - Migration statistics
 * @property {DomainResult[]} domainResults - Per-domain results
 * @property {string[]} [errors] - Error messages if any
 * @property {object} [metadata] - Optional response metadata
 */

/**
 * @typedef {object} MigrationStats
 * @property {number} totalDomains - Total domains in migration
 * @property {number} successfulDomains - Successfully migrated domains
 * @property {number} failedDomains - Failed domain migrations
 * @property {number} totalCapsules - Total capsules processed
 * @property {number} successfulCapsules - Successfully migrated capsules
 * @property {number} failedCapsules - Failed capsule migrations
 * @property {number} durationMs - Total duration in milliseconds
 */

/**
 * @typedef {object} DomainResult
 * @property {string} domainId - Domain identifier
 * @property {'success' | 'failed' | 'partial'} status - Domain migration status
 * @property {number} capsulesProcessed - Number of capsules processed
 * @property {number} capsulesSucceeded - Number of capsules succeeded
 * @property {number} capsulesFailed - Number of capsules failed
 * @property {ReceiptInfo[]} receipts - Migration receipts
 * @property {string[]} [errors] - Domain-level errors
 */

/**
 * @typedef {object} ReceiptInfo
 * @property {string} receiptId - Receipt identifier
 * @property {string} capsuleId - Associated capsule ID
 * @property {number} timestamp - Receipt timestamp (Unix)
 * @property {string} operation - Operation performed
 * @property {'success' | 'failed'} status - Receipt status
 * @property {string} [errorCode] - Error code if failed
 */

/**
 * Validate MigrationRequest DTO
 * @param {unknown} obj - Object to validate
 * @returns {{valid: boolean, errors: string[]}} Validation result
 */
export function validateMigrationRequest(obj) {
  const errors = [];

  if (!obj || typeof obj !== 'object') {
    return { valid: false, errors: ['Request must be an object'] };
  }

  if (typeof obj.migrationId !== 'string' || obj.migrationId.length === 0) {
    errors.push('migrationId must be a non-empty string');
  }

  if (typeof obj.sourceSystem !== 'string' || obj.sourceSystem.length === 0) {
    errors.push('sourceSystem must be a non-empty string');
  }

  if (typeof obj.targetSystem !== 'string' || obj.targetSystem.length === 0) {
    errors.push('targetSystem must be a non-empty string');
  }

  if (typeof obj.batchId !== 'string' || obj.batchId.length === 0) {
    errors.push('batchId must be a non-empty string');
  }

  if (!Array.isArray(obj.domains)) {
    errors.push('domains must be an array');
  } else if (obj.domains.length === 0) {
    errors.push('domains array cannot be empty');
  } else {
    obj.domains.forEach((domain, index) => {
      const domainErrors = validateDomainMigration(domain);
      if (!domainErrors.valid) {
        errors.push(`domains[${index}]: ${domainErrors.errors.join(', ')}`);
      }
    });
  }

  if (obj.metadata !== undefined) {
    if (typeof obj.metadata !== 'object' || obj.metadata === null) {
      errors.push('metadata must be an object if provided');
    } else {
      if (obj.metadata.priority !== undefined) {
        if (typeof obj.metadata.priority !== 'number' ||
            obj.metadata.priority < 1 ||
            obj.metadata.priority > 10) {
          errors.push('metadata.priority must be a number between 1-10');
        }
      }
      if (obj.metadata.tags !== undefined && !Array.isArray(obj.metadata.tags)) {
        errors.push('metadata.tags must be an array if provided');
      }
    }
  }

  return { valid: errors.length === 0, errors };
}

/**
 * Validate DomainMigration DTO
 * @param {unknown} obj - Object to validate
 * @returns {{valid: boolean, errors: string[]}} Validation result
 */
export function validateDomainMigration(obj) {
  const errors = [];

  if (!obj || typeof obj !== 'object') {
    return { valid: false, errors: ['Domain must be an object'] };
  }

  if (typeof obj.domainId !== 'string' || obj.domainId.length === 0) {
    errors.push('domainId must be a non-empty string');
  }

  if (typeof obj.name !== 'string' || obj.name.length === 0) {
    errors.push('name must be a non-empty string');
  }

  if (typeof obj.namespace !== 'string' || obj.namespace.length === 0) {
    errors.push('namespace must be a non-empty string');
  }

  if (typeof obj.sourceLensId !== 'string' || obj.sourceLensId.length === 0) {
    errors.push('sourceLensId must be a non-empty string');
  }

  if (typeof obj.targetLensId !== 'string' || obj.targetLensId.length === 0) {
    errors.push('targetLensId must be a non-empty string');
  }

  if (!Array.isArray(obj.capsules)) {
    errors.push('capsules must be an array');
  } else {
    obj.capsules.forEach((capsule, index) => {
      if (!capsule || typeof capsule !== 'object') {
        errors.push(`capsules[${index}] must be an object`);
      } else {
        if (typeof capsule.capsuleId !== 'string' || capsule.capsuleId.length === 0) {
          errors.push(`capsules[${index}].capsuleId must be a non-empty string`);
        }
        if (!capsule.data || typeof capsule.data !== 'object') {
          errors.push(`capsules[${index}].data must be an object`);
        }
        if (typeof capsule.schemaVersion !== 'string' || capsule.schemaVersion.length === 0) {
          errors.push(`capsules[${index}].schemaVersion must be a non-empty string`);
        }
      }
    });
  }

  return { valid: errors.length === 0, errors };
}

/**
 * Validate MigrationResponse DTO
 * @param {unknown} obj - Object to validate
 * @returns {{valid: boolean, errors: string[]}} Validation result
 */
export function validateMigrationResponse(obj) {
  const errors = [];
  const validStatuses = ['pending', 'in_progress', 'completed', 'failed', 'partial'];

  if (!obj || typeof obj !== 'object') {
    return { valid: false, errors: ['Response must be an object'] };
  }

  if (typeof obj.migrationId !== 'string' || obj.migrationId.length === 0) {
    errors.push('migrationId must be a non-empty string');
  }

  if (!validStatuses.includes(obj.status)) {
    errors.push(`status must be one of: ${validStatuses.join(', ')}`);
  }

  if (typeof obj.timestamp !== 'number' || obj.timestamp <= 0) {
    errors.push('timestamp must be a positive number');
  }

  if (!obj.stats || typeof obj.stats !== 'object') {
    errors.push('stats must be an object');
  } else {
    const requiredStats = [
      'totalDomains', 'successfulDomains', 'failedDomains',
      'totalCapsules', 'successfulCapsules', 'failedCapsules', 'durationMs'
    ];
    requiredStats.forEach(field => {
      if (typeof obj.stats[field] !== 'number' || obj.stats[field] < 0) {
        errors.push(`stats.${field} must be a non-negative number`);
      }
    });
  }

  if (!Array.isArray(obj.domainResults)) {
    errors.push('domainResults must be an array');
  }

  if (obj.errors !== undefined && !Array.isArray(obj.errors)) {
    errors.push('errors must be an array if provided');
  }

  return { valid: errors.length === 0, errors };
}

/**
 * Create empty MigrationStats object
 * @returns {MigrationStats} Empty stats
 */
export function createEmptyStats() {
  return {
    totalDomains: 0,
    successfulDomains: 0,
    failedDomains: 0,
    totalCapsules: 0,
    successfulCapsules: 0,
    failedCapsules: 0,
    durationMs: 0,
  };
}

/**
 * Create DomainResult from domain migration
 * @param {DomainMigration} domain - Domain migration
 * @param {'success' | 'failed' | 'partial'} status - Result status
 * @param {ReceiptInfo[]} receipts - Receipts
 * @param {string[]} [errors] - Errors if any
 * @returns {DomainResult} Domain result
 */
export function createDomainResult(domain, status, receipts, errors = []) {
  const succeeded = receipts.filter(r => r.status === 'success').length;
  const failed = receipts.filter(r => r.status === 'failed').length;

  return {
    domainId: domain.domainId,
    status,
    capsulesProcessed: receipts.length,
    capsulesSucceeded: succeeded,
    capsulesFailed: failed,
    receipts,
    ...(errors.length > 0 && { errors }),
  };
}
