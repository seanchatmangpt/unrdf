/**
 * @fileoverview Convention-preserving code generator
 * @module agent-7/generator
 */

import { hashGeneratedCode, hashInputs } from './determinism.mjs';

/**
 * Generate service façade matching organizational conventions
 *
 * @param {Object} serviceSpec - Service specification
 * @param {string} serviceSpec.name - Service name (e.g., 'CustomerService')
 * @param {Array<string>} serviceSpec.entities - Entity names (e.g., ['Customer'])
 * @param {Array<string>} serviceSpec.operations - Operations (e.g., ['create', 'read', 'update', 'delete', 'list'])
 * @param {Object} compiledProfile - Compiled convention profile from Agent 6
 * @param {Object} compiledProfile.naming - Naming conventions
 * @param {Object} compiledProfile.errorModel - Error handling model
 * @param {Object} compiledProfile.logging - Logging configuration
 * @param {Object} compiledProfile.testing - Testing configuration
 * @param {Object} compiledLens - Compiled lens from Agent 3
 * @returns {Object} Generated code result { code, testCode, filename, hash, metadata }
 */
export async function generateFacade(serviceSpec, compiledProfile, compiledLens) {
  // Apply lens transformation if provided
  let transformedSpec = serviceSpec;
  if (compiledLens && compiledLens.transform) {
    transformedSpec = compiledLens.transform(serviceSpec);
  }

  // Generate service class
  const serviceCode = templateServiceClass(
    transformedSpec.name,
    transformedSpec.entities,
    transformedSpec.operations,
    compiledProfile
  );

  // Generate test file
  const testCode = templateTest(
    transformedSpec.name,
    transformedSpec.entities,
    transformedSpec.operations,
    compiledProfile
  );

  // Generate filename based on naming conventions
  const filename = generateFilename(transformedSpec.name, compiledProfile);

  // Calculate hashes
  const codeHash = hashGeneratedCode(serviceCode);
  const inputHash = hashInputs(serviceSpec, compiledProfile, compiledLens);

  return {
    code: serviceCode,
    testCode,
    filename,
    hash: codeHash,
    metadata: {
      inputHash,
      generatedAt: new Date().toISOString(),
      profile: compiledProfile.name || 'unknown',
      lens: compiledLens?.name || 'identity',
      serviceName: transformedSpec.name,
      entities: transformedSpec.entities,
      operations: transformedSpec.operations
    }
  };
}

/**
 * Generate service class skeleton matching conventions
 *
 * @param {string} serviceName - Name of service class
 * @param {Array<string>} entities - Entity names
 * @param {Array<string>} operations - CRUD operations
 * @param {Object} profile - Compiled profile
 * @returns {string} Service class code
 */
export function templateServiceClass(serviceName, entities, operations, profile) {
  const errorClass = profile.errorModel?.className || 'AppError';
  const codeField = profile.errorModel?.codeField || 'code';
  const messageField = profile.errorModel?.messageField || 'message';
  const detailsField = profile.errorModel?.detailsField || 'details';

  const logFields = profile.logging?.fields || ['timestamp', 'level', 'service', 'operation'];
  const logMethod = profile.logging?.method || 'info';

  const entityName = entities[0] || 'Entity';
  const entityLower = entityName.toLowerCase();

  // Build methods for each operation
  const methods = operations.map(op => {
    return generateMethod(op, entityName, profile);
  }).join('\n\n');

  const code = `/**
 * @fileoverview ${serviceName} - Generated service façade
 * @generated Convention-preserving code generator (Agent 7)
 */

import { dataFactory, createStore } from '@unrdf/oxigraph';
import { ${errorClass} } from './errors.mjs';
import { logger } from './logger.mjs';

/**
 * ${serviceName} service
 * Provides CRUD operations for ${entityName} entities
 * @class
 */
export class ${serviceName} {
  /**
   * Create new ${serviceName}
   * @param {Object} store - RDF store instance
   */
  constructor(store) {
    this.store = store;
    this.serviceName = '${serviceName}';
  }

${methods}
}
`;

  return code;
}

/**
 * Generate individual method based on operation type
 *
 * @param {string} operation - Operation name (create, read, update, delete, list)
 * @param {string} entityName - Entity name
 * @param {Object} profile - Compiled profile
 * @returns {string} Method code
 */
function generateMethod(operation, entityName, profile) {
  const errorClass = profile.errorModel?.className || 'AppError';
  const codeField = profile.errorModel?.codeField || 'code';
  const logFields = profile.logging?.fields || ['timestamp', 'level', 'service', 'operation'];
  const logMethod = profile.logging?.method || 'info';

  const entityLower = entityName.toLowerCase();
  const capitalizedEntity = entityName.charAt(0).toUpperCase() + entityName.slice(1);

  switch (operation) {
    case 'create':
      return `  /**
   * Create a new ${entityLower}
   * @param {Object} dto - Data transfer object
   * @param {string} dto.id - ${capitalizedEntity} ID
   * @param {Object} dto.data - ${capitalizedEntity} data
   * @returns {Promise<Object>} Created ${entityLower}
   * @throws {${errorClass}}
   */
  async create${capitalizedEntity}(dto) {
    const operation = 'create${capitalizedEntity}';

    try {
      // Log operation start
      logger.${logMethod}({
        ${logFields.map(f => {
          if (f === 'timestamp') return 'timestamp: new Date().toISOString()';
          if (f === 'level') return `level: '${logMethod}'`;
          if (f === 'service') return 'service: this.serviceName';
          if (f === 'operation') return 'operation';
          if (f === 'traceId') return 'traceId: dto.traceId || ""';
          return `${f}: ""`;
        }).join(',\n        ')}
      });

      // Validate input
      if (!dto || !dto.id) {
        throw new ${errorClass}(
          'VALIDATION_ERROR',
          '${capitalizedEntity} ID is required',
          { field: 'id' }
        );
      }

      // Create RDF quads
      const subject = dataFactory.namedNode(\`urn:${entityLower}:\${dto.id}\`);
      const predicate = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
      const object = dataFactory.namedNode('urn:${entityLower}');
      const quad = dataFactory.quad(subject, predicate, object);

      // Store in RDF store
      this.store.add(quad);

      return {
        id: dto.id,
        type: '${entityLower}',
        created: true
      };
    } catch (error) {
      if (error instanceof ${errorClass}) {
        throw error;
      }
      throw new ${errorClass}(
        'CREATE_ERROR',
        \`Failed to create ${entityLower}: \${error.message}\`,
        { originalError: error.message }
      );
    }
  }`;

    case 'read':
      return `  /**
   * Read ${entityLower} by ID
   * @param {string} id - ${capitalizedEntity} ID
   * @returns {Promise<Object>} ${capitalizedEntity} data
   * @throws {${errorClass}}
   */
  async read${capitalizedEntity}(id) {
    const operation = 'read${capitalizedEntity}';

    try {
      logger.${logMethod}({
        ${logFields.map(f => {
          if (f === 'timestamp') return 'timestamp: new Date().toISOString()';
          if (f === 'level') return `level: '${logMethod}'`;
          if (f === 'service') return 'service: this.serviceName';
          if (f === 'operation') return 'operation';
          return `${f}: ""`;
        }).join(',\n        ')}
      });

      if (!id) {
        throw new ${errorClass}(
          'VALIDATION_ERROR',
          '${capitalizedEntity} ID is required',
          { field: 'id' }
        );
      }

      const subject = dataFactory.namedNode(\`urn:${entityLower}:\${id}\`);
      const quads = this.store.match(subject, null, null, null);

      if (quads.length === 0) {
        throw new ${errorClass}(
          'NOT_FOUND',
          \`${capitalizedEntity} not found: \${id}\`,
          { id }
        );
      }

      return {
        id,
        type: '${entityLower}',
        exists: true
      };
    } catch (error) {
      if (error instanceof ${errorClass}) {
        throw error;
      }
      throw new ${errorClass}(
        'READ_ERROR',
        \`Failed to read ${entityLower}: \${error.message}\`,
        { originalError: error.message }
      );
    }
  }`;

    case 'update':
      return `  /**
   * Update ${entityLower}
   * @param {string} id - ${capitalizedEntity} ID
   * @param {Object} data - Update data
   * @returns {Promise<Object>} Updated ${entityLower}
   * @throws {${errorClass}}
   */
  async update${capitalizedEntity}(id, data) {
    const operation = 'update${capitalizedEntity}';

    try {
      logger.${logMethod}({
        ${logFields.map(f => {
          if (f === 'timestamp') return 'timestamp: new Date().toISOString()';
          if (f === 'level') return `level: '${logMethod}'`;
          if (f === 'service') return 'service: this.serviceName';
          if (f === 'operation') return 'operation';
          return `${f}: ""`;
        }).join(',\n        ')}
      });

      if (!id) {
        throw new ${errorClass}(
          'VALIDATION_ERROR',
          '${capitalizedEntity} ID is required',
          { field: 'id' }
        );
      }

      // Verify exists
      await this.read${capitalizedEntity}(id);

      return {
        id,
        type: '${entityLower}',
        updated: true
      };
    } catch (error) {
      if (error instanceof ${errorClass}) {
        throw error;
      }
      throw new ${errorClass}(
        'UPDATE_ERROR',
        \`Failed to update ${entityLower}: \${error.message}\`,
        { originalError: error.message }
      );
    }
  }`;

    case 'delete':
      return `  /**
   * Delete ${entityLower}
   * @param {string} id - ${capitalizedEntity} ID
   * @returns {Promise<Object>} Deletion result
   * @throws {${errorClass}}
   */
  async delete${capitalizedEntity}(id) {
    const operation = 'delete${capitalizedEntity}';

    try {
      logger.${logMethod}({
        ${logFields.map(f => {
          if (f === 'timestamp') return 'timestamp: new Date().toISOString()';
          if (f === 'level') return `level: '${logMethod}'`;
          if (f === 'service') return 'service: this.serviceName';
          if (f === 'operation') return 'operation';
          return `${f}: ""`;
        }).join(',\n        ')}
      });

      if (!id) {
        throw new ${errorClass}(
          'VALIDATION_ERROR',
          '${capitalizedEntity} ID is required',
          { field: 'id' }
        );
      }

      const subject = dataFactory.namedNode(\`urn:${entityLower}:\${id}\`);
      const quads = this.store.match(subject, null, null, null);

      for (const quad of quads) {
        this.store.delete(quad);
      }

      return {
        id,
        deleted: true
      };
    } catch (error) {
      if (error instanceof ${errorClass}) {
        throw error;
      }
      throw new ${errorClass}(
        'DELETE_ERROR',
        \`Failed to delete ${entityLower}: \${error.message}\`,
        { originalError: error.message }
      );
    }
  }`;

    case 'list':
      return `  /**
   * List all ${entityLower}s
   * @param {Object} [options={}] - List options
   * @param {number} [options.limit=100] - Maximum results
   * @param {number} [options.offset=0] - Result offset
   * @returns {Promise<Array<Object>>} Array of ${entityLower}s
   * @throws {${errorClass}}
   */
  async list${capitalizedEntity}s(options = {}) {
    const operation = 'list${capitalizedEntity}s';
    const limit = options.limit || 100;
    const offset = options.offset || 0;

    try {
      logger.${logMethod}({
        ${logFields.map(f => {
          if (f === 'timestamp') return 'timestamp: new Date().toISOString()';
          if (f === 'level') return `level: '${logMethod}'`;
          if (f === 'service') return 'service: this.serviceName';
          if (f === 'operation') return 'operation';
          return `${f}: ""`;
        }).join(',\n        ')}
      });

      const type = dataFactory.namedNode('urn:${entityLower}');
      const quads = this.store.match(null, null, type, null);

      const results = [];
      for (const quad of quads) {
        const id = quad.subject.value.replace('urn:${entityLower}:', '');
        results.push({
          id,
          type: '${entityLower}'
        });
      }

      return results.slice(offset, offset + limit);
    } catch (error) {
      throw new ${errorClass}(
        'LIST_ERROR',
        \`Failed to list ${entityLower}s: \${error.message}\`,
        { originalError: error.message }
      );
    }
  }`;

    default:
      return `  /**
   * ${operation} operation
   * @param {Object} params - Operation parameters
   * @returns {Promise<Object>} Operation result
   */
  async ${operation}(params) {
    throw new ${errorClass}(
      'NOT_IMPLEMENTED',
      'Operation ${operation} not implemented',
      { operation: '${operation}' }
    );
  }`;
  }
}

/**
 * Generate error handler class matching profile
 *
 * @param {Object} profileErrorModel - Error model from profile
 * @returns {string} Error class code
 */
export function templateErrorHandler(profileErrorModel) {
  const className = profileErrorModel.className || 'AppError';
  const codeField = profileErrorModel.codeField || 'code';
  const messageField = profileErrorModel.messageField || 'message';
  const detailsField = profileErrorModel.detailsField || 'details';

  const code = `/**
 * Application error matching profile conventions
 * @generated Convention-preserving code generator (Agent 7)
 */

/**
 * ${className} - Custom error class
 * @extends Error
 */
export class ${className} extends Error {
  /**
   * Create application error
   * @param {string} code - Error code
   * @param {string} message - Error message
   * @param {Object} [details={}] - Additional error details
   */
  constructor(code, message, details = {}) {
    super(message);
    this.${codeField} = code;
    this.${messageField} = message;
    this.${detailsField} = details;
    this.name = this.constructor.name;
    Error.captureStackTrace(this, this.constructor);
  }

  /**
   * Convert to JSON representation
   * @returns {Object} JSON object
   */
  toJSON() {
    return {
      ${codeField}: this.${codeField},
      ${messageField}: this.${messageField},
      ${detailsField}: this.${detailsField},
      name: this.name,
      stack: this.stack
    };
  }
}
`;

  return code;
}

/**
 * Generate test file matching profile testing conventions
 *
 * @param {string} serviceName - Service name
 * @param {Array<string>} entities - Entity names
 * @param {Array<string>} operations - Operations to test
 * @param {Object} profile - Compiled profile
 * @returns {string} Test file code
 */
export function templateTest(serviceName, entities, operations, profile) {
  const framework = profile.testing?.framework || 'node:test';
  const minCoverage = profile.testing?.minCoverage || 80;
  const errorClass = profile.errorModel?.className || 'AppError';

  const entityName = entities[0] || 'Entity';
  const entityLower = entityName.toLowerCase();
  const capitalizedEntity = entityName.charAt(0).toUpperCase() + entityName.slice(1);

  // Generate tests for each operation
  const testCases = operations.map(op => generateTestCase(op, serviceName, capitalizedEntity, errorClass, framework)).join('\n\n');

  const code = `/**
 * @fileoverview Tests for ${serviceName}
 * @generated Convention-preserving code generator (Agent 7)
 * Target coverage: ${minCoverage}%
 */

import { describe, it, beforeEach } from '${framework}';
import assert from 'assert';
import { createStore } from '@unrdf/oxigraph';
import { ${serviceName} } from './${generateFilename(serviceName, profile)}';
import { ${errorClass} } from './errors.mjs';

describe('${serviceName}', () => {
  let service;
  let store;

  beforeEach(() => {
    store = createStore();
    service = new ${serviceName}(store);
  });

${testCases}
});
`;

  return code;
}

/**
 * Generate test case for specific operation
 *
 * @param {string} operation - Operation name
 * @param {string} serviceName - Service name
 * @param {string} entityName - Entity name
 * @param {string} errorClass - Error class name
 * @param {string} framework - Test framework
 * @returns {string} Test case code
 */
function generateTestCase(operation, serviceName, entityName, errorClass, framework) {
  const entityLower = entityName.toLowerCase();

  switch (operation) {
    case 'create':
      return `  describe('create${entityName}', () => {
    it('should create ${entityLower} successfully', async () => {
      const dto = { id: 'test-1', data: { name: 'Test' } };
      const result = await service.create${entityName}(dto);

      assert.strictEqual(result.id, 'test-1');
      assert.strictEqual(result.created, true);
    });

    it('should throw error when ID is missing', async () => {
      await assert.rejects(
        async () => await service.create${entityName}({}),
        ${errorClass}
      );
    });
  })`;

    case 'read':
      return `  describe('read${entityName}', () => {
    it('should read existing ${entityLower}', async () => {
      const dto = { id: 'test-2', data: {} };
      await service.create${entityName}(dto);

      const result = await service.read${entityName}('test-2');
      assert.strictEqual(result.id, 'test-2');
      assert.strictEqual(result.exists, true);
    });

    it('should throw error when ${entityLower} not found', async () => {
      await assert.rejects(
        async () => await service.read${entityName}('non-existent'),
        ${errorClass}
      );
    });
  })`;

    case 'update':
      return `  describe('update${entityName}', () => {
    it('should update existing ${entityLower}', async () => {
      const dto = { id: 'test-3', data: {} };
      await service.create${entityName}(dto);

      const result = await service.update${entityName}('test-3', { name: 'Updated' });
      assert.strictEqual(result.updated, true);
    });

    it('should throw error when ${entityLower} not found', async () => {
      await assert.rejects(
        async () => await service.update${entityName}('non-existent', {}),
        ${errorClass}
      );
    });
  })`;

    case 'delete':
      return `  describe('delete${entityName}', () => {
    it('should delete ${entityLower}', async () => {
      const dto = { id: 'test-4', data: {} };
      await service.create${entityName}(dto);

      const result = await service.delete${entityName}('test-4');
      assert.strictEqual(result.deleted, true);
    });

    it('should throw error when ID is missing', async () => {
      await assert.rejects(
        async () => await service.delete${entityName}(null),
        ${errorClass}
      );
    });
  })`;

    case 'list':
      return `  describe('list${entityName}s', () => {
    it('should list all ${entityLower}s', async () => {
      await service.create${entityName}({ id: 'test-5', data: {} });
      await service.create${entityName}({ id: 'test-6', data: {} });

      const results = await service.list${entityName}s();
      assert.ok(results.length >= 2);
    });

    it('should respect limit option', async () => {
      await service.create${entityName}({ id: 'test-7', data: {} });
      await service.create${entityName}({ id: 'test-8', data: {} });

      const results = await service.list${entityName}s({ limit: 1 });
      assert.strictEqual(results.length, 1);
    });
  })`;

    default:
      return `  describe('${operation}', () => {
    it('should throw NOT_IMPLEMENTED error', async () => {
      await assert.rejects(
        async () => await service.${operation}({}),
        ${errorClass}
      );
    });
  })`;
  }
}

/**
 * Generate filename based on naming conventions
 *
 * @param {string} serviceName - Service name
 * @param {Object} profile - Compiled profile
 * @returns {string} Filename (e.g., 'customer-service.mjs')
 */
function generateFilename(serviceName, profile) {
  const style = profile.naming?.fileStyle || 'kebab-case';

  let filename = serviceName;

  switch (style) {
    case 'kebab-case':
      filename = serviceName
        .replace(/([a-z])([A-Z])/g, '$1-$2')
        .toLowerCase();
      break;
    case 'snake_case':
      filename = serviceName
        .replace(/([a-z])([A-Z])/g, '$1_$2')
        .toLowerCase();
      break;
    case 'camelCase':
      filename = serviceName.charAt(0).toLowerCase() + serviceName.slice(1);
      break;
    case 'PascalCase':
      filename = serviceName;
      break;
    default:
      filename = serviceName
        .replace(/([a-z])([A-Z])/g, '$1-$2')
        .toLowerCase();
  }

  return `${filename}.mjs`;
}

/**
 * Demo service specification
 * @type {Object}
 */
export const demoCustomerServiceSpec = {
  name: 'CustomerService',
  entities: ['Customer'],
  operations: ['create', 'read', 'update', 'delete', 'list']
};
