/**
 * @file Demo Conventions Profile
 * @description Example profile for enterprise-style Node.js services
 */

/**
 * Enterprise Service Standards Profile
 * @type {import('./profile-schema.mjs').ConventionsProfile}
 */
export const demoProfile = {
  name: 'Enterprise Service Standards',
  description: 'Standard conventions for internal microservices following industry best practices',

  fileLayout: {
    serviceModules: [
      'src/services/*/index.mjs',
      'src/services/*.mjs'
    ],
    testModules: [
      'test/services/*.test.mjs',
      'test/**/*.test.mjs'
    ],
    naming: {
      serviceClass: 'PascalCase',
      method: 'camelCase',
      constant: 'UPPER_SNAKE_CASE'
    }
  },

  errorModel: {
    errorClass: 'AppError',
    fields: {
      code: 'string',
      message: 'string',
      details: 'object'
    }
  },

  logging: {
    fields: [
      'timestamp',
      'level',
      'service',
      'traceId',
      'userId',
      'message'
    ],
    format: 'json'
  },

  testing: {
    framework: 'vitest',
    minCoverage: 80,
    requiredPatterns: [
      'Test files MUST use .test.mjs extension',
      'Each service MUST have corresponding test file',
      'Tests MUST include unit and integration coverage',
      'Test descriptions MUST be clear and descriptive'
    ]
  },

  dataContracts: {
    dtoFormat: 'object',
    validation: 'zod',
    requiredFields: [
      'id',
      'createdAt',
      'updatedAt'
    ]
  }
};

/**
 * Alternative profile: Minimal conventions for rapid prototyping
 * @type {import('./profile-schema.mjs').ConventionsProfile}
 */
export const minimalProfile = {
  name: 'Minimal Conventions',
  description: 'Lightweight conventions for rapid prototyping and experimentation',

  fileLayout: {
    serviceModules: ['src/**/*.mjs'],
    testModules: ['test/**/*.test.mjs'],
    naming: {
      serviceClass: 'camelCase',
      method: 'camelCase',
      constant: 'UPPER_SNAKE_CASE'
    }
  },

  errorModel: {
    errorClass: 'Error',
    fields: {
      code: 'string',
      message: 'string',
      details: 'string'
    }
  },

  logging: {
    fields: ['timestamp', 'level', 'message'],
    format: 'plaintext'
  },

  testing: {
    framework: 'node:test',
    minCoverage: 60,
    requiredPatterns: [
      'Test files should use .test.mjs extension'
    ]
  },

  dataContracts: {
    dtoFormat: 'object',
    validation: 'none',
    requiredFields: []
  }
};

/**
 * Strict profile: Maximum enforcement for production systems
 * @type {import('./profile-schema.mjs').ConventionsProfile}
 */
export const strictProfile = {
  name: 'Strict Production Standards',
  description: 'Maximum enforcement for mission-critical production systems',

  fileLayout: {
    serviceModules: [
      'src/services/*/index.mjs',
      'src/domain/*/service.mjs'
    ],
    testModules: [
      'test/unit/**/*.test.mjs',
      'test/integration/**/*.test.mjs',
      'test/e2e/**/*.test.mjs'
    ],
    naming: {
      serviceClass: 'PascalCase',
      method: 'camelCase',
      constant: 'SCREAMING_SNAKE_CASE'
    }
  },

  errorModel: {
    errorClass: 'DomainError',
    fields: {
      code: 'string',
      message: 'string',
      details: 'object'
    }
  },

  logging: {
    fields: [
      'timestamp',
      'level',
      'service',
      'traceId',
      'spanId',
      'userId',
      'sessionId',
      'correlationId',
      'message',
      'metadata'
    ],
    format: 'json'
  },

  testing: {
    framework: 'vitest',
    minCoverage: 95,
    requiredPatterns: [
      'Test files MUST use .test.mjs extension',
      'Each service MUST have unit, integration, and e2e tests',
      'Tests MUST include happy path and error scenarios',
      'Tests MUST be deterministic and isolated',
      'Tests MUST use meaningful assertions',
      'Tests MUST clean up resources'
    ]
  },

  dataContracts: {
    dtoFormat: 'class',
    validation: 'zod',
    requiredFields: [
      'id',
      'version',
      'createdAt',
      'updatedAt',
      'createdBy',
      'updatedBy'
    ]
  }
};
