/**
 * @file Conventions Profile Schema Definition
 * @description Zod schemas for organizational engineering conventions
 */

import { z } from 'zod';

/**
 * Naming convention types
 */
export const NamingConventionSchema = z.enum([
  'PascalCase',
  'camelCase',
  'snake_case',
  'UPPER_SNAKE_CASE',
  'kebab-case',
  'SCREAMING_SNAKE_CASE'
]);

/**
 * File layout configuration
 */
export const FileLayoutSchema = z.object({
  serviceModules: z.array(z.string()).describe('Glob patterns for service modules'),
  testModules: z.array(z.string()).describe('Glob patterns for test modules'),
  naming: z.object({
    serviceClass: NamingConventionSchema.describe('Service class naming convention'),
    method: NamingConventionSchema.describe('Method naming convention'),
    constant: NamingConventionSchema.describe('Constant naming convention')
  })
});

/**
 * Error model configuration
 */
export const ErrorModelSchema = z.object({
  errorClass: z.string().describe('Error class name'),
  fields: z.object({
    code: z.string().describe('Error code field type'),
    message: z.string().describe('Error message field type'),
    details: z.string().describe('Error details field type')
  })
});

/**
 * Logging configuration
 */
export const LoggingSchema = z.object({
  fields: z.array(z.string()).describe('Required log fields'),
  format: z.enum(['json', 'structured', 'plaintext']).describe('Log output format')
});

/**
 * Testing configuration
 */
export const TestingSchema = z.object({
  framework: z.enum(['vitest', 'jest', 'mocha', 'node:test']).describe('Test framework'),
  minCoverage: z.number().min(0).max(100).describe('Minimum test coverage percentage'),
  requiredPatterns: z.array(z.string()).describe('Required test patterns')
});

/**
 * Data contract configuration
 */
export const DataContractsSchema = z.object({
  dtoFormat: z.enum(['object', 'class']).describe('DTO format'),
  validation: z.enum(['zod', 'joi', 'manual', 'none']).describe('Validation library'),
  requiredFields: z.array(z.string()).describe('Required fields in all DTOs')
});

/**
 * Complete Conventions Profile Schema
 */
export const ConventionsProfileSchema = z.object({
  name: z.string().min(1).describe('Profile name'),
  description: z.string().describe('Profile description'),
  fileLayout: FileLayoutSchema,
  errorModel: ErrorModelSchema,
  logging: LoggingSchema,
  testing: TestingSchema,
  dataContracts: DataContractsSchema
});

/**
 * Violation record schema
 */
export const ViolationSchema = z.object({
  file: z.string().describe('File path with violation'),
  rule: z.string().describe('Rule that was violated'),
  message: z.string().describe('Violation description'),
  suggestion: z.string().optional().describe('Suggested fix'),
  line: z.number().optional().describe('Line number'),
  column: z.number().optional().describe('Column number')
});

/**
 * Compiled profile schema
 */
export const CompiledProfileSchema = z.object({
  schema: ConventionsProfileSchema,
  violations: z.array(ViolationSchema),
  compiled: z.boolean(),
  timestamp: z.string().datetime()
});

/**
 * Validation result schema
 */
export const ValidationResultSchema = z.object({
  valid: z.boolean(),
  violations: z.array(ViolationSchema),
  filesChecked: z.number(),
  timestamp: z.string().datetime()
});

/**
 * Type exports (inferred from Zod schemas)
 */

/** @typedef {z.infer<typeof NamingConventionSchema>} NamingConvention */
/** @typedef {z.infer<typeof FileLayoutSchema>} FileLayout */
/** @typedef {z.infer<typeof ErrorModelSchema>} ErrorModel */
/** @typedef {z.infer<typeof LoggingSchema>} Logging */
/** @typedef {z.infer<typeof TestingSchema>} Testing */
/** @typedef {z.infer<typeof DataContractsSchema>} DataContracts */
/** @typedef {z.infer<typeof ConventionsProfileSchema>} ConventionsProfile */
/** @typedef {z.infer<typeof ViolationSchema>} Violation */
/** @typedef {z.infer<typeof CompiledProfileSchema>} CompiledProfile */
/** @typedef {z.infer<typeof ValidationResultSchema>} ValidationResult */
