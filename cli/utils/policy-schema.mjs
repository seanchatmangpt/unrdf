/**
 * @file Policy schema validation
 * @module cli/utils/policy-schema
 *
 * Validates policy JSON schema structure and format
 * FM-CLI-012: Invalid policy definition
 */

import { z } from 'zod';

/**
 * Policy schema validation
 */
export const policySchema = z.object({
  name: z.string().min(1).max(100),
  description: z.string().optional().default(''),
  version: z.string().optional().default('1.0.0'),
  enabled: z.boolean().optional().default(true),
  hooks: z.array(z.object({
    type: z.enum(['sparql-ask', 'sparql-select', 'shacl', 'custom']),
    name: z.string().min(1),
    condition: z.string().optional(),
    action: z.string().optional()
  })).optional().default([]),
  rules: z.array(z.object({
    id: z.string().min(1),
    pattern: z.string().min(1),
    action: z.enum(['allow', 'deny', 'log', 'transform']),
    metadata: z.record(z.string(), z.any()).optional()
  })).optional().default([]),
  metadata: z.record(z.string(), z.any()).optional().default({})
});

/**
 * Validate policy object
 */
export function validatePolicy(policyData) {
  try {
    const validated = policySchema.parse(policyData);
    return {
      valid: true,
      data: validated,
      warnings: []
    };
  } catch (error) {
    const issues = error.errors.map(e => ({
      path: e.path.join('.'),
      message: e.message,
      code: e.code
    }));

    return {
      valid: false,
      error: 'Policy schema validation failed',
      issues,
      suggestion: `Fix the following issues:\n${issues.map(i => `  • ${i.path}: ${i.message}`).join('\n')}`
    };
  }
}

/**
 * Validate policy JSON string
 */
export function validatePolicyJson(jsonString) {
  try {
    const data = JSON.parse(jsonString);
    return validatePolicy(data);
  } catch (parseError) {
    return {
      valid: false,
      error: 'Invalid JSON syntax',
      issues: [{
        path: 'root',
        message: parseError.message,
        code: 'invalid_json'
      }],
      suggestion: `Check JSON syntax: ${parseError.message}`
    };
  }
}

/**
 * Validate policy file
 */
export async function validatePolicyFile(filePath) {
  try {
    const fs = await import('node:fs/promises');
    const content = await fs.readFile(filePath, 'utf-8');
    const result = validatePolicyJson(content);

    return {
      ...result,
      file: filePath
    };
  } catch (error) {
    return {
      valid: false,
      error: `Failed to read policy file: ${error.message}`,
      file: filePath,
      suggestion: 'Check file path and permissions'
    };
  }
}

/**
 * Get policy schema description
 */
export function getPolicySchemaDescription() {
  return `
Policy Schema Structure:

{
  "name": "policy-name",           // Required: 1-100 characters
  "description": "...",            // Optional
  "version": "1.0.0",              // Optional: defaults to 1.0.0
  "enabled": true,                 // Optional: defaults to true

  "hooks": [                        // Optional: array of hooks
    {
      "type": "sparql-ask|sparql-select|shacl|custom",
      "name": "hook-name",
      "condition": "SPARQL query",  // Optional
      "action": "action-name"       // Optional
    }
  ],

  "rules": [                        // Optional: array of rules
    {
      "id": "rule-id",
      "pattern": "pattern-string",
      "action": "allow|deny|log|transform",
      "metadata": { ... }           // Optional
    }
  ],

  "metadata": { ... }               // Optional: custom metadata
}
`;
}

export default {
  policySchema,
  validatePolicy,
  validatePolicyJson,
  validatePolicyFile,
  getPolicySchemaDescription
};
