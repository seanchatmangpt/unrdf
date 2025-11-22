/**
 * @file Type-safety auditor - validates Zod schemas match TypeScript types
 * @module project-engine/type-auditor
 */

import { promises as fs } from 'fs';
import path from 'path';
import { z } from 'zod';

/* ========================================================================= */
/* Zod Schemas                                                               */
/* ========================================================================= */

const FieldInfoSchema = z.object({
  name: z.string(),
  type: z.string(),
  optional: z.boolean(),
  array: z.boolean().default(false),
});

const EntityFieldsSchema = z.object({
  file: z.string(),
  fields: z.record(FieldInfoSchema),
});

const MismatchSchema = z.object({
  entity: z.string(),
  zod: EntityFieldsSchema.optional(),
  typescript: EntityFieldsSchema.optional(),
  issues: z.array(z.string()),
  severity: z.enum(['low', 'medium', 'high', 'critical']),
});

const AuditResultSchema = z.object({
  mismatches: z.array(MismatchSchema),
  summary: z.string(),
  recommendation: z.string(),
  score: z.number().min(0).max(100),
});

const AuditOptionsSchema = z.object({
  domainStore: z.any().optional(),
  fsStore: z.any().optional(),
  stackProfile: z
    .object({
      hasZod: z.boolean().default(false),
      hasTypescript: z.boolean().default(false),
    })
    .passthrough()
    .optional(),
  projectRoot: z.string().optional(),
  schemaDir: z.string().default('src/schemas'),
  typesDir: z.string().default('src/types'),
});

const CompareTypesResultSchema = z.object({
  added: z.array(z.string()),
  removed: z.array(z.string()),
  modified: z.array(
    z.object({
      field: z.string(),
      zodInfo: FieldInfoSchema.optional(),
      tsInfo: FieldInfoSchema.optional(),
      issue: z.string(),
    })
  ),
  consistent: z.array(z.string()),
});

/**
 * @typedef {z.infer<typeof FieldInfoSchema>} FieldInfo
 * @typedef {z.infer<typeof EntityFieldsSchema>} EntityFields
 * @typedef {z.infer<typeof MismatchSchema>} Mismatch
 * @typedef {z.infer<typeof AuditResultSchema>} AuditResult
 * @typedef {z.infer<typeof CompareTypesResultSchema>} CompareTypesResult
 */

/* ========================================================================= */
/* File reading utilities                                                    */
/* ========================================================================= */

/**
 * Read file content from filesystem
 * @param {string} filePath
 * @returns {Promise<string|null>}
 */
async function readFileContent(filePath) {
  try {
    return await fs.readFile(filePath, 'utf-8');
  } catch {
    return null;
  }
}

/**
 * Find files in directory matching pattern
 * @param {string} dirPath
 * @param {string[]} extensions
 * @returns {Promise<string[]>}
 */
async function findFilesInDir(dirPath, extensions = ['.ts', '.tsx', '.mjs', '.js']) {
  try {
    const entries = await fs.readdir(dirPath, { withFileTypes: true });
    const files = [];

    for (const entry of entries) {
      if (entry.isFile()) {
        const ext = path.extname(entry.name);
        if (extensions.includes(ext)) {
          files.push(path.join(dirPath, entry.name));
        }
      } else if (entry.isDirectory() && !entry.name.startsWith('.')) {
        const subFiles = await findFilesInDir(path.join(dirPath, entry.name), extensions);
        files.push(...subFiles);
      }
    }

    return files;
  } catch {
    return [];
  }
}

/* ========================================================================= */
/* Zod schema parsing                                                        */
/* ========================================================================= */

/**
 * Parse Zod schema fields from file content
 * @param {string} content
 * @returns {Map<string, Record<string, FieldInfo>>}
 */
function parseZodSchemas(content) {
  const schemas = new Map();

  // Match: export const XxxSchema = z.object({ ... })
  const schemaPattern =
    /(?:export\s+)?(?:const|let)\s+(\w+)Schema\s*=\s*z\.object\(\s*\{([^}]+(?:\{[^}]*\}[^}]*)*)\}\s*\)/g;

  let match;
  while ((match = schemaPattern.exec(content)) !== null) {
    const entityName = match[1];
    const fieldsBlock = match[2];
    const fields = parseZodFieldsDetailed(fieldsBlock);

    if (Object.keys(fields).length > 0) {
      schemas.set(entityName, fields);
    }
  }

  return schemas;
}

/**
 * Parse Zod fields with detailed type info
 * @param {string} fieldsBlock
 * @returns {Record<string, FieldInfo>}
 */
function parseZodFieldsDetailed(fieldsBlock) {
  /** @type {Record<string, FieldInfo>} */
  const fields = {};

  // Match field definitions more carefully
  const lines = fieldsBlock
    .split(/[,\n]/)
    .map(l => l.trim())
    .filter(Boolean);

  for (const line of lines) {
    // Match: fieldName: z.type()
    const fieldMatch = line.match(/^(\w+)\s*:\s*z\.(\w+)/);
    if (!fieldMatch) continue;

    const fieldName = fieldMatch[1];
    const zodType = fieldMatch[2];

    // Check for .optional() or .nullable()
    const isOptional = line.includes('.optional()') || line.includes('.nullable()');
    const isArray = zodType === 'array' || line.includes('.array()');

    // Map zod types
    let type = 'string';
    if (zodType === 'number' || zodType === 'bigint') type = 'number';
    else if (zodType === 'boolean') type = 'boolean';
    else if (zodType === 'date') type = 'date';
    else if (zodType === 'array') type = 'array';
    else if (zodType === 'enum') type = 'enum';
    else if (zodType === 'object') type = 'object';

    fields[fieldName] = {
      name: fieldName,
      type,
      optional: isOptional,
      array: isArray,
    };
  }

  return fields;
}

/* ========================================================================= */
/* TypeScript type parsing                                                   */
/* ========================================================================= */

/**
 * Parse TypeScript interfaces and types from file content
 * @param {string} content
 * @returns {Map<string, Record<string, FieldInfo>>}
 */
function parseTypeScriptTypes(content) {
  const types = new Map();

  // Match interface declarations
  const interfacePattern =
    /(?:export\s+)?interface\s+(\w+)(?:\s+extends\s+[\w,\s]+)?\s*\{([^}]+(?:\{[^}]*\}[^}]*)*)\}/g;

  let match;
  while ((match = interfacePattern.exec(content)) !== null) {
    const typeName = match[1];
    const fieldsBlock = match[2];

    // Skip utility types
    if (typeName.endsWith('Props') || typeName.endsWith('Options') || typeName.endsWith('Config')) {
      continue;
    }

    const fields = parseTypeScriptFieldsDetailed(fieldsBlock);
    if (Object.keys(fields).length > 0) {
      types.set(typeName, fields);
    }
  }

  // Match type declarations
  const typePattern = /(?:export\s+)?type\s+(\w+)\s*=\s*\{([^}]+(?:\{[^}]*\}[^}]*)*)\}/g;

  while ((match = typePattern.exec(content)) !== null) {
    const typeName = match[1];
    const fieldsBlock = match[2];

    // Skip utility types
    if (typeName.endsWith('Props') || typeName.endsWith('Options') || typeName.endsWith('Config')) {
      continue;
    }

    const fields = parseTypeScriptFieldsDetailed(fieldsBlock);
    if (Object.keys(fields).length > 0) {
      types.set(typeName, fields);
    }
  }

  return types;
}

/**
 * Parse TypeScript fields with detailed info
 * @param {string} fieldsBlock
 * @returns {Record<string, FieldInfo>}
 */
function parseTypeScriptFieldsDetailed(fieldsBlock) {
  /** @type {Record<string, FieldInfo>} */
  const fields = {};

  // Match: fieldName: Type, fieldName?: Type
  const fieldPattern = /(?:readonly\s+)?(\w+)(\?)?:\s*([^;,\n]+)/g;

  let match;
  while ((match = fieldPattern.exec(fieldsBlock)) !== null) {
    const fieldName = match[1];
    const isOptional = !!match[2];
    let rawType = match[3].trim();

    // Check for array types
    const isArray = rawType.endsWith('[]') || rawType.startsWith('Array<');

    // Clean up the type
    let baseType = rawType
      .replace(/\[\]$/, '')
      .replace(/^Array<(.+)>$/, '$1')
      .replace(/\s*\|\s*null$/, '')
      .replace(/\s*\|\s*undefined$/, '')
      .trim();

    // Normalize type names
    let type = 'string';
    const normalizedType = baseType.toLowerCase();
    if (normalizedType === 'number' || normalizedType === 'bigint') type = 'number';
    else if (normalizedType === 'boolean') type = 'boolean';
    else if (normalizedType === 'date') type = 'date';
    else if (normalizedType === 'object' || normalizedType.startsWith('{')) type = 'object';

    fields[fieldName] = {
      name: fieldName,
      type,
      optional: isOptional,
      array: isArray,
    };
  }

  return fields;
}

/* ========================================================================= */
/* Type comparison                                                           */
/* ========================================================================= */

/**
 * Compare Zod fields with TypeScript fields
 *
 * @param {Record<string, FieldInfo>} zodFields
 * @param {Record<string, FieldInfo>} tsFields
 * @returns {CompareTypesResult}
 */
export function compareTypes(zodFields, tsFields) {
  const zodFieldNames = new Set(Object.keys(zodFields));
  const tsFieldNames = new Set(Object.keys(tsFields));

  /** @type {string[]} */
  const added = []; // In Zod but not in TS
  /** @type {string[]} */
  const removed = []; // In TS but not in Zod
  /** @type {CompareTypesResult['modified']} */
  const modified = [];
  /** @type {string[]} */
  const consistent = [];

  // Fields in Zod but not in TS
  for (const fieldName of zodFieldNames) {
    if (!tsFieldNames.has(fieldName)) {
      added.push(fieldName);
    }
  }

  // Fields in TS but not in Zod
  for (const fieldName of tsFieldNames) {
    if (!zodFieldNames.has(fieldName)) {
      removed.push(fieldName);
    }
  }

  // Compare common fields
  for (const fieldName of zodFieldNames) {
    if (!tsFieldNames.has(fieldName)) continue;

    const zodField = zodFields[fieldName];
    const tsField = tsFields[fieldName];

    const issues = [];

    // Check optionality mismatch
    if (zodField.optional && !tsField.optional) {
      issues.push(`${fieldName}: optional in Zod, required in TS`);
    } else if (!zodField.optional && tsField.optional) {
      issues.push(`${fieldName}: required in Zod, optional in TS`);
    }

    // Check array mismatch
    if (zodField.array !== tsField.array) {
      issues.push(`${fieldName}: array=${zodField.array} in Zod, array=${tsField.array} in TS`);
    }

    // Check type mismatch (simple check)
    if (zodField.type !== tsField.type) {
      issues.push(`${fieldName}: type=${zodField.type} in Zod, type=${tsField.type} in TS`);
    }

    if (issues.length > 0) {
      modified.push({
        field: fieldName,
        zodInfo: zodField,
        tsInfo: tsField,
        issue: issues.join('; '),
      });
    } else {
      consistent.push(fieldName);
    }
  }

  return CompareTypesResultSchema.parse({
    added,
    removed,
    modified,
    consistent,
  });
}

/* ========================================================================= */
/* Main audit API                                                            */
/* ========================================================================= */

/**
 * Audit type consistency between Zod schemas and TypeScript types
 *
 * @param {Object} options
 * @param {any} [options.domainStore] - Domain model RDF store (optional)
 * @param {any} [options.fsStore] - Filesystem RDF store (optional)
 * @param {Object} [options.stackProfile] - Stack detection profile
 * @param {string} [options.projectRoot] - Project root directory
 * @param {string} [options.schemaDir] - Directory containing Zod schemas
 * @param {string} [options.typesDir] - Directory containing TypeScript types
 * @returns {Promise<AuditResult>}
 */
export async function auditTypeConsistency(options) {
  const validated = AuditOptionsSchema.parse(options);
  const { projectRoot, schemaDir, typesDir } = validated;

  /** @type {Mismatch[]} */
  const mismatches = [];

  if (!projectRoot) {
    return AuditResultSchema.parse({
      mismatches: [],
      summary: 'No project root specified',
      recommendation: 'Provide projectRoot to enable type auditing',
      score: 100,
    });
  }

  const schemaPath = path.join(projectRoot, schemaDir);
  const typesPath = path.join(projectRoot, typesDir);

  // Find all schema files
  const schemaFiles = await findFilesInDir(schemaPath, ['.ts', '.tsx', '.mjs', '.js']);
  const typeFiles = await findFilesInDir(typesPath, ['.ts', '.tsx', '.d.ts']);

  // Parse all Zod schemas
  /** @type {Map<string, {file: string, fields: Record<string, FieldInfo>}>} */
  const zodSchemas = new Map();

  for (const file of schemaFiles) {
    const content = await readFileContent(file);
    if (!content) continue;

    // Skip if not a Zod file
    if (!content.includes("from 'zod'") && !content.includes('from "zod"')) continue;

    const schemas = parseZodSchemas(content);
    for (const [name, fields] of schemas.entries()) {
      zodSchemas.set(name, { file: path.relative(projectRoot, file), fields });
    }
  }

  // Parse all TypeScript types
  /** @type {Map<string, {file: string, fields: Record<string, FieldInfo>}>} */
  const tsTypes = new Map();

  for (const file of typeFiles) {
    const content = await readFileContent(file);
    if (!content) continue;

    const types = parseTypeScriptTypes(content);
    for (const [name, fields] of types.entries()) {
      tsTypes.set(name, { file: path.relative(projectRoot, file), fields });
    }
  }

  // Compare matching entities
  const allEntities = new Set([...zodSchemas.keys(), ...tsTypes.keys()]);

  for (const entityName of allEntities) {
    const zodInfo = zodSchemas.get(entityName);
    const tsInfo = tsTypes.get(entityName);

    /** @type {string[]} */
    const issues = [];
    let severity = /** @type {'low' | 'medium' | 'high' | 'critical'} */ ('low');

    if (zodInfo && !tsInfo) {
      issues.push(`Entity "${entityName}" exists in Zod but not in TypeScript`);
      severity = 'medium';
    } else if (!zodInfo && tsInfo) {
      issues.push(`Entity "${entityName}" exists in TypeScript but not in Zod`);
      severity = 'medium';
    } else if (zodInfo && tsInfo) {
      const comparison = compareTypes(zodInfo.fields, tsInfo.fields);

      // Report added fields (in Zod but not TS)
      for (const fieldName of comparison.added) {
        issues.push(`Field "${fieldName}" exists in Zod but not in TypeScript`);
        severity = severity === 'low' ? 'medium' : severity;
      }

      // Report removed fields (in TS but not Zod)
      for (const fieldName of comparison.removed) {
        issues.push(`Field "${fieldName}" removed from TS but exists in Zod`);
        severity = 'high'; // More critical - TS code expects field that Zod won't validate
      }

      // Report modified fields
      for (const mod of comparison.modified) {
        issues.push(mod.issue);
        // Optionality mismatch is high severity
        if (mod.issue.includes('optional') && mod.issue.includes('required')) {
          severity = 'high';
        }
      }
    }

    if (issues.length > 0) {
      mismatches.push({
        entity: entityName,
        zod: zodInfo ? { file: zodInfo.file, fields: zodInfo.fields } : undefined,
        typescript: tsInfo ? { file: tsInfo.file, fields: tsInfo.fields } : undefined,
        issues,
        severity,
      });
    }
  }

  // Calculate score
  const totalEntities = allEntities.size;
  const entitiesWithIssues = mismatches.length;
  const score =
    totalEntities > 0
      ? Math.round(((totalEntities - entitiesWithIssues) / totalEntities) * 100)
      : 100;

  // Generate summary and recommendation
  const summary =
    entitiesWithIssues === 0
      ? 'All Zod schemas and TypeScript types are consistent'
      : `${entitiesWithIssues} entities with type mismatches out of ${totalEntities} total`;

  const criticalCount = mismatches.filter(m => m.severity === 'critical').length;
  const highCount = mismatches.filter(m => m.severity === 'high').length;

  let recommendation = 'No action needed';
  if (criticalCount > 0) {
    recommendation = 'Critical type mismatches detected - fix immediately before production';
  } else if (highCount > 0) {
    recommendation = 'High severity mismatches - update TypeScript types to match Zod schemas';
  } else if (entitiesWithIssues > 0) {
    recommendation = 'Minor type mismatches - consider synchronizing schemas and types';
  }

  return AuditResultSchema.parse({
    mismatches,
    summary,
    recommendation,
    score,
  });
}

/**
 * Audit a single entity's type consistency
 *
 * @param {string} zodContent - Content of file containing Zod schema
 * @param {string} tsContent - Content of file containing TypeScript type
 * @param {string} entityName - Name of entity to audit
 * @returns {Mismatch | null}
 */
export function auditEntityTypes(zodContent, tsContent, entityName) {
  const zodSchemas = parseZodSchemas(zodContent);
  const tsTypes = parseTypeScriptTypes(tsContent);

  const zodFields = zodSchemas.get(entityName);
  const tsFields = tsTypes.get(entityName);

  if (!zodFields && !tsFields) {
    return null;
  }

  /** @type {string[]} */
  const issues = [];
  let severity = /** @type {'low' | 'medium' | 'high' | 'critical'} */ ('low');

  if (zodFields && !tsFields) {
    issues.push(`Entity "${entityName}" exists in Zod but not in TypeScript`);
    severity = 'medium';
  } else if (!zodFields && tsFields) {
    issues.push(`Entity "${entityName}" exists in TypeScript but not in Zod`);
    severity = 'medium';
  } else if (zodFields && tsFields) {
    const comparison = compareTypes(zodFields, tsFields);

    for (const fieldName of comparison.added) {
      issues.push(`Field "${fieldName}" exists in Zod but not in TypeScript`);
    }

    for (const fieldName of comparison.removed) {
      issues.push(`Field "${fieldName}" removed from TS but exists in Zod`);
      severity = 'high';
    }

    for (const mod of comparison.modified) {
      issues.push(mod.issue);
      if (mod.issue.includes('optional') && mod.issue.includes('required')) {
        severity = 'high';
      }
    }
  }

  if (issues.length === 0) {
    return null;
  }

  return MismatchSchema.parse({
    entity: entityName,
    zod: zodFields ? { file: 'inline', fields: zodFields } : undefined,
    typescript: tsFields ? { file: 'inline', fields: tsFields } : undefined,
    issues,
    severity,
  });
}

/* ========================================================================= */
/* Exports for module                                                        */
/* ========================================================================= */

export { FieldInfoSchema, MismatchSchema, AuditResultSchema, CompareTypesResultSchema };
