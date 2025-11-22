/**
 * @file API Contract Validator - validates API files against domain schemas
 * @module project-engine/api-contract-validator
 */

import { z } from 'zod'
import { DataFactory } from 'n3'

const { namedNode } = DataFactory

/* ========================================================================= */
/* Namespace prefixes                                                        */
/* ========================================================================= */

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  dom: 'http://example.org/unrdf/domain#',
}

/* ========================================================================= */
/* Zod Schemas                                                               */
/* ========================================================================= */

const FieldSchemaSchema = z.object({
  name: z.string(),
  type: z.string(),
  optional: z.boolean().default(false),
  array: z.boolean().default(false),
})

const EntitySchemaSchema = z.object({
  entityName: z.string(),
  fields: z.array(FieldSchemaSchema),
  zodSchema: z.string().optional(),
})

const ViolationSchema = z.object({
  type: z.enum(['missing_field', 'extra_field', 'type_mismatch', 'optionality_mismatch', 'missing_validation', 'missing_response_field']),
  field: z.string(),
  expected: z.string().optional(),
  actual: z.string().optional(),
  severity: z.enum(['low', 'medium', 'high', 'critical']),
  message: z.string(),
})

const ValidationResultSchema = z.object({
  violations: z.array(ViolationSchema),
  coverage: z.number().min(0).max(100),
  breaking: z.boolean(),
  summary: z.string(),
})

const BreakingChangeSchema = z.object({
  type: z.enum(['field_removed', 'field_required', 'type_changed', 'entity_removed']),
  entity: z.string(),
  field: z.string().optional(),
  oldValue: z.string().optional(),
  newValue: z.string().optional(),
  message: z.string(),
})

const ContractBreaksSchema = z.object({
  breakingChanges: z.array(BreakingChangeSchema),
  affectedAPIs: z.array(z.string()),
  safe: z.boolean(),
})

/**
 * @typedef {z.infer<typeof FieldSchemaSchema>} FieldSchema
 * @typedef {z.infer<typeof EntitySchemaSchema>} EntitySchema
 * @typedef {z.infer<typeof ViolationSchema>} Violation
 * @typedef {z.infer<typeof ValidationResultSchema>} ValidationResult
 * @typedef {z.infer<typeof BreakingChangeSchema>} BreakingChange
 * @typedef {z.infer<typeof ContractBreaksSchema>} ContractBreaks
 */

/* ========================================================================= */
/* Type mapping from XSD to Zod                                              */
/* ========================================================================= */

/**
 * Map XSD type to Zod type name
 * @param {string} xsdType
 * @returns {string}
 */
function xsdToZodType(xsdType) {
  const typeMap = {
    [`${NS.xsd}string`]: 'z.string()',
    [`${NS.xsd}integer`]: 'z.number()',
    [`${NS.xsd}decimal`]: 'z.number()',
    [`${NS.xsd}float`]: 'z.number()',
    [`${NS.xsd}double`]: 'z.number()',
    [`${NS.xsd}boolean`]: 'z.boolean()',
    [`${NS.xsd}date`]: 'z.string().date()',
    [`${NS.xsd}dateTime`]: 'z.string().datetime()',
    [`${NS.xsd}anyURI`]: 'z.string().url()',
    string: 'z.string()',
    number: 'z.number()',
    boolean: 'z.boolean()',
    date: 'z.string().date()',
  }

  return typeMap[xsdType] || 'z.string()'
}

/**
 * Map type string to simple type name for comparison
 * @param {string} xsdType
 * @returns {string}
 */
function normalizeTypeName(xsdType) {
  if (xsdType.includes('integer') || xsdType.includes('decimal') || xsdType.includes('float') || xsdType.includes('double')) {
    return 'number'
  }
  if (xsdType.includes('boolean')) {
    return 'boolean'
  }
  if (xsdType.includes('date')) {
    return 'date'
  }
  return 'string'
}

/* ========================================================================= */
/* Domain store extraction                                                   */
/* ========================================================================= */

/**
 * Extract entity fields from domain model store
 * @param {import('n3').Store} domainStore
 * @param {string} entityName
 * @param {string} [baseIri]
 * @returns {FieldSchema[]}
 */
function extractEntityFields(domainStore, entityName, baseIri = 'http://example.org/unrdf/domain#') {
  /** @type {FieldSchema[]} */
  const fields = []

  const entityIri = `${baseIri}${entityName}`

  // Get all fields for this entity
  const fieldQuads = domainStore.getQuads(
    namedNode(entityIri),
    namedNode(`${NS.dom}hasField`),
    null
  )

  for (const fieldQuad of fieldQuads) {
    const fieldIri = fieldQuad.object.value

    // Extract field name
    const nameQuads = domainStore.getQuads(
      namedNode(fieldIri),
      namedNode(`${NS.dom}fieldName`),
      null
    )
    const fieldName = nameQuads[0]?.object.value || fieldIri.split('.').pop() || ''

    // Extract field type
    const typeQuads = domainStore.getQuads(
      namedNode(fieldIri),
      namedNode(`${NS.dom}fieldType`),
      null
    )
    const fieldType = normalizeTypeName(typeQuads[0]?.object.value || 'string')

    // Extract optional flag
    const optionalQuads = domainStore.getQuads(
      namedNode(fieldIri),
      namedNode(`${NS.dom}isOptional`),
      null
    )
    const isOptional = optionalQuads[0]?.object.value === 'true'

    // Extract array flag
    const arrayQuads = domainStore.getQuads(
      namedNode(fieldIri),
      namedNode(`${NS.dom}isArray`),
      null
    )
    const isArray = arrayQuads[0]?.object.value === 'true'

    fields.push({
      name: fieldName,
      type: fieldType,
      optional: isOptional,
      array: isArray,
    })
  }

  return fields
}

/**
 * Get all entity names from domain store
 * @param {import('n3').Store} domainStore
 * @param {string} [baseIri]
 * @returns {string[]}
 */
function getEntityNames(domainStore, baseIri = 'http://example.org/unrdf/domain#') {
  const entityQuads = domainStore.getQuads(
    null,
    namedNode(`${NS.rdf}type`),
    namedNode(`${NS.dom}Entity`)
  )

  return entityQuads.map(quad => {
    const iri = quad.subject.value
    return iri.replace(baseIri, '')
  })
}

/* ========================================================================= */
/* API file parsing                                                          */
/* ========================================================================= */

/**
 * Extract request/response fields from Next.js API route
 * @param {string} content
 * @returns {{request: string[], response: string[], validations: string[]}}
 */
function parseNextJsRoute(content) {
  const request = []
  const response = []
  const validations = []

  // Match destructuring from request body: const { field1, field2 } = await req.json()
  const bodyPattern = /(?:const|let)\s*\{\s*([^}]+)\s*\}\s*=\s*(?:await\s+)?(?:req\.json|request\.json|body)/g
  let match
  while ((match = bodyPattern.exec(content)) !== null) {
    const fields = match[1].split(',').map(f => f.trim().split(':')[0].trim())
    request.push(...fields.filter(f => f && !f.startsWith('...')))
  }

  // Match direct body property access: body.fieldName, req.body.fieldName
  const directBodyPattern = /(?:body|req\.body)\.(\w+)/g
  while ((match = directBodyPattern.exec(content)) !== null) {
    if (!request.includes(match[1])) {
      request.push(match[1])
    }
  }

  // Match NextResponse.json({ ... }) or res.json({ ... })
  const responsePattern = /(?:NextResponse\.json|Response\.json|res\.(?:json|send))\s*\(\s*\{([^}]+(?:\{[^}]*\}[^}]*)*)\}/g
  while ((match = responsePattern.exec(content)) !== null) {
    const responseBlock = match[1]
    // Extract field names from response object
    const fieldPattern = /(\w+)\s*:/g
    let fieldMatch
    while ((fieldMatch = fieldPattern.exec(responseBlock)) !== null) {
      if (!response.includes(fieldMatch[1])) {
        response.push(fieldMatch[1])
      }
    }
  }

  // Match Zod schema validation: schema.parse(), schema.safeParse()
  const zodPattern = /(\w+)Schema\.(?:parse|safeParse|parseAsync)/g
  while ((match = zodPattern.exec(content)) !== null) {
    validations.push(match[1])
  }

  // Match z.object validation inline
  const zodInlinePattern = /z\.object\(\s*\{([^}]+)\}\s*\)\.(?:parse|safeParse)/g
  while ((match = zodInlinePattern.exec(content)) !== null) {
    validations.push('inline')
  }

  return { request, response, validations }
}

/**
 * Extract request/response fields from Express route handler
 * @param {string} content
 * @returns {{request: string[], response: string[], validations: string[]}}
 */
function parseExpressRoute(content) {
  const request = []
  const response = []
  const validations = []

  // Match destructuring from req.body: const { field1, field2 } = req.body
  const bodyPattern = /(?:const|let)\s*\{\s*([^}]+)\s*\}\s*=\s*req\.body/g
  let match
  while ((match = bodyPattern.exec(content)) !== null) {
    const fields = match[1].split(',').map(f => f.trim().split(':')[0].trim())
    request.push(...fields.filter(f => f && !f.startsWith('...')))
  }

  // Match direct req.body property access
  const directBodyPattern = /req\.body\.(\w+)/g
  while ((match = directBodyPattern.exec(content)) !== null) {
    if (!request.includes(match[1])) {
      request.push(match[1])
    }
  }

  // Match res.json({ ... }) or res.send({ ... })
  const responsePattern = /res\.(?:json|send)\s*\(\s*\{([^}]+(?:\{[^}]*\}[^}]*)*)\}/g
  while ((match = responsePattern.exec(content)) !== null) {
    const responseBlock = match[1]
    const fieldPattern = /(\w+)\s*:/g
    let fieldMatch
    while ((fieldMatch = fieldPattern.exec(responseBlock)) !== null) {
      if (!response.includes(fieldMatch[1])) {
        response.push(fieldMatch[1])
      }
    }
  }

  // Match Zod/Joi validation
  const zodPattern = /(\w+)Schema\.(?:parse|safeParse|validate)/g
  while ((match = zodPattern.exec(content)) !== null) {
    validations.push(match[1])
  }

  // Match express-validator
  const validatorPattern = /body\(['"](\w+)['"]\)/g
  while ((match = validatorPattern.exec(content)) !== null) {
    validations.push(match[1])
  }

  return { request, response, validations }
}

/**
 * Parse API file content and extract request/response fields
 * @param {string} content
 * @param {string} [framework]
 * @returns {{request: string[], response: string[], validations: string[]}}
 */
function parseAPIFile(content, framework) {
  // Auto-detect framework if not specified
  if (!framework) {
    if (content.includes('NextResponse') || content.includes('NextRequest') || content.includes('export async function')) {
      framework = 'nextjs'
    } else if (content.includes('express') || content.includes('req, res') || content.includes('Router')) {
      framework = 'express'
    } else {
      framework = 'nextjs' // Default
    }
  }

  if (framework === 'nextjs') {
    return parseNextJsRoute(content)
  } else {
    return parseExpressRoute(content)
  }
}

/* ========================================================================= */
/* Public API: generateAPISchema                                             */
/* ========================================================================= */

/**
 * Generate API schema from domain store for an entity
 *
 * @param {import('n3').Store} domainStore - Domain model RDF store
 * @param {string} entity - Entity name to generate schema for
 * @param {Object} [options]
 * @param {string} [options.baseIri] - Base IRI for domain resources
 * @returns {EntitySchema}
 */
export function generateAPISchema(domainStore, entity, options = {}) {
  const { baseIri = 'http://example.org/unrdf/domain#' } = options

  const fields = extractEntityFields(domainStore, entity, baseIri)

  // Generate Zod schema string
  const fieldDefs = fields.map(f => {
    let zodType = xsdToZodType(f.type)
    if (f.array) {
      zodType = `z.array(${zodType})`
    }
    if (f.optional) {
      zodType = `${zodType}.optional()`
    }
    return `  ${f.name}: ${zodType}`
  })

  const zodSchema = `const ${entity}Schema = z.object({\n${fieldDefs.join(',\n')}\n})`

  return EntitySchemaSchema.parse({
    entityName: entity,
    fields,
    zodSchema,
  })
}

/**
 * Generate API schemas for all entities in domain store
 *
 * @param {import('n3').Store} domainStore
 * @param {Object} [options]
 * @param {string} [options.baseIri]
 * @returns {EntitySchema[]}
 */
export function generateAllAPISchemas(domainStore, options = {}) {
  const { baseIri = 'http://example.org/unrdf/domain#' } = options

  const entityNames = getEntityNames(domainStore, baseIri)

  return entityNames.map(entity => generateAPISchema(domainStore, entity, options))
}

/* ========================================================================= */
/* Public API: validateAPIFiles                                              */
/* ========================================================================= */

/**
 * Validate API files against expected schema
 *
 * @param {Array<{path: string, content: string}>} apiFiles - API file contents
 * @param {EntitySchema} expectedSchema - Expected schema from domain model
 * @param {Object} [options]
 * @param {string} [options.framework] - 'nextjs' or 'express'
 * @returns {ValidationResult}
 */
export function validateAPIFiles(apiFiles, expectedSchema, options = {}) {
  const { framework } = options

  /** @type {Violation[]} */
  const violations = []
  const expectedFields = new Set(expectedSchema.fields.map(f => f.name))
  const foundRequestFields = new Set()
  const foundResponseFields = new Set()
  let hasValidation = false

  for (const file of apiFiles) {
    const parsed = parseAPIFile(file.content, framework)

    // Track found fields
    for (const field of parsed.request) {
      foundRequestFields.add(field)
    }
    for (const field of parsed.response) {
      foundResponseFields.add(field)
    }

    // Check for validation
    if (parsed.validations.length > 0) {
      hasValidation = true
    }

    // Check for extra fields not in schema
    for (const field of parsed.request) {
      if (!expectedFields.has(field)) {
        violations.push({
          type: 'extra_field',
          field,
          expected: 'not defined',
          actual: 'present in request',
          severity: 'medium',
          message: `Field "${field}" in API request is not defined in domain schema`,
        })
      }
    }
  }

  // Check for missing required fields
  for (const field of expectedSchema.fields) {
    if (!field.optional && !foundRequestFields.has(field.name)) {
      violations.push({
        type: 'missing_field',
        field: field.name,
        expected: 'required',
        actual: 'not found in API',
        severity: 'high',
        message: `Required field "${field.name}" not found in API request handling`,
      })
    }
  }

  // Check for missing response fields
  for (const field of expectedSchema.fields) {
    if (!foundResponseFields.has(field.name) && !field.optional) {
      violations.push({
        type: 'missing_response_field',
        field: field.name,
        expected: 'in response',
        actual: 'not found',
        severity: 'medium',
        message: `Field "${field.name}" not found in API response`,
      })
    }
  }

  // Check for missing validation
  if (!hasValidation && apiFiles.length > 0) {
    violations.push({
      type: 'missing_validation',
      field: 'request body',
      expected: 'schema validation',
      actual: 'no validation found',
      severity: 'high',
      message: 'No schema validation found in API handlers',
    })
  }

  // Calculate coverage
  const totalExpectedFields = expectedSchema.fields.length
  const coveredFields = expectedSchema.fields.filter(
    f => foundRequestFields.has(f.name) || foundResponseFields.has(f.name)
  ).length
  const coverage = totalExpectedFields > 0
    ? Math.round((coveredFields / totalExpectedFields) * 100)
    : 100

  // Determine if breaking
  const breaking = violations.some(v =>
    v.severity === 'critical' ||
    (v.severity === 'high' && v.type === 'missing_field')
  )

  // Generate summary
  const criticalCount = violations.filter(v => v.severity === 'critical').length
  const highCount = violations.filter(v => v.severity === 'high').length
  let summary = `${violations.length} violations found`
  if (criticalCount > 0) {
    summary += ` (${criticalCount} critical)`
  } else if (highCount > 0) {
    summary += ` (${highCount} high severity)`
  }
  if (violations.length === 0) {
    summary = 'API contract is valid'
  }

  return ValidationResultSchema.parse({
    violations,
    coverage,
    breaking,
    summary,
  })
}

/* ========================================================================= */
/* Public API: detectContractBreaks                                          */
/* ========================================================================= */

/**
 * Detect breaking changes between old and new schemas
 *
 * @param {EntitySchema} oldSchema - Previous version schema
 * @param {EntitySchema} newSchema - New version schema
 * @param {Array<{path: string, content: string}>} [implementations] - API implementation files
 * @returns {ContractBreaks}
 */
export function detectContractBreaks(oldSchema, newSchema, implementations = []) {
  /** @type {BreakingChange[]} */
  const breakingChanges = []
  /** @type {Set<string>} */
  const affectedAPIs = new Set()

  const oldFields = new Map(oldSchema.fields.map(f => [f.name, f]))
  const newFields = new Map(newSchema.fields.map(f => [f.name, f]))

  // Check for removed fields (breaking)
  for (const [fieldName, oldField] of oldFields) {
    if (!newFields.has(fieldName)) {
      breakingChanges.push({
        type: 'field_removed',
        entity: oldSchema.entityName,
        field: fieldName,
        oldValue: oldField.type,
        newValue: undefined,
        message: `Field "${fieldName}" was removed from ${oldSchema.entityName}`,
      })
    }
  }

  // Check for fields that became required (breaking)
  for (const [fieldName, newField] of newFields) {
    const oldField = oldFields.get(fieldName)
    if (oldField && oldField.optional && !newField.optional) {
      breakingChanges.push({
        type: 'field_required',
        entity: newSchema.entityName,
        field: fieldName,
        oldValue: 'optional',
        newValue: 'required',
        message: `Field "${fieldName}" changed from optional to required in ${newSchema.entityName}`,
      })
    }
  }

  // Check for type changes (breaking)
  for (const [fieldName, newField] of newFields) {
    const oldField = oldFields.get(fieldName)
    if (oldField && oldField.type !== newField.type) {
      breakingChanges.push({
        type: 'type_changed',
        entity: newSchema.entityName,
        field: fieldName,
        oldValue: oldField.type,
        newValue: newField.type,
        message: `Field "${fieldName}" type changed from ${oldField.type} to ${newField.type} in ${newSchema.entityName}`,
      })
    }
  }

  // Find affected API files
  for (const impl of implementations) {
    const parsed = parseAPIFile(impl.content)
    const allFields = [...parsed.request, ...parsed.response]

    for (const change of breakingChanges) {
      if (change.field && allFields.includes(change.field)) {
        affectedAPIs.add(impl.path)
      }
    }
  }

  return ContractBreaksSchema.parse({
    breakingChanges,
    affectedAPIs: Array.from(affectedAPIs),
    safe: breakingChanges.length === 0,
  })
}

/**
 * Compare two domain stores and detect API contract breaks
 *
 * @param {import('n3').Store} oldStore - Previous domain model
 * @param {import('n3').Store} newStore - New domain model
 * @param {Array<{path: string, content: string}>} [implementations] - API files
 * @param {Object} [options]
 * @param {string} [options.baseIri]
 * @returns {Map<string, ContractBreaks>}
 */
export function detectAllContractBreaks(oldStore, newStore, implementations = [], options = {}) {
  const { baseIri = 'http://example.org/unrdf/domain#' } = options

  const oldEntities = getEntityNames(oldStore, baseIri)
  const newEntities = getEntityNames(newStore, baseIri)

  const allEntities = new Set([...oldEntities, ...newEntities])
  /** @type {Map<string, ContractBreaks>} */
  const results = new Map()

  for (const entity of allEntities) {
    const oldSchema = oldEntities.includes(entity)
      ? generateAPISchema(oldStore, entity, options)
      : { entityName: entity, fields: [], zodSchema: '' }

    const newSchema = newEntities.includes(entity)
      ? generateAPISchema(newStore, entity, options)
      : { entityName: entity, fields: [], zodSchema: '' }

    // Check for entity removal
    if (oldEntities.includes(entity) && !newEntities.includes(entity)) {
      results.set(entity, {
        breakingChanges: [{
          type: 'entity_removed',
          entity,
          message: `Entity "${entity}" was removed`,
        }],
        affectedAPIs: implementations.map(i => i.path),
        safe: false,
      })
      continue
    }

    const breaks = detectContractBreaks(oldSchema, newSchema, implementations)
    if (breaks.breakingChanges.length > 0) {
      results.set(entity, breaks)
    }
  }

  return results
}

/* ========================================================================= */
/* Exports for module                                                        */
/* ========================================================================= */

export {
  FieldSchemaSchema,
  EntitySchemaSchema,
  ViolationSchema,
  ValidationResultSchema,
  BreakingChangeSchema,
  ContractBreaksSchema,
}
