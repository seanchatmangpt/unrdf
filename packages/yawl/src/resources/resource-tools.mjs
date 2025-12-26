/**
 * @fileoverview YAWL Tool Resource Management
 *
 * Handles tool-specific resource definitions, factories,
 * and allocation logic for software/hardware tools in YAWL workflows.
 *
 * @module @unrdf/yawl/resources/tools
 * @version 1.0.0
 */

import { z } from 'zod';

/* ========================================================================= */
/* Constants                                                                 */
/* ========================================================================= */

/**
 * Tool resource type constant
 * @constant {string}
 */
export const TOOL = 'Tool';

/* ========================================================================= */
/* Zod Schemas                                                               */
/* ========================================================================= */

/**
 * Tool-specific resource schema
 */
export const ToolSchema = z.object({
  id: z.string().min(1),
  type: z.literal(TOOL),
  name: z.string().optional(),
  capacity: z.number().int().min(-1).default(-1), // -1 = unlimited (default for tools)
  sparql: z.string().optional(),
  metadata: z.record(z.unknown()).optional(),
});

/**
 * @typedef {z.infer<typeof ToolSchema>} Tool
 */

/* ========================================================================= */
/* Factory Functions                                                         */
/* ========================================================================= */

/**
 * Create a Tool resource definition
 *
 * Tools are non-human resources (software services, hardware, APIs)
 * typically with unlimited capacity (-1) or high limits. They can
 * be allocated based on availability and capability checks.
 *
 * @param {Object} config - Tool configuration
 * @param {string} config.id - Tool identifier (e.g., 'email-service', 'printer-01')
 * @param {string} [config.name] - Tool display name
 * @param {number} [config.capacity=-1] - Maximum concurrent allocations (-1 = unlimited)
 * @param {string} [config.sparql] - Eligibility SPARQL query
 * @param {Object} [config.metadata] - Additional metadata (version, endpoint, etc.)
 * @returns {Tool} Validated tool resource
 *
 * @example
 * // Unlimited capacity tool (typical)
 * const emailService = createTool({
 *   id: 'email-service',
 *   name: 'Email Notification Service',
 *   capacity: -1,
 *   metadata: {
 *     endpoint: 'https://api.example.com/email',
 *     version: '2.0'
 *   }
 * });
 *
 * @example
 * // Limited capacity tool (physical resource)
 * const printer = createTool({
 *   id: 'printer-floor3',
 *   name: 'Floor 3 Color Printer',
 *   capacity: 5, // Max 5 concurrent print jobs
 *   metadata: {
 *     location: 'Building A, Floor 3',
 *     capabilities: ['color', 'duplex', 'a3']
 *   }
 * });
 *
 * @example
 * // Tool with availability check
 * const database = createTool({
 *   id: 'db-prod',
 *   name: 'Production Database',
 *   capacity: 100,
 *   sparql: `
 *     PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
 *     ASK {
 *       ?resource yawl:status "online" .
 *     }
 *   `
 * });
 */
export function createTool(config) {
  return ToolSchema.parse({
    id: config.id,
    type: TOOL,
    name: config.name,
    capacity: config.capacity ?? -1,
    sparql: config.sparql,
    metadata: config.metadata,
  });
}

/* ========================================================================= */
/* Tool Query Helpers                                                        */
/* ========================================================================= */

/**
 * Create SPARQL query to find tools by capability
 *
 * @param {string} capabilityUri - Required capability URI
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const query = createToolByCapabilityQuery('http://example.org/capabilities/print-color');
 * const results = manager.query(query);
 */
export function createToolByCapabilityQuery(capabilityUri) {
  return `
    PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?tool ?name WHERE {
      ?tool rdf:type yawl:Tool ;
            yawl:hasCapability <${capabilityUri}> .
      OPTIONAL { ?tool foaf:name ?name }
    }
  `;
}

/**
 * Create SPARQL query to find available tools
 *
 * @param {Object} [options] - Query options
 * @param {boolean} [options.onlineOnly=false] - Only return online tools
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const query = createAvailableToolsQuery({ onlineOnly: true });
 */
export function createAvailableToolsQuery(options = {}) {
  const statusFilter = options.onlineOnly
    ? 'FILTER(?status = "online")'
    : 'FILTER(!BOUND(?status) || ?status = "online")';

  return `
    PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?tool ?name ?status WHERE {
      ?tool rdf:type yawl:Tool .
      OPTIONAL { ?tool foaf:name ?name }
      OPTIONAL { ?tool yawl:status ?status }
      ${statusFilter}
    }
  `;
}

/**
 * Create SPARQL query to find tools by type/category
 *
 * @param {string} category - Tool category (e.g., 'printer', 'database', 'api')
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const query = createToolByCategoryQuery('printer');
 */
export function createToolByCategoryQuery(category) {
  return `
    PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?tool ?name ?capacity WHERE {
      ?tool rdf:type yawl:Tool ;
            yawl:category "${category}" .
      OPTIONAL { ?tool foaf:name ?name }
      OPTIONAL { ?tool yawl:capacity ?capacity }
    }
  `;
}

/**
 * Create SPARQL query for tool utilization stats
 *
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const query = createToolUtilizationQuery();
 * const results = manager.query(query);
 * // Results: [{ tool, name, capacity, allocations }]
 */
export function createToolUtilizationQuery() {
  return `
    PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?tool ?name ?capacity (COUNT(?allocation) as ?allocations) WHERE {
      ?tool rdf:type yawl:Tool .
      OPTIONAL { ?tool foaf:name ?name }
      OPTIONAL { ?tool yawl:capacity ?capacity }
      OPTIONAL {
        ?allocation rdf:type yawl:Allocation ;
                    yawl:resource ?tool .
        FILTER NOT EXISTS {
          ?allocation yawl:status "deallocated" .
        }
      }
    }
    GROUP BY ?tool ?name ?capacity
    ORDER BY DESC(?allocations)
  `;
}

/* ========================================================================= */
/* Tool Utilities                                                            */
/* ========================================================================= */

/**
 * Validate tool configuration
 *
 * @param {unknown} config - Configuration to validate
 * @returns {{ valid: boolean, errors?: z.ZodError }}
 *
 * @example
 * const result = validateTool({ id: 'email-svc', type: 'Tool' });
 * if (!result.valid) {
 *   console.error(result.errors);
 * }
 */
export function validateTool(config) {
  try {
    ToolSchema.parse(config);
    return { valid: true };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return { valid: false, errors: error };
    }
    throw error;
  }
}

/**
 * Check if a resource is a tool
 *
 * @param {unknown} resource - Resource to check
 * @returns {boolean}
 *
 * @example
 * if (isTool(resource)) {
 *   console.log('This is a tool resource');
 * }
 */
export function isTool(resource) {
  if (!resource || typeof resource !== 'object') return false;
  return 'type' in resource && resource.type === TOOL;
}

/**
 * Extract tool metadata
 *
 * @param {Tool} tool - Tool resource
 * @param {string} key - Metadata key to extract
 * @returns {unknown} Metadata value or undefined
 *
 * @example
 * const endpoint = getToolMetadata(tool, 'endpoint');
 */
export function getToolMetadata(tool, key) {
  if (!tool?.metadata) return undefined;
  return tool.metadata[key];
}

/**
 * Check if tool has unlimited capacity
 *
 * @param {Tool} tool - Tool resource
 * @returns {boolean}
 *
 * @example
 * if (isUnlimitedCapacity(emailService)) {
 *   console.log('Can allocate without capacity checks');
 * }
 */
export function isUnlimitedCapacity(tool) {
  return tool.capacity === -1;
}

/**
 * Create tool with online status check
 *
 * Helper to create a tool with automatic online/offline eligibility.
 *
 * @param {Object} config - Configuration
 * @param {string} config.id - Tool ID
 * @param {string} [config.name] - Tool name
 * @param {number} [config.capacity=-1] - Capacity
 * @param {Object} [config.metadata] - Additional metadata
 * @returns {Tool}
 *
 * @example
 * const db = createToolWithStatusCheck({
 *   id: 'db-prod',
 *   name: 'Production DB',
 *   capacity: 100
 * });
 */
export function createToolWithStatusCheck(config) {
  const sparql = `
    PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
    ASK {
      ?resource yawl:status "online" .
    }
  `;

  return createTool({
    id: config.id,
    name: config.name,
    capacity: config.capacity ?? -1,
    sparql,
    metadata: config.metadata,
  });
}

/**
 * Create API tool with endpoint metadata
 *
 * Convenience factory for API/service tools.
 *
 * @param {Object} config - Configuration
 * @param {string} config.id - Tool ID
 * @param {string} config.name - Tool name
 * @param {string} config.endpoint - API endpoint URL
 * @param {number} [config.capacity=-1] - Max concurrent requests
 * @param {Object} [config.metadata] - Additional metadata
 * @returns {Tool}
 *
 * @example
 * const api = createApiTool({
 *   id: 'payment-api',
 *   name: 'Payment Gateway',
 *   endpoint: 'https://api.payments.example.com',
 *   capacity: 50
 * });
 */
export function createApiTool(config) {
  return createTool({
    id: config.id,
    name: config.name,
    capacity: config.capacity ?? -1,
    metadata: {
      ...(config.metadata || {}),
      endpoint: config.endpoint,
      type: 'api',
    },
  });
}

/* ========================================================================= */
/* Exports                                                                   */
/* ========================================================================= */

export default {
  TOOL,
  ToolSchema,
  createTool,
  createToolWithStatusCheck,
  createApiTool,
  createToolByCapabilityQuery,
  createAvailableToolsQuery,
  createToolByCategoryQuery,
  createToolUtilizationQuery,
  validateTool,
  isTool,
  getToolMetadata,
  isUnlimitedCapacity,
};
