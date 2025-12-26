/**
 * @fileoverview YAWL Participant Resource Management
 *
 * Handles participant-specific resource definitions, factories,
 * and allocation logic for human resources in YAWL workflows.
 *
 * @module @unrdf/yawl/resources/participants
 * @version 1.0.0
 */

import { z } from 'zod';

/* ========================================================================= */
/* Constants                                                                 */
/* ========================================================================= */

/**
 * Participant resource type constant
 * @constant {string}
 */
export const PARTICIPANT = 'Participant';

/* ========================================================================= */
/* Zod Schemas                                                               */
/* ========================================================================= */

/**
 * Participant-specific resource schema
 */
export const ParticipantSchema = z.object({
  id: z.string().min(1),
  type: z.literal(PARTICIPANT),
  name: z.string().optional(),
  capacity: z.number().int().min(-1).default(1), // -1 = unlimited
  sparql: z.string().optional(),
  metadata: z.record(z.unknown()).optional(),
});

/**
 * @typedef {z.infer<typeof ParticipantSchema>} Participant
 */

/* ========================================================================= */
/* Factory Functions                                                         */
/* ========================================================================= */

/**
 * Create a Participant resource definition
 *
 * Participants are human resources with limited capacity (typically 1)
 * and availability windows. They can be allocated to work items based
 * on SPARQL eligibility conditions.
 *
 * @param {Object} config - Participant configuration
 * @param {string} config.id - Participant identifier (e.g., 'alice', 'emp-123')
 * @param {string} [config.name] - Participant display name
 * @param {number} [config.capacity=1] - Maximum concurrent work item allocations
 * @param {string} [config.sparql] - Eligibility SPARQL query (ASK or SELECT)
 * @param {Object} [config.metadata] - Additional metadata (skills, department, etc.)
 * @returns {Participant} Validated participant resource
 *
 * @example
 * // Simple participant
 * const alice = createParticipant({
 *   id: 'alice',
 *   name: 'Alice Smith',
 *   capacity: 1
 * });
 *
 * @example
 * // Participant with eligibility check
 * const manager = createParticipant({
 *   id: 'manager-001',
 *   name: 'John Manager',
 *   capacity: 1,
 *   sparql: `
 *     PREFIX foaf: <http://xmlns.com/foaf/0.1/>
 *     ASK {
 *       ?workItem :requiresRole :manager .
 *       ?resource foaf:hasRole :manager .
 *     }
 *   `,
 *   metadata: {
 *     department: 'Operations',
 *     level: 'Senior'
 *   }
 * });
 */
export function createParticipant(config) {
  return ParticipantSchema.parse({
    id: config.id,
    type: PARTICIPANT,
    name: config.name,
    capacity: config.capacity ?? 1,
    sparql: config.sparql,
    metadata: config.metadata,
  });
}

/* ========================================================================= */
/* Participant Query Helpers                                                 */
/* ========================================================================= */

/**
 * Create SPARQL query to find participants by role
 *
 * @param {string} roleUri - Role URI to filter by
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const query = createParticipantByRoleQuery('http://example.org/roles/approver');
 * const results = manager.query(query);
 */
export function createParticipantByRoleQuery(roleUri) {
  return `
    PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?participant ?name WHERE {
      ?participant rdf:type yawl:Participant ;
                   foaf:hasRole <${roleUri}> .
      OPTIONAL { ?participant foaf:name ?name }
    }
  `;
}

/**
 * Create SPARQL query to find available participants
 *
 * @param {Object} [options] - Query options
 * @param {string} [options.from] - Start time (ISO 8601)
 * @param {string} [options.to] - End time (ISO 8601)
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const query = createAvailableParticipantsQuery({
 *   from: '2024-01-15T09:00:00Z',
 *   to: '2024-01-15T17:00:00Z'
 * });
 */
export function createAvailableParticipantsQuery(options = {}) {
  const timeFilter = options.from && options.to
    ? `
      FILTER(?available = true)
      OPTIONAL {
        ?participant yawl:hasAvailabilityWindow ?window .
        ?window yawl:scheduleStart ?start .
        ?window yawl:scheduleEnd ?end .
        FILTER(?start <= "${options.from}"^^xsd:dateTime && ?end >= "${options.to}"^^xsd:dateTime)
      }
    `
    : 'FILTER(?available = true)';

  return `
    PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

    SELECT ?participant ?name ?available WHERE {
      ?participant rdf:type yawl:Participant .
      OPTIONAL { ?participant foaf:name ?name }
      OPTIONAL { ?participant foaf:available ?available }
      ${timeFilter}
    }
  `;
}

/**
 * Create SPARQL query to find participants by skill/capability
 *
 * @param {string} capabilityUri - Required capability URI
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const query = createParticipantByCapabilityQuery('http://example.org/skills/java');
 */
export function createParticipantByCapabilityQuery(capabilityUri) {
  return `
    PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?participant ?name WHERE {
      ?participant rdf:type yawl:Participant ;
                   yawl:hasCapability <${capabilityUri}> .
      OPTIONAL { ?participant foaf:name ?name }
    }
  `;
}

/**
 * Create SPARQL query for participant workload
 *
 * Returns participant allocation counts for workload balancing.
 *
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const query = createParticipantWorkloadQuery();
 * const results = manager.query(query);
 * // Results: [{ participant, name, allocations }]
 */
export function createParticipantWorkloadQuery() {
  return `
    PREFIX yawl: <http://yawlfoundation.org/yawlschema#>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    SELECT ?participant ?name (COUNT(?allocation) as ?allocations) WHERE {
      ?participant rdf:type yawl:Participant .
      OPTIONAL { ?participant foaf:name ?name }
      OPTIONAL {
        ?allocation rdf:type yawl:Allocation ;
                    yawl:resource ?participant .
        FILTER NOT EXISTS {
          ?allocation yawl:status "deallocated" .
        }
      }
    }
    GROUP BY ?participant ?name
    ORDER BY DESC(?allocations)
  `;
}

/* ========================================================================= */
/* Participant Utilities                                                     */
/* ========================================================================= */

/**
 * Validate participant configuration
 *
 * @param {unknown} config - Configuration to validate
 * @returns {{ valid: boolean, errors?: z.ZodError }}
 *
 * @example
 * const result = validateParticipant({ id: 'alice', type: 'Participant' });
 * if (!result.valid) {
 *   console.error(result.errors);
 * }
 */
export function validateParticipant(config) {
  try {
    ParticipantSchema.parse(config);
    return { valid: true };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return { valid: false, errors: error };
    }
    throw error;
  }
}

/**
 * Check if a resource is a participant
 *
 * @param {unknown} resource - Resource to check
 * @returns {boolean}
 *
 * @example
 * if (isParticipant(resource)) {
 *   console.log('This is a participant resource');
 * }
 */
export function isParticipant(resource) {
  if (!resource || typeof resource !== 'object') return false;
  return 'type' in resource && resource.type === PARTICIPANT;
}

/**
 * Extract participant metadata
 *
 * @param {Participant} participant - Participant resource
 * @param {string} key - Metadata key to extract
 * @returns {unknown} Metadata value or undefined
 *
 * @example
 * const department = getParticipantMetadata(participant, 'department');
 */
export function getParticipantMetadata(participant, key) {
  if (!participant?.metadata) return undefined;
  return participant.metadata[key];
}

/**
 * Create participant with role assignment
 *
 * Helper to create a participant with automatic role-based eligibility.
 *
 * @param {Object} config - Configuration
 * @param {string} config.id - Participant ID
 * @param {string} [config.name] - Participant name
 * @param {string} config.role - Role URI for eligibility
 * @param {number} [config.capacity=1] - Capacity
 * @returns {Participant}
 *
 * @example
 * const approver = createParticipantWithRole({
 *   id: 'alice',
 *   name: 'Alice Smith',
 *   role: 'http://example.org/roles/approver'
 * });
 */
export function createParticipantWithRole(config) {
  const sparql = `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    ASK {
      ?resource foaf:hasRole <${config.role}> ;
                foaf:available true .
    }
  `;

  return createParticipant({
    id: config.id,
    name: config.name,
    capacity: config.capacity ?? 1,
    sparql,
    metadata: {
      ...(config.metadata || {}),
      roleUri: config.role,
    },
  });
}

/* ========================================================================= */
/* Exports                                                                   */
/* ========================================================================= */

export default {
  PARTICIPANT,
  ParticipantSchema,
  createParticipant,
  createParticipantWithRole,
  createParticipantByRoleQuery,
  createAvailableParticipantsQuery,
  createParticipantByCapabilityQuery,
  createParticipantWorkloadQuery,
  validateParticipant,
  isParticipant,
  getParticipantMetadata,
};
