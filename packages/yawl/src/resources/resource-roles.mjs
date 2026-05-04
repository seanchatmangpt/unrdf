/**
 * @fileoverview YAWL Role Resource Management
 *
 * Handles role-specific resource definitions, factories, SPARQL helpers,
 * and role-based allocation logic for YAWL workflows.
 *
 * @module @unrdf/yawl/resources/roles
 * @version 1.0.0
 */

import { z } from 'zod';

/* ========================================================================= */
/* Constants                                                                 */
/* ========================================================================= */

/**
 * Role resource type constant
 * @constant {string}
 */
export const ROLE = 'Role';

/**
 * Common YAWL/FOAF namespace constants for role queries
 */
export const YAWL_NS = 'http://yawlfoundation.org/yawlschema#';
export const FOAF_NS = 'http://xmlns.com/foaf/0.1/';
export const RDF_NS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
export const XSD_NS = 'http://www.w3.org/2001/XMLSchema#';
export const TIME_NS = 'http://www.w3.org/2006/time#';

/* ========================================================================= */
/* Zod Schemas                                                               */
/* ========================================================================= */

/**
 * Role-specific resource schema
 */
export const RoleSchema = z.object({
  id: z.string().min(1),
  type: z.literal(ROLE),
  name: z.string().optional(),
  capacity: z.number().int().min(-1).default(-1), // -1 = unlimited (typical for roles)
  sparql: z.string().optional(),
  metadata: z.record(z.unknown()).optional(),
});

/**
 * @typedef {z.infer<typeof RoleSchema>} Role
 */

/* ========================================================================= */
/* Factory Functions                                                         */
/* ========================================================================= */

/**
 * Create a Role resource definition
 *
 * Roles represent abstract organizational positions or capabilities.
 * Multiple participants can fulfill a role, and roles typically have
 * unlimited capacity with eligibility based on role membership checks.
 *
 * @param {Object} config - Role configuration
 * @param {string} config.id - Role identifier (e.g., 'approvers', 'managers')
 * @param {string} [config.name] - Role display name
 * @param {number} [config.capacity=-1] - Maximum concurrent allocations (-1 = unlimited)
 * @param {string} [config.sparql] - Eligibility SPARQL query (for member checking)
 * @param {Object} [config.metadata] - Additional metadata (description, requirements, etc.)
 * @returns {Role} Validated role resource
 *
 * @example
 * // Simple role with membership check
 * const approvers = createRole({
 *   id: 'approvers',
 *   name: 'Approval Team',
 *   sparql: `
 *     PREFIX foaf: <http://xmlns.com/foaf/0.1/>
 *     ASK {
 *       ?person foaf:hasRole <http://example.org/roles/approvers> ;
 *               foaf:available true .
 *     }
 *   `
 * });
 *
 * @example
 * // Role with metadata and capacity limit
 * const seniorManagers = createRole({
 *   id: 'senior-managers',
 *   name: 'Senior Management',
 *   capacity: 3, // Limit to 3 concurrent work items
 *   metadata: {
 *     description: 'Senior managers with budget approval authority',
 *     level: 'L6+',
 *     department: 'Management'
 *   }
 * });
 */
export function createRole(config) {
  return RoleSchema.parse({
    id: config.id,
    type: ROLE,
    name: config.name,
    capacity: config.capacity ?? -1,
    sparql: config.sparql,
    metadata: config.metadata,
  });
}

/* ========================================================================= */
/* SPARQL Query Helpers for Roles                                            */
/* ========================================================================= */

/**
 * Common SPARQL prefixes for resource queries
 * @constant {string}
 */
export const RESOURCE_SPARQL_PREFIXES = `
PREFIX yawl: <${YAWL_NS}>
PREFIX foaf: <${FOAF_NS}>
PREFIX rdf: <${RDF_NS}>
PREFIX xsd: <${XSD_NS}>
PREFIX time: <${TIME_NS}>
`;

/**
 * Create an eligibility SPARQL ASK query for role membership
 *
 * Checks if a participant has the specified role and is available.
 *
 * @param {string} roleUri - Role URI to check membership
 * @returns {string} SPARQL ASK query
 *
 * @example
 * const sparql = createRoleMembershipQuery('http://example.org/roles/approvers');
 * // Use in role definition:
 * const role = createRole({
 *   id: 'approvers',
 *   sparql: createRoleMembershipQuery('http://example.org/roles/approvers')
 * });
 */
export function createRoleMembershipQuery(roleUri) {
  return `
    ${RESOURCE_SPARQL_PREFIXES}
    ASK {
      ?person foaf:hasRole <${roleUri}> ;
              foaf:available true .
    }
  `;
}

/**
 * Create an eligibility SPARQL ASK query for capability check
 *
 * Checks if a resource has the required capability.
 *
 * @param {string} capabilityUri - Required capability URI
 * @returns {string} SPARQL ASK query
 *
 * @example
 * const sparql = createCapabilityQuery('http://example.org/capabilities/sign-documents');
 */
export function createCapabilityQuery(capabilityUri) {
  return `
    ${RESOURCE_SPARQL_PREFIXES}
    ASK {
      ?resource yawl:hasCapability <${capabilityUri}> .
    }
  `;
}

/**
 * Create SPARQL query to find all members of a role
 *
 * @param {string} roleUri - Role URI
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const query = createRoleMembersQuery('http://example.org/roles/approvers');
 * const results = manager.query(query);
 */
export function createRoleMembersQuery(roleUri) {
  return `
    ${RESOURCE_SPARQL_PREFIXES}

    SELECT ?member ?name ?available WHERE {
      ?member foaf:hasRole <${roleUri}> .
      OPTIONAL { ?member foaf:name ?name }
      OPTIONAL { ?member foaf:available ?available }
    }
    ORDER BY ?name
  `;
}

/**
 * Create SPARQL query to find available role members
 *
 * @param {string} roleUri - Role URI
 * @param {Object} [options] - Query options
 * @param {number} [options.maxAllocations] - Max current allocations filter
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const query = createAvailableRoleMembersQuery(
 *   'http://example.org/roles/approvers',
 *   { maxAllocations: 2 }
 * );
 */
export function createAvailableRoleMembersQuery(roleUri, options = {}) {
  return `
    ${RESOURCE_SPARQL_PREFIXES}

    SELECT ?member ?name (COUNT(?allocation) as ?allocations) WHERE {
      ?member foaf:hasRole <${roleUri}> ;
              foaf:available true .
      OPTIONAL { ?member foaf:name ?name }
      OPTIONAL {
        ?allocation rdf:type yawl:Allocation ;
                    yawl:resource ?member .
        FILTER NOT EXISTS {
          ?allocation yawl:status "deallocated" .
        }
      }
    }
    GROUP BY ?member ?name
    ${options.maxAllocations ? `HAVING (COUNT(?allocation) <= ${options.maxAllocations})` : ''}
    ORDER BY ?allocations ?name
  `;
}

/**
 * Create SPARQL query to find roles by participant
 *
 * @param {string} participantUri - Participant URI
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const query = createRolesByParticipantQuery('http://example.org/participants/alice');
 */
export function createRolesByParticipantQuery(participantUri) {
  return `
    ${RESOURCE_SPARQL_PREFIXES}

    SELECT ?role ?name WHERE {
      <${participantUri}> foaf:hasRole ?role .
      OPTIONAL {
        ?role foaf:name ?name .
      }
    }
    ORDER BY ?name
  `;
}

/**
 * Create SPARQL query for role hierarchy
 *
 * Finds parent/child role relationships using rdfs:subClassOf.
 *
 * @param {string} roleUri - Role URI to get hierarchy for
 * @returns {string} SPARQL SELECT query
 *
 * @example
 * const query = createRoleHierarchyQuery('http://example.org/roles/manager');
 */
export function createRoleHierarchyQuery(roleUri) {
  return `
    ${RESOURCE_SPARQL_PREFIXES}
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?parentRole ?childRole WHERE {
      {
        <${roleUri}> rdfs:subClassOf ?parentRole .
      }
      UNION
      {
        ?childRole rdfs:subClassOf <${roleUri}> .
      }
    }
  `;
}

/* ========================================================================= */
/* Role Utilities                                                            */
/* ========================================================================= */

/**
 * Validate role configuration
 *
 * @param {unknown} config - Configuration to validate
 * @returns {{ valid: boolean, errors?: z.ZodError }}
 *
 * @example
 * const result = validateRole({ id: 'approvers', type: 'Role' });
 * if (!result.valid) {
 *   console.error(result.errors);
 * }
 */
export function validateRole(config) {
  try {
    RoleSchema.parse(config);
    return { valid: true };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return { valid: false, errors: error };
    }
    throw error;
  }
}

/**
 * Check if a resource is a role
 *
 * @param {unknown} resource - Resource to check
 * @returns {boolean}
 *
 * @example
 * if (isRole(resource)) {
 *   console.log('This is a role resource');
 * }
 */
export function isRole(resource) {
  if (!resource || typeof resource !== 'object') return false;
  return 'type' in resource && resource.type === ROLE;
}

/**
 * Extract role metadata
 *
 * @param {Role} role - Role resource
 * @param {string} key - Metadata key to extract
 * @returns {unknown} Metadata value or undefined
 *
 * @example
 * const description = getRoleMetadata(role, 'description');
 */
export function getRoleMetadata(role, key) {
  if (!role?.metadata) return undefined;
  return role.metadata[key];
}

/**
 * Create role with automatic membership query
 *
 * Helper to create a role with a standard membership eligibility query.
 *
 * @param {Object} config - Configuration
 * @param {string} config.id - Role ID
 * @param {string} [config.name] - Role name
 * @param {string} config.roleUri - Role URI for membership check
 * @param {number} [config.capacity=-1] - Capacity
 * @param {Object} [config.metadata] - Additional metadata
 * @returns {Role}
 *
 * @example
 * const approvers = createRoleWithMembership({
 *   id: 'approvers',
 *   name: 'Approval Team',
 *   roleUri: 'http://example.org/roles/approvers'
 * });
 */
export function createRoleWithMembership(config) {
  return createRole({
    id: config.id,
    name: config.name,
    capacity: config.capacity ?? -1,
    sparql: createRoleMembershipQuery(config.roleUri),
    metadata: {
      ...(config.metadata || {}),
      roleUri: config.roleUri,
    },
  });
}

/**
 * Create role with capability requirement
 *
 * Helper to create a role with capability-based eligibility.
 *
 * @param {Object} config - Configuration
 * @param {string} config.id - Role ID
 * @param {string} [config.name] - Role name
 * @param {string} config.capability - Required capability URI
 * @param {number} [config.capacity=-1] - Capacity
 * @param {Object} [config.metadata] - Additional metadata
 * @returns {Role}
 *
 * @example
 * const signers = createRoleWithCapability({
 *   id: 'document-signers',
 *   name: 'Document Signers',
 *   capability: 'http://example.org/capabilities/sign-documents'
 * });
 */
export function createRoleWithCapability(config) {
  return createRole({
    id: config.id,
    name: config.name,
    capacity: config.capacity ?? -1,
    sparql: createCapabilityQuery(config.capability),
    metadata: {
      ...(config.metadata || {}),
      capability: config.capability,
    },
  });
}

/**
 * Build role URI from ID
 *
 * @param {string} roleId - Role identifier
 * @param {string} [baseUri='http://example.org/roles/'] - Base URI for roles
 * @returns {string} Full role URI
 *
 * @example
 * const uri = buildRoleUri('approvers');
 * // Returns: 'http://example.org/roles/approvers'
 */
export function buildRoleUri(roleId, baseUri = 'http://example.org/roles/') {
  return `${baseUri}${roleId}`;
}

/* ========================================================================= */
/* Exports                                                                   */
/* ========================================================================= */

export default {
  ROLE,
  RoleSchema,
  createRole,
  createRoleWithMembership,
  createRoleWithCapability,
  RESOURCE_SPARQL_PREFIXES,
  createRoleMembershipQuery,
  createCapabilityQuery,
  createRoleMembersQuery,
  createAvailableRoleMembersQuery,
  createRolesByParticipantQuery,
  createRoleHierarchyQuery,
  validateRole,
  isRole,
  getRoleMetadata,
  buildRoleUri,
};
