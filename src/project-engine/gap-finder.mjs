/**
 * @file Predictive Gap Finder - analyze domain model + project files to identify missing roles
 * @module project-engine/gap-finder
 */

import { DataFactory } from 'n3';
import { z } from 'zod';

const { namedNode } = DataFactory;

/* ========================================================================= */
/* Namespace prefixes                                                        */
/* ========================================================================= */

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  dom: 'http://example.org/unrdf/domain#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
};

/* ========================================================================= */
/* Zod Schemas                                                               */
/* ========================================================================= */

const StackProfileSchema = z
  .object({
    webFramework: z.string().nullable().optional(),
    uiFramework: z.string().nullable().optional(),
    apiFramework: z.string().nullable().optional(),
    testFramework: z.string().nullable().optional(),
  })
  .passthrough()
  .optional();

const FindMissingRolesOptionsSchema = z.object({
  domainStore: z.custom(val => val && typeof val.getQuads === 'function', {
    message: 'domainStore must be an RDF store with getQuads method',
  }),
  projectStore: z.custom(val => val && typeof val.getQuads === 'function', {
    message: 'projectStore must be an RDF store with getQuads method',
  }),
  templateGraph: z.custom(val => val && typeof val.getQuads === 'function').optional(),
  stackProfile: StackProfileSchema,
});

/* ========================================================================= */
/* Role configuration                                                        */
/* ========================================================================= */

/**
 * Base role importance scores (0-100)
 */
const ROLE_BASE_SCORES = {
  Api: 95,
  Route: 95,
  Test: 90,
  Component: 80,
  Page: 80,
  View: 80,
  Schema: 70,
  Service: 60,
  Hook: 55,
  Doc: 50,
};

/**
 * Required roles per framework
 */
const FRAMEWORK_ROLES = {
  next: ['Page', 'Api', 'Component', 'Test', 'Schema'],
  'next-app-router': ['Page', 'Api', 'Component', 'Test', 'Schema'],
  'next-pages': ['Page', 'Api', 'Component', 'Test', 'Schema'],
  express: ['Route', 'Service', 'Test', 'Schema'],
  nest: ['Controller', 'Service', 'Test', 'Schema'],
  react: ['Component', 'Test', 'Schema'],
  default: ['Api', 'Component', 'Test', 'Schema'],
};

/**
 * Framework-specific role score boosts
 */
const FRAMEWORK_BOOSTS = {
  next: { Page: 15, Api: 10 },
  'next-app-router': { Page: 15, Api: 10 },
  'next-pages': { Page: 15, Api: 10 },
  express: { Route: 15, Service: 10 },
  nest: { Controller: 15, Service: 10 },
};

/* ========================================================================= */
/* Entity extraction                                                         */
/* ========================================================================= */

/**
 * Extract entity names from domain store
 * @param {Store} domainStore
 * @returns {string[]}
 */
function extractEntities(domainStore) {
  const entities = [];
  const quads = domainStore.getQuads(
    null,
    namedNode(`${NS.rdf}type`),
    namedNode(`${NS.dom}Entity`)
  );

  for (const quad of quads) {
    const iri = quad.subject.value;
    const name = iri.split('#').pop() || iri.split('/').pop();
    if (name) entities.push(name);
  }

  return entities;
}

/* ========================================================================= */
/* File role extraction                                                      */
/* ========================================================================= */

/**
 * Extract file paths with their roles from project store
 * @param {Store} projectStore
 * @returns {Array<{path: string, role: string|null}>}
 */
function extractFilesWithRoles(projectStore) {
  const files = [];

  const pathQuads = projectStore.getQuads(null, namedNode(`${NS.fs}relativePath`), null);

  for (const quad of pathQuads) {
    const fileIri = quad.subject;
    const path = quad.object.value;

    // Get role for this file
    const roleQuads = projectStore.getQuads(fileIri, namedNode(`${NS.proj}roleString`), null);

    const role = roleQuads.length > 0 ? roleQuads[0].object.value : null;
    files.push({ path, role });
  }

  return files;
}

/* ========================================================================= */
/* Entity-file matching                                                      */
/* ========================================================================= */

/**
 * Match file to entity by name pattern
 * @param {string} filePath
 * @param {string} entityName
 * @returns {boolean}
 */
function fileMatchesEntity(filePath, entityName) {
  const lowerPath = filePath.toLowerCase();
  const lowerEntity = entityName.toLowerCase();

  // Direct match
  if (lowerPath.includes(lowerEntity)) return true;

  // Plural form: User -> users
  if (lowerPath.includes(lowerEntity + 's')) return true;

  // Kebab-case: UserProfile -> user-profile
  const kebabEntity = entityName.replace(/([a-z])([A-Z])/g, '$1-$2').toLowerCase();
  if (lowerPath.includes(kebabEntity)) return true;

  // Snake_case: UserProfile -> user_profile
  const snakeEntity = entityName.replace(/([a-z])([A-Z])/g, '$1_$2').toLowerCase();
  if (lowerPath.includes(snakeEntity)) return true;

  return false;
}

/**
 * Find roles present for an entity
 * @param {string} entityName
 * @param {Array<{path: string, role: string|null}>} files
 * @returns {Set<string>}
 */
function findPresentRoles(entityName, files) {
  const roles = new Set();

  for (const { path, role } of files) {
    if (role && fileMatchesEntity(path, entityName)) {
      roles.add(role);
    }
  }

  return roles;
}

/* ========================================================================= */
/* Gap detection                                                             */
/* ========================================================================= */

/**
 * Determine required roles based on stack profile
 * @param {Object} stackProfile
 * @returns {string[]}
 */
function getRequiredRoles(stackProfile) {
  if (!stackProfile) return FRAMEWORK_ROLES.default;

  const framework = stackProfile.webFramework || stackProfile.apiFramework;
  if (framework && FRAMEWORK_ROLES[framework]) {
    return FRAMEWORK_ROLES[framework];
  }

  return FRAMEWORK_ROLES.default;
}

/**
 * Calculate missing roles for an entity
 * @param {string} entityName
 * @param {Set<string>} presentRoles
 * @param {string[]} requiredRoles
 * @returns {string[]}
 */
function calculateMissingRoles(entityName, presentRoles, requiredRoles) {
  return requiredRoles.filter(role => !presentRoles.has(role));
}

/**
 * Generate suggestion for a missing role
 * @param {string} entityName
 * @param {string} role
 * @returns {string}
 */
function generateSuggestion(entityName, role) {
  const suggestions = {
    Api: `${entityName}Api is needed for /${entityName.toLowerCase()} endpoint`,
    Route: `${entityName}Route handler needed for /${entityName.toLowerCase()} routes`,
    Page: `${entityName}Page needed for /${entityName.toLowerCase()} view`,
    Component: `${entityName}View component needed for UI`,
    Test: `${entityName} test suite needed for coverage`,
    Schema: `${entityName}Schema needed for validation`,
    Service: `${entityName}Service needed for business logic`,
    Doc: `${entityName} documentation needed`,
  };

  return suggestions[role] || `${entityName}${role} needed`;
}

/* ========================================================================= */
/* Main API                                                                  */
/* ========================================================================= */

/**
 * @typedef {Object} GapResult
 * @property {string} entity - Entity name
 * @property {string[]} missingRoles - Roles that are missing
 * @property {number} score - Priority score (0-100)
 * @property {string} suggestion - Human-readable suggestion
 * @property {string[]} [files] - Related files if they exist but aren't classified
 */

/**
 * @typedef {Object} FindMissingRolesResult
 * @property {GapResult[]} gaps - Array of gap results
 * @property {string} summary - Human-readable summary
 * @property {string} callToAction - CLI command suggestion
 */

/**
 * Analyze domain model + project files to predict which features are missing roles
 *
 * @param {Object} options
 * @param {Store} options.domainStore - Domain model store (from inferDomainModel)
 * @param {Store} options.projectStore - Project structure store (from buildProjectModelFromFs + classifyFiles)
 * @param {Store} [options.templateGraph] - Optional template graph
 * @param {Object} [options.stackProfile] - Stack profile for framework-specific rules
 * @returns {FindMissingRolesResult}
 */
export function findMissingRoles(options) {
  const validated = FindMissingRolesOptionsSchema.parse(options);
  const { domainStore, projectStore, stackProfile } = validated;

  // Extract entities from domain model
  const entities = extractEntities(domainStore);

  // Early exit if no entities
  if (entities.length === 0) {
    return {
      gaps: [],
      summary: '0 gaps found (no entities in domain model)',
      callToAction: 'Run: unrdf infer-domain to detect domain entities',
    };
  }

  // Extract files with roles from project store
  const files = extractFilesWithRoles(projectStore);

  // Determine required roles based on stack
  const requiredRoles = getRequiredRoles(stackProfile);

  // Find gaps for each entity
  const gaps = [];
  const gapCounts = { Api: 0, Test: 0, Component: 0, Schema: 0, other: 0 };

  for (const entityName of entities) {
    const presentRoles = findPresentRoles(entityName, files);
    const missingRoles = calculateMissingRoles(entityName, presentRoles, requiredRoles);

    // Calculate max score for this entity's gaps
    const maxScore = missingRoles.reduce((max, role) => {
      const score = scoreMissingRole(entityName, role, stackProfile || {});
      return Math.max(max, score);
    }, 0);

    // Count gaps by type
    for (const role of missingRoles) {
      if (gapCounts[role] !== undefined) {
        gapCounts[role]++;
      } else {
        gapCounts.other++;
      }
    }

    gaps.push({
      entity: entityName,
      missingRoles,
      score: maxScore,
      suggestion: missingRoles.length > 0 ? generateSuggestion(entityName, missingRoles[0]) : '',
    });
  }

  // Sort by score (highest first)
  gaps.sort((a, b) => b.score - a.score);

  // Generate summary
  const summaryParts = [];
  if (gapCounts.Api > 0) summaryParts.push(`${gapCounts.Api} missing APIs`);
  if (gapCounts.Test > 0) summaryParts.push(`${gapCounts.Test} missing tests`);
  if (gapCounts.Component > 0) summaryParts.push(`${gapCounts.Component} missing components`);
  if (gapCounts.Schema > 0) summaryParts.push(`${gapCounts.Schema} missing schemas`);
  if (gapCounts.other > 0) summaryParts.push(`${gapCounts.other} other gaps`);

  const summary =
    summaryParts.length > 0
      ? summaryParts.join(', ')
      : '0 gaps found - all entities have required roles';

  // Generate call to action
  const topGap = gaps.find(g => g.missingRoles.length > 0);
  const callToAction = topGap
    ? `Run: unrdf generate --entity ${topGap.entity} --roles ${topGap.missingRoles.join(',')}`
    : 'Run: unrdf analyze to check for other issues';

  return { gaps, summary, callToAction };
}

/**
 * Score the importance of a missing role for an entity
 *
 * @param {string} entity - Entity name
 * @param {string} role - Missing role name
 * @param {Object} stackProfile - Stack profile for framework-specific boosts
 * @returns {number} Score from 0-100
 */
export function scoreMissingRole(entity, role, stackProfile) {
  // Get base score
  let score = ROLE_BASE_SCORES[role] || 40;

  // Apply framework-specific boosts
  const framework = stackProfile?.webFramework || stackProfile?.apiFramework;
  if (framework && FRAMEWORK_BOOSTS[framework]) {
    const boost = FRAMEWORK_BOOSTS[framework][role] || 0;
    score += boost;
  }

  // Cap at 100
  return Math.min(score, 100);
}
