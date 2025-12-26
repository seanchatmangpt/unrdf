/**
 * @fileoverview Forbidden Operations (H) - Guards that prevent dangerous changes
 *
 * These guards define operations that are NEVER allowed, regardless of invariants.
 * They protect the integrity of the industrial substrate and corporate canon.
 *
 * Forbidden Operations (H):
 * - EditIndustrialSubstrate: Cannot modify protected substrate namespaces
 * - RedefineProtectedTerm: Cannot redefine terms in protected vocabularies
 * - WeakenCorporateCanon: Cannot violate canonical type assignments
 *
 * @module admission/forbidden-operations
 */

import { z } from 'zod';

/**
 * Protected namespaces that form the industrial substrate
 * These CANNOT be modified via overlays
 */
export const PROTECTED_NAMESPACES = new Set([
  'http://www.w3.org/1999/02/22-rdf-syntax-ns#',  // RDF
  'http://www.w3.org/2000/01/rdf-schema#',         // RDFS
  'http://www.w3.org/2002/07/owl#',                // OWL
  'http://www.w3.org/ns/shacl#',                   // SHACL
  'http://www.w3.org/2004/02/skos/core#',          // SKOS
  'http://purl.org/dc/elements/1.1/',              // Dublin Core
  'http://purl.org/dc/terms/',                     // Dublin Core Terms
  'http://xmlns.com/foaf/0.1/',                    // FOAF
]);

/**
 * Corporate canon - protected type assignments
 * Format: { namespace: Set<term> }
 */
export const CORPORATE_CANON = {
  'http://www.w3.org/1999/02/22-rdf-syntax-ns#': new Set([
    'type', 'Property', 'Statement', 'subject', 'predicate', 'object',
    'Bag', 'Seq', 'Alt', 'List', 'nil', 'first', 'rest'
  ]),
  'http://www.w3.org/2000/01/rdf-schema#': new Set([
    'Resource', 'Class', 'Literal', 'Datatype', 'Container',
    'label', 'comment', 'domain', 'range', 'subClassOf', 'subPropertyOf'
  ]),
  'http://www.w3.org/2002/07/owl#': new Set([
    'Class', 'Thing', 'Nothing', 'ObjectProperty', 'DatatypeProperty',
    'inverseOf', 'sameAs', 'equivalentClass', 'equivalentProperty'
  ])
};

/**
 * Schema for guard check result
 */
export const GuardResultSchema = z.object({
  allowed: z.boolean(),
  guardName: z.string(),
  reason: z.string(),
  violatingQuads: z.array(z.any()).optional(),
  severity: z.enum(['block', 'warn', 'info']).default('block')
});

/**
 * Check if IRI belongs to protected namespace
 * @param {string} iri - IRI to check
 * @returns {boolean} True if protected
 */
export function isProtectedNamespace(iri) {
  for (const protectedNS of PROTECTED_NAMESPACES) {
    if (iri.startsWith(protectedNS)) {
      return true;
    }
  }
  return false;
}

/**
 * Check if term is part of corporate canon
 * @param {string} iri - Full IRI to check
 * @returns {boolean} True if canonical
 */
export function isCanonicalTerm(iri) {
  for (const [namespace, terms] of Object.entries(CORPORATE_CANON)) {
    if (iri.startsWith(namespace)) {
      const localName = iri.substring(namespace.length);
      return terms.has(localName);
    }
  }
  return false;
}

/**
 * Extract namespace from IRI
 * @param {string} iri - IRI to process
 * @returns {string} Namespace portion
 */
function extractNamespace(iri) {
  try {
    const url = new URL(iri);
    const lastSlash = url.pathname.lastIndexOf('/');
    const lastHash = url.pathname.lastIndexOf('#');
    const separator = Math.max(lastSlash, lastHash);

    if (separator >= 0) {
      return iri.substring(0, iri.indexOf(url.pathname) + separator + 1);
    }
    return `${url.protocol}//${url.host}/`;
  } catch {
    // Fallback for malformed IRIs
    const hashIndex = iri.lastIndexOf('#');
    const slashIndex = iri.lastIndexOf('/');
    const separator = Math.max(hashIndex, slashIndex);
    return separator >= 0 ? iri.substring(0, separator + 1) : iri;
  }
}

/**
 * Guard: EditIndustrialSubstrate
 * Prevents modifications to protected namespaces
 *
 * @param {DeltaCapsule} capsule - Capsule to check
 * @returns {Object} Guard result
 */
export function guardEditIndustrialSubstrate(capsule) {
  const violations = [];

  for (const change of capsule.changes) {
    // Only check delete/update operations
    if (change.operation === 'delete' || change.operation === 'update') {
      for (const quad of change.quads) {
        // Check if any term uses protected namespace
        const terms = [
          quad.subject.value,
          quad.predicate.value,
          quad.object.value
        ];

        for (const term of terms) {
          if (typeof term === 'string' && isProtectedNamespace(term)) {
            violations.push({
              quad,
              term,
              operation: change.operation
            });
          }
        }
      }
    }
  }

  if (violations.length > 0) {
    return {
      allowed: false,
      guardName: 'EditIndustrialSubstrate',
      reason: `Attempted to ${violations[0].operation} protected industrial substrate. ` +
              `Protected namespaces cannot be modified. Violations: ${violations.length}`,
      violatingQuads: violations.map(v => v.quad),
      severity: 'block'
    };
  }

  return {
    allowed: true,
    guardName: 'EditIndustrialSubstrate',
    reason: 'No modifications to protected substrate detected',
    severity: 'info'
  };
}

/**
 * Guard: RedefineProtectedTerm
 * Prevents redefinition of canonical vocabulary terms
 *
 * @param {DeltaCapsule} capsule - Capsule to check
 * @returns {Object} Guard result
 */
export function guardRedefineProtectedTerm(capsule) {
  const violations = [];
  const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';

  for (const change of capsule.changes) {
    // Check all operations - even 'add' can try to redefine
    for (const quad of change.quads) {
      // Check if trying to redefine a canonical term's type
      if (quad.predicate.value === RDF_TYPE) {
        const subject = quad.subject.value;

        if (typeof subject === 'string' && isCanonicalTerm(subject)) {
          violations.push({
            quad,
            term: subject,
            operation: change.operation
          });
        }
      }

      // Check if trying to modify properties of canonical terms
      const subject = quad.subject.value;
      if (typeof subject === 'string' && isCanonicalTerm(subject)) {
        // Allow only safe metadata predicates
        const SAFE_PREDICATES = new Set([
          'http://www.w3.org/2000/01/rdf-schema#label',
          'http://www.w3.org/2000/01/rdf-schema#comment',
          'http://www.w3.org/2004/02/skos/core#note',
          'http://purl.org/dc/terms/description'
        ]);

        if (!SAFE_PREDICATES.has(quad.predicate.value)) {
          violations.push({
            quad,
            term: subject,
            operation: change.operation,
            reason: 'Attempted to modify structural property of canonical term'
          });
        }
      }
    }
  }

  if (violations.length > 0) {
    return {
      allowed: false,
      guardName: 'RedefineProtectedTerm',
      reason: `Attempted to redefine protected canonical terms. ` +
              `Canonical vocabulary terms cannot be redefined. Violations: ${violations.length}`,
      violatingQuads: violations.map(v => v.quad),
      severity: 'block'
    };
  }

  return {
    allowed: true,
    guardName: 'RedefineProtectedTerm',
    reason: 'No redefinition of protected terms detected',
    severity: 'info'
  };
}

/**
 * Guard: WeakenCorporateCanon
 * Prevents violations of canonical type assignments
 *
 * @param {DeltaCapsule} capsule - Capsule to check
 * @returns {Object} Guard result
 */
export function guardWeakenCorporateCanon(capsule) {
  const violations = [];
  const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';

  for (const change of capsule.changes) {
    // Only deletions can weaken the canon
    if (change.operation === 'delete') {
      for (const quad of change.quads) {
        // Check if deleting a type assertion for a canonical term
        if (quad.predicate.value === RDF_TYPE) {
          const subject = quad.subject.value;
          const object = quad.object.value;

          // Check if either subject or object is canonical
          if (
            (typeof subject === 'string' && isCanonicalTerm(subject)) ||
            (typeof object === 'string' && isCanonicalTerm(object))
          ) {
            violations.push({
              quad,
              subject,
              object,
              operation: change.operation
            });
          }
        }

        // Check if deleting structural predicates
        const STRUCTURAL_PREDICATES = new Set([
          'http://www.w3.org/2000/01/rdf-schema#subClassOf',
          'http://www.w3.org/2000/01/rdf-schema#subPropertyOf',
          'http://www.w3.org/2000/01/rdf-schema#domain',
          'http://www.w3.org/2000/01/rdf-schema#range',
          'http://www.w3.org/2002/07/owl#equivalentClass',
          'http://www.w3.org/2002/07/owl#equivalentProperty'
        ]);

        if (STRUCTURAL_PREDICATES.has(quad.predicate.value)) {
          const subject = quad.subject.value;
          if (typeof subject === 'string' && isCanonicalTerm(subject)) {
            violations.push({
              quad,
              predicate: quad.predicate.value,
              operation: change.operation,
              reason: 'Attempted to delete structural relationship of canonical term'
            });
          }
        }
      }
    }
  }

  if (violations.length > 0) {
    return {
      allowed: false,
      guardName: 'WeakenCorporateCanon',
      reason: `Attempted to weaken corporate canon by deleting canonical type assertions. ` +
              `Canon cannot be weakened. Violations: ${violations.length}`,
      violatingQuads: violations.map(v => v.quad),
      severity: 'block'
    };
  }

  return {
    allowed: true,
    guardName: 'WeakenCorporateCanon',
    reason: 'No weakening of corporate canon detected',
    severity: 'info'
  };
}

/**
 * Run all forbidden operation guards
 * @param {DeltaCapsule} capsule - Capsule to check
 * @returns {Object} Combined guard results
 */
export function checkForbiddenOperations(capsule) {
  const guards = [
    guardEditIndustrialSubstrate,
    guardRedefineProtectedTerm,
    guardWeakenCorporateCanon
  ];

  const results = guards.map(guard => guard(capsule));

  const blocked = results.filter(r => !r.allowed && r.severity === 'block');

  return {
    allowed: blocked.length === 0,
    guards: results,
    blockedCount: blocked.length,
    blockedBy: blocked.map(r => r.guardName),
    summary: blocked.length > 0
      ? `Blocked by ${blocked.length} guard(s): ${blocked.map(r => r.guardName).join(', ')}`
      : 'All guards passed'
  };
}

/**
 * Check if partition is protected
 * @param {Object} partition - Partition configuration
 * @returns {boolean} True if protected
 */
export function isProtectedPartition(partition) {
  return partition.protected === true ||
         isProtectedNamespace(partition.namespace);
}

/**
 * Get all protected namespaces
 * @returns {Array<string>} Array of protected namespace IRIs
 */
export function getProtectedNamespaces() {
  return Array.from(PROTECTED_NAMESPACES);
}

/**
 * Get all canonical terms
 * @returns {Array<string>} Array of canonical term IRIs
 */
export function getCanonicalTerms() {
  const terms = [];
  for (const [namespace, termSet] of Object.entries(CORPORATE_CANON)) {
    for (const term of termSet) {
      terms.push(`${namespace}${term}`);
    }
  }
  return terms;
}
