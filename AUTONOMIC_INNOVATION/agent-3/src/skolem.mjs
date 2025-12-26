/**
 * @fileoverview Stable IRI and skolem ID generation
 * @module @unrdf/lens/skolem
 */

import { createHash } from 'crypto';

/**
 * Create deterministic hash of entity data
 * @param {Object} data - Data to hash
 * @returns {string} SHA-256 hash (hex)
 */
function createContentHash(data) {
  // Canonical JSON: sort keys recursively
  const canonical = JSON.stringify(data, Object.keys(data).sort());
  return createHash('sha256').update(canonical, 'utf8').digest('hex');
}

/**
 * Encode IRI component (percent-encode special characters)
 * @param {string} component - Component to encode
 * @returns {string} Encoded component
 */
function encodeIRIComponent(component) {
  // Use encodeURIComponent but preserve some characters valid in IRIs
  return encodeURIComponent(component)
    .replace(/'/g, '%27')
    .replace(/\(/g, '%28')
    .replace(/\)/g, '%29');
}

/**
 * Substitute placeholders in pattern with values
 * @param {string} pattern - Pattern with placeholders like {id}
 * @param {Object} values - Values to substitute
 * @returns {string} Pattern with substitutions
 */
function substitutePlaceholders(pattern, values) {
  let result = pattern;

  // Replace each placeholder
  for (const [key, value] of Object.entries(values)) {
    const placeholder = `{${key}}`;
    if (result.includes(placeholder)) {
      // Don't encode values that are already IRIs (namespace, type, etc.) or special values (hash)
      // Only encode user data values (id, etc.)
      let substitution;
      if (key === 'namespace' || key === 'lensId' || key === 'hash' || key === 'type' ||
          (typeof value === 'string' && (value.startsWith('http://') || value.startsWith('https://') || value.startsWith('urn:') || value.startsWith('sha256:')))) {
        // Already an IRI/IRI component or special value - don't encode
        substitution = String(value);
      } else {
        // User data - encode for safe IRI usage
        substitution = typeof value === 'string' ? encodeIRIComponent(value) : String(value);
      }
      result = result.replace(new RegExp(`\\{${key}\\}`, 'g'), substitution);
    }
  }

  return result;
}

/**
 * Create stable IRI for entity using lens rules
 * @param {Object} entity - Entity data
 * @param {string} lensId - Lens identifier
 * @param {import('./lens.mjs').SubjectRule} rule - Subject generation rule
 * @param {string} namespace - Base namespace
 * @param {string} [entityType] - Entity type (for pattern substitution)
 * @returns {string} Stable IRI
 *
 * @example
 * const entity = { id: "123", name: "Alice" };
 * const rule = { pattern: "{namespace}Customer/{id}", keys: ["id"] };
 * const iri = createStableIRI(entity, "customer-v1", rule, "https://example.org/");
 * // Returns: "https://example.org/Customer/123"
 *
 * @example
 * // Fallback to hash when ID missing
 * const entity = { name: "Bob" };
 * const rule = { pattern: "{namespace}Customer/{hash}", keys: ["id"] };
 * const iri = createStableIRI(entity, "customer-v1", rule, "https://example.org/");
 * // Returns: "https://example.org/Customer/sha256:abc123..."
 */
export function createStableIRI(entity, lensId, rule, namespace, entityType) {
  // Extract key values from entity
  const keyValues = {};
  let allKeysPresent = true;

  for (const key of rule.keys) {
    if (entity[key] !== undefined && entity[key] !== null && entity[key] !== '') {
      keyValues[key] = entity[key];
    } else {
      allKeysPresent = false;
    }
  }

  // Build values for substitution
  const values = {
    namespace,
    lensId,
    type: entityType || 'Entity',
    ...keyValues,
  };

  // If all keys present, use pattern substitution
  if (allKeysPresent) {
    const iri = substitutePlaceholders(rule.pattern, values);

    // Check if all placeholders were replaced
    if (!iri.includes('{')) {
      return iri;
    }
  }

  // Fallback: use content hash
  const hash = createContentHash(entity);
  values.hash = `sha256:${hash}`;

  // Substitute with hash
  let iri = substitutePlaceholders(rule.pattern, values);

  // If {hash} placeholder not in pattern, append it
  if (rule.pattern.includes('{hash}')) {
    return iri;
  } else {
    // Replace any remaining placeholder with hash
    iri = iri.replace(/\{[^}]+\}/g, `sha256:${hash}`);
    return iri;
  }
}

/**
 * Create deterministic skolem ID (blank node alternative)
 * @param {string} skolemPattern - Pattern template (e.g., "urn:skolem:address:{hash}")
 * @param {Object} values - Values for pattern
 * @returns {string} Skolem IRI
 *
 * @example
 * const pattern = "urn:skolem:address:{hash}";
 * const values = { street: "Main St", city: "NYC" };
 * const skolemID = createSkolemID(pattern, values);
 * // Returns: "urn:skolem:address:sha256:def456..."
 */
export function createSkolemID(skolemPattern, values) {
  // Sort values canonically by key
  const sortedKeys = Object.keys(values).sort();
  const canonical = {};
  for (const key of sortedKeys) {
    canonical[key] = values[key];
  }

  // Compute content hash
  const hash = createContentHash(canonical);

  // Substitute into pattern
  const substituted = substitutePlaceholders(skolemPattern, {
    ...values,
    hash: `sha256:${hash}`,
  });

  // Ensure it has skolem prefix
  if (substituted.startsWith('urn:skolem:')) {
    return substituted;
  } else {
    return `urn:skolem:${substituted}`;
  }
}

/**
 * Extract ID from IRI using pattern (reverse operation)
 * @param {string} iri - IRI to extract from
 * @param {string} pattern - Original pattern
 * @param {string} namespace - Base namespace
 * @returns {Object} Extracted values
 *
 * @example
 * const iri = "https://example.org/Customer/123";
 * const pattern = "{namespace}Customer/{id}";
 * const values = extractFromIRI(iri, pattern, "https://example.org/");
 * // Returns: { id: "123" }
 */
export function extractFromIRI(iri, pattern, namespace) {
  // Substitute known values
  let regex = pattern
    .replace(/\{namespace\}/g, namespace.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'))
    .replace(/\{([^}]+)\}/g, '(?<$1>[^/]+)');

  const match = iri.match(new RegExp(`^${regex}$`));

  if (match && match.groups) {
    return match.groups;
  }

  return {};
}
