/**
 * @file Diátaxis schema definitions and validation
 * @module diataxis-schema
 */

import { hashString } from './hash.mjs';

/**
 * @typedef {Object} DiataxisConfidence
 * @property {number} tutorials - Confidence score 0-1
 * @property {number} howtos - Confidence score 0-1
 * @property {number} reference - Confidence score 0-1
 * @property {number} explanation - Confidence score 0-1
 */

/**
 * @typedef {Object} Tutorial
 * @property {string} id - Generated from title
 * @property {string} title - Tutorial title
 * @property {string} goal - Learning goal
 * @property {string[]} prerequisites - Required knowledge
 * @property {string[]} stepsOutline - Step-by-step outline
 * @property {number} confidenceScore - 0-1 confidence
 * @property {string[]} source - Evidence sources
 */

/**
 * @typedef {Object} HowTo
 * @property {string} id - Generated from title
 * @property {string} title - How-to title
 * @property {string} task - Task description
 * @property {string} context - When to use this
 * @property {string[]} steps - Action steps
 * @property {number} confidenceScore - 0-1 confidence
 * @property {string[]} source - Evidence sources
 */

/**
 * @typedef {Object} ReferenceItem
 * @property {string} name - Item name
 * @property {'export'|'bin'|'env'|'option'|'unknown'} type - Reference type
 * @property {string} description - Item description
 * @property {string|null} example - Usage example
 */

/**
 * @typedef {Object} Reference
 * @property {string} id - Generated identifier
 * @property {string} title - Reference title
 * @property {ReferenceItem[]} items - Reference items
 * @property {number} confidenceScore - 0-1 confidence
 * @property {string[]} source - Evidence sources
 */

/**
 * @typedef {Object} Explanation
 * @property {string} id - Generated identifier
 * @property {string} title - Explanation title
 * @property {string[]} concepts - Key concepts
 * @property {string} architecture - Architecture overview
 * @property {string[]} tradeoffs - Design tradeoffs
 * @property {number} confidenceScore - 0-1 confidence
 * @property {string[]} source - Evidence sources
 */

/**
 * @typedef {Object} DiataxisEvidence
 * @property {string[]} readmeHeadings - Headings from README
 * @property {string[]} docsFiles - Documentation files found
 * @property {string[]} examplesFiles - Example files found
 * @property {string} fingerprint - Content fingerprint hash
 */

/**
 * @typedef {Object} DiataxisEntry
 * @property {string} packageName - Package name
 * @property {string} version - Package version
 * @property {string} generatedAt - ISO-8601 timestamp
 * @property {DiataxisConfidence} confidence - Confidence scores
 * @property {Tutorial[]} tutorials - Tutorial entries
 * @property {HowTo[]} howtos - How-to entries
 * @property {Reference} reference - Reference documentation
 * @property {Explanation} explanation - Explanation documentation
 * @property {DiataxisEvidence} evidence - Evidence metadata
 */

/**
 * @typedef {Object} ValidationResult
 * @property {boolean} valid - Whether entry is valid
 * @property {string[]} errors - Validation errors
 */

/**
 * Generate a stable ID from a title string
 * @param {string} title - Title to convert
 * @returns {string} Kebab-case ID
 */
function generateId(title) {
  return title
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '');
}

/**
 * Get current timestamp, respecting DETERMINISTIC mode
 * @returns {string} ISO-8601 timestamp
 */
function getTimestamp() {
  if (process.env.DETERMINISTIC === '1') {
    return '2000-01-01T00:00:00.000Z';
  }
  return new Date().toISOString();
}

/**
 * Create a validated Diátaxis entry
 * @param {string} packageName - Package name
 * @param {string} version - Package version
 * @param {Object} evidence - Evidence object
 * @param {string[]} [evidence.readmeHeadings=[]] - README headings
 * @param {string[]} [evidence.docsFiles=[]] - Docs files
 * @param {string[]} [evidence.examplesFiles=[]] - Example files
 * @param {Tutorial[]} [evidence.tutorials=[]] - Tutorial entries
 * @param {HowTo[]} [evidence.howtos=[]] - How-to entries
 * @param {Reference} [evidence.reference] - Reference entry
 * @param {Explanation} [evidence.explanation] - Explanation entry
 * @param {DiataxisConfidence} [evidence.confidence] - Confidence scores
 * @returns {DiataxisEntry} Validated entry
 */
export function createDiataxisEntry(packageName, version, evidence = {}) {
  const {
    readmeHeadings = [],
    docsFiles = [],
    examplesFiles = [],
    tutorials = [],
    howtos = [],
    reference = null,
    explanation = null,
    confidence = {
      tutorials: 0,
      howtos: 0,
      reference: 0,
      explanation: 0
    }
  } = evidence;

  // Generate fingerprint from evidence
  const fingerprintInput = JSON.stringify({
    readmeHeadings: [...readmeHeadings].sort(),
    docsFiles: [...docsFiles].sort(),
    examplesFiles: [...examplesFiles].sort()
  });
  const fingerprint = hashString(fingerprintInput);

  // Ensure IDs on all items
  const tutorialsWithIds = tutorials.map(t => ({
    ...t,
    id: t.id || generateId(t.title)
  }));

  const howtosWithIds = howtos.map(h => ({
    ...h,
    id: h.id || generateId(h.title)
  }));

  const referenceWithId = reference ? {
    ...reference,
    id: reference.id || generateId(reference.title)
  } : {
    id: 'reference',
    title: `${packageName} Reference`,
    items: [],
    confidenceScore: 0,
    source: []
  };

  const explanationWithId = explanation ? {
    ...explanation,
    id: explanation.id || generateId(explanation.title)
  } : {
    id: 'explanation',
    title: `${packageName} Explanation`,
    concepts: [],
    architecture: '',
    tradeoffs: [],
    confidenceScore: 0,
    source: []
  };

  /** @type {DiataxisEntry} */
  const entry = {
    packageName,
    version,
    generatedAt: getTimestamp(),
    confidence: {
      tutorials: confidence.tutorials ?? 0,
      howtos: confidence.howtos ?? 0,
      reference: confidence.reference ?? 0,
      explanation: confidence.explanation ?? 0
    },
    tutorials: tutorialsWithIds,
    howtos: howtosWithIds,
    reference: referenceWithId,
    explanation: explanationWithId,
    evidence: {
      readmeHeadings,
      docsFiles,
      examplesFiles,
      fingerprint
    }
  };

  return entry;
}

/**
 * Validate a Diátaxis entry
 * @param {any} entry - Entry to validate
 * @returns {ValidationResult} Validation result
 */
export function validateDiataxisEntry(entry) {
  /** @type {string[]} */
  const errors = [];

  // Check required top-level fields
  if (!entry || typeof entry !== 'object') {
    errors.push('Entry must be an object');
    return { valid: false, errors };
  }

  if (typeof entry.packageName !== 'string' || entry.packageName === '') {
    errors.push('packageName must be a non-empty string');
  }

  if (typeof entry.version !== 'string' || entry.version === '') {
    errors.push('version must be a non-empty string');
  }

  if (typeof entry.generatedAt !== 'string') {
    errors.push('generatedAt must be an ISO-8601 string');
  }

  // Validate confidence
  if (!entry.confidence || typeof entry.confidence !== 'object') {
    errors.push('confidence must be an object');
  } else {
    const confidenceKeys = ['tutorials', 'howtos', 'reference', 'explanation'];
    for (const key of confidenceKeys) {
      const val = entry.confidence[key];
      if (typeof val !== 'number' || val < 0 || val > 1) {
        errors.push(`confidence.${key} must be a number between 0 and 1`);
      }
    }
  }

  // Validate tutorials
  if (!Array.isArray(entry.tutorials)) {
    errors.push('tutorials must be an array');
  } else {
    entry.tutorials.forEach((t, i) => {
      if (!t.id || typeof t.id !== 'string') {
        errors.push(`tutorials[${i}].id must be a string`);
      }
      if (!t.title || typeof t.title !== 'string') {
        errors.push(`tutorials[${i}].title must be a string`);
      }
      if (typeof t.confidenceScore !== 'number') {
        errors.push(`tutorials[${i}].confidenceScore must be a number`);
      }
      if (!Array.isArray(t.source)) {
        errors.push(`tutorials[${i}].source must be an array`);
      }
    });
  }

  // Validate howtos
  if (!Array.isArray(entry.howtos)) {
    errors.push('howtos must be an array');
  } else {
    entry.howtos.forEach((h, i) => {
      if (!h.id || typeof h.id !== 'string') {
        errors.push(`howtos[${i}].id must be a string`);
      }
      if (!h.title || typeof h.title !== 'string') {
        errors.push(`howtos[${i}].title must be a string`);
      }
      if (typeof h.confidenceScore !== 'number') {
        errors.push(`howtos[${i}].confidenceScore must be a number`);
      }
      if (!Array.isArray(h.source)) {
        errors.push(`howtos[${i}].source must be an array`);
      }
    });
  }

  // Validate reference
  if (!entry.reference || typeof entry.reference !== 'object') {
    errors.push('reference must be an object');
  } else {
    if (!entry.reference.id || typeof entry.reference.id !== 'string') {
      errors.push('reference.id must be a string');
    }
    if (!entry.reference.title || typeof entry.reference.title !== 'string') {
      errors.push('reference.title must be a string');
    }
    if (!Array.isArray(entry.reference.items)) {
      errors.push('reference.items must be an array');
    }
    if (typeof entry.reference.confidenceScore !== 'number') {
      errors.push('reference.confidenceScore must be a number');
    }
    if (!Array.isArray(entry.reference.source)) {
      errors.push('reference.source must be an array');
    }
  }

  // Validate explanation
  if (!entry.explanation || typeof entry.explanation !== 'object') {
    errors.push('explanation must be an object');
  } else {
    if (!entry.explanation.id || typeof entry.explanation.id !== 'string') {
      errors.push('explanation.id must be a string');
    }
    if (!entry.explanation.title || typeof entry.explanation.title !== 'string') {
      errors.push('explanation.title must be a string');
    }
    if (!Array.isArray(entry.explanation.concepts)) {
      errors.push('explanation.concepts must be an array');
    }
    if (typeof entry.explanation.architecture !== 'string') {
      errors.push('explanation.architecture must be a string');
    }
    if (!Array.isArray(entry.explanation.tradeoffs)) {
      errors.push('explanation.tradeoffs must be an array');
    }
    if (typeof entry.explanation.confidenceScore !== 'number') {
      errors.push('explanation.confidenceScore must be a number');
    }
    if (!Array.isArray(entry.explanation.source)) {
      errors.push('explanation.source must be an array');
    }
  }

  // Validate evidence
  if (!entry.evidence || typeof entry.evidence !== 'object') {
    errors.push('evidence must be an object');
  } else {
    if (!Array.isArray(entry.evidence.readmeHeadings)) {
      errors.push('evidence.readmeHeadings must be an array');
    }
    if (!Array.isArray(entry.evidence.docsFiles)) {
      errors.push('evidence.docsFiles must be an array');
    }
    if (!Array.isArray(entry.evidence.examplesFiles)) {
      errors.push('evidence.examplesFiles must be an array');
    }
    if (typeof entry.evidence.fingerprint !== 'string') {
      errors.push('evidence.fingerprint must be a string');
    }
  }

  return {
    valid: errors.length === 0,
    errors
  };
}

/**
 * Ensure entry has minimum Diátaxis structure (empty stubs if needed)
 * @param {DiataxisEntry} entry - Entry to ensure
 * @returns {DiataxisEntry} Entry with guaranteed structure
 */
export function ensureMinimumDiataxis(entry) {
  // Ensure tutorials array exists
  if (!Array.isArray(entry.tutorials) || entry.tutorials.length === 0) {
    entry.tutorials = [];
    entry.confidence.tutorials = 0;
  }

  // Ensure howtos array exists
  if (!Array.isArray(entry.howtos) || entry.howtos.length === 0) {
    entry.howtos = [];
    entry.confidence.howtos = 0;
  }

  // Ensure reference exists
  if (!entry.reference || typeof entry.reference !== 'object') {
    entry.reference = {
      id: 'reference',
      title: `${entry.packageName} Reference`,
      items: [],
      confidenceScore: 0,
      source: []
    };
    entry.confidence.reference = 0;
  } else if (!Array.isArray(entry.reference.items) || entry.reference.items.length === 0) {
    entry.reference.items = [];
    entry.reference.confidenceScore = 0;
    entry.confidence.reference = 0;
  }

  // Ensure explanation exists
  if (!entry.explanation || typeof entry.explanation !== 'object') {
    entry.explanation = {
      id: 'explanation',
      title: `${entry.packageName} Explanation`,
      concepts: [],
      architecture: '',
      tradeoffs: [],
      confidenceScore: 0,
      source: []
    };
    entry.confidence.explanation = 0;
  } else if (
    (!Array.isArray(entry.explanation.concepts) || entry.explanation.concepts.length === 0) &&
    (!entry.explanation.architecture || entry.explanation.architecture === '') &&
    (!Array.isArray(entry.explanation.tradeoffs) || entry.explanation.tradeoffs.length === 0)
  ) {
    entry.explanation.confidenceScore = 0;
    entry.confidence.explanation = 0;
  }

  return entry;
}
