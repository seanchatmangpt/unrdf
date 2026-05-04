/**
 * @fileoverview Reference Extractor - Extract API surface from package metadata and README
 * @module reference-extractor
 *
 * This module extracts API reference information from:
 * - package.json exports field
 * - package.json bin field
 * - README API sections
 *
 * Used by classify.mjs to generate reference documentation structure.
 */

/**
 * @typedef {Object} ReferenceItem
 * @property {string} name - The API item name (export path, bin name, etc)
 * @property {string} type - Item type: "export", "bin", "api", "unknown"
 * @property {string} description - Description of the item
 * @property {string|null} example - Optional usage example
 */

/**
 * @typedef {Object} Reference
 * @property {string} id - Always "reference"
 * @property {string} title - Reference title (e.g., "[PackageName] API Reference")
 * @property {ReferenceItem[]} items - List of reference items
 * @property {number} confidenceScore - Confidence score 0-1
 * @property {string[]} source - Sources that contributed data
 */

/**
 * Extract API reference from package metadata and README
 *
 * @param {Object} packageEntry - Package.json data
 * @param {Object} evidenceSnapshot - Contains README and other evidence
 * @param {string} [evidenceSnapshot.readme] - README content
 * @returns {Reference} Reference object with API items
 *
 * @example
 * const ref = extractReference(pkg, { readme: '# API\n...' });
 * // => { id: 'reference', title: 'pkg-name API Reference', items: [...], ... }
 */
export function extractReference(packageEntry, evidenceSnapshot) {
  const items = [];
  const sources = [];

  // Extract from package.json exports field
  const exportItems = extractFromExports(packageEntry);
  if (exportItems.length > 0) {
    items.push(...exportItems);
    sources.push('exports');
  }

  // Extract from package.json bin field
  const binItems = extractFromBin(packageEntry);
  if (binItems.length > 0) {
    items.push(...binItems);
    sources.push('bin');
  }

  // Extract from README API sections
  const readmeItems = extractFromReadme(evidenceSnapshot.readme || '');
  if (readmeItems.length > 0) {
    items.push(...readmeItems);
    sources.push('readme');
  }

  // Fallback if nothing found
  if (items.length === 0) {
    items.push({
      name: 'unknown',
      type: 'unknown',
      description: 'API reference not found in documentation',
      example: null
    });
    sources.push('inferred');
  }

  // Sort items by name for deterministic output
  items.sort((a, b) => a.name.localeCompare(b.name));

  // Calculate confidence score
  const confidenceScore = calculateConfidence(sources);

  return {
    id: 'reference',
    title: `${packageEntry.name || 'Package'} API Reference`,
    items,
    confidenceScore,
    source: sources
  };
}

/**
 * Extract reference items from package.json exports field
 *
 * @param {Object} packageEntry - Package.json data
 * @returns {ReferenceItem[]} Array of reference items from exports
 *
 * @example
 * extractFromExports({ exports: { '.': './index.js', './utils': './utils.js' } })
 * // => [{ name: '.', type: 'export', description: './index.js', example: null }, ...]
 */
function extractFromExports(packageEntry) {
  const items = [];

  if (!packageEntry.exports) {
    return items;
  }

  const exports = packageEntry.exports;

  // Handle string exports (single entry point)
  if (typeof exports === 'string') {
    items.push({
      name: '.',
      type: 'export',
      description: exports,
      example: null
    });
    return items;
  }

  // Handle object exports
  if (typeof exports === 'object' && exports !== null) {
    for (const [exportPath, value] of Object.entries(exports)) {
      // Handle conditional exports (import/require)
      let description = '';
      if (typeof value === 'string') {
        description = value;
      } else if (typeof value === 'object' && value !== null) {
        // Use import condition first, fallback to require, then default
        description = value.import || value.require || value.default || JSON.stringify(value);
      }

      items.push({
        name: exportPath,
        type: 'export',
        description: String(description),
        example: null
      });
    }
  }

  return items;
}

/**
 * Extract reference items from package.json bin field
 *
 * @param {Object} packageEntry - Package.json data
 * @returns {ReferenceItem[]} Array of reference items from bin
 *
 * @example
 * extractFromBin({ bin: 'cli.js' })
 * // => [{ name: 'package-name', type: 'bin', description: 'CLI entry point: cli.js', example: null }]
 */
function extractFromBin(packageEntry) {
  const items = [];

  if (!packageEntry.bin) {
    return items;
  }

  const bin = packageEntry.bin;

  // Handle string bin (single CLI entry point)
  if (typeof bin === 'string') {
    items.push({
      name: packageEntry.name || 'cli',
      type: 'bin',
      description: `CLI entry point: ${bin}`,
      example: null
    });
    return items;
  }

  // Handle object bin (multiple CLI commands)
  if (typeof bin === 'object' && bin !== null) {
    for (const [binName, binPath] of Object.entries(bin)) {
      items.push({
        name: binName,
        type: 'bin',
        description: `CLI entry point: ${binPath}`,
        example: null
      });
    }
  }

  return items;
}

/**
 * Extract reference items from README API sections
 *
 * Looks for headings like "API", "API Reference", "Exports", "CLI Options", "Options"
 * and extracts the content following these headings.
 *
 * @param {string} readme - README content
 * @returns {ReferenceItem[]} Array of reference items from README
 *
 * @example
 * extractFromReadme('## API\n\n- foo: Does something\n- bar: Does another thing')
 * // => [{ name: 'foo', type: 'api', description: 'Does something', ... }, ...]
 */
function extractFromReadme(readme) {
  const items = [];

  if (!readme || typeof readme !== 'string') {
    return items;
  }

  // API section heading patterns (case-insensitive)
  const apiHeadingPatterns = [
    /^#{1,6}\s+API\s*$/im,
    /^#{1,6}\s+API\s+Reference\s*$/im,
    /^#{1,6}\s+Exports\s*$/im,
    /^#{1,6}\s+CLI\s+Options\s*$/im,
    /^#{1,6}\s+Options\s*$/im,
    /^#{1,6}\s+Usage\s*$/im,
    /^#{1,6}\s+Methods\s*$/im
  ];

  for (const pattern of apiHeadingPatterns) {
    const match = readme.match(pattern);
    if (!match) {
      continue;
    }

    // Extract content after heading (next 500 chars or until next heading)
    const startIndex = match.index + match[0].length;
    const remainingContent = readme.slice(startIndex);

    // Find next heading or take 500 chars
    const nextHeadingMatch = remainingContent.match(/^#{1,6}\s+/m);
    const endIndex = nextHeadingMatch ? nextHeadingMatch.index : 500;
    const section = remainingContent.slice(0, endIndex);

    // Parse for list items: "- name: description" or "- name - description"
    const listItemPattern = /^[-*]\s+`?([^:`\n-]+)`?\s*[:\-]\s*(.+)$/gm;
    let itemMatch;

    while ((itemMatch = listItemPattern.exec(section)) !== null) {
      const name = itemMatch[1].trim();
      const description = itemMatch[2].trim();

      if (name && description) {
        items.push({
          name,
          type: 'api',
          description,
          example: null
        });
      }
    }

    // If we found items, we can stop searching
    if (items.length > 0) {
      break;
    }

    // If no list items found, create single item from section text
    if (items.length === 0 && section.trim()) {
      const sectionText = section.trim().split('\n')[0]; // First line
      if (sectionText && sectionText.length > 10) {
        items.push({
          name: 'API',
          type: 'api',
          description: sectionText.slice(0, 200),
          example: null
        });
      }
    }
  }

  return items;
}

/**
 * Calculate confidence score based on sources used
 *
 * Confidence levels:
 * - 1.0: exports found
 * - 0.8: bin found (no exports)
 * - 0.6: README API section found (no exports/bin)
 * - 0.3: partial data from multiple sources
 * - 0.1: only "unknown" fallback
 *
 * @param {string[]} sources - Array of source names
 * @returns {number} Confidence score between 0 and 1
 *
 * @example
 * calculateConfidence(['exports', 'readme']) // => 1.0
 * calculateConfidence(['bin']) // => 0.8
 * calculateConfidence(['inferred']) // => 0.1
 */
function calculateConfidence(sources) {
  // Fallback case
  if (sources.includes('inferred')) {
    return 0.1;
  }

  // Exports found (highest confidence)
  if (sources.includes('exports')) {
    return 1.0;
  }

  // Bin found (high confidence for CLI tools)
  if (sources.includes('bin')) {
    return 0.8;
  }

  // README API section found
  if (sources.includes('readme')) {
    return 0.6;
  }

  // Partial data (multiple sources but none of the above)
  if (sources.length > 1) {
    return 0.3;
  }

  // Default low confidence
  return 0.2;
}
