/**
 * @file CTAN Package Mapping
 * @module kgc-cli/lib/latex/cache/ctan-map
 *
 * @description
 * Maps LaTeX input files (.sty, .cls, etc.) to CTAN package URLs.
 * Provides deterministic URL resolution for package fetching.
 */

import { z } from 'zod';

// ============================================================================
// VALIDATION SCHEMAS
// ============================================================================

const FilenameMappingSchema = z.object({
  filename: z.string().min(1),
  mirror: z.string().url().optional(),
});

// ============================================================================
// CTAN PATH TEMPLATES
// ============================================================================

/**
 * CTAN package location templates by file extension.
 * The resolver tries each template in order until successful.
 *
 * Template variables:
 * - {package}: Package name (extracted from filename)
 * - {file}: Original filename
 *
 * @type {Record<string, string[]>}
 */
export const CTAN_PATH_TEMPLATES = {
  '.sty': [
    'macros/latex/contrib/{package}',
    'macros/latex/required/{package}',
    'macros/latex/base',
    'macros/latex/exptl/{package}',
  ],
  '.cls': [
    'macros/latex/contrib/{package}',
    'macros/latex/base',
    'macros/latex/exptl/{package}',
  ],
  '.def': [
    'macros/latex/contrib/{package}',
    'macros/latex/required/{package}',
    'macros/latex/base',
  ],
  '.bib': [
    'biblio/bibtex/contrib/{package}',
    'biblio/bibtex/base',
  ],
  '.bst': [
    'biblio/bibtex/bst/{package}',
    'biblio/bibtex/bst/base',
  ],
  '.bbx': [
    'macros/latex/contrib/biblatex/bbx',
  ],
  '.cbx': [
    'macros/latex/contrib/biblatex/cbx',
  ],
};

/**
 * VFS (Virtual File System) path templates by file extension.
 * Determines where resolved files are placed in the VFS.
 *
 * @type {Record<string, string>}
 */
export const VFS_PATH_TEMPLATES = {
  '.sty': 'texmf/tex/latex/{package}/{file}',
  '.cls': 'texmf/tex/latex/{package}/{file}',
  '.def': 'texmf/tex/latex/{package}/{file}',
  '.bib': 'texmf/bibtex/bib/{package}/{file}',
  '.bst': 'texmf/bibtex/bst/{package}/{file}',
  '.bbx': 'texmf/tex/latex/biblatex/bbx/{file}',
  '.cbx': 'texmf/tex/latex/biblatex/cbx/{file}',
  '.fd': 'texmf/tex/latex/{package}/{file}',
  '.tfm': 'texmf/fonts/tfm/{package}/{file}',
  '.afm': 'texmf/fonts/afm/{package}/{file}',
  '.pfb': 'texmf/fonts/type1/{package}/{file}',
  '.ttf': 'texmf/fonts/truetype/{package}/{file}',
  '.otf': 'texmf/fonts/opentype/{package}/{file}',
};

/**
 * Known package name exceptions where filename != package name
 * Maps filename to actual CTAN package name
 *
 * @type {Record<string, string>}
 */
export const PACKAGE_NAME_EXCEPTIONS = {
  // Algorithm packages
  'algorithm2e.sty': 'algorithm2e',
  'algpseudocode.sty': 'algorithmicx',
  'algorithmic.sty': 'algorithms',

  // Graphics
  'tikz.sty': 'pgf',
  'pgfplots.sty': 'pgfplots',
  'graphicx.sty': 'graphics',
  'color.sty': 'graphics',

  // Bibliography
  'natbib.sty': 'natbib',
  'biblatex.sty': 'biblatex',

  // Fonts
  'fontenc.sty': 'latex-base',
  'inputenc.sty': 'latex-base',

  // Hyperref
  'hyperref.sty': 'hyperref',
  'url.sty': 'url',

  // AMS packages
  'amsmath.sty': 'amsmath',
  'amsthm.sty': 'amscls',
  'amsfonts.sty': 'amsfonts',
  'amssymb.sty': 'amsfonts',

  // Beamer
  'beamer.cls': 'beamer',
  'beamerarticle.sty': 'beamer',

  // Common utilities
  'geometry.sty': 'geometry',
  'fancyhdr.sty': 'fancyhdr',
  'xcolor.sty': 'xcolor',
  'listings.sty': 'listings',
  'enumitem.sty': 'enumitem',
  'caption.sty': 'caption',
  'subcaption.sty': 'caption',
};

/**
 * Default CTAN mirror URL
 * @type {string}
 */
export const DEFAULT_CTAN_MIRROR = 'https://mirrors.ctan.org';

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Extract file extension from filename
 * @param {string} filename - Input filename
 * @returns {string} Extension including dot (e.g., '.sty') or empty string
 *
 * @example
 * getExtension('algorithm2e.sty') // => '.sty'
 * getExtension('beamer.cls')      // => '.cls'
 */
export function getExtension(filename) {
  const match = filename.match(/\.(sty|cls|def|bib|bst|bbx|cbx|fd|tfm|afm|pfb|ttf|otf)$/);
  return match ? match[0] : '';
}

/**
 * Extract package name from filename.
 * Checks PACKAGE_NAME_EXCEPTIONS first, then removes extension.
 *
 * @param {string} filename - Input filename (e.g., 'algorithm2e.sty')
 * @returns {string} Package name (e.g., 'algorithm2e')
 *
 * @example
 * extractPackageName('algorithm2e.sty')    // => 'algorithm2e'
 * extractPackageName('algpseudocode.sty')  // => 'algorithmicx' (from exceptions)
 * extractPackageName('tikz.sty')           // => 'pgf' (from exceptions)
 */
export function extractPackageName(filename) {
  // Check exceptions first
  if (PACKAGE_NAME_EXCEPTIONS[filename]) {
    return PACKAGE_NAME_EXCEPTIONS[filename];
  }

  // Default: strip extension
  const ext = getExtension(filename);
  if (ext) {
    return filename.slice(0, -ext.length);
  }

  return filename;
}

/**
 * Build VFS path for a resolved file
 * @param {string} filename - Input filename
 * @returns {string} VFS path (e.g., 'texmf/tex/latex/algorithm2e/algorithm2e.sty')
 *
 * @example
 * buildVfsPath('algorithm2e.sty')
 * // => 'texmf/tex/latex/algorithm2e/algorithm2e.sty'
 *
 * buildVfsPath('beamer.cls')
 * // => 'texmf/tex/latex/beamer/beamer.cls'
 */
export function buildVfsPath(filename) {
  const ext = getExtension(filename);
  const packageName = extractPackageName(filename);
  const template = VFS_PATH_TEMPLATES[ext] || 'work/{file}';

  return template
    .replace('{package}', packageName)
    .replace('{file}', filename);
}

/**
 * Build candidate CTAN URLs for a given filename.
 * Returns URLs in priority order (most likely first).
 *
 * @param {string} filename - Input filename
 * @param {string} [mirror] - CTAN mirror URL (default: DEFAULT_CTAN_MIRROR)
 * @returns {string[]} Candidate URLs to try
 *
 * @example
 * buildCtanUrls('algorithm2e.sty')
 * // => [
 * //   'https://mirrors.ctan.org/macros/latex/contrib/algorithm2e/algorithm2e.sty',
 * //   'https://mirrors.ctan.org/macros/latex/required/algorithm2e/algorithm2e.sty',
 * //   'https://mirrors.ctan.org/macros/latex/base/algorithm2e.sty',
 * // ]
 */
export function buildCtanUrls(filename, mirror = DEFAULT_CTAN_MIRROR) {
  // Validate inputs
  FilenameMappingSchema.parse({ filename, mirror });

  const ext = getExtension(filename);
  const packageName = extractPackageName(filename);
  const templates = CTAN_PATH_TEMPLATES[ext] || [];

  return templates.map(template => {
    const path = template.replace('{package}', packageName);
    return `${mirror}/${path}/${filename}`;
  });
}

/**
 * Check if URL is a local fixture server (for testing)
 * @param {string} url - URL to check
 * @returns {boolean} True if local (file:// or http://localhost)
 *
 * @example
 * isLocalFixture('file:///tmp/fixtures/test.sty')     // => true
 * isLocalFixture('http://localhost:3000/test.sty')    // => true
 * isLocalFixture('https://mirrors.ctan.org/test.sty') // => false
 */
export function isLocalFixture(url) {
  return url.startsWith('file://') || url.includes('localhost');
}

/**
 * Get package metadata from filename
 * @param {string} filename - Input filename
 * @returns {Object} Package metadata
 *
 * @example
 * getPackageMetadata('algorithm2e.sty')
 * // => {
 * //   filename: 'algorithm2e.sty',
 * //   package: 'algorithm2e',
 * //   extension: '.sty',
 * //   vfsPath: 'texmf/tex/latex/algorithm2e/algorithm2e.sty'
 * // }
 */
export function getPackageMetadata(filename) {
  return {
    filename,
    package: extractPackageName(filename),
    extension: getExtension(filename),
    vfsPath: buildVfsPath(filename),
  };
}

// ============================================================================
// EXPORTS SUMMARY
// ============================================================================

/**
 * Module exports:
 * - CTAN_PATH_TEMPLATES: Path templates for CTAN packages
 * - VFS_PATH_TEMPLATES: VFS path templates by file type
 * - PACKAGE_NAME_EXCEPTIONS: Known filename→package mappings
 * - DEFAULT_CTAN_MIRROR: Default CTAN mirror URL
 * - getExtension(filename): Extract file extension
 * - extractPackageName(filename): Get package name (checks exceptions)
 * - buildVfsPath(filename): Build VFS path for file
 * - buildCtanUrls(filename, mirror?): Build candidate CTAN URLs
 * - isLocalFixture(url): Check if URL is local fixture
 * - getPackageMetadata(filename): Get package metadata
 */
