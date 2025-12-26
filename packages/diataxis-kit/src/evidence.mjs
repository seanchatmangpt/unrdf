/**
 * @file evidence.mjs
 * @description Collect evidence from package files to guide Diátaxis classification
 */

import { readFile, readdir, access } from 'node:fs/promises';
import { join, extname } from 'node:path';
import { createHash } from 'node:crypto';

/**
 * @typedef {Object} EvidenceSnapshot
 * @property {string|null} readmeContent - Full README.md content or null if missing
 * @property {string[]} readmeHeadings - Extracted markdown headings (## level)
 * @property {string[]} examplesFiles - Files in examples/ directory
 * @property {Record<string, string>} examplesSnippets - First 200 chars of sample files
 * @property {string[]} docsFiles - Files in docs/ directory
 * @property {Record<string, string>} docsSnippets - First 200 chars of sample files
 * @property {string[]} srcFiles - Top-level .mjs/.js/.ts files from src/
 * @property {number} testFileCount - Count of test files
 * @property {Record<string, string>} binEntries - bin field from package.json
 * @property {Record<string, string>} exportSurface - exports field from package.json
 * @property {string[]} keywords - keywords from package.json
 * @property {boolean} hasLicense - LICENSE file presence
 * @property {boolean} hasTsConfig - tsconfig.json presence
 * @property {string} fingerprint - SHA256 hash of concatenated evidence
 */

/**
 * Extract markdown headings (## level) from content
 * @param {string} content - Markdown content
 * @returns {string[]} Array of heading text (without ## prefix)
 */
function extractHeadings(content) {
  const headingRegex = /^##\s+(.+)$/gm;
  const headings = [];
  let match;
  while ((match = headingRegex.exec(content)) !== null) {
    headings.push(match[1].trim());
  }
  return headings;
}

/**
 * Read first N chars from a file safely
 * @param {string} filePath - Path to file
 * @param {number} maxChars - Maximum characters to read
 * @returns {Promise<string>} File content snippet
 */
async function readSnippet(filePath, maxChars = 200) {
  try {
    const content = await readFile(filePath, 'utf-8');
    return content.slice(0, maxChars);
  } catch (error) {
    console.warn(`Failed to read ${filePath}: ${error.message}`);
    return '';
  }
}

/**
 * Check if a file exists
 * @param {string} filePath - Path to check
 * @returns {Promise<boolean>} True if file exists
 */
async function fileExists(filePath) {
  try {
    await access(filePath);
    return true;
  } catch {
    return false;
  }
}

/**
 * List files in a directory (non-recursive)
 * @param {string} dirPath - Directory path
 * @returns {Promise<string[]>} Sorted array of filenames
 */
async function listFiles(dirPath) {
  try {
    const entries = await readdir(dirPath, { withFileTypes: true });
    return entries
      .filter(entry => entry.isFile())
      .map(entry => entry.name)
      .sort();
  } catch {
    return [];
  }
}

/**
 * Sample first N files from directory and extract snippets
 * Only processes .mjs, .js, .md, .ts files
 * @param {string} dirPath - Directory path
 * @param {number} maxSamples - Maximum files to sample
 * @returns {Promise<Record<string, string>>} Map of filename to snippet
 */
async function sampleSnippets(dirPath, maxSamples = 10) {
  const files = await listFiles(dirPath);
  const snippets = {};

  // Filter to relevant extensions and take first N
  const sampleFiles = files
    .filter(file => {
      const ext = extname(file);
      return ['.mjs', '.js', '.md', '.ts'].includes(ext);
    })
    .slice(0, maxSamples);

  for (const file of sampleFiles) {
    const filePath = join(dirPath, file);
    snippets[file] = await readSnippet(filePath);
  }

  return snippets;
}

/**
 * Count files in test directory
 * @param {string} packageDir - Package directory path
 * @returns {Promise<number>} Count of test files
 */
async function countTestFiles(packageDir) {
  const testDir = join(packageDir, 'test');
  const files = await listFiles(testDir);
  return files.length;
}

/**
 * Compute SHA256 fingerprint from evidence components
 * @param {string[]} readmeHeadings - README headings
 * @param {string[]} examplesFiles - Examples file list
 * @param {string[]} docsFiles - Docs file list
 * @param {string[]} srcFiles - Source file list
 * @param {string[]} keywords - Package keywords
 * @returns {string} Hex-encoded SHA256 hash
 */
function computeFingerprint(readmeHeadings, examplesFiles, docsFiles, srcFiles, keywords) {
  const combined = [
    readmeHeadings.join('|'),
    examplesFiles.join('|'),
    docsFiles.join('|'),
    srcFiles.join('|'),
    keywords.join('|')
  ].join('|');

  return createHash('sha256').update(combined).digest('hex');
}

/**
 * Collect evidence from package files to guide Diátaxis classification
 * @param {string} packageDir - Absolute path to package directory
 * @param {Object} packageJson - Parsed package.json object
 * @returns {Promise<EvidenceSnapshot>} Evidence snapshot
 * @throws {Error} If package.json is invalid or cannot be processed
 */
export async function collectEvidence(packageDir, packageJson) {
  // Validate inputs
  if (!packageJson || typeof packageJson !== 'object') {
    throw new Error('Invalid package.json: must be a valid object');
  }

  // Read README
  let readmeContent = null;
  let readmeHeadings = [];
  const readmePath = join(packageDir, 'README.md');
  if (await fileExists(readmePath)) {
    try {
      readmeContent = await readFile(readmePath, 'utf-8');
      readmeHeadings = extractHeadings(readmeContent);
    } catch (error) {
      console.warn(`Failed to read README.md: ${error.message}`);
    }
  }

  // List and sample examples/
  const examplesDir = join(packageDir, 'examples');
  const examplesFiles = await listFiles(examplesDir);
  const examplesSnippets = await sampleSnippets(examplesDir);

  // List and sample docs/
  const docsDir = join(packageDir, 'docs');
  const docsFiles = await listFiles(docsDir);
  const docsSnippets = await sampleSnippets(docsDir);

  // List src/ top-level .mjs/.js/.ts files
  const srcDir = join(packageDir, 'src');
  const allSrcFiles = await listFiles(srcDir);
  const srcFiles = allSrcFiles.filter(file => {
    const ext = extname(file);
    return ['.mjs', '.js', '.ts'].includes(ext);
  });

  // Count test files
  const testFileCount = await countTestFiles(packageDir);

  // Extract from package.json
  const binEntries = typeof packageJson.bin === 'object' ? packageJson.bin : {};
  const exportSurface = typeof packageJson.exports === 'object' ? packageJson.exports : {};
  const keywords = Array.isArray(packageJson.keywords) ? packageJson.keywords : [];

  // Check file presence
  const hasLicense = await fileExists(join(packageDir, 'LICENSE'));
  const hasTsConfig = await fileExists(join(packageDir, 'tsconfig.json'));

  // Compute fingerprint
  const fingerprint = computeFingerprint(
    readmeHeadings,
    examplesFiles,
    docsFiles,
    srcFiles,
    keywords
  );

  return {
    readmeContent,
    readmeHeadings,
    examplesFiles,
    examplesSnippets,
    docsFiles,
    docsSnippets,
    srcFiles,
    testFileCount,
    binEntries,
    exportSurface,
    keywords,
    hasLicense,
    hasTsConfig,
    fingerprint
  };
}

/**
 * Hash all fields in evidence snapshot to detect changes
 * Creates deterministic hash from all evidence fields
 * @param {EvidenceSnapshot} evidenceSnapshot - Evidence snapshot to hash
 * @returns {string} Hex-encoded SHA256 hash
 */
export function hashEvidence(evidenceSnapshot) {
  // Create stable string representation of all fields
  const parts = [
    evidenceSnapshot.readmeContent || '',
    evidenceSnapshot.readmeHeadings.join('|'),
    evidenceSnapshot.examplesFiles.join('|'),
    Object.entries(evidenceSnapshot.examplesSnippets)
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([k, v]) => `${k}:${v}`)
      .join('|'),
    evidenceSnapshot.docsFiles.join('|'),
    Object.entries(evidenceSnapshot.docsSnippets)
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([k, v]) => `${k}:${v}`)
      .join('|'),
    evidenceSnapshot.srcFiles.join('|'),
    String(evidenceSnapshot.testFileCount),
    Object.entries(evidenceSnapshot.binEntries)
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([k, v]) => `${k}:${v}`)
      .join('|'),
    JSON.stringify(evidenceSnapshot.exportSurface),
    evidenceSnapshot.keywords.join('|'),
    String(evidenceSnapshot.hasLicense),
    String(evidenceSnapshot.hasTsConfig),
    evidenceSnapshot.fingerprint
  ];

  const combined = parts.join('||');
  return createHash('sha256').update(combined).digest('hex');
}
