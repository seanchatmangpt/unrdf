/**
 * Documentation Pipeline - Collect, CONSTRUCT, LaTeX, PDF
 * @module @unrdf/v6-core/docs/pipeline
 */

import { readdir, readFile, writeFile, mkdir } from 'node:fs/promises';
import { join, dirname } from 'node:path';
import { existsSync } from 'node:fs';
import { createHash } from 'node:crypto';

/**
 * @typedef {Object} PackageDocs
 * @property {string} packageName - Package name
 * @property {string} version - Package version
 * @property {string} dir - Package directory
 * @property {DocFile[]} tutorials - Tutorial files
 * @property {DocFile[]} howtos - How-to files
 * @property {DocFile[]} reference - Reference files
 * @property {DocFile[]} explanation - Explanation files
 */

/**
 * @typedef {Object} DocFile
 * @property {string} path - Absolute file path
 * @property {string} name - File name
 * @property {Object} frontmatter - YAML frontmatter
 * @property {string} content - Markdown content
 * @property {string} hash - SHA256 hash
 */

/**
 * @typedef {Object} DiataxisOutput
 * @property {PackageDocs[]} packages - All package docs
 * @property {string} merkleRoot - Merkle root of all docs
 * @property {string} generatedAt - ISO timestamp
 * @property {Object} stats - Statistics
 */

/**
 * Collect documentation from all packages
 * @param {string[]} packageDirs - Array of package directory paths
 * @param {Object} [options={}] - Collection options
 * @param {string[]} [options.categories=['tutorials','how-to','reference','explanation']] - Categories to collect
 * @returns {Promise<PackageDocs[]>}
 */
export async function collectPackageDocs(packageDirs, options = {}) {
  const { categories = ['tutorials', 'how-to', 'reference', 'explanation'] } = options;
  
  const allDocs = [];
  
  for (const dir of packageDirs) {
    const packageJson = await readPackageJson(join(dir, 'package.json'));
    if (!packageJson) continue;
    
    const docsDir = join(dir, 'docs');
    if (!existsSync(docsDir)) continue;
    
    const packageDocs = {
      packageName: packageJson.name,
      version: packageJson.version,
      dir: dir,
      tutorials: [],
      howtos: [],
      reference: [],
      explanation: []
    };
    
    // Collect each category
    for (const category of categories) {
      const categoryDir = join(docsDir, category);
      if (!existsSync(categoryDir)) continue;
      
      const files = await collectMarkdownFiles(categoryDir);
      const categoryKey = category === 'how-to' ? 'howtos' : category;
      packageDocs[categoryKey] = files;
    }
    
    allDocs.push(packageDocs);
  }
  
  return allDocs;
}

/**
 * Generate Diataxis output structure
 * @param {PackageDocs[]} packageDocs - Collected package docs
 * @returns {Promise<DiataxisOutput>}
 */
export async function generateDiataxisOutput(packageDocs) {
  const hashes = [];
  let tutorialCount = 0;
  let howtoCount = 0;
  let referenceCount = 0;
  let explanationCount = 0;
  
  for (const pkg of packageDocs) {
    tutorialCount += pkg.tutorials.length;
    howtoCount += pkg.howtos.length;
    referenceCount += pkg.reference.length;
    explanationCount += pkg.explanation.length;
    
    // Collect all hashes for Merkle root
    hashes.push(...pkg.tutorials.map(f => f.hash));
    hashes.push(...pkg.howtos.map(f => f.hash));
    hashes.push(...pkg.reference.map(f => f.hash));
    hashes.push(...pkg.explanation.map(f => f.hash));
  }
  
  // Calculate Merkle root (simple hash of sorted hashes)
  const merkleRoot = calculateMerkleRoot(hashes);
  
  return {
    packages: packageDocs,
    merkleRoot,
    generatedAt: new Date().toISOString(),
    stats: {
      packageCount: packageDocs.length,
      tutorialCount,
      howtoCount,
      referenceCount,
      explanationCount,
      totalDocs: tutorialCount + howtoCount + referenceCount + explanationCount
    }
  };
}

/**
 * Emit receipt for pipeline stage
 * @param {string} stage - Pipeline stage name
 * @param {Object} input - Stage input
 * @param {Object} output - Stage output
 * @param {string} [outputPath] - Optional path to write receipt
 * @returns {Promise<Object>} Receipt object
 */
export async function emitReceipt(stage, input, output, outputPath) {
  const receipt = {
    stage,
    input: {
      type: typeof input,
      hash: hashObject(input)
    },
    output: {
      type: typeof output,
      hash: hashObject(output)
    },
    timestamp: new Date().toISOString(),
    merkleRoot: output.merkleRoot || hashObject(output)
  };
  
  if (outputPath) {
    await mkdir(dirname(outputPath), { recursive: true });
    await writeFile(outputPath, JSON.stringify(receipt, null, 2));
  }
  
  return receipt;
}

/**
 * Collect all markdown files from a directory
 * @param {string} dir - Directory path
 * @returns {Promise<DocFile[]>}
 */
async function collectMarkdownFiles(dir) {
  const files = [];
  
  try {
    const entries = await readdir(dir, { withFileTypes: true });
    
    for (const entry of entries) {
      if (entry.isFile() && entry.name.endsWith('.md') && entry.name !== 'README.md') {
        const filePath = join(dir, entry.name);
        const content = await readFile(filePath, 'utf-8');
        const { frontmatter, body } = parseFrontmatter(content);
        
        files.push({
          path: filePath,
          name: entry.name,
          frontmatter,
          content: body,
          hash: hashString(content)
        });
      }
    }
  } catch (error) {
    // Gracefully handle missing directories
  }
  
  return files;
}

/**
 * Read and parse package.json
 * @param {string} path - Path to package.json
 * @returns {Promise<Object|null>}
 */
async function readPackageJson(path) {
  try {
    const content = await readFile(path, 'utf-8');
    return JSON.parse(content);
  } catch (error) {
    return null;
  }
}

/**
 * Parse frontmatter from markdown
 * @param {string} content - Markdown content
 * @returns {{ frontmatter: Object, body: string }}
 */
function parseFrontmatter(content) {
  const match = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
  
  if (!match) {
    return { frontmatter: {}, body: content };
  }
  
  const frontmatterText = match[1];
  const body = match[2];
  
  // Simple YAML parsing (key: value only)
  const frontmatter = {};
  const lines = frontmatterText.split('\n');
  
  for (const line of lines) {
    const colonIndex = line.indexOf(':');
    if (colonIndex > 0) {
      const key = line.slice(0, colonIndex).trim();
      const value = line.slice(colonIndex + 1).trim().replace(/^["']|["']$/g, '');
      frontmatter[key] = value;
    }
  }
  
  return { frontmatter, body };
}

/**
 * Calculate Merkle root from hashes
 * @param {string[]} hashes - Array of SHA256 hashes
 * @returns {string} Merkle root hash
 */
function calculateMerkleRoot(hashes) {
  if (hashes.length === 0) return hashString('');
  
  // Sort for determinism
  const sorted = [...hashes].sort();
  
  // Simple Merkle tree (hash of concatenated hashes)
  return hashString(sorted.join(''));
}

/**
 * Hash a string with SHA256
 * @param {string} str - String to hash
 * @returns {string} Hex hash
 */
function hashString(str) {
  return createHash('sha256').update(str).digest('hex');
}

/**
 * Hash an object (deterministic JSON)
 * @param {Object} obj - Object to hash
 * @returns {string} Hex hash
 */
function hashObject(obj) {
  // Stable stringify (sorted keys)
  const json = JSON.stringify(obj, Object.keys(obj).sort());
  return hashString(json);
}

