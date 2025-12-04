/**
 * @file Documentation Generator - Auto-generate API docs from JSDoc
 * @module @unrdf/project-engine/doc-generator
 */

import { readFile, readdir } from 'node:fs/promises';
import { join, basename, relative } from 'node:path';
import { z } from 'zod';

/**
 * JSDoc tag schema
 */
const _JSDocTagSchema = z.object({
  name: z.string(),
  description: z.string(),
  params: z.array(
    z.object({
      name: z.string(),
      type: z.string(),
      description: z.string(),
    })
  ),
  returns: z
    .object({
      type: z.string(),
      description: z.string(),
    })
    .optional(),
  throws: z.array(z.string()).optional(),
  examples: z.array(z.string()).optional(),
});

/**
 * Generate API documentation from JSDoc comments
 * @param {string} packagePath - Path to package directory
 * @returns {Promise<string>} Markdown API documentation
 *
 * @throws {TypeError} If packagePath is not a string
 * @throws {Error} If package directory doesn't exist
 *
 * @example
 * const docs = await generateApiDocs('./packages/core');
 * console.log(docs);
 */
export async function generateApiDocs(packagePath) {
  if (typeof packagePath !== 'string') {
    throw new TypeError('generateApiDocs: packagePath must be a string');
  }

  try {
    const srcPath = join(packagePath, 'src');
    const files = await findSourceFiles(srcPath);

    const sections = [];
    sections.push('# API Documentation\n');
    sections.push(`Package: \`${basename(packagePath)}\`\n`);

    for (const file of files) {
      const content = await readFile(file, 'utf-8');
      const functions = extractFunctions(content);

      if (functions.length > 0) {
        const relPath = relative(srcPath, file);
        sections.push(`## ${relPath}\n`);

        for (const fn of functions) {
          sections.push(formatFunctionDoc(fn));
        }
      }
    }

    return sections.join('\n');
  } catch (error) {
    throw new Error(`generateApiDocs failed: ${error.message}`);
  }
}

/**
 * Generate package user guide template
 * @param {string} packagePath - Path to package directory
 * @returns {Promise<string>} Markdown user guide template
 *
 * @throws {TypeError} If packagePath is not a string
 * @throws {Error} If package.json cannot be read
 *
 * @example
 * const guide = await generatePackageGuide('./packages/core');
 * console.log(guide);
 */
export async function generatePackageGuide(packagePath) {
  if (typeof packagePath !== 'string') {
    throw new TypeError('generatePackageGuide: packagePath must be a string');
  }

  try {
    const packageJsonPath = join(packagePath, 'package.json');
    const packageJsonContent = await readFile(packageJsonPath, 'utf-8');
    const packageJson = JSON.parse(packageJsonContent);

    const sections = [];
    sections.push(`# ${packageJson.name}\n`);
    sections.push(`${packageJson.description}\n`);
    sections.push('## Installation\n');
    sections.push('```bash');
    sections.push(`pnpm add ${packageJson.name}`);
    sections.push('```\n');
    sections.push('## Quick Start\n');
    sections.push('```javascript');
    sections.push(`import { /* exports */ } from '${packageJson.name}';`);
    sections.push('```\n');
    sections.push('## Features\n');
    sections.push('- Feature 1');
    sections.push('- Feature 2\n');
    sections.push('## API Reference\n');
    sections.push('See [API.md](./API.md) for detailed API documentation.\n');
    sections.push('## Examples\n');
    sections.push('### Basic Usage\n');
    sections.push('```javascript');
    sections.push('// Add example code here');
    sections.push('```\n');
    sections.push('## License\n');
    sections.push(`${packageJson.license || 'MIT'}\n`);

    return sections.join('\n');
  } catch (error) {
    throw new Error(`generatePackageGuide failed: ${error.message}`);
  }
}

/**
 * Generate changelog from git commits
 * @param {string} repoPath - Path to git repository
 * @returns {Promise<string>} Markdown changelog
 *
 * @throws {TypeError} If repoPath is not a string
 * @throws {Error} If git operations fail
 *
 * @example
 * const changelog = await generateChangelog('.');
 * console.log(changelog);
 */
export async function generateChangelog(repoPath) {
  if (typeof repoPath !== 'string') {
    throw new TypeError('generateChangelog: repoPath must be a string');
  }

  try {
    const { execFile } = await import('node:child_process');
    const { promisify } = await import('node:util');
    const execFileAsync = promisify(execFile);

    // Get git log with commit messages
    const { stdout } = await execFileAsync(
      'git',
      ['log', '--pretty=format:%H|%ai|%s|%an', '--no-merges'],
      { cwd: repoPath }
    );

    const commits = stdout
      .trim()
      .split('\n')
      .map(line => {
        const [hash, date, message, author] = line.split('|');
        return { hash, date, message, author };
      });

    const sections = [];
    sections.push('# Changelog\n');

    // Group by date (YYYY-MM-DD)
    const byDate = new Map();
    for (const commit of commits) {
      const dateKey = commit.date.split(' ')[0];
      if (!byDate.has(dateKey)) {
        byDate.set(dateKey, []);
      }
      byDate.get(dateKey).push(commit);
    }

    // Sort dates descending
    const sortedDates = Array.from(byDate.keys()).sort().reverse();

    for (const date of sortedDates.slice(0, 30)) {
      sections.push(`## ${date}\n`);
      const dateCommits = byDate.get(date);
      for (const commit of dateCommits) {
        sections.push(`- ${commit.message} (${commit.hash.slice(0, 7)})`);
      }
      sections.push('');
    }

    return sections.join('\n');
  } catch (error) {
    throw new Error(`generateChangelog failed: ${error.message}`);
  }
}

/**
 * Find all source files in directory
 * @param {string} dirPath - Directory path
 * @returns {Promise<Array<string>>} List of file paths
 */
async function findSourceFiles(dirPath) {
  const files = [];

  try {
    const entries = await readdir(dirPath, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dirPath, entry.name);

      if (entry.isDirectory()) {
        const subFiles = await findSourceFiles(fullPath);
        files.push(...subFiles);
      } else if (entry.isFile() && entry.name.endsWith('.mjs')) {
        files.push(fullPath);
      }
    }
  } catch (error) {
    // Directory doesn't exist or not accessible
  }

  return files;
}

/**
 * Extract function documentation from source code
 * @param {string} content - Source code content
 * @returns {Array<Object>} Function documentation objects
 */
function extractFunctions(content) {
  const functions = [];
  const lines = content.split('\n');

  let currentDoc = null;
  let inDocComment = false;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();

    // Start of JSDoc comment
    if (line.startsWith('/**')) {
      inDocComment = true;
      currentDoc = { lines: [] };
      continue;
    }

    // End of JSDoc comment
    if (line.startsWith('*/') && inDocComment) {
      inDocComment = false;

      // Check next non-empty line for function declaration
      let j = i + 1;
      while (j < lines.length && lines[j].trim() === '') {
        j++;
      }

      if (j < lines.length) {
        const nextLine = lines[j].trim();
        const fnMatch = nextLine.match(/export\s+(async\s+)?function\s+(\w+)/);

        if (fnMatch) {
          const fnName = fnMatch[2];
          const doc = parseJSDoc(currentDoc.lines);
          functions.push({
            name: fnName,
            description: doc.description,
            params: doc.params,
            returns: doc.returns,
            throws: doc.throws,
            examples: doc.examples,
          });
        }
      }

      currentDoc = null;
      continue;
    }

    // Inside JSDoc comment
    if (inDocComment && currentDoc) {
      const cleaned = line.replace(/^\*\s?/, '');
      currentDoc.lines.push(cleaned);
    }
  }

  return functions;
}

/**
 * Parse JSDoc comment lines
 * @param {Array<string>} lines - JSDoc comment lines
 * @returns {Object} Parsed documentation
 */
function parseJSDoc(lines) {
  const doc = {
    description: '',
    params: [],
    returns: null,
    throws: [],
    examples: [],
  };

  let currentTag = null;
  let currentText = [];

  for (const line of lines) {
    if (line.startsWith('@')) {
      // Save previous tag
      if (currentTag) {
        saveTag(doc, currentTag, currentText.join(' '));
      }

      // Parse new tag
      const match = line.match(/@(\w+)\s+(.+)/);
      if (match) {
        currentTag = match[1];
        currentText = [match[2]];
      }
    } else if (currentTag) {
      currentText.push(line);
    } else {
      doc.description += line + ' ';
    }
  }

  // Save last tag
  if (currentTag) {
    saveTag(doc, currentTag, currentText.join(' '));
  }

  doc.description = doc.description.trim();

  return doc;
}

/**
 * Save parsed JSDoc tag
 * @param {Object} doc - Documentation object
 * @param {string} tag - Tag name
 * @param {string} text - Tag text
 */
function saveTag(doc, tag, text) {
  if (tag === 'param') {
    const match = text.match(/\{([^}]+)\}\s+(\w+)\s+-\s+(.+)/);
    if (match) {
      doc.params.push({
        type: match[1],
        name: match[2],
        description: match[3].trim(),
      });
    }
  } else if (tag === 'returns') {
    const match = text.match(/\{([^}]+)\}\s+(.+)/);
    if (match) {
      doc.returns = {
        type: match[1],
        description: match[2].trim(),
      };
    }
  } else if (tag === 'throws') {
    doc.throws.push(text.trim());
  } else if (tag === 'example') {
    doc.examples.push(text.trim());
  }
}

/**
 * Format function documentation as Markdown
 * @param {Object} fn - Function documentation
 * @returns {string} Markdown formatted documentation
 */
function formatFunctionDoc(fn) {
  const sections = [];

  sections.push(`### ${fn.name}\n`);
  sections.push(`${fn.description}\n`);

  if (fn.params.length > 0) {
    sections.push('**Parameters:**\n');
    for (const param of fn.params) {
      sections.push(`- \`${param.name}\` (\`${param.type}\`): ${param.description}`);
    }
    sections.push('');
  }

  if (fn.returns) {
    sections.push(`**Returns:** \`${fn.returns.type}\` - ${fn.returns.description}\n`);
  }

  if (fn.throws && fn.throws.length > 0) {
    sections.push('**Throws:**\n');
    for (const throwsDesc of fn.throws) {
      sections.push(`- ${throwsDesc}`);
    }
    sections.push('');
  }

  if (fn.examples && fn.examples.length > 0) {
    sections.push('**Example:**\n');
    sections.push('```javascript');
    sections.push(fn.examples.join('\n'));
    sections.push('```\n');
  }

  return sections.join('\n');
}
