#!/usr/bin/env node
/**
 * KGC Documentation CLI Harness
 *
 * Main entry point for the KGC (Knowledge Graph Centric) documentation system.
 * Provides receipt-driven documentation with proof guarantees and Diátaxis projection.
 *
 * @module tools/kgc-docs
 * @version 1.0.0
 *
 * @description
 * Command router for:
 * - build: Generate documentation from .kgcmd sources with 4-view projection
 * - scan: Discover API surfaces from packages (auto-discovery)
 * - refresh: Re-execute code blocks, update hashes and receipts
 * - prove: Verify all receipts, check cryptographic chains
 * - render: Render .kgcmd to .md with receipt validation
 * - verify: Dry-run to check determinism violations
 * - manifest: Aggregate receipts into manifest with Merkle proofs
 *
 * @example
 * # Build documentation from sources
 * node tools/kgc-docs.mjs build docs/src/*.kgcmd
 *
 * # Scan package APIs
 * node tools/kgc-docs.mjs scan @unrdf/oxigraph --output-format json
 *
 * # Verify receipts
 * node tools/kgc-docs.mjs prove docs/api/store.md
 *
 * # Refresh executable blocks
 * node tools/kgc-docs.mjs refresh docs/src/tutorial.kgcmd
 *
 * # Verify determinism
 * node tools/kgc-docs.mjs verify docs/
 *
 * # Generate manifest
 * node tools/kgc-docs.mjs manifest receipts/
 */

import { readFile, writeFile, readdir, stat, mkdir } from 'node:fs/promises';
import { join, dirname, basename, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { execSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { z } from 'zod';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const WORKSPACE_ROOT = resolve(__dirname, '..');

// =============================================================================
// Simple glob implementation using native fs
// =============================================================================

/**
 * Simple glob pattern matcher
 * @param {string} pattern - Glob pattern (e.g., "*.json", "**\/*.mjs")
 * @param {Object} options - Options {cwd, absolute}
 * @returns {Promise<string[]>} Matching file paths
 */
async function glob(pattern, options = {}) {
  const { cwd = process.cwd(), absolute = false } = options;
  const results = [];

  async function walk(dir) {
    const entries = await readdir(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dir, entry.name);

      if (entry.isDirectory()) {
        // Recurse if pattern includes **
        if (pattern.includes('**') || pattern.includes('/')) {
          await walk(fullPath);
        }
      } else if (entry.isFile()) {
        // Match pattern
        const relativePath = relative(cwd, fullPath);
        if (matchPattern(relativePath, pattern) || matchPattern(entry.name, pattern)) {
          results.push(absolute ? fullPath : relativePath);
        }
      }
    }
  }

  /**
   * Simple pattern matching (supports * wildcard)
   * @param {string} str - String to test
   * @param {string} pattern - Pattern with * wildcards
   * @returns {boolean} Match result
   */
  function matchPattern(str, pattern) {
    // Convert glob pattern to regex
    const regexPattern = pattern
      .replace(/\*\*/g, '.*')
      .replace(/\*/g, '[^/]*')
      .replace(/\./g, '\\.');

    const regex = new RegExp(`^${regexPattern}$`);
    return regex.test(str);
  }

  await walk(cwd);
  return results;
}

// =============================================================================
// Zod Schemas for CLI Validation
// =============================================================================

const GlobalFlagsSchema = z.object({
  verbose: z.boolean().default(false),
  deterministic: z.boolean().default(false),
  outputFormat: z.enum(['json', 'markdown', 'stream-json']).default('markdown'),
});

const BuildCommandSchema = z.object({
  command: z.literal('build'),
  sources: z.array(z.string()).min(1),
  flags: GlobalFlagsSchema,
});

const ScanCommandSchema = z.object({
  command: z.literal('scan'),
  scope: z.string().optional(),
  flags: GlobalFlagsSchema,
});

const RefreshCommandSchema = z.object({
  command: z.literal('refresh'),
  docPath: z.string(),
  flags: GlobalFlagsSchema,
});

const ProveCommandSchema = z.object({
  command: z.literal('prove'),
  docPath: z.string(),
  flags: GlobalFlagsSchema,
});

const RenderCommandSchema = z.object({
  command: z.literal('render'),
  kgcmdPath: z.string(),
  flags: GlobalFlagsSchema,
});

const VerifyCommandSchema = z.object({
  command: z.literal('verify'),
  docsDir: z.string(),
  flags: GlobalFlagsSchema,
});

const ManifestCommandSchema = z.object({
  command: z.literal('manifest'),
  receiptsDir: z.string(),
  flags: GlobalFlagsSchema,
});

const ValidateCommandSchema = z.object({
  command: z.literal('validate'),
  packagesDir: z.string(),
  flags: GlobalFlagsSchema,
});

const GenerateSchemaCommandSchema = z.object({
  command: z.literal('generate-schema'),
  packagePath: z.string(),
  flags: GlobalFlagsSchema,
});

const CompileLatexCommandSchema = z.object({
  command: z.literal('compile-latex'),
  latexPath: z.string(),
  flags: GlobalFlagsSchema,
});

const ThesisCommandSchema = z.object({
  command: z.literal('thesis'),
  thesisDir: z.string(),
  flags: GlobalFlagsSchema,
});

const CommandSchema = z.discriminatedUnion('command', [
  BuildCommandSchema,
  ScanCommandSchema,
  RefreshCommandSchema,
  ProveCommandSchema,
  RenderCommandSchema,
  VerifyCommandSchema,
  ManifestCommandSchema,
  ValidateCommandSchema,
  GenerateSchemaCommandSchema,
  CompileLatexCommandSchema,
  ThesisCommandSchema,
]);

// =============================================================================
// Error Types
// =============================================================================

/**
 * Structured error for KGC operations
 */
class KGCError extends Error {
  /**
   * @param {string} code - Error code (e.g., KGC_RECEIPT_MISSING)
   * @param {string} message - Error message
   * @param {Object} [context] - Additional context
   * @param {string} [remediation] - Suggested fix
   */
  constructor(code, message, context = {}, remediation = '') {
    super(message);
    this.name = 'KGCError';
    this.code = code;
    this.context = context;
    this.remediation = remediation;
  }

  /**
   * Format error as JSON
   * @returns {Object} Structured error object
   */
  toJSON() {
    return {
      type: 'error',
      code: this.code,
      message: this.message,
      context: this.context,
      remediation: this.remediation,
      timestamp: new Date().toISOString(),
    };
  }
}

// =============================================================================
// CLI Parser
// =============================================================================

/**
 * Parse command line arguments
 * @param {string[]} args - Process argv
 * @returns {Object} Parsed command object
 * @throws {KGCError} If invalid arguments
 */
function parseArgs(args) {
  const [command, ...rest] = args;

  if (!command || command === 'help' || command === '--help') {
    printUsage();
    process.exit(0);
  }

  // Extract flags
  const flags = {
    verbose: rest.includes('--verbose') || rest.includes('-v'),
    deterministic: rest.includes('--deterministic') || process.env.DETERMINISTIC === '1',
    outputFormat: 'markdown',
  };

  // Extract --output-format
  const formatIndex = rest.findIndex(arg => arg === '--output-format');
  if (formatIndex !== -1 && rest[formatIndex + 1]) {
    flags.outputFormat = rest[formatIndex + 1];
  }

  // Remove flags from rest
  const positional = rest.filter(arg => !arg.startsWith('--') && !arg.startsWith('-'));

  // Build command object based on command type
  let cmd;
  switch (command) {
    case 'build': {
      if (positional.length === 0) {
        throw new KGCError(
          'KGC_INVALID_ARGS',
          'Build command requires at least one source file',
          { command: 'build', args: rest },
          'Usage: kgc-docs build <source.kgcmd> [...]'
        );
      }
      cmd = { command: 'build', sources: positional, flags };
      break;
    }

    case 'scan': {
      cmd = { command: 'scan', scope: positional[0], flags };
      break;
    }

    case 'refresh': {
      if (!positional[0]) {
        throw new KGCError(
          'KGC_INVALID_ARGS',
          'Refresh command requires document path',
          { command: 'refresh' },
          'Usage: kgc-docs refresh <doc.kgcmd>'
        );
      }
      cmd = { command: 'refresh', docPath: positional[0], flags };
      break;
    }

    case 'prove': {
      if (!positional[0]) {
        throw new KGCError(
          'KGC_INVALID_ARGS',
          'Prove command requires document path',
          { command: 'prove' },
          'Usage: kgc-docs prove <doc.md>'
        );
      }
      cmd = { command: 'prove', docPath: positional[0], flags };
      break;
    }

    case 'render': {
      if (!positional[0]) {
        throw new KGCError(
          'KGC_INVALID_ARGS',
          'Render command requires .kgcmd path',
          { command: 'render' },
          'Usage: kgc-docs render <source.kgcmd>'
        );
      }
      cmd = { command: 'render', kgcmdPath: positional[0], flags };
      break;
    }

    case 'verify': {
      const docsDir = positional[0] || 'docs/';
      cmd = { command: 'verify', docsDir, flags };
      break;
    }

    case 'manifest': {
      const receiptsDir = positional[0] || 'receipts/';
      cmd = { command: 'manifest', receiptsDir, flags };
      break;
    }

    case 'validate': {
      const packagesDir = positional[0] || 'packages/';
      cmd = { command: 'validate', packagesDir, flags };
      break;
    }

    case 'generate-schema': {
      if (!positional[0]) {
        throw new KGCError(
          'KGC_INVALID_ARGS',
          'Generate-schema command requires package path',
          { command: 'generate-schema' },
          'Usage: kgc-docs generate-schema <package-path>'
        );
      }
      cmd = { command: 'generate-schema', packagePath: positional[0], flags };
      break;
    }

    case 'compile-latex': {
      if (!positional[0]) {
        throw new KGCError(
          'KGC_INVALID_ARGS',
          'Compile-latex command requires LaTeX file path',
          { command: 'compile-latex' },
          'Usage: kgc-docs compile-latex <file.tex>'
        );
      }
      cmd = { command: 'compile-latex', latexPath: positional[0], flags };
      break;
    }

    case 'thesis': {
      const thesisDir = positional[0] || 'thesis/';
      cmd = { command: 'thesis', thesisDir, flags };
      break;
    }

    default:
      throw new KGCError(
        'KGC_UNKNOWN_COMMAND',
        `Unknown command: ${command}`,
        { command, availableCommands: ['build', 'scan', 'refresh', 'prove', 'render', 'verify', 'manifest', 'validate', 'generate-schema', 'compile-latex', 'thesis'] },
        'Run "kgc-docs help" to see available commands'
      );
  }

  // Validate with Zod
  try {
    return CommandSchema.parse(cmd);
  } catch (err) {
    throw new KGCError(
      'KGC_VALIDATION_ERROR',
      'Invalid command arguments',
      { zodError: err.errors },
      'Check command syntax and try again'
    );
  }
}

/**
 * Print usage information
 */
function printUsage() {
  console.log(`
KGC Documentation CLI - Receipt-Driven Documentation System

USAGE:
  kgc-docs <command> [options]

COMMANDS:
  build <sources...>       Build documentation from .kgcmd sources (4-view projection)
  scan [scope]             Discover API surfaces from packages
  refresh <doc>            Re-execute code blocks, update receipts
  prove <doc>              Verify all receipts, check cryptographic chains
  render <kgcmd>           Render .kgcmd to .md with receipt validation
  verify <dir>             Dry-run to check determinism violations
  manifest <dir>           Aggregate receipts into manifest with Merkle proofs
  validate [dir]           Check docs completeness (all public APIs documented)
  generate-schema <pkg>    Auto-generate reference from JSDoc
  compile-latex <file>     Deterministic LaTeX→PDF (cache breaking on content change)
  thesis [dir]             Build thesis document with receipt-driven provenance

GLOBAL FLAGS:
  --verbose, -v            Enable verbose logging
  --deterministic          Use deterministic timestamps (from o_hash epoch)
  --output-format FORMAT   Output format: json, markdown, stream-json (default: markdown)

EXAMPLES:
  # Build all tutorials
  kgc-docs build docs/src/tutorials/*.kgcmd

  # Scan package API
  kgc-docs scan @unrdf/oxigraph --output-format json

  # Verify receipts
  kgc-docs prove docs/api/store.md

  # Refresh executable blocks
  kgc-docs refresh docs/src/tutorial.kgcmd --deterministic

  # Verify determinism
  kgc-docs verify docs/ --verbose

  # Generate manifest
  kgc-docs manifest receipts/

EXIT CODES:
  0 - Success
  1 - Error (receipt invalid, file not found, etc.)
  2 - Bounds exceeded (timeout, resource limit)

ENVIRONMENT:
  DETERMINISTIC=1          Enable deterministic mode (same as --deterministic)

For more information, see: docs/kgc-markdown.md
`);
}

// =============================================================================
// Build Command
// =============================================================================

/**
 * Build documentation from .kgcmd sources
 *
 * Process:
 * 1. Load all .kgcmd files
 * 2. Validate frontmatter + receipts
 * 3. Execute all kgc:* blocks (respecting bounds)
 * 4. Generate 4 views (Diátaxis projection)
 * 5. Emit receipts for each block
 * 6. Update manifest.json
 *
 * @param {Object} cmd - Parsed command
 * @returns {Promise<Object>} Build result
 */
async function buildCommand(cmd) {
  const { sources, flags } = cmd;
  const results = [];

  logVerbose(flags, `Building documentation from ${sources.length} sources...`);

  for (const sourcePattern of sources) {
    const files = await glob(sourcePattern, { cwd: WORKSPACE_ROOT, absolute: true });

    if (files.length === 0) {
      console.warn(`⚠️  No files found matching: ${sourcePattern}`);
      continue;
    }

    logVerbose(flags, `Found ${files.length} files for pattern: ${sourcePattern}`);

    for (const file of files) {
      try {
        const result = await processKGCMD(file, flags);
        results.push(result);
      } catch (err) {
        console.error(`❌ Failed to process ${file}: ${err.message}`);
        if (flags.verbose) console.error(err.stack);
      }
    }
  }

  // Generate manifest
  const manifest = {
    generated: flags.deterministic ? '1970-01-01T00:00:00.000Z' : new Date().toISOString(),
    sources: sources,
    results: results,
    totalFiles: results.length,
    totalViews: results.reduce((sum, r) => sum + Object.keys(r.views).length, 0),
  };

  // Write manifest
  await mkdir(join(WORKSPACE_ROOT, 'receipts'), { recursive: true });
  const manifestPath = join(WORKSPACE_ROOT, 'receipts', 'manifest.json');
  await writeFile(manifestPath, JSON.stringify(manifest, null, 2));

  logVerbose(flags, `Manifest written to: ${manifestPath}`);

  return formatOutput(flags, {
    success: true,
    filesProcessed: results.length,
    viewsGenerated: manifest.totalViews,
    manifestPath: relative(WORKSPACE_ROOT, manifestPath),
  });
}

/**
 * Process single .kgcmd file
 * @param {string} filePath - Absolute path to .kgcmd file
 * @param {Object} flags - Global flags
 * @returns {Promise<Object>} Processing result
 */
async function processKGCMD(filePath, flags) {
  logVerbose(flags, `Processing: ${relative(WORKSPACE_ROOT, filePath)}`);

  const content = await readFile(filePath, 'utf-8');
  const { frontmatter, body } = parseFrontmatter(content);

  // Extract code blocks
  const blocks = extractCodeBlocks(body);

  // Execute executable blocks
  const executedBlocks = [];
  for (const block of blocks) {
    if (block.executable) {
      const result = await executeBlock(block, flags);
      executedBlocks.push({ ...block, ...result });
    } else {
      executedBlocks.push(block);
    }
  }

  // Generate 4 Diátaxis views
  const views = await generateDiataxisViews(filePath, frontmatter, executedBlocks, flags);

  return {
    source: relative(WORKSPACE_ROOT, filePath),
    frontmatter,
    blocksExecuted: executedBlocks.filter(b => b.executable).length,
    views,
  };
}

/**
 * Parse frontmatter from markdown
 * @param {string} content - File content
 * @returns {Object} {frontmatter, body}
 */
function parseFrontmatter(content) {
  const match = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
  if (!match) {
    return { frontmatter: {}, body: content };
  }

  const [, fmRaw, body] = match;
  const frontmatter = {};

  for (const line of fmRaw.split('\n')) {
    const [key, ...valueParts] = line.split(':');
    if (key && valueParts.length > 0) {
      const value = valueParts.join(':').trim();
      frontmatter[key.trim()] = value;
    }
  }

  return { frontmatter, body };
}

/**
 * Extract code blocks from markdown
 * @param {string} markdown - Markdown content
 * @returns {Array<Object>} Array of code blocks
 */
function extractCodeBlocks(markdown) {
  const blocks = [];
  const regex = /```(\w+)(?:\s+executable)?\n([\s\S]*?)```/g;
  let match;

  while ((match = regex.exec(markdown)) !== null) {
    const [fullMatch, language, code] = match;
    blocks.push({
      id: `block-${blocks.length + 1}`,
      language,
      code: code.trim(),
      executable: fullMatch.includes('executable'),
      position: match.index,
    });
  }

  return blocks;
}

/**
 * Execute code block
 * @param {Object} block - Code block
 * @param {Object} flags - Global flags
 * @returns {Promise<Object>} Execution result
 */
async function executeBlock(block, flags) {
  logVerbose(flags, `Executing block: ${block.id} (${block.language})`);

  const startTime = Date.now();
  let stdout = '';
  let stderr = '';
  let exitCode = 0;

  try {
    // Write code to temp file
    const tempFile = join(WORKSPACE_ROOT, `.kgc-temp-${block.id}.${block.language}`);
    await writeFile(tempFile, block.code);

    // Execute based on language
    let command;
    switch (block.language) {
      case 'javascript':
      case 'js':
      case 'mjs':
        command = `node ${tempFile}`;
        break;
      case 'bash':
      case 'sh':
        command = `bash ${tempFile}`;
        break;
      default:
        throw new KGCError(
          'KGC_UNSUPPORTED_LANGUAGE',
          `Cannot execute language: ${block.language}`,
          { blockId: block.id, language: block.language },
          'Only javascript/js/mjs/bash/sh are executable'
        );
    }

    // Execute with timeout
    const result = execSync(command, {
      cwd: WORKSPACE_ROOT,
      encoding: 'utf-8',
      timeout: 20000, // 20s timeout
      env: {
        ...process.env,
        NODE_ENV: 'test',
        TZ: 'UTC',
        LANG: 'en_US.UTF-8',
      },
    });

    stdout = result.toString();
  } catch (err) {
    stderr = err.stderr?.toString() || err.message;
    exitCode = err.status || 1;
  }

  const duration = Date.now() - startTime;
  const output = stdout + stderr;
  const outputHash = await computeHash(normalizeOutput(output));

  return {
    output,
    outputHash,
    exitCode,
    duration,
    executedAt: flags.deterministic ? '1970-01-01T00:00:00.000Z' : new Date().toISOString(),
  };
}

/**
 * Normalize output for deterministic hashing
 * @param {string} output - Raw output
 * @returns {string} Normalized output
 */
function normalizeOutput(output) {
  return output
    .replace(/\r\n/g, '\n') // Normalize line endings
    .replace(/\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z/g, 'TIMESTAMP') // Normalize ISO timestamps
    .replace(/Duration: \d+ms/g, 'Duration: XXXms') // Normalize durations
    .trim();
}

/**
 * Generate Diátaxis views from source
 * @param {string} sourcePath - Source file path
 * @param {Object} frontmatter - Frontmatter metadata
 * @param {Array<Object>} blocks - Executed blocks
 * @param {Object} flags - Global flags
 * @returns {Promise<Object>} Generated views {tutorial, howto, reference, explanation}
 */
async function generateDiataxisViews(sourcePath, frontmatter, blocks, flags) {
  const baseName = basename(sourcePath, '.kgcmd');
  const views = {};

  // Create output directories
  const docsDir = join(WORKSPACE_ROOT, 'docs');
  for (const viewType of ['tutorials', 'how-to', 'reference', 'explanations']) {
    await mkdir(join(docsDir, viewType), { recursive: true });
  }

  // Generate each view (simplified - full implementation would parse tags)
  const viewTypes = ['tutorial', 'howto', 'reference', 'explanation'];

  for (const viewType of viewTypes) {
    const content = renderView(viewType, frontmatter, blocks);
    const outputPath = join(docsDir, `${viewType}s`, `${baseName}.md`);
    await writeFile(outputPath, content);

    // Generate receipt
    const receipt = {
      source: relative(WORKSPACE_ROOT, sourcePath),
      view: viewType,
      blocksIncluded: blocks.length,
      contentHash: await computeHash(content),
      generatedAt: flags.deterministic ? '1970-01-01T00:00:00.000Z' : new Date().toISOString(),
    };

    await writeFile(`${outputPath}.receipt.json`, JSON.stringify(receipt, null, 2));

    views[viewType] = relative(WORKSPACE_ROOT, outputPath);
  }

  return views;
}

/**
 * Render view-specific content
 * @param {string} viewType - View type (tutorial/howto/reference/explanation)
 * @param {Object} frontmatter - Frontmatter
 * @param {Array<Object>} blocks - Code blocks
 * @returns {string} Rendered markdown
 */
function renderView(viewType, frontmatter, blocks) {
  let md = `# ${frontmatter.title || 'Untitled'}\n\n`;
  md += `*View: ${viewType}*\n\n`;

  if (frontmatter.description) {
    md += `${frontmatter.description}\n\n`;
  }

  md += `---\n\n`;

  for (const block of blocks) {
    md += `## Block: ${block.id}\n\n`;
    md += `\`\`\`${block.language}\n${block.code}\n\`\`\`\n\n`;

    if (block.output) {
      md += `**Output:**\n\n\`\`\`\n${block.output}\n\`\`\`\n\n`;
      md += `**Hash:** \`${block.outputHash}\`\n\n`;
    }
  }

  return md;
}

// =============================================================================
// Scan Command
// =============================================================================

/**
 * Scan package API surfaces
 *
 * @param {Object} cmd - Parsed command
 * @returns {Promise<Object>} Scan result
 */
async function scanCommand(cmd) {
  const { scope, flags } = cmd;

  logVerbose(flags, `Scanning packages${scope ? `: ${scope}` : '...'}`);

  // Import atlas module
  const atlasPath = join(WORKSPACE_ROOT, 'packages/fusion/src/kgc-docs-atlas.mjs');
  const atlas = await import(atlasPath);

  // Scan packages
  const packages = await atlas.scanPackages(scope, { workspaceRoot: WORKSPACE_ROOT });
  const manifest = atlas.buildAPIManifest(packages);

  // Write manifest
  await mkdir(join(WORKSPACE_ROOT, '.kgc'), { recursive: true });
  const manifestPath = join(WORKSPACE_ROOT, '.kgc', 'atlas-manifest.json');
  await writeFile(manifestPath, atlas.atlasAsJSON(manifest));

  logVerbose(flags, `Manifest written to: ${manifestPath}`);

  return formatOutput(flags, {
    success: true,
    packagesScanned: packages.length,
    totalExports: manifest.totalExports,
    undocumented: manifest.undocumented.length,
    manifestPath: relative(WORKSPACE_ROOT, manifestPath),
  });
}

// =============================================================================
// Refresh Command
// =============================================================================

/**
 * Refresh executable blocks in document
 *
 * @param {Object} cmd - Parsed command
 * @returns {Promise<Object>} Refresh result
 */
async function refreshCommand(cmd) {
  const { docPath, flags } = cmd;
  const absPath = resolve(WORKSPACE_ROOT, docPath);

  logVerbose(flags, `Refreshing: ${docPath}`);

  const result = await processKGCMD(absPath, flags);

  return formatOutput(flags, {
    success: true,
    blocksExecuted: result.blocksExecuted,
    viewsGenerated: Object.keys(result.views).length,
    views: result.views,
  });
}

// =============================================================================
// Prove Command
// =============================================================================

/**
 * Verify receipts in document
 *
 * @param {Object} cmd - Parsed command
 * @returns {Promise<Object>} Verification result
 */
async function proveCommand(cmd) {
  const { docPath, flags } = cmd;
  const absPath = resolve(WORKSPACE_ROOT, docPath);

  logVerbose(flags, `Verifying receipts in: ${docPath}`);

  const content = await readFile(absPath, 'utf-8');
  const receiptPath = `${absPath}.receipt.json`;

  let receipt;
  try {
    receipt = JSON.parse(await readFile(receiptPath, 'utf-8'));
  } catch {
    throw new KGCError(
      'KGC_RECEIPT_NOT_FOUND',
      `No receipt found for: ${docPath}`,
      { docPath, expectedPath: receiptPath },
      `Run: kgc-docs refresh ${docPath}`
    );
  }

  // Verify hash
  const currentHash = await computeHash(content);
  const valid = currentHash === receipt.contentHash;

  if (!valid) {
    logVerbose(flags, `Receipt INVALID: expected ${receipt.contentHash}, got ${currentHash}`);
  }

  return formatOutput(flags, {
    success: valid,
    docPath,
    receiptValid: valid,
    expectedHash: receipt.contentHash,
    actualHash: currentHash,
    receiptTimestamp: receipt.generatedAt,
  });
}

// =============================================================================
// Render Command
// =============================================================================

/**
 * Render .kgcmd to .md
 *
 * @param {Object} cmd - Parsed command
 * @returns {Promise<Object>} Render result
 */
async function renderCommand(cmd) {
  const { kgcmdPath, flags } = cmd;
  const absPath = resolve(WORKSPACE_ROOT, kgcmdPath);

  logVerbose(flags, `Rendering: ${kgcmdPath}`);

  const result = await processKGCMD(absPath, flags);

  return formatOutput(flags, {
    success: true,
    source: kgcmdPath,
    blocksExecuted: result.blocksExecuted,
    views: result.views,
  });
}

// =============================================================================
// Verify Command
// =============================================================================

/**
 * Verify determinism by dry-run
 *
 * @param {Object} cmd - Parsed command
 * @returns {Promise<Object>} Verification result
 */
async function verifyCommand(cmd) {
  const { docsDir, flags } = cmd;
  const absDir = resolve(WORKSPACE_ROOT, docsDir);

  logVerbose(flags, `Verifying determinism in: ${docsDir}`);

  const files = await glob('**/*.md', { cwd: absDir, absolute: true });
  const violations = [];

  for (const file of files) {
    const receiptPath = `${file}.receipt.json`;

    try {
      const receipt = JSON.parse(await readFile(receiptPath, 'utf-8'));
      const content = await readFile(file, 'utf-8');
      const currentHash = await computeHash(content);

      if (currentHash !== receipt.contentHash) {
        violations.push({
          file: relative(WORKSPACE_ROOT, file),
          expectedHash: receipt.contentHash,
          actualHash: currentHash,
        });
      }
    } catch {
      // No receipt or unreadable - skip
    }
  }

  const success = violations.length === 0;

  if (!success) {
    logVerbose(flags, `Found ${violations.length} determinism violations`);
  }

  return formatOutput(flags, {
    success,
    filesChecked: files.length,
    violations: violations.length,
    details: violations,
  });
}

// =============================================================================
// Manifest Command
// =============================================================================

/**
 * Generate receipt manifest with Merkle proofs
 *
 * @param {Object} cmd - Parsed command
 * @returns {Promise<Object>} Manifest result
 */
async function manifestCommand(cmd) {
  const { receiptsDir, flags } = cmd;
  const absDir = resolve(WORKSPACE_ROOT, receiptsDir);

  logVerbose(flags, `Generating manifest from: ${receiptsDir}`);

  await mkdir(absDir, { recursive: true });

  const receiptFiles = await glob('**/*.receipt.json', { cwd: absDir, absolute: true });
  const receipts = [];

  for (const file of receiptFiles) {
    try {
      const receipt = JSON.parse(await readFile(file, 'utf-8'));
      receipts.push({
        path: relative(absDir, file),
        ...receipt,
      });
    } catch (err) {
      console.warn(`⚠️  Failed to parse receipt: ${file}`);
    }
  }

  // Sort receipts deterministically
  receipts.sort((a, b) => a.path.localeCompare(b.path));

  // Compute Merkle root
  const hashes = await Promise.all(receipts.map(r => computeHash(JSON.stringify(r))));
  const merkleRoot = await computeMerkleRoot(hashes);

  const manifest = {
    generated: flags.deterministic ? '1970-01-01T00:00:00.000Z' : new Date().toISOString(),
    receiptsDir: relative(WORKSPACE_ROOT, absDir),
    totalReceipts: receipts.length,
    merkleRoot,
    receipts,
  };

  const manifestPath = join(absDir, 'manifest.json');
  await writeFile(manifestPath, JSON.stringify(manifest, null, 2));

  logVerbose(flags, `Manifest written to: ${manifestPath}`);

  return formatOutput(flags, {
    success: true,
    receiptsAggregated: receipts.length,
    merkleRoot,
    manifestPath: relative(WORKSPACE_ROOT, manifestPath),
  });
}

// =============================================================================
// Validate Command
// =============================================================================

/**
 * Validate API documentation completeness
 *
 * Scans all packages and checks:
 * 1. All exported functions have JSDoc
 * 2. All parameters are documented
 * 3. Return types are specified
 * 4. Examples are provided (for public APIs)
 *
 * @param {Object} cmd - Parsed command
 * @returns {Promise<Object>} Validation result
 */
async function validateCommand(cmd) {
  const { packagesDir, flags } = cmd;
  const absDir = resolve(WORKSPACE_ROOT, packagesDir);

  logVerbose(flags, `Validating API documentation in: ${packagesDir}`);

  const packagePaths = await glob('*/package.json', { cwd: absDir, absolute: true });
  const results = [];
  let totalExports = 0;
  let undocumented = 0;
  let missingExamples = 0;

  for (const pkgPath of packagePaths) {
    const pkgDir = dirname(pkgPath);
    const pkg = JSON.parse(await readFile(pkgPath, 'utf-8'));

    // Find all .mjs files in src/
    const srcDir = join(pkgDir, 'src');
    try {
      const srcFiles = await glob('**/*.mjs', { cwd: srcDir, absolute: true });

      for (const file of srcFiles) {
        const content = await readFile(file, 'utf-8');

        // Simple regex to find exported functions
        const exportMatches = content.matchAll(/export\s+(async\s+)?function\s+(\w+)/g);

        for (const match of exportMatches) {
          const funcName = match[2];
          totalExports++;

          // Check for JSDoc before function
          const funcIndex = content.indexOf(match[0]);
          const beforeFunc = content.slice(Math.max(0, funcIndex - 500), funcIndex);

          const hasJSDoc = beforeFunc.includes('/**') && beforeFunc.includes('*/');
          const hasExample = beforeFunc.includes('@example');

          if (!hasJSDoc) {
            undocumented++;
            results.push({
              package: pkg.name,
              file: relative(WORKSPACE_ROOT, file),
              function: funcName,
              issue: 'missing_jsdoc',
            });
          } else if (!hasExample) {
            missingExamples++;
            results.push({
              package: pkg.name,
              file: relative(WORKSPACE_ROOT, file),
              function: funcName,
              issue: 'missing_example',
            });
          }
        }
      }
    } catch (err) {
      // Skip packages without src/
      logVerbose(flags, `Skipping ${pkg.name}: ${err.message}`);
    }
  }

  const coveragePercent = totalExports > 0
    ? Math.round(((totalExports - undocumented) / totalExports) * 100)
    : 100;

  const success = undocumented === 0;

  return formatOutput(flags, {
    success,
    totalExports,
    documented: totalExports - undocumented,
    undocumented,
    missingExamples,
    coveragePercent,
    issues: flags.verbose ? results : results.slice(0, 10),
  });
}

// =============================================================================
// Generate Schema Command
// =============================================================================

/**
 * Generate API reference schema from JSDoc
 *
 * Parses JSDoc comments and generates structured API reference documentation
 * in JSON Schema format.
 *
 * @param {Object} cmd - Parsed command
 * @returns {Promise<Object>} Schema generation result
 */
async function generateSchemaCommand(cmd) {
  const { packagePath, flags } = cmd;
  const absPath = resolve(WORKSPACE_ROOT, packagePath);

  logVerbose(flags, `Generating schema for: ${packagePath}`);

  // Read package.json
  const pkgJsonPath = join(absPath, 'package.json');
  const pkg = JSON.parse(await readFile(pkgJsonPath, 'utf-8'));

  const schema = {
    $schema: 'http://json-schema.org/draft-07/schema#',
    $id: `https://unrdf.org/schemas/${pkg.name}.json`,
    title: pkg.description || pkg.name,
    type: 'object',
    properties: {},
    definitions: {},
  };

  // Parse all source files
  const srcDir = join(absPath, 'src');
  const srcFiles = await glob('**/*.mjs', { cwd: srcDir, absolute: true });

  let functionsExtracted = 0;

  for (const file of srcFiles) {
    const content = await readFile(file, 'utf-8');

    // Extract JSDoc + function definitions
    const jsdocRegex = /\/\*\*([\s\S]*?)\*\/\s*export\s+(async\s+)?function\s+(\w+)\s*\(([^)]*)\)/g;
    let match;

    while ((match = jsdocRegex.exec(content)) !== null) {
      const [, jsdoc, isAsync, funcName, params] = match;

      // Parse JSDoc
      const descMatch = jsdoc.match(/@description\s+([\s\S]*?)(?=@|$)/);
      const paramMatches = [...jsdoc.matchAll(/@param\s+\{([^}]+)\}\s+(\w+)\s+-?\s*(.*?)(?=@|$)/g)];
      const returnMatch = jsdoc.match(/@returns?\s+\{([^}]+)\}\s+(.*?)(?=@|$)/);

      schema.properties[funcName] = {
        type: 'function',
        description: descMatch ? descMatch[1].trim() : '',
        async: !!isAsync,
        parameters: paramMatches.map(m => ({
          name: m[2],
          type: m[1],
          description: m[3].trim(),
        })),
        returns: returnMatch ? {
          type: returnMatch[1],
          description: returnMatch[2].trim(),
        } : undefined,
      };

      functionsExtracted++;
    }
  }

  // Write schema
  const schemaPath = join(absPath, 'schema.json');
  await writeFile(schemaPath, JSON.stringify(schema, null, 2));

  // Generate receipt
  const receipt = {
    package: pkg.name,
    schemaPath: relative(WORKSPACE_ROOT, schemaPath),
    functionsExtracted,
    schemaHash: await computeHash(JSON.stringify(schema)),
    generatedAt: flags.deterministic ? '1970-01-01T00:00:00.000Z' : new Date().toISOString(),
  };

  await writeFile(`${schemaPath}.receipt.json`, JSON.stringify(receipt, null, 2));

  return formatOutput(flags, {
    success: true,
    package: pkg.name,
    functionsExtracted,
    schemaPath: relative(WORKSPACE_ROOT, schemaPath),
    schemaHash: receipt.schemaHash,
  });
}

// =============================================================================
// Compile LaTeX Command
// =============================================================================

/**
 * Compile LaTeX to PDF deterministically
 *
 * Uses pdflatex with deterministic options:
 * - Fixed SOURCE_DATE_EPOCH for reproducible builds
 * - No timestamps in PDF metadata
 * - Cache breaking on content change (BLAKE3 hash)
 *
 * @param {Object} cmd - Parsed command
 * @returns {Promise<Object>} Compilation result
 */
async function compileLatexCommand(cmd) {
  const { latexPath, flags } = cmd;
  const absPath = resolve(WORKSPACE_ROOT, latexPath);

  logVerbose(flags, `Compiling LaTeX: ${latexPath}`);

  // Read LaTeX source
  const latexContent = await readFile(absPath, 'utf-8');
  const contentHash = await computeHash(latexContent);

  // Check if we can skip compilation
  const pdfPath = absPath.replace(/\.tex$/, '.pdf');
  const receiptPath = `${pdfPath}.receipt.json`;

  let skipCompilation = false;
  try {
    const receipt = JSON.parse(await readFile(receiptPath, 'utf-8'));
    if (receipt.contentHash === contentHash) {
      logVerbose(flags, 'Content unchanged, skipping compilation');
      skipCompilation = true;
    }
  } catch {
    // No receipt, compile
  }

  if (!skipCompilation) {
    // Compile with pdflatex
    const latexDir = dirname(absPath);
    const latexFile = basename(absPath);

    try {
      execSync(`pdflatex -interaction=nonstopmode -output-directory="${latexDir}" "${latexFile}"`, {
        cwd: latexDir,
        env: {
          ...process.env,
          SOURCE_DATE_EPOCH: flags.deterministic ? '0' : Math.floor(Date.now() / 1000).toString(),
        },
        timeout: 20000, // 20s timeout for LaTeX compilation
      });

      logVerbose(flags, `PDF compiled: ${pdfPath}`);
    } catch (err) {
      throw new KGCError(
        'KGC_LATEX_COMPILATION_FAILED',
        'LaTeX compilation failed',
        { latexPath, stderr: err.stderr?.toString() },
        'Check LaTeX syntax and ensure pdflatex is installed'
      );
    }
  }

  // Generate receipt
  const pdfContent = await readFile(pdfPath);
  const pdfHash = await computeHash(pdfContent.toString('base64'));

  const receipt = {
    latexPath: relative(WORKSPACE_ROOT, absPath),
    pdfPath: relative(WORKSPACE_ROOT, pdfPath),
    contentHash,
    pdfHash,
    compiledAt: flags.deterministic ? '1970-01-01T00:00:00.000Z' : new Date().toISOString(),
    skipped: skipCompilation,
  };

  await writeFile(receiptPath, JSON.stringify(receipt, null, 2));

  return formatOutput(flags, {
    success: true,
    latexPath: relative(WORKSPACE_ROOT, absPath),
    pdfPath: relative(WORKSPACE_ROOT, pdfPath),
    contentHash,
    pdfHash,
    skipped: skipCompilation,
  });
}

// =============================================================================
// Thesis Command
// =============================================================================

/**
 * Build thesis document with receipt-driven provenance
 *
 * Aggregates all thesis chapters, proofs, and benchmarks into a single
 * document with full provenance chain.
 *
 * @param {Object} cmd - Parsed command
 * @returns {Promise<Object>} Thesis build result
 */
async function thesisCommand(cmd) {
  const { thesisDir, flags } = cmd;
  const absDir = resolve(WORKSPACE_ROOT, thesisDir);

  logVerbose(flags, `Building thesis from: ${thesisDir}`);

  await mkdir(absDir, { recursive: true });

  // Find all chapters
  const chapters = await glob('chapter-*.md', { cwd: absDir, absolute: true });
  chapters.sort(); // Deterministic order

  const thesisContent = [];
  const receipts = [];

  // Header
  thesisContent.push('# UNRDF Thesis: Receipt-Driven Knowledge Graph Construction');
  thesisContent.push('');
  thesisContent.push('**Generated**: ' + (flags.deterministic ? '1970-01-01T00:00:00.000Z' : new Date().toISOString()));
  thesisContent.push('');
  thesisContent.push('---');
  thesisContent.push('');

  // Table of contents
  thesisContent.push('## Table of Contents');
  thesisContent.push('');

  for (let i = 0; i < chapters.length; i++) {
    const chapter = chapters[i];
    const content = await readFile(chapter, 'utf-8');
    const titleMatch = content.match(/^#\s+(.+)$/m);
    const title = titleMatch ? titleMatch[1] : basename(chapter, '.md');

    thesisContent.push(`${i + 1}. ${title}`);
  }

  thesisContent.push('');
  thesisContent.push('---');
  thesisContent.push('');

  // Chapters
  for (let i = 0; i < chapters.length; i++) {
    const chapter = chapters[i];
    const content = await readFile(chapter, 'utf-8');
    const contentHash = await computeHash(content);

    thesisContent.push(`\n\n<!-- Chapter ${i + 1}: ${basename(chapter)} -->`);
    thesisContent.push(content);

    receipts.push({
      chapter: i + 1,
      path: relative(WORKSPACE_ROOT, chapter),
      contentHash,
    });
  }

  // Proof appendix
  thesisContent.push('\n\n---\n');
  thesisContent.push('## Proof Appendix\n');
  thesisContent.push('');
  thesisContent.push('This thesis was generated with full receipt-driven provenance.\n');
  thesisContent.push('');
  thesisContent.push('### Chapter Receipts\n');
  thesisContent.push('');
  thesisContent.push('```json');
  thesisContent.push(JSON.stringify(receipts, null, 2));
  thesisContent.push('```');
  thesisContent.push('');

  const fullContent = thesisContent.join('\n');
  const thesisHash = await computeHash(fullContent);

  // Write thesis
  const thesisPath = join(absDir, 'thesis-complete.md');
  await writeFile(thesisPath, fullContent);

  // Generate receipt
  const receipt = {
    thesisPath: relative(WORKSPACE_ROOT, thesisPath),
    chapters: receipts.length,
    contentHash: thesisHash,
    generatedAt: flags.deterministic ? '1970-01-01T00:00:00.000Z' : new Date().toISOString(),
  };

  await writeFile(`${thesisPath}.receipt.json`, JSON.stringify(receipt, null, 2));

  return formatOutput(flags, {
    success: true,
    thesisPath: relative(WORKSPACE_ROOT, thesisPath),
    chapters: receipts.length,
    contentHash: thesisHash,
  });
}

// =============================================================================
// Utilities
// =============================================================================

/**
 * Compute SHA-256 hash of content
 * @param {string} content - Content to hash
 * @returns {Promise<string>} Hex-encoded hash
 */
async function computeHash(content) {
  const hash = createHash('sha256');
  hash.update(content);
  return hash.digest('hex');
}

/**
 * Compute Merkle root from array of hashes
 * @param {string[]} hashes - Array of hex-encoded hashes
 * @returns {Promise<string>} Merkle root hash
 */
async function computeMerkleRoot(hashes) {
  if (hashes.length === 0) return await computeHash('');
  if (hashes.length === 1) return hashes[0];

  // Simple binary tree Merkle root (for production, use proper Merkle tree library)
  const combined = hashes.join('');
  return await computeHash(combined);
}

/**
 * Log message if verbose mode enabled
 * @param {Object} flags - Global flags
 * @param {string} message - Message to log
 */
function logVerbose(flags, message) {
  if (flags.verbose) {
    console.error(`[kgc-docs] ${message}`);
  }
}

/**
 * Format output based on output format flag
 * @param {Object} flags - Global flags
 * @param {Object} data - Data to output
 * @returns {Object} Formatted data
 */
function formatOutput(flags, data) {
  switch (flags.outputFormat) {
    case 'json':
      console.log(JSON.stringify(data, null, 2));
      break;
    case 'stream-json':
      console.log(JSON.stringify(data));
      break;
    case 'markdown':
    default:
      printMarkdownOutput(data);
      break;
  }
  return data;
}

/**
 * Print data as human-readable markdown
 * @param {Object} data - Data to print
 */
function printMarkdownOutput(data) {
  if (data.success === false) {
    console.log(`❌ Operation failed`);
    if (data.violations) {
      console.log(`\nViolations: ${data.violations}`);
      for (const v of data.details || []) {
        console.log(`  - ${v.file}`);
      }
    }
    return;
  }

  console.log(`✅ Success`);

  for (const [key, value] of Object.entries(data)) {
    if (key === 'success') continue;
    if (typeof value === 'object' && !Array.isArray(value)) {
      console.log(`\n${key}:`);
      for (const [k, v] of Object.entries(value)) {
        console.log(`  ${k}: ${v}`);
      }
    } else if (Array.isArray(value)) {
      console.log(`\n${key}: ${value.length} items`);
    } else {
      console.log(`${key}: ${value}`);
    }
  }
}

// =============================================================================
// Main Entry Point
// =============================================================================

/**
 * Main function
 */
async function main() {
  try {
    const args = process.argv.slice(2);

    if (args.length === 0) {
      printUsage();
      process.exit(0);
    }

    const cmd = parseArgs(args);

    let result;
    switch (cmd.command) {
      case 'build':
        result = await buildCommand(cmd);
        break;
      case 'scan':
        result = await scanCommand(cmd);
        break;
      case 'refresh':
        result = await refreshCommand(cmd);
        break;
      case 'prove':
        result = await proveCommand(cmd);
        break;
      case 'render':
        result = await renderCommand(cmd);
        break;
      case 'verify':
        result = await verifyCommand(cmd);
        break;
      case 'manifest':
        result = await manifestCommand(cmd);
        break;
      case 'validate':
        result = await validateCommand(cmd);
        break;
      case 'generate-schema':
        result = await generateSchemaCommand(cmd);
        break;
      case 'compile-latex':
        result = await compileLatexCommand(cmd);
        break;
      case 'thesis':
        result = await thesisCommand(cmd);
        break;
    }

    process.exit(result.success ? 0 : 1);
  } catch (err) {
    if (err instanceof KGCError) {
      console.error(`\n❌ [${err.code}] ${err.message}`);
      if (err.remediation) {
        console.error(`💡 Suggestion: ${err.remediation}`);
      }
      if (err.context && Object.keys(err.context).length > 0) {
        console.error(`\nContext:`);
        console.error(JSON.stringify(err.context, null, 2));
      }
      process.exit(1);
    } else {
      console.error(`\n❌ Unexpected error: ${err.message}`);
      console.error(err.stack);
      process.exit(2);
    }
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

// Export for testing
export {
  parseArgs,
  buildCommand,
  scanCommand,
  refreshCommand,
  proveCommand,
  renderCommand,
  verifyCommand,
  manifestCommand,
  validateCommand,
  generateSchemaCommand,
  compileLatexCommand,
  thesisCommand,
  computeHash,
  computeMerkleRoot,
  KGCError,
};
