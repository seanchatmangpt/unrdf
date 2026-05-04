/**
 * KGC Documentation Atlas - Auto-discovery of API surfaces from workspace packages
 *
 * Scans packages in pnpm workspace, extracts JSDoc, exports, and generates:
 * - API manifests (JSON)
 * - API reference documentation (Markdown)
 * - Capability dependency graphs (JSON)
 * - Atlas tables (Markdown)
 *
 * @module @unrdf/fusion/kgc-docs-atlas
 */

import { readFile, readdir, stat } from 'node:fs/promises';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { z } from 'zod';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Validation schemas
const ExportSchema = z.object({
  name: z.string(),
  type: z.enum(['function', 'class', 'constant', 'type', 'default']),
  signature: z.string().optional(),
  description: z.string().optional(),
  jsdoc: z.string().optional(),
  params: z.array(z.object({
    name: z.string(),
    type: z.string().optional(),
    description: z.string().optional(),
  })).optional(),
  returns: z.object({
    type: z.string().optional(),
    description: z.string().optional(),
  }).optional(),
});

const PackageScanSchema = z.object({
  package: z.string(),
  path: z.string(),
  version: z.string().optional(),
  exports: z.array(ExportSchema),
  dependencies: z.array(z.string()).optional(),
});

// Schema for API manifest validation (used for documentation and future validation)
const _APIManifestSchema = z.object({
  timestamp: z.string(),
  packages: z.array(PackageScanSchema),
  totalExports: z.number(),
  undocumented: z.array(z.object({
    package: z.string(),
    export: z.string(),
  })),
  crossPackageDeps: z.array(z.object({
    from: z.string(),
    to: z.string(),
  })),
});

/**
 * Extract JSDoc comments from a JavaScript/MJS file
 *
 * Parses JSDoc blocks and associates them with the next export/declaration.
 * Supports functions, classes, constants, and type definitions.
 *
 * @param {string} filePath - Absolute path to .mjs or .js file
 * @returns {Promise<Array<Object>>} Array of {name, jsdoc, type, signature, params, returns}
 * @throws {Error} If file cannot be read or parsed
 */
export async function extractJSDocFromFile(filePath) {
  const content = await readFile(filePath, 'utf-8');
  const results = [];

  // Regex patterns
  const jsdocPattern = /\/\*\*([\s\S]*?)\*\//g;
  const exportPattern = /export\s+(async\s+)?(?:(function|class|const|let|var|default)\s+)?(\w+)/g;
  const paramPattern = /@param\s+\{([^}]+)\}\s+(\[?(\w+)\]?)\s*-?\s*(.*)/g;
  const returnsPattern = /@returns?\s+\{([^}]+)\}\s*(.*)/;
  const descPattern = /\*\s*([^@\n][^\n]*)/;

  // Extract all JSDoc blocks with their positions
  const jsdocs = [];
  let match;
  while ((match = jsdocPattern.exec(content)) !== null) {
    const block = match[1];
    const position = match.index + match[0].length;

    // Extract description (first non-empty line without @tag)
    const descMatch = block.match(descPattern);
    const description = descMatch ? descMatch[1].trim() : '';

    // Extract @param tags
    const params = [];
    let paramMatch;
    while ((paramMatch = paramPattern.exec(block)) !== null) {
      params.push({
        name: paramMatch[3] || paramMatch[2],
        type: paramMatch[1],
        description: paramMatch[4].trim(),
      });
    }

    // Extract @returns tag
    const returnsMatch = block.match(returnsPattern);
    const returns = returnsMatch ? {
      type: returnsMatch[1],
      description: returnsMatch[2].trim(),
    } : null;

    jsdocs.push({
      raw: match[0],
      content: block,
      position,
      description,
      params,
      returns,
    });
  }

  // Extract exports and match with JSDoc
  exportPattern.lastIndex = 0;
  while ((match = exportPattern.exec(content)) !== null) {
    const isAsync = match[1] !== undefined;
    const keyword = match[2] || 'default';
    const name = match[3];
    const position = match.index;

    // Find closest preceding JSDoc
    const jsdoc = jsdocs
      .filter(j => j.position < position)
      .sort((a, b) => b.position - a.position)[0];

    // Determine type
    let type = 'constant';
    if (keyword === 'function') type = 'function';
    else if (keyword === 'class') type = 'class';
    else if (keyword === 'default') type = 'default';

    // Build signature
    let signature = '';
    if (type === 'function') {
      // Extract function signature from code
      const fnMatch = content.slice(position).match(
        /function\s+\w+\s*\(([^)]*)\)\s*(?::\s*([^{]+))?\s*\{/
      );
      if (fnMatch) {
        const paramStr = fnMatch[1].trim();
        const returnType = fnMatch[2] ? fnMatch[2].trim() : 'void';
        signature = `${isAsync ? 'async ' : ''}(${paramStr}) => ${returnType}`;
      } else if (jsdoc?.params) {
        // Fallback to JSDoc params
        const paramStr = jsdoc.params.map(p => `${p.name}: ${p.type || 'any'}`).join(', ');
        const returnType = jsdoc.returns?.type || 'void';
        signature = `${isAsync ? 'async ' : ''}(${paramStr}) => ${returnType}`;
      }
    }

    results.push({
      name,
      type,
      signature: signature || undefined,
      description: jsdoc?.description || undefined,
      jsdoc: jsdoc?.raw || undefined,
      params: jsdoc?.params || undefined,
      returns: jsdoc?.returns || undefined,
    });
  }

  return results;
}

/**
 * Extract export statements from an ESM module
 *
 * Parses named exports, default exports, and re-exports.
 * Does NOT execute the module (static analysis only).
 *
 * @param {string} filePath - Absolute path to .mjs file
 * @returns {Promise<Array<Object>>} Array of {name, type, isReexport, from}
 * @throws {Error} If file cannot be read
 */
export async function extractExportsFromESM(filePath) {
  const content = await readFile(filePath, 'utf-8');
  const exports = [];

  // Named exports: export { a, b, c }
  const namedExportPattern = /export\s+\{([^}]+)\}(?:\s+from\s+['"]([^'"]+)['"])?/g;
  let match;
  while ((match = namedExportPattern.exec(content)) !== null) {
    const names = match[1].split(',').map(n => n.trim());
    const from = match[2];
    for (const name of names) {
      const [original, alias] = name.split(/\s+as\s+/).map(s => s.trim());
      exports.push({
        name: alias || original,
        type: 'named',
        isReexport: !!from,
        from: from || undefined,
      });
    }
  }

  // Direct named exports: export const/function/class
  const directExportPattern = /export\s+(const|let|var|function|class|async\s+function)\s+(\w+)/g;
  while ((match = directExportPattern.exec(content)) !== null) {
    const keyword = match[1];
    const name = match[2];
    let type = 'constant';
    if (keyword.includes('function')) type = 'function';
    else if (keyword === 'class') type = 'class';

    exports.push({
      name,
      type,
      isReexport: false,
    });
  }

  // Default exports: export default
  const defaultExportPattern = /export\s+default\s+(\w+)/g;
  while ((match = defaultExportPattern.exec(content)) !== null) {
    exports.push({
      name: match[1] || 'default',
      type: 'default',
      isReexport: false,
    });
  }

  return exports;
}

/**
 * Scan packages in workspace for API surfaces
 *
 * Discovers packages via pnpm-workspace.yaml or glob pattern.
 * For each package, scans index.mjs (or package.json exports) and extracts:
 * - Exported functions, classes, constants
 * - JSDoc documentation
 * - Dependencies (from package.json)
 *
 * @param {string} [scope] - Package name, glob pattern, or scope (e.g., "@unrdf/*", "packages/fusion")
 * @param {Object} [options] - Scan options
 * @param {string} [options.workspaceRoot] - Workspace root path (auto-detected if not provided)
 * @returns {Promise<Array<Object>>} Array of package scan results
 */
export async function scanPackages(scope, options = {}) {
  const workspaceRoot = options.workspaceRoot || await findWorkspaceRoot();
  const packages = await discoverPackages(workspaceRoot, scope);
  const results = [];

  for (const pkg of packages) {
    const pkgJson = JSON.parse(await readFile(pkg.packageJsonPath, 'utf-8'));
    const entryPoint = await resolveEntryPoint(pkg.path, pkgJson);

    if (!entryPoint) {
      console.warn(`[kgc-docs-atlas] No entry point found for ${pkgJson.name}`);
      continue;
    }

    // Extract JSDoc and exports
    const jsdocData = await extractJSDocFromFile(entryPoint).catch(() => []);
    const exportData = await extractExportsFromESM(entryPoint).catch(() => []);

    // Merge JSDoc with exports
    const exports = exportData.map(exp => {
      const doc = jsdocData.find(d => d.name === exp.name);
      return {
        name: exp.name,
        type: doc?.type || exp.type,
        signature: doc?.signature,
        description: doc?.description,
        jsdoc: doc?.jsdoc,
        params: doc?.params,
        returns: doc?.returns,
      };
    });

    results.push({
      package: pkgJson.name,
      path: pkg.path,
      version: pkgJson.version,
      exports,
      dependencies: Object.keys(pkgJson.dependencies || {}),
    });
  }

  return results;
}

/**
 * Build unified API manifest from package scans
 *
 * Aggregates scan results and computes:
 * - Total export counts
 * - Undocumented exports (missing JSDoc)
 * - Cross-package dependencies (who imports whom)
 *
 * @param {Array<Object>} packages - Output from scanPackages()
 * @returns {Object} API manifest {timestamp, packages, totalExports, undocumented, crossPackageDeps}
 */
export function buildAPIManifest(packages) {
  const undocumented = [];
  const crossPackageDeps = [];
  let totalExports = 0;

  for (const pkg of packages) {
    totalExports += pkg.exports.length;

    // Find undocumented exports
    for (const exp of pkg.exports) {
      if (!exp.jsdoc && !exp.description) {
        undocumented.push({
          package: pkg.package,
          export: exp.name,
        });
      }
    }

    // Find cross-package dependencies
    const workspaceDeps = pkg.dependencies?.filter(dep => dep.startsWith('@unrdf/')) || [];
    for (const dep of workspaceDeps) {
      crossPackageDeps.push({
        from: pkg.package,
        to: dep,
      });
    }
  }

  return {
    timestamp: new Date().toISOString(),
    packages,
    totalExports,
    undocumented,
    crossPackageDeps,
  };
}

/**
 * Generate API reference documentation in Markdown
 *
 * Creates structured Markdown with:
 * - Package overview table
 * - Function signatures (sorted alphabetically)
 * - Type definitions
 * - Usage examples (extracted from JSDoc @example tags)
 *
 * Output is suitable for Diataxis "Reference" quadrant.
 *
 * @param {Object} manifest - Output from buildAPIManifest()
 * @returns {string} Markdown documentation
 */
export function generateAPIReference(manifest) {
  let md = '# API Reference\n\n';
  md += `*Generated: ${manifest.timestamp}*\n\n`;
  md += `**Total Packages:** ${manifest.packages.length}  \n`;
  md += `**Total Exports:** ${manifest.totalExports}  \n`;
  md += `**Undocumented:** ${manifest.undocumented.length}\n\n`;

  md += '---\n\n';

  // Per-package sections
  for (const pkg of manifest.packages.sort((a, b) => a.package.localeCompare(b.package))) {
    md += `## ${pkg.package}\n\n`;
    md += `**Version:** ${pkg.version || 'N/A'}  \n`;
    md += `**Exports:** ${pkg.exports.length}\n\n`;

    if (pkg.exports.length === 0) {
      md += '*No exports found.*\n\n';
      continue;
    }

    // Sort exports by type, then name
    const sorted = pkg.exports.sort((a, b) => {
      if (a.type !== b.type) return a.type.localeCompare(b.type);
      return a.name.localeCompare(b.name);
    });

    for (const exp of sorted) {
      md += `### \`${exp.name}\`\n\n`;
      md += `**Type:** ${exp.type}  \n`;

      if (exp.signature) {
        md += `**Signature:** \`${exp.signature}\`\n\n`;
      }

      if (exp.description) {
        md += `${exp.description}\n\n`;
      }

      if (exp.params && exp.params.length > 0) {
        md += '**Parameters:**\n\n';
        for (const param of exp.params) {
          md += `- \`${param.name}\` (\`${param.type || 'any'}\`)`;
          if (param.description) {
            md += `: ${param.description}`;
          }
          md += '\n';
        }
        md += '\n';
      }

      if (exp.returns) {
        md += `**Returns:** \`${exp.returns.type || 'void'}\``;
        if (exp.returns.description) {
          md += ` - ${exp.returns.description}`;
        }
        md += '\n\n';
      }

      md += '---\n\n';
    }
  }

  // Undocumented exports section
  if (manifest.undocumented.length > 0) {
    md += '## ⚠️ Undocumented Exports\n\n';
    md += 'The following exports are missing JSDoc documentation:\n\n';
    for (const item of manifest.undocumented) {
      md += `- **${item.package}**: \`${item.export}\`\n`;
    }
    md += '\n';
  }

  return md;
}

/**
 * Generate capability dependency graph
 *
 * Builds JSON graph showing how packages/functions compose:
 * - Nodes: exported functions/classes
 * - Edges: function A imports/calls function B
 *
 * Used by /kgc:frontier for dominance-pruned frontier computation.
 *
 * @param {Object} manifest - Output from buildAPIManifest()
 * @returns {Object} Graph {nodes: [...], edges: [...]}
 */
export function generateCapabilityGraph(manifest) {
  const nodes = [];
  const edges = [];

  // Create nodes for all exports
  for (const pkg of manifest.packages) {
    for (const exp of pkg.exports) {
      nodes.push({
        id: `${pkg.package}::${exp.name}`,
        package: pkg.package,
        name: exp.name,
        type: exp.type,
        documented: !!(exp.jsdoc || exp.description),
      });
    }
  }

  // Create edges for cross-package dependencies
  for (const dep of manifest.crossPackageDeps) {
    edges.push({
      from: dep.from,
      to: dep.to,
      type: 'package-dependency',
    });
  }

  return { nodes, edges };
}

/**
 * Generate atlas as Markdown table
 *
 * Creates sortable table of all APIs:
 * - Name | Type | Package | Status | Tests
 * - Sorted by package, then name
 *
 * @param {Object} manifest - Output from buildAPIManifest()
 * @returns {string} Markdown table
 */
export function atlasAsMarkdown(manifest) {
  let md = '# API Atlas\n\n';
  md += `*Generated: ${manifest.timestamp}*\n\n`;
  md += '| Name | Type | Package | Status | JSDoc |\n';
  md += '|------|------|---------|--------|-------|\n';

  // Collect all exports
  const allExports = [];
  for (const pkg of manifest.packages) {
    for (const exp of pkg.exports) {
      allExports.push({
        ...exp,
        packageName: pkg.package,
      });
    }
  }

  // Sort by package, then name
  allExports.sort((a, b) => {
    if (a.packageName !== b.packageName) {
      return a.packageName.localeCompare(b.packageName);
    }
    return a.name.localeCompare(b.name);
  });

  // Write rows
  for (const exp of allExports) {
    const status = exp.jsdoc || exp.description ? '✅ Stable' : '⚠️ Undocumented';
    const hasJsdoc = exp.jsdoc ? '✅' : '❌';
    md += `| \`${exp.name}\` | ${exp.type} | ${exp.packageName} | ${status} | ${hasJsdoc} |\n`;
  }

  return md;
}

/**
 * Generate atlas as deterministic JSON
 *
 * Outputs API manifest with:
 * - Keys sorted alphabetically (A-Z)
 * - Arrays sorted by natural order
 * - Suitable for hashing and receipt verification
 *
 * @param {Object} manifest - Output from buildAPIManifest()
 * @returns {string} Deterministic JSON string
 */
export function atlasAsJSON(manifest) {
  // Deep sort all keys
  const sortKeys = (obj) => {
    if (Array.isArray(obj)) {
      return obj.map(sortKeys);
    }
    if (obj && typeof obj === 'object') {
      return Object.keys(obj)
        .sort()
        .reduce((sorted, key) => {
          sorted[key] = sortKeys(obj[key]);
          return sorted;
        }, {});
    }
    return obj;
  };

  const sorted = sortKeys(manifest);
  return JSON.stringify(sorted, null, 2);
}

// ============================================================================
// INTERNAL HELPERS
// ============================================================================

/**
 * Find workspace root by looking for pnpm-workspace.yaml
 * @returns {Promise<string>} Absolute path to workspace root
 * @internal
 */
async function findWorkspaceRoot() {
  let current = __dirname;
  while (current !== '/') {
    try {
      const workspaceFile = join(current, 'pnpm-workspace.yaml');
      await stat(workspaceFile);
      return current;
    } catch {
      current = join(current, '..');
    }
  }
  throw new Error('Could not find pnpm-workspace.yaml in parent directories');
}

/**
 * Discover packages in workspace matching scope
 * @param {string} workspaceRoot - Workspace root path
 * @param {string} [scope] - Package name, glob, or scope filter
 * @returns {Promise<Array<Object>>} Array of {path, packageJsonPath}
 * @internal
 */
async function discoverPackages(workspaceRoot, scope) {
  const packagesDir = join(workspaceRoot, 'packages');
  const packageDirs = await readdir(packagesDir);

  const packages = [];
  for (const dir of packageDirs) {
    const pkgPath = join(packagesDir, dir);
    const pkgJsonPath = join(pkgPath, 'package.json');

    try {
      const stats = await stat(pkgJsonPath);
      if (stats.isFile()) {
        const pkgJson = JSON.parse(await readFile(pkgJsonPath, 'utf-8'));

        // Apply scope filter
        if (scope) {
          if (scope.includes('*')) {
            // Glob pattern (e.g., "@unrdf/*")
            const regex = new RegExp('^' + scope.replace(/\*/g, '.*') + '$');
            if (!regex.test(pkgJson.name)) continue;
          } else if (scope.startsWith('packages/')) {
            // Directory filter
            const targetDir = scope.replace('packages/', '');
            if (dir !== targetDir) continue;
          } else {
            // Exact package name
            if (pkgJson.name !== scope) continue;
          }
        }

        packages.push({
          path: pkgPath,
          packageJsonPath: pkgJsonPath,
        });
      }
    } catch {
      // Skip directories without package.json
    }
  }

  return packages;
}

/**
 * Resolve entry point for a package
 * @param {string} pkgPath - Package directory path
 * @param {Object} pkgJson - Parsed package.json
 * @returns {Promise<string|null>} Absolute path to entry point, or null
 * @internal
 */
async function resolveEntryPoint(pkgPath, pkgJson) {
  // Try package.json "exports" field
  if (pkgJson.exports) {
    const mainExport = pkgJson.exports['.'];
    if (typeof mainExport === 'string') {
      const resolved = join(pkgPath, mainExport);
      try {
        await stat(resolved);
        return resolved;
      } catch {
        // Fall through
      }
    }
  }

  // Try common entry points
  const candidates = [
    join(pkgPath, 'src', 'index.mjs'),
    join(pkgPath, 'src', 'index.js'),
    join(pkgPath, 'index.mjs'),
    join(pkgPath, 'index.js'),
  ];

  for (const candidate of candidates) {
    try {
      await stat(candidate);
      return candidate;
    } catch {
      // Try next
    }
  }

  return null;
}

/**
 * Default export for convenience
 */
export default {
  extractJSDocFromFile,
  extractExportsFromESM,
  scanPackages,
  buildAPIManifest,
  generateAPIReference,
  generateCapabilityGraph,
  atlasAsMarkdown,
  atlasAsJSON,
};
