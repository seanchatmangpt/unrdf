/**
 * Projection - Uniform surface parity via projections
 *
 * All surfaces are generated from the same universe and receipts:
 * UI = Π_ui(μ(O)), CLI = Π_cli(μ(O)), IDE = Π_ide(μ(O))
 *
 * "Missing features" become "missing projections" which can be implemented.
 * KGC gives a single source of truth with multiple projections.
 *
 * @module @unrdf/kgc-claude/projection
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { dataFactory } from '@unrdf/oxigraph';
import { now, toISO } from '@unrdf/kgc-4d';
import { GRAPHS } from './constants.mjs';

/**
 * Projection configuration schema
 */
export const ProjectionConfigSchema = z.object({
  /** Projection name */
  name: z.string(),
  /** Target surface */
  surface: z.enum(['cli', 'ide', 'ui', 'api', 'doc', 'custom']),
  /** SPARQL query to extract data */
  query: z.string().optional(),
  /** Graph URIs to project from */
  sourceGraphs: z.array(z.string()).default([GRAPHS.UNIVERSE]),
  /** Transform function name */
  transform: z.string().optional(),
  /** Output format */
  format: z.enum(['json', 'markdown', 'html', 'text', 'yaml', 'custom']).default('json'),
});

/**
 * @typedef {z.infer<typeof ProjectionConfigSchema>} ProjectionConfig
 */

/**
 * Projection result schema
 */
export const ProjectionResultSchema = z.object({
  id: z.string().uuid(),
  config: ProjectionConfigSchema,
  content: z.any(),
  contentHash: z.string(),
  sourceHash: z.string(),
  t_ns: z.bigint(),
  timestamp_iso: z.string(),
});

/**
 * @typedef {z.infer<typeof ProjectionResultSchema>} ProjectionResult
 */

/**
 * Generate UUID v4
 * @returns {string}
 */
function generateUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Registered transforms
 * @type {Map<string, Function>}
 */
const transforms = new Map();

/**
 * Projection registry
 * @type {Map<string, ProjectionConfig>}
 */
const projectionRegistry = new Map();

/**
 * Register a transform function
 *
 * @param {string} name - Transform name
 * @param {Function} fn - Transform function (data) => transformedData
 */
export function registerTransform(name, fn) {
  transforms.set(name, fn);
}

/**
 * Register a projection configuration
 *
 * @param {ProjectionConfig} config
 */
export function registerProjection(config) {
  const validated = ProjectionConfigSchema.parse(config);
  projectionRegistry.set(validated.name, validated);
}

/**
 * Project universe state to a surface
 *
 * @param {Object} store - KGCStore instance
 * @param {string|ProjectionConfig} projection - Projection name or config
 * @returns {Promise<ProjectionResult>}
 *
 * @example
 * registerProjection({
 *   name: 'run-history-cli',
 *   surface: 'cli',
 *   query: 'SELECT ?run ?status WHERE { ?run a kgcc:Run ; kgcc:status ?status }',
 *   format: 'text',
 * });
 *
 * const result = await project(store, 'run-history-cli');
 * console.log(result.content);
 */
export async function project(store, projection) {
  const config =
    typeof projection === 'string' ? projectionRegistry.get(projection) : ProjectionConfigSchema.parse(projection);

  if (!config) {
    throw new Error(`Projection not found: ${projection}`);
  }

  const t_ns = now();

  // 1. Compute source hash
  let sourceQuads = [];
  for (const graphUri of config.sourceGraphs) {
    const graph = dataFactory.namedNode(graphUri);
    sourceQuads = sourceQuads.concat([...store.match(null, null, null, graph)]);
  }

  const sourceContent = sourceQuads.map((q) => `${q.subject.value}|${q.predicate.value}|${q.object.value}`).join('\n');
  const sourceHash = await blake3(sourceContent);

  // 2. Extract data
  let data;
  if (config.query) {
    data = await store.query(config.query);
  } else {
    data = sourceQuads.map((q) => ({
      subject: q.subject.value,
      predicate: q.predicate.value,
      object: q.object.value,
      graph: q.graph.value,
    }));
  }

  // 3. Apply transform
  if (config.transform && transforms.has(config.transform)) {
    const transformFn = transforms.get(config.transform);
    data = transformFn(data);
  }

  // 4. Format output
  const content = formatOutput(data, config.format, config.surface);

  // 5. Compute content hash
  const contentHash = await blake3(typeof content === 'string' ? content : JSON.stringify(content));

  return ProjectionResultSchema.parse({
    id: generateUUID(),
    config,
    content,
    contentHash,
    sourceHash,
    t_ns,
    timestamp_iso: toISO(t_ns),
  });
}

/**
 * Format output based on format and surface
 *
 * @param {any} data
 * @param {string} format
 * @param {string} surface
 * @returns {any}
 */
function formatOutput(data, format, surface) {
  switch (format) {
    case 'json':
      return data;

    case 'text':
      return formatAsText(data, surface);

    case 'markdown':
      return formatAsMarkdown(data, surface);

    case 'html':
      return formatAsHtml(data, surface);

    case 'yaml':
      return formatAsYaml(data);

    default:
      return data;
  }
}

/**
 * Format as plain text
 * @param {any} data
 * @param {string} surface
 * @returns {string}
 */
function formatAsText(data, surface) {
  if (Array.isArray(data)) {
    if (surface === 'cli') {
      // CLI table format
      if (data.length === 0) return '(no results)';
      const keys = Object.keys(data[0]);
      const header = keys.join('\t');
      const rows = data.map((row) => keys.map((k) => row[k]?.value || row[k] || '').join('\t'));
      return [header, ...rows].join('\n');
    }
    return JSON.stringify(data, null, 2);
  }
  return String(data);
}

/**
 * Format as markdown
 * @param {any} data
 * @param {string} surface
 * @returns {string}
 */
function formatAsMarkdown(data, surface) {
  if (Array.isArray(data)) {
    if (data.length === 0) return '*No results*';
    const keys = Object.keys(data[0]);
    const header = `| ${keys.join(' | ')} |`;
    const separator = `| ${keys.map(() => '---').join(' | ')} |`;
    const rows = data.map((row) => `| ${keys.map((k) => row[k]?.value || row[k] || '').join(' | ')} |`);
    return [header, separator, ...rows].join('\n');
  }
  return `\`\`\`json\n${JSON.stringify(data, null, 2)}\n\`\`\``;
}

/**
 * Format as HTML
 * @param {any} data
 * @param {string} surface
 * @returns {string}
 */
function formatAsHtml(data, surface) {
  if (Array.isArray(data)) {
    if (data.length === 0) return '<p>No results</p>';
    const keys = Object.keys(data[0]);
    const header = `<tr>${keys.map((k) => `<th>${escapeHtml(k)}</th>`).join('')}</tr>`;
    const rows = data.map(
      (row) => `<tr>${keys.map((k) => `<td>${escapeHtml(row[k]?.value || row[k] || '')}</td>`).join('')}</tr>`,
    );
    return `<table>\n<thead>${header}</thead>\n<tbody>${rows.join('\n')}</tbody>\n</table>`;
  }
  return `<pre>${escapeHtml(JSON.stringify(data, null, 2))}</pre>`;
}

/**
 * Format as YAML
 * @param {any} data
 * @returns {string}
 */
function formatAsYaml(data) {
  // Simple YAML serialization
  return toYaml(data, 0);
}

/**
 * Simple YAML converter
 * @param {any} value
 * @param {number} indent
 * @returns {string}
 */
function toYaml(value, indent) {
  const prefix = '  '.repeat(indent);

  if (value === null || value === undefined) {
    return 'null';
  }

  if (typeof value === 'boolean' || typeof value === 'number') {
    return String(value);
  }

  if (typeof value === 'string') {
    if (value.includes('\n') || value.includes(':') || value.includes('#')) {
      return `|\n${prefix}  ${value.split('\n').join(`\n${prefix}  `)}`;
    }
    return value;
  }

  if (Array.isArray(value)) {
    if (value.length === 0) return '[]';
    return value.map((v) => `${prefix}- ${toYaml(v, indent + 1)}`).join('\n');
  }

  if (typeof value === 'object') {
    const entries = Object.entries(value);
    if (entries.length === 0) return '{}';
    return entries.map(([k, v]) => `${prefix}${k}: ${toYaml(v, indent + 1)}`).join('\n');
  }

  return String(value);
}

/**
 * Escape HTML special characters
 * @param {string} str
 * @returns {string}
 */
function escapeHtml(str) {
  return String(str)
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}

/**
 * Calculate projection drift
 *
 * drift(M_t) = distance between current projection and ideal projection
 *
 * @param {ProjectionResult} current - Current projection
 * @param {ProjectionResult} expected - Expected projection
 * @returns {number} Drift score (0 = perfect, higher = more drift)
 */
export function calculateProjectionDrift(current, expected) {
  if (current.contentHash === expected.contentHash) {
    return 0;
  }

  // Compare source hashes
  if (current.sourceHash !== expected.sourceHash) {
    return 1; // Source changed, full drift
  }

  // Same source, different content = transform/format drift
  // Estimate based on content similarity
  const currentStr = typeof current.content === 'string' ? current.content : JSON.stringify(current.content);
  const expectedStr = typeof expected.content === 'string' ? expected.content : JSON.stringify(expected.content);

  // Simple Jaccard similarity on tokens
  const currentTokens = new Set(currentStr.split(/\s+/));
  const expectedTokens = new Set(expectedStr.split(/\s+/));

  let intersection = 0;
  for (const token of currentTokens) {
    if (expectedTokens.has(token)) intersection++;
  }

  const union = currentTokens.size + expectedTokens.size - intersection;
  const similarity = union === 0 ? 1 : intersection / union;

  return 1 - similarity;
}

/**
 * Pre-defined projections for common surfaces
 */

// CLI: Run history projection
registerProjection({
  name: 'cli:run-history',
  surface: 'cli',
  query: `
    PREFIX kgcc: <http://kgc.io/claude/>
    SELECT ?runId ?status ?timestamp
    WHERE {
      GRAPH <${GRAPHS.RUN_CAPSULES}> {
        ?run kgcc:runId ?runId ;
             kgcc:runStatus ?status .
      }
    }
    ORDER BY DESC(?timestamp)
    LIMIT 20
  `,
  format: 'text',
});

// CLI: Checkpoint list
registerProjection({
  name: 'cli:checkpoints',
  surface: 'cli',
  sourceGraphs: [GRAPHS.SYSTEM],
  format: 'text',
});

// IDE: Active work items
registerProjection({
  name: 'ide:work-items',
  surface: 'ide',
  query: `
    PREFIX kgcc: <http://kgc.io/claude/>
    SELECT ?id ?status ?type
    WHERE {
      GRAPH <${GRAPHS.WORK_ITEMS}> {
        ?item kgcc:workItemId ?id ;
              kgcc:workItemStatus ?status .
      }
    }
  `,
  format: 'json',
});

// UI: Dashboard summary
registerProjection({
  name: 'ui:dashboard',
  surface: 'ui',
  sourceGraphs: [GRAPHS.UNIVERSE, GRAPHS.RUN_CAPSULES, GRAPHS.WORK_ITEMS],
  format: 'json',
});

// Doc: Markdown documentation
registerProjection({
  name: 'doc:run-manifest',
  surface: 'doc',
  query: `
    PREFIX kgcc: <http://kgc.io/claude/>
    SELECT ?runId ?status ?toolTrace ?artifacts
    WHERE {
      GRAPH <${GRAPHS.RUN_CAPSULES}> {
        ?run kgcc:runId ?runId ;
             kgcc:runStatus ?status ;
             kgcc:toolTrace ?toolTrace ;
             kgcc:artifacts ?artifacts .
      }
    }
  `,
  format: 'markdown',
});

/**
 * Get all registered projections
 * @returns {ProjectionConfig[]}
 */
export function getProjections() {
  return [...projectionRegistry.values()];
}

/**
 * Get projections for a surface
 * @param {string} surface
 * @returns {ProjectionConfig[]}
 */
export function getProjectionsForSurface(surface) {
  return [...projectionRegistry.values()].filter((p) => p.surface === surface);
}

/**
 * Clear all projections (for testing)
 */
export function clearProjections() {
  projectionRegistry.clear();
  transforms.clear();
}
