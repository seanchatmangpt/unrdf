/**
 * @file KGC Markdown Parser
 * @module kgc-docs/parser
 *
 * Parse KGC Markdown files with YAML frontmatter and specialized fenced blocks
 */

import { z } from 'zod';

/**
 * Frontmatter schema for KGC Markdown
 */
const FrontmatterSchema = z.object({
  o_hash: z.string().min(1),
  policy_id: z.string().min(1),
  bounds: z.array(z.any()),
  receipts: z.array(z.string()),
  views: z.array(z.string()),
  sources: z.array(z.string()),
});

/**
 * Parse YAML frontmatter from markdown
 * @param {string} markdown - Full markdown content
 * @returns {object} Parsed and validated frontmatter
 */
export function parseFrontmatter(markdown) {
  const frontmatterRegex = /^---\n([\s\S]*?)\n---/;
  const match = markdown.match(frontmatterRegex);

  if (!match) {
    throw new Error('No frontmatter found in markdown');
  }

  const yamlContent = match[1];
  const frontmatter = parseYAML(yamlContent);

  return FrontmatterSchema.parse(frontmatter);
}

/**
 * Simple YAML parser for frontmatter (supports basic structures)
 * @param {string} yaml - YAML content
 * @returns {object} Parsed object
 */
function parseYAML(yaml) {
  const result = {};
  const lines = yaml.split('\n').filter((line) => line.trim());

  let currentKey = null;
  let arrayMode = false;

  for (const line of lines) {
    const trimmed = line.trim();

    // Array item
    if (trimmed.startsWith('- ')) {
      const value = trimmed.slice(2).trim();
      if (currentKey && Array.isArray(result[currentKey])) {
        result[currentKey].push(value);
      }
      continue;
    }

    // Key-value pair
    if (trimmed.includes(':')) {
      const colonIndex = trimmed.indexOf(':');
      const key = trimmed.slice(0, colonIndex).trim();
      const value = trimmed.slice(colonIndex + 1).trim();

      if (value === '' || value === '[]') {
        // Array key
        result[key] = [];
        currentKey = key;
        arrayMode = true;
      } else if (value.startsWith('[') && value.endsWith(']')) {
        // Inline array
        const items = value
          .slice(1, -1)
          .split(',')
          .map((item) => item.trim());
        result[key] = items;
        currentKey = null;
        arrayMode = false;
      } else {
        // Simple value
        result[key] = value;
        currentKey = null;
        arrayMode = false;
      }
    }
  }

  return result;
}

/**
 * Parse fenced code blocks with KGC types
 * @param {string} markdown - Markdown content
 * @returns {object} Parsed block with type and content
 */
export function parseFencedBlock(markdown) {
  const fenceRegex = /```(kgc:\w+)\n([\s\S]*?)```/;
  const match = markdown.match(fenceRegex);

  if (!match) {
    throw new Error('No KGC fenced block found');
  }

  const fullType = match[1]; // e.g., "kgc:query"
  const type = fullType.split(':')[1]; // e.g., "query"
  const content = match[2].trim();

  return { type, content };
}

/**
 * Parse all fenced blocks from markdown
 * @param {string} markdown - Markdown content
 * @returns {Array<object>} Array of parsed blocks
 */
export function parseAllFencedBlocks(markdown) {
  const blocks = [];
  const fenceRegex = /```(kgc:\w+)\n([\s\S]*?)```/g;

  let match;
  while ((match = fenceRegex.exec(markdown)) !== null) {
    const fullType = match[1];
    const type = fullType.split(':')[1];
    const content = match[2].trim();
    blocks.push({ type, content });
  }

  return blocks;
}

/**
 * Build deterministic AST from KGC Markdown
 * @param {string} markdown - Full markdown content
 * @returns {object} Abstract syntax tree
 */
export function buildAST(markdown) {
  const frontmatter = parseFrontmatter(markdown);

  // Remove frontmatter to get body
  const bodyStart = markdown.indexOf('---', 3) + 4;
  const body = markdown.slice(bodyStart);

  const blocks = [];

  // Parse headings
  const headingRegex = /^(#{1,6})\s+(.+)$/gm;
  let match;
  while ((match = headingRegex.exec(body)) !== null) {
    blocks.push({
      type: 'heading',
      level: match[1].length,
      content: match[2],
    });
  }

  // Parse KGC fenced blocks
  const fencedBlocks = parseAllFencedBlocks(body);
  blocks.push(...fencedBlocks);

  return {
    frontmatter,
    blocks,
  };
}

/**
 * Parse complete KGC Markdown file
 * @param {string} markdown - Full markdown content
 * @returns {object} Parsed document with frontmatter and AST
 */
export function parseKGCMarkdown(markdown) {
  const ast = buildAST(markdown);
  return ast;
}
