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
  version: z.string().optional(), // Semver version tracking
  author: z.string().optional(), // Document author
  created: z.string().optional(), // Creation date
  updated: z.string().optional(), // Last update date
  status: z.string().optional(), // Document status (draft, active, deprecated)
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
 * Parse cross-document references from markdown links
 * @param {string} markdown - Markdown content
 * @returns {Array<object>} Array of cross-references
 */
export function parseCrossReferences(markdown) {
  const references = [];
  const linkRegex = /\[([^\]]+)\]\(([^)]+)\)/g;

  let match;
  while ((match = linkRegex.exec(markdown)) !== null) {
    const text = match[1];
    const href = match[2];

    // Check if it's a cross-document reference (relative path)
    if (href.startsWith('../') || href.startsWith('./')) {
      const [path, anchor] = href.split('#');
      references.push({
        text,
        path,
        anchor: anchor || null,
        full_href: href,
      });
    }
  }

  return references;
}

/**
 * Parse paragraphs from markdown
 * @param {string} markdown - Markdown content
 * @returns {Array<object>} Array of paragraph blocks
 */
export function parseParagraphs(markdown) {
  const paragraphs = [];
  // Split by double newlines but exclude code blocks and headings
  const lines = markdown.split('\n');
  let currentParagraph = '';
  let inCodeBlock = false;

  for (const line of lines) {
    if (line.trim().startsWith('```')) {
      inCodeBlock = !inCodeBlock;
      if (currentParagraph.trim()) {
        paragraphs.push({
          type: 'paragraph',
          content: currentParagraph.trim(),
        });
        currentParagraph = '';
      }
      continue;
    }

    if (inCodeBlock) continue;
    if (line.trim().startsWith('#')) continue; // Skip headings
    if (line.trim().startsWith('|')) continue; // Skip tables
    if (line.trim().startsWith('-') || line.trim().startsWith('*')) continue; // Skip lists
    if (line.trim().startsWith('>')) continue; // Skip blockquotes

    if (line.trim() === '') {
      if (currentParagraph.trim()) {
        paragraphs.push({
          type: 'paragraph',
          content: currentParagraph.trim(),
        });
        currentParagraph = '';
      }
    } else {
      currentParagraph += (currentParagraph ? ' ' : '') + line.trim();
    }
  }

  if (currentParagraph.trim()) {
    paragraphs.push({
      type: 'paragraph',
      content: currentParagraph.trim(),
    });
  }

  return paragraphs;
}

/**
 * Parse lists from markdown (both ordered and unordered)
 * @param {string} markdown - Markdown content
 * @returns {Array<object>} Array of list blocks
 */
export function parseLists(markdown) {
  const lists = [];
  const lines = markdown.split('\n');
  let currentList = null;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const trimmed = line.trim();

    // Unordered list item
    if (trimmed.match(/^[-*+]\s+/)) {
      const content = trimmed.replace(/^[-*+]\s+/, '');
      const indent = line.search(/\S/);

      if (!currentList || currentList.ordered) {
        if (currentList) lists.push(currentList);
        currentList = { type: 'list', ordered: false, items: [] };
      }

      currentList.items.push({ content, indent });
    }
    // Ordered list item
    else if (trimmed.match(/^\d+\.\s+/)) {
      const content = trimmed.replace(/^\d+\.\s+/, '');
      const indent = line.search(/\S/);

      if (!currentList || !currentList.ordered) {
        if (currentList) lists.push(currentList);
        currentList = { type: 'list', ordered: true, items: [] };
      }

      currentList.items.push({ content, indent });
    }
    // End of list
    else if (trimmed === '' && currentList) {
      lists.push(currentList);
      currentList = null;
    }
  }

  if (currentList) lists.push(currentList);

  return lists;
}

/**
 * Parse tables from markdown
 * @param {string} markdown - Markdown content
 * @returns {Array<object>} Array of table blocks
 */
export function parseTables(markdown) {
  const tables = [];
  const lines = markdown.split('\n');
  let currentTable = null;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i].trim();

    if (line.startsWith('|') && line.endsWith('|')) {
      if (!currentTable) {
        currentTable = { type: 'table', headers: [], rows: [] };
      }

      const cells = line
        .split('|')
        .slice(1, -1)
        .map((cell) => cell.trim());

      // Check if separator row
      if (cells.every((cell) => /^:?-+:?$/.test(cell))) {
        continue; // Skip separator
      }

      if (currentTable.headers.length === 0) {
        currentTable.headers = cells;
      } else {
        currentTable.rows.push(cells);
      }
    } else if (currentTable) {
      tables.push(currentTable);
      currentTable = null;
    }
  }

  if (currentTable) tables.push(currentTable);

  return tables;
}

/**
 * Parse blockquotes from markdown
 * @param {string} markdown - Markdown content
 * @returns {Array<object>} Array of blockquote blocks
 */
export function parseBlockquotes(markdown) {
  const blockquotes = [];
  const lines = markdown.split('\n');
  let currentQuote = null;

  for (const line of lines) {
    if (line.trim().startsWith('>')) {
      const content = line.trim().replace(/^>\s*/, '');
      if (!currentQuote) {
        currentQuote = { type: 'blockquote', content: '' };
      }
      currentQuote.content += (currentQuote.content ? '\n' : '') + content;
    } else if (currentQuote) {
      blockquotes.push(currentQuote);
      currentQuote = null;
    }
  }

  if (currentQuote) blockquotes.push(currentQuote);

  return blockquotes;
}

/**
 * Parse images from markdown
 * @param {string} markdown - Markdown content
 * @returns {Array<object>} Array of image blocks
 */
export function parseImages(markdown) {
  const images = [];
  const imageRegex = /!\[([^\]]*)\]\(([^)]+)\)/g;

  let match;
  while ((match = imageRegex.exec(markdown)) !== null) {
    images.push({
      type: 'image',
      alt: match[1],
      src: match[2],
    });
  }

  return images;
}

/**
 * Parse standard code blocks (non-KGC)
 * @param {string} markdown - Markdown content
 * @returns {Array<object>} Array of code block objects
 */
export function parseCodeBlocks(markdown) {
  const codeBlocks = [];
  const codeRegex = /```(\w+)?\n([\s\S]*?)```/g;

  let match;
  while ((match = codeRegex.exec(markdown)) !== null) {
    const language = match[1] || '';
    const content = match[2].trim();

    // Skip KGC blocks (handled separately)
    if (language.startsWith('kgc:')) continue;

    codeBlocks.push({
      type: 'code',
      language,
      content,
      executable: false,
    });
  }

  return codeBlocks;
}

/**
 * Parse executable code blocks (kgc:executable)
 * @param {string} markdown - Markdown content
 * @returns {Array<object>} Array of executable code blocks
 */
export function parseExecutableBlocks(markdown) {
  const execBlocks = [];
  const execRegex = /```kgc:executable\n([\s\S]*?)```/g;

  let match;
  while ((match = execRegex.exec(markdown)) !== null) {
    execBlocks.push({
      type: 'code',
      language: 'javascript',
      content: match[1].trim(),
      executable: true,
    });
  }

  return execBlocks;
}

/**
 * Extract metadata from paragraph text
 * @param {Array<object>} paragraphs - Paragraph blocks
 * @returns {object} Extracted metadata
 */
export function extractMetadata(paragraphs) {
  const metadata = {
    authors: [],
    dates: {},
    status: null,
    tags: [],
    category: null,
  };

  for (const para of paragraphs) {
    const content = para.content;

    // Extract authors
    const authorMatch = content.match(/(?:Author|By):\s*([^\n]+)/i);
    if (authorMatch) {
      metadata.authors.push(authorMatch[1].trim());
    }

    // Extract dates
    const createdMatch = content.match(/Created:\s*([^\n]+)/i);
    if (createdMatch) {
      metadata.dates.created = createdMatch[1].trim();
    }

    const updatedMatch = content.match(/Updated:\s*([^\n]+)/i);
    if (updatedMatch) {
      metadata.dates.updated = updatedMatch[1].trim();
    }

    // Extract status
    const statusMatch = content.match(/Status:\s*(\w+)/i);
    if (statusMatch) {
      metadata.status = statusMatch[1].toLowerCase();
    }

    // Extract tags
    const tagsMatch = content.match(/Tags:\s*([^\n]+)/i);
    if (tagsMatch) {
      metadata.tags = tagsMatch[1]
        .split(',')
        .map((tag) => tag.trim())
        .filter(Boolean);
    }

    // Extract category
    const categoryMatch = content.match(/Category:\s*(\w+)/i);
    if (categoryMatch) {
      metadata.category = categoryMatch[1].toLowerCase();
    }
  }

  return metadata;
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

  // Parse all markdown elements
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

  // Parse full markdown elements
  const paragraphs = parseParagraphs(body);
  blocks.push(...paragraphs);

  const lists = parseLists(body);
  blocks.push(...lists);

  const tables = parseTables(body);
  blocks.push(...tables);

  const blockquotes = parseBlockquotes(body);
  blocks.push(...blockquotes);

  const images = parseImages(body);
  blocks.push(...images);

  const codeBlocks = parseCodeBlocks(body);
  blocks.push(...codeBlocks);

  const execBlocks = parseExecutableBlocks(body);
  blocks.push(...execBlocks);

  // Parse cross-references
  const crossReferences = parseCrossReferences(body);

  // Extract metadata
  const metadata = extractMetadata(paragraphs);

  return {
    frontmatter,
    blocks,
    crossReferences,
    metadata,
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
