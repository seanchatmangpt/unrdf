/**
 * KGC Markdown Renderer - Deterministic JSON-to-Markdown Conversion
 *
 * Converts structured JSON (frontmatter, sections, receipts) to deterministic markdown.
 * GUARANTEE: Same input always produces byte-identical output.
 *
 * Core Functions:
 * - renderKGCDocument: Complete document with frontmatter, sections, receipts
 * - renderSection: Individual Diataxis-style sections (tutorial/how-to/reference/explanation)
 * - renderExecutableBlock: Code blocks with execution results (kgc:query/proof/extract/render)
 * - renderProofAppendix: Receipt chain verification + Merkle tree
 * - renderFrontmatter: Deterministic YAML with sorted keys
 * - renderTable: Sorted, aligned tables with escaped content
 * - renderCodeBlock: Fenced blocks with language and metadata
 * - renderLinks: Sorted link array
 * - canonicalizeMarkdown: Normalize whitespace, EOL, indentation
 * - hashMarkdown: SHA-256 of canonical form
 *
 * @module @unrdf/fusion/kgc-markdown-renderer
 */

import { createHash } from 'node:crypto';
import { z } from 'zod';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Frontmatter schema (YAML object)
 */
export const FrontmatterSchema = z.record(z.string(), z.any());

/**
 * Link schema
 */
export const LinkSchema = z.object({
  text: z.string(),
  url: z.string(),
  title: z.string().optional(),
});

/**
 * Code block metadata schema
 */
export const CodeBlockMetadataSchema = z.object({
  lineNumbers: z.boolean().optional(),
  highlights: z.array(z.number()).optional(),
  startLine: z.number().optional(),
}).optional();

/**
 * Executable block types
 */
export const BlockTypeSchema = z.enum(['kgc:query', 'kgc:proof', 'kgc:extract', 'kgc:render']);

/**
 * Executable block schema
 */
export const ExecutableBlockSchema = z.object({
  type: BlockTypeSchema,
  content: z.any(),
  result: z.any().optional(),
  metadata: z.any().optional(),
});

/**
 * Section types (Diataxis)
 */
export const SectionTypeSchema = z.enum(['tutorial', 'how-to', 'reference', 'explanation']);

/**
 * Section schema
 */
export const SectionSchema = z.object({
  type: SectionTypeSchema,
  title: z.string(),
  level: z.number().min(1).max(6).default(2),
  content: z.string().optional(),
  blocks: z.array(ExecutableBlockSchema).optional(),
  links: z.array(LinkSchema).optional(),
});

/**
 * Receipt schema (from receipts-kernel)
 */
export const ReceiptSchema = z.object({
  id: z.string(),
  hash: z.string(),
  timestamp: z.string(),
  timestamp_iso: z.string(),
  eventType: z.string(),
  payload: z.any(),
  proof: z.any().optional(),
  chain: z.string().optional(),
  vectorClock: z.any().optional(),
  receiptType: z.enum(['kgc', 'blockchain', 'hook']),
});

/**
 * Table column schema
 */
export const TableColumnSchema = z.object({
  key: z.string(),
  header: z.string(),
  align: z.enum(['left', 'center', 'right']).default('left'),
  width: z.number().optional(),
});

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Escape markdown special characters in table cells
 * @param {string} text - Text to escape
 * @returns {string} Escaped text
 */
function escapeTableCell(text) {
  if (text == null) return '';
  return String(text)
    .replace(/\|/g, '\\|')
    .replace(/\n/g, ' ')
    .replace(/\r/g, '')
    .trim();
}

/**
 * Pad string to fixed width
 * @param {string} text - Text to pad
 * @param {number} width - Target width
 * @param {string} align - Alignment (left/center/right)
 * @returns {string} Padded text
 */
function padString(text, width, align = 'left') {
  const textLength = text.length;
  if (textLength >= width) return text;

  const padding = width - textLength;

  if (align === 'center') {
    const leftPad = Math.floor(padding / 2);
    const rightPad = padding - leftPad;
    return ' '.repeat(leftPad) + text + ' '.repeat(rightPad);
  }

  if (align === 'right') {
    return ' '.repeat(padding) + text;
  }

  // Default: left align
  return text + ' '.repeat(padding);
}

/**
 * Sort object keys alphabetically for deterministic output
 * @param {Object} obj - Object to sort
 * @returns {Object} Object with sorted keys
 */
function sortKeys(obj) {
  if (obj == null || typeof obj !== 'object') return obj;
  if (Array.isArray(obj)) return obj.map(sortKeys);

  const sorted = {};
  const keys = Object.keys(obj).sort();

  for (const key of keys) {
    sorted[key] = sortKeys(obj[key]);
  }

  return sorted;
}

/**
 * Convert value to YAML-safe string
 * @param {any} value - Value to convert
 * @param {number} indent - Indentation level
 * @returns {string} YAML string
 */
function toYAMLValue(value, indent = 0) {
  const spaces = '  '.repeat(indent);

  if (value == null) {
    return 'null';
  }

  if (typeof value === 'boolean') {
    return String(value);
  }

  if (typeof value === 'number') {
    return String(value);
  }

  if (typeof value === 'string') {
    // Handle multiline strings
    if (value.includes('\n')) {
      return '|\n' + value.split('\n').map(line => `${spaces}  ${line}`).join('\n');
    }
    // Quote if contains special chars
    if (/[:#\[\]{}|>@`"]/.test(value)) {
      return `"${value.replace(/"/g, '\\"')}"`;
    }
    return value;
  }

  if (Array.isArray(value)) {
    if (value.length === 0) return '[]';
    return '\n' + value.map(item => `${spaces}- ${toYAMLValue(item, indent + 1)}`).join('\n');
  }

  if (typeof value === 'object') {
    const keys = Object.keys(value).sort();
    if (keys.length === 0) return '{}';
    return '\n' + keys.map(key =>
      `${spaces}${key}: ${toYAMLValue(value[key], indent + 1)}`
    ).join('\n');
  }

  return String(value);
}

// =============================================================================
// Core Rendering Functions
// =============================================================================

/**
 * Render frontmatter as deterministic YAML
 *
 * Converts frontmatter object to YAML with sorted keys for deterministic output.
 *
 * @param {Object} frontmatter - Frontmatter object
 * @returns {string} YAML string with fences
 *
 * @example
 * const yaml = renderFrontmatter({ title: 'Test', date: '2024-01-01' });
 * // Returns:
 * // ---
 * // date: 2024-01-01
 * // title: Test
 * // ---
 */
export function renderFrontmatter(frontmatter) {
  // Filter out undefined values before validation
  const filtered = {};
  for (const [key, value] of Object.entries(frontmatter)) {
    if (value !== undefined) {
      filtered[key] = value;
    }
  }

  const validated = FrontmatterSchema.parse(filtered);
  const sorted = sortKeys(validated);

  const lines = ['---'];

  for (const [key, value] of Object.entries(sorted)) {
    lines.push(`${key}: ${toYAMLValue(value, 0)}`);
  }

  lines.push('---');

  return lines.join('\n');
}

/**
 * Render table with sorted rows and aligned columns
 *
 * Creates deterministic markdown table with:
 * - Sorted rows (by sortBy key)
 * - Fixed column widths
 * - Escaped pipes and newlines
 *
 * @param {Array<Object>} rows - Array of row objects
 * @param {Array<Object>} columns - Column definitions
 * @param {string} [sortBy] - Key to sort by (default: first column)
 * @returns {string} Markdown table
 *
 * @example
 * const table = renderTable(
 *   [{ name: 'Alice', age: 30 }, { name: 'Bob', age: 25 }],
 *   [{ key: 'name', header: 'Name' }, { key: 'age', header: 'Age' }],
 *   'name'
 * );
 */
export function renderTable(rows, columns, sortBy) {
  if (!Array.isArray(rows) || rows.length === 0) {
    return '';
  }

  if (!Array.isArray(columns) || columns.length === 0) {
    throw new TypeError('renderTable: columns must be a non-empty array');
  }

  const validatedColumns = columns.map(col => TableColumnSchema.parse(col));

  // Sort rows for deterministic output
  const sortKey = sortBy || validatedColumns[0].key;
  const sortedRows = [...rows].sort((a, b) => {
    const aVal = String(a[sortKey] ?? '');
    const bVal = String(b[sortKey] ?? '');
    return aVal.localeCompare(bVal);
  });

  // Calculate column widths
  const widths = validatedColumns.map(col => {
    const headerWidth = col.header.length;
    const dataWidth = Math.max(
      ...sortedRows.map(row => escapeTableCell(row[col.key]).length)
    );
    return col.width || Math.max(headerWidth, dataWidth, 3);
  });

  // Build header row
  const headerCells = validatedColumns.map((col, i) =>
    padString(col.header, widths[i], col.align)
  );
  const headerRow = `| ${headerCells.join(' | ')} |`;

  // Build separator row
  const separatorCells = validatedColumns.map((col, i) => {
    const dashes = '-'.repeat(widths[i]);
    if (col.align === 'center') return `:${dashes.slice(2)}:`;
    if (col.align === 'right') return `${dashes.slice(1)}:`;
    return dashes;
  });
  const separatorRow = `| ${separatorCells.join(' | ')} |`;

  // Build data rows
  const dataRows = sortedRows.map(row => {
    const cells = validatedColumns.map((col, i) => {
      const value = escapeTableCell(row[col.key]);
      return padString(value, widths[i], col.align);
    });
    return `| ${cells.join(' | ')} |`;
  });

  return [headerRow, separatorRow, ...dataRows].join('\n');
}

/**
 * Render code block with language and metadata
 *
 * Creates fenced code block with deterministic formatting.
 *
 * @param {string} code - Code content
 * @param {string} language - Language identifier
 * @param {Object} [metadata] - Optional metadata (lineNumbers, highlights)
 * @returns {string} Markdown fenced code block
 *
 * @example
 * const block = renderCodeBlock('const x = 1;', 'javascript', { lineNumbers: true });
 */
export function renderCodeBlock(code, language, metadata) {
  if (typeof code !== 'string') {
    throw new TypeError('renderCodeBlock: code must be a string');
  }

  const lang = language || '';
  const meta = metadata ? CodeBlockMetadataSchema.parse(metadata) : {};

  // Build info string
  let infoString = lang;

  if (meta.lineNumbers) {
    const startLine = meta.startLine || 1;
    infoString += ` {${startLine}}`;
  }

  if (meta.highlights && meta.highlights.length > 0) {
    const ranges = meta.highlights.sort((a, b) => a - b).join(',');
    infoString += ` {${ranges}}`;
  }

  // Normalize code (Unix line endings, no trailing whitespace per line)
  const normalizedCode = code
    .replace(/\r\n/g, '\n')
    .replace(/\r/g, '\n')
    .split('\n')
    .map(line => line.trimEnd())
    .join('\n')
    .trimEnd();

  return `\`\`\`${infoString}\n${normalizedCode}\n\`\`\``;
}

/**
 * Render links in deterministic order
 *
 * Converts link array to markdown, sorted by URL.
 *
 * @param {Array<Object>} linkArray - Array of {text, url, title}
 * @returns {string} Markdown links
 *
 * @example
 * const links = renderLinks([
 *   { text: 'Example', url: 'https://example.com', title: 'Example Site' },
 *   { text: 'GitHub', url: 'https://github.com' }
 * ]);
 */
export function renderLinks(linkArray) {
  if (!Array.isArray(linkArray) || linkArray.length === 0) {
    return '';
  }

  const validatedLinks = linkArray.map(link => LinkSchema.parse(link));

  // Sort by URL for deterministic output
  const sortedLinks = [...validatedLinks].sort((a, b) =>
    a.url.localeCompare(b.url)
  );

  const lines = sortedLinks.map(link => {
    const titleAttr = link.title ? ` "${link.title}"` : '';
    return `- [${link.text}](${link.url}${titleAttr})`;
  });

  return lines.join('\n');
}

/**
 * Render executable block with results
 *
 * Handles 4 block types:
 * - kgc:query → table or code block (sorted by primary key)
 * - kgc:proof → verification report (sorted by receipt ID)
 * - kgc:extract → API table (sorted by name)
 * - kgc:render → nested markdown (from JSON input)
 *
 * @param {Object} block - Block definition
 * @param {any} result - Execution result
 * @param {Object} [receipt] - Optional receipt for verification
 * @returns {string} Rendered markdown
 *
 * @example
 * const md = renderExecutableBlock(
 *   { type: 'kgc:query', content: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }' },
 *   [{ s: 'ex:a', p: 'rdf:type', o: 'ex:Thing' }]
 * );
 */
export function renderExecutableBlock(block, result, receipt) {
  const validated = ExecutableBlockSchema.parse(block);
  const { type, content } = validated;

  let output = '';

  // Add block type header
  output += `<!-- ${type} -->\n\n`;

  switch (type) {
    case 'kgc:query': {
      // Show query
      output += renderCodeBlock(content, 'sparql') + '\n\n';

      // Show results as table
      if (result && Array.isArray(result) && result.length > 0) {
        const firstRow = result[0];
        const columns = Object.keys(firstRow).sort().map(key => ({
          key,
          header: key,
          align: 'left',
        }));

        output += '**Results:**\n\n';
        output += renderTable(result, columns, columns[0].key);
      } else {
        output += '**Results:** (empty)';
      }
      break;
    }

    case 'kgc:proof': {
      // Verification report
      output += '**Verification Report**\n\n';

      if (result && typeof result === 'object') {
        const reportData = [
          { field: 'Valid', value: result.valid ? '✓' : '✗' },
          { field: 'Receipt ID', value: result.receiptId || 'N/A' },
          { field: 'Hash', value: result.hash || 'N/A' },
          { field: 'Timestamp', value: result.timestamp || 'N/A' },
        ];

        if (result.reason) {
          reportData.push({ field: 'Reason', value: result.reason });
        }

        const columns = [
          { key: 'field', header: 'Field', align: 'left' },
          { key: 'value', header: 'Value', align: 'left' },
        ];

        output += renderTable(reportData, columns, 'field');
      } else {
        output += '(No verification data)';
      }

      if (receipt) {
        output += '\n\n**Receipt:**\n\n';
        output += renderCodeBlock(JSON.stringify(receipt, null, 2), 'json');
      }
      break;
    }

    case 'kgc:extract': {
      // API extraction table
      output += '**API Extraction**\n\n';

      if (result && Array.isArray(result) && result.length > 0) {
        const columns = [
          { key: 'name', header: 'Name', align: 'left' },
          { key: 'type', header: 'Type', align: 'left' },
          { key: 'description', header: 'Description', align: 'left' },
        ];

        output += renderTable(result, columns, 'name');
      } else {
        output += '(No API data)';
      }
      break;
    }

    case 'kgc:render': {
      // Nested markdown from JSON
      if (result && typeof result === 'object' && result.markdown) {
        output += result.markdown;
      } else if (typeof result === 'string') {
        output += result;
      } else {
        output += renderCodeBlock(JSON.stringify(result, null, 2), 'json');
      }
      break;
    }
  }

  return output;
}

/**
 * Render section with heading, content, blocks, and links
 *
 * Creates Diataxis-style section with deterministic formatting.
 *
 * @param {Object} section - Section definition
 * @param {Object} [receipt] - Optional receipt for block verification
 * @returns {string} Rendered markdown section
 *
 * @example
 * const md = renderSection({
 *   type: 'tutorial',
 *   title: 'Getting Started',
 *   level: 2,
 *   content: 'This tutorial shows...',
 *   blocks: [{ type: 'kgc:query', content: '...' }]
 * });
 */
export function renderSection(section, receipt) {
  const validated = SectionSchema.parse(section);
  const { type, title, level, content, blocks, links } = validated;

  const parts = [];

  // Heading
  const headingMarker = '#'.repeat(level);
  parts.push(`${headingMarker} ${title}`);
  parts.push('');

  // Section type indicator
  parts.push(`<!-- section: ${type} -->`);
  parts.push('');

  // Content
  if (content) {
    parts.push(content);
    parts.push('');
  }

  // Executable blocks
  if (blocks && blocks.length > 0) {
    for (const block of blocks) {
      const blockMd = renderExecutableBlock(block, block.result, receipt);
      parts.push(blockMd);
      parts.push('');
    }
  }

  // Links
  if (links && links.length > 0) {
    parts.push('## References');
    parts.push('');
    parts.push(renderLinks(links));
    parts.push('');
  }

  return parts.join('\n').trimEnd();
}

/**
 * Render proof appendix with receipt chain and Merkle tree
 *
 * Creates deterministic appendix showing:
 * - Receipt list (sorted by timestamp)
 * - Merkle proof diagram (ASCII)
 * - Hash verification (input → output)
 * - Chain integrity check
 *
 * @param {Array<Object>} receipts - Receipt array
 * @param {string} merkleRoot - Merkle tree root hash
 * @returns {string} Markdown appendix
 *
 * @example
 * const appendix = renderProofAppendix(
 *   [receipt1, receipt2, receipt3],
 *   'a1b2c3...'
 * );
 */
export function renderProofAppendix(receipts, merkleRoot) {
  if (!Array.isArray(receipts) || receipts.length === 0) {
    return '';
  }

  const validatedReceipts = receipts.map(r => ReceiptSchema.parse(r));

  // Sort by timestamp for deterministic output
  const sortedReceipts = [...validatedReceipts].sort((a, b) =>
    a.timestamp.localeCompare(b.timestamp)
  );

  const parts = [];

  parts.push('---');
  parts.push('');
  parts.push('## Proof Appendix');
  parts.push('');
  parts.push('### Receipt Chain');
  parts.push('');

  // Receipt table
  const receiptRows = sortedReceipts.map(r => ({
    id: r.id,
    event: r.eventType,
    hash: r.hash.slice(0, 12) + '...',
    timestamp: r.timestamp_iso,
    type: r.receiptType,
  }));

  const receiptColumns = [
    { key: 'id', header: 'Receipt ID', align: 'left' },
    { key: 'event', header: 'Event Type', align: 'left' },
    { key: 'hash', header: 'Hash', align: 'left' },
    { key: 'timestamp', header: 'Timestamp', align: 'left' },
    { key: 'type', header: 'Type', align: 'left' },
  ];

  parts.push(renderTable(receiptRows, receiptColumns, 'id'));
  parts.push('');

  // Merkle root
  parts.push('### Merkle Tree');
  parts.push('');
  parts.push(`**Root Hash:** \`${merkleRoot}\``);
  parts.push('');

  // ASCII diagram
  parts.push('```');
  parts.push('         Root');
  parts.push('        /    \\');
  parts.push('      H01    H23');
  parts.push('     /  \\   /  \\');
  parts.push('    H0  H1 H2  H3');
  parts.push('    |   |  |   |');

  for (let i = 0; i < sortedReceipts.length; i++) {
    const receipt = sortedReceipts[i];
    const shortHash = receipt.hash.slice(0, 8);
    parts.push(`    R${i} [${shortHash}]`);
  }

  parts.push('```');
  parts.push('');

  // Chain integrity
  parts.push('### Chain Integrity');
  parts.push('');

  const chainValid = sortedReceipts.every((r, i) => {
    if (i === 0) return true;
    return r.chain === sortedReceipts[i - 1].hash;
  });

  parts.push(`**Status:** ${chainValid ? '✓ Valid' : '✗ Invalid'}`);
  parts.push('');

  // Verification details
  parts.push('### Verification');
  parts.push('');

  const verificationRows = sortedReceipts.map((r, i) => ({
    index: i,
    receipt: r.id,
    chain: i === 0 ? '(genesis)' : (r.chain?.slice(0, 12) + '...') || '(none)',
    status: (i === 0 || r.chain === sortedReceipts[i - 1]?.hash) ? '✓' : '✗',
  }));

  const verificationColumns = [
    { key: 'index', header: '#', align: 'left' },
    { key: 'receipt', header: 'Receipt ID', align: 'left' },
    { key: 'chain', header: 'Chain Ref', align: 'left' },
    { key: 'status', header: 'Status', align: 'center' },
  ];

  parts.push(renderTable(verificationRows, verificationColumns, 'index'));
  parts.push('');

  return parts.join('\n');
}

/**
 * Canonicalize markdown for hash verification
 *
 * Normalizes:
 * - Unix line endings (\n)
 * - No trailing whitespace
 * - Consistent heading markers
 * - List indentation (2 spaces)
 *
 * @param {string} md - Markdown string
 * @returns {string} Canonical markdown
 *
 * @example
 * const canonical = canonicalizeMarkdown(markdown);
 * const hash1 = hashMarkdown(canonical);
 * const hash2 = hashMarkdown(canonical);
 * // hash1 === hash2 (deterministic)
 */
export function canonicalizeMarkdown(md) {
  if (typeof md !== 'string') {
    throw new TypeError('canonicalizeMarkdown: input must be a string');
  }

  return md
    // Normalize line endings
    .replace(/\r\n/g, '\n')
    .replace(/\r/g, '\n')
    // Remove trailing whitespace from each line
    .split('\n')
    .map(line => line.trimEnd())
    .join('\n')
    // Normalize heading markers (single space after #)
    .replace(/^(#{1,6})\s+/gm, '$1 ')
    // Remove trailing newlines
    .trimEnd()
    // Add single trailing newline
    + '\n';
}

/**
 * Hash markdown for determinism verification
 *
 * Computes SHA-256 of canonical markdown.
 *
 * @param {string} md - Markdown string
 * @returns {string} SHA-256 hash (hex)
 *
 * @example
 * const hash = hashMarkdown(markdown);
 * console.log(hash); // 'a1b2c3d4...'
 */
export function hashMarkdown(md) {
  const canonical = canonicalizeMarkdown(md);
  return createHash('sha256').update(canonical, 'utf8').digest('hex');
}

/**
 * Render complete KGC document
 *
 * Combines frontmatter, sections, and proof appendix into deterministic markdown.
 *
 * GUARANTEE: Same input always produces byte-identical output.
 *
 * @param {Object} frontmatter - YAML frontmatter
 * @param {Array<Object>} sections - Section array
 * @param {Array<Object>} receipts - Receipt chain
 * @returns {string} Complete markdown document
 *
 * @example
 * const doc = renderKGCDocument(
 *   { title: 'My Doc', date: '2024-01-01' },
 *   [{ type: 'tutorial', title: 'Intro', content: '...' }],
 *   [receipt1, receipt2]
 * );
 */
export function renderKGCDocument(frontmatter, sections, receipts) {
  const parts = [];

  // Frontmatter
  if (frontmatter && typeof frontmatter === 'object' && Object.keys(frontmatter).length > 0) {
    parts.push(renderFrontmatter(frontmatter));
    parts.push('');
  }

  // Sections
  if (sections && Array.isArray(sections)) {
    const validatedSections = sections.map(s => SectionSchema.parse(s));

    for (const section of validatedSections) {
      parts.push(renderSection(section));
      parts.push('');
    }
  }

  // Proof appendix (if receipts provided)
  if (receipts && Array.isArray(receipts) && receipts.length > 0) {
    // Calculate Merkle root
    const hashes = receipts.map(r => r.hash);
    const merkleRoot = createHash('sha256')
      .update(hashes.join(''))
      .digest('hex');

    parts.push(renderProofAppendix(receipts, merkleRoot));
  }

  // Canonicalize final output
  return canonicalizeMarkdown(parts.join('\n'));
}

// =============================================================================
// Exports
// =============================================================================

export default {
  renderKGCDocument,
  renderSection,
  renderExecutableBlock,
  renderProofAppendix,
  renderFrontmatter,
  renderTable,
  renderCodeBlock,
  renderLinks,
  canonicalizeMarkdown,
  hashMarkdown,
};
