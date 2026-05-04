/**
 * @fileoverview Π_docs - Documentation Generation Projections
 * Transforms KGC structures into Markdown documentation with code examples
 */

import { z } from 'zod';

/**
 * Documentation projection schema
 */
export const DocsProjectionSchema = z.object({
  type: z.literal('docs'),
  format: z.enum(['markdown', 'html', 'json']),
  content: z.string(),
  frontMatter: z.record(z.any()).optional(),
  sections: z.array(z.object({
    title: z.string(),
    level: z.number(),
    content: z.string(),
  })).optional(),
  crossRefs: z.array(z.object({
    source: z.string(),
    target: z.string(),
    type: z.string(),
  })).optional(),
});

/**
 * @typedef {z.infer<typeof DocsProjectionSchema>} DocsProjection
 */

/**
 * Escape markdown special characters
 * @param {string} text - Text to escape
 * @returns {string} Escaped text
 */
function escapeMarkdown(text) {
  return text.replace(/([*_`#\[\]()>])/g, '\\$1');
}

/**
 * Generate markdown heading
 * @param {string} text - Heading text
 * @param {number} level - Heading level (1-6)
 * @returns {string} Markdown heading
 */
function heading(text, level = 1) {
  return `${'#'.repeat(Math.min(level, 6))} ${text}`;
}

/**
 * Generate markdown code block
 * @param {string} code - Code content
 * @param {string} [language='javascript'] - Language for syntax highlighting
 * @returns {string} Markdown code block
 */
function codeBlock(code, language = 'javascript') {
  return `\`\`\`${language}\n${code}\n\`\`\``;
}

/**
 * Project receipt to documentation
 * @param {import('./receipt.mjs').Receipt} receipt - Receipt to document
 * @returns {DocsProjection} Documentation projection
 */
export function projectReceiptToDocs(receipt) {
  const sections = [
    {
      title: 'Overview',
      level: 2,
      content: `Receipt ID: \`${receipt.id}\`\n\nOperation: **${receipt.operation}**\n\nTimestamp: ${receipt.timestamp}`,
    },
    {
      title: 'Inputs',
      level: 2,
      content: codeBlock(JSON.stringify(receipt.inputs, null, 2), 'json'),
    },
    {
      title: 'Outputs',
      level: 2,
      content: codeBlock(JSON.stringify(receipt.outputs, null, 2), 'json'),
    },
    {
      title: 'Verification',
      level: 2,
      content: `Content Hash: \`${receipt.hash}\`` +
        (receipt.parentHash ? `\n\nParent Hash: \`${receipt.parentHash}\`` : ''),
    },
  ];

  const content = [
    heading('Receipt Documentation', 1),
    '',
    ...sections.map(s => [heading(s.title, s.level), '', s.content, ''].join('\n')),
  ].join('\n');

  return DocsProjectionSchema.parse({
    type: 'docs',
    format: 'markdown',
    content,
    frontMatter: {
      id: receipt.id,
      operation: receipt.operation,
      timestamp: receipt.timestamp,
    },
    sections,
  });
}

/**
 * Project schema to documentation with examples
 * @param {string} schemaName - Schema name
 * @param {import('zod').ZodType} schema - Zod schema
 * @param {Record<string, any>} [example] - Example object
 * @returns {DocsProjection} Documentation projection
 */
export function projectSchemaToDocs(schemaName, schema, example) {
  const sections = [
    {
      title: 'Schema Definition',
      level: 2,
      content: `\`${schemaName}\` - Type-safe schema for validation and type inference.`,
    },
  ];

  if (example) {
    sections.push({
      title: 'Example',
      level: 2,
      content: codeBlock(JSON.stringify(example, null, 2), 'json'),
    });

    sections.push({
      title: 'Usage',
      level: 2,
      content: codeBlock(
        `import { ${schemaName} } from '@unrdf/kgc-runtime/schemas';\n\n` +
        `const validated = ${schemaName}.parse(data);`,
        'javascript'
      ),
    });
  }

  const content = [
    heading(`${schemaName} Documentation`, 1),
    '',
    ...sections.map(s => [heading(s.title, s.level), '', s.content, ''].join('\n')),
  ].join('\n');

  return DocsProjectionSchema.parse({
    type: 'docs',
    format: 'markdown',
    content,
    frontMatter: {
      schema: schemaName,
      type: 'schema-documentation',
    },
    sections,
  });
}

/**
 * Generate API documentation from function
 * @param {Function} fn - Function to document
 * @param {object} metadata - Function metadata
 * @param {string} metadata.name - Function name
 * @param {string} metadata.description - Function description
 * @param {Array<{name: string, type: string, description: string}>} metadata.params - Parameters
 * @param {object} metadata.returns - Return value info
 * @param {string} [metadata.example] - Usage example
 * @returns {DocsProjection} Documentation projection
 */
export function projectFunctionToDocs(fn, metadata) {
  const sections = [
    {
      title: 'Description',
      level: 2,
      content: metadata.description,
    },
    {
      title: 'Parameters',
      level: 2,
      content: metadata.params.map(p =>
        `- **${p.name}** (\`${p.type}\`): ${p.description}`
      ).join('\n'),
    },
    {
      title: 'Returns',
      level: 2,
      content: `\`${metadata.returns.type}\` - ${metadata.returns.description}`,
    },
  ];

  if (metadata.example) {
    sections.push({
      title: 'Example',
      level: 2,
      content: codeBlock(metadata.example, 'javascript'),
    });
  }

  const content = [
    heading(`${metadata.name}()`, 1),
    '',
    ...sections.map(s => [heading(s.title, s.level), '', s.content, ''].join('\n')),
  ].join('\n');

  return DocsProjectionSchema.parse({
    type: 'docs',
    format: 'markdown',
    content,
    frontMatter: {
      function: metadata.name,
      type: 'api-documentation',
    },
    sections,
  });
}

/**
 * Generate cross-reference links in markdown
 * @param {string} text - Text with potential references
 * @param {Map<string, string>} refMap - Map of reference IDs to URLs
 * @returns {string} Text with markdown links
 */
export function generateCrossRefs(text, refMap) {
  let result = text;

  for (const [ref, url] of refMap.entries()) {
    const pattern = new RegExp(`\\b${ref}\\b`, 'g');
    result = result.replace(pattern, `[${ref}](${url})`);
  }

  return result;
}

/**
 * Generate table of contents from sections
 * @param {Array<{title: string, level: number}>} sections - Document sections
 * @returns {string} Markdown table of contents
 */
export function generateTableOfContents(sections) {
  const toc = sections.map(section => {
    const indent = '  '.repeat(section.level - 1);
    const anchor = section.title.toLowerCase().replace(/\s+/g, '-').replace(/[^\w-]/g, '');
    return `${indent}- [${section.title}](#${anchor})`;
  });

  return '## Table of Contents\n\n' + toc.join('\n');
}

/**
 * Project workflow to documentation
 * @param {object} workflow - Workflow definition
 * @param {string} workflow.id - Workflow ID
 * @param {string} workflow.name - Workflow name
 * @param {string} workflow.description - Workflow description
 * @param {Array<{id: string, type: string, description: string}>} workflow.steps - Workflow steps
 * @returns {DocsProjection} Documentation projection
 */
export function projectWorkflowToDocs(workflow) {
  const sections = [
    {
      title: 'Overview',
      level: 2,
      content: workflow.description,
    },
    {
      title: 'Workflow Steps',
      level: 2,
      content: workflow.steps.map((step, i) =>
        `${i + 1}. **${step.type}** (\`${step.id}\`)\n   ${step.description}`
      ).join('\n\n'),
    },
  ];

  const content = [
    heading(workflow.name, 1),
    '',
    ...sections.map(s => [heading(s.title, s.level), '', s.content, ''].join('\n')),
  ].join('\n');

  const crossRefs = workflow.steps.map((step, i) => ({
    source: workflow.id,
    target: step.id,
    type: 'step',
  }));

  return DocsProjectionSchema.parse({
    type: 'docs',
    format: 'markdown',
    content,
    frontMatter: {
      workflowId: workflow.id,
      name: workflow.name,
    },
    sections,
    crossRefs,
  });
}
