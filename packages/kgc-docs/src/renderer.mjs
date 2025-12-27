/**
 * @file KGC Markdown Renderer
 * @module kgc-docs/renderer
 *
 * Render KGC AST into 4 Diataxis documentation views
 */

import { generateProofTree } from './proof.mjs';

/**
 * Render tutorial view (learning-oriented)
 * @param {object} ast - KGC AST
 * @returns {string} Tutorial markdown
 */
export function renderTutorial(ast) {
  const { frontmatter, blocks } = ast;

  let output = `# ${getMainHeading(blocks)} - Tutorial\n\n`;
  output += `> **Learning Path**: Get started with ${getMainHeading(blocks)}\n\n`;

  output += `## Getting Started\n\n`;
  output += `This tutorial will help you learn the fundamentals step by step.\n\n`;

  // Extract learning content
  output += `### What You'll Learn\n\n`;
  for (const block of blocks) {
    if (block.type === 'heading' && block.level >= 2) {
      output += `- ${block.content}\n`;
    }
  }
  output += `\n`;

  // Add query examples if present
  const queryBlocks = blocks.filter((b) => b.type === 'query');
  if (queryBlocks.length > 0) {
    output += `### Example Queries\n\n`;
    for (const query of queryBlocks) {
      output += `\`\`\`sparql\n${query.content}\n\`\`\`\n\n`;
    }
  }

  return output;
}

/**
 * Render how-to view (problem-solving oriented)
 * @param {object} ast - KGC AST
 * @returns {string} How-to markdown
 */
export function renderHowTo(ast) {
  const { frontmatter, blocks } = ast;

  let output = `# How to Use ${getMainHeading(blocks)}\n\n`;
  output += `> **Goal**: Solve specific problems and accomplish tasks\n\n`;

  output += `## Common Tasks\n\n`;

  // Extract actionable content
  const headings = blocks.filter((b) => b.type === 'heading' && b.level >= 2);
  for (let i = 0; i < headings.length; i++) {
    output += `### ${i + 1}. ${headings[i].content}\n\n`;
    output += `Follow these steps to accomplish this task:\n\n`;
  }

  // Add query blocks as examples
  const queryBlocks = blocks.filter((b) => b.type === 'query');
  if (queryBlocks.length > 0) {
    output += `### Query Examples\n\n`;
    for (const query of queryBlocks) {
      output += `\`\`\`sparql\n${query.content}\n\`\`\`\n\n`;
    }
  }

  return output;
}

/**
 * Render reference view (information-oriented)
 * @param {object} ast - KGC AST
 * @returns {string} Reference markdown
 */
export function renderReference(ast) {
  const { frontmatter, blocks } = ast;

  let output = `# ${getMainHeading(blocks)} - API Reference\n\n`;
  output += `> **Reference Documentation**: Technical specifications and API details\n\n`;

  // Metadata table
  output += `## Metadata\n\n`;
  output += `| Property | Value |\n`;
  output += `|----------|-------|\n`;
  output += `| O-Hash | \`${frontmatter.o_hash}\` |\n`;
  output += `| Policy ID | \`${frontmatter.policy_id}\` |\n`;
  output += `| Receipts | ${frontmatter.receipts.length} |\n`;
  output += `| Sources | ${frontmatter.sources.length} |\n`;
  output += `\n`;

  // Parameters section
  output += `## Parameters\n\n`;
  if (frontmatter.bounds && frontmatter.bounds.length > 0) {
    output += `### Bounds\n\n`;
    output += `\`\`\`json\n${JSON.stringify(frontmatter.bounds, null, 2)}\n\`\`\`\n\n`;
  }

  // Returns section
  output += `## Returns\n\n`;
  output += `This API returns structured data according to the KGC specification.\n\n`;

  // Query reference
  const queryBlocks = blocks.filter((b) => b.type === 'query');
  if (queryBlocks.length > 0) {
    output += `## Query Reference\n\n`;
    for (let i = 0; i < queryBlocks.length; i++) {
      output += `### Query ${i + 1}\n\n`;
      output += `\`\`\`sparql\n${queryBlocks[i].content}\n\`\`\`\n\n`;
    }
  }

  return output;
}

/**
 * Render explanation view (understanding-oriented)
 * @param {object} ast - KGC AST
 * @returns {string} Explanation markdown
 */
export function renderExplanation(ast) {
  const { frontmatter, blocks } = ast;

  let output = `# Understanding ${getMainHeading(blocks)}\n\n`;
  output += `> **Conceptual Overview**: Learn the background and theory\n\n`;

  output += `## Why This Matters\n\n`;
  output += `This concept is fundamental to understanding Knowledge Graph Circuits.\n\n`;

  output += `## Background\n\n`;
  output += `The KGC framework provides:\n\n`;
  output += `- Deterministic proof generation\n`;
  output += `- Receipt-based verification\n`;
  output += `- O-Hash linkage for integrity\n\n`;

  // Extract conceptual content from headings
  const headings = blocks.filter((b) => b.type === 'heading');
  if (headings.length > 0) {
    output += `## Key Concepts\n\n`;
    for (const heading of headings) {
      if (heading.level >= 2) {
        output += `### ${heading.content}\n\n`;
        output += `This concept explains the underlying principles.\n\n`;
      }
    }
  }

  // Add proof explanation
  const proofBlocks = blocks.filter((b) => b.type === 'proof');
  if (proofBlocks.length > 0) {
    output += `## Proof Structure\n\n`;
    output += `Proofs use Merkle trees for efficient verification.\n\n`;
  }

  return output;
}

/**
 * Generate proof appendix for documentation
 * @param {object} ast - KGC AST
 * @returns {string} Proof appendix markdown
 */
export function generateProofAppendix(ast) {
  const { frontmatter } = ast;
  const proof = generateProofTree(ast);

  let output = `## Proof Appendix\n\n`;
  output += `### Verification Data\n\n`;

  output += `\`\`\`json\n`;
  output += JSON.stringify(
    {
      merkle_root: proof.merkle_root,
      o_hash: frontmatter.o_hash,
      receipt_count: frontmatter.receipts.length,
      timestamp: proof.timestamp,
    },
    null,
    2
  );
  output += `\n\`\`\`\n\n`;

  output += `### Receipt Hashes\n\n`;
  for (const receipt of frontmatter.receipts) {
    output += `- \`${receipt}\`\n`;
  }
  output += `\n`;

  output += `### O-Hash Linkage\n\n`;
  output += `The document is cryptographically linked to o_hash \`${frontmatter.o_hash}\` via Merkle tree.\n\n`;

  if (proof.linkage && proof.linkage.length > 0) {
    output += `### Receipt Linkages\n\n`;
    output += `\`\`\`json\n`;
    output += JSON.stringify(proof.linkage, null, 2);
    output += `\n\`\`\`\n`;
  }

  return output;
}

/**
 * Render complete Diataxis view with proof appendix
 * @param {object} ast - KGC AST
 * @param {string} view - View type (tutorial|how-to|reference|explanation)
 * @returns {string} Complete markdown document
 */
export function renderDiataxisView(ast, view) {
  let content = '';

  switch (view) {
    case 'tutorial':
      content = renderTutorial(ast);
      break;
    case 'how-to':
      content = renderHowTo(ast);
      break;
    case 'reference':
      content = renderReference(ast);
      break;
    case 'explanation':
      content = renderExplanation(ast);
      break;
    default:
      throw new Error(`Unknown view type: ${view}`);
  }

  // Append proof to all views
  content += `\n---\n\n`;
  content += generateProofAppendix(ast);

  return content;
}

/**
 * Extract main heading from blocks
 * @param {Array} blocks - AST blocks
 * @returns {string} Main heading text
 */
function getMainHeading(blocks) {
  const heading = blocks.find((b) => b.type === 'heading' && b.level === 1);
  return heading ? heading.content : 'Documentation';
}
