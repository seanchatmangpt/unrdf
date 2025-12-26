/**
 * @file Scaffold generator - converts DiataxisEntry to markdown files
 * @module scaffold
 */

import { mkdir, writeFile, readFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { stableStringify } from './stable-json.mjs';
import { hashString, hashObject } from './hash.mjs';

/**
 * @typedef {Object} ScaffoldOutput
 * @property {string} packageName - Package name
 * @property {string[]} filesGenerated - Absolute paths to generated files
 * @property {string} indexPath - Absolute path to index.md
 * @property {string} filesHash - SHA256 of concatenated file hashes
 */

/**
 * Generate scaffold markdown files from a DiataxisEntry
 * @param {import('./diataxis-schema.mjs').DiataxisEntry} diataxisEntry - Entry to scaffold
 * @param {string} outputDir - Output directory path
 * @returns {Promise<ScaffoldOutput>} Scaffold output metadata
 */
export async function generateScaffold(diataxisEntry, outputDir) {
  const absOutputDir = resolve(outputDir);

  // Create output directory structure
  await mkdir(absOutputDir, { recursive: true });
  await mkdir(join(absOutputDir, 'tutorials'), { recursive: true });
  await mkdir(join(absOutputDir, 'how-to'), { recursive: true });
  await mkdir(join(absOutputDir, 'reference'), { recursive: true });
  await mkdir(join(absOutputDir, 'explanation'), { recursive: true });

  /** @type {string[]} */
  const filesGenerated = [];

  // Generate tutorial files
  for (const tutorial of diataxisEntry.tutorials) {
    const filePath = join(absOutputDir, 'tutorials', `tutorial-${tutorial.id}.md`);
    const content = generateTutorialMarkdown(tutorial, diataxisEntry);
    await writeFile(filePath, content, 'utf8');
    filesGenerated.push(filePath);
  }

  // Generate how-to files
  for (const howto of diataxisEntry.howtos) {
    const filePath = join(absOutputDir, 'how-to', `howto-${howto.id}.md`);
    const content = generateHowToMarkdown(howto, diataxisEntry);
    await writeFile(filePath, content, 'utf8');
    filesGenerated.push(filePath);
  }

  // Generate reference file
  const referencePath = join(absOutputDir, 'reference', 'reference.md');
  const referenceContent = generateReferenceMarkdown(diataxisEntry.reference, diataxisEntry);
  await writeFile(referencePath, referenceContent, 'utf8');
  filesGenerated.push(referencePath);

  // Generate explanation file
  const explanationPath = join(absOutputDir, 'explanation', 'explanation.md');
  const explanationContent = generateExplanationMarkdown(diataxisEntry.explanation, diataxisEntry);
  await writeFile(explanationPath, explanationContent, 'utf8');
  filesGenerated.push(explanationPath);

  // Generate index.md
  const indexPath = join(absOutputDir, 'index.md');
  const indexContent = generateIndexMarkdown(diataxisEntry, filesGenerated);
  await writeFile(indexPath, indexContent, 'utf8');

  // Calculate filesHash (SHA256 of concatenated file hashes)
  const fileHashes = [];
  const allFiles = [...filesGenerated, indexPath].sort();
  for (const filePath of allFiles) {
    const fileContent = await readFile(filePath, 'utf8');
    const hash = hashString(fileContent);
    fileHashes.push(hash);
  }
  const filesHash = hashString(fileHashes.join(''));

  return {
    packageName: diataxisEntry.packageName,
    filesGenerated: filesGenerated.sort(),
    indexPath,
    filesHash
  };
}

/**
 * Generate markdown content for a tutorial
 * @param {import('./diataxis-schema.mjs').Tutorial} tutorial - Tutorial object
 * @param {import('./diataxis-schema.mjs').DiataxisEntry} entry - Parent entry
 * @returns {string} Markdown content
 */
function generateTutorialMarkdown(tutorial, entry) {
  const proof = calculateProof({
    source: tutorial.source,
    title: tutorial.title,
    confidenceScore: tutorial.confidenceScore
  });

  const frontmatter = generateFrontmatter({
    title: tutorial.title,
    type: 'tutorial',
    packageName: entry.packageName,
    version: entry.version,
    generatedAt: entry.generatedAt,
    confidenceScore: tutorial.confidenceScore,
    proof
  });

  const prerequisites = tutorial.prerequisites?.length > 0
    ? tutorial.prerequisites.map(p => `- ${p}`).join('\n')
    : '- None';

  const steps = tutorial.stepsOutline?.length > 0
    ? tutorial.stepsOutline.map((step, i) => `${i + 1}. ${step}`).join('\n')
    : '1. (Steps to be defined)';

  const proofBlock = generateProofBlock({
    sources: tutorial.source,
    fingerprintInput: `${tutorial.source.sort().join('|')}|${tutorial.title}|${tutorial.confidenceScore}`,
    hash: proof
  });

  return `${frontmatter}

## Overview

${tutorial.goal || 'Learn how to use this feature.'}

## Prerequisites

${prerequisites}

## Steps

${steps}

${proofBlock}
`;
}

/**
 * Generate markdown content for a how-to
 * @param {import('./diataxis-schema.mjs').HowTo} howto - How-to object
 * @param {import('./diataxis-schema.mjs').DiataxisEntry} entry - Parent entry
 * @returns {string} Markdown content
 */
function generateHowToMarkdown(howto, entry) {
  const proof = calculateProof({
    source: howto.source,
    title: howto.title,
    confidenceScore: howto.confidenceScore
  });

  const frontmatter = generateFrontmatter({
    title: howto.title,
    type: 'how-to',
    packageName: entry.packageName,
    version: entry.version,
    generatedAt: entry.generatedAt,
    confidenceScore: howto.confidenceScore,
    proof
  });

  const steps = howto.steps?.length > 0
    ? howto.steps.map((step, i) => `${i + 1}. ${step}`).join('\n')
    : '1. (Steps to be defined)';

  const proofBlock = generateProofBlock({
    sources: howto.source,
    fingerprintInput: `${howto.source.sort().join('|')}|${howto.title}|${howto.confidenceScore}`,
    hash: proof
  });

  return `${frontmatter}

## Task

${howto.task || 'Perform a specific task.'}

## Context

${howto.context || 'Use this guide when you need to accomplish a specific goal.'}

## Steps

${steps}

${proofBlock}
`;
}

/**
 * Generate markdown content for reference
 * @param {import('./diataxis-schema.mjs').Reference} reference - Reference object
 * @param {import('./diataxis-schema.mjs').DiataxisEntry} entry - Parent entry
 * @returns {string} Markdown content
 */
function generateReferenceMarkdown(reference, entry) {
  const proof = calculateProof({
    source: reference.source,
    title: reference.title,
    confidenceScore: reference.confidenceScore
  });

  const frontmatter = generateFrontmatter({
    title: reference.title,
    type: 'reference',
    packageName: entry.packageName,
    version: entry.version,
    generatedAt: entry.generatedAt,
    confidenceScore: reference.confidenceScore,
    proof
  });

  let tableContent = '| Name | Type | Description |\n|------|------|-------------|\n';

  if (reference.items?.length > 0) {
    for (const item of reference.items) {
      const name = escapeMarkdown(item.name || '');
      const type = escapeMarkdown(item.type || 'unknown');
      const description = escapeMarkdown(item.description || '');
      tableContent += `| ${name} | ${type} | ${description} |\n`;
    }
  } else {
    tableContent += '| - | - | (No items documented) |\n';
  }

  const proofBlock = generateProofBlock({
    sources: reference.source,
    fingerprintInput: `${reference.source.sort().join('|')}|${reference.title}|${reference.confidenceScore}`,
    hash: proof
  });

  return `${frontmatter}

## Overview

This reference documentation provides technical details for ${entry.packageName}.

## API Reference

${tableContent}

${proofBlock}
`;
}

/**
 * Generate markdown content for explanation
 * @param {import('./diataxis-schema.mjs').Explanation} explanation - Explanation object
 * @param {import('./diataxis-schema.mjs').DiataxisEntry} entry - Parent entry
 * @returns {string} Markdown content
 */
function generateExplanationMarkdown(explanation, entry) {
  const proof = calculateProof({
    source: explanation.source,
    title: explanation.title,
    confidenceScore: explanation.confidenceScore
  });

  const frontmatter = generateFrontmatter({
    title: explanation.title,
    type: 'explanation',
    packageName: entry.packageName,
    version: entry.version,
    generatedAt: entry.generatedAt,
    confidenceScore: explanation.confidenceScore,
    proof
  });

  const concepts = explanation.concepts?.length > 0
    ? explanation.concepts.map(c => `- ${c}`).join('\n')
    : '- (No concepts documented)';

  const architecture = explanation.architecture || '(No architecture documented)';

  const tradeoffs = explanation.tradeoffs?.length > 0
    ? explanation.tradeoffs.map(t => `- ${t}`).join('\n')
    : '- (No tradeoffs documented)';

  const proofBlock = generateProofBlock({
    sources: explanation.source,
    fingerprintInput: `${explanation.source.sort().join('|')}|${explanation.title}|${explanation.confidenceScore}`,
    hash: proof
  });

  return `${frontmatter}

## Concepts

${concepts}

## Architecture

${architecture}

## Tradeoffs

${tradeoffs}

${proofBlock}
`;
}

/**
 * Generate index.md content
 * @param {import('./diataxis-schema.mjs').DiataxisEntry} entry - Diataxis entry
 * @param {string[]} filesGenerated - List of generated files
 * @returns {string} Markdown content
 */
function generateIndexMarkdown(entry, filesGenerated) {
  const tutorialCount = entry.tutorials?.length || 0;
  const howtoCount = entry.howtos?.length || 0;
  const hasReference = entry.reference?.items?.length > 0;
  const hasExplanation = entry.explanation?.concepts?.length > 0 ||
                         entry.explanation?.architecture?.length > 0 ||
                         entry.explanation?.tradeoffs?.length > 0;

  let tutorialLinks = '';
  if (tutorialCount > 0) {
    tutorialLinks = '### Tutorials\n\n';
    for (const tutorial of entry.tutorials) {
      tutorialLinks += `- [${tutorial.title}](tutorials/tutorial-${tutorial.id}.md)\n`;
    }
    tutorialLinks += '\n';
  }

  let howtoLinks = '';
  if (howtoCount > 0) {
    howtoLinks = '### How-To Guides\n\n';
    for (const howto of entry.howtos) {
      howtoLinks += `- [${howto.title}](how-to/howto-${howto.id}.md)\n`;
    }
    howtoLinks += '\n';
  }

  let referenceLink = '';
  if (hasReference) {
    referenceLink = '### Reference\n\n';
    referenceLink += `- [${entry.reference.title}](reference/reference.md)\n\n`;
  }

  let explanationLink = '';
  if (hasExplanation) {
    explanationLink = '### Explanation\n\n';
    explanationLink += `- [${entry.explanation.title}](explanation/explanation.md)\n\n`;
  }

  return `---
title: "${entry.packageName} Documentation"
packageName: "${entry.packageName}"
version: "${entry.version}"
generatedAt: "${entry.generatedAt}"
---

# ${entry.packageName} Documentation

Generated documentation using the Diátaxis framework.

## Quick Stats

- **Tutorials**: ${tutorialCount}
- **How-To Guides**: ${howtoCount}
- **Reference**: ${hasReference ? 'Available' : 'Not available'}
- **Explanation**: ${hasExplanation ? 'Available' : 'Not available'}

## Documentation Sections

${tutorialLinks}${howtoLinks}${referenceLink}${explanationLink}

---

*Generated at ${entry.generatedAt}*
`;
}

/**
 * Generate YAML frontmatter
 * @param {Object} metadata - Frontmatter metadata
 * @param {string} metadata.title - Document title
 * @param {string} metadata.type - Document type
 * @param {string} metadata.packageName - Package name
 * @param {string} metadata.version - Package version
 * @param {string} metadata.generatedAt - Generation timestamp
 * @param {number} metadata.confidenceScore - Confidence score
 * @param {string} metadata.proof - Proof hash
 * @returns {string} YAML frontmatter block
 */
function generateFrontmatter(metadata) {
  // Fixed order for determinism
  return `---
title: "${metadata.title}"
type: "${metadata.type}"
packageName: "${metadata.packageName}"
version: "${metadata.version}"
generatedAt: "${metadata.generatedAt}"
confidenceScore: ${metadata.confidenceScore}
proof: "${metadata.proof}"
---`;
}

/**
 * Generate proof block
 * @param {Object} proofData - Proof data
 * @param {string[]} proofData.sources - Source list
 * @param {string} proofData.fingerprintInput - Fingerprint input
 * @param {string} proofData.hash - Computed hash
 * @returns {string} Proof block markdown
 */
function generateProofBlock(proofData) {
  const sourcesList = proofData.sources?.length > 0
    ? proofData.sources.map(s => `- ${s}`).join('\n')
    : '- (No sources recorded)';

  const proofJson = stableStringify({
    sources: proofData.sources || [],
    fingerprintInput: proofData.fingerprintInput,
    hash: proofData.hash
  }, { indent: 2 });

  return `## Proof

This file was generated from the following evidence sources:

${sourcesList}

\`\`\`json
${proofJson}
\`\`\``;
}

/**
 * Calculate proof hash for a content object
 * @param {Object} data - Data to hash
 * @param {string[]} data.source - Source array
 * @param {string} data.title - Content title
 * @param {number} data.confidenceScore - Confidence score
 * @returns {string} SHA256 hash (hex)
 */
function calculateProof(data) {
  const fingerprintInput = `${data.source.sort().join('|')}|${data.title}|${data.confidenceScore}`;
  return hashString(fingerprintInput);
}

/**
 * Escape markdown special characters in table cells
 * @param {string} text - Text to escape
 * @returns {string} Escaped text
 */
function escapeMarkdown(text) {
  return text
    .replace(/\|/g, '\\|')
    .replace(/\n/g, ' ')
    .replace(/\r/g, '');
}
