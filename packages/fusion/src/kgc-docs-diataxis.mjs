/**
 * KGC Diataxis Documentation Projection System
 *
 * Projects one canonical source document into 4 Diataxis views:
 * - Tutorial (learning-oriented)
 * - How-To (problem-oriented)
 * - Reference (information-oriented)
 * - Explanation (understanding-oriented)
 *
 * Each projection maintains provenance chain (same o_hash, policy_id, receipts)
 * while transforming content structure, tone, and sections according to Diataxis rules.
 *
 * @module @unrdf/fusion/kgc-docs-diataxis
 */

import { z } from 'zod';
import { createHash } from 'node:crypto';

// =============================================================================
// Schemas
// =============================================================================

/**
 * Frontmatter schema for KGC documentation
 */
const FrontmatterSchema = z.object({
  /** Document title */
  title: z.string(),
  /** Document description */
  description: z.string().optional(),
  /** Ontological hash (immutable) */
  o_hash: z.string().optional(),
  /** Policy ID (immutable) */
  policy_id: z.string().optional(),
  /** Receipt chain (immutable) */
  receipts: z.array(z.string()).optional(),
  /** Diataxis views this projection belongs to (canonical source can have any view type) */
  views: z.array(z.string()),
  /** Creation timestamp (immutable) */
  createdAt: z.string().optional(),
  /** Last proof timestamp (immutable) */
  lastProved: z.string().optional(),
  /** Tags for categorization */
  tags: z.array(z.string()).optional(),
  /** Difficulty level (for tutorials) */
  difficulty: z.enum(['beginner', 'intermediate', 'advanced']).optional(),
  /** Prerequisites (for tutorials/howtos) */
  prerequisites: z.array(z.string()).optional(),
  /** Estimated time (for tutorials) */
  estimatedTime: z.string().optional(),
});

/**
 * API manifest schema - describes API surface for reference extraction
 */
const ApiManifestSchema = z.object({
  /** Package or module name */
  name: z.string(),
  /** Version */
  version: z.string().optional(),
  /** Exported functions */
  functions: z.array(z.object({
    name: z.string(),
    signature: z.string(),
    description: z.string(),
    parameters: z.array(z.object({
      name: z.string(),
      type: z.string(),
      required: z.boolean().default(true),
      defaultValue: z.string().optional(),
      description: z.string().optional(),
    })).optional(),
    returns: z.object({
      type: z.string(),
      description: z.string().optional(),
    }).optional(),
    throws: z.array(z.string()).optional(),
    examples: z.array(z.string()).optional(),
  })).optional(),
  /** Exported classes */
  classes: z.array(z.object({
    name: z.string(),
    description: z.string(),
    methods: z.array(z.any()).optional(),
    properties: z.array(z.any()).optional(),
  })).optional(),
  /** Constants/types */
  constants: z.array(z.object({
    name: z.string(),
    type: z.string(),
    value: z.string().optional(),
    description: z.string().optional(),
  })).optional(),
});

/**
 * Canonical source document schema
 */
const SourceDocSchema = z.object({
  /** Frontmatter */
  frontmatter: FrontmatterSchema,
  /** Markdown content (without frontmatter) */
  content: z.string(),
  /** Optional API manifest */
  apiManifest: ApiManifestSchema.optional(),
});

/**
 * Projection result schema
 */
const ProjectionResultSchema = z.object({
  /** Projected frontmatter */
  frontmatter: FrontmatterSchema,
  /** Projected markdown content */
  content: z.string(),
  /** View type */
  view: z.enum(['tutorial', 'howto', 'reference', 'explanation']),
  /** Content hash */
  hash: z.string(),
});

/**
 * Validation result schema
 */
const ValidationResultSchema = z.object({
  /** Whether projection is valid */
  valid: z.boolean(),
  /** Validation warnings */
  warnings: z.array(z.string()),
  /** Validation errors */
  errors: z.array(z.string()).optional(),
});

// =============================================================================
// Type Definitions (JSDoc)
// =============================================================================

/**
 * @typedef {Object} Frontmatter
 * @property {string} title
 * @property {string} [description]
 * @property {string} [o_hash]
 * @property {string} [policy_id]
 * @property {string[]} [receipts]
 * @property {Array<'tutorial'|'howto'|'reference'|'explanation'>} views
 * @property {string} [createdAt]
 * @property {string} [lastProved]
 * @property {string[]} [tags]
 * @property {'beginner'|'intermediate'|'advanced'} [difficulty]
 * @property {string[]} [prerequisites]
 * @property {string} [estimatedTime]
 */

/**
 * @typedef {Object} ApiManifest
 * @property {string} name
 * @property {string} [version]
 * @property {Array<Object>} [functions]
 * @property {Array<Object>} [classes]
 * @property {Array<Object>} [constants]
 */

/**
 * @typedef {Object} SourceDoc
 * @property {Frontmatter} frontmatter
 * @property {string} content
 * @property {ApiManifest} [apiManifest]
 */

/**
 * @typedef {Object} ProjectionResult
 * @property {Frontmatter} frontmatter
 * @property {string} content
 * @property {'tutorial'|'howto'|'reference'|'explanation'} view
 * @property {string} hash
 */

/**
 * @typedef {Object} ValidationResult
 * @property {boolean} valid
 * @property {string[]} warnings
 * @property {string[]} [errors]
 */

// =============================================================================
// Markdown Parsing Helpers
// =============================================================================

/**
 * Parse frontmatter from markdown string
 * @param {string} markdown - Full markdown document
 * @returns {{frontmatter: Object, content: string}}
 */
function parseFrontmatter(markdown) {
  const match = markdown.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
  if (!match) {
    return { frontmatter: {}, content: markdown };
  }

  const [, frontmatterStr, content] = match;
  const frontmatter = {};

  // Simple YAML parser (sufficient for our needs)
  const lines = frontmatterStr.split('\n');
  let currentKey = null;
  let arrayMode = false;
  let indentLevel = 0;

  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed) continue;

    // Calculate indent
    const currentIndent = line.search(/\S/);

    // Array item at any indent level
    if (trimmed.startsWith('- ')) {
      const value = trimmed.slice(2).trim();

      if (arrayMode && currentKey) {
        frontmatter[currentKey].push(value);
      } else {
        // Start new array for this key
        if (currentKey) {
          if (!frontmatter[currentKey]) {
            frontmatter[currentKey] = [];
          }
          if (!Array.isArray(frontmatter[currentKey])) {
            frontmatter[currentKey] = [frontmatter[currentKey]];
          }
          frontmatter[currentKey].push(value);
          arrayMode = true;
        }
      }
      continue;
    }

    // Key-value pair
    const colonIndex = trimmed.indexOf(':');
    if (colonIndex > 0) {
      const key = trimmed.slice(0, colonIndex).trim();
      const value = trimmed.slice(colonIndex + 1).trim();

      currentKey = key;
      indentLevel = currentIndent;
      arrayMode = false;

      if (value === '') {
        // Array or object (we only handle arrays)
        frontmatter[key] = [];
        arrayMode = true;
      } else {
        frontmatter[key] = value;
      }
    }
  }

  return { frontmatter, content };
}

/**
 * Serialize frontmatter to YAML
 * @param {Frontmatter} frontmatter - Frontmatter object
 * @returns {string} YAML string
 */
function serializeFrontmatter(frontmatter) {
  const lines = [];

  for (const [key, value] of Object.entries(frontmatter)) {
    if (Array.isArray(value)) {
      if (value.length === 0) {
        lines.push(`${key}: []`);
      } else {
        lines.push(`${key}:`);
        for (const item of value) {
          lines.push(`  - ${item}`);
        }
      }
    } else {
      lines.push(`${key}: ${value}`);
    }
  }

  return lines.join('\n');
}

/**
 * Extract sections from markdown content
 * @param {string} content - Markdown content
 * @returns {Array<{level: number, title: string, content: string}>}
 */
function extractSections(content) {
  const sections = [];
  const lines = content.split('\n');
  let currentSection = null;

  for (const line of lines) {
    const match = line.match(/^(#{1,6})\s+(.+)$/);
    if (match) {
      // Save previous section
      if (currentSection) {
        sections.push(currentSection);
      }

      // Start new section
      const [, hashes, title] = match;
      currentSection = {
        level: hashes.length,
        title: title.trim(),
        content: '',
      };
    } else if (currentSection) {
      currentSection.content += line + '\n';
    }
  }

  // Save last section
  if (currentSection) {
    sections.push(currentSection);
  }

  return sections;
}

/**
 * Extract code blocks from content
 * @param {string} content - Markdown content
 * @returns {Array<{language: string, code: string}>}
 */
function extractCodeBlocks(content) {
  const blocks = [];
  const regex = /```(\w+)?\n([\s\S]*?)```/g;
  let match;

  while ((match = regex.exec(content)) !== null) {
    blocks.push({
      language: match[1] || 'text',
      code: match[2].trim(),
    });
  }

  return blocks;
}

/**
 * Extract bullet lists from content
 * @param {string} content - Markdown content
 * @returns {string[]}
 */
function extractBulletLists(content) {
  const items = [];
  const lines = content.split('\n');

  for (const line of lines) {
    const match = line.match(/^[\s]*[-*]\s+(.+)$/);
    if (match) {
      items.push(match[1].trim());
    }
  }

  return items;
}

/**
 * Find section by title (case-insensitive, partial match)
 * @param {Array<{level: number, title: string, content: string}>} sections - Sections array
 * @param {string} titlePattern - Title to match (case-insensitive)
 * @returns {{level: number, title: string, content: string}|null}
 */
function findSection(sections, titlePattern) {
  const pattern = titlePattern.toLowerCase();
  return sections.find(s => s.title.toLowerCase().includes(pattern)) || null;
}

// =============================================================================
// Content Transformation Helpers
// =============================================================================

/**
 * Simplify content by removing advanced sections
 * @param {string} content - Markdown content
 * @param {string[]} excludePatterns - Section titles to exclude
 * @returns {string}
 */
function simplifyContent(content, excludePatterns = ['advanced', 'edge case', 'internals', 'troubleshooting']) {
  const sections = extractSections(content);
  const filtered = sections.filter(section => {
    const title = section.title.toLowerCase();
    return !excludePatterns.some(pattern => title.includes(pattern.toLowerCase()));
  });

  return filtered.map(s => `${'#'.repeat(s.level)} ${s.title}\n${s.content}`).join('\n');
}

/**
 * Add encouraging tone markers to content
 * @param {string} content - Markdown content
 * @returns {string}
 */
function addEncouragingTone(content) {
  // Add gentle introductory phrases
  let result = content;

  // Replace imperative with friendlier versions
  result = result.replace(/^(#+\s+)Step (\d+):/gm, '$1Step $2: Let\'s');
  result = result.replace(/\bYou must\b/gi, 'You\'ll want to');
  result = result.replace(/\bNote:/gi, '💡 Tip:');

  return result;
}

/**
 * Extract step-by-step instructions from content
 * @param {string} content - Markdown content
 * @returns {string[]}
 */
function extractSteps(content) {
  const steps = [];
  const sections = extractSections(content);

  for (const section of sections) {
    if (/step|getting started|installation|setup/i.test(section.title)) {
      const bullets = extractBulletLists(section.content);
      steps.push(...bullets);

      // Also check for numbered lists
      const lines = section.content.split('\n');
      for (const line of lines) {
        const match = line.match(/^\d+\.\s+(.+)$/);
        if (match) {
          steps.push(match[1].trim());
        }
      }
    }
  }

  return steps;
}

/**
 * Generate API table from manifest
 * @param {ApiManifest} apiManifest - API manifest
 * @param {'functions'|'classes'|'constants'} type - Type of API items
 * @returns {string} Markdown table
 */
function generateApiTable(apiManifest, type) {
  const items = apiManifest[type] || [];
  if (items.length === 0) return '';

  const lines = [];

  if (type === 'functions') {
    lines.push('| Function | Description | Returns |');
    lines.push('|----------|-------------|---------|');

    for (const fn of items) {
      const name = fn.signature || `${fn.name}()`;
      const desc = fn.description || '';
      const ret = fn.returns?.type || 'void';
      lines.push(`| \`${name}\` | ${desc} | \`${ret}\` |`);
    }
  } else if (type === 'classes') {
    lines.push('| Class | Description |');
    lines.push('|-------|-------------|');

    for (const cls of items) {
      lines.push(`| \`${cls.name}\` | ${cls.description || ''} |`);
    }
  } else if (type === 'constants') {
    lines.push('| Constant | Type | Description |');
    lines.push('|----------|------|-------------|');

    for (const constant of items) {
      const type = constant.type || 'unknown';
      const desc = constant.description || '';
      lines.push(`| \`${constant.name}\` | \`${type}\` | ${desc} |`);
    }
  }

  return lines.join('\n');
}

/**
 * Generate function reference documentation
 * @param {Object} fn - Function definition from API manifest
 * @returns {string} Markdown documentation
 */
function generateFunctionReference(fn) {
  const lines = [];

  lines.push(`### ${fn.name}`);
  lines.push('');
  lines.push(fn.description || '');
  lines.push('');
  lines.push('**Signature:**');
  lines.push('');
  lines.push('```javascript');
  lines.push(fn.signature || `${fn.name}()`);
  lines.push('```');
  lines.push('');

  if (fn.parameters && fn.parameters.length > 0) {
    lines.push('**Parameters:**');
    lines.push('');
    lines.push('| Name | Type | Required | Default | Description |');
    lines.push('|------|------|----------|---------|-------------|');

    for (const param of fn.parameters) {
      const name = param.name;
      const type = param.type || 'any';
      const required = param.required ? 'Yes' : 'No';
      const defaultValue = param.defaultValue || '-';
      const desc = param.description || '';
      lines.push(`| \`${name}\` | \`${type}\` | ${required} | \`${defaultValue}\` | ${desc} |`);
    }
    lines.push('');
  }

  if (fn.returns) {
    lines.push('**Returns:**');
    lines.push('');
    lines.push(`\`${fn.returns.type}\` - ${fn.returns.description || ''}`);
    lines.push('');
  }

  if (fn.throws && fn.throws.length > 0) {
    lines.push('**Throws:**');
    lines.push('');
    for (const error of fn.throws) {
      lines.push(`- ${error}`);
    }
    lines.push('');
  }

  if (fn.examples && fn.examples.length > 0) {
    lines.push('**Examples:**');
    lines.push('');
    for (const example of fn.examples) {
      lines.push('```javascript');
      lines.push(example);
      lines.push('```');
      lines.push('');
    }
  }

  return lines.join('\n');
}

// =============================================================================
// Projection Functions
// =============================================================================

/**
 * Project canonical source to Tutorial view
 *
 * Tutorial characteristics:
 * - Learning-oriented: helps beginners understand
 * - Narrative flow with hands-on examples
 * - No edge cases or advanced topics
 * - Encouraging, forgiving tone
 * - Step-by-step guide with working example
 *
 * @param {SourceDoc} source - Canonical source document
 * @param {ApiManifest} [apiManifest] - API surface (optional)
 * @returns {ProjectionResult}
 */
export function projectToTutorial(source, apiManifest) {
  const { frontmatter, content } = source;
  const sections = extractSections(content);

  const tutorialSections = [];

  // 1. Motivation (why does this exist?)
  const introSection = findSection(sections, 'introduction') ||
                       findSection(sections, 'overview') ||
                       findSection(sections, 'getting started');

  if (introSection) {
    tutorialSections.push('## Why This Matters');
    tutorialSections.push('');
    tutorialSections.push(introSection.content.trim());
    tutorialSections.push('');
  }

  // 2. Prerequisites (what you'll need)
  if (frontmatter.prerequisites && frontmatter.prerequisites.length > 0) {
    tutorialSections.push('## Before You Begin');
    tutorialSections.push('');
    tutorialSections.push('Make sure you have:');
    tutorialSections.push('');
    for (const prereq of frontmatter.prerequisites) {
      tutorialSections.push(`- ${prereq}`);
    }
    tutorialSections.push('');
  }

  // 3. Step-by-step guide
  const steps = extractSteps(content);
  if (steps.length > 0) {
    tutorialSections.push('## Step-by-Step Guide');
    tutorialSections.push('');
    steps.forEach((step, i) => {
      tutorialSections.push(`### Step ${i + 1}: ${step}`);
      tutorialSections.push('');
    });
  }

  // 4. Working example (complete, runnable)
  const codeBlocks = extractCodeBlocks(content);
  if (codeBlocks.length > 0) {
    tutorialSections.push('## Complete Working Example');
    tutorialSections.push('');
    tutorialSections.push('Here\'s a complete example you can run right now:');
    tutorialSections.push('');

    // Prefer javascript/js code blocks for tutorials
    const jsBlock = codeBlocks.find(b => b.language === 'javascript' || b.language === 'js') || codeBlocks[0];

    tutorialSections.push('```' + jsBlock.language);
    tutorialSections.push(jsBlock.code);
    tutorialSections.push('```');
    tutorialSections.push('');
  }

  // 5. Common pitfalls (1-2 only)
  const troubleshootingSection = findSection(sections, 'troubleshooting') ||
                                 findSection(sections, 'common issues');

  if (troubleshootingSection) {
    const pitfalls = extractBulletLists(troubleshootingSection.content).slice(0, 2);
    if (pitfalls.length > 0) {
      tutorialSections.push('## Common Pitfalls');
      tutorialSections.push('');
      for (const pitfall of pitfalls) {
        tutorialSections.push(`- ${pitfall}`);
      }
      tutorialSections.push('');
    }
  }

  // Apply encouraging tone
  let tutorialContent = tutorialSections.join('\n');
  tutorialContent = addEncouragingTone(tutorialContent);

  // Create tutorial frontmatter
  const tutorialFrontmatter = {
    ...frontmatter,
    views: ['tutorial'],
    difficulty: 'beginner',
    title: frontmatter.title.includes('Tutorial') ? frontmatter.title : `Tutorial: ${frontmatter.title}`,
  };

  const hash = createHash('sha256').update(tutorialContent).digest('hex');

  return {
    frontmatter: tutorialFrontmatter,
    content: tutorialContent,
    view: 'tutorial',
    hash,
  };
}

/**
 * Project canonical source to How-To view
 *
 * How-To characteristics:
 * - Problem-oriented: solve specific problems
 * - Assumes user knows basics
 * - Direct solution with variations
 * - Practical, actionable tone
 * - Includes troubleshooting
 *
 * @param {SourceDoc} source - Canonical source document
 * @param {ApiManifest} [apiManifest] - API surface (optional)
 * @returns {ProjectionResult}
 */
export function projectToHowTo(source, apiManifest) {
  const { frontmatter, content } = source;
  const sections = extractSections(content);

  const howtoSections = [];

  // 1. Problem statement
  howtoSections.push('## Problem');
  howtoSections.push('');

  const problemSection = findSection(sections, 'problem') ||
                        findSection(sections, 'use case') ||
                        findSection(sections, 'overview');

  if (problemSection) {
    howtoSections.push(problemSection.content.trim());
  } else {
    howtoSections.push(`How to ${frontmatter.title.toLowerCase()}.`);
  }
  howtoSections.push('');

  // 2. Prerequisites
  if (frontmatter.prerequisites && frontmatter.prerequisites.length > 0) {
    howtoSections.push('## Prerequisites');
    howtoSections.push('');
    for (const prereq of frontmatter.prerequisites) {
      howtoSections.push(`- ${prereq}`);
    }
    howtoSections.push('');
  }

  // 3. Solution (code + explanation)
  const solutionSection = findSection(sections, 'solution') ||
                         findSection(sections, 'implementation') ||
                         findSection(sections, 'usage');

  howtoSections.push('## Solution');
  howtoSections.push('');

  if (solutionSection) {
    howtoSections.push(solutionSection.content.trim());
  } else {
    // Extract first code block as solution
    const codeBlocks = extractCodeBlocks(content);
    if (codeBlocks.length > 0) {
      howtoSections.push('```' + codeBlocks[0].language);
      howtoSections.push(codeBlocks[0].code);
      howtoSections.push('```');
    }
  }
  howtoSections.push('');

  // 4. Variations (alternative approaches)
  const variationsSection = findSection(sections, 'variation') ||
                           findSection(sections, 'alternative') ||
                           findSection(sections, 'advanced');

  if (variationsSection) {
    howtoSections.push('## Variations');
    howtoSections.push('');
    howtoSections.push(variationsSection.content.trim());
    howtoSections.push('');
  }

  // 5. Troubleshooting
  const troubleshootingSection = findSection(sections, 'troubleshooting') ||
                                 findSection(sections, 'common issues') ||
                                 findSection(sections, 'debugging');

  if (troubleshootingSection) {
    howtoSections.push('## Troubleshooting');
    howtoSections.push('');
    howtoSections.push(troubleshootingSection.content.trim());
    howtoSections.push('');
  }

  const howtoContent = howtoSections.join('\n');

  // Create how-to frontmatter
  const howtoFrontmatter = {
    ...frontmatter,
    views: ['howto'],
    title: frontmatter.title.includes('How to') ? frontmatter.title : `How to ${frontmatter.title}`,
  };

  const hash = createHash('sha256').update(howtoContent).digest('hex');

  return {
    frontmatter: howtoFrontmatter,
    content: howtoContent,
    view: 'howto',
    hash,
  };
}

/**
 * Project canonical source to Reference view
 *
 * Reference characteristics:
 * - Information-oriented: complete API documentation
 * - API tables, signatures, parameters, returns
 * - Formal, precise tone
 * - Alphabetically sorted
 * - Usage examples (1-2 per item)
 *
 * @param {SourceDoc} source - Canonical source document
 * @param {ApiManifest} [apiManifest] - API surface (required for full reference)
 * @returns {ProjectionResult}
 */
export function projectToReference(source, apiManifest) {
  const { frontmatter, content } = source;
  const referenceSections = [];

  // 1. Overview
  referenceSections.push('## API Reference');
  referenceSections.push('');

  if (frontmatter.description) {
    referenceSections.push(frontmatter.description);
    referenceSections.push('');
  }

  // 2. API Tables and Details
  if (apiManifest) {
    // Functions
    if (apiManifest.functions && apiManifest.functions.length > 0) {
      referenceSections.push('## Functions');
      referenceSections.push('');

      // Sort alphabetically
      const sortedFunctions = [...apiManifest.functions].sort((a, b) =>
        a.name.localeCompare(b.name)
      );

      // Summary table
      referenceSections.push(generateApiTable(apiManifest, 'functions'));
      referenceSections.push('');

      // Detailed documentation for each function
      for (const fn of sortedFunctions) {
        referenceSections.push(generateFunctionReference(fn));
        referenceSections.push('');
      }
    }

    // Classes
    if (apiManifest.classes && apiManifest.classes.length > 0) {
      referenceSections.push('## Classes');
      referenceSections.push('');

      const sortedClasses = [...apiManifest.classes].sort((a, b) =>
        a.name.localeCompare(b.name)
      );

      referenceSections.push(generateApiTable(apiManifest, 'classes'));
      referenceSections.push('');

      for (const cls of sortedClasses) {
        referenceSections.push(`### ${cls.name}`);
        referenceSections.push('');
        referenceSections.push(cls.description || '');
        referenceSections.push('');

        // Methods
        if (cls.methods && cls.methods.length > 0) {
          referenceSections.push('**Methods:**');
          referenceSections.push('');
          for (const method of cls.methods) {
            referenceSections.push(`- \`${method.name}\`: ${method.description || ''}`);
          }
          referenceSections.push('');
        }

        // Properties
        if (cls.properties && cls.properties.length > 0) {
          referenceSections.push('**Properties:**');
          referenceSections.push('');
          for (const prop of cls.properties) {
            referenceSections.push(`- \`${prop.name}\` (\`${prop.type}\`): ${prop.description || ''}`);
          }
          referenceSections.push('');
        }
      }
    }

    // Constants
    if (apiManifest.constants && apiManifest.constants.length > 0) {
      referenceSections.push('## Constants');
      referenceSections.push('');

      const sortedConstants = [...apiManifest.constants].sort((a, b) =>
        a.name.localeCompare(b.name)
      );

      referenceSections.push(generateApiTable(apiManifest, 'constants'));
      referenceSections.push('');
    }
  } else {
    // Fallback: extract from content
    const sections = extractSections(content);
    const apiSection = findSection(sections, 'api') ||
                       findSection(sections, 'reference') ||
                       findSection(sections, 'methods');

    if (apiSection) {
      referenceSections.push(apiSection.content.trim());
      referenceSections.push('');
    }
  }

  const referenceContent = referenceSections.join('\n');

  // Create reference frontmatter
  const referenceFrontmatter = {
    ...frontmatter,
    views: ['reference'],
    title: frontmatter.title.includes('Reference') ? frontmatter.title : `${frontmatter.title} Reference`,
  };

  const hash = createHash('sha256').update(referenceContent).digest('hex');

  return {
    frontmatter: referenceFrontmatter,
    content: referenceContent,
    view: 'reference',
    hash,
  };
}

/**
 * Project canonical source to Explanation view
 *
 * Explanation characteristics:
 * - Understanding-oriented: explain concepts and tradeoffs
 * - Conceptual overview, design decisions
 * - Historical context, alternatives
 * - Reflective, contextual tone
 * - Common misconceptions
 *
 * @param {SourceDoc} source - Canonical source document
 * @param {ApiManifest} [apiManifest] - API surface (optional)
 * @returns {ProjectionResult}
 */
export function projectToExplanation(source, apiManifest) {
  const { frontmatter, content } = source;
  const sections = extractSections(content);

  const explanationSections = [];

  // 1. Concept introduction
  const conceptSection = findSection(sections, 'concept') ||
                        findSection(sections, 'overview') ||
                        findSection(sections, 'introduction');

  if (conceptSection) {
    explanationSections.push('## Core Concepts');
    explanationSections.push('');
    explanationSections.push(conceptSection.content.trim());
    explanationSections.push('');
  }

  // 2. Design rationale (why built this way)
  const designSection = findSection(sections, 'design') ||
                       findSection(sections, 'rationale') ||
                       findSection(sections, 'architecture');

  if (designSection) {
    explanationSections.push('## Design Rationale');
    explanationSections.push('');
    explanationSections.push(designSection.content.trim());
    explanationSections.push('');
  }

  // 3. Related concepts
  const relatedSection = findSection(sections, 'related') ||
                        findSection(sections, 'background') ||
                        findSection(sections, 'context');

  if (relatedSection) {
    explanationSections.push('## Related Concepts');
    explanationSections.push('');
    explanationSections.push(relatedSection.content.trim());
    explanationSections.push('');
  }

  // 4. Tradeoffs and alternatives
  const tradeoffsSection = findSection(sections, 'tradeoff') ||
                          findSection(sections, 'comparison') ||
                          findSection(sections, 'alternative');

  if (tradeoffsSection) {
    explanationSections.push('## Tradeoffs and Alternatives');
    explanationSections.push('');
    explanationSections.push(tradeoffsSection.content.trim());
    explanationSections.push('');
  }

  // 5. Common misconceptions
  const misconceptionsSection = findSection(sections, 'misconception') ||
                                findSection(sections, 'gotcha') ||
                                findSection(sections, 'pitfall');

  if (misconceptionsSection) {
    explanationSections.push('## Common Misconceptions');
    explanationSections.push('');
    explanationSections.push(misconceptionsSection.content.trim());
    explanationSections.push('');
  }

  // 6. Historical context (if available)
  const historySection = findSection(sections, 'history') ||
                        findSection(sections, 'evolution') ||
                        findSection(sections, 'background');

  if (historySection) {
    explanationSections.push('## Historical Context');
    explanationSections.push('');
    explanationSections.push(historySection.content.trim());
    explanationSections.push('');
  }

  const explanationContent = explanationSections.join('\n');

  // Create explanation frontmatter
  const explanationFrontmatter = {
    ...frontmatter,
    views: ['explanation'],
    title: frontmatter.title.includes('Explanation') ? frontmatter.title : `Understanding ${frontmatter.title}`,
  };

  const hash = createHash('sha256').update(explanationContent).digest('hex');

  return {
    frontmatter: explanationFrontmatter,
    content: explanationContent,
    view: 'explanation',
    hash,
  };
}

// =============================================================================
// Frontmatter Unification
// =============================================================================

/**
 * Unify frontmatter across projections
 *
 * Ensures all 4 views share the same provenance chain:
 * - Same o_hash (ontological hash)
 * - Same policy_id
 * - Same receipts chain
 * - Same createdAt
 * - Same lastProved
 *
 * Only views array changes per projection.
 *
 * @param {Frontmatter} baseFrontmatter - Base frontmatter from canonical source
 * @param {'tutorial'|'howto'|'reference'|'explanation'} view - Target view
 * @returns {Frontmatter}
 */
export function unifyFrontmatter(baseFrontmatter, view) {
  return {
    ...baseFrontmatter,
    views: [view],
    // Preserve immutable provenance fields
    o_hash: baseFrontmatter.o_hash,
    policy_id: baseFrontmatter.policy_id,
    receipts: baseFrontmatter.receipts ? [...baseFrontmatter.receipts] : [],
    createdAt: baseFrontmatter.createdAt,
    lastProved: baseFrontmatter.lastProved,
  };
}

// =============================================================================
// Projection Matrix
// =============================================================================

/**
 * Define transformation rules for each Diataxis view
 *
 * @returns {Object} Projection matrix with rules for each view
 */
export function diataxisProjectionMatrix() {
  return {
    tutorial: {
      tone: 'encouraging, forgiving',
      sections: [
        'motivation (why)',
        'prerequisites',
        'step-by-step guide',
        'working example',
        'common pitfalls (1-2 max)',
      ],
      exclude: ['advanced topics', 'edge cases', 'internals', 'complete troubleshooting'],
      transformations: [
        'add encouraging phrases',
        'simplify technical jargon',
        'emphasize hands-on learning',
        'include complete runnable examples',
      ],
      audience: 'beginners',
      goal: 'understanding the concept through practice',
    },

    howto: {
      tone: 'direct, practical',
      sections: [
        'problem statement',
        'prerequisites',
        'solution (code + explanation)',
        'variations (alternatives)',
        'troubleshooting',
      ],
      exclude: ['basic concepts', 'motivation', 'history'],
      transformations: [
        'assume basic knowledge',
        'focus on specific use case',
        'provide actionable steps',
        'include multiple approaches',
      ],
      audience: 'practitioners with basic knowledge',
      goal: 'solving a specific problem',
    },

    reference: {
      tone: 'formal, precise',
      sections: [
        'API tables (summary)',
        'detailed function/class documentation',
        'parameters with types',
        'return values',
        'exceptions',
        'usage examples (1-2 per item)',
      ],
      exclude: ['motivation', 'step-by-step guides', 'conceptual explanations'],
      transformations: [
        'alphabetically sort items',
        'generate parameter tables',
        'extract type information',
        'provide minimal but complete examples',
      ],
      audience: 'developers looking up specifics',
      goal: 'finding accurate API information quickly',
    },

    explanation: {
      tone: 'reflective, contextual',
      sections: [
        'concept introduction',
        'design rationale',
        'related concepts',
        'tradeoffs and alternatives',
        'common misconceptions',
        'historical context',
      ],
      exclude: ['step-by-step guides', 'code examples (unless illustrating concepts)', 'troubleshooting'],
      transformations: [
        'emphasize why over how',
        'discuss design decisions',
        'compare with alternatives',
        'provide broader context',
      ],
      audience: 'developers seeking deeper understanding',
      goal: 'understanding the bigger picture and tradeoffs',
    },
  };
}

// =============================================================================
// Validation
// =============================================================================

/**
 * Validate projection result
 *
 * Checks:
 * - All code examples are syntactically valid (heuristic)
 * - All external references are present
 * - Tone matches expected for view type
 * - Required sections are present
 *
 * @param {SourceDoc} source - Original source document
 * @param {'tutorial'|'howto'|'reference'|'explanation'} view - Target view
 * @param {ProjectionResult} result - Projection result
 * @returns {ValidationResult}
 */
export function validateProjection(source, view, result) {
  const warnings = [];
  const errors = [];

  // 1. Validate code blocks (syntax check)
  const codeBlocks = extractCodeBlocks(result.content);
  for (const block of codeBlocks) {
    if (block.language === 'javascript' || block.language === 'js') {
      try {
        // Simple syntax check (not full parse)
        if (block.code.includes('function') && !block.code.includes('{')) {
          warnings.push(`Code block may be incomplete: missing function body`);
        }
        if (block.code.includes('import') && !block.code.includes('from')) {
          warnings.push(`Code block may be incomplete: import statement missing 'from'`);
        }
      } catch (err) {
        warnings.push(`Potential syntax issue in code block: ${err.message}`);
      }
    }
  }

  // 2. Check for required sections based on view
  const matrix = diataxisProjectionMatrix();
  const requiredSections = matrix[view].sections;
  const sections = extractSections(result.content);

  for (const required of requiredSections.slice(0, 3)) { // Check first 3 required sections
    const normalized = required.toLowerCase().replace(/[()]/g, '');
    const found = sections.some(s => {
      const sectionTitle = s.title.toLowerCase();
      return normalized.split(' ').some(word => sectionTitle.includes(word));
    });

    if (!found) {
      warnings.push(`Expected section not found: ${required}`);
    }
  }

  // 3. Tone check (heuristic)
  const content = result.content.toLowerCase();

  if (view === 'tutorial') {
    const hasEncouragement = content.includes('let\'s') ||
                            content.includes('you\'ll') ||
                            content.includes('tip:') ||
                            content.includes('great!');
    if (!hasEncouragement) {
      warnings.push('Tutorial should have encouraging tone (consider adding phrases like "let\'s", "you\'ll", etc.)');
    }
  }

  if (view === 'howto') {
    const hasProblemStatement = sections.some(s =>
      s.title.toLowerCase().includes('problem') ||
      s.title.toLowerCase().includes('solution')
    );
    if (!hasProblemStatement) {
      warnings.push('How-to should clearly state the problem and solution');
    }
  }

  if (view === 'reference') {
    const hasTables = result.content.includes('|');
    if (!hasTables && codeBlocks.length === 0) {
      warnings.push('Reference should include API tables or function signatures');
    }
  }

  if (view === 'explanation') {
    const hasConceptual = sections.some(s =>
      s.title.toLowerCase().includes('concept') ||
      s.title.toLowerCase().includes('design') ||
      s.title.toLowerCase().includes('rationale')
    );
    if (!hasConceptual) {
      warnings.push('Explanation should focus on concepts and design rationale');
    }
  }

  // 4. Validate frontmatter preservation
  if (source.frontmatter.o_hash && result.frontmatter.o_hash !== source.frontmatter.o_hash) {
    errors.push('o_hash must be preserved across projections');
  }

  if (source.frontmatter.policy_id && result.frontmatter.policy_id !== source.frontmatter.policy_id) {
    errors.push('policy_id must be preserved across projections');
  }

  if (source.frontmatter.createdAt && result.frontmatter.createdAt !== source.frontmatter.createdAt) {
    errors.push('createdAt timestamp must be preserved across projections');
  }

  // 5. Content length checks
  const wordCount = result.content.split(/\s+/).length;

  if (view === 'tutorial' && wordCount < 200) {
    warnings.push(`Tutorial seems short (${wordCount} words). Consider adding more explanatory content.`);
  }

  if (view === 'reference' && wordCount < 100) {
    warnings.push(`Reference seems short (${wordCount} words). Ensure all API items are documented.`);
  }

  return {
    valid: errors.length === 0,
    warnings,
    errors: errors.length > 0 ? errors : undefined,
  };
}

// =============================================================================
// High-Level Projection API
// =============================================================================

/**
 * Project canonical source to all 4 Diataxis views
 *
 * @param {SourceDoc} source - Canonical source document
 * @param {ApiManifest} [apiManifest] - API surface (optional)
 * @returns {Object} All 4 projections with validation results
 */
export function projectToAllViews(source, apiManifest) {
  const tutorial = projectToTutorial(source, apiManifest);
  const howto = projectToHowTo(source, apiManifest);
  const reference = projectToReference(source, apiManifest);
  const explanation = projectToExplanation(source, apiManifest);

  return {
    tutorial: {
      ...tutorial,
      validation: validateProjection(source, 'tutorial', tutorial),
    },
    howto: {
      ...howto,
      validation: validateProjection(source, 'howto', howto),
    },
    reference: {
      ...reference,
      validation: validateProjection(source, 'reference', reference),
    },
    explanation: {
      ...explanation,
      validation: validateProjection(source, 'explanation', explanation),
    },
  };
}

/**
 * Serialize projection result to markdown file
 *
 * @param {ProjectionResult} result - Projection result
 * @returns {string} Complete markdown file with frontmatter
 */
export function serializeProjection(result) {
  const frontmatterYaml = serializeFrontmatter(result.frontmatter);
  return `---\n${frontmatterYaml}\n---\n\n${result.content}`;
}

/**
 * Parse markdown file into source document
 *
 * @param {string} markdown - Complete markdown file
 * @param {ApiManifest} [apiManifest] - API surface (optional)
 * @returns {SourceDoc}
 */
export function parseSourceDoc(markdown, apiManifest) {
  const { frontmatter, content } = parseFrontmatter(markdown);

  // Validate frontmatter
  const validated = FrontmatterSchema.parse(frontmatter);

  return {
    frontmatter: validated,
    content,
    apiManifest,
  };
}

// =============================================================================
// Exports
// =============================================================================

export default {
  // Projection functions
  projectToTutorial,
  projectToHowTo,
  projectToReference,
  projectToExplanation,
  projectToAllViews,

  // Utilities
  unifyFrontmatter,
  diataxisProjectionMatrix,
  validateProjection,
  serializeProjection,
  parseSourceDoc,

  // Schemas
  FrontmatterSchema,
  ApiManifestSchema,
  SourceDocSchema,
  ProjectionResultSchema,
  ValidationResultSchema,
};
