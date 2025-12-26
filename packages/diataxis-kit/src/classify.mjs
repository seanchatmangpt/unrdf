/**
 * @file Diátaxis classifier - evidence-driven documentation classification
 * @module classify
 * @description Consumes InventoryEntry + EvidenceSnapshot and produces DiataxisEntry
 * with stubs for tutorials, how-tos, reference, and explanation.
 */

import { createDiataxisEntry, validateDiataxisEntry, ensureMinimumDiataxis } from './diataxis-schema.mjs';
import { hashEvidence } from './evidence.mjs';

/**
 * Generate a stable ID from a title string with type prefix
 * @param {string} title - Title to convert
 * @param {string} type - Type prefix (tutorial, howto, reference, explanation)
 * @returns {string} Prefixed kebab-case ID
 */
function generateId(title, type) {
  const slug = title
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '');
  return `${type}-${slug}`;
}

/**
 * Calculate confidence score based on evidence strength
 * @param {Object} evidence - Evidence object containing various sources
 * @param {string} type - Type of documentation (tutorials, howtos, reference, explanation)
 * @returns {number} Confidence score 0-1
 */
function confidenceScore(evidence, type) {
  let score = 0;

  switch (type) {
    case 'tutorials':
      // +0.3 for examples/, +0.3 for README, +0.1 for keywords
      if (evidence.examplesFiles && evidence.examplesFiles.length > 0) {
        score += 0.3;
      }
      if (evidence.readmeHeadings && evidence.readmeHeadings.some(h =>
        /tutorial|getting started|quick start|first steps/i.test(h)
      )) {
        score += 0.3;
      }
      if (evidence.keywords && evidence.keywords.length > 0) {
        score += 0.1;
      }
      // Additional boost for README content with intro sections
      if (evidence.readmeContent && /## (tutorial|getting started|quick start)/i.test(evidence.readmeContent)) {
        score += 0.3;
      }
      break;

    case 'howtos':
      // +0.25 README sections, +0.25 bin, +0.25 keywords, +0.25 tests
      if (evidence.readmeHeadings && evidence.readmeHeadings.some(h =>
        /usage|api|options|configuration|how to/i.test(h)
      )) {
        score += 0.25;
      }
      if (evidence.binEntries && Object.keys(evidence.binEntries).length > 0) {
        score += 0.25;
      }
      if (evidence.keywords && evidence.keywords.length > 2) {
        score += 0.25;
      }
      if (evidence.testFileCount && evidence.testFileCount > 0) {
        score += 0.25;
      }
      break;

    case 'reference':
      // +0.5 exports, +0.3 bin, +0.2 README API
      if (evidence.exportSurface && Object.keys(evidence.exportSurface).length > 0) {
        score += 0.5;
      }
      if (evidence.binEntries && Object.keys(evidence.binEntries).length > 0) {
        score += 0.3;
      }
      if (evidence.readmeHeadings && evidence.readmeHeadings.some(h =>
        /api|reference|exports|methods/i.test(h)
      )) {
        score += 0.2;
      }
      break;

    case 'explanation':
      // +0.3 README, +0.3 docs/, +0.4 keywords
      if (evidence.readmeContent && evidence.readmeContent.length > 100) {
        score += 0.3;
      }
      if (evidence.docsFiles && evidence.docsFiles.length > 0) {
        score += 0.3;
      }
      if (evidence.keywords && evidence.keywords.length > 0) {
        score += 0.4;
      }
      break;

    default:
      return 0;
  }

  return Math.min(score, 1.0);
}

/**
 * Extract API section content from README (first 500 chars after API heading)
 * @param {string|null} readmeContent - README content
 * @returns {string} API section snippet or empty string
 */
function extractApiSection(readmeContent) {
  if (!readmeContent) return '';

  const apiMatch = readmeContent.match(/##\s+(API|Reference|Exports|Methods)\s*\n([\s\S]{0,500})/i);
  return apiMatch ? apiMatch[2].trim() : '';
}

/**
 * Extract first sentence from README intro (architecture description)
 * @param {string|null} readmeContent - README content
 * @returns {string} First sentence or empty string
 */
function extractFirstSentence(readmeContent) {
  if (!readmeContent) return '';

  // Skip over title and find first paragraph
  const paragraphMatch = readmeContent.match(/^#[^\n]*\n+([^#\n][^\n]*\.)/m);
  return paragraphMatch ? paragraphMatch[1].trim() : '';
}

/**
 * Check if README contains tradeoff/comparison language
 * @param {string|null} readmeContent - README content
 * @returns {boolean} True if tradeoff language detected
 */
function hasTradeoffLanguage(readmeContent) {
  if (!readmeContent) return false;
  return /\b(trade[-\s]?off|vs\.?|versus|compared to|alternative|choice)\b/i.test(readmeContent);
}

/**
 * Generate tutorial stubs based on evidence
 * @param {string} packageName - Package name
 * @param {Object} evidence - Evidence snapshot
 * @returns {Array<{id: string, title: string, goal: string, prerequisites: string[], stepsOutline: string[], confidenceScore: number, source: string[]}>} Tutorial stubs
 */
function generateTutorials(packageName, evidence) {
  const tutorials = [];
  const sources = [];

  // Determine confidence and sources
  let confidence = 0;
  const hasExamples = evidence.examplesFiles && evidence.examplesFiles.length > 0;
  const hasReadmeIntro = evidence.readmeHeadings && evidence.readmeHeadings.some(h =>
    /tutorial|getting started|quick start/i.test(h)
  );

  if (hasExamples) {
    sources.push('examples');
    confidence += 0.6;
  }
  if (hasReadmeIntro) {
    sources.push('readme');
    confidence += 0.3;
  }
  if (evidence.keywords && evidence.keywords.length > 0) {
    sources.push('keywords');
    confidence += 0.1;
  }

  confidence = Math.min(confidence, 1.0);

  // Always generate "Getting Started" tutorial
  const prerequisites = [];
  if (evidence.keywords) {
    // Infer prerequisites from keywords
    if (evidence.keywords.some(k => /rdf|semantic|triple/i.test(k))) {
      prerequisites.push('Basic RDF knowledge');
    }
    if (evidence.keywords.some(k => /graph|query|sparql/i.test(k))) {
      prerequisites.push('Understanding of graph databases');
    }
    if (evidence.keywords.some(k => /node|javascript|npm/i.test(k))) {
      prerequisites.push('Node.js environment');
    }
  }

  // Generate steps from evidence headings or use generic
  let stepsOutline = [];
  if (hasReadmeIntro) {
    // Try to extract steps from README headings
    const stepHeadings = evidence.readmeHeadings.filter(h =>
      /install|setup|usage|example|run/i.test(h)
    ).slice(0, 5);

    if (stepHeadings.length >= 3) {
      stepsOutline = stepHeadings;
    }
  }

  if (stepsOutline.length === 0) {
    // Use generic steps
    stepsOutline = [
      'Install the package',
      'Import required modules',
      'Create a basic example',
      'Run and verify output'
    ];
  }

  tutorials.push({
    id: generateId(`Getting Started with ${packageName}`, 'tutorial'),
    title: `Getting Started with ${packageName}`,
    goal: `Learn how to set up and use ${packageName} in your project`,
    prerequisites,
    stepsOutline,
    confidenceScore: confidence,
    source: sources.length > 0 ? sources : ['inferred']
  });

  // Generate "First Steps" if multiple examples exist
  if (hasExamples && evidence.examplesFiles.length > 2) {
    tutorials.push({
      id: generateId('First Steps', 'tutorial'),
      title: 'First Steps',
      goal: 'Complete your first practical examples',
      prerequisites: [`Completed "Getting Started with ${packageName}"`],
      stepsOutline: evidence.examplesFiles.slice(0, 5).map(file => `Work through ${file}`),
      confidenceScore: 0.8,
      source: ['examples']
    });
  }

  // Sort by id for stable ordering
  tutorials.sort((a, b) => a.id.localeCompare(b.id));

  return tutorials;
}

/**
 * Generate how-to stubs based on evidence (minimum 2 required)
 * @param {string} packageName - Package name
 * @param {Object} evidence - Evidence snapshot
 * @returns {Array<{id: string, title: string, task: string, context: string, steps: string[], confidenceScore: number, source: string[]}>} How-to stubs
 */
function generateHowTos(packageName, evidence) {
  const howtos = [];

  // Check for Configuration section
  if (evidence.readmeHeadings && evidence.readmeHeadings.some(h => /configuration|config|options/i.test(h))) {
    howtos.push({
      id: generateId(`Configure ${packageName}`, 'howto'),
      title: `Configure ${packageName}`,
      task: `Set up custom configuration for ${packageName}`,
      context: 'When you need to customize package behavior',
      steps: [
        'Create configuration file',
        'Define configuration options',
        'Apply configuration to package'
      ],
      confidenceScore: 1.0,
      source: ['readme']
    });
  }

  // Check for CLI usage (bin entries)
  if (evidence.binEntries && Object.keys(evidence.binEntries).length > 0) {
    const binNames = Object.keys(evidence.binEntries);
    howtos.push({
      id: generateId('Use the CLI', 'howto'),
      title: 'Use the CLI',
      task: `Execute ${binNames[0]} command-line interface`,
      context: 'When you need to use the package from the command line',
      steps: [
        'Install package globally or locally',
        'Run CLI command with options',
        'Process command output'
      ],
      confidenceScore: 1.0,
      source: ['bin']
    });
  }

  // Check for integration opportunities (keywords)
  if (evidence.keywords && evidence.keywords.length > 2) {
    const keyword = evidence.keywords[0];
    howtos.push({
      id: generateId(`Integrate with ${keyword}`, 'howto'),
      title: `Integrate with ${keyword}`,
      task: `Connect ${packageName} with ${keyword}`,
      context: `When working with ${keyword} in your project`,
      steps: [
        'Install dependencies',
        'Set up integration',
        'Test integration'
      ],
      confidenceScore: 0.7,
      source: ['keywords', 'inferred']
    });
  }

  // Check for error handling (test files exist)
  if (evidence.testFileCount && evidence.testFileCount > 0) {
    howtos.push({
      id: generateId('Handle Errors', 'howto'),
      title: 'Handle Errors',
      task: `Implement error handling for ${packageName}`,
      context: 'When you need robust error handling in production',
      steps: [
        'Set up try-catch blocks',
        'Handle common error types',
        'Log errors appropriately'
      ],
      confidenceScore: 0.5,
      source: ['tests']
    });
  }

  // Ensure minimum 2 how-tos (add generic ones if needed)
  if (howtos.length === 0) {
    howtos.push({
      id: generateId(`Use ${packageName}`, 'howto'),
      title: `Use ${packageName}`,
      task: `Perform basic operations with ${packageName}`,
      context: 'When you need to accomplish common tasks',
      steps: [
        'Import the package',
        'Initialize with options',
        'Execute operations'
      ],
      confidenceScore: 0.4,
      source: ['inferred']
    });
  }

  if (howtos.length === 1) {
    howtos.push({
      id: generateId('Troubleshoot Common Issues', 'howto'),
      title: 'Troubleshoot Common Issues',
      task: 'Resolve common problems and errors',
      context: 'When encountering issues during usage',
      steps: [
        'Check configuration',
        'Review error messages',
        'Apply fixes'
      ],
      confidenceScore: 0.4,
      source: ['inferred']
    });
  }

  // Sort by id for stable ordering
  howtos.sort((a, b) => a.id.localeCompare(b.id));

  return howtos;
}

/**
 * Generate reference documentation based on exports and bin
 * @param {string} packageName - Package name
 * @param {Object} evidence - Evidence snapshot
 * @returns {{id: string, title: string, items: Array, confidenceScore: number, source: string[]}} Reference object
 */
function generateReference(packageName, evidence) {
  const items = [];
  const sources = [];
  let confidence = 0;

  // Extract from exports field
  if (evidence.exportSurface && typeof evidence.exportSurface === 'object') {
    const exportKeys = Object.keys(evidence.exportSurface);
    if (exportKeys.length > 0) {
      sources.push('exports');
      confidence = 1.0;

      for (const key of exportKeys.sort()) {
        const exportPath = evidence.exportSurface[key];
        items.push({
          name: key === '.' ? packageName : key,
          type: 'export',
          description: `Export: ${key}`,
          example: typeof exportPath === 'string' ? `import from "${exportPath}"` : null
        });
      }
    }
  }

  // Extract from bin field
  if (evidence.binEntries && typeof evidence.binEntries === 'object') {
    const binKeys = Object.keys(evidence.binEntries);
    if (binKeys.length > 0) {
      sources.push('bin');
      if (confidence === 0) confidence = 0.8;

      for (const binName of binKeys.sort()) {
        const binPath = evidence.binEntries[binName];
        items.push({
          name: binName,
          type: 'bin',
          description: `CLI command: ${binName}`,
          example: `${binName} [options]`
        });
      }
    }
  }

  // Parse README for API section
  const apiSection = extractApiSection(evidence.readmeContent);
  if (apiSection) {
    sources.push('readme');
    if (confidence === 0) confidence = 0.6;
  }

  // Add placeholder if no exports or bin found
  if (items.length === 0) {
    items.push({
      name: 'unknown',
      type: 'unknown',
      description: 'Reference documentation to be added',
      example: null
    });
    sources.push('inferred');
    confidence = 0.5;
  }

  // Sort items by name for stable ordering
  items.sort((a, b) => a.name.localeCompare(b.name));

  return {
    id: generateId(`${packageName} Reference`, 'reference'),
    title: `${packageName} Reference`,
    items,
    confidenceScore: confidence,
    source: sources
  };
}

/**
 * Generate explanation documentation based on README and docs
 * @param {string} packageName - Package name
 * @param {Object} evidence - Evidence snapshot
 * @returns {{id: string, title: string, concepts: string[], architecture: string, tradeoffs: string[], confidenceScore: number, source: string[]}} Explanation object
 */
function generateExplanation(packageName, evidence) {
  const concepts = [];
  const tradeoffs = [];
  const sources = [];
  let confidence = 0;

  // Extract concepts from keywords
  if (evidence.keywords && evidence.keywords.length > 0) {
    concepts.push(...evidence.keywords.slice(0, 5));
    sources.push('keywords');
    confidence += 0.4;
  }

  // Generate architecture description from README intro
  let architecture = '';
  if (evidence.readmeContent) {
    architecture = extractFirstSentence(evidence.readmeContent);
    if (architecture) {
      sources.push('readme');
      confidence += 0.3;
    }
  }

  if (!architecture) {
    architecture = `${packageName} is a package in the UNRDF ecosystem`;
  }

  // Check for docs directory
  if (evidence.docsFiles && evidence.docsFiles.length > 0) {
    sources.push('docs');
    confidence += 0.3;
  }

  // List tradeoffs if detected
  if (hasTradeoffLanguage(evidence.readmeContent)) {
    tradeoffs.push('Performance vs. flexibility tradeoffs discussed in README');
    if (!sources.includes('readme')) {
      sources.push('readme');
    }
  }

  // Generate default tradeoff stubs if none found
  if (tradeoffs.length === 0) {
    tradeoffs.push(
      'Ease of use vs. advanced configuration options',
      'Memory usage vs. processing speed',
      'Bundle size vs. feature completeness'
    );
    if (!sources.includes('inferred')) {
      sources.push('inferred');
    }
  }

  confidence = Math.min(confidence, 1.0);

  return {
    id: generateId(`${packageName} Explanation`, 'explanation'),
    title: `Understanding ${packageName}`,
    concepts: concepts.length > 0 ? concepts : ['core functionality'],
    architecture,
    tradeoffs,
    confidenceScore: confidence,
    source: sources.length > 0 ? sources : ['inferred']
  };
}

/**
 * Classify a package and produce DiataxisEntry with stubs
 * @param {import('./inventory.mjs').PackageEntry} packageEntry - Package metadata from inventory
 * @param {import('./evidence.mjs').EvidenceSnapshot} evidenceSnapshot - Evidence collected from package
 * @returns {Promise<import('./diataxis-schema.mjs').DiataxisEntry>} Complete Diátaxis entry
 * @throws {Error} If packageEntry is invalid
 */
export async function classifyPackage(packageEntry, evidenceSnapshot) {
  // Validate packageEntry
  if (!packageEntry || typeof packageEntry !== 'object') {
    throw new Error('Invalid packageEntry: must be an object');
  }
  if (!packageEntry.name || typeof packageEntry.name !== 'string') {
    throw new Error('Invalid packageEntry: name must be a non-empty string');
  }
  if (!packageEntry.version || typeof packageEntry.version !== 'string') {
    throw new Error('Invalid packageEntry: version must be a non-empty string');
  }

  // Use empty snapshot if evidenceSnapshot is missing
  const evidence = evidenceSnapshot || {
    readmeContent: null,
    readmeHeadings: [],
    examplesFiles: [],
    examplesSnippets: {},
    docsFiles: [],
    docsSnippets: {},
    srcFiles: [],
    testFileCount: 0,
    binEntries: {},
    exportSurface: {},
    keywords: [],
    hasLicense: false,
    hasTsConfig: false,
    fingerprint: ''
  };

  // Generate all Diátaxis sections
  const tutorials = generateTutorials(packageEntry.name, evidence);
  const howtos = generateHowTos(packageEntry.name, evidence);
  const reference = generateReference(packageEntry.name, evidence);
  const explanation = generateExplanation(packageEntry.name, evidence);

  // Calculate overall confidence scores
  const confidence = {
    tutorials: confidenceScore(evidence, 'tutorials'),
    howtos: confidenceScore(evidence, 'howtos'),
    reference: confidenceScore(evidence, 'reference'),
    explanation: confidenceScore(evidence, 'explanation')
  };

  // Create Diátaxis entry
  const entry = createDiataxisEntry(packageEntry.name, packageEntry.version, {
    readmeHeadings: evidence.readmeHeadings || [],
    docsFiles: evidence.docsFiles || [],
    examplesFiles: evidence.examplesFiles || [],
    tutorials,
    howtos,
    reference,
    explanation,
    confidence
  });

  // Validate the entry
  const validation = validateDiataxisEntry(entry);
  if (!validation.valid) {
    throw new Error(`Generated invalid DiataxisEntry: ${validation.errors.join(', ')}`);
  }

  // Ensure minimum structure
  const finalEntry = ensureMinimumDiataxis(entry);

  return finalEntry;
}
