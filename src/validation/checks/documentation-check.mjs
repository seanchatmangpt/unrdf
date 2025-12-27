/**
 * @fileoverview Documentation Check - Validates documentation coverage and quality
 *
 * **Checks performed**:
 * 1. README.md existence and quality
 * 2. API documentation coverage (>=80%)
 * 3. Code examples presence
 * 4. Type definitions/hints
 * 5. Changelog maintenance
 * 6. Usage examples
 * 7. Installation instructions
 * 8. Contributing guidelines
 *
 * **Scoring**:
 * - 100: Excellent documentation
 * - 95-99: Good documentation with minor gaps
 * - 80-94: Adequate documentation
 * - 60-79: Missing important documentation
 * - <60: Critical documentation gaps
 *
 * @module validation/checks/documentation-check
 */

import { readdir, readFile, stat, access } from 'node:fs/promises';
import { join, extname, relative, basename } from 'node:path';

/**
 * Documentation check thresholds
 */
export const DOCUMENTATION_THRESHOLDS = {
  minReadmeLength: 500,
  minApiCoverage: 80,
  minExampleCount: 3,
  minReadmeSections: 4
};

/**
 * Required README sections
 */
const README_SECTIONS = {
  required: [
    { name: 'Installation', patterns: [/^#+\s*installation/mi, /^#+\s*getting started/mi, /^#+\s*setup/mi] },
    { name: 'Usage', patterns: [/^#+\s*usage/mi, /^#+\s*quick start/mi, /^#+\s*examples?/mi] }
  ],
  recommended: [
    { name: 'API', patterns: [/^#+\s*api/mi, /^#+\s*documentation/mi, /^#+\s*reference/mi] },
    { name: 'License', patterns: [/^#+\s*license/mi, /^#+\s*licensing/mi] },
    { name: 'Contributing', patterns: [/^#+\s*contribut/mi] },
    { name: 'Changelog', patterns: [/^#+\s*changelog/mi, /^#+\s*changes/mi, /^#+\s*history/mi] }
  ]
};

/**
 * Documentation file patterns
 */
const DOC_FILES = {
  readme: ['README.md', 'README', 'readme.md', 'Readme.md'],
  changelog: ['CHANGELOG.md', 'CHANGES.md', 'HISTORY.md', 'changelog.md'],
  contributing: ['CONTRIBUTING.md', 'contributing.md', 'CONTRIBUTE.md'],
  license: ['LICENSE', 'LICENSE.md', 'LICENSE.txt', 'LICENCE.md'],
  codeOfConduct: ['CODE_OF_CONDUCT.md', 'CONDUCT.md'],
  security: ['SECURITY.md', 'security.md']
};

/**
 * Check if README exists and analyze quality
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} README analysis result
 */
async function analyzeReadme(packagePath) {
  const result = {
    exists: false,
    path: null,
    length: 0,
    wordCount: 0,
    hasCodeBlocks: false,
    codeBlockCount: 0,
    sections: [],
    missingSections: [],
    quality: 'none'
  };

  // Find README
  for (const readmeName of DOC_FILES.readme) {
    try {
      const readmePath = join(packagePath, readmeName);
      const content = await readFile(readmePath, 'utf-8');

      result.exists = true;
      result.path = readmeName;
      result.length = content.length;
      result.wordCount = content.split(/\s+/).filter(w => w.length > 0).length;

      // Count code blocks
      const codeBlocks = content.match(/```[\s\S]*?```/g) || [];
      result.hasCodeBlocks = codeBlocks.length > 0;
      result.codeBlockCount = codeBlocks.length;

      // Check sections
      for (const section of [...README_SECTIONS.required, ...README_SECTIONS.recommended]) {
        const hasSection = section.patterns.some(pattern => pattern.test(content));
        if (hasSection) {
          result.sections.push(section.name);
        }
      }

      // Identify missing required sections
      for (const section of README_SECTIONS.required) {
        const hasSection = section.patterns.some(pattern => pattern.test(content));
        if (!hasSection) {
          result.missingSections.push(section.name);
        }
      }

      // Determine quality
      if (result.length >= 2000 && result.sections.length >= 5 && result.codeBlockCount >= 3) {
        result.quality = 'excellent';
      } else if (result.length >= 1000 && result.sections.length >= 3 && result.codeBlockCount >= 1) {
        result.quality = 'good';
      } else if (result.length >= DOCUMENTATION_THRESHOLDS.minReadmeLength) {
        result.quality = 'adequate';
      } else {
        result.quality = 'minimal';
      }

      break;
    } catch (error) {
      // File not found, continue to next option
    }
  }

  return result;
}

/**
 * Check for documentation files
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} Documentation files check result
 */
async function checkDocumentationFiles(packagePath) {
  const result = {
    hasChangelog: false,
    hasContributing: false,
    hasLicense: false,
    hasCodeOfConduct: false,
    hasSecurity: false,
    hasDocsFolder: false,
    docFiles: []
  };

  // Check standard doc files
  for (const [type, files] of Object.entries(DOC_FILES)) {
    for (const file of files) {
      try {
        await access(join(packagePath, file));
        result[`has${type.charAt(0).toUpperCase() + type.slice(1)}`] = true;
        result.docFiles.push(file);
        break;
      } catch {
        // File not found
      }
    }
  }

  // Check for docs folder
  try {
    const stats = await stat(join(packagePath, 'docs'));
    if (stats.isDirectory()) {
      result.hasDocsFolder = true;

      // Count docs
      const docEntries = await readdir(join(packagePath, 'docs'));
      result.docsCount = docEntries.filter(e => e.endsWith('.md')).length;
    }
  } catch {
    // Docs folder not found
  }

  return result;
}

/**
 * Analyze JSDoc coverage in source files
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} JSDoc coverage result
 */
async function analyzeJsdocCoverage(packagePath) {
  const result = {
    totalExports: 0,
    documentedExports: 0,
    coverage: 0,
    filesAnalyzed: 0,
    undocumentedItems: []
  };

  async function scanFile(filePath) {
    try {
      const content = await readFile(filePath, 'utf-8');

      // Find exported items
      const exportMatches = [
        ...content.matchAll(/export\s+(?:async\s+)?function\s+(\w+)/g),
        ...content.matchAll(/export\s+(?:const|let|var)\s+(\w+)/g),
        ...content.matchAll(/export\s+class\s+(\w+)/g),
        ...content.matchAll(/export\s+default\s+(?:class\s+)?(\w+)?/g)
      ];

      for (const match of exportMatches) {
        const name = match[1] || 'default';
        result.totalExports++;

        // Check if preceded by JSDoc
        const beforeExport = content.substring(Math.max(0, match.index - 500), match.index);
        const hasJsdoc = /\/\*\*[\s\S]*?\*\/\s*$/.test(beforeExport);

        if (hasJsdoc) {
          result.documentedExports++;
        } else {
          result.undocumentedItems.push({
            file: relative(packagePath, filePath),
            name
          });
        }
      }

      result.filesAnalyzed++;
    } catch (error) {
      // File not readable
    }
  }

  async function scanDir(dir) {
    try {
      const entries = await readdir(dir, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(dir, entry.name);

        if (entry.isDirectory()) {
          if (!['node_modules', 'dist', 'build', 'coverage', '.git', 'test', 'tests', '__tests__'].includes(entry.name)) {
            await scanDir(fullPath);
          }
        } else if (entry.isFile()) {
          if (/\.(mjs|js|ts|jsx|tsx)$/.test(entry.name) && !entry.name.includes('.test.') && !entry.name.includes('.spec.')) {
            await scanFile(fullPath);
          }
        }
      }
    } catch (error) {
      // Directory not accessible
    }
  }

  await scanDir(packagePath);

  result.coverage = result.totalExports > 0
    ? Math.round((result.documentedExports / result.totalExports) * 100)
    : 100;

  return result;
}

/**
 * Check for examples
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} Examples check result
 */
async function checkExamples(packagePath) {
  const result = {
    hasExamplesFolder: false,
    exampleCount: 0,
    readmeExamples: 0,
    exampleFiles: []
  };

  // Check for examples folder
  const exampleFolders = ['examples', 'example', 'demos', 'demo', 'samples'];

  for (const folder of exampleFolders) {
    try {
      const examplePath = join(packagePath, folder);
      const stats = await stat(examplePath);

      if (stats.isDirectory()) {
        result.hasExamplesFolder = true;

        const entries = await readdir(examplePath);
        result.exampleFiles = entries.filter(e =>
          e.endsWith('.mjs') || e.endsWith('.js') || e.endsWith('.ts') || e.endsWith('.md')
        );
        result.exampleCount = result.exampleFiles.length;
        break;
      }
    } catch {
      // Folder not found
    }
  }

  // Count code examples in README
  try {
    const readmePath = join(packagePath, 'README.md');
    const readme = await readFile(readmePath, 'utf-8');
    const codeBlocks = readme.match(/```(?:javascript|js|typescript|ts|mjs)[\s\S]*?```/g) || [];
    result.readmeExamples = codeBlocks.length;
  } catch {
    // README not found
  }

  return result;
}

/**
 * Check package.json documentation fields
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} Package.json documentation check
 */
async function checkPackageJsonDocs(packagePath) {
  const result = {
    hasDescription: false,
    hasKeywords: false,
    hasRepository: false,
    hasHomepage: false,
    hasBugs: false,
    hasAuthor: false,
    hasTypes: false,
    hasMain: false,
    hasExports: false
  };

  try {
    const content = await readFile(join(packagePath, 'package.json'), 'utf-8');
    const pkg = JSON.parse(content);

    result.hasDescription = Boolean(pkg.description && pkg.description.length > 10);
    result.hasKeywords = Boolean(pkg.keywords && pkg.keywords.length > 0);
    result.hasRepository = Boolean(pkg.repository);
    result.hasHomepage = Boolean(pkg.homepage);
    result.hasBugs = Boolean(pkg.bugs);
    result.hasAuthor = Boolean(pkg.author);
    result.hasTypes = Boolean(pkg.types || pkg.typings);
    result.hasMain = Boolean(pkg.main || pkg.module);
    result.hasExports = Boolean(pkg.exports);
  } catch {
    // Package.json not readable
  }

  return result;
}

/**
 * Analyze inline documentation quality
 *
 * @param {string} packagePath - Package path
 * @returns {Promise<Object>} Inline documentation analysis
 */
async function analyzeInlineDocs(packagePath) {
  const result = {
    filesWithComments: 0,
    totalFiles: 0,
    averageCommentRatio: 0,
    filesWithTypeHints: 0
  };

  let totalCommentRatio = 0;

  async function scanFile(filePath) {
    try {
      const content = await readFile(filePath, 'utf-8');
      const lines = content.split('\n');
      const totalLines = lines.length;

      if (totalLines === 0) return;

      // Count comment lines
      let commentLines = 0;
      let inBlockComment = false;

      for (const line of lines) {
        const trimmed = line.trim();

        if (inBlockComment) {
          commentLines++;
          if (trimmed.includes('*/')) {
            inBlockComment = false;
          }
        } else if (trimmed.startsWith('//')) {
          commentLines++;
        } else if (trimmed.startsWith('/*')) {
          commentLines++;
          if (!trimmed.includes('*/')) {
            inBlockComment = true;
          }
        }
      }

      const ratio = commentLines / totalLines;
      totalCommentRatio += ratio;

      if (ratio > 0.05) {
        result.filesWithComments++;
      }

      // Check for type hints (JSDoc @type, @param, @returns)
      if (/@(?:type|param|returns?|typedef)\s/g.test(content)) {
        result.filesWithTypeHints++;
      }

      result.totalFiles++;
    } catch {
      // File not readable
    }
  }

  async function scanDir(dir) {
    try {
      const entries = await readdir(dir, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(dir, entry.name);

        if (entry.isDirectory()) {
          if (!['node_modules', 'dist', 'build', 'coverage', '.git'].includes(entry.name)) {
            await scanDir(fullPath);
          }
        } else if (entry.isFile()) {
          if (/\.(mjs|js|ts)$/.test(entry.name)) {
            await scanFile(fullPath);
          }
        }
      }
    } catch {
      // Directory not accessible
    }
  }

  await scanDir(packagePath);

  result.averageCommentRatio = result.totalFiles > 0
    ? Math.round((totalCommentRatio / result.totalFiles) * 100)
    : 0;

  return result;
}

/**
 * Perform documentation check on a package
 *
 * @param {string} packagePath - Path to package
 * @param {Object} options - Check options
 * @returns {Promise<Object>} Check result
 */
export async function documentationCheck(packagePath, options = {}) {
  const startTime = Date.now();
  const warnings = [];
  const failures = [];
  const remediation = [];

  let totalScore = 0;
  const details = {
    hasReadme: false,
    readmeQuality: 'none',
    readmeLength: 0,
    jsdocCoverage: 0,
    hasExamples: false,
    exampleCount: 0,
    hasChangelog: false,
    hasLicense: false,
    packageJsonComplete: false
  };

  try {
    // README analysis
    const readmeResult = await analyzeReadme(packagePath);
    details.hasReadme = readmeResult.exists;
    details.readmeQuality = readmeResult.quality;
    details.readmeLength = readmeResult.length;

    if (!readmeResult.exists) {
      failures.push('No README.md found');
      remediation.push('Create README.md with installation and usage instructions');
    } else {
      if (readmeResult.quality === 'minimal') {
        warnings.push('README.md is minimal (< 500 characters)');
        remediation.push('Expand README with more details and examples');
      }

      if (readmeResult.missingSections.length > 0) {
        warnings.push(`README missing sections: ${readmeResult.missingSections.join(', ')}`);
        remediation.push(`Add ${readmeResult.missingSections[0]} section to README`);
      }

      if (!readmeResult.hasCodeBlocks) {
        warnings.push('README has no code examples');
        remediation.push('Add code examples with ```javascript blocks');
      }
    }

    // Documentation files
    const docFiles = await checkDocumentationFiles(packagePath);
    details.hasChangelog = docFiles.hasChangelog;
    details.hasLicense = docFiles.hasLicense;

    if (!docFiles.hasLicense) {
      warnings.push('No LICENSE file found');
      remediation.push('Add LICENSE file (MIT recommended)');
    }

    if (!docFiles.hasChangelog) {
      warnings.push('No CHANGELOG.md found');
    }

    // JSDoc coverage
    const jsdocResult = await analyzeJsdocCoverage(packagePath);
    details.jsdocCoverage = jsdocResult.coverage;

    if (jsdocResult.coverage < DOCUMENTATION_THRESHOLDS.minApiCoverage) {
      failures.push(`JSDoc coverage ${jsdocResult.coverage}% < ${DOCUMENTATION_THRESHOLDS.minApiCoverage}%`);
      remediation.push('Add JSDoc comments to exported functions and classes');
    } else if (jsdocResult.coverage < 100) {
      warnings.push(`JSDoc coverage ${jsdocResult.coverage}% (target: 100%)`);
    }

    // Examples
    const examplesResult = await checkExamples(packagePath);
    details.hasExamples = examplesResult.hasExamplesFolder || examplesResult.readmeExamples >= 3;
    details.exampleCount = examplesResult.exampleCount + examplesResult.readmeExamples;

    if (details.exampleCount < DOCUMENTATION_THRESHOLDS.minExampleCount) {
      warnings.push(`Only ${details.exampleCount} code example(s) found`);
      remediation.push('Add more code examples to README or examples/ folder');
    }

    // Package.json completeness
    const pkgDocs = await checkPackageJsonDocs(packagePath);
    const pkgCompleteness = Object.values(pkgDocs).filter(Boolean).length / Object.keys(pkgDocs).length;
    details.packageJsonComplete = pkgCompleteness >= 0.7;

    if (!pkgDocs.hasDescription) {
      warnings.push('package.json missing description');
    }

    if (!pkgDocs.hasKeywords) {
      warnings.push('package.json missing keywords');
    }

    // Inline documentation
    const inlineDocs = await analyzeInlineDocs(packagePath);

    // Calculate score
    // README (30 points)
    let readmeScore = 0;
    if (readmeResult.exists) {
      switch (readmeResult.quality) {
        case 'excellent': readmeScore = 30; break;
        case 'good': readmeScore = 25; break;
        case 'adequate': readmeScore = 20; break;
        case 'minimal': readmeScore = 10; break;
        default: readmeScore = 0;
      }
    }

    // JSDoc coverage (30 points)
    const jsdocScore = Math.round((jsdocResult.coverage / 100) * 30);

    // Examples (15 points)
    const exampleScore = Math.min(15, (details.exampleCount / DOCUMENTATION_THRESHOLDS.minExampleCount) * 15);

    // Supporting docs (15 points)
    let supportScore = 0;
    if (docFiles.hasLicense) supportScore += 5;
    if (docFiles.hasChangelog) supportScore += 5;
    if (docFiles.hasContributing) supportScore += 3;
    if (docFiles.hasDocsFolder) supportScore += 2;

    // Package.json (10 points)
    const pkgScore = Math.round(pkgCompleteness * 10);

    totalScore = Math.round(readmeScore + jsdocScore + exampleScore + supportScore + pkgScore);

  } catch (error) {
    failures.push(`Documentation check failed: ${error.message}`);
    totalScore = 0;
  }

  return {
    passed: totalScore >= 80,
    score: totalScore,
    status: totalScore >= 95 ? 'pass' : totalScore >= 80 ? 'warn' : 'fail',
    warnings: [...new Set(warnings)].slice(0, 20),
    failures: [...new Set(failures)].slice(0, 10),
    remediation: [...new Set(remediation)].slice(0, 10),
    duration: Date.now() - startTime,
    details
  };
}

export default documentationCheck;
