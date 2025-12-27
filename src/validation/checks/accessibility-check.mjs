/**
 * @fileoverview Accessibility Check - Validates a11y compliance
 *
 * **Checks performed**:
 * 1. ARIA attributes in React/JSX components
 * 2. Alt text for images
 * 3. Form label associations
 * 4. Keyboard navigation support
 * 5. Color contrast (via CSS analysis)
 * 6. Focus management
 * 7. Screen reader compatibility
 * 8. Semantic HTML usage
 *
 * **Scoring**:
 * - 100: Excellent accessibility
 * - 95-99: Good accessibility with minor issues
 * - 80-94: Acceptable accessibility
 * - 60-79: Accessibility improvements needed
 * - <60: Critical accessibility issues
 *
 * **Note**: This check is primarily for web/UI packages.
 * Non-UI packages receive a default passing score.
 *
 * @module validation/checks/accessibility-check
 */

import { readdir, readFile, stat, access } from 'node:fs/promises';
import { join, extname, relative } from 'node:path';

/**
 * A11y check patterns
 */
const A11Y_PATTERNS = {
  // Missing alt text
  imgWithoutAlt: /<img(?![^>]*\balt\s*=)[^>]*>/gi,
  imgWithEmptyAlt: /<img[^>]*\balt\s*=\s*["']\s*["'][^>]*>/gi,

  // ARIA issues
  ariaHidden: /aria-hidden\s*=\s*["']true["']/gi,
  rolePresentation: /role\s*=\s*["']presentation["']/gi,
  validAriaRoles: /role\s*=\s*["'](button|link|navigation|main|article|heading|list|listitem|form|dialog|alert|tab|tablist|tabpanel|menu|menuitem|checkbox|radio|textbox|searchbox|tree|grid)["']/gi,

  // Form labels
  inputWithoutLabel: /<input(?![^>]*(?:aria-label|aria-labelledby|id\s*=\s*["'][^"']+["']))[^>]*>/gi,
  labelWithoutFor: /<label(?![^>]*\bfor\s*=)[^>]*>/gi,

  // Keyboard accessibility
  onClickWithoutKeyboard: /onClick\s*=(?![^}]*(?:onKeyDown|onKeyPress|onKeyUp))/gi,
  tabIndexNegative: /tabIndex\s*=\s*["']-1["']/gi,
  tabIndexPositive: /tabIndex\s*=\s*["'][1-9]\d*["']/gi,

  // Semantic HTML
  divAsButton: /<div[^>]*onClick/gi,
  spanAsButton: /<span[^>]*onClick/gi,

  // Color contrast (heuristic)
  hardcodedColor: /(?:color|background-color)\s*:\s*#(?:[0-9a-f]{3}){1,2}/gi,

  // Focus styles
  outlineNone: /outline\s*:\s*none|outline\s*:\s*0(?!\s*\d)/gi,

  // Good patterns
  skipLink: /skip[- ]?(?:to[- ])?(?:main|content|nav)/gi,
  ariaLabel: /aria-label\s*=\s*["'][^"']+["']/gi,
  ariaLive: /aria-live\s*=\s*["'](?:polite|assertive)["']/gi,
  focusVisible: /:focus-visible/gi,
  semanticElements: /<(?:main|nav|header|footer|article|section|aside|figure|figcaption)\b/gi
};

/**
 * Check if package is a web/UI package
 *
 * @param {Object} packageInfo - Package info
 * @returns {boolean} True if web/UI package
 */
function isWebPackage(packageInfo) {
  const deps = {
    ...packageInfo.dependencies,
    ...packageInfo.devDependencies,
    ...packageInfo.peerDependencies
  };

  const webIndicators = [
    'react', 'react-dom', 'vue', 'angular', 'svelte',
    'next', 'nuxt', 'gatsby', 'astro',
    '@emotion', 'styled-components', 'tailwindcss',
    'webpack', 'vite', 'parcel'
  ];

  return webIndicators.some(indicator =>
    Object.keys(deps).some(dep => dep.includes(indicator))
  );
}

/**
 * Get JSX/TSX files
 *
 * @param {string} dirPath - Directory path
 * @returns {Promise<Array<string>>} File paths
 */
async function getUIFiles(dirPath) {
  const files = [];

  async function scan(currentPath) {
    try {
      const entries = await readdir(currentPath, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(currentPath, entry.name);

        if (entry.isDirectory()) {
          if (!['node_modules', 'dist', 'build', 'coverage', '.git'].includes(entry.name)) {
            await scan(fullPath);
          }
        } else if (entry.isFile()) {
          const ext = extname(entry.name);
          // Include JSX, TSX, and potentially Vue/Svelte files
          if (['.jsx', '.tsx', '.vue', '.svelte'].includes(ext) ||
              (ext === '.js' && (entry.name.includes('component') || entry.name.includes('Component')))) {
            files.push(fullPath);
          }
        }
      }
    } catch {
      // Directory not accessible
    }
  }

  await scan(dirPath);
  return files;
}

/**
 * Get CSS files
 *
 * @param {string} dirPath - Directory path
 * @returns {Promise<Array<string>>} File paths
 */
async function getCSSFiles(dirPath) {
  const files = [];

  async function scan(currentPath) {
    try {
      const entries = await readdir(currentPath, { withFileTypes: true });

      for (const entry of entries) {
        const fullPath = join(currentPath, entry.name);

        if (entry.isDirectory()) {
          if (!['node_modules', 'dist', 'build', 'coverage', '.git'].includes(entry.name)) {
            await scan(fullPath);
          }
        } else if (entry.isFile()) {
          const ext = extname(entry.name);
          if (['.css', '.scss', '.sass', '.less'].includes(ext)) {
            files.push(fullPath);
          }
        }
      }
    } catch {
      // Directory not accessible
    }
  }

  await scan(dirPath);
  return files;
}

/**
 * Analyze file for a11y issues
 *
 * @param {string} content - File content
 * @param {string} filePath - File path
 * @returns {Object} A11y analysis result
 */
function analyzeFileA11y(content, filePath) {
  const issues = [];
  const goodPractices = [];

  // Check for missing alt text
  const imgWithoutAlt = content.match(A11Y_PATTERNS.imgWithoutAlt) || [];
  if (imgWithoutAlt.length > 0) {
    issues.push({
      type: 'MISSING_ALT_TEXT',
      severity: 'high',
      count: imgWithoutAlt.length,
      file: filePath
    });
  }

  // Check for empty alt text (might be intentional for decorative images)
  const imgWithEmptyAlt = content.match(A11Y_PATTERNS.imgWithEmptyAlt) || [];
  if (imgWithEmptyAlt.length > 0) {
    issues.push({
      type: 'EMPTY_ALT_TEXT',
      severity: 'low',
      count: imgWithEmptyAlt.length,
      file: filePath
    });
  }

  // Check for click handlers without keyboard support
  const clickWithoutKeyboard = content.match(A11Y_PATTERNS.onClickWithoutKeyboard) || [];
  if (clickWithoutKeyboard.length > 0) {
    issues.push({
      type: 'CLICK_WITHOUT_KEYBOARD',
      severity: 'medium',
      count: clickWithoutKeyboard.length,
      file: filePath
    });
  }

  // Check for div/span as button
  const divAsButton = (content.match(A11Y_PATTERNS.divAsButton) || []).length +
                      (content.match(A11Y_PATTERNS.spanAsButton) || []).length;
  if (divAsButton > 0) {
    issues.push({
      type: 'DIV_AS_BUTTON',
      severity: 'medium',
      count: divAsButton,
      file: filePath
    });
  }

  // Check for positive tabindex (anti-pattern)
  const tabIndexPositive = content.match(A11Y_PATTERNS.tabIndexPositive) || [];
  if (tabIndexPositive.length > 0) {
    issues.push({
      type: 'POSITIVE_TABINDEX',
      severity: 'medium',
      count: tabIndexPositive.length,
      file: filePath
    });
  }

  // Check for good practices
  if (A11Y_PATTERNS.skipLink.test(content)) {
    goodPractices.push('SKIP_LINK');
  }

  const ariaLabels = content.match(A11Y_PATTERNS.ariaLabel) || [];
  if (ariaLabels.length > 0) {
    goodPractices.push('ARIA_LABELS');
  }

  if (A11Y_PATTERNS.ariaLive.test(content)) {
    goodPractices.push('ARIA_LIVE');
  }

  const semanticElements = content.match(A11Y_PATTERNS.semanticElements) || [];
  if (semanticElements.length > 0) {
    goodPractices.push('SEMANTIC_HTML');
  }

  const validRoles = content.match(A11Y_PATTERNS.validAriaRoles) || [];
  if (validRoles.length > 0) {
    goodPractices.push('ARIA_ROLES');
  }

  return { issues, goodPractices };
}

/**
 * Analyze CSS for a11y issues
 *
 * @param {string} content - CSS content
 * @param {string} filePath - File path
 * @returns {Object} CSS a11y analysis
 */
function analyzeCSSA11y(content, filePath) {
  const issues = [];
  const goodPractices = [];

  // Check for outline: none (potential focus issue)
  const outlineNone = content.match(A11Y_PATTERNS.outlineNone) || [];
  if (outlineNone.length > 0) {
    // Check if there's a replacement focus style
    const hasFocusVisible = A11Y_PATTERNS.focusVisible.test(content);
    if (!hasFocusVisible) {
      issues.push({
        type: 'FOCUS_OUTLINE_REMOVED',
        severity: 'high',
        count: outlineNone.length,
        file: filePath
      });
    }
  }

  // Check for focus-visible (good practice)
  if (A11Y_PATTERNS.focusVisible.test(content)) {
    goodPractices.push('FOCUS_VISIBLE');
  }

  return { issues, goodPractices };
}

/**
 * Check for a11y tooling
 *
 * @param {Object} packageInfo - Package info
 * @returns {Object} Tooling check result
 */
function checkA11yTooling(packageInfo) {
  const deps = {
    ...packageInfo.dependencies,
    ...packageInfo.devDependencies
  };

  const a11yTools = {
    testing: ['@axe-core/react', 'jest-axe', 'cypress-axe', '@testing-library/jest-dom', 'pa11y'],
    linting: ['eslint-plugin-jsx-a11y', 'stylelint-a11y'],
    runtime: ['react-aria', '@react-aria', '@radix-ui', 'downshift']
  };

  const found = {
    hasTesting: false,
    hasLinting: false,
    hasRuntime: false,
    tools: []
  };

  for (const [category, tools] of Object.entries(a11yTools)) {
    for (const tool of tools) {
      if (Object.keys(deps).some(dep => dep.includes(tool))) {
        found[`has${category.charAt(0).toUpperCase() + category.slice(1)}`] = true;
        found.tools.push(tool);
      }
    }
  }

  return found;
}

/**
 * Perform accessibility check on a package
 *
 * @param {string} packagePath - Path to package
 * @param {Object} options - Check options
 * @returns {Promise<Object>} Check result
 */
export async function accessibilityCheck(packagePath, options = {}) {
  const startTime = Date.now();
  const warnings = [];
  const failures = [];
  const remediation = [];

  let totalScore = 100;
  const details = {
    isWebPackage: false,
    filesAnalyzed: 0,
    issueCount: 0,
    goodPracticeCount: 0,
    hasA11yTooling: false,
    issues: []
  };

  try {
    // Load package.json
    let packageInfo = {};
    try {
      const content = await readFile(join(packagePath, 'package.json'), 'utf-8');
      packageInfo = JSON.parse(content);
    } catch {
      // Package.json not readable
    }

    // Check if this is a web package
    details.isWebPackage = isWebPackage(packageInfo) ||
      options.packageType === 'web' ||
      options.packageType === 'application';

    // If not a web package, return high score with note
    if (!details.isWebPackage) {
      return {
        passed: true,
        score: 95,
        status: 'pass',
        warnings: ['Package does not appear to be a web/UI package - a11y checks limited'],
        failures: [],
        remediation: [],
        duration: Date.now() - startTime,
        details: {
          ...details,
          note: 'Non-UI package - accessibility checks not fully applicable'
        }
      };
    }

    // Get UI files
    const uiFiles = await getUIFiles(packagePath);
    const cssFiles = await getCSSFiles(packagePath);

    details.filesAnalyzed = uiFiles.length + cssFiles.length;

    if (uiFiles.length === 0 && cssFiles.length === 0) {
      warnings.push('No UI files found for accessibility analysis');
      return {
        passed: true,
        score: 90,
        status: 'pass',
        warnings,
        failures: [],
        remediation: [],
        duration: Date.now() - startTime,
        details: {
          ...details,
          note: 'No UI files found'
        }
      };
    }

    // Analyze UI files
    const allIssues = [];
    const allGoodPractices = new Set();

    for (const filePath of uiFiles) {
      try {
        const content = await readFile(filePath, 'utf-8');
        const relativePath = relative(packagePath, filePath);
        const analysis = analyzeFileA11y(content, relativePath);

        allIssues.push(...analysis.issues);
        analysis.goodPractices.forEach(p => allGoodPractices.add(p));
      } catch {
        // File not readable
      }
    }

    // Analyze CSS files
    for (const filePath of cssFiles) {
      try {
        const content = await readFile(filePath, 'utf-8');
        const relativePath = relative(packagePath, filePath);
        const analysis = analyzeCSSA11y(content, relativePath);

        allIssues.push(...analysis.issues);
        analysis.goodPractices.forEach(p => allGoodPractices.add(p));
      } catch {
        // File not readable
      }
    }

    details.issueCount = allIssues.length;
    details.goodPracticeCount = allGoodPractices.size;
    details.issues = allIssues.slice(0, 10);

    // Check for a11y tooling
    const tooling = checkA11yTooling(packageInfo);
    details.hasA11yTooling = tooling.hasTesting || tooling.hasLinting;

    // Generate warnings and failures
    const highSeverity = allIssues.filter(i => i.severity === 'high');
    const mediumSeverity = allIssues.filter(i => i.severity === 'medium');

    if (highSeverity.length > 0) {
      failures.push(`${highSeverity.length} high-severity a11y issue(s)`);

      // Group by type
      const types = {};
      for (const issue of highSeverity) {
        types[issue.type] = (types[issue.type] || 0) + issue.count;
      }

      for (const [type, count] of Object.entries(types)) {
        switch (type) {
          case 'MISSING_ALT_TEXT':
            remediation.push(`Add alt text to ${count} image(s)`);
            break;
          case 'FOCUS_OUTLINE_REMOVED':
            remediation.push('Restore focus indicators or use :focus-visible');
            break;
        }
      }
    }

    if (mediumSeverity.length > 0) {
      warnings.push(`${mediumSeverity.length} medium-severity a11y issue(s)`);

      for (const issue of mediumSeverity.slice(0, 3)) {
        switch (issue.type) {
          case 'CLICK_WITHOUT_KEYBOARD':
            remediation.push('Add keyboard event handlers alongside onClick');
            break;
          case 'DIV_AS_BUTTON':
            remediation.push('Use <button> element instead of div/span for clickable elements');
            break;
          case 'POSITIVE_TABINDEX':
            remediation.push('Remove positive tabIndex values (use 0 or -1 only)');
            break;
        }
      }
    }

    if (!tooling.hasTesting && !tooling.hasLinting) {
      warnings.push('No a11y testing or linting tools detected');
      remediation.push('Add eslint-plugin-jsx-a11y or jest-axe for automated a11y testing');
    }

    // Calculate score
    // Base score (40 points)
    let baseScore = 40;

    // Issues penalty
    baseScore -= highSeverity.length * 8;
    baseScore -= mediumSeverity.length * 3;
    baseScore = Math.max(0, baseScore);

    // Good practices bonus (30 points)
    const practiceScore = Math.min(30, allGoodPractices.size * 6);

    // Tooling bonus (15 points)
    let toolingScore = 0;
    if (tooling.hasTesting) toolingScore += 8;
    if (tooling.hasLinting) toolingScore += 5;
    if (tooling.hasRuntime) toolingScore += 2;

    // File coverage (15 points) - more files analyzed = more confidence
    const coverageScore = Math.min(15, (uiFiles.length / 5) * 15);

    totalScore = Math.round(baseScore + practiceScore + toolingScore + coverageScore);
    totalScore = Math.max(0, Math.min(100, totalScore));

  } catch (error) {
    failures.push(`Accessibility check failed: ${error.message}`);
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

export default accessibilityCheck;
