/**
 * @file Stack Linter - validates stack consistency and best practices
 * @module project-engine/stack-linter
 */

import { DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { z } from 'zod';

const { namedNode } = DataFactory;

const NS = { fs: 'http://example.org/unrdf/filesystem#' };

const StackLintOptionsSchema = z.object({
  projectStore: z.custom(val => val && typeof val.getQuads === 'function', {
    message: 'projectStore must be an RDF store with getQuads method',
  }),
  stackProfile: z
    .object({
      webFramework: z.string().nullable().optional(),
      hasTypescript: z.boolean().optional(),
    })
    .passthrough()
    .optional(),
  projectRoot: z.string().optional(),
});

const StackLintIssueSchema = z.object({
  rule: z.string(),
  category: z.enum(['structure', 'naming', 'dependencies', 'config', 'security']),
  severity: z.enum(['error', 'warning', 'info']),
  files: z.array(z.string()),
  message: z.string(),
  fix: z.string().optional(),
});

/**
 * Lint project for stack-specific best practices
 * @param {Object} options
 * @param {Store} options.projectStore - Project RDF store
 * @param {Object} [options.stackProfile] - Stack profile
 * @returns {{ issues: Array, summary: string, score: number }}
 */
export function lintStack(options) {
  const validated = StackLintOptionsSchema.parse(options);
  const { projectStore, stackProfile } = validated;

  const issues = [];
  const fileQuads = projectStore.getQuads(null, namedNode(`${NS.fs}relativePath`), null);
  const allFiles = fileQuads.map(q => q.object.value);

  // Framework-specific rules
  if (stackProfile?.webFramework?.includes('next')) {
    issues.push(...lintNextJs(allFiles));
  }

  // Universal rules
  issues.push(...lintStructure(allFiles));
  issues.push(...lintDependencies(allFiles));
  issues.push(...lintConfig(allFiles, stackProfile));
  issues.push(...lintSecurity(allFiles));

  const severityOrder = { error: 0, warning: 1, info: 2 };
  issues.sort((a, b) => severityOrder[a.severity] - severityOrder[b.severity]);

  const errorCount = issues.filter(i => i.severity === 'error').length;
  const warningCount = issues.filter(i => i.severity === 'warning').length;
  const score = Math.max(0, 100 - errorCount * 10 - warningCount * 3);

  const summary =
    issues.length > 0
      ? `${issues.length} lint issues (${errorCount} errors, ${warningCount} warnings)`
      : 'No lint issues found';

  return { issues, summary, score };
}

function lintNextJs(allFiles) {
  const issues = [];
  const hasAppDir = allFiles.some(f => f.startsWith('app/') || f.includes('/app/'));
  const hasPagesDir = allFiles.some(f => f.startsWith('pages/') || f.includes('/pages/'));

  if (hasAppDir && hasPagesDir) {
    issues.push({
      rule: 'next/no-mixed-routers',
      category: 'structure',
      severity: 'warning',
      files: ['app/', 'pages/'],
      message: 'Both app/ and pages/ directories exist',
      fix: 'Migrate to app router or keep consistent',
    });
  }

  if (hasAppDir) {
    const hasLayout = allFiles.some(f => f.includes('layout.'));
    if (!hasLayout) {
      issues.push({
        rule: 'next/require-layout',
        category: 'structure',
        severity: 'error',
        files: ['app/'],
        message: 'App router requires root layout.tsx',
        fix: 'Create app/layout.tsx',
      });
    }
  }
  return issues;
}

function lintStructure(allFiles) {
  const issues = [];
  const hasSrcDir = allFiles.some(f => f.startsWith('src/'));
  const hasRootSource = allFiles.some(
    f =>
      !f.startsWith('src/') &&
      !f.startsWith('test/') &&
      !f.startsWith('docs/') &&
      /\.(tsx?|jsx?|mjs)$/.test(f) &&
      !f.includes('.config.')
  );

  if (!hasSrcDir && hasRootSource) {
    issues.push({
      rule: 'structure/use-src-dir',
      category: 'structure',
      severity: 'warning',
      files: ['./'],
      message: 'Source files should be in src/',
      fix: 'Move source files to src/',
    });
  }
  return issues;
}

function lintDependencies(allFiles) {
  const issues = [];
  const lockfiles = ['package-lock.json', 'yarn.lock', 'pnpm-lock.yaml'].filter(f =>
    allFiles.includes(f)
  );
  if (lockfiles.length > 1) {
    issues.push({
      rule: 'deps/single-lockfile',
      category: 'dependencies',
      severity: 'error',
      files: lockfiles,
      message: 'Multiple lockfiles detected',
      fix: 'Remove extra lockfiles',
    });
  }
  return issues;
}

function lintConfig(allFiles, stackProfile) {
  const issues = [];
  const hasEslint = allFiles.some(f => f.includes('.eslintrc') || f.includes('eslint.config'));
  if (!hasEslint) {
    issues.push({
      rule: 'config/require-eslint',
      category: 'config',
      severity: 'warning',
      files: ['./'],
      message: 'No ESLint configuration found',
      fix: 'Add .eslintrc.json',
    });
  }

  if (stackProfile?.hasTypescript) {
    const hasTsConfig = allFiles.some(f => f.includes('tsconfig'));
    if (!hasTsConfig) {
      issues.push({
        rule: 'config/require-tsconfig',
        category: 'config',
        severity: 'error',
        files: ['./'],
        message: 'TypeScript project missing tsconfig.json',
        fix: 'Create tsconfig.json',
      });
    }
  }
  return issues;
}

function lintSecurity(allFiles) {
  const issues = [];
  const envFiles = allFiles.filter(f => f.includes('.env') && !f.includes('.env.example'));
  if (envFiles.length > 0) {
    issues.push({
      rule: 'security/no-env-files',
      category: 'security',
      severity: 'error',
      files: envFiles,
      message: 'Environment files should not be committed',
      fix: 'Add .env* to .gitignore',
    });
  }
  return issues;
}

export { StackLintIssueSchema };

// Alias exports for backwards compatibility with existing index.mjs
export const deriveLinterRules = lintStack;
export const analyzeCodePatterns = options => {
  const result = lintStack(options);
  return result.issues.map(i => ({ pattern: i.rule, category: i.category }));
};
export const generateESLintConfig = options => {
  const result = lintStack(options);
  return {
    rules: Object.fromEntries(
      result.issues.map(i => [i.rule, i.severity === 'error' ? 'error' : 'warn'])
    ),
  };
};
