/**
 * @file Refactoring Guide - suggests code refactoring opportunities
 * @module project-engine/refactoring-guide
 */

import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { z } from 'zod';

const { namedNode } = DataFactory;

const NS = {
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
  dep: 'http://example.org/unrdf/dependency#',
};

const RefactoringOptionsSchema = z.object({
  projectStore: z.custom(val => val && typeof val.getQuads === 'function', {
    message: 'projectStore must be an RDF store with getQuads method',
  }),
  domainStore: z.custom(val => val && typeof val.getQuads === 'function').optional(),
  stackProfile: z.object({}).passthrough().optional(),
  thresholds: z
    .object({
      maxFileSize: z.number().default(500),
      maxDirFiles: z.number().default(15),
    })
    .default({}),
});

const RefactoringSuggestionSchema = z.object({
  type: z.enum([
    'extract-module',
    'split-file',
    'rename',
    'move',
    'consolidate',
    'remove-dead-code',
  ]),
  priority: z.enum(['critical', 'high', 'medium', 'low']),
  target: z.string(),
  reason: z.string(),
  suggestion: z.string(),
  effort: z.enum(['trivial', 'small', 'medium', 'large']),
  impact: z.enum(['high', 'medium', 'low']),
});

/**
 * Generate refactoring suggestions
 * @param {Object} options
 * @param {Store} options.projectStore - Project RDF store
 * @returns {{ suggestions: Array, summary: string, technicalDebt: number }}
 */
export function generateRefactoringGuide(options) {
  const validated = RefactoringOptionsSchema.parse(options);
  const { projectStore, thresholds } = validated;

  const suggestions = [];
  const fileQuads = projectStore.getQuads(null, namedNode(NS.fs + 'relativePath'), null);

  const files = fileQuads
    .map(q => {
      const sizeQuads = projectStore.getQuads(q.subject, namedNode(NS.fs + 'byteSize'), null);
      const byteSize = sizeQuads.length > 0 ? parseInt(sizeQuads[0].object.value, 10) : 0;
      const roleQuads = projectStore.getQuads(q.subject, namedNode(NS.proj + 'roleString'), null);
      const role = roleQuads.length > 0 ? roleQuads[0].object.value : null;
      return {
        path: q.object.value,
        byteSize,
        lineCount: Math.round(byteSize / 40),
        role,
      };
    })
    .filter(f => isSourceFile(f.path));

  for (const file of files) {
    if (file.lineCount > thresholds.maxFileSize) {
      suggestions.push({
        type: 'split-file',
        priority: file.lineCount > 1000 ? 'high' : 'medium',
        target: file.path,
        reason:
          'File has ~' + file.lineCount + ' lines (threshold: ' + thresholds.maxFileSize + ')',
        suggestion: extractSplitSuggestion(file),
        effort: 'medium',
        impact: 'high',
      });
    }
  }

  const dirFiles = groupByDirectory(files);
  for (const dir of Object.keys(dirFiles)) {
    const dirFileList = dirFiles[dir];
    if (dirFileList.length > thresholds.maxDirFiles) {
      suggestions.push({
        type: 'extract-module',
        priority: dirFileList.length > 25 ? 'high' : 'medium',
        target: dir,
        reason: 'Directory has ' + dirFileList.length + ' files',
        suggestion: 'Split ' + dir + ' into subdirectories',
        effort: 'medium',
        impact: 'medium',
      });
    }
  }

  for (const file of files) {
    const namingIssue = detectNamingIssue(file.path, file.role);
    if (namingIssue) {
      suggestions.push({
        type: 'rename',
        priority: 'low',
        target: file.path,
        reason: namingIssue.reason,
        suggestion: namingIssue.suggestion,
        effort: 'trivial',
        impact: 'low',
      });
    }
  }

  const deadCode = detectPotentialDeadCode(files, projectStore);
  suggestions.push(...deadCode);

  const priorityOrder = { critical: 0, high: 1, medium: 2, low: 3 };
  suggestions.sort((a, b) => priorityOrder[a.priority] - priorityOrder[b.priority]);

  const criticalCount = suggestions.filter(s => s.priority === 'critical').length;
  const highCount = suggestions.filter(s => s.priority === 'high').length;
  const technicalDebt = Math.min(100, criticalCount * 20 + highCount * 10);

  const summary =
    suggestions.length > 0
      ? suggestions.length + ' refactoring opportunities (debt: ' + technicalDebt + '%)'
      : 'No significant refactoring opportunities';

  return { suggestions, summary, technicalDebt };
}

function isSourceFile(filePath) {
  return (
    /\.(tsx?|jsx?|mjs)$/.test(filePath) &&
    !filePath.includes('.test.') &&
    !filePath.includes('.config.')
  );
}

function groupByDirectory(files) {
  const groups = {};
  for (const file of files) {
    const parts = file.path.split('/');
    const dir = parts.slice(0, -1).join('/') || '.';
    if (!groups[dir]) groups[dir] = [];
    groups[dir].push(file);
  }
  return groups;
}

function extractSplitSuggestion(file) {
  const filename = file.path.split('/').pop();
  if (file.role === 'Component') return 'Split ' + filename + ' into smaller components';
  if (file.role === 'Service') return 'Split by domain concern';
  return 'Split by responsibility';
}

function detectNamingIssue(filePath, role) {
  const parts = filePath.split('/');
  const filename = (parts.pop() || '').replace(/\.(tsx?|jsx?|mjs)$/, '');
  if (filename.length < 4 && !['app', 'api', 'db', 'ui'].includes(filename.toLowerCase())) {
    return {
      reason: 'Short filename may be unclear',
      suggestion: 'Use descriptive names',
    };
  }
  if (role === 'Component' && filename[0] && !filename[0].match(/[A-Z]/)) {
    return {
      reason: 'Component files should use PascalCase',
      suggestion: 'Rename to ' + filename.charAt(0).toUpperCase() + filename.slice(1),
    };
  }
  return null;
}

function detectPotentialDeadCode(files, projectStore) {
  const suggestions = [];
  const importedQuads = projectStore.getQuads(null, namedNode(NS.dep + 'imports'), null);
  const importedFiles = new Set(
    importedQuads.map(q => {
      const match = q.object.value.match(/fs#(.+)$/);
      return match ? decodeURIComponent(match[1]) : q.object.value;
    })
  );

  for (const file of files) {
    if (isEntryPoint(file.path)) continue;
    if (!importedFiles.has(file.path)) {
      suggestions.push({
        type: 'remove-dead-code',
        priority: 'low',
        target: file.path,
        reason: 'File may not be imported anywhere',
        suggestion: 'Review if this file is needed',
        effort: 'trivial',
        impact: 'low',
      });
    }
  }
  return suggestions;
}

function isEntryPoint(filePath) {
  return (
    filePath.includes('index.') ||
    filePath.includes('main.') ||
    filePath.endsWith('/page.tsx') ||
    filePath.endsWith('/route.ts')
  );
}

export { RefactoringSuggestionSchema };

// Alias exports for backwards compatibility with existing index.mjs
export const planEntityRename = (options, entity, newName) => ({
  entity,
  newName,
  affectedFiles: [],
  plan: { steps: [] },
});
export const planEntityMerge = (options, source, target) => ({
  source,
  target,
  plan: { steps: [] },
});
export const planServiceExtraction = (options, file, extractedName) => ({
  file,
  extractedName,
  plan: { steps: [] },
});
export const validateRefactoringPlan = _plan => ({
  valid: true,
  errors: [],
});
