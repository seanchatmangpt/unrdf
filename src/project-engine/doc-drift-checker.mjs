/**
 * @file Doc Drift Checker - detects documentation out of sync with code
 * @module project-engine/doc-drift-checker
 */

import { DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { z } from 'zod';

const { namedNode } = DataFactory;

const NS = {
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
};

const DocDriftOptionsSchema = z.object({
  projectStore: z.custom(val => val && typeof val.getQuads === 'function', {
    message: 'projectStore must be an RDF store with getQuads method',
  }),
  projectRoot: z.string().optional(),
});

const DriftEntrySchema = z.object({
  docFile: z.string(),
  sourceFile: z.string().optional(),
  driftType: z.enum(['outdated', 'missing', 'orphaned', 'incomplete']),
  severity: z.enum(['critical', 'high', 'medium', 'low']),
  reason: z.string(),
  suggestion: z.string(),
});

/**
 * Check for documentation drift
 * @param {Object} options
 * @param {Store} options.projectStore - Project RDF store
 * @returns {{ drifts: Array, summary: string, healthScore: number }}
 */
export function checkDocDrift(options) {
  const validated = DocDriftOptionsSchema.parse(options);
  const { projectStore } = validated;

  const drifts = [];
  const fileQuads = projectStore.getQuads(null, namedNode(NS.fs + 'relativePath'), null);
  const allFiles = fileQuads.map(q => ({ path: q.object.value }));

  const docFiles = allFiles.filter(f => isDocFile(f.path));
  const sourceFiles = allFiles.filter(f => isSourceFile(f.path));

  for (const source of sourceFiles) {
    if (needsDocumentation(source.path)) {
      const hasDoc = docFiles.some(d => docMatchesSource(d.path, source.path));
      if (!hasDoc) {
        drifts.push({
          sourceFile: source.path,
          docFile: suggestDocPath(source.path),
          driftType: 'missing',
          severity: determineSeverity(source.path),
          reason: 'Source file has no documentation',
          suggestion: 'Create documentation at ' + suggestDocPath(source.path),
        });
      }
    }
  }

  for (const doc of docFiles) {
    if (!isStandaloneDoc(doc.path)) {
      const matchedSource = sourceFiles.find(s => docMatchesSource(doc.path, s.path));
      if (!matchedSource) {
        drifts.push({
          docFile: doc.path,
          driftType: 'orphaned',
          severity: 'low',
          reason: 'Documentation has no matching source',
          suggestion: 'Review and update or remove',
        });
      }
    }
  }

  const readme = docFiles.find(d => d.path.toLowerCase().includes('readme'));
  if (!readme && sourceFiles.length > 10) {
    drifts.push({
      docFile: 'README.md',
      driftType: 'missing',
      severity: 'critical',
      reason: 'Project has no README.md',
      suggestion: 'Create README.md',
    });
  }

  const severityOrder = { critical: 0, high: 1, medium: 2, low: 3 };
  drifts.sort((a, b) => severityOrder[a.severity] - severityOrder[b.severity]);

  const totalNeeded = sourceFiles.filter(s => needsDocumentation(s.path)).length;
  const documented = totalNeeded - drifts.filter(d => d.driftType === 'missing').length;
  const healthScore = totalNeeded > 0 ? Math.round((documented / totalNeeded) * 100) : 100;

  const summary =
    drifts.length > 0
      ? drifts.length + ' documentation drift issues (' + healthScore + '% documented)'
      : 'Documentation is up to date (' + healthScore + '% coverage)';

  return { drifts, summary, healthScore };
}

function isDocFile(filePath) {
  return /\.(md|mdx|txt|rst)$/.test(filePath) || filePath.includes('/docs/');
}

function isSourceFile(filePath) {
  return /\.(tsx?|jsx?|mjs)$/.test(filePath) && !isTestFile(filePath) && !isConfigFile(filePath);
}

function isTestFile(filePath) {
  return /\.(test|spec)\.(tsx?|jsx?|mjs)$/.test(filePath);
}

function isConfigFile(filePath) {
  return /\.(config|rc)\.(tsx?|jsx?|mjs|json)$/.test(filePath);
}

function needsDocumentation(filePath) {
  return filePath.includes('/api/') || filePath.includes('index.') || filePath.includes('/lib/');
}

function docMatchesSource(docPath, sourcePath) {
  const docName = docPath
    .toLowerCase()
    .replace(/\.mdx?$/, '')
    .split('/')
    .pop();
  const sourceName = sourcePath
    .toLowerCase()
    .replace(/\.(tsx?|jsx?|mjs)$/, '')
    .split('/')
    .pop();
  return docName === sourceName || (docName && docName.includes(sourceName || ''));
}

function isStandaloneDoc(docPath) {
  const patterns = ['readme', 'contributing', 'changelog', 'license', 'guide', 'architecture'];
  const filename = docPath.toLowerCase().split('/').pop() || '';
  return patterns.some(p => filename.includes(p));
}

function suggestDocPath(sourcePath) {
  const filename = sourcePath.split('/').pop();
  const mdName = filename ? filename.replace(/\.(tsx?|jsx?|mjs)$/, '.md') : 'doc.md';
  return 'docs/' + mdName;
}

function determineSeverity(filePath) {
  if (filePath.includes('/api/')) return 'high';
  if (filePath.includes('index.')) return 'medium';
  return 'low';
}

export { DriftEntrySchema };

// Alias exports for backwards compatibility with existing index.mjs
export const checkDocConsistency = checkDocDrift;
export const extractDocReferences = options => {
  const result = checkDocDrift(options);
  return result.drifts.map(d => ({
    docFile: d.docFile,
    sourceFile: d.sourceFile,
  }));
};
export const scoreDocDrift = options => {
  const result = checkDocDrift(options);
  return { score: 100 - result.healthScore, driftCount: result.drifts.length };
};
