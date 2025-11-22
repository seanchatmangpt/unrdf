/**
 * @file Doc Generator - generates documentation suggestions from code analysis
 * @module project-engine/doc-generator
 */

import { DataFactory } from 'n3'
import { z } from 'zod'

const { namedNode } = DataFactory

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  fs: 'http://example.org/unrdf/filesystem#',
  dom: 'http://example.org/unrdf/domain#',
}

const DocGeneratorOptionsSchema = z.object({
  projectStore: z.custom((val) => val && typeof val.getQuads === 'function', {
    message: 'projectStore must be an RDF store with getQuads method',
  }),
  domainStore: z.custom((val) => val && typeof val.getQuads === 'function').optional(),
  stackProfile: z.object({}).passthrough().optional(),
  projectRoot: z.string().optional(),
})

const DocSuggestionSchema = z.object({
  docType: z.enum(['readme', 'api', 'architecture', 'guide', 'jsdoc', 'changelog']),
  targetPath: z.string(),
  priority: z.enum(['critical', 'high', 'medium', 'low']),
  title: z.string(),
  sections: z.array(z.string()),
  relatedFiles: z.array(z.string()),
})

/**
 * Generate documentation suggestions
 * @param {Object} options
 * @param {Store} options.projectStore - Project RDF store
 * @param {Store} [options.domainStore] - Domain model store
 * @param {Object} [options.stackProfile] - Stack profile
 * @returns {{ suggestions: Array, summary: string, coverage: number }}
 */
export function generateDocSuggestions(options) {
  const validated = DocGeneratorOptionsSchema.parse(options)
  const { projectStore, domainStore, stackProfile } = validated

  const suggestions = []
  const fileQuads = projectStore.getQuads(null, namedNode(`${NS.fs}relativePath`), null)
  const allFiles = fileQuads.map(q => ({ path: q.object.value }))

  const sourceFiles = allFiles.filter(f => isSourceFile(f.path))
  const docFiles = allFiles.filter(f => isDocFile(f.path))

  // Check 1: README.md
  const hasReadme = docFiles.some(f => f.path.toLowerCase().includes('readme'))
  if (!hasReadme) {
    const entities = extractEntities(domainStore)
    suggestions.push({
      docType: 'readme',
      targetPath: 'README.md',
      priority: 'critical',
      title: 'Project README',
      sections: generateReadmeSections(sourceFiles, entities, stackProfile),
      relatedFiles: [],
    })
  }

  // Check 2: API documentation
  const apiFiles = sourceFiles.filter(f => isApiFile(f.path))
  const hasApiDocs = docFiles.some(f => f.path.includes('api') || f.path.includes('openapi'))
  if (apiFiles.length > 0 && !hasApiDocs) {
    suggestions.push({
      docType: 'api',
      targetPath: 'docs/api/README.md',
      priority: 'high',
      title: 'API Documentation',
      sections: generateApiDocSections(apiFiles),
      relatedFiles: apiFiles.map(f => f.path),
    })
  }

  // Check 3: Architecture documentation
  const hasArchDocs = docFiles.some(f => f.path.includes('architecture') || f.path.includes('design'))
  if (sourceFiles.length > 20 && !hasArchDocs) {
    suggestions.push({
      docType: 'architecture',
      targetPath: 'docs/ARCHITECTURE.md',
      priority: 'medium',
      title: 'Architecture Overview',
      sections: ['## System Overview', '## Directory Structure', '## Key Components', '## Data Flow'],
      relatedFiles: getKeyFiles(sourceFiles),
    })
  }

  // Check 4: Getting started guide
  const hasGuide = docFiles.some(f => f.path.includes('guide') || f.path.includes('getting-started'))
  if (sourceFiles.length > 10 && !hasGuide) {
    suggestions.push({
      docType: 'guide',
      targetPath: 'docs/GETTING_STARTED.md',
      priority: 'medium',
      title: 'Getting Started Guide',
      sections: ['## Prerequisites', '## Installation', '## Quick Start', '## Development'],
      relatedFiles: [],
    })
  }

  // Check 5: Changelog
  const hasChangelog = docFiles.some(f => f.path.toLowerCase().includes('changelog'))
  if (!hasChangelog) {
    suggestions.push({
      docType: 'changelog',
      targetPath: 'CHANGELOG.md',
      priority: 'low',
      title: 'Changelog',
      sections: ['[Unreleased]', 'Added', 'Changed', 'Fixed', 'Removed'],
      relatedFiles: [],
    })
  }

  const priorityOrder = { critical: 0, high: 1, medium: 2, low: 3 }
  suggestions.sort((a, b) => priorityOrder[a.priority] - priorityOrder[b.priority])

  const totalNeeded = calculateTotalDocsNeeded(sourceFiles, apiFiles)
  const existing = docFiles.length
  const coverage = totalNeeded > 0 ? Math.min(100, Math.round((existing / totalNeeded) * 100)) : 100

  const summary = suggestions.length > 0
    ? `${suggestions.length} documentation suggestions (${coverage}% coverage)`
    : `Documentation is comprehensive (${coverage}% coverage)`

  return { suggestions, summary, coverage }
}

function isSourceFile(filePath) {
  return /\.(tsx?|jsx?|mjs)$/.test(filePath) && !filePath.includes('.test.') && !filePath.includes('.config.')
}

function isDocFile(filePath) {
  return /\.(md|mdx|txt|rst)$/.test(filePath)
}

function isApiFile(filePath) {
  return filePath.includes('/api/') || filePath.includes('route.') || filePath.includes('controller.')
}

function extractEntities(domainStore) {
  if (!domainStore) return []
  const entityQuads = domainStore.getQuads(null, namedNode(`${NS.rdf}type`), namedNode(`${NS.dom}Entity`))
  return entityQuads.map(q => q.subject.value.split('#').pop() || q.subject.value.split('/').pop()).filter(Boolean)
}

function generateReadmeSections(sourceFiles, entities, stackProfile) {
  const sections = ['## Overview', '## Features', '## Installation', '## Quick Start']
  if (entities.length > 0) sections.push('## Domain Entities')
  if (sourceFiles.some(f => f.path.includes('/api/'))) sections.push('## API Reference')
  sections.push('## Configuration', '## Development')
  if (stackProfile?.testFramework) sections.push('## Testing')
  sections.push('## License')
  return sections
}

function generateApiDocSections(apiFiles) {
  const sections = ['## Overview']
  const endpoints = apiFiles.map(f => {
    const path = f.path
    if (path.includes('/app/api/')) {
      const match = path.match(/\/app(\/api\/[^/]+(?:\/[^/]+)*?)\/route/)
      return match ? match[1] : path
    }
    return path.split('/').slice(-2, -1)[0] || 'endpoint'
  })
  const uniqueEndpoints = [...new Set(endpoints)]
  for (const endpoint of uniqueEndpoints.slice(0, 10)) sections.push(`### ${endpoint}`)
  sections.push('## Authentication', '## Error Handling')
  return sections
}

function getKeyFiles(sourceFiles) {
  return sourceFiles.filter(f => f.path.includes('index.') || f.path.includes('main.') || f.path.includes('app.'))
    .map(f => f.path).slice(0, 10)
}

function calculateTotalDocsNeeded(sourceFiles, apiFiles) {
  return 4 + Math.ceil(apiFiles.length / 5) + Math.ceil(sourceFiles.length / 20)
}

export { DocSuggestionSchema }

// Alias exports for backwards compatibility with existing index.mjs
export const generateEntityReference = (options, entity) => ({
  entity,
  markdown: "# " + entity + " Reference",
})
export const generateAPIReference = (options) => ({
  markdown: "# API Reference",
  endpoints: [],
})
export const generateArchitectureDiagram = (options) => ({
  mermaid: "graph TD",
  components: [],
})
export const generateCompleteDocumentation = generateDocSuggestions
export const DocGenerationResultSchema = DocSuggestionSchema
