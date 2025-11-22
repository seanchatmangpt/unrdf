/**
 * @file Auto Test Generator - generates test suggestions from code analysis
 * @module project-engine/auto-test-generator
 */

import { DataFactory } from 'n3'
import { z } from 'zod'

const { namedNode } = DataFactory

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
}

const AutoTestOptionsSchema = z.object({
  projectStore: z.custom((val) => val && typeof val.getQuads === 'function', {
    message: 'projectStore must be an RDF store with getQuads method',
  }),
  domainStore: z.custom((val) => val && typeof val.getQuads === 'function', {
    message: 'domainStore must be an RDF store with getQuads method',
  }).optional(),
  stackProfile: z.object({
    testFramework: z.string().nullable().optional(),
  }).passthrough().optional(),
})

const TestSuggestionSchema = z.object({
  file: z.string(),
  testFile: z.string(),
  testType: z.enum(['unit', 'integration', 'e2e']),
  priority: z.enum(['critical', 'high', 'medium', 'low']),
  reason: z.string(),
  suggestedTests: z.array(z.string()),
})

/**
 * Generate test suggestions based on code analysis
 * @param {Object} options
 * @param {Store} options.projectStore - Project RDF store
 * @param {Store} [options.domainStore] - Domain model store
 * @param {Object} [options.stackProfile] - Stack profile
 * @returns {{ suggestions: Array, summary: string, coverage: number }}
 */
export function generateTestSuggestions(options) {
  const validated = AutoTestOptionsSchema.parse(options)
  const { projectStore, stackProfile } = validated

  const suggestions = []
  const fileQuads = projectStore.getQuads(null, namedNode(`${NS.fs}relativePath`), null)

  let totalFiles = 0
  let filesWithTests = 0

  for (const quad of fileQuads) {
    const filePath = quad.object.value
    if (isTestFile(filePath) || isConfigFile(filePath) || !isSourceFile(filePath)) continue

    totalFiles++
    const testPath = generateTestPath(filePath, stackProfile)
    const hasTest = hasExistingTest(filePath, projectStore)

    if (hasTest) {
      filesWithTests++
      continue
    }

    const roleQuads = projectStore.getQuads(quad.subject, namedNode(`${NS.proj}roleString`), null)
    const role = roleQuads.length > 0 ? roleQuads[0].object.value : 'Unknown'
    const { testType, priority } = determineTestType(filePath, role)
    const suggestedTests = generateSuggestedTestCases(filePath, role)

    suggestions.push({ file: filePath, testFile: testPath, testType, priority, reason: `${role} file without test`, suggestedTests })
  }

  const priorityOrder = { critical: 0, high: 1, medium: 2, low: 3 }
  suggestions.sort((a, b) => priorityOrder[a.priority] - priorityOrder[b.priority])

  const coverage = totalFiles > 0 ? Math.round((filesWithTests / totalFiles) * 100) : 100
  const summary = suggestions.length > 0
    ? `${suggestions.length} files need tests (${coverage}% coverage)`
    : `All files have tests (${coverage}% coverage)`

  return { suggestions, summary, coverage }
}

function isTestFile(filePath) {
  return /\.(test|spec)\.(tsx?|jsx?|mjs)$/.test(filePath) || /^(test|tests|__tests__|spec)\//.test(filePath)
}

function isConfigFile(filePath) {
  return /\.(config|rc)\.(tsx?|jsx?|mjs|json)$/.test(filePath)
}

function isSourceFile(filePath) {
  return /\.(tsx?|jsx?|mjs)$/.test(filePath)
}

function generateTestPath(filePath, stackProfile) {
  const ext = filePath.match(/\.(tsx?|jsx?|mjs)$/)?.[1] || 'mjs'
  const baseName = filePath.replace(/\.(tsx?|jsx?|mjs)$/, '')
  return `${baseName}.test.${ext}`
}

function hasExistingTest(filePath, projectStore) {
  const baseName = filePath.replace(/\.(tsx?|jsx?|mjs)$/, '')
  const testPatterns = [`${baseName}.test.`, `${baseName}.spec.`]
  const allPaths = projectStore.getQuads(null, namedNode(`${NS.fs}relativePath`), null).map(q => q.object.value)
  return testPatterns.some(pattern => allPaths.some(p => p.includes(pattern)))
}

function determineTestType(filePath, role) {
  const roleMap = {
    Api: { testType: 'integration', priority: 'critical' },
    Route: { testType: 'integration', priority: 'critical' },
    Service: { testType: 'unit', priority: 'high' },
    Schema: { testType: 'unit', priority: 'high' },
    Component: { testType: 'unit', priority: 'medium' },
  }
  return roleMap[role] || { testType: 'unit', priority: 'medium' }
}

function generateSuggestedTestCases(filePath, role) {
  const baseName = filePath.split('/').pop()?.replace(/\.(tsx?|jsx?|mjs)$/, '')
  const tests = role === 'Api' || role === 'Route'
    ? ['should handle GET requests', 'should handle POST requests', 'should return 400 for invalid input']
    : ['should work correctly with valid input', 'should handle edge cases']
  return tests.map(t => `${baseName}: ${t}`)
}

// Alias exports for backwards compatibility with existing index.mjs
export const inferTestPatterns = generateTestSuggestions
export const generateTestSkeleton = (options) => {
  const result = generateTestSuggestions(options)
  return result.suggestions.map(s => ({
    path: s.testFile,
    template: s.suggestedTests.join('\n'),
    testType: s.testType,
  }))
}
export const scoreTestCoverage = (options) => {
  const result = generateTestSuggestions(options)
  return { coverage: result.coverage, filesWithTests: 0, totalFiles: 0 }
}
export const generateTestFactory = generateTestSuggestions

export { TestSuggestionSchema }
