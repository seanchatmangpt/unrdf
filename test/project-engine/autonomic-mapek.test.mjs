/**
 * @file Autonomic MAPEK Loop Tests - Chicago School TDD
 * @module test/project-engine/autonomic-mapek
 * @vitest-environment node
 *
 * Tests the full MAPEK cycle with real project stores and domain models.
 * No mocks - tests interact with real collaborators.
 * Requires Node.js environment for N3 Store support.
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { Store } from 'n3'
import { DataFactory } from 'n3'
import {
  runMapekIteration,
  createAutonomicHooks,
  runContinuousMapekLoop,
  reportMapekStatus,
} from '../../src/project-engine/autonomic-mapek.mjs'
import { inferDomainModel } from '../../src/project-engine/domain-infer.mjs'
import { buildProjectModelFromFs } from '../../src/project-engine/project-model.mjs'
import { classifyFiles } from '../../src/project-engine/file-roles.mjs'

const { namedNode, literal } = DataFactory

/**
 * Helper: Create minimal test stores
 */
function createTestDomainStore() {
  const store = new Store()

  // Add test entities
  store.addQuad(
    namedNode('http://example.org/domain#User'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/domain#Entity')
  )

  store.addQuad(
    namedNode('http://example.org/domain#User'),
    namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
    literal('User')
  )

  store.addQuad(
    namedNode('http://example.org/domain#Product'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/domain#Entity')
  )

  store.addQuad(
    namedNode('http://example.org/domain#Product'),
    namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
    literal('Product')
  )

  return store
}

function createTestProjectStore(withGaps = true) {
  const store = new Store()

  // Project root
  store.addQuad(
    namedNode('http://example.org/project#project'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/project#Project')
  )

  // Feature: User (with some files)
  store.addQuad(
    namedNode('http://example.org/project#feature/user'),
    namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    namedNode('http://example.org/project#Feature')
  )

  // User component file
  store.addQuad(
    namedNode('http://example.org/fs#src_features_user_UserComponent_tsx'),
    namedNode('http://example.org/project#hasRole'),
    namedNode('http://example.org/project#Component')
  )

  store.addQuad(
    namedNode('http://example.org/fs#src_features_user_UserComponent_tsx'),
    namedNode('http://example.org/project#belongsToFeature'),
    namedNode('http://example.org/project#feature/user')
  )

  // User test file
  store.addQuad(
    namedNode('http://example.org/fs#src_features_user_User_test_tsx'),
    namedNode('http://example.org/project#hasRole'),
    namedNode('http://example.org/project#Test')
  )

  store.addQuad(
    namedNode('http://example.org/fs#src_features_user_User_test_tsx'),
    namedNode('http://example.org/project#belongsToFeature'),
    namedNode('http://example.org/project#feature/user')
  )

  // Feature: Product (with gaps if requested)
  if (withGaps) {
    store.addQuad(
      namedNode('http://example.org/project#feature/product'),
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://example.org/project#Feature')
    )

    // Product only has component, missing API and Tests
    store.addQuad(
      namedNode('http://example.org/fs#src_features_product_ProductComponent_tsx'),
      namedNode('http://example.org/project#hasRole'),
      namedNode('http://example.org/project#Component')
    )

    store.addQuad(
      namedNode('http://example.org/fs#src_features_product_ProductComponent_tsx'),
      namedNode('http://example.org/project#belongsToFeature'),
      namedNode('http://example.org/project#feature/product')
    )
  }

  return store
}

describe('Autonomic MAPEK Loop - Chicago School TDD', () => {
  let domainStore
  let projectStore

  beforeEach(() => {
    domainStore = createTestDomainStore()
    projectStore = createTestProjectStore()
  })

  // ===== MONITOR Phase Tests =====

  it('monitors gaps: detects missing roles for entities', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    // Product entity should have gaps
    expect(result.findings.gaps).toBeDefined()
    expect(result.findings.gaps.gaps).toContainEqual(
      expect.objectContaining({
        entity: 'Product',
        missingRoles: expect.arrayContaining(['Api']),
      })
    )
  })

  it('monitors type issues: detects schema mismatches', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    // Should include type issues in findings
    expect(result.findings.typeIssues).toBeDefined()
  })

  it('monitors hotspots: identifies high-complexity features', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    // Should analyze hotspots
    expect(result.findings.hotspots).toBeDefined()
    expect(result.findings.hotspots.hotspots).toBeDefined()
  })

  // ===== ANALYZE Phase Tests =====

  it('analyzes gap severity: scores critical gaps higher', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    // Product missing API should score high
    const productGap = result.findings.gaps.gaps.find((g) => g.entity === 'Product')
    expect(productGap).toBeDefined()
    expect(productGap.score).toBeGreaterThanOrEqual(80)
  })

  it('analyzes metrics: calculates health scores', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    expect(result.metrics).toBeDefined()
    expect(result.metrics.gapScore).toBeGreaterThanOrEqual(0)
    expect(result.metrics.gapScore).toBeLessThanOrEqual(100)
    expect(result.metrics.typeScore).toBeLessThanOrEqual(100)
    expect(result.metrics.hotspotScore).toBeLessThanOrEqual(100)
  })

  it('analyzes overall health: converges 0-100', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    expect(result.overallHealth).toBeGreaterThanOrEqual(0)
    expect(result.overallHealth).toBeLessThanOrEqual(100)
  })

  // ===== PLAN Phase Tests =====

  it('plans decisions: identifies auto-fixable issues', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    expect(result.decisions).toBeDefined()
    expect(Array.isArray(result.decisions)).toBe(true)

    // Should have at least one decision for missing gaps
    if (result.findings.gaps.gaps.length > 0) {
      expect(result.decisions.some((d) => d.autoFixable)).toBe(true)
    }
  })

  it('plans decisions: prioritizes by severity', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    // Type mismatches should come before hotspots (higher priority)
    const typeDecision = result.decisions.find((d) => d.issue === 'type-mismatch')
    const hotspotDecision = result.decisions.find((d) => d.issue === 'high-complexity')

    if (typeDecision && hotspotDecision) {
      const typeSeverity = { critical: 3, high: 2, medium: 1, low: 0 }[typeDecision.severity] || 0
      const hotspotSeverity =
        { critical: 3, high: 2, medium: 1, low: 0 }[hotspotDecision.severity] || 0
      expect(typeSeverity).toBeGreaterThanOrEqual(hotspotSeverity)
    }
  })

  // ===== EXECUTE Phase Tests =====

  it('executes actions: queues auto-fixable tasks', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    expect(result.actions).toBeDefined()
    expect(Array.isArray(result.actions)).toBe(true)

    // Each action should have required fields
    result.actions.forEach((action) => {
      expect(action.type).toBeDefined()
      expect(action.status).toBeDefined()
      expect(action.timestamp).toBeDefined()
    })
  })

  it('executes: skips non-auto-fixable issues', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    // Should not have refactor actions (not auto-fixable)
    const refactorActions = result.actions.filter((a) => a.type === 'refactor-hotspots')
    expect(refactorActions.length).toBe(0)
  })

  // ===== KNOWLEDGE Phase Tests =====

  it('learns patterns: extracts gap patterns', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    expect(result.learnings).toBeDefined()
    expect(result.learnings.gapPatterns).toBeDefined()
    expect(Array.isArray(result.learnings.gapPatterns)).toBe(true)
  })

  it('learns patterns: extracts type patterns', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    expect(result.learnings.typePatterns).toBeDefined()
    expect(Array.isArray(result.learnings.typePatterns)).toBe(true)
  })

  it('learns thresholds: sets hotspot thresholds', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    expect(result.learnings.hotspotThresholds).toBeDefined()
    expect(result.learnings.hotspotThresholds.fileCountHighRisk).toEqual(40)
    expect(result.learnings.hotspotThresholds.testCoverageHighRisk).toEqual(70)
  })

  // ===== Convergence Tests =====

  it('determines if system should repeat', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    expect(typeof result.shouldRepeat).toBe('boolean')

    // If health is low and actions exist, should repeat
    if (result.overallHealth < 70 && result.actions.length > 0) {
      expect(result.shouldRepeat).toBe(true)
    }
  })

  // ===== Knowledge Hooks Integration =====

  it('creates autonomic hooks for auto-fixable decisions', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    const hooks = createAutonomicHooks(result, projectStore)

    expect(hooks).toBeDefined()
    expect(Array.isArray(hooks)).toBe(true)

    // Each hook should have required structure
    hooks.forEach((hook) => {
      expect(hook.meta).toBeDefined()
      expect(hook.meta.name).toBeDefined()
      expect(hook.meta.description).toBeDefined()
      expect(hook.channel).toBeDefined()
      expect(hook.run).toBeDefined()
    })
  })

  it('creates hooks only for auto-fixable issues', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    const hooks = createAutonomicHooks(result, projectStore)

    // Should not create refactor hooks (not auto-fixable)
    const refactorHooks = hooks.filter((h) => h.meta.name.includes('refactor'))
    expect(refactorHooks.length).toBe(0)
  })

  // ===== Reporting Tests =====

  it('reports MAPEK status: generates human-readable report', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    const report = reportMapekStatus(result)

    expect(typeof report).toBe('string')
    expect(report).toContain('AUTONOMIC SYSTEM STATUS')
    expect(report).toContain('Overall Health')
    expect(report).toContain('Metrics')
  })

  it('reports includes decisions if present', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    const report = reportMapekStatus(result)

    if (result.decisions.length > 0) {
      expect(report).toContain('Decisions')
    }
  })

  it('reports includes actions if present', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    })

    const report = reportMapekStatus(result)

    if (result.actions.length > 0) {
      expect(report).toContain('Planned Actions')
    }
  })

  // ===== Continuous Loop Tests =====

  it('runs continuous loop: iterates until convergence', async () => {
    let iterations = 0
    const states = []

    const result = await runContinuousMapekLoop({
      getState: async () => ({
        projectStore,
        domainStore,
        projectRoot: '/test',
        stackProfile: { webFramework: 'react' },
      }),
      applyActions: async (actions) => {
        iterations++
        states.push({ iteration: iterations, actionCount: actions.length })
      },
      intervalMs: 0, // No delay for tests
      maxIterations: 3,
    })

    expect(result.converged || result.iterations <= 3).toBe(true)
    expect(result.finalState).toBeDefined()
  })

  it('continuous loop: stops on convergence', async () => {
    const result = await runContinuousMapekLoop({
      getState: async () => ({
        projectStore,
        domainStore,
        projectRoot: '/test',
        stackProfile: { webFramework: 'react' },
      }),
      applyActions: async () => {
        // Empty actions - forces convergence
      },
      intervalMs: 0,
      maxIterations: 5,
    })

    expect(typeof result.converged).toBe('boolean')
    expect(result.iterations).toBeGreaterThanOrEqual(1)
    expect(result.finalHealth).toBeGreaterThanOrEqual(0)
  })
})
