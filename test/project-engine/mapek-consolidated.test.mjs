/**
 * @file MAPEK Consolidated Tests - 80/20 principle
 * @vitest-environment node
 *
 * Consolidated tests covering 80% of MAPEK functionality with 20% of test cases.
 * Merges: autonomic-mapek, autonomic-e2e, mapek-full-integration
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  runMapekIteration,
  createAutonomicHooks,
  runContinuousMapekLoop,
  reportMapekStatus,
} from '../../packages/project-engine/autonomic-mapek.mjs';
import {
  _runFullMapekWithAllInnovations,
  runInnovationsParallel,
  aggregateInnovationFindings,
  _ALL_INNOVATIONS,
} from '../../packages/project-engine/mapek-orchestration.mjs';

const { namedNode, literal } = dataFactory;

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  dom: 'http://example.org/unrdf/domain#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
};

function createTestDomainStore() {
  const store = createStore();
  ['User', 'Product'].forEach(entity => {
    store.addQuad(
      namedNode(`${NS.dom}${entity}`),
      namedNode(`${NS.rdf}type`),
      namedNode(`${NS.dom}Entity`)
    );
    store.addQuad(
      namedNode(`${NS.dom}${entity}`),
      namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
      literal(entity)
    );
  });
  return store;
}

function createTestProjectStore() {
  const store = createStore();
  // User feature (complete)
  ['Component', 'Api', 'Test'].forEach(role => {
    const fileIri = namedNode(`${NS.fs}src/features/user/User${role}.tsx`);
    store.addQuad(
      fileIri,
      namedNode(`${NS.fs}relativePath`),
      literal(`src/features/user/User${role}.tsx`)
    );
    store.addQuad(fileIri, namedNode(`${NS.proj}roleString`), literal(role));
  });
  // Product feature (missing Api, Test) - only has Component
  const productIri = namedNode(`${NS.fs}src/features/product/ProductComponent.tsx`);
  store.addQuad(
    productIri,
    namedNode(`${NS.fs}relativePath`),
    literal('src/features/product/ProductComponent.tsx')
  );
  store.addQuad(productIri, namedNode(`${NS.proj}roleString`), literal('Component'));
  return store;
}

describe('MAPEK Consolidated - 80/20 Core Functionality', () => {
  let domainStore, projectStore;

  beforeEach(() => {
    domainStore = createTestDomainStore();
    projectStore = createTestProjectStore();
  });

  // ===== Core MAPEK Iteration (80% of value) =====
  it('runs full MAPEK iteration: monitor, analyze, plan, execute, knowledge', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    });

    // Monitor phase
    expect(result.findings.gaps).toBeDefined();
    expect(result.findings.typeIssues).toBeDefined();
    expect(result.findings.hotspots).toBeDefined();

    // Analyze phase
    expect(result.metrics).toBeDefined();
    expect(result.overallHealth).toBeGreaterThanOrEqual(0);
    expect(result.overallHealth).toBeLessThanOrEqual(100);

    // Plan phase
    expect(result.decisions).toBeDefined();
    expect(Array.isArray(result.decisions)).toBe(true);

    // Execute phase
    expect(result.actions).toBeDefined();
    expect(Array.isArray(result.actions)).toBe(true);

    // Knowledge phase
    expect(result.learnings).toBeDefined();
    expect(typeof result.shouldRepeat).toBe('boolean');
  });

  // ===== Continuous Loop (20% of value) =====
  it('runs continuous loop until convergence', async () => {
    const result = await runContinuousMapekLoop({
      getState: async () => ({
        projectStore,
        domainStore,
        projectRoot: '/test',
        stackProfile: { webFramework: 'react' },
      }),
      applyActions: async () => {},
      intervalMs: 0,
      maxIterations: 3,
    });

    expect(result.iterations).toBeGreaterThanOrEqual(1);
    expect(typeof result.converged).toBe('boolean');
    expect(result.finalState).toBeDefined();
  });

  // ===== Knowledge Hooks (20% of value) =====
  it('creates autonomic hooks from MAPEK decisions', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    });

    const hooks = createAutonomicHooks(result, projectStore);
    expect(Array.isArray(hooks)).toBe(true);
    hooks.forEach(hook => {
      expect(hook.meta).toBeDefined();
      expect(hook.channel).toBeDefined();
      expect(typeof hook.run).toBe('function');
    });
  });

  // ===== Reporting (10% of value) =====
  it('generates human-readable status report', async () => {
    const result = await runMapekIteration({
      projectStore,
      domainStore,
      projectRoot: '/test',
      stackProfile: { webFramework: 'react' },
    });

    const report = reportMapekStatus(result);
    expect(typeof report).toBe('string');
    expect(report).toContain('AUTONOMIC SYSTEM STATUS');
  });

  // ===== Orchestration (20% of value) =====
  it('runs innovations in parallel and aggregates findings', async () => {
    const results = await runInnovationsParallel({
      projectStore,
      domainStore,
      stackProfile: { webFramework: 'react' },
      projectRoot: '/test',
    });

    expect(results.innovationsRun).toBeGreaterThan(0);
    expect(results.results).toBeDefined();

    const aggregated = aggregateInnovationFindings(results);
    expect(aggregated.overallHealth).toBeGreaterThanOrEqual(0);
    expect(aggregated.overallHealth).toBeLessThanOrEqual(100);
  });
});
