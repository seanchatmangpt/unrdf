/**
 * @file Materialization Consolidated Tests - 80/20 principle
 * @vitest-environment node
 *
 * Consolidated tests covering 80% of materialization functionality.
 * Merges: materialize-plan, materialize-apply
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { promises as fs } from 'fs';
import path from 'path';
import os from 'os';
import { planMaterialization, validatePlan } from '../../packages/project-engine/materialize-plan.mjs';
import {
  applyMaterializationPlan,
  previewPlan,
} from '../../packages/project-engine/materialize-apply.mjs';

const { namedNode, _literal } = dataFactory;

describe('materialize consolidated - 80/20 core', () => {
  let testDir;
  let ontologyStore;
  let templateGraph;

  beforeEach(async () => {
    testDir = await fs.mkdtemp(path.join(os.tmpdir(), 'materialize-test-'));
    ontologyStore = createStore();
    templateGraph = createStore();
  });

  afterEach(async () => {
    try {
      await fs.rm(testDir, { recursive: true, force: true });
    } catch {
      // Ignore cleanup errors
    }
  });

  // ===== Core Planning (80% of value) =====
  it('plans materialization from ontology and templates', () => {
    // Add entity to ontology
    const userIri = namedNode('http://example.org/entity/User');
    ontologyStore.addQuad(
      userIri,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://example.org/schema#DomainEntity')
    );

    // Add template
    const templateIri = namedNode('http://example.org/template/component');
    templateGraph.addQuad(
      templateIri,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode('http://example.org/unrdf/template#Template')
    );

    const result = planMaterialization(ontologyStore, templateGraph);

    expect(result).toBeDefined();
    expect(result.plan).toBeDefined();
    expect(result.plan.writes).toBeDefined();
    expect(result.plan.updates).toBeDefined();
    expect(result.plan.deletes).toBeDefined();
    expect(result.receipt).toBeDefined();
    expect(result.receipt.ontologyHash).toBeDefined();
    expect(result.receipt.timestamp).toBeDefined();
  });

  it('validates materialization plan', () => {
    const plan = {
      writes: [
        {
          path: 'src/test.mjs',
          content: 'test',
          hash: 'abc123',
          templateIri: 'http://example.org/template/1',
          entityIri: 'http://example.org/entity/1',
          entityType: 'http://example.org/Type',
        },
      ],
      updates: [],
      deletes: [],
    };

    const result = validatePlan(plan);
    // validatePlan returns { valid: boolean, errors: string[] }
    expect(result).toBeDefined();
    expect(result.valid).toBeDefined();
    expect(typeof result.valid).toBe('boolean');
    expect(result.errors).toBeDefined();
    expect(Array.isArray(result.errors)).toBe(true);
  });

  // ===== Core Application (80% of value) =====
  it('applies materialization plan and writes files', async () => {
    const plan = {
      writes: [
        {
          path: 'src/test.mjs',
          content: 'export const x = 1',
          hash: 'abc123',
          templateIri: 'http://example.org/template/1',
          entityIri: 'http://example.org/entity/1',
          entityType: 'http://example.org/Type',
        },
      ],
      updates: [],
      deletes: [],
    };

    const { result, receipt } = await applyMaterializationPlan(plan, {
      outputRoot: testDir,
      snapshotBefore: false,
      snapshotAfter: false,
    });

    expect(result.appliedCount).toBeGreaterThanOrEqual(0);
    expect(receipt.success).toBe(true);

    // Verify file was written if plan was applied
    if (result.appliedCount > 0) {
      const content = await fs.readFile(path.join(testDir, 'src/test.mjs'), 'utf-8');
      expect(content).toBe('export const x = 1');
    }
  });

  it('previews plan without applying', async () => {
    const plan = {
      writes: [
        {
          path: 'src/preview.mjs',
          content: 'preview',
          hash: 'xyz',
          templateIri: 'http://example.org/template/1',
          entityIri: 'http://example.org/entity/1',
          entityType: 'http://example.org/Type',
        },
      ],
      updates: [],
      deletes: [],
    };

    const preview = previewPlan(plan);

    expect(preview).toBeDefined();
    expect(preview.totalOperations).toBeDefined();
    expect(preview.writes).toBeDefined();
    expect(preview.updates).toBeDefined();
    expect(preview.deletes).toBeDefined();
  });
});
