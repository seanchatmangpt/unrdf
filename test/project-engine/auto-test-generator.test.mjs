/**
 * @file Auto-Test Generator tests - 80/20 consolidated
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { createStore } from '@unrdf/oxigraph';
import {
  inferTestPatterns,
  generateTestSkeleton,
  scoreTestCoverage,
  generateTestFactory,
} from '../../src/project-engine/auto-test-generator.mjs';

const { namedNode, literal } = DataFactory;

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  dom: 'http://example.org/unrdf/domain#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
};

function createFsStore(files) {
  const store = createStore();
  for (const { path, content } of files) {
    const iri = namedNode(`http://example.org/unrdf/fs#${encodeURIComponent(path)}`);
    store.addQuad(iri, namedNode(`${NS.fs}relativePath`), literal(path));
    if (content) store.addQuad(iri, namedNode(`${NS.fs}content`), literal(content));
  }
  return store;
}

function createProjectStore(files) {
  const store = createStore();
  for (const { path, role } of files) {
    const iri = namedNode(`http://example.org/unrdf/fs#${encodeURIComponent(path)}`);
    store.addQuad(iri, namedNode(`${NS.fs}relativePath`), literal(path));
    if (role) store.addQuad(iri, namedNode(`${NS.proj}roleString`), literal(role));
  }
  return store;
}

function createDomainStore(entities) {
  const store = createStore();
  for (const { name, fields = [] } of entities) {
    const iri = namedNode(`${NS.dom}${name}`);
    store.addQuad(iri, namedNode(`${NS.rdf}type`), namedNode(`${NS.dom}Entity`));
    store.addQuad(iri, namedNode(`${NS.rdfs}label`), literal(name));
    for (const field of fields) {
      store.addQuad(iri, namedNode(`${NS.dom}hasField`), namedNode(`${NS.dom}${name}.${field}`));
    }
  }
  return store;
}

const SAMPLE_VITEST_TEST = `
import { describe, it, expect, beforeEach, vi } from 'vitest'
import { User } from '../../src/user.mjs'

describe('User', () => {
  let user
  beforeEach(() => { user = new User() })
  describe('creation', () => {
    it('should create a user instance', () => { expect(user).toBeDefined() })
    it('should have default values', () => { expect(user.id).toBe(null) })
    it('should validate email format', () => { expect(() => user.setEmail('invalid')).toThrow() })
  })
})
`;

describe('auto-test-generator', () => {
  describe('inferTestPatterns', () => {
    it('returns default patterns when no tests exist', () => {
      const fsStore = createFsStore([{ path: 'src/user.mjs' }]);
      const result = inferTestPatterns({
        fsStore,
        projectStore: createProjectStore([]),
      });
      expect(result.testFramework).toBe('vitest');
      expect(result.fileExtension).toBe('mjs');
      expect(result.assertionPatterns).toContain('toBe');
    });

    it('detects vitest framework and extracts patterns', () => {
      const fsStore = createFsStore([{ path: 'test/user.test.mjs', content: SAMPLE_VITEST_TEST }]);
      const result = inferTestPatterns({
        fsStore,
        projectStore: createProjectStore([]),
      });
      expect(result.testFramework).toBe('vitest');
      expect(result.describeBlocks).toContain('User');
      expect(result.assertionPatterns).toContain('toBeDefined');
      expect(result.setupTeardown.hasBeforeEach).toBe(true);
    });
  });

  describe('generateTestSkeleton', () => {
    const defaultPatterns = {
      describeBlocks: ['User'],
      assertionPatterns: ['toBe', 'toEqual', 'toBeDefined'],
      setupTeardown: {
        hasBeforeEach: true,
        hasAfterEach: false,
        hasBeforeAll: false,
        hasAfterAll: false,
      },
      imports: [],
      testFramework: 'vitest',
      fileExtension: 'mjs',
      testSuffix: 'test',
    };

    it('generates test skeleton with correct structure', () => {
      const result = generateTestSkeleton({
        entity: 'User',
        existingTestPatterns: defaultPatterns,
      });
      expect(result.filename).toBe('user.test.mjs');
      expect(result.content).toContain("import { describe, it, expect, beforeEach } from 'vitest'");
      expect(result.content).toContain("describe('User'");
      expect(result.suggestedTests).toContain('should create User instance');
    });

    it('generates field tests with domain store', () => {
      const domainStore = createDomainStore([{ name: 'User', fields: ['id', 'email'] }]);
      const result = generateTestSkeleton({
        entity: 'User',
        existingTestPatterns: defaultPatterns,
        domainStore,
      });
      expect(result.suggestedTests).toContain('should have id property');
      expect(result.content).toContain('user.id');
    });

    it('uses .spec suffix and .ts extension when patterns indicate', () => {
      const tsPatterns = {
        ...defaultPatterns,
        testSuffix: 'spec',
        fileExtension: 'ts',
      };
      const result = generateTestSkeleton({
        entity: 'User',
        existingTestPatterns: tsPatterns,
      });
      expect(result.filename).toBe('user.spec.ts');
    });
  });

  describe('scoreTestCoverage', () => {
    it('calculates coverage correctly', () => {
      const noTests = scoreTestCoverage({ entity: 'User', testFiles: [] });
      expect(noTests.coverage).toBe(0);
      expect(noTests.needsTests).toBe(true);

      const withTests = scoreTestCoverage({
        entity: 'User',
        testFiles: ['test/user.test.mjs', 'test/user.integration.test.mjs'],
        sourceFiles: ['src/user.mjs'],
      });
      expect(withTests.coverage).toBe(100);
      expect(withTests.needsTests).toBe(false);
    });

    it('matches entity name variations', () => {
      const kebab = scoreTestCoverage({
        entity: 'UserProfile',
        testFiles: ['test/user-profile.test.mjs'],
      });
      expect(kebab.existingTests).toContain('test/user-profile.test.mjs');
    });
  });

  describe('generateTestFactory', () => {
    it('generates factory function', () => {
      const result = generateTestFactory('User');
      expect(result).toContain('function createUser(');
      expect(result).toContain('overrides = {}');
    });
  });

  describe('integration', () => {
    it('full workflow: infer patterns -> generate skeleton -> score coverage', () => {
      const fsStore = createFsStore([{ path: 'test/user.test.mjs', content: SAMPLE_VITEST_TEST }]);
      const projectStore = createProjectStore([{ path: 'test/user.test.mjs', role: 'Test' }]);
      const domainStore = createDomainStore([{ name: 'Order', fields: ['id', 'total'] }]);

      const patterns = inferTestPatterns({ fsStore, projectStore });
      const skeleton = generateTestSkeleton({
        entity: 'Order',
        existingTestPatterns: patterns,
        domainStore,
      });
      const coverage = scoreTestCoverage({
        entity: 'Order',
        testFiles: [],
        sourceFiles: ['src/order.mjs'],
      });

      expect(skeleton.filename).toBe('order.test.mjs');
      expect(coverage.needsTests).toBe(true);
    });
  });
});
