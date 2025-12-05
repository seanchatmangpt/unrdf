/**
 * @fileoverview Minimal tests for project engine modules
 * Tests core functionality: config, stack detection, materialization
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import {
  materializeArtifacts,
  getProjectEngineConfig,
  deriveHooksFromStructure,
  analyzePatternViolations,
  createCustomPatternHook,
  inferTemplatesFromProject,
  inferTemplatesWithDomainBinding,
  getTemplatesByKind,
  serializeTemplates,
} from '../packages/project-engine/index.mjs';

const { namedNode, literal } = dataFactory;

describe('project-engine', () => {
  describe('Config', () => {
    it('loads default configuration', () => {
      const config = getProjectEngineConfig();
      expect(config.fs.ignorePatterns).toContain('node_modules');
      expect(config.project.conventions.sourcePaths).toContain('src');
      expect(config.diff.structureLens).toBe('project-structure');
    });

    it('merges configuration overrides', () => {
      const config = getProjectEngineConfig({
        fs: { ignorePatterns: ['custom'] },
      });
      expect(config.fs.ignorePatterns).toContain('custom');
      expect(config.fs.ignorePatterns).toContain('node_modules');
    });

    it('validates configuration schema', () => {
      expect(() => {
        getProjectEngineConfig({
          golden: { profile: 'invalid-profile' },
        });
      }).toThrow();
    });
  });

  describe('Materialization', () => {
    it('creates materialization plan from ontology', () => {
      const store = createStore();
      const projectIri = namedNode('http://example.org/unrdf/project#project');
      store.addQuad(
        projectIri,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/unrdf/project#Project')
      );

      const { plan, receipt } = materializeArtifacts({
        ontologyStore: store,
      });

      expect(plan).toHaveProperty('writes');
      expect(plan).toHaveProperty('deletes');
      expect(plan).toHaveProperty('moves');
      expect(receipt).toHaveProperty('beforeHash');
      expect(receipt).toHaveProperty('planHash');
    });

    it('includes metadata in receipt', () => {
      const store = createStore();
      const { receipt } = materializeArtifacts({
        ontologyStore: store,
      });

      expect(receipt).toHaveProperty('timestamp');
      expect(receipt).toHaveProperty('changes');
      expect(receipt.dryRun).toBe(false);
    });
  });

  describe('Policy Derivation', () => {
    /**
     * Create a mock project store with features and files
     */
    function createMockProjectStore() {
      const store = createStore();
      const projectNs = 'http://example.org/unrdf/project#';
      const fsNs = 'http://example.org/unrdf/filesystem#';
      const rdfType = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';

      // Add a project
      store.addQuad(
        namedNode(`${projectNs}project`),
        namedNode(rdfType),
        namedNode(`${projectNs}Project`)
      );

      // Add a feature with a view
      store.addQuad(
        namedNode(`${projectNs}feature/auth`),
        namedNode(rdfType),
        namedNode(`${projectNs}Feature`)
      );
      store.addQuad(
        namedNode(`${projectNs}feature/auth`),
        namedNode('http://www.w3.org/2000/01/rdf-schema#label'),
        literal('auth')
      );

      // Add a file that belongs to the feature with Component role
      const authFile = namedNode(`${fsNs}src%2Ffeatures%2Fauth%2FLoginForm.tsx`);
      store.addQuad(
        authFile,
        namedNode(`${fsNs}relativePath`),
        literal('src/features/auth/LoginForm.tsx')
      );
      store.addQuad(
        authFile,
        namedNode(`${projectNs}belongsToFeature`),
        namedNode(`${projectNs}feature/auth`)
      );
      store.addQuad(authFile, namedNode(`${projectNs}roleString`), literal('Component'));

      // Add a feature without view (violation)
      store.addQuad(
        namedNode(`${projectNs}feature/utils`),
        namedNode(rdfType),
        namedNode(`${projectNs}Feature`)
      );

      // Add a file for utils with no view role
      const utilsFile = namedNode(`${fsNs}src%2Ffeatures%2Futils%2Fhelpers.ts`);
      store.addQuad(
        utilsFile,
        namedNode(`${fsNs}relativePath`),
        literal('src/features/utils/helpers.ts')
      );
      store.addQuad(
        utilsFile,
        namedNode(`${projectNs}belongsToFeature`),
        namedNode(`${projectNs}feature/utils`)
      );
      store.addQuad(utilsFile, namedNode(`${projectNs}roleString`), literal('Service'));

      return store;
    }

    it('derives hooks from project structure', () => {
      const store = createMockProjectStore();
      const stackProfile = { uiFramework: 'react', webFramework: null };

      const hooks = deriveHooksFromStructure(store, stackProfile);

      expect(hooks).toBeInstanceOf(Array);
      expect(hooks.length).toBeGreaterThan(0);

      // Check that all hooks have required properties
      for (const hook of hooks) {
        expect(hook).toHaveProperty('meta');
        expect(hook.meta).toHaveProperty('name');
        expect(hook).toHaveProperty('when');
        expect(hook).toHaveProperty('run');
        expect(typeof hook.run).toBe('function');
      }
    });

    it('generates hook for feature-view policy', () => {
      const store = createMockProjectStore();
      const stackProfile = { uiFramework: 'react' };

      const hooks = deriveHooksFromStructure(store, stackProfile, {
        enableFeatureViewPolicy: true,
        enableApiTestPolicy: false,
        enableOrphanFilePolicy: false,
        enableTestCompanionPolicy: false,
        enableStackPolicies: false,
      });

      const featureViewHook = hooks.find(h => h.meta.name === 'derived:feature-must-have-view');
      expect(featureViewHook).toBeDefined();
      expect(featureViewHook.meta.description).toContain('Feature');
      expect(featureViewHook.when.kind).toBe('sparql-ask');
    });

    it('analyzes pattern violations without generating hooks', () => {
      const store = createMockProjectStore();
      const stackProfile = { uiFramework: 'react' };

      const report = analyzePatternViolations(store, stackProfile);

      expect(report).toHaveProperty('timestamp');
      expect(report).toHaveProperty('violations');
      expect(report).toHaveProperty('summary');
      expect(report.summary).toHaveProperty('total');
      expect(typeof report.summary.total).toBe('number');
    });

    it('creates custom pattern hooks', () => {
      const customHook = createCustomPatternHook({
        name: 'no-console-logs',
        description: 'Disallow console.log in production files',
        pattern: 'no-console',
        condition: {
          kind: 'sparql-ask',
          query: 'ASK { ?s ?p ?o }',
        },
        validator: _store => [],
      });

      expect(customHook.meta.name).toBe('custom:no-console-logs');
      expect(customHook.meta.description).toContain('console.log');
      expect(typeof customHook.run).toBe('function');
    });

    it('hook run function returns valid result structure', () => {
      const store = createMockProjectStore();
      const stackProfile = { uiFramework: 'react' };

      const hooks = deriveHooksFromStructure(store, stackProfile);
      const hook = hooks[0];

      const result = hook.run({
        payload: {},
        context: { graph: store, env: {} },
      });

      expect(result).toHaveProperty('result');
      expect(result.result).toHaveProperty('valid');
      expect(result.result).toHaveProperty('violations');
      expect(typeof result.result.valid).toBe('boolean');
      expect(result.result.violations).toBeInstanceOf(Array);
    });

    it('throws on invalid projectStore', () => {
      expect(() => {
        deriveHooksFromStructure(null, {});
      }).toThrow('deriveHooksFromStructure: projectStore must be an N3 Store');

      expect(() => {
        deriveHooksFromStructure({}, {});
      }).toThrow('deriveHooksFromStructure: projectStore must be an N3 Store');
    });

    it('handles empty store gracefully', () => {
      const emptyStore = createStore();
      const hooks = deriveHooksFromStructure(emptyStore, {});

      expect(hooks).toBeInstanceOf(Array);
      expect(hooks.length).toBeGreaterThan(0);

      // Run hooks on empty store - should not error
      for (const hook of hooks) {
        const result = hook.run({
          payload: {},
          context: { graph: emptyStore, env: {} },
        });
        expect(result.result.valid).toBe(true);
        expect(result.result.violations).toHaveLength(0);
      }
    });

    it('respects options to disable specific policies', () => {
      const store = createMockProjectStore();

      const allHooks = deriveHooksFromStructure(store, {});
      const limitedHooks = deriveHooksFromStructure(
        store,
        {},
        {
          enableFeatureViewPolicy: true,
          enableApiTestPolicy: false,
          enableOrphanFilePolicy: false,
          enableTestCompanionPolicy: false,
          enableStackPolicies: false,
        }
      );

      expect(limitedHooks.length).toBeLessThan(allHooks.length);
      expect(limitedHooks.length).toBe(1);
    });

    it('adds Next.js app router hook when stack matches', () => {
      const store = createMockProjectStore();
      const stackProfile = { webFramework: 'next-app-router' };

      const hooks = deriveHooksFromStructure(store, stackProfile, {
        enableStackPolicies: true,
      });

      const nextHook = hooks.find(h => h.meta.name === 'derived:next-app-router-structure');
      expect(nextHook).toBeDefined();
      expect(nextHook.meta.ontology).toContain('next-app-router');
    });
  });

  describe('Template Inference', () => {
    const NS = {
      rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
      rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
      gen: 'http://example.org/unrdf/generator#',
      fs: 'http://example.org/unrdf/filesystem#',
    };

    /**
     * Create a mock FS store with file paths
     */
    function createMockFsStore(paths) {
      const store = createStore();
      for (const path of paths) {
        const fileIri = namedNode(`http://example.org/fs#${encodeURIComponent(path)}`);
        store.addQuad(fileIri, namedNode(`${NS.fs}relativePath`), literal(path));
      }
      return store;
    }

    it('returns empty result for empty store', () => {
      const fsStore = createStore();
      const { store, summary } = inferTemplatesFromProject(fsStore);

      expect(summary.templateCount).toBe(0);
      expect(Object.keys(summary.byKind)).toHaveLength(0);
      expect(store.size).toBe(0);
    });

    it('does not infer templates from single file (needs >= 2)', () => {
      const fsStore = createMockFsStore(['src/features/user/UserPage.tsx']);

      const { summary } = inferTemplatesFromProject(fsStore);
      expect(summary.templateCount).toBe(0);
    });

    it('infers Component templates from multiple component files', () => {
      const fsStore = createMockFsStore([
        'src/features/user/UserComponent.tsx',
        'src/features/product/ProductComponent.tsx',
        'src/features/order/OrderComponent.tsx',
      ]);

      const { store, summary } = inferTemplatesFromProject(fsStore);

      expect(summary.templateCount).toBeGreaterThan(0);
      expect(summary.byKind.Component).toBe(1);

      const templates = getTemplatesByKind(store, 'Component');
      expect(templates).toHaveLength(1);
      expect(templates[0].variantCount).toBe(3);
    });

    it('infers Page templates', () => {
      const fsStore = createMockFsStore([
        'src/app/dashboard/page.tsx',
        'src/app/settings/page.tsx',
        'src/app/profile/page.tsx',
      ]);

      const { store, summary } = inferTemplatesFromProject(fsStore);

      expect(summary.byKind.Page).toBe(1);
      const templates = getTemplatesByKind(store, 'Page');
      expect(templates).toHaveLength(1);
      expect(templates[0].outputPattern).toContain('{{route}}');
    });

    it('infers Test templates', () => {
      const fsStore = createMockFsStore([
        'src/utils/format.test.ts',
        'src/utils/validate.test.ts',
        'src/services/api.spec.ts',
      ]);

      const { store, summary } = inferTemplatesFromProject(fsStore);
      expect(summary.byKind.Test).toBe(1);

      const templates = getTemplatesByKind(store, 'Test');
      expect(templates).toHaveLength(1);
    });

    it('infers multiple template kinds from mixed project', () => {
      const fsStore = createMockFsStore([
        'src/features/user/UserView.tsx',
        'src/features/product/ProductView.tsx',
        'src/app/home/page.tsx',
        'src/app/about/page.tsx',
        'test/unit/utils.test.ts',
        'test/unit/helpers.test.ts',
        'src/hooks/useAuth.ts',
        'src/hooks/useUser.ts',
      ]);

      const { summary } = inferTemplatesFromProject(fsStore);

      expect(summary.templateCount).toBeGreaterThanOrEqual(4);
      expect(summary.byKind.Component).toBe(1);
      expect(summary.byKind.Page).toBe(1);
      expect(summary.byKind.Test).toBe(1);
      expect(summary.byKind.Hook).toBe(1);
    });

    it('includes examples in template RDF', () => {
      const fsStore = createMockFsStore([
        'src/features/user/UserComponent.tsx',
        'src/features/product/ProductComponent.tsx',
      ]);

      const { store } = inferTemplatesFromProject(fsStore);

      const examples = store.getQuads(null, namedNode(`${NS.gen}example`), null);
      expect(examples.length).toBeGreaterThan(0);
    });

    it('extracts variables from output pattern', () => {
      const fsStore = createMockFsStore([
        'src/features/user/UserComponent.tsx',
        'src/features/product/ProductComponent.tsx',
      ]);

      const { store } = inferTemplatesFromProject(fsStore);

      const variables = store.getQuads(null, namedNode(`${NS.gen}variable`), null);
      expect(variables.length).toBeGreaterThan(0);
      const varNames = variables.map(q => q.object.value);
      expect(varNames).toContain('entity');
    });

    it('binds templates to domain entities', () => {
      const fsStore = createMockFsStore([
        'src/features/user/UserComponent.tsx',
        'src/features/user/UserPage.tsx',
        'src/features/product/ProductComponent.tsx',
        'src/features/product/ProductPage.tsx',
      ]);

      const domainStore = createStore();
      domainStore.addQuad(
        namedNode('http://example.org/domain#User'),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.rdfs}Class`)
      );
      domainStore.addQuad(
        namedNode('http://example.org/domain#Product'),
        namedNode(`${NS.rdf}type`),
        namedNode(`${NS.rdfs}Class`)
      );

      const { store, summary } = inferTemplatesWithDomainBinding(fsStore, domainStore);

      expect(summary.boundEntities).toBeGreaterThan(0);
      const bindings = store.getQuads(null, namedNode(`${NS.gen}targetsClass`), null);
      expect(bindings.length).toBeGreaterThan(0);
    });

    it('works without domain store', () => {
      const fsStore = createMockFsStore([
        'src/features/user/UserComponent.tsx',
        'src/features/product/ProductComponent.tsx',
      ]);

      const { summary } = inferTemplatesWithDomainBinding(fsStore, null);

      expect(summary.templateCount).toBeGreaterThan(0);
      expect(summary.boundEntities).toBe(0);
    });

    it('serializes templates to plain objects', () => {
      const fsStore = createMockFsStore([
        'src/features/user/UserComponent.tsx',
        'src/features/product/ProductComponent.tsx',
        'src/app/home/page.tsx',
        'src/app/about/page.tsx',
      ]);

      const { store } = inferTemplatesFromProject(fsStore);
      const serialized = serializeTemplates(store);

      expect(serialized).toBeInstanceOf(Array);
      expect(serialized.length).toBeGreaterThan(0);

      for (const template of serialized) {
        expect(template).toHaveProperty('id');
        expect(template).toHaveProperty('iri');
        expect(template).toHaveProperty('templateKind');
        expect(template).toHaveProperty('outputPattern');
      }
    });
  });
});
