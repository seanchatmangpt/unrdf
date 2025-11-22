/**
 * @file Gap finder tests - 80/20 consolidated
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest';
import { Store, DataFactory } from 'n3';
import { findMissingRoles, scoreMissingRole } from '../../src/project-engine/gap-finder.mjs';

const { namedNode, literal } = DataFactory;

const NS = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  dom: 'http://example.org/unrdf/domain#',
  fs: 'http://example.org/unrdf/filesystem#',
  proj: 'http://example.org/unrdf/project#',
};

function createDomainStore(entities) {
  const store = new Store();
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

function createProjectStore(files) {
  const store = new Store();
  for (const { path, role } of files) {
    const iri = namedNode(`http://example.org/unrdf/fs#${encodeURIComponent(path)}`);
    store.addQuad(iri, namedNode(`${NS.fs}relativePath`), literal(path));
    if (role) store.addQuad(iri, namedNode(`${NS.proj}roleString`), literal(role));
  }
  return store;
}

describe('gap-finder', () => {
  describe('findMissingRoles', () => {
    it('reports missing roles for entity', () => {
      const domainStore = createDomainStore([{ name: 'User', fields: ['id', 'email'] }]);
      const projectStore = createProjectStore([
        { path: 'src/components/User.tsx', role: 'Component' },
      ]);

      const result = findMissingRoles({ domainStore, projectStore });

      expect(result.gaps).toContainEqual(
        expect.objectContaining({
          entity: 'User',
          missingRoles: expect.arrayContaining(['Api']),
        })
      );
    });

    it('returns no gaps when all roles present', () => {
      const domainStore = createDomainStore([{ name: 'User', fields: ['id'] }]);
      const projectStore = createProjectStore([
        { path: 'src/api/user.ts', role: 'Api' },
        { path: 'src/components/UserView.tsx', role: 'Component' },
        { path: 'test/user.test.ts', role: 'Test' },
        { path: 'src/types/user.ts', role: 'Schema' },
      ]);

      const result = findMissingRoles({ domainStore, projectStore });
      const userGaps = result.gaps.find(g => g.entity === 'User');
      expect(userGaps.missingRoles).toHaveLength(0);
    });

    it('prioritizes by framework', () => {
      const domainStore = createDomainStore([{ name: 'Product', fields: ['id'] }]);
      const projectStore = createProjectStore([{ path: 'src/api/product.ts', role: 'Api' }]);

      const result = findMissingRoles({
        domainStore,
        projectStore,
        stackProfile: { webFramework: 'next' },
      });
      const productGaps = result.gaps.find(g => g.entity === 'Product');
      expect(productGaps.missingRoles).toContain('Page');
    });

    it('matches files by name pattern variations', () => {
      const domainStore = createDomainStore([{ name: 'User', fields: ['id'] }]);
      const projectStore = createProjectStore([
        { path: 'src/api/users.ts', role: 'Api' },
        { path: 'src/components/user-profile.tsx', role: 'Component' },
      ]);

      const result = findMissingRoles({ domainStore, projectStore });
      const userGaps = result.gaps.find(g => g.entity === 'User');
      expect(userGaps.missingRoles).not.toContain('Api');
      expect(userGaps.missingRoles).not.toContain('Component');
    });
  });

  describe('scoreMissingRole', () => {
    it('scores roles by priority', () => {
      expect(scoreMissingRole('User', 'Api', {})).toBe(95);
      expect(scoreMissingRole('User', 'Test', {})).toBe(90);
      expect(scoreMissingRole('User', 'Component', {})).toBe(80);
      expect(scoreMissingRole('User', 'Schema', {})).toBe(70);
      expect(scoreMissingRole('User', 'Doc', {})).toBe(50);
    });

    it('boosts framework-specific roles', () => {
      const baseScore = scoreMissingRole('Product', 'Page', {});
      const nextScore = scoreMissingRole('Product', 'Page', {
        webFramework: 'next',
      });
      expect(nextScore).toBeGreaterThan(baseScore);
    });
  });
});
