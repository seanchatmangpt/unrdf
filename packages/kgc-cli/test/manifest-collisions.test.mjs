/**
 * @fileoverview Collision-resolution tests for the manifest-driven registry.
 *
 * Regression: seven extensions (federation, core, validation, claude, deploy,
 * graphql, substrate) failed to load under failOnCollision=true because the
 * manifest overrides were never applied (format mismatch package/winner, and
 * cli.mjs never passed them) and a failing extension left partial ownership.
 */
import { describe, it, expect } from 'vitest';
import { Registry } from '../src/lib/registry.mjs';
import { extensions, overrides, loadManifest } from '../src/manifest/extensions.mjs';
import { buildCittyTree, initializeRegistry } from '../src/cli.mjs';

const ext = (id, nouns) => ({ id, nouns });
const verb = description => ({ description, handler: async () => description });

describe('manifest collision resolution', () => {
  it('loads every enabled extension with failOnCollision=true', async () => {
    const registry = new Registry({ failOnCollision: true });
    await loadManifest(registry, { failOnMissing: true });
    // (docs.mjs registers under its own id @unrdf/diataxis-kit, so compare counts)
    expect(registry.extensions.size).toBe(extensions.filter(e => e.enabled).length);
    expect(Object.keys(registry.getCollisionSummary())).toEqual([]);
  });

  it('the seven previously failing extensions register', async () => {
    const registry = new Registry({ failOnCollision: true });
    await loadManifest(registry, { failOnMissing: true });
    for (const name of ['federation', 'core', 'validation', 'claude', 'deploy', 'graphql', 'substrate']) {
      expect(registry.extensions.has(`@unrdf/${name}`)).toBe(true);
    }
  });

  it('every override names the winner as owner and drops the loser from the tree', async () => {
    const registry = new Registry({ failOnCollision: true });
    await loadManifest(registry, { failOnMissing: true });
    const tree = registry.buildCommandTree();
    for (const o of overrides) {
      const [noun, verbName] = o.rule.split(':');
      expect(registry.getCommandSource(noun, verbName)).toBe(o.package);
      expect(tree.nouns[noun].verbs[verbName]._source).toBe(o.package);
    }
  });

  it('federation query is reachable under its own verb', async () => {
    const registry = new Registry({ failOnCollision: true });
    await loadManifest(registry, { failOnMissing: true });
    expect(registry.getCommandSource('query', 'federated')).toBe('@unrdf/federation');
  });

  it('initializeRegistry (used by `kgc --help`) exposes all nouns from all extensions', async () => {
    const { registry, tree } = await initializeRegistry();
    expect(registry.extensions.size).toBe(extensions.filter(e => e.enabled).length);
    const cmd = buildCittyTree(registry, tree);
    // citty reads `subCommands` (capital C); lowercase is silently ignored
    expect(Object.keys(cmd.subCommands)).toEqual(Object.keys(tree.nouns));
    expect(Object.keys(cmd.subCommands)).toContain('peer');
    expect(Object.keys(cmd.subCommands)).toContain('function');
  });
});

describe('Registry override semantics', () => {
  it('accepts manifest format ({rule, package}) and treats package as winner', () => {
    const r = new Registry({ overrides: [{ rule: 'a:b', package: '@t/two' }] });
    r.registerExtension(ext('@t/one', { a: { verbs: { b: verb('one') } } }), 1);
    r.registerExtension(ext('@t/two', { a: { verbs: { b: verb('two') } } }), 2);
    expect(r.getCommandSource('a', 'b')).toBe('@t/two');
    expect(r.buildCommandTree().nouns.a.verbs.b._source).toBe('@t/two');
  });

  it('later loser does not replace an override winner in the tree', () => {
    const r = new Registry({ overrides: [{ rule: 'a:b', package: '@t/one' }] });
    r.registerExtension(ext('@t/one', { a: { verbs: { b: verb('one') } } }), 1);
    r.registerExtension(ext('@t/two', { a: { verbs: { b: verb('two'), c: verb('c') } } }), 2);
    const tree = r.buildCommandTree();
    expect(tree.nouns.a.verbs.b._source).toBe('@t/one');
    expect(tree.nouns.a.verbs.c._source).toBe('@t/two');
  });

  it('unresolved collision throws and leaves no partial ownership', () => {
    const r = new Registry({ failOnCollision: true });
    r.registerExtension(ext('@t/one', { a: { verbs: { b: verb('one') } } }), 1);
    expect(() =>
      r.registerExtension(
        ext('@t/two', { z: { verbs: { first: verb('z') } }, a: { verbs: { b: verb('two') } } }),
        2
      )
    ).toThrow(/Collision: a:b/);
    expect(r.getCommandSource('z', 'first')).toBeUndefined();
    expect(r.extensions.has('@t/two')).toBe(false);
  });

  it('rejects malformed override rules', () => {
    expect(() => new Registry({ overrides: [{ rule: 'a:b' }] })).toThrow(/Invalid override/);
  });
});
