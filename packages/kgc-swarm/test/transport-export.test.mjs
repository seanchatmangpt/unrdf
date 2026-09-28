/**
 * @file Regression: '@unrdf/kgc-swarm/transport' must be a declared subpath export.
 * packages/cli/src/cli/commands/publish.mjs imports createHypercoreTransport from it;
 * without the exports entry Node throws ERR_PACKAGE_PATH_NOT_EXPORTED.
 */
import { describe, it, expect } from 'vitest';
import { readFileSync } from 'node:fs';

describe('@unrdf/kgc-swarm/transport', () => {
  it('is declared in package.json exports and points at an existing module', async () => {
    const pkg = JSON.parse(
      readFileSync(new URL('../package.json', import.meta.url), 'utf8')
    );
    expect(pkg.exports['./transport']).toBe('./src/transport/hypercore-transport.mjs');
  });

  it('resolves via the package self-reference (as external importers do)', async () => {
    const mod = await import('@unrdf/kgc-swarm/transport');
    expect(typeof mod.createHypercoreTransport).toBe('function');
    const t = mod.createHypercoreTransport({ name: 'test' });
    expect(t).toBeInstanceOf(mod.HypercoreTransport);
    expect(t.name).toBe('test');
  });
});
