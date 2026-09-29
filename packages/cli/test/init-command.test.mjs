/**
 * @file Regression: `unrdf init` must be wired into main.mjs and loadable.
 * init.mjs used to statically import createProjectInitializationPipeline, which
 * @unrdf/project-engine does not export, so importing the command threw a link error.
 */
import { describe, it, expect } from 'vitest';
import { readFileSync } from 'node:fs';

describe('init command wiring', () => {
  it('init.mjs imports without a static project-engine dependency', async () => {
    const mod = await import('../src/cli/commands/init.mjs');
    expect(mod.initCommand).toBeDefined();
    expect(mod.initCommand.meta.name).toBe('init');
    expect(Object.keys(mod.initCommand.args)).toEqual(
      expect.arrayContaining(['root', 'dry-run', 'verbose'])
    );
    const src = readFileSync(new URL('../src/cli/commands/init.mjs', import.meta.url), 'utf8');
    expect(src).not.toMatch(/^import .* from '@unrdf\/project-engine'/m);
  });

  it('main.mjs registers the init subcommand', () => {
    const src = readFileSync(new URL('../src/cli/main.mjs', import.meta.url), 'utf8');
    expect(src).toMatch(/import \{ initCommand \} from '\.\/commands\/init\.mjs'/);
    expect(src).toMatch(/init:\s*initCommand/);
  });
});
