/**
 * scripts/build.mjs must produce a runnable .avm on a machine that has erlc but
 * no PackBEAM, by falling back to the JS packer. Real erlc, real packer, real
 * AtomVM (wasm launcher) - no mocks. Skipped when erlc is not installed.
 */
import { describe, it, expect, afterEach } from 'vitest';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { buildModule } from '../../scripts/build.mjs';
import { resolvePacker, packBeamFile } from '../../scripts/pack-beam.mjs';
import { parseAvm } from '../../src/avm-packer.mjs';
import { PACKAGE_ROOT } from './helpers.mjs';

const erlc = spawnSync('sh', ['-c', 'command -v erlc'], { encoding: 'utf8' }).stdout.trim();
const LAUNCHER = join(PACKAGE_ROOT, 'bin/atomvm-wasm.mjs');
const saved = { PATH: process.env.PATH, PACKBEAM_BIN: process.env.PACKBEAM_BIN, ERLC_BIN: process.env.ERLC_BIN };

afterEach(() => {
  for (const [key, value] of Object.entries(saved)) {
    if (value === undefined) delete process.env[key];
    else process.env[key] = value;
  }
});

describe('resolvePacker', () => {
  it('falls back to the JS packer when PackBEAM is not on PATH', () => {
    const empty = mkdtempSync(join(tmpdir(), 'no-packbeam-'));
    expect(resolvePacker({ PATH: empty })).toEqual({ kind: 'js' });
  });

  it('honours an explicit PACKBEAM_BIN without silently falling back', () => {
    expect(resolvePacker({ PACKBEAM_BIN: '/x/PackBEAM', PATH: '' })).toEqual({
      kind: 'native',
      binary: '/x/PackBEAM',
    });
    const dir = mkdtempSync(join(tmpdir(), 'pb-'));
    const beam = join(dir, 'a.beam');
    writeFileSync(beam, Buffer.from('FOR1\0\0\0\0'));
    expect(() => packBeamFile(join(dir, 'a.avm'), beam, { PACKBEAM_BIN: '/nonexistent/PackBEAM' })).toThrow(
      /PackBEAM failed/
    );
  });
});

describe.skipIf(!erlc)('scripts/build.mjs without PackBEAM', () => {
  it('compiles with erlc, packs with the JS packer, and the result runs on AtomVM', async () => {
    const dir = mkdtempSync(join(tmpdir(), 'build-fallback-'));
    const srcDir = join(dir, 'erl');
    const publicDir = join(dir, 'out');
    mkdirSync(srcDir);
    const empty = mkdtempSync(join(tmpdir(), 'empty-path-'));
    process.env.ERLC_BIN = erlc; // absolute, so an empty PATH still finds it
    delete process.env.PACKBEAM_BIN;
    process.env.PATH = empty;

    const built = await buildModule('fallback_probe', { srcDir, publicDir });
    process.env.PATH = saved.PATH;

    expect(built.packer).toBe('js');
    const parsed = parseAvm(new Uint8Array(readFileSync(built.avmFile)));
    expect(parsed.startModule).toBe('fallback_probe.beam');
    const run = spawnSync(process.execPath, [LAUNCHER, built.avmFile], { encoding: 'utf8', timeout: 15_000 });
    expect(run.status).toBe(0);
    expect(run.stdout).toContain('{atomvm_module_alive,fallback_probe}');
    expect(run.stdout).toContain('Return value: ok');
  });
});
