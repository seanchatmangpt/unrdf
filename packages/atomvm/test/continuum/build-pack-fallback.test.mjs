/**
 * scripts/build.mjs and scripts/pack-beam.mjs must produce a runnable .avm on a machine
 * without PackBEAM by falling back to the JS packer. Real packer, real AtomVM (wasm
 * launcher), no mocks and no compiler needed: the beam is a committed fixture.
 */
import { describe, it, expect } from 'vitest';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { resolvePacker, packBeamFile } from '../../scripts/pack-beam.mjs';
import { parseAvm } from '../../src/avm-packer.mjs';
import { PACKAGE_ROOT } from './helpers.mjs';

const LAUNCHER = join(PACKAGE_ROOT, 'bin/atomvm-wasm.mjs');
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

describe('packing without PackBEAM', () => {
  it('packs a precompiled beam with the JS packer and the result runs on AtomVM', () => {
    const dir = mkdtempSync(join(tmpdir(), 'pack-fallback-'));
    const avmFile = join(dir, 'fallback_probe.avm');
    const empty = mkdtempSync(join(tmpdir(), 'empty-path-'));

    const packed = packBeamFile(avmFile, join(PACKAGE_ROOT, 'test/fixtures/beams/fallback_probe.beam'), {
      PATH: empty, // no PackBEAM reachable
    });

    expect(packed.kind).toBe('js');
    const parsed = parseAvm(new Uint8Array(readFileSync(avmFile)));
    expect(parsed.startModule).toBe('fallback_probe.beam');
    const run = spawnSync(process.execPath, [LAUNCHER, avmFile], { encoding: 'utf8', timeout: 15_000 });
    expect(run.status).toBe(0);
    expect(run.stdout).toContain('{atomvm_module_alive,fallback_probe}');
    expect(run.stdout).toContain('Return value: ok');
  });
});
