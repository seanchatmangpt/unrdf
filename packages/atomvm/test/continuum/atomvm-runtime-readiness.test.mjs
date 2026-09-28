/**
 * Chicago-school readiness suite: the AtomVM pieces every tier stands on,
 * exercised for real (real wasm binary, real child processes, real files).
 * No mocks; assertions are on observable state and output.
 */
import { describe, it, expect } from 'vitest';
import { existsSync, readFileSync, writeFileSync, mkdtempSync } from 'node:fs';
import { spawnSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { REQUIRED_ASSET_NAMES, ATOMVM_VERSION, atomvmAssetName } from '../../src/assets.mjs';
import { packAvm, parseAvm, AvmFormatError, AVM_HEADER } from '../../src/avm-packer.mjs';
import { AtomVMNodeRuntime } from '../../src/node-runtime.mjs';
import { AtomVMProcessBroker, AtomVMProcessRefusal } from '../../src/process-broker.mjs';
import { PACKAGE_ROOT, programAvm, tierAvm } from './helpers.mjs';

const LAUNCHER = join(PACKAGE_ROOT, 'bin/atomvm-wasm.mjs');
const publicFile = name => join(PACKAGE_ROOT, 'public', name);
const quiet = { log: () => {}, errorLog: () => {} };

describe('shipped WASM assets', () => {
  it('ships every required asset under a name derived from one source of truth', () => {
    expect(ATOMVM_VERSION).not.toMatch(/[[\]{}]/); // never an unsubstituted template placeholder
    for (const name of REQUIRED_ASSET_NAMES) {
      expect(existsSync(publicFile(name)), `${name} missing from public/`).toBe(true);
    }
  });

  it('ships real WebAssembly binaries, not error pages saved under the asset name', () => {
    for (const variant of ['web', 'node']) {
      const bytes = readFileSync(publicFile(atomvmAssetName(variant, 'wasm')));
      expect([...bytes.subarray(0, 4)]).toEqual([0x00, 0x61, 0x73, 0x6d]);
      expect(WebAssembly.validate(bytes)).toBe(true);
    }
  });

  it('ships a hello_world.avm that is a structurally valid AVM archive', () => {
    const parsed = parseAvm(new Uint8Array(readFileSync(publicFile('hello_world.avm'))));
    expect(parsed.startModule).toBe('hello_world.beam');
  });
});

describe('AVM archive format', () => {
  const beam = new Uint8Array([0x46, 0x4f, 0x52, 0x31, 0, 0, 0, 4, 1, 2, 3, 4]); // FOR1 + minimal body

  it('round-trips: what is packed is what is parsed', () => {
    const parsed = parseAvm(
      packAvm([
        { name: 'main.beam', data: beam, start: true },
        { name: 'lib.beam', data: beam },
      ])
    );
    expect(parsed.entries.map(entry => entry.name)).toEqual(['main.beam', 'lib.beam']);
    expect(parsed.startModule).toBe('main.beam');
  });

  it.each([
    ['an HTTP 404 body saved as .avm', new TextEncoder().encode('404: Not Found')],
    ['an empty file', new Uint8Array(0)],
    ['the right header but truncated', AVM_HEADER],
    ['random bytes', Uint8Array.from({ length: 64 }, (_, i) => (i * 37) & 0xff)],
  ])('refuses %s', (_label, bytes) => {
    expect(() => parseAvm(bytes)).toThrow(AvmFormatError);
  });

  it('refuses an archive with no start module and one whose start module is not BEAM', () => {
    expect(() => parseAvm(packAvm([{ name: 'a.beam', data: beam }]))).toThrow(
      /exactly one start module/
    );
    expect(() =>
      packAvm([{ name: 'a.beam', data: new TextEncoder().encode('not beam'), start: true }])
    ).toThrow(/FOR1/);
  });
});

describe('bin/atomvm-wasm launcher (real AtomVM on WebAssembly)', () => {
  const run = (...args) =>
    spawnSync(process.execPath, [LAUNCHER, ...args], { encoding: 'utf8', timeout: 15_000 });

  it('reports itself and exits 0 for -v', () => {
    const result = run('-v');
    expect(result.status).toBe(0);
    expect(result.stdout).toContain('AtomVM');
  });

  it('runs the shipped hello_world.avm to completion', () => {
    const result = run(publicFile('hello_world.avm'));
    expect(result.status).toBe(0);
    expect(result.stdout).toContain('{atomvm_module_alive,hello_world}');
    expect(result.stdout).toContain('Return value: ok');
  });

  it('computes correctly: the tier witness Adler-32 equals an independent JS computation', () => {
    let a = 1;
    let b = 0;
    for (const byte of Buffer.from('unrdf-atomvm-continuum')) {
      a = (a + byte) % 65521;
      b = (b + a) % 65521;
    }
    const result = run(tierAvm('fog'));
    expect(result.status).toBe(0);
    expect(result.stdout).toContain(`{atomvm_tier_alive,fog,2,22,${a},${b}}`);
  });

  it('surfaces a crashing program as a non-zero exit with a crash report, never as success', () => {
    const result = run(programAvm('crash_now'));
    expect(result.status).toBe(1);
    expect(result.stderr).toContain('CRASH');
    expect(result.stdout).toContain('Return value: error');
    expect(result.stdout).not.toContain('Return value: ok');
  });

  it('refuses malformed and missing applications with exit 2 and a reason', () => {
    const dir = mkdtempSync(join(tmpdir(), 'avm-bad-'));
    const fake = join(dir, 'fake.avm');
    writeFileSync(fake, '404: Not Found');
    const malformed = run(fake);
    expect(malformed.status).toBe(2);
    expect(malformed.stderr).toContain('AVM_FORMAT_REFUSED');

    const missing = run(join(dir, 'absent.avm'));
    expect(missing.status).toBe(2);
    expect(missing.stderr).toContain('cannot read');
    expect(run().status).toBe(2);
  });
});

describe('AtomVMNodeRuntime', () => {
  it('boots without a native install by falling back to the bundled wasm launcher', async () => {
    const runtime = new AtomVMNodeRuntime(quiet);
    await runtime.load();
    expect(runtime.isReady()).toBe(true);
    expect(['native', 'wasm-node']).toContain(runtime.backend);

    const result = await runtime.execute(publicFile('hello_world.avm'));
    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('atomvm_module_alive');
    expect(runtime.state).toBe('Ready'); // returns to Ready, can run again
    expect((await runtime.execute(tierAvm('edge'))).stdout).toContain('atomvm_tier_alive,edge');
    runtime.destroy();
    expect(runtime.state).toBe('Destroyed');
  });

  it('never silently substitutes a binary the caller explicitly asked for', async () => {
    const runtime = new AtomVMNodeRuntime({ ...quiet, atomvmBinary: '/nonexistent/AtomVM' });
    await expect(runtime.load()).rejects.toThrow('ATOMVM_BINARY_NOT_FOUND_REFUSED');
    expect(runtime.state).toBe('Error');
  });

  it('rejects a crashing program and leaves the runtime in Error, not Ready', async () => {
    const runtime = new AtomVMNodeRuntime(quiet);
    await runtime.load();
    await expect(runtime.execute(programAvm('crash_now'))).rejects.toThrow('ATOMVM_EXIT_BLOCKED');
    expect(runtime.state).toBe('Error');
  });
});

describe('AtomVMProcessBroker safety net', () => {
  const broker = timeoutMs =>
    new AtomVMProcessBroker({
      atomvmBinary: LAUNCHER,
      timeoutMs,
      swarms: {
        runaway: { avmPath: programAvm('loop_forever') },
        witness: { avmPath: tierAvm('cloud'), expectedMarker: 'atomvm_tier_alive,cloud' },
        wrong: { avmPath: tierAvm('cloud'), expectedMarker: 'atomvm_tier_alive,edge' },
      },
    });
  const call = (b, id) =>
    b.execute({ intent: { operation: 'atomvm.execute' }, target: { id }, route: [id] });

  it('kills a runaway program at the deadline instead of hanging the node', async () => {
    const started = Date.now();
    await expect(call(broker(1500), 'runaway')).rejects.toMatchObject({
      code: 'ATOMVM_TIMEOUT_REFUSED',
    });
    expect(Date.now() - started).toBeLessThan(6000);
  });

  it('accepts a program only when its required marker is actually observed', async () => {
    const ok = await call(broker(15_000), 'witness');
    expect(ok.markerObserved).toBe(true);
    expect(ok.exitCode).toBe(0);
    await expect(call(broker(15_000), 'wrong')).rejects.toBeInstanceOf(AtomVMProcessRefusal);
    await expect(call(broker(15_000), 'wrong')).rejects.toMatchObject({
      code: 'ATOMVM_MARKER_MISSING_REFUSED',
    });
  });
});
