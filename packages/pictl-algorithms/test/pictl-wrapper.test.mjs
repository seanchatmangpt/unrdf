/**
 * @file Behavioural tests for the pictl WASM bridge.
 *
 * The wrapper keeps module-level caches, so every test re-imports a fresh copy via
 * vi.resetModules(). The "pictl" implementation under test is a real ES module written
 * to a temp dir and loaded through the wrapper's own wasmPath fallback.
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach, afterEach } from 'vitest';
import { vi } from 'vitest';
import { mkdtempSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

let dir;

function writeModule(name, source) {
  const file = join(dir, name);
  writeFileSync(file, source);
  return file;
}

async function freshWrapper() {
  vi.resetModules();
  return import('../src/pictl-wrapper.mjs');
}

beforeAll(() => {
  dir = mkdtempSync(join(tmpdir(), 'pictl-wrapper-'));
});

afterAll(() => {
  rmSync(dir, { recursive: true, force: true });
});

afterEach(() => {
  delete globalThis.otel;
});

describe('loadPictlWasm', () => {
  it('fails with a wrapped error when the pictl package is absent and no wasmPath is given', async () => {
    const { loadPictlWasm } = await freshWrapper();
    await expect(loadPictlWasm()).rejects.toThrow(/^Failed to load pictl WASM: /);
  });

  it('falls back to wasmPath and passes the profile to the custom loader', async () => {
    const file = writeModule(
      'custom-loader.mjs',
      `export async function loadPictlWasm(opts) { return { loadedWith: opts, marker: 'custom' }; }`
    );
    const { loadPictlWasm } = await freshWrapper();
    const mod = await loadPictlWasm({ wasmPath: file, profile: 'edge' });
    expect(mod).toEqual({ loadedWith: { profile: 'edge' }, marker: 'custom' });
  });

  it("defaults the profile to 'cloud'", async () => {
    const file = writeModule(
      'default-profile.mjs',
      `export async function loadPictlWasm(opts) { return { profile: opts.profile }; }`
    );
    const { loadPictlWasm } = await freshWrapper();
    expect((await loadPictlWasm({ wasmPath: file })).profile).toBe('cloud');
  });

  it('caches the loaded module: a second call ignores different options', async () => {
    const file = writeModule(
      'counting-loader.mjs',
      `let n = 0; export async function loadPictlWasm() { n += 1; return { n }; }`
    );
    const { loadPictlWasm } = await freshWrapper();
    const first = await loadPictlWasm({ wasmPath: file });
    const second = await loadPictlWasm({ wasmPath: '/does/not/exist.mjs' });
    expect(second).toBe(first);
    expect(first.n).toBe(1);
  });

  it('does not cache failures: a later call with a valid wasmPath succeeds', async () => {
    const good = writeModule('recover.mjs', `export async function loadPictlWasm() { return { ok: true }; }`);
    const { loadPictlWasm } = await freshWrapper();
    await expect(loadPictlWasm({ wasmPath: join(dir, 'missing.mjs') })).rejects.toThrow(
      /Failed to load pictl WASM/
    );
    await expect(loadPictlWasm({ wasmPath: good })).resolves.toEqual({ ok: true });
  });

  it('accepts a relative wasmPath (resolved against the cwd)', async () => {
    const file = writeModule('relative.mjs', `export async function loadPictlWasm() { return { rel: 1 }; }`);
    const { relative } = await import('node:path');
    const { loadPictlWasm } = await freshWrapper();
    await expect(loadPictlWasm({ wasmPath: relative(process.cwd(), file) })).resolves.toEqual({ rel: 1 });
  });
});

describe('getKernel', () => {
  const kernelLoader = (body) =>
    `export async function loadPictlWasm() { return ${body}; }`;

  it('creates a kernel through PictlKernel.create()', async () => {
    const file = writeModule(
      'kernel-class.mjs',
      kernelLoader(`{ PictlKernel: { create: async () => ({ kind: 'class-kernel' }) } }`)
    );
    const { getKernel } = await freshWrapper();
    expect(await getKernel({ wasmPath: file })).toEqual({ kind: 'class-kernel' });
  });

  it('supports the default-export shape and the direct create() shape', async () => {
    const viaDefault = writeModule(
      'kernel-default.mjs',
      kernelLoader(`{ default: { PictlKernel: { create: async () => ({ kind: 'default' }) } } }`)
    );
    let w = await freshWrapper();
    expect(await w.getKernel({ wasmPath: viaDefault })).toEqual({ kind: 'default' });

    const viaCreate = writeModule('kernel-create.mjs', kernelLoader(`{ create: async () => ({ kind: 'direct' }) }`));
    w = await freshWrapper();
    expect(await w.getKernel({ wasmPath: viaCreate })).toEqual({ kind: 'direct' });
  });

  it('memoises the kernel instance', async () => {
    const file = writeModule(
      'kernel-once.mjs',
      `let n = 0; export async function loadPictlWasm() { return { PictlKernel: { create: async () => ({ id: ++n }) } }; }`
    );
    const { getKernel } = await freshWrapper();
    const a = await getKernel({ wasmPath: file });
    const b = await getKernel();
    expect(b).toBe(a);
    expect(a.id).toBe(1);
  });

  it('rejects when the module exposes no kernel constructor', async () => {
    const file = writeModule('kernel-none.mjs', kernelLoader(`{ nothing: true }`));
    const { getKernel } = await freshWrapper();
    await expect(getKernel({ wasmPath: file })).rejects.toThrow(
      /Failed to create pictl kernel: Cannot find pictl kernel constructor/
    );
  });

  it('wraps kernel creation failures', async () => {
    const file = writeModule(
      'kernel-throws.mjs',
      kernelLoader(`{ PictlKernel: { create: async () => { throw new Error('wasm trap'); } } }`)
    );
    const { getKernel } = await freshWrapper();
    await expect(getKernel({ wasmPath: file })).rejects.toThrow(
      'Failed to create pictl kernel: wasm trap'
    );
  });
});

describe('resetKernel', () => {
  it('calls softReset and keeps the kernel when supported', async () => {
    const file = writeModule(
      'kernel-soft.mjs',
      `export async function loadPictlWasm() { return { PictlKernel: { create: async () => ({ resets: 0, async softReset() { this.resets += 1; } }) } }; }`
    );
    const { getKernel, resetKernel } = await freshWrapper();
    const kernel = await getKernel({ wasmPath: file });
    await resetKernel();
    expect(kernel.resets).toBe(1);
    expect(await getKernel()).toBe(kernel);
  });

  it('drops the kernel when softReset is unavailable so the next call recreates it', async () => {
    const file = writeModule(
      'kernel-hard.mjs',
      `let n = 0; export async function loadPictlWasm() { return { PictlKernel: { create: async () => ({ id: ++n }) } }; }`
    );
    const { getKernel, resetKernel } = await freshWrapper();
    const first = await getKernel({ wasmPath: file });
    await resetKernel();
    const second = await getKernel();
    expect(second).not.toBe(first);
    expect(second.id).toBe(2);
  });

  it('is a no-op before any kernel exists', async () => {
    const { resetKernel } = await freshWrapper();
    await expect(resetKernel()).resolves.toBeUndefined();
  });
});

describe('getKernelInfo', () => {
  it('reports version, algorithm names and capabilities from the kernel', async () => {
    const file = writeModule(
      'kernel-info.mjs',
      `export async function loadPictlWasm() { return { PictlKernel: { create: async () => ({
        version: () => '2.1.0',
        algorithms: () => ({ dfg: {}, alpha: {} }),
        getCapabilities: () => ({ prediction: true }),
      }) } }; }`
    );
    const { getKernelInfo } = await freshWrapper();
    expect(await getKernelInfo({ wasmPath: file })).toEqual({
      version: '2.1.0',
      algorithms: ['dfg', 'alpha'],
      capabilities: { prediction: true },
    });
  });

  it('falls back to defaults for kernels without introspection', async () => {
    const file = writeModule(
      'kernel-bare.mjs',
      `export async function loadPictlWasm() { return { PictlKernel: { create: async () => ({}) } }; }`
    );
    const { getKernelInfo } = await freshWrapper();
    expect(await getKernelInfo({ wasmPath: file })).toEqual({
      version: 'unknown',
      algorithms: [],
      capabilities: { discovery: true, conformance: true, prediction: false },
    });
  });

  it('tolerates a versioned kernel lacking algorithms/getCapabilities', async () => {
    const file = writeModule(
      'kernel-min.mjs',
      `export async function loadPictlWasm() { return { PictlKernel: { create: async () => ({ version: () => '0.1' }) } }; }`
    );
    const { getKernelInfo } = await freshWrapper();
    expect(await getKernelInfo({ wasmPath: file })).toEqual({
      version: '0.1',
      algorithms: [],
      capabilities: {},
    });
  });
});

describe('telemetry seam (globalThis.otel)', () => {
  let recorded;

  beforeEach(() => {
    recorded = [];
    globalThis.otel = {
      trace: {
        getTracer: name => ({
          startSpan: (spanName, opts) => {
            const span = { name: spanName, tracer: name, attributes: opts.attributes, events: [] };
            recorded.push(span);
            return {
              recordException: e => span.events.push(['exception', e]),
              setStatus: s => (span.status = s),
              setAttributes: a => Object.assign(span.attributes, a),
              end: () => (span.ended = true),
            };
          },
        }),
      },
    };
  });

  it('records an ok span for a successful load', async () => {
    const file = writeModule('otel-ok.mjs', `export async function loadPictlWasm() { return { ok: 1 }; }`);
    const { loadPictlWasm } = await freshWrapper();
    await loadPictlWasm({ wasmPath: file, profile: 'iot' });

    expect(recorded).toHaveLength(1);
    expect(recorded[0]).toMatchObject({
      name: 'pictl.load_wasm',
      tracer: '@unrdf/pictl-algorithms',
      ended: true,
      attributes: { profile: 'iot', status: 'ok' },
    });
    expect(recorded[0].status).toBeUndefined();
  });

  it('records an ERROR-status span with the exception for a failed load', async () => {
    const { loadPictlWasm } = await freshWrapper();
    await expect(loadPictlWasm({ wasmPath: join(dir, 'nope.mjs') })).rejects.toThrow();

    expect(recorded).toHaveLength(1);
    expect(recorded[0].ended).toBe(true);
    expect(recorded[0].status.code).toBe(2);
    expect(recorded[0].events[0][0]).toBe('exception');
  });
});

describe('package entrypoint', () => {
  it('re-exports the bridge API', async () => {
    const entry = await import('../src/index.mjs');
    expect(Object.keys(entry).sort()).toEqual(['getKernel', 'getKernelInfo', 'loadPictlWasm', 'resetKernel']);
  });
});
