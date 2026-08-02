import { createServer } from 'node:http';
import { webcrypto } from 'node:crypto';
import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { AtomVMNodeRuntime } from '../src/node-runtime.mjs';
import { HotCodeLoader } from '../src/hot-code-loader.mjs';

let server;
let baseUrl;
let moduleBytes;

beforeAll(async () => {
  if (!globalThis.crypto?.subtle) globalThis.crypto = webcrypto;
  server = createServer((request, response) => {
    if (request.url !== '/module.avm') {
      response.writeHead(404).end();
      return;
    }
    response.writeHead(200, { 'content-type': 'application/octet-stream' });
    response.end(moduleBytes);
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const address = server.address();
  baseUrl = `http://127.0.0.1:${address.port}`;
});

afterAll(async () => {
  await new Promise((resolve, reject) => server.close(error => error ? reject(error) : resolve()));
});

beforeEach(() => {
  moduleBytes = Buffer.from('atomvm-module-version-1');
});

function createLoader() {
  const runtime = new AtomVMNodeRuntime({ atomvmBinary: process.env.ATOMVM_BIN ?? process.execPath });
  return new HotCodeLoader(runtime);
}

describe('HotCodeLoader — real bytes, HTTP, crypto, and state', () => {
  it('loads bytes over a real HTTP boundary and records their SHA-256 identity', async () => {
    const loader = createLoader();
    const result = await loader.loadModule(`${baseUrl}/module.avm`);

    expect(result.success).toBe(true);
    expect(result.moduleName).toBe('module');
    expect(result.signature).toMatch(/^[0-9a-f]{64}$/);
    expect(loader.getActiveModules()[0].version).toBe(1);
  });

  it('reloads changed bytes and exposes lifecycle events as state', async () => {
    const loader = createLoader();
    const lifecycle = [];
    await loader.loadModule(`${baseUrl}/module.avm`);
    const firstSignature = loader.getActiveModules()[0].signature;

    loader.registerHotSwap('module', {
      beforeSwap: event => lifecycle.push({ phase: 'before', moduleName: event.moduleName }),
      afterSwap: event => lifecycle.push({ phase: 'after', version: event.version }),
      onError: event => lifecycle.push({ phase: 'error', message: event.error.message }),
    });
    moduleBytes = Buffer.from('atomvm-module-version-2');
    const result = await loader.reloadModule('module');

    expect(result.success).toBe(true);
    expect(result.version).toBe(2);
    expect(loader.getActiveModules()[0].signature).not.toBe(firstSignature);
    expect(lifecycle).toEqual([
      { phase: 'before', moduleName: 'module' },
      { phase: 'after', version: 2 },
    ]);
  });

  it('returns bounded failure for a missing module and unloads admitted state', async () => {
    const loader = createLoader();
    const missing = await loader.loadModule(`${baseUrl}/missing.avm`);
    expect(missing.success).toBe(false);

    await loader.loadModule(`${baseUrl}/module.avm`);
    expect(loader.unloadModule('module')).toBe(true);
    expect(loader.getActiveModules()).toEqual([]);
  });
});
