/** @vitest-environment jsdom */
import { describe, it, expect, afterEach } from 'vitest';
import {
  registerServiceWorker,
  checkCrossOriginIsolation,
  getCOIStatus,
} from '../src/service-worker-manager.mjs';

const originalCrossOriginIsolated = globalThis.crossOriginIsolated;
const originalSharedArrayBuffer = globalThis.SharedArrayBuffer;

afterEach(() => {
  if (originalCrossOriginIsolated === undefined) delete globalThis.crossOriginIsolated;
  else globalThis.crossOriginIsolated = originalCrossOriginIsolated;
  if (originalSharedArrayBuffer === undefined) delete globalThis.SharedArrayBuffer;
  else globalThis.SharedArrayBuffer = originalSharedArrayBuffer;
});

describe('Service Worker Manager — real environment state', () => {
  it('reports cross-origin isolation from the actual global flag', () => {
    globalThis.crossOriginIsolated = true;
    expect(checkCrossOriginIsolation()).toBe(true);
    expect(getCOIStatus().crossOriginIsolated).toBe(true);
  });

  it('falls back to actual SharedArrayBuffer availability', () => {
    delete globalThis.crossOriginIsolated;
    globalThis.SharedArrayBuffer = class SharedArrayBuffer {};
    expect(checkCrossOriginIsolation()).toBe(true);
    expect(getCOIStatus().sharedArrayBufferAvailable).toBe(true);
  });

  it('reports an unsupported service-worker environment without manufacturing registration', async () => {
    expect('serviceWorker' in navigator).toBe(false);
    expect(await registerServiceWorker()).toBe(false);
    expect(getCOIStatus().serviceWorkerSupported).toBe(false);
  });
});
