import { describe, it, expect } from 'vitest';
import sidecar, { SidecarClient, createSidecarClient } from '../../sidecar/index.mjs';

describe('sidecar gRPC client module', () => {
  it('index default export exposes the client factory', () => {
    expect(sidecar.createSidecarClient).toBe(createSidecarClient);
    expect(createSidecarClient()).toBeInstanceOf(SidecarClient);
  });

  it('connect() finds the shipped proto by its default path (gRPC channels connect lazily)', async () => {
    const client = createSidecarClient({ enableHealthCheck: false });
    await client.connect('127.0.0.1:1');
    try {
      expect(client.connected).toBe(true);
      expect(typeof client.client.constructor).toBe('function');
    } finally {
      await client.disconnect?.();
    }
  });
});
