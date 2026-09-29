/**
 * @file Regression test: OTel SDK initialization resolves resource attribute constants
 */
import { describe, it, expect } from 'vitest';
import { initializeOTelSDK, shutdownOTelSDK } from '../src/integrations/otel-sdk.mjs';

describe('otel-sdk initialization', () => {
  it('initializes and shuts down without ReferenceError', async () => {
    const sdk = await initializeOTelSDK({ serviceName: 'test-svc', version: '1.0.0' });
    expect(sdk).toBeTruthy();
    // Second call returns the same instance
    expect(await initializeOTelSDK()).toBe(sdk);
    await shutdownOTelSDK();
  });
});
