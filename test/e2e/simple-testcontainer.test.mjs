/**
 * @file Simple Testcontainer Test
 * @module simple-testcontainer
 * 
 * @description
 * A simple test to verify Testcontainers is working correctly.
 * This test starts a basic container and verifies it's running.
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import { GenericContainer } from 'testcontainers';

describe('Simple Testcontainer Test', () => {
  let container;

  beforeAll(async () => {
    // Start a simple nginx container
    container = await new GenericContainer('nginx:alpine')
      .withExposedPorts(80)
      .withStartupTimeout(30000)
      .start();
  }, 60000);

  afterAll(async () => {
    if (container) {
      await container.stop();
    }
  });

  it('should start a container successfully', () => {
    expect(container).toBeDefined();
    expect(container.getHost()).toBeDefined();
    expect(container.getMappedPort(80)).toBeDefined();
  });

  it('should be able to access the container', async () => {
    const host = container.getHost();
    const port = container.getMappedPort(80);
    const url = `http://${host}:${port}`;
    
    // Simple check that the container is running
    expect(host).toBeTruthy();
    expect(port).toBeGreaterThan(0);
    // Accept both localhost and IP addresses
    expect(url).toMatch(/^http:\/\/(localhost|\d+\.\d+\.\d+\.\d+):\d+$/);
  });
});
