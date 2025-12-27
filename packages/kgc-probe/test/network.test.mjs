/**
 * @fileoverview Tests for Network Probe
 */

import { describe, it, expect } from 'vitest';
import { probeNetwork } from '../src/probes/network.mjs';

describe('Network Probe', () => {
  describe('Guard Enforcement (Poka Yoke)', () => {
    it('should return denied observation when netAllow is empty', async () => {
      const observations = await probeNetwork({ netAllow: [] });

      // Should have at least 2 observations: fetch-api check + denied
      expect(observations.length).toBeGreaterThanOrEqual(2);

      const deniedObs = observations.find(obs => obs.guardDecision === 'denied');
      expect(deniedObs).toBeDefined();
      expect(deniedObs.available).toBe(false);
      expect(deniedObs.reason).toContain('No URLs in allowlist');
    });

    it('should return denied observation when netAllow is undefined', async () => {
      const observations = await probeNetwork({});

      const deniedObs = observations.find(obs => obs.guardDecision === 'denied');
      expect(deniedObs).toBeDefined();
      expect(deniedObs.guardDecision).toBe('denied');
    });

    it('should validate config with Zod', async () => {
      // Invalid URL in netAllow
      await expect(
        probeNetwork({ netAllow: ['not-a-url'] })
      ).rejects.toThrow();

      // Invalid timeout
      await expect(
        probeNetwork({ netAllow: ['https://example.com'], timeout: 10000 })
      ).rejects.toThrow();
    });
  });

  describe('Fetch API Detection', () => {
    it('should detect Fetch API availability', async () => {
      const observations = await probeNetwork({ netAllow: [] });

      const fetchObs = observations.find(obs => obs.capability === 'fetch-api');
      expect(fetchObs).toBeDefined();
      expect(fetchObs.guardDecision).toBe('allowed'); // No network call
      expect(typeof fetchObs.available).toBe('boolean');
      expect(fetchObs.metadata).toBeDefined();
    });
  });

  describe('Allowlisted URL Probing', () => {
    it('should probe allowlisted HTTPS URL', async () => {
      const observations = await probeNetwork({
        netAllow: ['https://example.com'],
        timeout: 5000,
      });

      // Should have multiple observations for the URL
      const urlObservations = observations.filter(obs => obs.url === 'https://example.com');
      expect(urlObservations.length).toBeGreaterThan(0);

      // All should be allowed
      for (const obs of urlObservations) {
        expect(obs.guardDecision).toBe('allowed');
      }

      // Should include HEAD request observation
      const headObs = urlObservations.find(obs => obs.capability === 'http-head-request');
      expect(headObs).toBeDefined();

      // Should include TLS validation (HTTPS)
      const tlsObs = urlObservations.find(obs => obs.capability === 'tls-certificate-validation');
      expect(tlsObs).toBeDefined();
    }, { timeout: 10000 });

    it('should probe HTTP URL without TLS check', async () => {
      const observations = await probeNetwork({
        netAllow: ['http://example.com'],
        timeout: 5000,
      });

      const urlObservations = observations.filter(obs => obs.url === 'http://example.com');
      expect(urlObservations.length).toBeGreaterThan(0);

      // Should NOT include TLS validation for HTTP
      const tlsObs = urlObservations.find(obs => obs.capability === 'tls-certificate-validation');
      expect(tlsObs).toBeUndefined();
    }, { timeout: 10000 });

    it('should record response metadata', async () => {
      const observations = await probeNetwork({
        netAllow: ['https://example.com'],
        timeout: 5000,
      });

      const headObs = observations.find(
        obs => obs.capability === 'http-head-request' && obs.url === 'https://example.com'
      );

      expect(headObs).toBeDefined();
      if (headObs.available) {
        expect(headObs.metadata.statusCode).toBeDefined();
        expect(headObs.metadata.responseTimeMs).toBeGreaterThan(0);
      } else {
        expect(headObs.metadata.error).toBeDefined();
      }
    }, { timeout: 10000 });

    it('should probe cache headers', async () => {
      const observations = await probeNetwork({
        netAllow: ['https://example.com'],
        timeout: 5000,
      });

      const cacheObs = observations.find(
        obs => obs.capability === 'cache-headers' && obs.url === 'https://example.com'
      );

      expect(cacheObs).toBeDefined();
      expect(cacheObs.guardDecision).toBe('allowed');
      if (cacheObs.available) {
        expect(cacheObs.metadata).toHaveProperty('cacheControl');
      }
    }, { timeout: 10000 });

    it('should probe DNS resolution time', async () => {
      const observations = await probeNetwork({
        netAllow: ['https://example.com'],
        timeout: 5000,
      });

      const dnsObs = observations.find(
        obs => obs.capability === 'dns-resolution' && obs.url === 'https://example.com'
      );

      expect(dnsObs).toBeDefined();
      expect(dnsObs.guardDecision).toBe('allowed');
      if (dnsObs.available) {
        expect(dnsObs.metadata.totalRequestTimeMs).toBeGreaterThan(0);
      }
    }, { timeout: 10000 });
  });

  describe('Multiple URLs', () => {
    it('should probe multiple allowlisted URLs', async () => {
      const observations = await probeNetwork({
        netAllow: ['https://example.com', 'https://www.google.com'],
        timeout: 5000,
      });

      const url1Obs = observations.filter(obs => obs.url === 'https://example.com');
      const url2Obs = observations.filter(obs => obs.url === 'https://www.google.com');

      expect(url1Obs.length).toBeGreaterThan(0);
      expect(url2Obs.length).toBeGreaterThan(0);
    }, { timeout: 15000 });
  });

  describe('Timeout Behavior', () => {
    it('should respect timeout setting', async () => {
      const startTime = Date.now();

      await probeNetwork({
        netAllow: ['https://httpbin.org/delay/10'], // 10s delay
        timeout: 1000, // 1s timeout
      });

      const elapsed = Date.now() - startTime;
      // Should timeout before 10s (with some margin)
      expect(elapsed).toBeLessThan(5000);
    }, { timeout: 10000 });
  });

  describe('Observation Schema Validation', () => {
    it('should return valid observation objects', async () => {
      const observations = await probeNetwork({
        netAllow: ['https://example.com'],
      });

      for (const obs of observations) {
        expect(obs).toHaveProperty('capability');
        expect(obs).toHaveProperty('available');
        expect(obs).toHaveProperty('guardDecision');
        expect(['allowed', 'denied']).toContain(obs.guardDecision);
      }
    }, { timeout: 10000 });
  });
});
