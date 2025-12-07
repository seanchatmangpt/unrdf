/**
 * @fileoverview Service Worker Manager Tests
 * @vitest-environment jsdom
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  registerServiceWorker,
  checkCrossOriginIsolation,
  getCOIStatus,
} from '../src/service-worker-manager.mjs';

describe('Service Worker Manager', () => {
  beforeEach(() => {
    // Reset mocks
    vi.clearAllMocks();
    
    // Mock navigator.serviceWorker
    global.navigator = {
      serviceWorker: {
        getRegistration: vi.fn(),
        controller: null,
      },
    };
    
    // Mock window.location
    delete window.location;
    window.location = { reload: vi.fn() };
  });

  describe('checkCrossOriginIsolation', () => {
    it('should return true when crossOriginIsolated is true', () => {
      global.crossOriginIsolated = true;
      expect(checkCrossOriginIsolation()).toBe(true);
    });

    it('should return false when crossOriginIsolated is false', () => {
      global.crossOriginIsolated = false;
      expect(checkCrossOriginIsolation()).toBe(false);
    });

    it('should check SharedArrayBuffer when crossOriginIsolated is undefined', () => {
      delete global.crossOriginIsolated;
      global.SharedArrayBuffer = class SharedArrayBuffer {
        constructor() {
          this.byteLength = 0;
        }
      };
      
      expect(checkCrossOriginIsolation()).toBe(true);
    });

    it('should return false when neither COI nor SharedArrayBuffer available', () => {
      delete global.crossOriginIsolated;
      delete global.SharedArrayBuffer;
      
      expect(checkCrossOriginIsolation()).toBe(false);
    });
  });

  describe('getCOIStatus', () => {
    it('should return status object with all properties', () => {
      global.crossOriginIsolated = true;
      global.SharedArrayBuffer = class {};
      global.navigator.serviceWorker = {};
      
      const status = getCOIStatus();
      
      expect(status).toHaveProperty('crossOriginIsolated');
      expect(status).toHaveProperty('sharedArrayBufferAvailable');
      expect(status).toHaveProperty('serviceWorkerSupported');
      expect(status).toHaveProperty('headers');
      expect(status.headers).toHaveProperty('coep');
      expect(status.headers).toHaveProperty('coop');
    });
  });

  describe('registerServiceWorker', () => {
    it('should return false when service workers not supported', async () => {
      delete global.navigator.serviceWorker;
      
      const result = await registerServiceWorker();
      expect(result).toBe(false);
    });

    it('should handle service worker registration', async () => {
      const mockRegistration = {
        scope: '/',
        installing: null,
        waiting: null,
      };
      
      global.navigator.serviceWorker.getRegistration = vi.fn().mockResolvedValue(mockRegistration);
      
      // Mock coi-serviceworker import
      vi.mock('coi-serviceworker', () => ({}));
      
      const result = await registerServiceWorker();
      
      // Should attempt to get registration
      expect(global.navigator.serviceWorker.getRegistration).toHaveBeenCalled();
    });
  });
});

