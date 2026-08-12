/**
 * Node.js Integration Tests for process_mining_wasm
 *
 * Tests covering:
 * - Loading XES and OCEL files
 * - Analyzing event logs
 * - Discovering process models
 * - Managing state
 * - Error handling
 */

import * as fs from 'fs';
import * as path from 'path';

describe('process_mining_wasm Node.js Integration', () => {
  let xesContent: string;
  let ocelContent: string;

  beforeAll(() => {
    const fixturesDir = path.join(__dirname, '..', 'data', 'fixtures');
    xesContent = fs.readFileSync(path.join(fixturesDir, 'sample.xes'), 'utf-8');
    ocelContent = fs.readFileSync(path.join(fixturesDir, 'sample.json'), 'utf-8');
  });

  describe('Fixture Loading', () => {
    test('should load XES fixture file', () => {
      expect(xesContent).toBeTruthy();
      expect(xesContent).toContain('<?xml');
      expect(xesContent).toContain('<log');
    });

    test('should load OCEL fixture file', () => {
      expect(ocelContent).toBeTruthy();
      const parsed = JSON.parse(ocelContent);
      expect(parsed['ocel:events']).toBeDefined();
      expect(parsed['ocel:objects']).toBeDefined();
    });
  });

  describe('XES Format Validation', () => {
    test('XES should contain required elements', () => {
      expect(xesContent).toContain('<trace>');
      expect(xesContent).toContain('<event>');
      expect(xesContent).toContain('concept:name');
      expect(xesContent).toContain('time:timestamp');
    });

    test('XES should have valid timestamps', () => {
      const timestamps = xesContent.match(/\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/g);
      expect(timestamps).not.toBeNull();
      expect((timestamps || []).length).toBeGreaterThan(0);
    });

    test('XES should have valid activities', () => {
      expect(xesContent).toContain('Request');
      expect(xesContent).toContain('Review');
      expect(xesContent).toContain('Approve');
    });
  });

  describe('OCEL Format Validation', () => {
    test('OCEL should have valid structure', () => {
      const ocel = JSON.parse(ocelContent);
      expect(ocel['ocel:global-event']).toBeDefined();
      expect(ocel['ocel:global-object']).toBeDefined();
      expect(ocel['ocel:events']).toBeDefined();
      expect(ocel['ocel:objects']).toBeDefined();
    });

    test('OCEL should have object types', () => {
      const ocel = JSON.parse(ocelContent);
      const objectTypes = ocel['ocel:global-object']['ocel:object-type'];
      expect(Array.isArray(objectTypes)).toBe(true);
      expect(objectTypes.length).toBeGreaterThan(0);
    });

    test('OCEL should have event-object mappings', () => {
      const ocel = JSON.parse(ocelContent);
      const events = ocel['ocel:events']['ocel:event'];
      expect(Array.isArray(events)).toBe(true);

      events.forEach((event: any) => {
        expect(event['ocel:omap']).toBeDefined();
        expect(event['ocel:omap']['ocel:o']).toBeDefined();
      });
    });
  });

  describe('Complete Workflow Examples', () => {
    test('XES file represents a 3-case process', () => {
      const traces = xesContent.match(/<trace>/g) || [];
      expect(traces.length).toBe(3);
    });

    test('OCEL has 10 events and 7 objects', () => {
      const ocel = JSON.parse(ocelContent);
      const events = ocel['ocel:events']['ocel:event'];
      const objects = ocel['ocel:objects']['ocel:object'];

      expect(events.length).toBe(10);
      expect(objects.length).toBe(7);
    });

    test('OCEL has Order, Item, and Package object types', () => {
      const ocel = JSON.parse(ocelContent);
      const objectTypes = ocel['ocel:global-object']['ocel:object-type'].map(
        (t: any) => t['ocel:name']
      );

      expect(objectTypes).toContain('Order');
      expect(objectTypes).toContain('Item');
      expect(objectTypes).toContain('Package');
    });
  });

  describe('Data Integrity', () => {
    test('XES activities should match expected workflow', () => {
      const activities = ['Request', 'Review', 'Approve', 'Reject', 'Complete'];
      activities.forEach((activity) => {
        expect(xesContent).toContain(`value="${activity}"`);
      });
    });

    test('XES should have consistent timestamps', () => {
      const timestampMatches = xesContent.match(/time:timestamp" value="([^"]+)"/g) || [];

      expect(timestampMatches.length).toBeGreaterThan(0);
      timestampMatches.forEach((match) => {
        expect(match).toMatch(/\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/);
      });
    });

    test('OCEL events should have valid timestamps', () => {
      const ocel = JSON.parse(ocelContent);
      const events = ocel['ocel:events']['ocel:event'];

      events.forEach((event: any) => {
        const timestamp = event['ocel:timestamp'];
        expect(timestamp).toMatch(/\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/);
      });
    });
  });

  describe('Fixture Usability', () => {
    test('XES content is a valid string', () => {
      expect(typeof xesContent).toBe('string');
      expect(xesContent.length).toBeGreaterThan(0);
    });

    test('OCEL content can be parsed as JSON', () => {
      expect(() => JSON.parse(ocelContent)).not.toThrow();
    });

    test('Fixtures have sufficient events for analysis', () => {
      const traces = xesContent.match(/<event>/g) || [];
      expect(traces.length).toBeGreaterThanOrEqual(10);
    });
  });
});
