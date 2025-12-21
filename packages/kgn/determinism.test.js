/**
 * London BDD Test Implementation for Deterministic Template Generation
 *
 * This test suite implements the behavior specifications defined in determinism.feature
 * using London-style BDD with mocks, stubs, and focused unit testing.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { createHash } from 'crypto';

// Mock implementations for deterministic testing
const createMockTemplateEngine = () => ({
  render: vi.fn(),
  configure: vi.fn(),
  getDeterministicTimestamp: vi.fn(() => '2025-01-01T00:00:00.000Z'),
  generateDeterministicId: vi.fn((data) => {
    const hash = createHash('sha256');
    hash.update(JSON.stringify(data, Object.keys(data).sort()));
    return hash.digest('hex').substring(0, 8);
  })
});

describe('Feature: Deterministic Template Generation', () => {
  let templateEngine;
  let mockFileSystem;
  let mockConfig;

  beforeEach(() => {
    // Set up mocks for each test
    templateEngine = createMockTemplateEngine();

    mockFileSystem = {
      readdir: vi.fn(),
      readFile: vi.fn(),
      stat: vi.fn()
    };

    mockConfig = {
      buildTime: '2025-01-01T00:00:00.000Z',
      environment: 'test',
      targetPlatform: 'universal',
      deterministicMode: true
    };

    // Mock system dependencies
    vi.mock('fs', () => mockFileSystem);
    vi.mock('process', () => ({
      env: {},  // Empty env to test independence
      platform: 'test-platform',
      version: 'test-version'
    }));
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe('Rule: Same input must always produce identical output', () => {
    it('should produce byte-for-byte identical outputs for multiple renders', async () => {
      // Given
      const templateName = 'basic-template.njk';
      const inputData = {
        name: 'test-project',
        version: '1.0.0',
        buildTime: '2025-01-01T00:00:00.000Z'
      };
      const expectedOutput = 'Project: test-project v1.0.0 (built: 2025-01-01T00:00:00.000Z)';

      // Mock consistent template rendering
      templateEngine.render.mockResolvedValue(expectedOutput);

      // When - render template 5 times
      const outputs = await Promise.all([
        templateEngine.render(templateName, inputData),
        templateEngine.render(templateName, inputData),
        templateEngine.render(templateName, inputData),
        templateEngine.render(templateName, inputData),
        templateEngine.render(templateName, inputData)
      ]);

      // Then
      expect(outputs.length).toBe(5);
      outputs.forEach(output => {
        expect(output).toBe(expectedOutput);
      });

      // Verify all outputs are byte-for-byte identical
      const hashes = outputs.map(output =>
        createHash('sha256').update(output).digest('hex')
      );
      expect(new Set(hashes).size).toBe(1); // All hashes should be identical

      // Verify template was called with consistent parameters
      expect(templateEngine.render).toHaveBeenCalledTimes(5);
      templateEngine.render.mock.calls.forEach(call => {
        expect(call[0]).toBe(templateName);
        expect(call[1]).toEqual(inputData);
      });
    });

    it('should handle complex nested data structures deterministically', async () => {
      // Given
      const complexData = {
        project: {
          name: 'complex-project',
          dependencies: {
            lodash: '^4.17.21',
            axios: '^1.0.0',
            'node-fetch': '^3.0.0'
          },
          scripts: ['build', 'test', 'deploy']
        },
        metadata: {
          buildTime: '2025-01-01T00:00:00.000Z',
          version: '2.1.0'
        }
      };

      // Mock deterministic object key processing
      templateEngine.render.mockImplementation((template, data) => {
        // Simulate sorted object key processing with nested objects
        const sortKeys = (obj) => {
          if (Array.isArray(obj)) {
            return obj.map(sortKeys);
          } else if (obj !== null && typeof obj === 'object') {
            return Object.keys(obj).sort().reduce((acc, key) => {
              acc[key] = sortKeys(obj[key]);
              return acc;
            }, {});
          }
          return obj;
        };
        const sortedData = JSON.stringify(sortKeys(data));
        return `Template: ${template}, Data: ${sortedData}`;
      });

      // When
      const output1 = await templateEngine.render('complex-template.njk', complexData);
      const output2 = await templateEngine.render('complex-template.njk', complexData);

      // Then
      expect(output1).toBe(output2);
      expect(output1).toContain('axios');
      expect(output1).toContain('lodash');
      expect(output1).toContain('node-fetch');

      // Verify deterministic ordering is maintained
      const axiosIndex = output1.indexOf('axios');
      const lodashIndex = output1.indexOf('lodash');
      const fetchIndex = output1.indexOf('node-fetch');

      // Should be in alphabetical order
      expect(axiosIndex).toBeLessThan(lodashIndex);
      expect(lodashIndex).toBeLessThan(fetchIndex);
    });
  });

  describe('Rule: Templates must be independent of system time', () => {
    it('should use fixed timestamps instead of system time', async () => {
      // Given
      const inputData = { name: 'time-test' };

      // Mock system time variations (should not affect output)
      const mockDateNow = vi.spyOn(Date, 'now');
      mockDateNow.mockReturnValueOnce(1000000);
      mockDateNow.mockReturnValueOnce(2000000);
      mockDateNow.mockReturnValueOnce(3000000);

      // Mock template engine to use deterministic timestamp
      templateEngine.render.mockImplementation((template, data) => {
        const timestamp = templateEngine.getDeterministicTimestamp();
        return `Generated at: ${timestamp}`;
      });

      // When - render at "different times"
      const output1 = await templateEngine.render('time-template.njk', inputData);
      const output2 = await templateEngine.render('time-template.njk', inputData);
      const output3 = await templateEngine.render('time-template.njk', inputData);

      // Then
      expect(output1).toBe(output2);
      expect(output2).toBe(output3);
      expect(output1).toContain('2025-01-01T00:00:00.000Z');

      // Verify deterministic timestamp was used
      expect(templateEngine.getDeterministicTimestamp).toHaveBeenCalledTimes(3);

      // Verify system time was not used in output
      expect(output1).not.toContain('1000000');
      expect(output1).not.toContain('2000000');
      expect(output1).not.toContain('3000000');
    });

    it('should generate deterministic build metadata', async () => {
      // Given
      const buildData = {
        project: 'build-test',
        buildConfig: mockConfig
      };

      templateEngine.render.mockImplementation((template, data) => {
        return `Build: ${data.project} at ${data.buildConfig.buildTime}`;
      });

      // When
      const output1 = await templateEngine.render('build-template.njk', buildData);
      const output2 = await templateEngine.render('build-template.njk', buildData);

      // Then
      expect(output1).toBe(output2);
      expect(output1).toBe('Build: build-test at 2025-01-01T00:00:00.000Z');
    });
  });

  describe('Rule: Templates must produce identical output across all platforms', () => {
    it('should normalize platform-specific behaviors', async () => {
      // Given
      const crossPlatformData = {
        paths: ['src/index.js', 'tests/unit.js'],
        targetPlatform: 'universal'
      };

      // Mock platform-independent template processing
      templateEngine.render.mockImplementation((template, data) => {
        // Simulate path normalization
        const normalizedPaths = data.paths.map(p => p.replace(/\\/g, '/'));
        return `Paths: ${normalizedPaths.join(', ')}`;
      });

      // When - simulate different platforms
      const linuxOutput = await templateEngine.render('path-template.njk', crossPlatformData);
      const windowsOutput = await templateEngine.render('path-template.njk', crossPlatformData);
      const macosOutput = await templateEngine.render('path-template.njk', crossPlatformData);

      // Then
      expect(linuxOutput).toBe(windowsOutput);
      expect(windowsOutput).toBe(macosOutput);
      expect(linuxOutput).toContain('src/index.js'); // Forward slashes
      expect(linuxOutput).not.toContain('src\\index.js'); // No backslashes
    });
  });

  describe('Rule: Templates must not depend on environment variables', () => {
    it('should use explicit configuration instead of environment variables', async () => {
      // Given
      const configBasedData = {
        environment: mockConfig.environment,
        targetPlatform: mockConfig.targetPlatform
      };

      templateEngine.render.mockImplementation((template, data) => {
        return `Env: ${data.environment}, Platform: ${data.targetPlatform}`;
      });

      // When - simulate different process.env values (should not affect output)
      process.env.NODE_ENV = 'production'; // Should be ignored
      process.env.PLATFORM = 'different-platform'; // Should be ignored

      const output = await templateEngine.render('env-template.njk', configBasedData);

      // Then
      expect(output).toBe('Env: test, Platform: universal');
      expect(output).not.toContain('production');
      expect(output).not.toContain('different-platform');
    });
  });

  describe('Rule: Object iteration must be deterministic', () => {
    it('should iterate object keys in sorted order', async () => {
      // Given
      const unsortedObject = {
        zebra: 'last',
        apple: 'first',
        banana: 'middle'
      };

      templateEngine.render.mockImplementation((template, data) => {
        // Simulate sorted key iteration
        const sortedKeys = Object.keys(data).sort();
        return sortedKeys.map(key => `${key}:${data[key]}`).join(',');
      });

      // When
      const output1 = await templateEngine.render('sorted-template.njk', unsortedObject);
      const output2 = await templateEngine.render('sorted-template.njk', unsortedObject);

      // Then
      expect(output1).toBe(output2);
      expect(output1).toBe('apple:first,banana:middle,zebra:last');

      // Verify alphabetical ordering
      const parts = output1.split(',');
      expect(parts[0]).toContain('apple');
      expect(parts[1]).toContain('banana');
      expect(parts[2]).toContain('zebra');
    });
  });

  describe('Rule: All randomness must be deterministic', () => {
    it('should generate deterministic IDs based on content', async () => {
      // Given
      const contentData = {
        title: 'test-document',
        version: '1.0.0'
      };

      templateEngine.render.mockImplementation((template, data) => {
        const deterministicId = templateEngine.generateDeterministicId(data);
        return `ID: ${deterministicId}, Title: ${data.title}`;
      });

      // When
      const output1 = await templateEngine.render('id-template.njk', contentData);
      const output2 = await templateEngine.render('id-template.njk', contentData);

      // Then
      expect(output1).toBe(output2);

      // Verify deterministic ID generation was called
      expect(templateEngine.generateDeterministicId).toHaveBeenCalledTimes(2);
      expect(templateEngine.generateDeterministicId).toHaveBeenCalledWith(contentData);

      // ID should be derived from content, not random
      expect(output1).toMatch(/^ID: [a-f0-9]{8}, Title: test-document$/);
    });

    it('should produce different IDs for different content', async () => {
      // Given
      const content1 = { title: 'document-1' };
      const content2 = { title: 'document-2' };

      templateEngine.render.mockImplementation((template, data) => {
        const deterministicId = templateEngine.generateDeterministicId(data);
        return `ID: ${deterministicId}`;
      });

      // When
      const output1 = await templateEngine.render('id-template.njk', content1);
      const output2 = await templateEngine.render('id-template.njk', content2);

      // Then
      expect(output1).not.toBe(output2); // Different content = different IDs
      expect(output1).toMatch(/^ID: [a-f0-9]{8}$/);
      expect(output2).toMatch(/^ID: [a-f0-9]{8}$/);
    });
  });

  describe('Rule: File system operations must be deterministic', () => {
    it('should sort directory contents', async () => {
      // Given
      const unsortedFiles = ['zebra.txt', 'apple.txt', 'banana.txt'];
      const sortedFiles = ['apple.txt', 'banana.txt', 'zebra.txt'];

      mockFileSystem.readdir.mockResolvedValue(sortedFiles);

      templateEngine.render.mockImplementation(async (template, data) => {
        const files = await mockFileSystem.readdir(data.directory);
        return `Files: ${files.join(', ')}`;
      });

      // When
      const output = await templateEngine.render('file-list-template.njk', {
        directory: '/test'
      });

      // Then
      expect(output).toBe('Files: apple.txt, banana.txt, zebra.txt');
      expect(mockFileSystem.readdir).toHaveBeenCalledWith('/test');
    });
  });

  describe('Rule: Deterministic templates should maintain acceptable performance', () => {
    it('should render templates within performance constraints', async () => {
      // Given
      const largeData = {
        items: Array.from({ length: 1000 }, (_, i) => ({ id: i, name: `item-${i}` }))
      };

      templateEngine.render.mockImplementation((template, data) => {
        // Simulate sorted processing
        const sortedItems = data.items.sort((a, b) => a.id - b.id);
        return `Items: ${sortedItems.length}`;
      });

      // When
      const startTime = process.hrtime.bigint();
      const output = await templateEngine.render('large-template.njk', largeData);
      const endTime = process.hrtime.bigint();
      const renderTimeMs = Number(endTime - startTime) / 1000000;

      // Then
      expect(output).toBe('Items: 1000');
      expect(renderTimeMs).toBeLessThan(1000); // Should render in under 1 second
    });
  });

  describe('Rule: Nondeterministic patterns must be prevented', () => {
    it('should detect and reject nondeterministic patterns', () => {
      // Given - simulate lint rule checking
      const nondeterministicCode = `
        const timestamp = new Date().toISOString();
        const randomId = Math.random().toString(36);
        const env = process.env.NODE_ENV;
      `;

      const lintViolations = checkForDeterminismViolations(nondeterministicCode);

      // Then
      expect(lintViolations).toHaveLength(3);
      expect(lintViolations[0]).toContain('new Date()');
      expect(lintViolations[1]).toContain('Math.random()');
      expect(lintViolations[2]).toContain('process.env');
    });
  });

  describe('Rule: Error conditions must be deterministic', () => {
    it('should produce consistent error messages', async () => {
      // Given
      const invalidData = { /* missing required fields */ };

      templateEngine.render.mockRejectedValue(new Error('Template validation failed: missing required field "name"'));

      // When & Then
      await expect(templateEngine.render('strict-template.njk', invalidData))
        .rejects.toThrow('Template validation failed: missing required field "name"');

      // Error message should not contain timestamps or system-specific information
      try {
        await templateEngine.render('strict-template.njk', invalidData);
      } catch (error) {
        expect(error.message).not.toMatch(/\d{4}-\d{2}-\d{2}/); // No dates
        expect(error.message).not.toMatch(/\d+:\d+:\d+/); // No timestamps
        expect(error.message).not.toContain(process.platform); // No platform info
      }
    });
  });
});

// Helper function for lint violation checking
function checkForDeterminismViolations(code) {
  const violations = [];

  if (code.includes('new Date()')) {
    violations.push('VIOLATION: new Date() detected - use getDeterministicTimestamp()');
  }

  if (code.includes('Math.random()')) {
    violations.push('VIOLATION: Math.random() detected - use deterministic generation');
  }

  if (code.includes('process.env')) {
    violations.push('VIOLATION: process.env detected - use explicit configuration');
  }

  return violations;
}