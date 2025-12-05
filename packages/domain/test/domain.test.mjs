/**
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  // Constants
  PAPER_FAMILIES,
  THESIS_TYPES,
  OUTPUT_FORMATS,
  // Schemas
  PaperSchema,
  ThesisSchema,
  ConfigSchema,
  // Models
  Paper,
  Thesis,
  Config,
  // Formatters
  formatOutput,
  getFormatter,
  isValidFormat,
  detectFormat,
  jsonFormatter,
  parseJSON,
  safeParseJSON,
  isValidJSON,
  yamlFormatter,
  parseYAML,
  safeParseYAML,
  isValidYAML,
  jsonToYAML,
  yamlToJSON,
  tableFormatter,
  dataToRows,
} from '../src/index.mjs';

describe('Domain Module - Constants', () => {
  it('should export paper families', () => {
    expect(PAPER_FAMILIES).toBeDefined();
    expect(Array.isArray(PAPER_FAMILIES) || typeof PAPER_FAMILIES === 'object').toBe(true);
  });

  it('should export thesis types', () => {
    expect(THESIS_TYPES).toBeDefined();
    expect(Array.isArray(THESIS_TYPES) || typeof THESIS_TYPES === 'object').toBe(true);
  });

  it('should export output formats', () => {
    expect(OUTPUT_FORMATS).toBeDefined();
    expect(Array.isArray(OUTPUT_FORMATS) || typeof OUTPUT_FORMATS === 'object').toBe(true);
  });
});

describe('Domain Module - Zod Schemas', () => {
  it('should have PaperSchema defined', () => {
    expect(PaperSchema).toBeDefined();
    expect(typeof PaperSchema.parse).toBe('function');
  });

  it('should have ThesisSchema defined', () => {
    expect(ThesisSchema).toBeDefined();
    expect(typeof ThesisSchema.parse).toBe('function');
  });

  it('should have ConfigSchema defined', () => {
    expect(ConfigSchema).toBeDefined();
    expect(typeof ConfigSchema.parse).toBe('function');
  });
});

describe('Domain Module - Models', () => {
  describe('Paper Model', () => {
    it('should instantiate Paper model', () => {
      const paper = new Paper({
        title: 'Test Paper',
        authors: ['Author 1'],
        year: 2024,
      });
      expect(paper).toBeDefined();
      expect(paper.title).toBe('Test Paper');
    });

    it('should have required paper properties', () => {
      const paper = new Paper({
        title: 'Test Paper',
        authors: ['Author 1'],
        year: 2024,
      });
      expect(paper.title).toBeDefined();
      expect(paper.authors).toBeDefined();
      expect(paper.year).toBeDefined();
    });
  });

  describe('Thesis Model', () => {
    it('should instantiate Thesis model', () => {
      const thesis = new Thesis({
        title: 'Test Thesis',
        author: 'Test Author',
        year: 2024,
        type: 'phd',
      });
      expect(thesis).toBeDefined();
      expect(thesis.title).toBe('Test Thesis');
    });

    it('should have required thesis properties', () => {
      const thesis = new Thesis({
        title: 'Test Thesis',
        author: 'Test Author',
        year: 2024,
        type: 'phd',
      });
      expect(thesis.title).toBeDefined();
      expect(thesis.author).toBeDefined();
      expect(thesis.year).toBeDefined();
      expect(thesis.type).toBeDefined();
    });
  });

  describe('Config Model', () => {
    it('should instantiate Config model', () => {
      const config = new Config({
        name: 'test-config',
        version: '1.0.0',
      });
      expect(config).toBeDefined();
      expect(config.name).toBe('test-config');
    });

    it('should have required config properties', () => {
      const config = new Config({
        name: 'test-config',
        version: '1.0.0',
      });
      expect(config.name).toBeDefined();
      expect(config.version).toBeDefined();
    });
  });
});

describe('Domain Module - Formatters', () => {
  describe('Format Detection', () => {
    it('should detect valid formats', () => {
      expect(isValidFormat('json')).toBe(true);
      expect(isValidFormat('yaml')).toBe(true);
      expect(isValidFormat('table')).toBe(true);
    });

    it('should detect invalid formats', () => {
      expect(isValidFormat('invalid')).toBe(false);
    });

    it('should detect format from content', () => {
      const jsonContent = '{"test": "data"}';
      const yamlContent = 'test: data';

      expect(detectFormat(jsonContent)).toBeDefined();
      expect(detectFormat(yamlContent)).toBeDefined();
    });

    it('should get formatter by name', () => {
      const jsonFmt = getFormatter('json');
      const yamlFmt = getFormatter('yaml');
      const tableFmt = getFormatter('table');

      expect(jsonFmt).toBeDefined();
      expect(yamlFmt).toBeDefined();
      expect(tableFmt).toBeDefined();
    });
  });

  describe('JSON Formatter', () => {
    it('should format data to JSON', () => {
      const data = { key: 'value', number: 42 };
      const result = jsonFormatter(data);

      expect(result).toBeDefined();
      expect(typeof result).toBe('string');
      expect(result).toContain('key');
    });

    it('should parse valid JSON', () => {
      const jsonString = '{"test": "data"}';
      const result = parseJSON(jsonString);

      expect(result).toBeDefined();
      expect(result.test).toBe('data');
    });

    it('should safely parse invalid JSON', () => {
      const invalidJSON = 'not valid json';
      const result = safeParseJSON(invalidJSON);

      expect(result).toBeDefined();
      // Should either return null or an error object, not throw
    });

    it('should validate JSON strings', () => {
      expect(isValidJSON('{"valid": true}')).toBe(true);
      expect(isValidJSON('not json')).toBe(false);
    });
  });

  describe('YAML Formatter', () => {
    it('should format data to YAML', () => {
      const data = { key: 'value', items: [1, 2, 3] };
      const result = yamlFormatter(data);

      expect(result).toBeDefined();
      expect(typeof result).toBe('string');
    });

    it('should parse valid YAML', () => {
      const yamlString = 'key: value\nitems:\n  - 1\n  - 2';
      const result = parseYAML(yamlString);

      expect(result).toBeDefined();
      expect(result.key).toBe('value');
    });

    it('should safely parse invalid YAML', () => {
      const invalidYAML = 'invalid: [unclosed';
      const result = safeParseYAML(invalidYAML);

      expect(result).toBeDefined();
      // Should either return null or an error object, not throw
    });

    it('should validate YAML strings', () => {
      expect(isValidYAML('key: value')).toBe(true);
    });
  });

  describe('Format Conversion', () => {
    it('should convert JSON to YAML', () => {
      const jsonData = { test: 'data', count: 5 };
      const result = jsonToYAML(jsonData);

      expect(result).toBeDefined();
      expect(typeof result).toBe('string');
      expect(result).toContain('test');
    });

    it('should convert YAML to JSON', () => {
      const yamlString = 'test: data\ncount: 5';
      const result = yamlToJSON(yamlString);

      expect(result).toBeDefined();
      expect(result.test).toBe('data');
    });
  });

  describe('Table Formatter', () => {
    it('should format data as table', () => {
      const data = [
        { name: 'Alice', age: 30 },
        { name: 'Bob', age: 25 },
      ];
      const result = tableFormatter(data);

      expect(result).toBeDefined();
      expect(typeof result).toBe('string');
    });

    it('should convert data to table rows', () => {
      const data = [
        { col1: 'value1', col2: 'value2' },
        { col1: 'value3', col2: 'value4' },
      ];
      const rows = dataToRows(data);

      expect(Array.isArray(rows)).toBe(true);
      expect(rows.length).toBeGreaterThan(0);
    });
  });

  describe('Format Output', () => {
    it('should format output with specified format', () => {
      const data = { test: 'data' };
      const jsonResult = formatOutput(data, 'json');
      const yamlResult = formatOutput(data, 'yaml');

      expect(jsonResult).toBeDefined();
      expect(yamlResult).toBeDefined();
    });

    it('should handle multiple data types', () => {
      expect(formatOutput({ key: 'value' }, 'json')).toBeDefined();
      expect(formatOutput([1, 2, 3], 'json')).toBeDefined();
      expect(formatOutput('string value', 'json')).toBeDefined();
    });
  });
});

describe('Domain Module - Integration', () => {
  it('should create paper and format as JSON', () => {
    const paper = new Paper({
      title: 'Integration Test Paper',
      authors: ['Test Author'],
      year: 2024,
    });

    const formatted = formatOutput(paper, 'json');
    expect(formatted).toBeDefined();
    expect(typeof formatted).toBe('string');
  });

  it('should create thesis and format as YAML', () => {
    const thesis = new Thesis({
      title: 'Integration Test Thesis',
      author: 'Test Author',
      year: 2024,
      type: 'phd',
    });

    const formatted = formatOutput(thesis, 'yaml');
    expect(formatted).toBeDefined();
    expect(typeof formatted).toBe('string');
  });

  it('should create config and format as table', () => {
    const config = new Config({
      name: 'integration-test',
      version: '1.0.0',
    });

    const formatted = formatOutput(config, 'table');
    expect(formatted).toBeDefined();
  });
});
