/**
 * Cross-Platform Test Matrix for Linux, macOS, and Windows
 * Ensures consistent behavior across operating systems
 */
import path from 'path';
import os from 'os';
import { TemplateEngine } from '../../../src/engine/template-engine.js';
import { GoldenTestValidator } from '../golden/golden-validator.js';
import { createTestDataFactory } from '../fixtures/test-data-factory.js';

export class CrossPlatformTestMatrix {
  constructor(options = {}) {
    this.templateEngine = new TemplateEngine(options.engineConfig);
    this.goldenValidator = new GoldenTestValidator({
      ...options.goldenConfig,
      goldenDir: path.join(__dirname, '../golden/cross-platform')
    });
    this.currentPlatform = process.platform;
    this.testDataFactory = null;
    this.results = new Map();
  }

  /**
   * Initialize the test matrix
   */
  async initialize() {
    this.testDataFactory = await createTestDataFactory();
    await this.setupPlatformSpecificData();
  }

  /**
   * Run the complete cross-platform test matrix
   */
  async runCrossPlatformTests() {
    await this.initialize();

    const testSuites = [
      'path-handling',
      'line-endings',
      'file-system-operations',
      'template-rendering',
      'character-encoding',
      'environment-variables'
    ];

    const results = {};

    for (const suite of testSuites) {
      console.log(`Running cross-platform test suite: ${suite}`);
      results[suite] = await this.runTestSuite(suite);
    }

    return this.generateCrossPlatformReport(results);
  }

  /**
   * Run a specific test suite
   */
  async runTestSuite(suiteName) {
    const suiteMethod = `test${this.capitalizeFirst(suiteName.replace(/-/g, ''))}`;

    if (typeof this[suiteMethod] === 'function') {
      return await this[suiteMethod]();
    }

    throw new Error(`Test suite method ${suiteMethod} not found`);
  }

  /**
   * Test path handling across platforms
   */
  async testPathhandling() {
    const template = `
import path from 'path';

export const paths = {
  projectRoot: "{{ projectRoot }}",
  srcDir: path.join("{{ projectRoot }}", "src"),
  buildDir: path.join("{{ projectRoot }}", "build"),
  configFile: "{{ configPath }}",

  // Normalized paths for cross-platform compatibility
  normalizedSrc: "{{ srcPath|normalizePath }}",
  normalizedBuild: "{{ buildPath|normalizePath }}",

  // Platform-specific paths
  platformSpecific: {
    {% if platform === 'win32' %}
    executable: "{{ projectRoot }}\\\\bin\\\\app.exe",
    config: "{{ userHome }}\\\\AppData\\\\Roaming\\\\{{ appName }}\\\\config.json"
    {% elif platform === 'darwin' %}
    executable: "{{ projectRoot }}/bin/app",
    config: "{{ userHome }}/Library/Application Support/{{ appName }}/config.json"
    {% else %}
    executable: "{{ projectRoot }}/bin/app",
    config: "{{ userHome }}/.config/{{ appName }}/config.json"
    {% endif %}
  }
};`;

    const testData = {
      projectRoot: this.currentPlatform === 'win32' ? 'C:\\Projects\\MyApp' : '/usr/local/projects/myapp',
      configPath: this.currentPlatform === 'win32' ? 'C:\\Config\\app.json' : '/etc/myapp/config.json',
      srcPath: this.currentPlatform === 'win32' ? 'C:\\Projects\\MyApp\\src' : '/usr/local/projects/myapp/src',
      buildPath: this.currentPlatform === 'win32' ? 'C:\\Projects\\MyApp\\build' : '/usr/local/projects/myapp/build',
      platform: this.currentPlatform,
      userHome: os.homedir(),
      appName: 'MyApp'
    };

    const result = await this.templateEngine.render(template, testData);

    // Validate against platform-specific golden file
    const goldenFile = `paths-${this.currentPlatform}.golden.js`;
    const validation = await this.goldenValidator.validate(result, goldenFile, {
      normalizePaths: false, // Keep platform-specific paths
      createIfMissing: true
    });

    return {
      platform: this.currentPlatform,
      template: 'path-handling',
      validation,
      result,
      testData,
      platformSpecific: true
    };
  }

  /**
   * Test line ending handling
   */
  async testLineendings() {
    const template = `Line 1: {{ message1 }}
Line 2: {{ message2 }}
{% for item in items %}
Item {{ loop.index }}: {{ item }}
{% endfor %}
Final line: {{ finalMessage }}`;

    const testData = {
      message1: 'Hello World',
      message2: 'Cross-platform test',
      items: ['Apple', 'Banana', 'Cherry'],
      finalMessage: 'End of file'
    };

    const result = await this.templateEngine.render(template, testData);

    // Normalize line endings for cross-platform golden test
    const normalizedResult = result.replace(/\r\n/g, '\n').replace(/\r/g, '\n');

    const validation = await this.goldenValidator.validate(
      normalizedResult,
      'line-endings.golden.txt',
      { createIfMissing: true }
    );

    // Also test that the original result has correct platform line endings
    const expectedLineEnding = this.currentPlatform === 'win32' ? '\r\n' : '\n';
    const hasCorrectLineEndings = result.includes(expectedLineEnding);

    return {
      platform: this.currentPlatform,
      template: 'line-endings',
      validation,
      normalizedResult,
      originalResult: result,
      hasCorrectLineEndings,
      expectedLineEnding
    };
  }

  /**
   * Test file system operations
   */
  async testFilesystemoperations() {
    const template = `
// File system operations for {{ platform }}
import fs from 'fs';
import path from 'path';

export class FileManager {
  constructor() {
    this.baseDir = "{{ baseDirectory }}";
    this.tempDir = "{{ tempDirectory }}";
    this.homeDir = "{{ homeDirectory }}";
  }

  getPlatformSpecificPath(relativePath) {
    {% if platform === 'win32' %}
    return path.win32.join(this.baseDir, relativePath);
    {% else %}
    return path.posix.join(this.baseDir, relativePath);
    {% endif %}
  }

  async createDirectoryStructure() {
    const dirs = [
      {{ directories|map('quote')|join(',\n      ') }}
    ];

    for (const dir of dirs) {
      await fs.mkdir(path.join(this.baseDir, dir), { recursive: true });
    }
  }

  getConfigPath() {
    {% if platform === 'win32' %}
    return path.join(this.homeDir, 'AppData', 'Roaming', '{{ appName }}', 'config.json');
    {% elif platform === 'darwin' %}
    return path.join(this.homeDir, 'Library', 'Application Support', '{{ appName }}', 'config.json');
    {% else %}
    return path.join(this.homeDir, '.config', '{{ appName }}', 'config.json');
    {% endif %}
  }
}`;

    const testData = {
      platform: this.currentPlatform,
      baseDirectory: this.currentPlatform === 'win32' ? 'C:\\MyApp' : '/opt/myapp',
      tempDirectory: os.tmpdir(),
      homeDirectory: os.homedir(),
      appName: 'MyTestApp',
      directories: ['src', 'lib', 'tests', 'docs', 'config']
    };

    const result = await this.templateEngine.render(template, testData);

    // Validate cross-platform compatibility
    const validation = await this.goldenValidator.validate(
      result,
      `filesystem-${this.currentPlatform}.golden.js`,
      { createIfMissing: true }
    );

    return {
      platform: this.currentPlatform,
      template: 'filesystem-operations',
      validation,
      result,
      testData,
      platformSpecific: true
    };
  }

  /**
   * Test template rendering consistency
   */
  async testTemplaterendering() {
    const complexTemplate = `
{%- set timestamp = "2024-01-01T00:00:00Z" -%}
{%- set platform = platform -%}
/**
 * Generated {{ componentType }} for {{ platform }}
 * Timestamp: {{ timestamp }}
 */

{% if componentType === 'class' %}
export class {{ name|pascalCase }} {
  constructor({{ parameters|map('formatParam')|join(', ') }}) {
    {% for param in parameters %}
    this.{{ param.name }} = {{ param.name }};
    {% endfor %}
    this.platform = "{{ platform }}";
    this.createdAt = new Date("{{ timestamp }}");
  }

  {% for method in methods %}
  {{ method.name }}({{ method.params|join(', ') }}) {
    {{ method.body|indent(4) }}
  }

  {% endfor %}
}
{% else %}
export const {{ name|camelCase }} = {
  platform: "{{ platform }}",
  createdAt: "{{ timestamp }}",
  {% for prop in properties %}
  {{ prop.name }}: {{ prop.value|jsonStringify }}{{ "," if not loop.last }}
  {% endfor %}
};
{% endif %}`;

    const testData = {
      platform: this.currentPlatform,
      componentType: 'class',
      name: 'UserManager',
      parameters: [
        { name: 'apiUrl', type: 'string' },
        { name: 'timeout', type: 'number' }
      ],
      methods: [
        {
          name: 'getUser',
          params: ['id'],
          body: 'return this.api.get(`/users/${id}`);'
        },
        {
          name: 'createUser',
          params: ['userData'],
          body: 'return this.api.post("/users", userData);'
        }
      ],
      properties: [
        { name: 'version', value: '1.0.0' },
        { name: 'features', value: ['auth', 'cache'] }
      ]
    };

    const result = await this.templateEngine.render(complexTemplate, testData);

    // This should be identical across platforms (no platform-specific elements in output)
    const validation = await this.goldenValidator.validate(
      result,
      'template-rendering.golden.js',
      { createIfMissing: true }
    );

    return {
      platform: this.currentPlatform,
      template: 'template-rendering',
      validation,
      result,
      shouldBeIdentical: true
    };
  }

  /**
   * Test character encoding handling
   */
  async testCharacterencoding() {
    const template = `
// Unicode and special character test
export const messages = {
  english: "{{ messages.english }}",
  japanese: "{{ messages.japanese }}",
  emoji: "{{ messages.emoji }}",
  symbols: "{{ messages.symbols }}",

  // Escaped characters
  quotes: "{{ messages.quotes }}",
  newlines: {{ messages.newlines|jsonStringify }},

  // Unicode escapes
  unicode: "{{ messages.unicode }}"
};

export const paths = {
  // Test path with unicode characters
  unicodePath: "{{ paths.unicode }}",

  // Test with special characters that might be problematic
  specialChars: "{{ paths.special }}"
};`;

    const testData = {
      messages: {
        english: 'Hello World',
        japanese: 'こんにちは世界',
        emoji: '🚀 🌟 ✨',
        symbols: '©®™€£¥',
        quotes: 'He said "Hello" and she replied \'Hi\'',
        newlines: 'Line 1\nLine 2\nLine 3',
        unicode: '\\u0048\\u0065\\u006C\\u006C\\u006F' // "Hello" in unicode escapes
      },
      paths: {
        unicode: this.currentPlatform === 'win32' ? 'C:\\用户\\项目' : '/用户/项目',
        special: this.currentPlatform === 'win32' ? 'C:\\Test [brackets] & symbols' : '/test/[brackets] & symbols'
      }
    };

    const result = await this.templateEngine.render(template, testData);

    const validation = await this.goldenValidator.validate(
      result,
      'character-encoding.golden.js',
      { createIfMissing: true }
    );

    return {
      platform: this.currentPlatform,
      template: 'character-encoding',
      validation,
      result,
      testData,
      shouldBeIdentical: true
    };
  }

  /**
   * Test environment variable handling
   */
  async testEnvironmentvariables() {
    const template = `
// Environment configuration for {{ platform }}
export const environment = {
  platform: "{{ platform }}",
  nodeEnv: "{{ env.NODE_ENV || 'development' }}",

  // Platform-specific environment handling
  {% if platform === 'win32' %}
  pathSeparator: ";",
  executable: ".exe",
  homeVar: "USERPROFILE",
  tempVar: "TEMP"
  {% else %}
  pathSeparator: ":",
  executable: "",
  homeVar: "HOME",
  tempVar: "TMPDIR"
  {% endif %}
};

export const paths = {
  home: process.env.{{ environment.homeVar }},
  temp: process.env.{{ environment.tempVar }},
  path: process.env.PATH?.split("{{ environment.pathSeparator }}")
};`;

    // Mock environment variables for consistent testing
    const mockEnv = {
      NODE_ENV: 'test',
      PATH: this.currentPlatform === 'win32'
        ? 'C:\\Windows\\System32;C:\\Program Files'
        : '/usr/local/bin:/usr/bin:/bin'
    };

    const testData = {
      platform: this.currentPlatform,
      env: mockEnv
    };

    const result = await this.templateEngine.render(template, testData);

    const validation = await this.goldenValidator.validate(
      result,
      `environment-${this.currentPlatform}.golden.js`,
      { createIfMissing: true }
    );

    return {
      platform: this.currentPlatform,
      template: 'environment-variables',
      validation,
      result,
      testData,
      platformSpecific: true
    };
  }

  /**
   * Setup platform-specific test data
   */
  async setupPlatformSpecificData() {
    // Register platform-specific filters
    await this.templateEngine.registerFilters({
      normalizePath: (filePath) => {
        return filePath.replace(/\\/g, '/');
      },

      formatParam: (param) => {
        return `${param.name}${param.type ? `: ${param.type}` : ''}`;
      },

      jsonStringify: (value) => {
        return JSON.stringify(value);
      },

      quote: (str) => {
        return `"${str}"`;
      },

      indent: (text, levels = 1) => {
        const indent = '  '.repeat(levels);
        return text.split('\n').map(line => line ? indent + line : line).join('\n');
      }
    });
  }

  /**
   * Generate comprehensive cross-platform report
   */
  generateCrossPlatformReport(allResults) {
    const platformInfo = {
      current: this.currentPlatform,
      node: process.version,
      arch: process.arch,
      os: os.type(),
      release: os.release()
    };

    const summary = this.generateTestSummary(allResults);
    const compatibility = this.analyzeCompatibility(allResults);
    const recommendations = this.generateCompatibilityRecommendations(allResults);

    return {
      platform: platformInfo,
      summary,
      compatibility,
      testResults: allResults,
      recommendations,
      timestamp: new Date().toISOString()
    };
  }

  /**
   * Generate test summary
   */
  generateTestSummary(allResults) {
    const totalTests = Object.keys(allResults).length;
    const passedTests = Object.values(allResults).filter(r => r.validation?.valid).length;
    const platformSpecificTests = Object.values(allResults).filter(r => r.platformSpecific).length;
    const universalTests = totalTests - platformSpecificTests;

    return {
      totalTests,
      passedTests,
      failedTests: totalTests - passedTests,
      successRate: passedTests / totalTests,
      platformSpecificTests,
      universalTests
    };
  }

  /**
   * Analyze cross-platform compatibility
   */
  analyzeCompatibility(allResults) {
    const issues = [];
    const strengths = [];

    Object.entries(allResults).forEach(([testName, result]) => {
      if (!result.validation?.valid) {
        issues.push({
          test: testName,
          platform: this.currentPlatform,
          issue: result.validation?.message || 'Test failed'
        });
      } else if (result.shouldBeIdentical) {
        strengths.push({
          test: testName,
          strength: 'Platform-independent output achieved'
        });
      }
    });

    return {
      issues,
      strengths,
      compatibilityScore: (Object.keys(allResults).length - issues.length) / Object.keys(allResults).length
    };
  }

  /**
   * Generate compatibility recommendations
   */
  generateCompatibilityRecommendations(allResults) {
    const recommendations = [];

    Object.entries(allResults).forEach(([testName, result]) => {
      if (!result.validation?.valid) {
        recommendations.push({
          test: testName,
          priority: 'high',
          recommendation: 'Fix platform-specific issues in template rendering',
          details: result.validation?.message
        });
      }

      if (result.platformSpecific && result.validation?.valid) {
        recommendations.push({
          test: testName,
          priority: 'low',
          recommendation: 'Consider abstracting platform-specific logic',
          details: 'Template handles platform differences correctly but could be more maintainable'
        });
      }
    });

    return recommendations;
  }

  /**
   * Utility: Capitalize first letter
   */
  capitalizeFirst(str) {
    return str.charAt(0).toUpperCase() + str.slice(1);
  }
}