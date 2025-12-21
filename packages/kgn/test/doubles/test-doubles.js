/**
 * LONDON TDD Test Doubles for Deterministic Template Engine
 * 
 * Provides stubs, mocks, and fakes for complete isolation testing
 * following London School TDD principles with dependency injection.
 */

/**
 *
 */
export class StubTemplateLoader {
  /**
   *
   */
  constructor(templates = {}) {
    this.templates = templates;
    this.loadCount = 0;
    this.lastLoadPath = null;
  }

  /**
   *
   */
  async load(templatePath) {
    this.loadCount++;
    this.lastLoadPath = templatePath;
    
    if (this.templates[templatePath]) {
      return this.templates[templatePath];
    }
    
    throw new Error(`Template not found: ${templatePath}`);
  }

  /**
   *
   */
  addTemplate(path, content) {
    this.templates[path] = content;
  }

  /**
   *
   */
  getStats() {
    return {
      loadCount: this.loadCount,
      lastLoadPath: this.lastLoadPath,
      templateCount: Object.keys(this.templates).length
    };
  }

  /**
   *
   */
  reset() {
    this.loadCount = 0;
    this.lastLoadPath = null;
  }
}

/**
 *
 */
export class MockIncludeResolver {
  /**
   *
   */
  constructor() {
    this.resolveHistory = [];
    this.resolutionMap = {};
    this.shouldFail = false;
    this.failureMessage = 'Resolution failed';
  }

  /**
   *
   */
  resolve(includePath, basePath) {
    const call = { includePath, basePath, timestamp: Date.now() };
    this.resolveHistory.push(call);
    
    if (this.shouldFail) {
      throw new Error(this.failureMessage);
    }
    
    if (this.resolutionMap[includePath]) {
      return this.resolutionMap[includePath];
    }
    
    // Default resolution logic
    return `${basePath}/${includePath}`;
  }

  /**
   *
   */
  setResolution(includePath, resolvedPath) {
    this.resolutionMap[includePath] = resolvedPath;
  }

  /**
   *
   */
  setFailure(shouldFail, message = 'Resolution failed') {
    this.shouldFail = shouldFail;
    this.failureMessage = message;
  }

  /**
   *
   */
  getHistory() {
    return [...this.resolveHistory];
  }

  /**
   *
   */
  getCallCount() {
    return this.resolveHistory.length;
  }

  /**
   *
   */
  wasCalled() {
    return this.resolveHistory.length > 0;
  }

  /**
   *
   */
  wasCalledWith(includePath, basePath) {
    return this.resolveHistory.some(call => 
      call.includePath === includePath && call.basePath === basePath
    );
  }

  /**
   *
   */
  reset() {
    this.resolveHistory = [];
    this.shouldFail = false;
    this.failureMessage = 'Resolution failed';
  }
}

/**
 *
 */
export class FakeRDFDataProvider {
  /**
   *
   */
  constructor() {
    this.data = new Map();
    this.queryHistory = [];
    this.shouldFail = false;
    this.failureMessage = 'Query failed';
  }

  /**
   *
   */
  async query(sparqlQuery) {
    const call = { query: sparqlQuery, timestamp: Date.now() };
    this.queryHistory.push(call);
    
    if (this.shouldFail) {
      throw new Error(this.failureMessage);
    }
    
    // Simple fake query processing
    const queryKey = this._extractQueryKey(sparqlQuery);
    return this.data.get(queryKey) || [];
  }

  /**
   *
   */
  setQueryResult(queryKey, result) {
    this.data.set(queryKey, result);
  }

  /**
   *
   */
  setFailure(shouldFail, message = 'Query failed') {
    this.shouldFail = shouldFail;
    this.failureMessage = message;
  }

  /**
   *
   */
  _extractQueryKey(sparqlQuery) {
    // Extract a simple key from SPARQL query for matching
    const match = sparqlQuery.match(/SELECT\s+\?([a-zA-Z]+)/);
    return match ? match[1] : 'default';
  }

  /**
   *
   */
  getQueryHistory() {
    return [...this.queryHistory];
  }

  /**
   *
   */
  getQueryCount() {
    return this.queryHistory.length;
  }

  /**
   *
   */
  reset() {
    this.data.clear();
    this.queryHistory = [];
    this.shouldFail = false;
    this.failureMessage = 'Query failed';
  }
}

/**
 *
 */
export class SpyRenderer {
  /**
   *
   */
  constructor(realRenderer) {
    this.realRenderer = realRenderer;
    this.calls = {
      plan: [],
      render: [],
      post: [],
      attest: []
    };
  }

  /**
   *
   */
  async plan(template, data) {
    const call = { template, data, timestamp: Date.now() };
    this.calls.plan.push(call);
    return await this.realRenderer.plan(template, data);
  }

  /**
   *
   */
  async render(plan) {
    const call = { plan, timestamp: Date.now() };
    this.calls.render.push(call);
    return await this.realRenderer.render(plan);
  }

  /**
   *
   */
  async post(rendered) {
    const call = { rendered, timestamp: Date.now() };
    this.calls.post.push(call);
    return await this.realRenderer.post(rendered);
  }

  /**
   *
   */
  async attest(output) {
    const call = { output, timestamp: Date.now() };
    this.calls.attest.push(call);
    return await this.realRenderer.attest(output);
  }

  /**
   *
   */
  wasMethodCalled(method) {
    return this.calls[method] && this.calls[method].length > 0;
  }

  /**
   *
   */
  getMethodCallCount(method) {
    return this.calls[method] ? this.calls[method].length : 0;
  }

  /**
   *
   */
  getMethodCalls(method) {
    return this.calls[method] ? [...this.calls[method]] : [];
  }

  /**
   *
   */
  reset() {
    this.calls = {
      plan: [],
      render: [],
      post: [],
      attest: []
    };
  }
}

/**
 *
 */
export class DeterministicTestData {
  /**
   *
   */
  static getShuffledObjectData() {
    // Create object with shuffled key order for determinism testing
    const data1 = { z: 3, a: 1, m: 2 };
    const data2 = { a: 1, m: 2, z: 3 };
    const data3 = { m: 2, z: 3, a: 1 };
    
    return { data1, data2, data3 };
  }

  /**
   *
   */
  static getArrayWithObjects() {
    return [
      { id: '2', name: 'Beta', value: 200 },
      { id: '1', name: 'Alpha', value: 100 },
      { id: '3', name: 'Gamma', value: 300 }
    ];
  }

  /**
   *
   */
  static getTemplateWithIncludes() {
    return {
      main: `
# Main Template
{% include "header.njk" %}
{{ content }}
{% include "footer.njk" %}
`,
      header: `<!-- Header: Generated at {{ __deterministic.buildTime }} -->`,
      footer: `<!-- Footer: Total includes processed: {{ __meta.includeCount || 0 }} -->`
    };
  }

  /**
   *
   */
  static getComplexNestedData() {
    return {
      project: {
        name: 'KGEN',
        version: '1.0.0',
        components: [
          { name: 'Templates', type: 'Core' },
          { name: 'RDF', type: 'Data' },
          { name: 'CLI', type: 'Interface' }
        ],
        metadata: {
          created: '2024-01-01T00:00:00.000Z',
          updated: '2024-01-01T00:00:00.000Z'
        }
      }
    };
  }

  /**
   *
   */
  static getNonDeterministicData() {
    return {
      timestamp: new Date(),
      random: Math.random(),
      uuid: crypto.randomUUID ? crypto.randomUUID() : 'fake-uuid'
    };
  }
}

/**
 *
 */
export class TestScenarios {
  /**
   *
   */
  static createDeterminismTest(renderer) {
    return {
      template: '{{ items | sort | join(", ") }}',
      data: {
        items: ['gamma', 'alpha', 'beta']
      },
      expectedOutput: 'alpha, beta, gamma\n'
    };
  }

  /**
   *
   */
  static createIncludeTest(templateLoader) {
    const templates = DeterministicTestData.getTemplateWithIncludes();
    
    // Setup stub loader
    templateLoader.addTemplate('main.njk', templates.main);
    templateLoader.addTemplate('header.njk', templates.header);
    templateLoader.addTemplate('footer.njk', templates.footer);
    
    return {
      template: templates.main,
      data: { content: 'Main content here' },
      allowedIncludes: ['header.njk', 'footer.njk']
    };
  }

  /**
   *
   */
  static createWhitespaceTest() {
    return {
      template: '  {{ value }}  \r\n\t{{ other }}\t  \r\n',
      data: { value: 'test', other: 'content' },
      expectedTransformations: ['line-endings-normalized', 'trailing-spaces-trimmed'],
      expectedFinalNewline: true
    };
  }

  /**
   *
   */
  static createValidationFailureTest() {
    return {
      template: '{{ missing_variable }}',
      data: { available_variable: 'value' },
      shouldFail: true,
      expectedError: /Missing required template variables/
    };
  }

  /**
   *
   */
  static createDeepIncludeTest(templateLoader) {
    // Test maximum include depth
    for (let i = 0; i <= 6; i++) {
      const nextInclude = i < 6 ? `{% include "level${i + 1}.njk" %}` : 'Max depth reached';
      templateLoader.addTemplate(`level${i}.njk`, 
        `Level ${i}\n${nextInclude}\n`
      );
    }
    
    return {
      template: '{% include "level0.njk" %}',
      data: {},
      shouldFail: true,
      expectedError: /Include depth limit exceeded/
    };
  }
}

// Test utility functions
/**
 *
 */
export class TestUtils {
  /**
   *
   */
  static async verifyDeterministicOutput(pipeline, template, data, iterations = 3) {
    const outputs = [];
    const hashes = new Set();
    
    for (let i = 0; i < iterations; i++) {
      const result = await pipeline.execute(template, data);
      if (!result.success) {
        throw new Error(`Iteration ${i + 1} failed: ${result.error}`);
      }
      
      outputs.push(result.content);
      hashes.add(result.contentHash);
    }
    
    return {
      deterministic: hashes.size === 1,
      uniqueOutputs: hashes.size,
      outputs,
      firstHash: hashes.values().next().value
    };
  }

  /**
   *
   */
  static async createMinimalRenderer(options = {}) {
    const stubLoader = new StubTemplateLoader(options.templates || {});
    const mockResolver = new MockIncludeResolver();
    const fakeProvider = new FakeRDFDataProvider();

    return {
      renderer: new (await import('../src/engine/renderer.js')).DeterministicRenderer({
        templateLoader: stubLoader,
        includeResolver: mockResolver,
        dataProvider: fakeProvider,
        ...options
      }),
      doubles: {
        loader: stubLoader,
        resolver: mockResolver,
        provider: fakeProvider
      }
    };
  }

  /**
   *
   */
  static assertHashConsistency(results) {
    if (results.length < 2) {
      throw new Error('Need at least 2 results for hash comparison');
    }
    
    const firstHash = results[0].contentHash;
    const allMatch = results.every(result => result.contentHash === firstHash);
    
    if (!allMatch) {
      const hashes = results.map((r, i) => `${i}: ${r.contentHash}`);
      throw new Error(`Hash mismatch detected:\n${hashes.join('\n')}`);
    }
    
    return true;
  }

  /**
   *
   */
  static createAttestationVerifier() {
    return {
      verify(attestation, content) {
        // Verify attestation structure
        const requiredFields = ['contentHash', 'contentDigest', 'algorithm', 'timestamp'];
        const missing = requiredFields.filter(field => !attestation[field]);
        
        if (missing.length > 0) {
          throw new Error(`Missing attestation fields: ${missing.join(', ')}`);
        }
        
        // Verify content hash matches
        const crypto = require('crypto');
        const expectedHash = crypto.createHash('sha256').update(content, 'utf8').digest('hex');
        
        if (attestation.contentHash !== expectedHash) {
          throw new Error(`Content hash mismatch: expected ${expectedHash}, got ${attestation.contentHash}`);
        }
        
        return true;
      }
    };
  }
}

export default {
  StubTemplateLoader,
  MockIncludeResolver,
  FakeRDFDataProvider,
  SpyRenderer,
  DeterministicTestData,
  TestScenarios,
  TestUtils
};
