// semantic-extractor.mjs
// Extract what packages actually compute, not what they claim

export class SemanticExtractor {
  constructor() {
    this.packages = new Map();
    this.exports = new Map();
    this.computations = new Map();
    this.contradictions = [];
  }

  async extract(packageName, modulePath) {
    try {
      const module = await import(modulePath);
      const signature = this._getSignature(module);
      const claims = this._parseMetadata(packageName);
      const actual = this._analyzeExports(module, signature);

      const gap = this._findGap(claims, actual);
      if (gap.exists) {
        this.contradictions.push({
          package: packageName,
          claimed: claims,
          actual: actual,
          gap: gap.description
        });
      }

      this.exports.set(packageName, actual);
      return { packageName, signature, gap };
    } catch (e) {
      this.contradictions.push({
        package: packageName,
        error: e.message,
        type: 'extraction_failure'
      });
      return null;
    }
  }

  _getSignature(module) {
    const exports = {};
    for (const [key, value] of Object.entries(module)) {
      if (typeof value === 'function') {
        exports[key] = {
          type: 'function',
          arity: value.length,
          isAsync: value.constructor.name === 'AsyncFunction'
        };
      } else if (typeof value === 'object' && value !== null) {
        exports[key] = {
          type: 'object',
          keys: Object.keys(value).length
        };
      }
    }
    return exports;
  }

  _analyzeExports(module, signature) {
    const computed = [];
    for (const [name, meta] of Object.entries(signature)) {
      if (meta.type === 'function') {
        computed.push({
          name,
          produces: this._inferProduction(name),
          consumes: this._inferConsumption(name),
          isAsync: meta.isAsync
        });
      }
    }
    return computed;
  }

  _inferProduction(name) {
    const patterns = {
      'create': 'instance',
      'make': 'instance',
      'build': 'structure',
      'generate': 'content',
      'query': 'result',
      'search': 'results',
      'analyze': 'analysis',
      'transform': 'transformed_data',
      'resolve': 'resolution'
    };

    for (const [pattern, produces] of Object.entries(patterns)) {
      if (name.toLowerCase().includes(pattern)) {
        return produces;
      }
    }
    return 'unknown';
  }

  _inferConsumption(name) {
    const patterns = {
      'from': 'stream',
      'parse': 'serialized',
      'load': 'stored',
      'fetch': 'remote',
      'read': 'file'
    };

    for (const [pattern, consumes] of Object.entries(patterns)) {
      if (name.toLowerCase().includes(pattern)) {
        return consumes;
      }
    }
    return 'unknown';
  }

  _findGap(claimed, actual) {
    if (!claimed || !actual) return { exists: false };

    const claimedCount = claimed.capabilities?.length || 0;
    const actualCount = actual.length;

    if (claimedCount !== actualCount) {
      return {
        exists: true,
        description: `claimed ${claimedCount} capabilities, found ${actualCount}`
      };
    }

    return { exists: false };
  }

  _parseMetadata(packageName) {
    // Stub: would read from ontology
    return null;
  }

  getContradictions() {
    return this.contradictions;
  }

  getSemanticsFor(packageName) {
    return this.exports.get(packageName);
  }
}
