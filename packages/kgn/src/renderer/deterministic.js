/**
 * Deterministic Renderer - Ensures identical output across template runs
 * Migrated from ~/unjucks with enhanced deterministic guarantees
 */

import crypto from 'crypto';

export class DeterministicRenderer {
  constructor(options = {}) {
    this.staticBuildTime = options.staticBuildTime || '2024-01-01T00:00:00.000Z';
    this.blockNonDeterministic = options.blockNonDeterministic !== false;
    this.strictMode = options.strictMode !== false;
  }

  /**
   * Render template with deterministic context
   */
  async render(nunjucksEnv, templateContent, context) {
    try {
      // Create deterministic context
      const deterministicContext = this.createDeterministicContext(context);
      
      // Block non-deterministic operations if enabled
      if (this.blockNonDeterministic) {
        this.validateDeterministicContext(deterministicContext);
      }

      // Render with enhanced error handling
      const rendered = nunjucksEnv.renderString(templateContent, deterministicContext);
      
      return this.postProcessOutput(rendered);

    } catch (error) {
      if (this.strictMode) {
        throw new Error(`Deterministic rendering failed: ${error.message}`);
      }
      
      // Fallback to non-deterministic rendering
      console.warn('[DeterministicRenderer] Fallback to non-deterministic rendering:', error.message);
      return nunjucksEnv.renderString(templateContent, context);
    }
  }

  /**
   * Render string template with deterministic context
   */
  renderString(nunjucksEnv, templateContent, context) {
    const deterministicContext = this.createDeterministicContext(context);
    const rendered = nunjucksEnv.renderString(templateContent, deterministicContext);
    return this.postProcessOutput(rendered);
  }

  /**
   * Create deterministic context by replacing non-deterministic values
   */
  createDeterministicContext(context) {
    const deterministicContext = { ...context };

    // Add deterministic metadata
    deterministicContext.__deterministic = {
      buildTime: this.staticBuildTime,
      renderTime: this.staticBuildTime, // Use same time for consistency
      hash: this.hashContent(JSON.stringify(context)),
      mode: 'deterministic'
    };

    // Override potentially non-deterministic built-ins
    this.overrideNonDeterministicValues(deterministicContext);

    return deterministicContext;
  }

  /**
   * Override non-deterministic values in context
   */
  overrideNonDeterministicValues(context) {
    // Replace any Date objects with static date
    this.replaceInObject(context, (key, value) => {
      if (value instanceof Date) {
        return new Date(this.staticBuildTime);
      }
      
      // Replace current timestamp references
      if (typeof value === 'string' && this.looksLikeTimestamp(value)) {
        return this.staticBuildTime;
      }
      
      return value;
    });
  }

  /**
   * Recursively replace values in object
   */
  replaceInObject(obj, replacer) {
    for (const [key, value] of Object.entries(obj)) {
      if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
        this.replaceInObject(value, replacer);
      } else if (Array.isArray(value)) {
        for (let i = 0; i < value.length; i++) {
          if (typeof value[i] === 'object' && value[i] !== null) {
            this.replaceInObject(value[i], replacer);
          } else {
            value[i] = replacer(i.toString(), value[i]);
          }
        }
      } else {
        obj[key] = replacer(key, value);
      }
    }
  }

  /**
   * Check if string looks like a timestamp
   */
  looksLikeTimestamp(value) {
    // ISO 8601 format
    if (/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d{3})?Z?$/.test(value)) {
      return true;
    }
    
    // Unix timestamp (seconds or milliseconds)
    if (/^\d{10,13}$/.test(value)) {
      return true;
    }
    
    return false;
  }

  /**
   * Validate context doesn't contain non-deterministic operations
   */
  validateDeterministicContext(context) {
    const nonDeterministicPatterns = [
      /Math\.random/,
      /Date\.now/,
      /new Date\(\)/,
      /crypto\.randomUUID/,
      /Math\.floor\(Math\.random/
    ];

    const contextStr = JSON.stringify(context);
    
    nonDeterministicPatterns.forEach(pattern => {
      if (pattern.test(contextStr)) {
        throw new Error(`Non-deterministic operation detected: ${pattern.source}`);
      }
    });
  }

  /**
   * Post-process rendered output for consistency
   */
  postProcessOutput(output) {
    // Normalize line endings
    let processed = output.replace(/\r\n|\r/g, '\n');
    
    // Remove trailing whitespace from lines (but keep empty lines)
    processed = processed.replace(/[^\S\n]+$/gm, '');
    
    // Ensure consistent final newline
    if (processed.length > 0 && !processed.endsWith('\n')) {
      processed += '\n';
    }

    return processed;
  }

  /**
   * Get deterministic time string
   */
  getDeterministicTime() {
    return this.staticBuildTime;
  }

  /**
   * Generate deterministic hash
   */
  hashContent(content) {
    return crypto.createHash('sha256').update(content, 'utf8').digest('hex');
  }

  /**
   * Verify template produces identical output across runs
   */
  async verifyDeterminism(nunjucksEnv, templateContent, context, iterations = 3) {
    const outputs = [];
    const hashes = new Set();

    for (let i = 0; i < iterations; i++) {
      const output = await this.render(nunjucksEnv, templateContent, context);
      const hash = this.hashContent(output);
      
      outputs.push({ iteration: i + 1, output, hash });
      hashes.add(hash);
    }

    const isDeterministic = hashes.size === 1;
    
    return {
      isDeterministic,
      iterations,
      uniqueOutputs: hashes.size,
      outputs: outputs.slice(0, 2), // Include first 2 for comparison
      firstHash: outputs[0]?.hash,
      allHashesSame: isDeterministic
    };
  }

  /**
   * Create reproducibility report
   */
  async createReproducibilityReport(nunjucksEnv, templateContent, context) {
    const verification = await this.verifyDeterminism(nunjucksEnv, templateContent, context, 5);
    
    const report = {
      timestamp: this.getDeterministicTime(),
      templateHash: this.hashContent(templateContent),
      contextHash: this.hashContent(JSON.stringify(context)),
      deterministicSettings: {
        staticBuildTime: this.staticBuildTime,
        blockNonDeterministic: this.blockNonDeterministic,
        strictMode: this.strictMode
      },
      verification,
      reproducible: verification.isDeterministic,
      confidence: verification.isDeterministic ? 'HIGH' : 'LOW'
    };

    return report;
  }

  /**
   * Get renderer statistics
   */
  getStats() {
    return {
      staticBuildTime: this.staticBuildTime,
      blockNonDeterministic: this.blockNonDeterministic,
      strictMode: this.strictMode
    };
  }
}

export default DeterministicRenderer;