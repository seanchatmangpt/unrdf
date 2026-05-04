/**
 * Golden Test Validator
 * Ensures byte-identical outputs across test runs and platforms
 */
import fs from 'fs/promises';
import path from 'path';
import crypto from 'crypto';
import { diffLines } from 'diff';

export class GoldenTestValidator {
  constructor(options = {}) {
    this.goldenDir = options.goldenDir || path.join(process.cwd(), 'tests/bdd/golden');
    this.updateGolden = options.updateGolden || false;
    this.platform = process.platform;
    this.encoding = options.encoding || 'utf8';
  }

  /**
   * Validate output against golden file
   */
  async validate(actualOutput, goldenFileName, options = {}) {
    const goldenPath = path.join(this.goldenDir, goldenFileName);
    const platformSpecificPath = this.getPlatformSpecificPath(goldenPath);

    try {
      // First try platform-specific golden file
      let expectedOutput;
      try {
        expectedOutput = await fs.readFile(platformSpecificPath, this.encoding);
      } catch (error) {
        // Fall back to generic golden file
        expectedOutput = await fs.readFile(goldenPath, this.encoding);
      }

      // Normalize line endings for cross-platform compatibility
      const normalizedActual = this.normalizeOutput(actualOutput, options);
      const normalizedExpected = this.normalizeOutput(expectedOutput, options);

      // Generate content hashes for byte-identical verification
      const actualHash = this.generateHash(normalizedActual);
      const expectedHash = this.generateHash(normalizedExpected);

      if (actualHash === expectedHash) {
        return {
          valid: true,
          hash: actualHash,
          message: 'Output matches golden file exactly'
        };
      }

      // If hashes don't match, provide detailed diff
      const diff = this.generateDiff(normalizedExpected, normalizedActual);

      if (this.updateGolden) {
        await this.updateGoldenFile(goldenPath, actualOutput);
        return {
          valid: true,
          updated: true,
          message: 'Golden file updated with new output'
        };
      }

      return {
        valid: false,
        expectedHash,
        actualHash,
        diff,
        message: 'Output does not match golden file'
      };

    } catch (error) {
      if (error.code === 'ENOENT') {
        // Golden file doesn't exist
        if (this.updateGolden || options.createIfMissing) {
          await this.createGoldenFile(goldenPath, actualOutput);
          return {
            valid: true,
            created: true,
            message: 'Golden file created'
          };
        }

        throw new Error(`Golden file not found: ${goldenFileName}`);
      }

      throw error;
    }
  }

  /**
   * Generate multiple golden test variations
   */
  async validatePermutations(actualOutputs, goldenFileBase, options = {}) {
    const results = {};

    for (const [variation, output] of Object.entries(actualOutputs)) {
      const goldenFileName = `${goldenFileBase}-${variation}.golden`;
      results[variation] = await this.validate(output, goldenFileName, options);
    }

    return results;
  }

  /**
   * Validate cross-platform consistency
   */
  async validateCrossPlatform(actualOutput, goldenFileBase) {
    const platforms = ['linux', 'darwin', 'win32'];
    const results = {};

    for (const platform of platforms) {
      const goldenFileName = `${goldenFileBase}-${platform}.golden`;
      const goldenPath = path.join(this.goldenDir, goldenFileName);

      try {
        const expectedOutput = await fs.readFile(goldenPath, this.encoding);
        const normalizedActual = this.normalizeOutput(actualOutput);
        const normalizedExpected = this.normalizeOutput(expectedOutput);

        results[platform] = {
          valid: normalizedActual === normalizedExpected,
          hash: this.generateHash(normalizedActual),
          expectedHash: this.generateHash(normalizedExpected)
        };
      } catch (error) {
        results[platform] = {
          valid: false,
          error: error.message
        };
      }
    }

    return results;
  }

  /**
   * Normalize output for consistent comparison
   */
  normalizeOutput(output, options = {}) {
    let normalized = output;

    // Normalize line endings
    normalized = normalized.replace(/\r\n/g, '\n').replace(/\r/g, '\n');

    // Normalize paths if requested
    if (options.normalizePaths) {
      normalized = normalized.replace(/\\/g, '/');
    }

    // Remove timestamps if requested
    if (options.removeTimestamps) {
      normalized = normalized.replace(/\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d{3})?Z?/g, 'TIMESTAMP');
    }

    // Remove UUIDs if requested
    if (options.removeUUIDs) {
      normalized = normalized.replace(/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/gi, 'UUID');
    }

    // Trim whitespace if requested
    if (options.trim) {
      normalized = normalized.trim();
    }

    return normalized;
  }

  /**
   * Generate content hash for byte-identical verification
   */
  generateHash(content) {
    return crypto.createHash('sha256').update(content, 'utf8').digest('hex');
  }

  /**
   * Generate detailed diff between expected and actual
   */
  generateDiff(expected, actual) {
    const diff = diffLines(expected, actual);
    let diffText = '';

    diff.forEach((part) => {
      const prefix = part.added ? '+ ' : part.removed ? '- ' : '  ';
      diffText += part.value
        .split('\n')
        .map(line => line ? prefix + line : '')
        .join('\n');
    });

    return diffText;
  }

  /**
   * Get platform-specific golden file path
   */
  getPlatformSpecificPath(goldenPath) {
    const ext = path.extname(goldenPath);
    const base = path.basename(goldenPath, ext);
    const dir = path.dirname(goldenPath);

    return path.join(dir, `${base}-${this.platform}${ext}`);
  }

  /**
   * Update golden file with new content
   */
  async updateGoldenFile(goldenPath, content) {
    await fs.mkdir(path.dirname(goldenPath), { recursive: true });
    await fs.writeFile(goldenPath, content, this.encoding);
  }

  /**
   * Create new golden file
   */
  async createGoldenFile(goldenPath, content) {
    await fs.mkdir(path.dirname(goldenPath), { recursive: true });
    await fs.writeFile(goldenPath, content, this.encoding);
  }

  /**
   * Generate golden test report
   */
  async generateReport(testResults, outputPath) {
    const report = {
      timestamp: new Date().toISOString(),
      platform: this.platform,
      summary: {
        total: Object.keys(testResults).length,
        passed: Object.values(testResults).filter(r => r.valid).length,
        failed: Object.values(testResults).filter(r => !r.valid).length,
        updated: Object.values(testResults).filter(r => r.updated).length,
        created: Object.values(testResults).filter(r => r.created).length
      },
      results: testResults
    };

    if (outputPath) {
      await fs.writeFile(outputPath, JSON.stringify(report, null, 2));
    }

    return report;
  }

  /**
   * Batch validate multiple outputs
   */
  async batchValidate(testCases) {
    const results = {};

    for (const [testName, { output, goldenFile, options }] of Object.entries(testCases)) {
      try {
        results[testName] = await this.validate(output, goldenFile, options);
      } catch (error) {
        results[testName] = {
          valid: false,
          error: error.message
        };
      }
    }

    return results;
  }

  /**
   * Verify deterministic output by running test multiple times
   */
  async verifyDeterministic(testFunction, iterations = 10) {
    const outputs = [];

    for (let i = 0; i < iterations; i++) {
      outputs.push(await testFunction());
    }

    // Check that all outputs are identical
    const firstOutput = outputs[0];
    const firstHash = this.generateHash(firstOutput);

    const allIdentical = outputs.every(output => {
      return this.generateHash(output) === firstHash;
    });

    return {
      deterministic: allIdentical,
      iterations,
      uniqueOutputs: [...new Set(outputs.map(o => this.generateHash(o)))].length,
      firstHash,
      outputs: outputs.slice(0, 3) // Include first few outputs for debugging
    };
  }
}