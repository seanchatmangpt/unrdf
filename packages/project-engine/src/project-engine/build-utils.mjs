/**
 * @file Build Utilities - Package building and verification
 * @module @unrdf/project-engine/build-utils
 */

import { readFile, readdir, access, stat } from 'node:fs/promises';
import { join } from 'node:path';
import { z } from 'zod';

/**
 * Package verification schema
 */
const VerificationResultSchema = z.object({
  valid: z.boolean(),
  errors: z.array(z.string()),
  warnings: z.array(z.string()),
  checkedFiles: z.array(z.string()),
});

/**
 * Build single package
 * @param {string} packagePath - Path to package directory
 * @returns {Promise<Object>} Build result
 *
 * @throws {TypeError} If packagePath is not a string
 * @throws {Error} If build fails
 *
 * @example
 * const result = await buildPackage('./packages/core');
 * console.log('Build status:', result.success);
 */
export async function buildPackage(packagePath) {
  if (typeof packagePath !== 'string') {
    throw new TypeError('buildPackage: packagePath must be a string');
  }

  try {
    const { execFile } = await import('node:child_process');
    const { promisify } = await import('node:util');
    const execFileAsync = promisify(execFile);

    // Check if package has build script
    const packageJsonPath = join(packagePath, 'package.json');
    const packageJsonContent = await readFile(packageJsonPath, 'utf-8');
    const packageJson = JSON.parse(packageJsonContent);

    if (!packageJson.scripts || !packageJson.scripts.build) {
      return {
        success: true,
        message: 'No build script defined',
        output: '',
      };
    }

    // Run build script
    const { stdout, stderr } = await execFileAsync('pnpm', ['run', 'build'], {
      cwd: packagePath,
    });

    return {
      success: true,
      message: 'Build completed successfully',
      output: stdout,
      errors: stderr,
    };
  } catch (error) {
    throw new Error(`buildPackage failed: ${error.message}`);
  }
}

/**
 * Verify package integrity
 * @param {string} packagePath - Path to package directory
 * @returns {Promise<Object>} Verification result with errors/warnings
 *
 * @throws {TypeError} If packagePath is not a string
 * @throws {Error} If verification fails
 *
 * @example
 * const result = await verifyPackage('./packages/core');
 * console.log('Valid:', result.valid);
 * console.log('Errors:', result.errors);
 */
export async function verifyPackage(packagePath) {
  if (typeof packagePath !== 'string') {
    throw new TypeError('verifyPackage: packagePath must be a string');
  }

  const errors = [];
  const warnings = [];
  const checkedFiles = [];

  try {
    // Check package.json exists
    const packageJsonPath = join(packagePath, 'package.json');
    try {
      await access(packageJsonPath);
      checkedFiles.push('package.json');

      const content = await readFile(packageJsonPath, 'utf-8');
      const packageJson = JSON.parse(content);

      // Validate required fields
      if (!packageJson.name) {
        errors.push('package.json missing "name" field');
      }
      if (!packageJson.version) {
        errors.push('package.json missing "version" field');
      }
      if (!packageJson.type || packageJson.type !== 'module') {
        errors.push('package.json must have "type": "module"');
      }

      // Check exports field
      if (!packageJson.exports) {
        warnings.push('package.json missing "exports" field');
      }

      // Verify main entry point exists
      if (packageJson.main) {
        const mainPath = join(packagePath, packageJson.main);
        try {
          await access(mainPath);
          checkedFiles.push(packageJson.main);
        } catch {
          errors.push(`Main entry point not found: ${packageJson.main}`);
        }
      }

      // Check for README
      const readmePath = join(packagePath, 'README.md');
      try {
        await access(readmePath);
        checkedFiles.push('README.md');
      } catch {
        warnings.push('README.md not found');
      }

      // Check for LICENSE
      const licensePath = join(packagePath, 'LICENSE');
      try {
        await access(licensePath);
        checkedFiles.push('LICENSE');
      } catch {
        warnings.push('LICENSE file not found');
      }

      // Check src directory exists
      const srcPath = join(packagePath, 'src');
      try {
        const srcStat = await stat(srcPath);
        if (!srcStat.isDirectory()) {
          errors.push('src must be a directory');
        }
        checkedFiles.push('src/');
      } catch {
        errors.push('src directory not found');
      }

      // Check test directory
      const testPath = join(packagePath, 'test');
      try {
        const testStat = await stat(testPath);
        if (!testStat.isDirectory()) {
          warnings.push('test should be a directory');
        }
        checkedFiles.push('test/');
      } catch {
        warnings.push('test directory not found');
      }
    } catch {
      errors.push('package.json not found or invalid JSON');
    }

    const result = {
      valid: errors.length === 0,
      errors,
      warnings,
      checkedFiles,
    };

    return VerificationResultSchema.parse(result);
  } catch (error) {
    throw new Error(`verifyPackage failed: ${error.message}`);
  }
}

/**
 * List all packages in monorepo
 * @param {string} [monorepoPath='.'] - Path to monorepo root
 * @returns {Promise<Array<Object>>} List of packages with metadata
 *
 * @throws {TypeError} If monorepoPath is not a string
 * @throws {Error} If monorepo cannot be scanned
 *
 * @example
 * const packages = await listPackages();
 * packages.forEach(pkg => console.log(pkg.name, pkg.path));
 */
export async function listPackages(monorepoPath = '.') {
  if (typeof monorepoPath !== 'string') {
    throw new TypeError('listPackages: monorepoPath must be a string');
  }

  try {
    const packagesPath = join(monorepoPath, 'packages');
    const entries = await readdir(packagesPath, { withFileTypes: true });

    const packages = [];

    for (const entry of entries) {
      if (entry.isDirectory()) {
        const packagePath = join(packagesPath, entry.name);
        const packageJsonPath = join(packagePath, 'package.json');

        try {
          const content = await readFile(packageJsonPath, 'utf-8');
          const packageJson = JSON.parse(content);

          packages.push({
            name: packageJson.name,
            version: packageJson.version,
            description: packageJson.description || '',
            path: packagePath,
            private: packageJson.private || false,
          });
        } catch {
          // Skip invalid packages
        }
      }
    }

    return packages;
  } catch (error) {
    throw new Error(`listPackages failed: ${error.message}`);
  }
}
