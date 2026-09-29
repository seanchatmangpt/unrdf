/**
 * @fileoverview Version information sourced from the playground package.json
 *
 * Never hardcode versions in the CLI: package.json is the single source.
 *
 * @module playground/version
 */

import { createRequire } from 'node:module';

const require = createRequire(import.meta.url);
const pkg = require('../package.json');

/** Playground package version. */
export const VERSION = pkg.version;

/**
 * Declared dependency versions (range prefix stripped), keyed by package name.
 *
 * @param {...string} names - Dependency names to look up
 * @returns {Record<string, string>} Version per name ('unknown' if not declared)
 */
export function dependencyVersions(...names) {
  return Object.fromEntries(
    names.map(name => {
      const range = pkg.dependencies?.[name] ?? pkg.devDependencies?.[name];
      return [name, range ? range.replace(/^[\^~>=<\s]+/, '') : 'unknown'];
    })
  );
}
