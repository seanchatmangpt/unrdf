/**
 * @file Package constants
 * @module @unrdf/package-name/constants
 */

import { createRequire } from 'node:module';

const require = createRequire(import.meta.url);
const pkg = require('../package.json');

/**
 * Default configuration options
 */
export const DEFAULT_OPTIONS = {
  option: 'default',
};

/**
 * Package version
 */
export const VERSION = pkg.version;
