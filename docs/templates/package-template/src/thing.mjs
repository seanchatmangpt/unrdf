/**
 * @file Core thing implementation
 * @module @unrdf/package-name/thing
 */

import { validateThing } from './validation.mjs';
import { DEFAULT_OPTIONS } from './constants.mjs';

/**
 * Creates a new thing
 *
 * @param {Object} [options={}] - Configuration options
 * @param {string} [options.option='default'] - Option description
 * @returns {Thing} Thing instance
 * @throws {TypeError} If options are invalid
 * @example
 * const thing = createThing({ option: 'value' });
 * thing.doSomething();
 */
export function createThing(options = {}) {
  const config = { ...DEFAULT_OPTIONS, ...options };

  // Validate configuration
  if (!validateThing(config)) {
    throw new TypeError('Invalid thing options');
  }

  return {
    option: config.option,

    /**
     * Does something
     *
     * @returns {void}
     */
    doSomething() {
      console.log(`Doing something with: ${this.option}`);
    },
  };
}
