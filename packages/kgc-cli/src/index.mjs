/**
 * @fileoverview KGC CLI - Main exports.
 */

export { Registry, ExtensionSchema, createEnvelope } from './lib/registry.mjs';
export { loadManifest, extensions, overrides, getLoadOrder } from './manifest/extensions.mjs';
export { buildCittyTree, initializeRegistry } from './cli.mjs';

export default {
  name: 'kgc-cli',
  version: '5.0.1'
};
