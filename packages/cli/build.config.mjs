/**
 * @file Build configuration for @unrdf/cli
 * @description
 * Unbuild configuration for the CLI package.
 * Specifies the entry points to build.
 */

import { defineBuildConfig } from 'unbuild';

export default defineBuildConfig({
  declaration: true,
  failOnWarn: false,
  rollup: {
    emitCJS: false,
    resolve: {
      preferBuiltins: true,
    },
    external: [
      'zod',
      'hash-wasm',
      'citty',
      '@unrdf/core',
      '@unrdf/oxigraph',
      '@unrdf/v6-core',
      '@unrdf/decision-fabric',
      '@unrdf/federation',
      '@unrdf/hooks',
      '@unrdf/streaming',
      '@unrdf/knowledge-engine',
      '@unrdf/project-engine',
    ],
  },
  entries: [
    'src/index.mjs',
    'src/cli.mjs',
  ],
});
