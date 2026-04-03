import { defineBuildConfig } from 'unbuild';

export default defineBuildConfig({
  entries: ['src/index.mjs'],
  outDir: 'dist',
  declaration: true,
  rollup: {
    emitCJS: false,
    inlineDependencies: true
  },
  externals: [
    '@unrdf/knowledge-engine',
    '@unrdf/atomvm',
    '../knowledge-engine/index.mjs',
    '../knowledge-engine/define-hook.mjs',
    '../atomvm/playground/src/kgc4d-bridge.mjs',
    '../atomvm/src/node-runtime.mjs'
  ],
  failOnWarn: false
});
