import { defineBuildConfig } from 'unbuild';

export default defineBuildConfig({
  entries: ['src/index.mjs'],
  outDir: 'dist',
  declaration: true,
  failOnWarn: false,
  rollup: {
    emitCJS: false,
    inlineDependencies: false
  }
});
