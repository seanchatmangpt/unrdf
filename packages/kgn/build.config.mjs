import { defineBuildConfig } from 'unbuild';

export default defineBuildConfig({
  entries: ['src/index.js'],
  outDir: 'dist',
  declaration: true,
  rollup: {
    emitCJS: false,
    inlineDependencies: false
  }
});
