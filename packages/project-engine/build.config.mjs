import { defineBuildConfig} from 'unbuild';

export default defineBuildConfig({
  entries: ['src/index.mjs'],
  outDir: 'dist',
  declaration: true,
  rollup: {
    emitCJS: false,
    inlineDependencies: false
  }
});
