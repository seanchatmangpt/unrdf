import { defineBuildConfig } from 'unbuild';

const isDev = process.env.NODE_ENV !== 'production';

export default defineBuildConfig({
  entries: [
    'src/index',
    'src/client',
    'src/hdit/index',
  ],
  declaration: isDev ? false : true, // Skip type generation in dev for speed
  clean: true,
  rollup: {
    emitCJS: false,
    inlineDependencies: false,
    esbuild: {
      target: 'esnext',
      minify: false,
    },
  },
  outDir: 'dist',
  failOnWarn: false,
});
