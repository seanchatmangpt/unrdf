import { defineBuildConfig } from 'unbuild';

const isDev = process.env.NODE_ENV !== 'production';

export default defineBuildConfig({
  entries: [
    'src/index',
    'src/api/workflow-api',
    'src/ontology/yawl-ontology',
    'src/store/yawl-store',
    'src/types/yawl-types',
    'src/types/yawl-schemas',
    'src/hooks/yawl-hooks',
    'src/resources/yawl-resources',
    'src/cancellation/index',
    'src/receipt',
  ],
  declaration: isDev ? false : 'compatible', // Skip types in dev for speed  clean: true,
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
