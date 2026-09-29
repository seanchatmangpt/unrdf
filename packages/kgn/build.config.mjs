import { defineBuildConfig } from 'unbuild';

export default defineBuildConfig({
  entries: ['src/index.mjs'],
  outDir: 'dist',
  declaration: true,
  // Workspace packages are resolved at runtime; bundling them pulls in oxigraph's node.d.ts
  externals: ['@unrdf/core', '@unrdf/oxigraph', 'oxigraph'],
  rollup: {
    emitCJS: false,
    inlineDependencies: false
  }
});
