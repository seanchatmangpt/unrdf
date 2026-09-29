export default {
  // Nitro configuration for hooks runtime
  compatibilityDate: '2025-01-01',
  // Routes live in server/routes (nitropack defaults srcDir to the project root)
  srcDir: 'server',
  dev: true,
  // The route handlers are .mjs files; unimport only transforms .js/.ts by
  // default, so defineEventHandler/createError/readBody would be undefined.
  imports: {
    include: [/\.[cm]?[jt]sx?$/]
  },
  experimental: {
    wasm: true
  },
  runtimeConfig: {
    // Runtime config for hooks engine
    hooks: {
      dataDir: './data',
      maxHooks: 100,
      timeoutMs: 30000
    }
  }
}
