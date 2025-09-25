export default {
  // Nitro configuration for hooks runtime
  dev: true,
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
  },
  nitro: {
    experimental: {
      wasm: true
    }
  }
}
