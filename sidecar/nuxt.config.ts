export default defineNuxtConfig({
  compatibilityDate: '2025-01-01',

  // API-only mode (no pages/SSR overhead)
  ssr: false,
  pages: false,

  // Runtime configuration
  runtimeConfig: {
    // Server-side only
    otelEndpoint: process.env.OTEL_EXPORTER_OTLP_ENDPOINT || '',
    otelServiceName: process.env.OTEL_SERVICE_NAME || 'kgc-sidecar',
    kgcEnableTelemetry: process.env.KGC_ENABLE_TELEMETRY === 'true',
    kgcGitRepoUrl: process.env.KGC_GIT_REPO_URL || '',
    kgcSandboxTimeout: parseInt(process.env.KGC_SANDBOX_TIMEOUT || '30000'),
    kgcSandboxMemoryLimit: parseInt(process.env.KGC_SANDBOX_MEMORY_LIMIT || '67108864'),
    kgcBasePath: process.env.KGC_BASE_PATH || process.cwd(),

    // Public (exposed to client, but we don't have client)
    public: {}
  },

  // Nitro server configuration
  nitro: {
    // Production port
    port: process.env.PORT ? parseInt(process.env.PORT) : 3000,

    // Enable experimental features
    experimental: {
      openAPI: true
    },

    // Prerender routes (none for API-only)
    prerender: {
      routes: []
    }
  },

  // TypeScript configuration
  typescript: {
    strict: true,
    typeCheck: true
  },

  // Development tools
  devtools: {
    enabled: true
  },

  // Module auto-imports
  imports: {
    dirs: ['types']
  }
})
