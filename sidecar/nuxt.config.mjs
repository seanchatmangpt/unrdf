// @ts-check
import fs from 'fs';

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

    // TLS/mTLS configuration
    tlsCertPath: process.env.TLS_CERT_PATH || '',
    tlsKeyPath: process.env.TLS_KEY_PATH || '',
    tlsCaPath: process.env.TLS_CA_PATH || '',
    tlsDhParams: process.env.TLS_DH_PARAMS || '',
    tlsMinVersion: process.env.TLS_MIN_VERSION || 'TLSv1.3',
    tlsCiphers: process.env.TLS_CIPHERS || 'TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256',
    mtlsEnabled: process.env.MTLS_ENABLED === 'true',
    mtlsRequireClientCert: process.env.MTLS_REQUIRE_CLIENT_CERT === 'true',
    enforceHttps: process.env.ENFORCE_HTTPS === 'true',
    hstsMaxAge: parseInt(process.env.HSTS_MAX_AGE || '31536000'),
    hstsIncludeSubdomains: process.env.HSTS_INCLUDE_SUBDOMAINS !== 'false',
    hstsPreload: process.env.HSTS_PRELOAD !== 'false',

    // Public (exposed to client, but we don't have client)
    public: {}
  },

  // Nitro server configuration
  nitro: {
    // Production port
    port: process.env.PORT ? parseInt(process.env.PORT) : 3000,

    // HTTPS/TLS configuration
    ...(process.env.TLS_CERT_PATH && process.env.TLS_KEY_PATH && fs.existsSync(process.env.TLS_CERT_PATH) && fs.existsSync(process.env.TLS_KEY_PATH) ? {
      https: {
        key: fs.readFileSync(process.env.TLS_KEY_PATH, 'utf-8'),
        cert: fs.readFileSync(process.env.TLS_CERT_PATH, 'utf-8'),
        ...(process.env.TLS_CA_PATH && fs.existsSync(process.env.TLS_CA_PATH) ? {
          ca: fs.readFileSync(process.env.TLS_CA_PATH, 'utf-8')
        } : {}),
        ...(process.env.TLS_DH_PARAMS && fs.existsSync(process.env.TLS_DH_PARAMS) ? {
          dhparam: fs.readFileSync(process.env.TLS_DH_PARAMS, 'utf-8')
        } : {}),
        minVersion: process.env.TLS_MIN_VERSION || 'TLSv1.3',
        maxVersion: 'TLSv1.3',
        ciphers: process.env.TLS_CIPHERS || 'TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256',
        honorCipherOrder: true,
        requestCert: process.env.MTLS_REQUIRE_CLIENT_CERT === 'true',
        rejectUnauthorized: process.env.MTLS_REQUIRE_CLIENT_CERT === 'true',
        // Perfect Forward Secrecy
        ecdhCurve: 'prime256v1:secp384r1:secp521r1',
        // Session resumption disabled for security
        sessionTimeout: 0,
        ticketKeys: false
      }
    } : {}),

    // Enable experimental features
    experimental: {
      openAPI: true,
      tasks: true  // Enable Nitro scheduled tasks
    },

    // Scheduled tasks with SAFLA neural autonomic systems
    scheduledTasks: {
      // Evaluate knowledge hooks every 5 minutes
      'hooks:evaluate-periodic': {
        interval: '*/5 * * * *',
        task: 'hooks/evaluate-periodic'
      },
      // Refresh policy packs every hour
      'policies:refresh-packs': {
        interval: '0 * * * *',
        task: 'policies/refresh-packs'
      },
      // Archive old lockchain entries daily
      'lockchain:archive': {
        interval: '0 0 * * *',
        task: 'lockchain/archive'
      },
      // Self-healing health check every minute
      'health:self-heal': {
        interval: '*/1 * * * *',
        task: 'health/self-heal'
      }
    },

    // Prerender routes (none for API-only)
    prerender: {
      routes: []
    },

    // Node module resolution (don't bundle OpenTelemetry in tests)
    noExternal: process.env.NODE_ENV === 'test' ? [] : undefined
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
