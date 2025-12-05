/**
 * @file KGC Manager Initialization Plugin
 * @description Initialize all KGC managers on server startup with Vault secret integration
 */

import { defineNitroPlugin, useRuntimeConfig } from '#imports'
import { setManagers } from '../utils/managers.mjs'
import { createVaultClient } from '../utils/vault-client.mjs'

// Import KGC library managers from parent project
import { KnowledgeHookManager } from '../../../packages/knowledge-engine/knowledge-hook-manager.mjs'
import { TransactionManager } from '../../../packages/knowledge-engine/transaction.mjs'
import { PolicyPack } from '../../../packages/knowledge-engine/policy-pack.mjs'
import { EffectSandbox } from '../../../packages/knowledge-engine/effect-sandbox.mjs'
import { LockchainWriter } from '../../../packages/knowledge-engine/lockchain-writer.mjs'
import { ResolutionLayer } from '../../../packages/knowledge-engine/resolution-layer.mjs'
import { ObservabilityManager } from '../../../packages/knowledge-engine/observability.mjs'

/**
 * Initialize Vault client and retrieve secrets
 * @param {Object} config - Runtime configuration
 * @returns {Promise<Object>} Secrets from Vault
 */
async function initializeVaultSecrets(config) {
  if (!config.vaultEnabled) {
    console.log('[KGC] Vault integration disabled, using environment variables')
    return {
      apiKey: config.apiKey || process.env.API_KEY,
      encryptionKey: config.encryptionKey || process.env.ENCRYPTION_KEY,
      database: {
        url: config.databaseUrl || process.env.DATABASE_URL
      }
    }
  }

  try {
    console.log('[KGC] Initializing Vault client...')

    const vaultClient = await createVaultClient({
      endpoint: config.vaultAddress || process.env.VAULT_ADDR || 'http://vault:8200',
      token: config.vaultToken || process.env.VAULT_TOKEN,
      mountPath: config.vaultMountPath || 'kgc',
      namespace: config.vaultNamespace,
      cacheTTL: config.vaultCacheTTL || 300000,
      enableQuorum: config.vaultEnableQuorum !== false,
      quorumShares: config.vaultQuorumShares || 5,
      quorumThreshold: config.vaultQuorumThreshold || 3
    })

    // Retrieve all secrets
    const secrets = await vaultClient.getAllSecrets()

    console.log('[KGC] Vault secrets retrieved successfully')

    // Store vault client globally for secret rotation
    globalThis.__vaultClient = vaultClient

    return secrets
  } catch (error) {
    console.error('[KGC] Vault initialization failed:', error.message)

    // Fallback to environment variables
    console.log('[KGC] Falling back to environment variables')
    return {
      apiKey: config.apiKey || process.env.API_KEY,
      encryptionKey: config.encryptionKey || process.env.ENCRYPTION_KEY,
      database: {
        url: config.databaseUrl || process.env.DATABASE_URL
      }
    }
  }
}

/**
 * Initialize KGC managers
 * @param {Object} nitroApp - Nitro app instance
 */
export default defineNitroPlugin(async (nitroApp) => {
  const config = useRuntimeConfig()

  console.log('[KGC] Initializing managers...')

  try {
    // Initialize Vault and retrieve secrets
    const secrets = await initializeVaultSecrets(config)

    // Initialize observability first
    const observability = new ObservabilityManager({
      enableTelemetry: config.kgcEnableTelemetry,
      serviceName: config.otelServiceName,
      otlpEndpoint: config.otelEndpoint
    })

    // Initialize core managers with secrets from Vault
    const hookManager = new KnowledgeHookManager({
      apiKey: secrets.apiKey,
      encryptionKey: secrets.encryptionKey
    })

    const transactionManager = new TransactionManager({
      databaseUrl: secrets.database.url
    })

    const policyPack = new PolicyPack({
      id: '00000000-0000-0000-0000-000000000000',
      meta: {
        id: 'default',
        name: 'default-policy-pack',
        version: '1.0.0',
        description: 'Default policy pack for KGC Sidecar'
      },
      config: {
        apiKey: secrets.apiKey,
        encryptionKey: secrets.encryptionKey
      },
      hooks: []
    })

    const effectSandbox = new EffectSandbox({
      timeout: config.kgcSandboxTimeout,
      memoryLimit: config.kgcSandboxMemoryLimit
    })

    const lockchainWriter = config.kgcGitRepoUrl
      ? new LockchainWriter({
          repoUrl: config.kgcGitRepoUrl,
          apiKey: secrets.apiKey
        })
      : null

    const resolutionLayer = new ResolutionLayer()

    // Store in singleton
    setManagers({
      hookManager,
      transactionManager,
      policyPack,
      effectSandbox,
      lockchainWriter,
      resolutionLayer,
      observability,
      vaultClient: globalThis.__vaultClient
    })

    console.log('[KGC] Managers initialized successfully')
  } catch (error) {
    console.error('[KGC] Manager initialization failed:', error)
    throw error
  }
})
