/**
 * @file HashiCorp Vault Client with Quorum-Based Secret Recovery
 * @description Vault integration with Shamir's Secret Sharing for distributed secret management
 */

import vault from 'node-vault'
import { z } from 'zod'

/**
 * Vault configuration schema
 */
const VaultConfigSchema = z.object({
  endpoint: z.string().url().default('http://vault:8200'),
  token: z.string().optional(),
  mountPath: z.string().default('kgc'),
  namespace: z.string().optional(),
  cacheTTL: z.number().default(300000), // 5 minutes
  enableQuorum: z.boolean().default(true),
  quorumShares: z.number().min(3).max(10).default(5),
  quorumThreshold: z.number().min(2).max(10).default(3)
})

/**
 * Secret metadata schema
 */
const SecretMetadataSchema = z.object({
  version: z.number(),
  created_time: z.string(),
  deletion_time: z.string().optional(),
  destroyed: z.boolean().optional(),
  custom_metadata: z.record(z.string()).optional()
})

/**
 * Vault Client with quorum-based unsealing and secret management
 */
export class VaultClient {
  /**
   * @param {z.infer<typeof VaultConfigSchema>} config - Vault configuration
   */
  constructor(config) {
    this.config = VaultConfigSchema.parse(config)

    /** @type {import('node-vault').VaultClient} */
    this.client = vault({
      apiVersion: 'v1',
      endpoint: this.config.endpoint,
      token: this.config.token,
      namespace: this.config.namespace
    })

    /** @type {Map<string, {value: any, expiresAt: number}>} */
    this.cache = new Map()

    /** @type {NodeJS.Timeout | null} */
    this.renewalInterval = null

    /** @type {boolean} */
    this.initialized = false

    /** @type {string[]} */
    this.unsealKeys = []
  }

  /**
   * Initialize Vault client and verify connection
   * @returns {Promise<{initialized: boolean, sealed: boolean, version: string}>}
   */
  async initialize() {
    try {
      const health = await this.client.health()
      const status = await this.client.status()

      this.initialized = status.initialized

      if (status.sealed && this.config.enableQuorum) {
        console.log('[Vault] Vault is sealed, quorum unsealing required')
        // In production, unseal keys should be distributed to quorum members
        // For development, keys are stored locally (see vault-init.sh)
      }

      // Start token renewal
      if (this.config.token) {
        await this.startTokenRenewal()
      }

      console.log(`[Vault] Connected to Vault ${status.version}`)
      console.log(`[Vault] Initialized: ${status.initialized}, Sealed: ${status.sealed}`)

      return {
        initialized: status.initialized,
        sealed: status.sealed,
        version: status.version
      }
    } catch (error) {
      console.error('[Vault] Initialization failed:', error.message)
      throw new Error(`Vault initialization failed: ${error.message}`)
    }
  }

  /**
   * Initialize Vault with Shamir's Secret Sharing
   * @param {Object} options - Initialization options
   * @param {number} [options.secret_shares=5] - Number of key shares to generate
   * @param {number} [options.secret_threshold=3] - Number of shares required to unseal
   * @returns {Promise<{keys: string[], root_token: string}>}
   */
  async initializeVault(options = {}) {
    const { secret_shares = this.config.quorumShares, secret_threshold = this.config.quorumThreshold } = options

    try {
      const result = await this.client.init({
        secret_shares,
        secret_threshold
      })

      this.unsealKeys = result.keys
      this.client.token = result.root_token

      console.log(`[Vault] Initialized with ${secret_shares} shares, ${secret_threshold} threshold`)
      console.log(`[Vault] Root token: ${result.root_token}`)
      console.log(`[Vault] Unseal keys generated (distribute to quorum members)`)

      return {
        keys: result.keys,
        root_token: result.root_token
      }
    } catch (error) {
      console.error('[Vault] Vault initialization failed:', error.message)
      throw error
    }
  }

  /**
   * Unseal Vault using quorum of unseal keys
   * @param {string[]} keys - Unseal keys from quorum members
   * @returns {Promise<{sealed: boolean, progress: number, threshold: number}>}
   */
  async unsealWithQuorum(keys) {
    try {
      let result

      for (const key of keys) {
        result = await this.client.unseal({ key })

        console.log(`[Vault] Unseal progress: ${result.progress}/${result.t}`)

        if (!result.sealed) {
          console.log('[Vault] Vault successfully unsealed')
          break
        }
      }

      return {
        sealed: result.sealed,
        progress: result.progress,
        threshold: result.t
      }
    } catch (error) {
      console.error('[Vault] Unseal failed:', error.message)
      throw error
    }
  }

  /**
   * Get secret from Vault with caching
   * @param {string} path - Secret path (relative to mount)
   * @returns {Promise<any>}
   */
  async getSecret(path) {
    const cacheKey = `${this.config.mountPath}/${path}`

    // Check cache first
    const cached = this.cache.get(cacheKey)
    if (cached && Date.now() < cached.expiresAt) {
      console.log(`[Vault] Cache hit for ${path}`)
      return cached.value
    }

    try {
      const result = await this.client.read(`${this.config.mountPath}/data/${path}`)

      if (!result?.data?.data) {
        throw new Error(`Secret not found: ${path}`)
      }

      const secretData = result.data.data

      // Cache the secret
      this.cache.set(cacheKey, {
        value: secretData,
        expiresAt: Date.now() + this.config.cacheTTL
      })

      console.log(`[Vault] Secret retrieved: ${path}`)
      return secretData
    } catch (error) {
      console.error(`[Vault] Failed to get secret ${path}:`, error.message)
      throw error
    }
  }

  /**
   * Write secret to Vault
   * @param {string} path - Secret path (relative to mount)
   * @param {Object} data - Secret data
   * @returns {Promise<void>}
   */
  async writeSecret(path, data) {
    try {
      await this.client.write(`${this.config.mountPath}/data/${path}`, {
        data
      })

      // Invalidate cache
      const cacheKey = `${this.config.mountPath}/${path}`
      this.cache.delete(cacheKey)

      console.log(`[Vault] Secret written: ${path}`)
    } catch (error) {
      console.error(`[Vault] Failed to write secret ${path}:`, error.message)
      throw error
    }
  }

  /**
   * Rotate secret and maintain version history
   * @param {string} path - Secret path
   * @param {Object} newData - New secret data
   * @returns {Promise<{version: number}>}
   */
  async rotateSecret(path, newData) {
    try {
      // Get current version
      const metadata = await this.client.read(`${this.config.mountPath}/metadata/${path}`)
      const currentVersion = metadata.data.current_version

      // Write new version
      await this.writeSecret(path, {
        ...newData,
        rotated_at: new Date().toISOString(),
        previous_version: currentVersion
      })

      console.log(`[Vault] Secret rotated: ${path} (v${currentVersion} -> v${currentVersion + 1})`)

      return { version: currentVersion + 1 }
    } catch (error) {
      console.error(`[Vault] Failed to rotate secret ${path}:`, error.message)
      throw error
    }
  }

  /**
   * Get all KGC secrets for application startup
   * @returns {Promise<{apiKey: string, encryptionKey: string, database: Object}>}
   */
  async getAllSecrets() {
    try {
      const [apiCreds, encryptionCreds, dbCreds] = await Promise.all([
        this.getSecret('api-credentials'),
        this.getSecret('encryption-credentials'),
        this.getSecret('database-credentials')
      ])

      return {
        apiKey: apiCreds.api_key,
        encryptionKey: encryptionCreds.encryption_key,
        database: {
          url: dbCreds.url,
          username: dbCreds.username,
          password: dbCreds.password
        }
      }
    } catch (error) {
      console.error('[Vault] Failed to get all secrets:', error.message)
      throw error
    }
  }

  /**
   * Start automatic token renewal
   * @private
   * @returns {Promise<void>}
   */
  async startTokenRenewal() {
    try {
      // Get token TTL
      const lookup = await this.client.tokenLookupSelf()
      const ttl = lookup.data.ttl

      if (ttl > 0) {
        // Renew token at 50% of TTL
        const renewalInterval = (ttl * 1000) / 2

        this.renewalInterval = setInterval(async () => {
          try {
            await this.client.tokenRenewSelf()
            console.log('[Vault] Token renewed successfully')
          } catch (error) {
            console.error('[Vault] Token renewal failed:', error.message)
          }
        }, renewalInterval)

        console.log(`[Vault] Token auto-renewal enabled (every ${renewalInterval / 1000}s)`)
      }
    } catch (error) {
      console.error('[Vault] Failed to setup token renewal:', error.message)
    }
  }

  /**
   * Stop token renewal
   */
  stopTokenRenewal() {
    if (this.renewalInterval) {
      clearInterval(this.renewalInterval)
      this.renewalInterval = null
      console.log('[Vault] Token auto-renewal stopped')
    }
  }

  /**
   * Clear secret cache
   */
  clearCache() {
    this.cache.clear()
    console.log('[Vault] Secret cache cleared')
  }

  /**
   * Get audit log for secret access
   * @param {string} path - Secret path
   * @returns {Promise<Array>}
   */
  async getAuditLog(path) {
    try {
      // Read audit log (requires audit device to be enabled)
      const log = await this.client.read(`sys/audit/${path}`)
      return log.data
    } catch (error) {
      console.error('[Vault] Failed to get audit log:', error.message)
      return []
    }
  }

  /**
   * Cleanup resources
   */
  async destroy() {
    this.stopTokenRenewal()
    this.clearCache()
    console.log('[Vault] Vault client destroyed')
  }
}

/**
 * Create and initialize Vault client
 * @param {z.infer<typeof VaultConfigSchema>} config - Vault configuration
 * @returns {Promise<VaultClient>}
 */
export async function createVaultClient(config) {
  const client = new VaultClient(config)
  await client.initialize()
  return client
}
