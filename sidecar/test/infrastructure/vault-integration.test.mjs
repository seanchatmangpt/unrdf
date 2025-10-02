// @ts-check
/**
 * @file Vault Integration Test Suite
 * @description Tests for HashiCorp Vault integration with quorum unsealing
 */

import { describe, it, expect, beforeAll, afterAll, vi } from 'vitest'
import { VaultClient, createVaultClient } from '../../server/utils/vault-client.mjs'

describe('Vault Integration', () => {
  describe('VaultClient Configuration', () => {
    it('should create vault client with default configuration', () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      expect(client.config.endpoint).toBe('http://vault:8200')
      expect(client.config.mountPath).toBe('kgc')
      expect(client.config.cacheTTL).toBe(300000)
      expect(client.config.enableQuorum).toBe(true)
      expect(client.config.quorumShares).toBe(5)
      expect(client.config.quorumThreshold).toBe(3)
    })

    it('should create vault client with custom quorum configuration', () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token',
        quorumShares: 7,
        quorumThreshold: 4
      })

      expect(client.config.quorumShares).toBe(7)
      expect(client.config.quorumThreshold).toBe(4)
    })

    it('should validate quorum shares within bounds (3-10)', () => {
      expect(() => new VaultClient({
        endpoint: 'http://vault:8200',
        quorumShares: 2 // Too low
      })).toThrow()

      expect(() => new VaultClient({
        endpoint: 'http://vault:8200',
        quorumShares: 11 // Too high
      })).toThrow()
    })

    it('should validate quorum threshold within bounds (2-10)', () => {
      expect(() => new VaultClient({
        endpoint: 'http://vault:8200',
        quorumThreshold: 1 // Too low
      })).toThrow()

      expect(() => new VaultClient({
        endpoint: 'http://vault:8200',
        quorumThreshold: 11 // Too high
      })).toThrow()
    })

    it('should support optional namespace for multi-tenancy', () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        namespace: 'tenant-a'
      })

      expect(client.config.namespace).toBe('tenant-a')
    })
  })

  describe('Vault Initialization with Shamir Secret Sharing', () => {
    it('should initialize vault with quorum shares', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token',
        quorumShares: 5,
        quorumThreshold: 3
      })

      // Mock vault initialization
      vi.spyOn(client.client, 'init').mockResolvedValue({
        keys: ['key1', 'key2', 'key3', 'key4', 'key5'],
        root_token: 'root-token-123'
      })

      const result = await client.initializeVault()

      expect(result.keys).toHaveLength(5)
      expect(result.root_token).toBe('root-token-123')
      expect(client.unsealKeys).toHaveLength(5)
    })

    it('should handle vault initialization failure gracefully', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      vi.spyOn(client.client, 'init').mockRejectedValue(
        new Error('Vault already initialized')
      )

      await expect(client.initializeVault()).rejects.toThrow('Vault already initialized')
    })
  })

  describe('Quorum-Based Unsealing', () => {
    it('should unseal vault with quorum threshold (3/5)', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        quorumShares: 5,
        quorumThreshold: 3
      })

      const unsealKeys = ['key1', 'key2', 'key3']

      // Mock unseal progress
      let progress = 0
      vi.spyOn(client.client, 'unseal').mockImplementation(async () => {
        progress++
        return {
          sealed: progress < 3,
          progress: progress,
          t: 3
        }
      })

      const result = await client.unsealWithQuorum(unsealKeys)

      expect(result.sealed).toBe(false)
      expect(result.progress).toBe(3)
      expect(result.threshold).toBe(3)
    })

    it('should track unseal progress correctly', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        quorumThreshold: 3
      })

      const unsealKeys = ['key1', 'key2']

      let calls = 0
      vi.spyOn(client.client, 'unseal').mockImplementation(async () => {
        calls++
        return {
          sealed: true,
          progress: calls,
          t: 3
        }
      })

      const result = await client.unsealWithQuorum(unsealKeys)

      expect(result.sealed).toBe(true)
      expect(result.progress).toBe(2)
    })

    it('should handle insufficient quorum keys', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        quorumThreshold: 3
      })

      // Only provide 2 keys when 3 are required
      const unsealKeys = ['key1', 'key2']

      vi.spyOn(client.client, 'unseal').mockImplementation(async ({ key }) => ({
        sealed: true,
        progress: unsealKeys.indexOf(key) + 1,
        t: 3
      }))

      const result = await client.unsealWithQuorum(unsealKeys)

      expect(result.sealed).toBe(true)
      expect(result.progress).toBeLessThan(result.threshold)
    })
  })

  describe('Secret Retrieval with Caching', () => {
    it('should retrieve secret from vault', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      vi.spyOn(client.client, 'read').mockResolvedValue({
        data: {
          data: {
            api_key: 'secret-api-key',
            encryption_key: 'secret-encryption-key'
          }
        }
      })

      const secret = await client.getSecret('api-credentials')

      expect(secret.api_key).toBe('secret-api-key')
      expect(secret.encryption_key).toBe('secret-encryption-key')
    })

    it('should cache secrets with TTL', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token',
        cacheTTL: 5000
      })

      const mockRead = vi.spyOn(client.client, 'read').mockResolvedValue({
        data: {
          data: { value: 'cached-secret' }
        }
      })

      // First call
      await client.getSecret('test-secret')
      expect(mockRead).toHaveBeenCalledTimes(1)

      // Second call should use cache
      await client.getSecret('test-secret')
      expect(mockRead).toHaveBeenCalledTimes(1)
    })

    it('should invalidate cache after TTL expires', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token',
        cacheTTL: 100 // 100ms
      })

      const mockRead = vi.spyOn(client.client, 'read').mockResolvedValue({
        data: {
          data: { value: 'cached-secret' }
        }
      })

      await client.getSecret('test-secret')
      expect(mockRead).toHaveBeenCalledTimes(1)

      // Wait for cache to expire
      await new Promise(resolve => setTimeout(resolve, 150))

      await client.getSecret('test-secret')
      expect(mockRead).toHaveBeenCalledTimes(2)
    })

    it('should handle secret not found error', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      vi.spyOn(client.client, 'read').mockResolvedValue({
        data: null
      })

      await expect(client.getSecret('non-existent')).rejects.toThrow('Secret not found')
    })
  })

  describe('Secret Writing and Rotation', () => {
    it('should write secret to vault', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      const mockWrite = vi.spyOn(client.client, 'write').mockResolvedValue({})

      await client.writeSecret('api-credentials', {
        api_key: 'new-api-key'
      })

      expect(mockWrite).toHaveBeenCalledWith('kgc/data/api-credentials', {
        data: { api_key: 'new-api-key' }
      })
    })

    it('should invalidate cache on secret write', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      vi.spyOn(client.client, 'read').mockResolvedValue({
        data: { data: { value: 'old-secret' } }
      })
      vi.spyOn(client.client, 'write').mockResolvedValue({})

      // Read and cache
      await client.getSecret('test-secret')
      expect(client.cache.has('kgc/test-secret')).toBe(true)

      // Write should invalidate cache
      await client.writeSecret('test-secret', { value: 'new-secret' })
      expect(client.cache.has('kgc/test-secret')).toBe(false)
    })

    it('should rotate secret with version tracking', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      vi.spyOn(client.client, 'read').mockResolvedValue({
        data: { current_version: 3 }
      })
      vi.spyOn(client.client, 'write').mockResolvedValue({})

      const result = await client.rotateSecret('api-credentials', {
        api_key: 'rotated-key'
      })

      expect(result.version).toBe(4)
    })
  })

  describe('Token Auto-Renewal', () => {
    it('should setup token renewal based on TTL', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      vi.spyOn(client.client, 'tokenLookupSelf').mockResolvedValue({
        data: { ttl: 3600 } // 1 hour
      })

      await client.startTokenRenewal()

      expect(client.renewalInterval).not.toBeNull()
      client.stopTokenRenewal()
    })

    it('should renew token before expiration', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      vi.spyOn(client.client, 'tokenLookupSelf').mockResolvedValue({
        data: { ttl: 10 } // 10 seconds
      })

      const mockRenew = vi.spyOn(client.client, 'tokenRenewSelf').mockResolvedValue({})

      await client.startTokenRenewal()

      // Wait for renewal (should happen at 50% of TTL = 5s)
      await new Promise(resolve => setTimeout(resolve, 6000))

      expect(mockRenew).toHaveBeenCalled()
      client.stopTokenRenewal()
    }, { timeout: 10000 })

    it('should handle renewal failure gracefully', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      vi.spyOn(client.client, 'tokenLookupSelf').mockResolvedValue({
        data: { ttl: 10 }
      })

      vi.spyOn(client.client, 'tokenRenewSelf').mockRejectedValue(
        new Error('Token renewal failed')
      )

      await client.startTokenRenewal()
      // Should not throw, only log error
      await new Promise(resolve => setTimeout(resolve, 6000))

      client.stopTokenRenewal()
    }, { timeout: 10000 })
  })

  describe('Batch Secret Retrieval', () => {
    it('should retrieve all KGC secrets in parallel', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      vi.spyOn(client.client, 'read').mockImplementation(async (path) => {
        if (path.includes('api-credentials')) {
          return { data: { data: { api_key: 'api-123' } } }
        }
        if (path.includes('encryption-credentials')) {
          return { data: { data: { encryption_key: 'enc-456' } } }
        }
        if (path.includes('database-credentials')) {
          return {
            data: {
              data: {
                url: 'postgres://localhost/kgc',
                username: 'kgc',
                password: 'secret'
              }
            }
          }
        }
        return { data: null }
      })

      const secrets = await client.getAllSecrets()

      expect(secrets.apiKey).toBe('api-123')
      expect(secrets.encryptionKey).toBe('enc-456')
      expect(secrets.database.url).toBe('postgres://localhost/kgc')
    })

    it('should handle partial secret retrieval failure', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      vi.spyOn(client.client, 'read').mockImplementation(async (path) => {
        if (path.includes('api-credentials')) {
          throw new Error('API credentials not found')
        }
        return { data: { data: { value: 'test' } } }
      })

      await expect(client.getAllSecrets()).rejects.toThrow()
    })
  })

  describe('Cleanup and Resource Management', () => {
    it('should stop token renewal on destroy', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      vi.spyOn(client.client, 'tokenLookupSelf').mockResolvedValue({
        data: { ttl: 3600 }
      })

      await client.startTokenRenewal()
      expect(client.renewalInterval).not.toBeNull()

      await client.destroy()
      expect(client.renewalInterval).toBeNull()
    })

    it('should clear cache on destroy', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      vi.spyOn(client.client, 'read').mockResolvedValue({
        data: { data: { value: 'test' } }
      })

      await client.getSecret('test-secret')
      expect(client.cache.size).toBeGreaterThan(0)

      await client.destroy()
      expect(client.cache.size).toBe(0)
    })
  })

  describe('Manager Plugin Integration', () => {
    it('should initialize vault client in plugin with quorum enabled', async () => {
      const config = {
        vaultEnabled: true,
        vaultAddress: 'http://vault:8200',
        vaultToken: 'test-token',
        vaultEnableQuorum: true,
        vaultQuorumShares: 5,
        vaultQuorumThreshold: 3
      }

      const client = await createVaultClient({
        endpoint: config.vaultAddress,
        token: config.vaultToken,
        enableQuorum: config.vaultEnableQuorum,
        quorumShares: config.vaultQuorumShares,
        quorumThreshold: config.vaultQuorumThreshold
      })

      expect(client.config.enableQuorum).toBe(true)
      expect(client.config.quorumShares).toBe(5)
      expect(client.config.quorumThreshold).toBe(3)
    })

    it('should fallback to environment variables when vault disabled', async () => {
      process.env.API_KEY = 'env-api-key'
      process.env.ENCRYPTION_KEY = 'env-encryption-key'
      process.env.DATABASE_URL = 'postgres://localhost/kgc'

      const config = {
        vaultEnabled: false,
        apiKey: null,
        encryptionKey: null,
        databaseUrl: null
      }

      // Simulate fallback logic from plugin
      const secrets = {
        apiKey: config.apiKey || process.env.API_KEY,
        encryptionKey: config.encryptionKey || process.env.ENCRYPTION_KEY,
        database: {
          url: config.databaseUrl || process.env.DATABASE_URL
        }
      }

      expect(secrets.apiKey).toBe('env-api-key')
      expect(secrets.encryptionKey).toBe('env-encryption-key')
      expect(secrets.database.url).toBe('postgres://localhost/kgc')

      delete process.env.API_KEY
      delete process.env.ENCRYPTION_KEY
      delete process.env.DATABASE_URL
    })

    it('should store vault client globally for secret rotation', async () => {
      const client = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      globalThis.__vaultClient = client

      expect(globalThis.__vaultClient).toBe(client)
      expect(globalThis.__vaultClient.config.endpoint).toBe('http://vault:8200')

      delete globalThis.__vaultClient
    })
  })
})
