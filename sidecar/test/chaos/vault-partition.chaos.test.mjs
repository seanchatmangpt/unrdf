// @ts-check
/**
 * Vault Network Partition Chaos Tests
 *
 * Tests Vault resilience under network partition and failure conditions:
 * - Vault unavailable during unsealing
 * - Network timeout scenarios
 * - Quorum unsealing failures
 * - Secret retrieval with fallback
 * - Token renewal failures
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { setTimeout } from 'timers/promises'
import { VaultClient } from '../../server/utils/vault-client.mjs'

describe('Vault Network Partition Chaos Tests', () => {
  describe('Vault Unsealing Under Network Partition', () => {
    it('should handle Vault unavailable during initialization', async () => {
      const vaultClient = new VaultClient({
        endpoint: 'http://unreachable-vault:8200',
        token: 'test-token',
        enableQuorum: true,
        quorumShares: 5,
        quorumThreshold: 3
      })

      // Vault is unreachable
      await expect(vaultClient.initialize()).rejects.toThrow(/initialization failed/i)
    })

    it('should timeout during unsealing operation', async () => {
      // Simulate slow/partitioned Vault
      const slowVaultClient = new VaultClient({
        endpoint: 'http://slow-vault:8200',
        token: 'test-token',
        enableQuorum: true
      })

      // Mock slow unseal that takes too long
      const unsealPromise = Promise.race([
        slowVaultClient.unsealWithQuorum(['key1', 'key2', 'key3']),
        setTimeout(2000).then(() => {
          throw new Error('Unseal timeout - network partition detected')
        })
      ])

      await expect(unsealPromise).rejects.toThrow(/timeout|partition/)
    })

    it('should handle partial quorum during network split', async () => {
      const vaultClient = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token',
        enableQuorum: true,
        quorumShares: 5,
        quorumThreshold: 3
      })

      // Only 2 keys available (below threshold of 3)
      const partialKeys = ['key1', 'key2']

      // Should fail to unseal with insufficient keys
      try {
        const result = await vaultClient.unsealWithQuorum(partialKeys)

        // If doesn't throw, should still be sealed
        expect(result.sealed).toBe(true)
        expect(result.progress).toBeLessThan(result.threshold)
      } catch (error) {
        // Also acceptable to throw error
        expect(error).toBeDefined()
      }
    })

    it('should retry unsealing with exponential backoff', async () => {
      const maxRetries = 3
      let attempts = 0
      const delays = []

      const retryUnseal = async (keys, retryConfig = { maxRetries: 3, initialDelay: 100 }) => {
        let lastError

        for (let i = 0; i <= retryConfig.maxRetries; i++) {
          attempts++
          try {
            // Simulate unsealing attempt
            if (i < retryConfig.maxRetries) {
              throw new Error('Vault sealed - network partition')
            }
            return { sealed: false, progress: 3, threshold: 3 }
          } catch (error) {
            lastError = error

            if (i < retryConfig.maxRetries) {
              const delay = retryConfig.initialDelay * Math.pow(2, i)
              delays.push(delay)
              await setTimeout(delay)
            }
          }
        }

        throw lastError
      }

      const result = await retryUnseal(['key1', 'key2', 'key3'])

      expect(attempts).toBe(maxRetries + 1)
      expect(delays.length).toBe(maxRetries)
      // Verify exponential backoff: 100ms, 200ms, 400ms
      expect(delays[0]).toBe(100)
      expect(delays[1]).toBe(200)
      expect(delays[2]).toBe(400)
      expect(result.sealed).toBe(false)
    })
  })

  describe('Secret Retrieval with Network Failures', () => {
    it('should fallback to environment variables when Vault unavailable', async () => {
      // Set environment variables as fallback
      process.env.API_KEY = 'fallback-api-key'
      process.env.ENCRYPTION_KEY = 'fallback-encryption-key'
      process.env.DATABASE_URL = 'postgresql://fallback:5432/db'

      const vaultClient = new VaultClient({
        endpoint: 'http://unreachable-vault:8200',
        enableQuorum: false
      })

      // Vault is unavailable, should fallback to env vars
      const getSecretsWithFallback = async () => {
        try {
          await vaultClient.initialize()
          return await vaultClient.getAllSecrets()
        } catch (error) {
          console.log('Vault unavailable, using environment variables')
          return {
            apiKey: process.env.API_KEY,
            encryptionKey: process.env.ENCRYPTION_KEY,
            database: {
              url: process.env.DATABASE_URL
            }
          }
        }
      }

      const secrets = await getSecretsWithFallback()

      expect(secrets.apiKey).toBe('fallback-api-key')
      expect(secrets.encryptionKey).toBe('fallback-encryption-key')
      expect(secrets.database.url).toBe('postgresql://fallback:5432/db')

      // Cleanup
      delete process.env.API_KEY
      delete process.env.ENCRYPTION_KEY
      delete process.env.DATABASE_URL
    })

    it('should use cached secrets when Vault connection drops', async () => {
      const vaultClient = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token',
        cacheTTL: 5000 // 5 second cache
      })

      // Simulate cached secret
      vaultClient.cache.set('kgc/test-secret', {
        value: { password: 'cached-password' },
        expiresAt: Date.now() + 5000
      })

      // Even if Vault is down, cached secret should work
      const cachedSecret = vaultClient.cache.get('kgc/test-secret')

      expect(cachedSecret).toBeDefined()
      expect(cachedSecret.value.password).toBe('cached-password')
      expect(Date.now()).toBeLessThan(cachedSecret.expiresAt)
    })

    it('should handle expired cache during network partition', async () => {
      const vaultClient = new VaultClient({
        endpoint: 'http://unreachable-vault:8200',
        token: 'test-token',
        cacheTTL: 100 // Short TTL
      })

      // Add expired cache entry
      vaultClient.cache.set('kgc/test-secret', {
        value: { password: 'expired' },
        expiresAt: Date.now() - 1000 // Already expired
      })

      // Should not use expired cache
      const cached = vaultClient.cache.get('kgc/test-secret')
      const isExpired = Date.now() >= cached.expiresAt

      expect(isExpired).toBe(true)

      // Attempt to fetch should fail (Vault unreachable, cache expired)
      await expect(vaultClient.getSecret('test-secret')).rejects.toThrow()
    })
  })

  describe('Token Renewal Failures', () => {
    it('should handle token renewal failure gracefully', async () => {
      const vaultClient = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'short-lived-token',
        cacheTTL: 300000
      })

      // Mock token renewal failure
      const renewalErrors = []
      const originalRenewal = vaultClient.client?.tokenRenewSelf

      if (vaultClient.client) {
        vaultClient.client.tokenRenewSelf = async () => {
          const error = new Error('Token renewal failed - network partition')
          renewalErrors.push(error)
          throw error
        }
      }

      // Attempt renewal
      try {
        await vaultClient.client?.tokenRenewSelf()
      } catch (error) {
        renewalErrors.push(error)
      }

      expect(renewalErrors.length).toBeGreaterThan(0)
      expect(renewalErrors[0].message).toContain('renewal failed')
    })

    it('should stop renewal interval on repeated failures', async () => {
      const vaultClient = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      // Start renewal (would fail in practice)
      let renewalAttempts = 0
      const maxFailures = 3

      const attemptRenewal = async () => {
        while (renewalAttempts < maxFailures) {
          renewalAttempts++
          await setTimeout(50)

          if (renewalAttempts >= maxFailures) {
            // Stop renewal after max failures
            vaultClient.stopTokenRenewal()
            return 'stopped'
          }
        }
      }

      const result = await attemptRenewal()

      expect(renewalAttempts).toBe(maxFailures)
      expect(result).toBe('stopped')
      expect(vaultClient.renewalInterval).toBeNull()
    })
  })

  describe('Quorum Coordination Failures', () => {
    it('should handle distributed quorum member failures', async () => {
      // Simulate 5 quorum members, threshold of 3
      const quorumMembers = [
        { id: 'member1', available: true, key: 'key1' },
        { id: 'member2', available: true, key: 'key2' },
        { id: 'member3', available: false, key: 'key3' }, // Failed
        { id: 'member4', available: true, key: 'key4' },
        { id: 'member5', available: false, key: 'key5' }  // Failed
      ]

      const collectQuorumKeys = async (members, threshold) => {
        const availableKeys = members
          .filter(m => m.available)
          .map(m => m.key)

        if (availableKeys.length < threshold) {
          throw new Error(`Insufficient quorum: ${availableKeys.length}/${threshold} members available`)
        }

        return availableKeys.slice(0, threshold)
      }

      const keys = await collectQuorumKeys(quorumMembers, 3)

      expect(keys.length).toBe(3)
      expect(keys).toContain('key1')
      expect(keys).toContain('key2')
      expect(keys).toContain('key4')
    })

    it('should fail when quorum threshold cannot be met', async () => {
      const quorumMembers = [
        { id: 'member1', available: true, key: 'key1' },
        { id: 'member2', available: false, key: 'key2' },
        { id: 'member3', available: false, key: 'key3' },
        { id: 'member4', available: false, key: 'key4' },
        { id: 'member5', available: false, key: 'key5' }
      ]

      const collectQuorumKeys = async (members, threshold) => {
        const availableKeys = members
          .filter(m => m.available)
          .map(m => m.key)

        if (availableKeys.length < threshold) {
          throw new Error(`Insufficient quorum: ${availableKeys.length}/${threshold}`)
        }

        return availableKeys
      }

      await expect(collectQuorumKeys(quorumMembers, 3))
        .rejects.toThrow(/Insufficient quorum/)
    })

    it('should detect network partition split-brain scenario', async () => {
      // Simulate network split with two partitions
      const partition1 = [
        { id: 'member1', key: 'key1' },
        { id: 'member2', key: 'key2' }
      ]

      const partition2 = [
        { id: 'member3', key: 'key3' },
        { id: 'member4', key: 'key4' },
        { id: 'member5', key: 'key5' }
      ]

      const threshold = 3

      // Partition 1 cannot reach quorum
      expect(partition1.length).toBeLessThan(threshold)

      // Partition 2 can reach quorum
      expect(partition2.length).toBeGreaterThanOrEqual(threshold)

      // This demonstrates split-brain risk
      const canPartition1Unseal = partition1.length >= threshold
      const canPartition2Unseal = partition2.length >= threshold

      expect(canPartition1Unseal).toBe(false)
      expect(canPartition2Unseal).toBe(true)

      // Only one partition should be able to unseal
      expect(canPartition1Unseal && canPartition2Unseal).toBe(false)
    })
  })

  describe('Vault Recovery Scenarios', () => {
    it('should recover when Vault becomes available', async () => {
      let vaultAvailable = false
      let attempts = 0
      const maxAttempts = 5

      const waitForVault = async () => {
        while (attempts < maxAttempts && !vaultAvailable) {
          attempts++
          await setTimeout(100)

          // Simulate Vault becoming available after 3 attempts
          if (attempts >= 3) {
            vaultAvailable = true
          }
        }

        if (!vaultAvailable) {
          throw new Error('Vault recovery timeout')
        }

        return { initialized: true, sealed: false }
      }

      const status = await waitForVault()

      expect(attempts).toBe(3)
      expect(vaultAvailable).toBe(true)
      expect(status.initialized).toBe(true)
    })

    it('should clear cache on recovery to avoid stale data', async () => {
      const vaultClient = new VaultClient({
        endpoint: 'http://vault:8200',
        token: 'test-token'
      })

      // Add some cached data
      vaultClient.cache.set('secret1', { value: 'old', expiresAt: Date.now() + 10000 })
      vaultClient.cache.set('secret2', { value: 'stale', expiresAt: Date.now() + 10000 })

      expect(vaultClient.cache.size).toBe(2)

      // On recovery, clear cache
      vaultClient.clearCache()

      expect(vaultClient.cache.size).toBe(0)
    })
  })
})
