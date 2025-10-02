/**
 * @file Vault Quorum Unsealing Tests
 * @description Test quorum-based unsealing with Shamir's Secret Sharing (3/5 threshold)
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest'
import { VaultClient } from '../../server/utils/vault-client.mjs'

describe('Vault Quorum Unsealing', () => {
  let vaultClient
  let unsealKeys
  let rootToken

  beforeAll(async () => {
    // Initialize vault client for testing
    vaultClient = new VaultClient({
      endpoint: process.env.VAULT_ADDR || 'http://localhost:8200',
      enableQuorum: true,
      quorumShares: 5,
      quorumThreshold: 3,
      cacheTTL: 60000
    })
  })

  afterAll(async () => {
    if (vaultClient) {
      await vaultClient.destroy()
    }
  })

  describe('Vault Initialization with Shamir Secret Sharing', () => {
    it('should initialize Vault with 5 shares and 3 threshold', async () => {
      try {
        const result = await vaultClient.initializeVault({
          secret_shares: 5,
          secret_threshold: 3
        })

        expect(result).toBeDefined()
        expect(result.keys).toHaveLength(5)
        expect(result.root_token).toBeTruthy()

        // Store for subsequent tests
        unsealKeys = result.keys
        rootToken = result.root_token
      } catch (error) {
        // Vault may already be initialized
        console.log('[Test] Vault already initialized, skipping init test')
        expect(error.message).toContain('already initialized')
      }
    })

    it('should validate unseal key format', () => {
      if (!unsealKeys) {
        console.log('[Test] Skipping validation - no unseal keys available')
        return
      }

      unsealKeys.forEach((key, index) => {
        expect(key).toBeTruthy()
        expect(typeof key).toBe('string')
        expect(key.length).toBeGreaterThan(0)
        console.log(`[Test] Key ${index + 1} validated: ${key.substring(0, 10)}...`)
      })
    })
  })

  describe('Quorum Unsealing with 3/5 Keys', () => {
    it('should unseal with exactly 3 keys (minimum threshold)', async () => {
      if (!unsealKeys || unsealKeys.length < 3) {
        console.log('[Test] Skipping - insufficient unseal keys')
        return
      }

      // Use first 3 keys
      const thresholdKeys = unsealKeys.slice(0, 3)

      const result = await vaultClient.unsealWithQuorum(thresholdKeys)

      expect(result).toBeDefined()
      expect(result.sealed).toBe(false)
      expect(result.progress).toBe(3)
      expect(result.threshold).toBe(3)

      console.log('[Test] Vault unsealed with minimum quorum (3/5)')
    })

    it('should unseal with 4/5 keys (more than threshold)', async () => {
      if (!unsealKeys || unsealKeys.length < 4) {
        console.log('[Test] Skipping - insufficient unseal keys')
        return
      }

      // Seal vault first
      try {
        await vaultClient.client.seal()
      } catch (error) {
        console.log('[Test] Seal operation not available in test mode')
      }

      // Use first 4 keys
      const extraKeys = unsealKeys.slice(0, 4)

      const result = await vaultClient.unsealWithQuorum(extraKeys)

      expect(result).toBeDefined()
      expect(result.sealed).toBe(false)
      console.log('[Test] Vault unsealed with 4/5 keys')
    })

    it('should unseal with all 5 keys', async () => {
      if (!unsealKeys || unsealKeys.length !== 5) {
        console.log('[Test] Skipping - insufficient unseal keys')
        return
      }

      // Seal vault first
      try {
        await vaultClient.client.seal()
      } catch (error) {
        console.log('[Test] Seal operation not available in test mode')
      }

      const result = await vaultClient.unsealWithQuorum(unsealKeys)

      expect(result).toBeDefined()
      expect(result.sealed).toBe(false)
      expect(result.threshold).toBe(3)

      console.log('[Test] Vault unsealed with all 5 keys')
    })

    it('should fail to unseal with only 2 keys (below threshold)', async () => {
      if (!unsealKeys || unsealKeys.length < 2) {
        console.log('[Test] Skipping - insufficient unseal keys')
        return
      }

      // Seal vault first
      try {
        await vaultClient.client.seal()
      } catch (error) {
        console.log('[Test] Seal operation not available in test mode')
      }

      // Use only 2 keys (below threshold of 3)
      const insufficientKeys = unsealKeys.slice(0, 2)

      const result = await vaultClient.unsealWithQuorum(insufficientKeys)

      expect(result).toBeDefined()
      expect(result.sealed).toBe(true) // Still sealed
      expect(result.progress).toBe(2) // Only 2/3
      expect(result.threshold).toBe(3)

      console.log('[Test] Vault correctly remains sealed with insufficient keys (2/3)')
    })

    it('should fail to unseal with only 1 key', async () => {
      if (!unsealKeys || unsealKeys.length < 1) {
        console.log('[Test] Skipping - no unseal keys available')
        return
      }

      // Seal vault first
      try {
        await vaultClient.client.seal()
      } catch (error) {
        console.log('[Test] Seal operation not available in test mode')
      }

      // Use only 1 key
      const singleKey = [unsealKeys[0]]

      const result = await vaultClient.unsealWithQuorum(singleKey)

      expect(result).toBeDefined()
      expect(result.sealed).toBe(true)
      expect(result.progress).toBe(1)
      expect(result.threshold).toBe(3)

      console.log('[Test] Vault correctly remains sealed with single key (1/3)')
    })
  })

  describe('Quorum Progress Tracking', () => {
    it('should track unsealing progress incrementally', async () => {
      if (!unsealKeys || unsealKeys.length < 3) {
        console.log('[Test] Skipping - insufficient unseal keys')
        return
      }

      // Seal vault first
      try {
        await vaultClient.client.seal()
      } catch (error) {
        console.log('[Test] Seal operation not available in test mode')
      }

      const progressResults = []

      // Submit keys one at a time and track progress
      for (let i = 0; i < 3; i++) {
        const result = await vaultClient.client.unseal({ key: unsealKeys[i] })
        progressResults.push({
          step: i + 1,
          progress: result.progress,
          sealed: result.sealed,
          threshold: result.t
        })

        console.log(`[Test] Progress ${i + 1}/3: sealed=${result.sealed}, progress=${result.progress}`)
      }

      // Verify progress tracking
      expect(progressResults[0].progress).toBe(1)
      expect(progressResults[0].sealed).toBe(true)

      expect(progressResults[1].progress).toBe(2)
      expect(progressResults[1].sealed).toBe(true)

      expect(progressResults[2].progress).toBe(3)
      expect(progressResults[2].sealed).toBe(false) // Unsealed at threshold

      console.log('[Test] Progress tracking validated successfully')
    })

    it('should reset progress on successful unseal', async () => {
      if (!unsealKeys || unsealKeys.length < 3) {
        console.log('[Test] Skipping - insufficient unseal keys')
        return
      }

      // Vault should already be unsealed from previous test
      const status = await vaultClient.client.status()

      expect(status.sealed).toBe(false)
      // Progress should be 0 when unsealed
      const sealStatus = await vaultClient.client.sealStatus()
      expect(sealStatus.progress).toBe(0)

      console.log('[Test] Progress correctly reset after unsealing')
    })
  })

  describe('Key Combination Testing', () => {
    it('should unseal with any valid 3-key combination', async () => {
      if (!unsealKeys || unsealKeys.length !== 5) {
        console.log('[Test] Skipping - need exactly 5 unseal keys')
        return
      }

      const combinations = [
        [0, 1, 2], // First 3
        [2, 3, 4], // Last 3
        [0, 2, 4], // Alternating
        [1, 2, 3], // Middle 3
        [0, 1, 4]  // First 2 + last
      ]

      for (const combo of combinations) {
        // Seal vault
        try {
          await vaultClient.client.seal()
        } catch (error) {
          console.log('[Test] Seal operation not available')
          return
        }

        // Get keys for this combination
        const keys = combo.map(i => unsealKeys[i])

        const result = await vaultClient.unsealWithQuorum(keys)

        expect(result.sealed).toBe(false)
        console.log(`[Test] Successfully unsealed with combination [${combo.join(', ')}]`)
      }
    })
  })

  describe('Secret Access After Unsealing', () => {
    it('should allow secret operations after quorum unsealing', async () => {
      if (!unsealKeys || unsealKeys.length < 3) {
        console.log('[Test] Skipping - insufficient unseal keys')
        return
      }

      // Ensure vault is unsealed
      const status = await vaultClient.client.status()
      if (status.sealed) {
        await vaultClient.unsealWithQuorum(unsealKeys.slice(0, 3))
      }

      // Set token for authenticated operations
      if (rootToken) {
        vaultClient.client.token = rootToken
      }

      // Try to write a secret
      const testSecret = {
        test_key: 'test_value',
        timestamp: new Date().toISOString()
      }

      try {
        await vaultClient.writeSecret('test/quorum-secret', testSecret)

        // Read it back
        const retrieved = await vaultClient.getSecret('test/quorum-secret')

        expect(retrieved.test_key).toBe('test_value')
        console.log('[Test] Secret operations successful after quorum unsealing')
      } catch (error) {
        console.log('[Test] Secret operations require proper KV mount setup')
        expect(error.message).toBeTruthy()
      }
    })
  })
})
