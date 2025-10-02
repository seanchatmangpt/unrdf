/**
 * @file Code Signing Tests
 * @description Tests for cryptographic signature verification and public key validation
 */
import { describe, it, expect, beforeEach } from 'vitest'
import { SandboxThreatDetector } from '../../server/utils/sandbox-threat-detector.mjs'
import crypto from 'node:crypto'

describe('Code Signing', () => {
  let detector
  let keyPair

  beforeEach(() => {
    detector = new SandboxThreatDetector({
      blockThreshold: 80,
      enableCodeSigning: true,
      trustedSigners: [],
      logAllThreats: false
    })

    // Generate RSA keypair for testing
    keyPair = crypto.generateKeyPairSync('rsa', {
      modulusLength: 2048,
      publicKeyEncoding: { type: 'spki', format: 'der' },
      privateKeyEncoding: { type: 'pkcs8', format: 'der' }
    })
  })

  describe('Signature Generation', () => {
    it('should create valid signature for code', () => {
      const code = `const x = 1;`

      const sign = crypto.createSign('SHA256')
      sign.update(code)
      sign.end()

      const signature = sign.sign(keyPair.privateKey)

      expect(signature).toBeInstanceOf(Buffer)
      expect(signature.length).toBeGreaterThan(0)
    })

    it('should create different signatures for different code', () => {
      const code1 = `const x = 1;`
      const code2 = `const y = 2;`

      const sign1 = crypto.createSign('SHA256')
      sign1.update(code1)
      sign1.end()
      const sig1 = sign1.sign(keyPair.privateKey)

      const sign2 = crypto.createSign('SHA256')
      sign2.update(code2)
      sign2.end()
      const sig2 = sign2.sign(keyPair.privateKey)

      expect(sig1.toString('hex')).not.toBe(sig2.toString('hex'))
    })
  })

  describe('Signature Verification', () => {
    it('should verify valid signature', async () => {
      const code = `const trusted = true;`

      const sign = crypto.createSign('SHA256')
      sign.update(code)
      sign.end()
      const signature = sign.sign(keyPair.privateKey).toString('hex')
      const publicKey = keyPair.publicKey.toString('hex')

      const result = await detector._verifySignature(code, signature, publicKey)

      expect(result).toBe(true)
    })

    it('should reject invalid signature', async () => {
      const code = `const trusted = true;`
      const invalidSignature = 'deadbeef'
      const publicKey = keyPair.publicKey.toString('hex')

      const result = await detector._verifySignature(code, invalidSignature, publicKey)

      expect(result).toBe(false)
    })

    it('should reject signature from different key', async () => {
      const code = `const trusted = true;`

      // Create signature with one key
      const sign = crypto.createSign('SHA256')
      sign.update(code)
      sign.end()
      const signature = sign.sign(keyPair.privateKey).toString('hex')

      // Try to verify with different key
      const otherKeyPair = crypto.generateKeyPairSync('rsa', {
        modulusLength: 2048,
        publicKeyEncoding: { type: 'spki', format: 'der' },
        privateKeyEncoding: { type: 'pkcs8', format: 'der' }
      })
      const wrongPublicKey = otherKeyPair.publicKey.toString('hex')

      const result = await detector._verifySignature(code, signature, wrongPublicKey)

      expect(result).toBe(false)
    })

    it('should reject modified code with valid signature', async () => {
      const originalCode = `const trusted = true;`
      const modifiedCode = `const trusted = false;`

      const sign = crypto.createSign('SHA256')
      sign.update(originalCode)
      sign.end()
      const signature = sign.sign(keyPair.privateKey).toString('hex')
      const publicKey = keyPair.publicKey.toString('hex')

      const result = await detector._verifySignature(modifiedCode, signature, publicKey)

      expect(result).toBe(false)
    })
  })

  describe('Trusted Signers', () => {
    it('should recognize trusted signer', async () => {
      const publicKey = keyPair.publicKey.toString('hex')

      const trustedDetector = new SandboxThreatDetector({
        blockThreshold: 80,
        enableCodeSigning: true,
        trustedSigners: [publicKey]
      })

      expect(trustedDetector._isTrustedSigner(publicKey)).toBe(true)
    })

    it('should reject untrusted signer', () => {
      const publicKey = keyPair.publicKey.toString('hex')

      expect(detector._isTrustedSigner(publicKey)).toBe(false)
    })

    it('should bypass threat detection for trusted signed code', async () => {
      const publicKey = keyPair.publicKey.toString('hex')

      const trustedDetector = new SandboxThreatDetector({
        blockThreshold: 80,
        enableCodeSigning: true,
        trustedSigners: [publicKey]
      })

      // Code with high threat score
      const code = `eval('dangerous'); process.exit(1);`

      const sign = crypto.createSign('SHA256')
      sign.update(code)
      sign.end()
      const signature = sign.sign(keyPair.privateKey).toString('hex')

      const result = await trustedDetector.analyzeCode(code, {
        signature,
        publicKey
      })

      expect(result.score).toBe(0)
      expect(result.severity).toBe('low')
      expect(result.blocked).toBe(false)
      expect(result.signatureValid).toBe(true)
    })

    it('should analyze code from untrusted signer even with signature', async () => {
      const publicKey = keyPair.publicKey.toString('hex')

      // Code with high threat score
      const code = `eval('dangerous');`

      const sign = crypto.createSign('SHA256')
      sign.update(code)
      sign.end()
      const signature = sign.sign(keyPair.privateKey).toString('hex')

      // Detector does not trust this signer
      const result = await detector.analyzeCode(code, {
        signature,
        publicKey
      })

      expect(result.score).toBeGreaterThan(0)
      expect(result.blocked).toBe(true)
    })
  })

  describe('Public Key Validation', () => {
    it('should validate hex-encoded public key format', () => {
      const publicKey = keyPair.publicKey.toString('hex')

      expect(/^[0-9a-f]+$/i.test(publicKey)).toBe(true)
    })

    it('should reject invalid public key format', async () => {
      const code = `const x = 1;`
      const signature = 'valid-signature'
      const invalidPublicKey = 'not-hex-encoded!'

      const result = await detector._verifySignature(code, signature, invalidPublicKey)

      expect(result).toBe(false)
    })
  })

  describe('Signature Format', () => {
    it('should validate hex-encoded signature format', () => {
      const code = `const x = 1;`

      const sign = crypto.createSign('SHA256')
      sign.update(code)
      sign.end()
      const signature = sign.sign(keyPair.privateKey).toString('hex')

      expect(/^[0-9a-f]+$/i.test(signature)).toBe(true)
    })

    it('should handle different signature formats gracefully', async () => {
      const code = `const x = 1;`
      const invalidSignature = 'not a valid signature format'
      const publicKey = keyPair.publicKey.toString('hex')

      const result = await detector._verifySignature(code, invalidSignature, publicKey)

      expect(result).toBe(false)
    })
  })

  describe('Integration with Threat Detection', () => {
    it('should use signature verification before pattern matching', async () => {
      const publicKey = keyPair.publicKey.toString('hex')

      const trustedDetector = new SandboxThreatDetector({
        blockThreshold: 80,
        enableCodeSigning: true,
        trustedSigners: [publicKey],
        logAllThreats: false
      })

      const code = `
        // High-threat code that would normally be blocked
        eval('trusted operations');
        require('trusted-module');
      `

      const sign = crypto.createSign('SHA256')
      sign.update(code)
      sign.end()
      const signature = sign.sign(keyPair.privateKey).toString('hex')

      const result = await trustedDetector.analyzeCode(code, {
        signature,
        publicKey
      })

      // Should bypass pattern matching
      expect(result.patterns).toEqual([])
      expect(result.score).toBe(0)
    })

    it('should fall through to pattern matching on invalid signature', async () => {
      const code = `eval('untrusted');`

      const result = await detector.analyzeCode(code, {
        signature: 'invalid',
        publicKey: 'invalid'
      })

      // Should perform pattern matching
      expect(result.patterns.length).toBeGreaterThan(0)
      expect(result.score).toBeGreaterThan(0)
    })
  })

  describe('Code Hashing', () => {
    it('should create consistent hash for same code', () => {
      const code = `const x = 1;`

      const hash1 = detector._hashCode(code)
      const hash2 = detector._hashCode(code)

      expect(hash1).toBe(hash2)
    })

    it('should create different hashes for different code', () => {
      const code1 = `const x = 1;`
      const code2 = `const y = 2;`

      const hash1 = detector._hashCode(code1)
      const hash2 = detector._hashCode(code2)

      expect(hash1).not.toBe(hash2)
    })

    it('should use SHA-256 for hashing', () => {
      const code = `const x = 1;`

      const hash = detector._hashCode(code)
      const expectedHash = crypto.createHash('sha256').update(code).digest('hex')

      expect(hash).toBe(expectedHash)
    })
  })

  describe('registerEffectSchema Validation', () => {
    it('should validate signature as hex string', async () => {
      const { registerEffectSchema } = await import('../../server/utils/validation.mjs')

      const validData = {
        id: 'test-effect',
        code: 'const x = 1;',
        signature: 'deadbeef0123456789',
        publicKey: 'abcdef0123456789'
      }

      const result = registerEffectSchema.safeParse(validData)

      expect(result.success).toBe(true)
    })

    it('should reject non-hex signature', async () => {
      const { registerEffectSchema } = await import('../../server/utils/validation.mjs')

      const invalidData = {
        id: 'test-effect',
        code: 'const x = 1;',
        signature: 'not-hex!',
        publicKey: 'deadbeef'
      }

      const result = registerEffectSchema.safeParse(invalidData)

      expect(result.success).toBe(false)
      expect(result.error?.issues[0]?.path).toContain('signature')
    })

    it('should validate publicKey as hex string', async () => {
      const { registerEffectSchema } = await import('../../server/utils/validation.mjs')

      const validData = {
        id: 'test-effect',
        code: 'const x = 1;',
        publicKey: 'deadbeef0123456789'
      }

      const result = registerEffectSchema.safeParse(validData)

      expect(result.success).toBe(true)
    })

    it('should reject non-hex publicKey', async () => {
      const { registerEffectSchema } = await import('../../server/utils/validation.mjs')

      const invalidData = {
        id: 'test-effect',
        code: 'const x = 1;',
        publicKey: 'not-hex!'
      }

      const result = registerEffectSchema.safeParse(invalidData)

      expect(result.success).toBe(false)
      expect(result.error?.issues[0]?.path).toContain('publicKey')
    })
  })

  describe('End-to-End Signing Workflow', () => {
    it('should complete full signing and verification workflow', async () => {
      const code = `
        const effect = (input) => {
          return { result: input.value * 2 };
        };
      `

      // 1. Generate signature
      const sign = crypto.createSign('SHA256')
      sign.update(code)
      sign.end()
      const signature = sign.sign(keyPair.privateKey).toString('hex')
      const publicKey = keyPair.publicKey.toString('hex')

      // 2. Register as trusted signer
      const trustedDetector = new SandboxThreatDetector({
        blockThreshold: 80,
        enableCodeSigning: true,
        trustedSigners: [publicKey]
      })

      // 3. Analyze code with signature
      const result = await trustedDetector.analyzeCode(code, {
        signature,
        publicKey
      })

      // 4. Verify bypass
      expect(result.signatureValid).toBe(true)
      expect(result.score).toBe(0)
      expect(result.blocked).toBe(false)
    })
  })
})
