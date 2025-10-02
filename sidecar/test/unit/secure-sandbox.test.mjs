/**
 * @file Secure Sandbox Tests
 * @description Unit tests for isolated-vm sandbox with threat detection
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest'
import { SecureSandbox } from '../../server/utils/secure-sandbox.mjs'
import { SandboxThreatDetector } from '../../server/utils/sandbox-threat-detector.mjs'

describe('SecureSandbox', () => {
  let sandbox

  beforeEach(() => {
    sandbox = new SecureSandbox({
      memoryLimit: 64,
      timeout: 3000
    })
  })

  afterEach(async () => {
    await sandbox.cleanup()
  })

  describe('Isolate Creation', () => {
    it('should create isolate for effect', async () => {
      await sandbox.createIsolate('test-effect')
      expect(sandbox.isolates.has('test-effect')).toBe(true)
      expect(sandbox.contexts.has('test-effect')).toBe(true)
    })

    it('should create multiple isolates', async () => {
      await sandbox.createIsolate('effect-1')
      await sandbox.createIsolate('effect-2')
      expect(sandbox.isolates.size).toBe(2)
    })

    it('should enforce memory limits', async () => {
      const smallSandbox = new SecureSandbox({ memoryLimit: 8 })
      await smallSandbox.createIsolate('test')

      const memUsage = await smallSandbox.getMemoryUsage('test')
      expect(memUsage.limit).toBeLessThanOrEqual(8 * 1024 * 1024)

      await smallSandbox.cleanup()
    })
  })

  describe('Effect Registration', () => {
    it('should register simple effect', async () => {
      const code = `
        const effect = function(input) {
          return { result: input.value * 2 }
        }
      `

      await expect(sandbox.registerEffect('double', code)).resolves.not.toThrow()
    })

    it('should register effect with console', async () => {
      const code = `
        const effect = function(input) {
          console.log('Processing:', input)
          return { processed: true }
        }
      `

      await expect(sandbox.registerEffect('logger', code)).resolves.not.toThrow()
    })

    it('should timeout on infinite loop', async () => {
      const code = `
        while(true) { }
      `

      await expect(sandbox.registerEffect('infinite', code))
        .rejects.toThrow(/timed out/i)
    })

    it('should reject invalid syntax', async () => {
      const code = `
        const effect = function(input) {
          invalid syntax here
        }
      `

      await expect(sandbox.registerEffect('invalid', code)).rejects.toThrow()
    })
  })

  describe('Effect Execution', () => {
    it('should execute simple calculation', async () => {
      const code = `
        const effect = function(input) {
          return { result: input.a + input.b }
        }
      `

      await sandbox.registerEffect('add', code)
      const result = await sandbox.executeEffect('add', { a: 5, b: 3 })

      expect(result).toEqual({ result: 8 })
    })

    it('should execute string manipulation', async () => {
      const code = `
        const effect = function(input) {
          return {
            upper: input.text.toUpperCase(),
            length: input.text.length
          }
        }
      `

      await sandbox.registerEffect('string', code)
      const result = await sandbox.executeEffect('string', { text: 'hello' })

      expect(result).toEqual({ upper: 'HELLO', length: 5 })
    })

    it('should execute JSON operations', async () => {
      const code = `
        const effect = function(input) {
          const data = JSON.parse(input.json)
          return { parsed: data.value }
        }
      `

      await sandbox.registerEffect('json', code)
      const result = await sandbox.executeEffect('json', {
        json: '{"value": 42}'
      })

      expect(result).toEqual({ parsed: 42 })
    })

    it('should timeout on long execution', async () => {
      const code = `
        const effect = function(input) {
          let sum = 0
          for (let i = 0; i < 1e9; i++) {
            sum += i
          }
          return { sum }
        }
      `

      await sandbox.registerEffect('slow', code)
      await expect(sandbox.executeEffect('slow', {}))
        .rejects.toThrow(/timed out/i)
    })

    it('should isolate effects from each other', async () => {
      const code1 = `
        const effect = function(input) {
          globalThis.shared = 'effect1'
          return { value: globalThis.shared }
        }
      `

      const code2 = `
        const effect = function(input) {
          return { value: globalThis.shared || 'isolated' }
        }
      `

      await sandbox.registerEffect('effect1', code1)
      await sandbox.registerEffect('effect2', code2)

      const result1 = await sandbox.executeEffect('effect1', {})
      const result2 = await sandbox.executeEffect('effect2', {})

      expect(result1.value).toBe('effect1')
      expect(result2.value).toBe('isolated') // Should not see effect1's data
    })
  })

  describe('Security Isolation', () => {
    it('should block process access', async () => {
      const code = `
        const effect = function(input) {
          return process.env
        }
      `

      await sandbox.registerEffect('malicious', code)
      await expect(sandbox.executeEffect('malicious', {}))
        .rejects.toThrow()
    })

    it('should block require', async () => {
      const code = `
        const effect = function(input) {
          const fs = require('fs')
          return fs.readFileSync('/etc/passwd')
        }
      `

      await sandbox.registerEffect('malicious', code)
      await expect(sandbox.executeEffect('malicious', {}))
        .rejects.toThrow()
    })

    it('should block filesystem access', async () => {
      const code = `
        const effect = function(input) {
          const fs = require('fs')
          return { files: fs.readdirSync('/') }
        }
      `

      await expect(sandbox.registerEffect('malicious', code))
        .rejects.toThrow()
    })

    it('should prevent VM escape via constructor', async () => {
      const code = `
        const effect = function(input) {
          return this.constructor.constructor('return process')()
        }
      `

      await sandbox.registerEffect('escape', code)
      await expect(sandbox.executeEffect('escape', {}))
        .rejects.toThrow()
    })
  })

  describe('Resource Management', () => {
    it('should track memory usage', async () => {
      await sandbox.createIsolate('test')
      const usage = await sandbox.getMemoryUsage('test')

      expect(usage).toHaveProperty('used')
      expect(usage).toHaveProperty('total')
      expect(usage).toHaveProperty('limit')
      expect(usage).toHaveProperty('percentage')
      expect(usage.percentage).toBeGreaterThanOrEqual(0)
      expect(usage.percentage).toBeLessThanOrEqual(100)
    })

    it('should destroy isolate and free resources', async () => {
      await sandbox.createIsolate('test')
      expect(sandbox.isolates.has('test')).toBe(true)

      await sandbox.destroyIsolate('test')
      expect(sandbox.isolates.has('test')).toBe(false)
      expect(sandbox.contexts.has('test')).toBe(false)
    })

    it('should cleanup all isolates', async () => {
      await sandbox.createIsolate('effect-1')
      await sandbox.createIsolate('effect-2')
      await sandbox.createIsolate('effect-3')

      await sandbox.cleanup()
      expect(sandbox.isolates.size).toBe(0)
      expect(sandbox.contexts.size).toBe(0)
    })
  })
})

describe('SandboxThreatDetector', () => {
  let detector

  beforeEach(() => {
    detector = new SandboxThreatDetector({
      blockThreshold: 80,
      logAllThreats: false
    })
  })

  describe('Safe Code Detection', () => {
    it('should allow safe mathematical operations', async () => {
      const code = `
        function calculate(a, b) {
          return a + b * 2
        }
      `

      const result = await detector.analyzeCode(code)
      expect(result.score).toBeLessThan(40)
      expect(result.blocked).toBe(false)
      expect(result.severity).toBe('low')
    })

    it('should allow string operations', async () => {
      const code = `
        function processText(text) {
          return text.toUpperCase().trim()
        }
      `

      const result = await detector.analyzeCode(code)
      expect(result.score).toBeLessThan(40)
      expect(result.blocked).toBe(false)
    })

    it('should allow JSON operations', async () => {
      const code = `
        function parseData(input) {
          const data = JSON.parse(input)
          return JSON.stringify({ processed: data })
        }
      `

      const result = await detector.analyzeCode(code)
      expect(result.score).toBeLessThan(40)
      expect(result.blocked).toBe(false)
    })
  })

  describe('Malicious Code Detection', () => {
    it('should detect eval usage', async () => {
      const code = `
        function malicious() {
          eval('process.exit(1)')
        }
      `

      const result = await detector.analyzeCode(code)
      expect(result.score).toBeGreaterThanOrEqual(80)
      expect(result.blocked).toBe(true)
      expect(result.patterns.some(p => p.name === 'EVAL')).toBe(true)
    })

    it('should detect process manipulation', async () => {
      const code = `
        function exploit() {
          process.exit(0)
          process.env.SECRET = 'hacked'
        }
      `

      const result = await detector.analyzeCode(code)
      expect(result.score).toBeGreaterThanOrEqual(80)
      expect(result.blocked).toBe(true)
      expect(result.patterns.some(p => p.name === 'PROCESS_ACCESS')).toBe(true)
    })

    it('should detect filesystem access', async () => {
      const code = `
        const fs = require('fs')
        fs.readFileSync('/etc/passwd')
      `

      const result = await detector.analyzeCode(code)
      expect(result.score).toBeGreaterThanOrEqual(70)
      expect(result.patterns.some(p => p.name === 'FILESYSTEM')).toBe(true)
    })

    it('should detect child process spawning', async () => {
      const code = `
        const { exec } = require('child_process')
        exec('rm -rf /')
      `

      const result = await detector.analyzeCode(code)
      expect(result.score).toBeGreaterThanOrEqual(80)
      expect(result.blocked).toBe(true)
    })

    it('should detect prototype pollution', async () => {
      const code = `
        Object.prototype.isAdmin = true
        const obj = {}
        obj.__proto__.polluted = 'yes'
      `

      const result = await detector.analyzeCode(code)
      expect(result.score).toBeGreaterThanOrEqual(80)
      expect(result.patterns.some(p => p.name === 'PROTOTYPE_POLLUTION')).toBe(true)
    })

    it('should detect VM escape attempts', async () => {
      const code = `
        this.constructor.constructor('return process')().exit(0)
      `

      const result = await detector.analyzeCode(code)
      expect(result.score).toBe(100)
      expect(result.blocked).toBe(true)
      expect(result.patterns.some(p => p.name === 'VM_ESCAPE')).toBe(true)
    })

    it('should detect cryptomining', async () => {
      const code = `
        const miner = new CoinHive.User('site-key')
        miner.start()
      `

      const result = await detector.analyzeCode(code)
      expect(result.score).toBeGreaterThanOrEqual(80)
      expect(result.patterns.some(p => p.name === 'CRYPTOMINING')).toBe(true)
    })
  })

  describe('Code Complexity Analysis', () => {
    it('should detect excessive branching', async () => {
      const code = `
        function complex(x) {
          if (x > 10) {
            if (x > 20) {
              if (x > 30) {
                if (x > 40) {
                  return 'complex'
                }
              }
            }
          }
        }
      `

      const result = await detector.analyzeCode(code)
      expect(result.complexityScore).toBeGreaterThan(0)
    })

    it('should detect obfuscation patterns', async () => {
      const code = `
        const x = '\\x68\\x65\\x6c\\x6c\\x6f'
        const y = '\\u0077\\u006f\\u0072\\u006c\\u0064'
      `

      const result = await detector.analyzeCode(code)
      expect(result.complexityScore).toBeGreaterThan(10)
    })
  })

  describe('Caching', () => {
    it('should cache analysis results', async () => {
      const code = 'function test() { return 42 }'

      const result1 = await detector.analyzeCode(code)
      const result2 = await detector.analyzeCode(code)

      expect(result1).toEqual(result2)
      expect(detector.threatCache.size).toBe(1)
    })

    it('should clear cache', async () => {
      await detector.analyzeCode('function test() {}')
      expect(detector.threatCache.size).toBe(1)

      detector.clearCache()
      expect(detector.threatCache.size).toBe(0)
    })
  })

  describe('Statistics', () => {
    it('should track threat statistics', async () => {
      await detector.analyzeCode('eval("test")')
      await detector.analyzeCode('process.exit()')
      await detector.analyzeCode('eval("another")')

      const stats = detector.getStatistics()
      expect(stats).toHaveProperty('cacheSize')
      expect(stats).toHaveProperty('patternHistory')
      expect(stats).toHaveProperty('mostCommonThreats')
    })
  })
})
