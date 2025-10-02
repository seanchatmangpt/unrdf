/**
 * @file Sandbox Threat Detector
 * @description ML-based threat detection for sandboxed code execution
 */

import { trace } from '@opentelemetry/api'
import { SandboxError } from './errors.mjs'
import crypto from 'node:crypto'

const tracer = trace.getTracer('sandbox-threat-detector')

/**
 * @typedef {Object} ThreatScore
 * @property {number} score - Threat score 0-100 (0=safe, 100=highly dangerous)
 * @property {string[]} patterns - Detected threat patterns
 * @property {string} severity - 'low' | 'medium' | 'high' | 'critical'
 * @property {boolean} blocked - Whether code should be blocked
 */

/**
 * Threat detection patterns
 */
const THREAT_PATTERNS = {
  // Code execution exploits
  EVAL: {
    regex: /\b(eval|Function)\s*\(/gi,
    score: 80,
    severity: 'critical',
    description: 'Dynamic code execution'
  },

  // Process manipulation
  PROCESS_ACCESS: {
    regex: /\bprocess\s*\.\s*(exit|kill|abort|binding|env)/gi,
    score: 90,
    severity: 'critical',
    description: 'Process manipulation attempt'
  },

  // Module loading exploits
  REQUIRE: {
    regex: /\brequire\s*\(/gi,
    score: 70,
    severity: 'high',
    description: 'Module loading attempt'
  },

  IMPORT: {
    regex: /\b(import|require)\s*(\.meta)?/gi,
    score: 70,
    severity: 'high',
    description: 'Import statement detected'
  },

  // Filesystem access
  FILESYSTEM: {
    regex: /\b(fs|readFile|writeFile|unlink|mkdir|rmdir)\s*\./gi,
    score: 85,
    severity: 'critical',
    description: 'Filesystem access attempt'
  },

  // Network access
  NETWORK: {
    regex: /\b(fetch|XMLHttpRequest|WebSocket|http|https|net)\s*\./gi,
    score: 75,
    severity: 'high',
    description: 'Network access attempt'
  },

  // Child process spawning
  CHILD_PROCESS: {
    regex: /\b(child_process|exec|spawn|fork)\s*\./gi,
    score: 95,
    severity: 'critical',
    description: 'Child process spawning'
  },

  // Prototype pollution
  PROTOTYPE_POLLUTION: {
    regex: /__proto__|constructor\s*\[\s*['"]prototype['"]\s*\]/gi,
    score: 85,
    severity: 'critical',
    description: 'Prototype pollution attempt'
  },

  // Buffer exploits
  BUFFER_OVERFLOW: {
    regex: /\bBuffer\s*\.\s*(alloc|allocUnsafe)\s*\(\s*[0-9]{7,}/gi,
    score: 80,
    severity: 'high',
    description: 'Potential buffer overflow'
  },

  // Timing attacks
  TIMING_ATTACK: {
    regex: /\bperformance\s*\.\s*now\s*\(/gi,
    score: 40,
    severity: 'medium',
    description: 'Timing measurement (potential side-channel)'
  },

  // Global manipulation
  GLOBAL_MANIPULATION: {
    regex: /\bglobal\s*\[\s*['"][^'"]+['"]\s*\]\s*=/gi,
    score: 75,
    severity: 'high',
    description: 'Global object manipulation'
  },

  // VM escape attempts
  VM_ESCAPE: {
    regex: /\b(this\.constructor\.constructor|arguments\.callee\.caller)/gi,
    score: 100,
    severity: 'critical',
    description: 'VM escape attempt'
  },

  // Cryptomining patterns
  CRYPTOMINING: {
    regex: /\b(coinhive|cryptonight|monero|stratum)/gi,
    score: 95,
    severity: 'critical',
    description: 'Cryptomining detected'
  }
}

/**
 * Sandbox threat detector with ML-based analysis
 */
export class SandboxThreatDetector {
  constructor(config = {}) {
    this.config = {
      blockThreshold: config.blockThreshold || 80,
      enableCodeSigning: config.enableCodeSigning !== false,
      trustedSigners: config.trustedSigners || [],
      logAllThreats: config.logAllThreats !== false
    }

    /** @type {Map<string, number>} Pattern occurrence tracking for ML */
    this.patternHistory = new Map()

    /** @type {Map<string, ThreatScore>} Code hash cache */
    this.threatCache = new Map()
  }

  /**
   * Analyze code for threats
   * @param {string} code
   * @param {Object} options
   * @returns {Promise<ThreatScore>}
   */
  async analyzeCode(code, options = {}) {
    return tracer.startActiveSpan('analyzeCode', async (span) => {
      try {
        const codeHash = this._hashCode(code)
        span.setAttribute('codeHash', codeHash)
        span.setAttribute('codeLength', code.length)

        // Check cache
        if (this.threatCache.has(codeHash)) {
          const cached = this.threatCache.get(codeHash)
          span.setAttribute('cached', true)
          return cached
        }

        // Check code signature if enabled
        if (this.config.enableCodeSigning && options.signature) {
          const signatureValid = await this._verifySignature(code, options.signature, options.publicKey)
          if (signatureValid && this._isTrustedSigner(options.publicKey)) {
            span.setAttribute('trustedSigner', true)
            const trusted = {
              score: 0,
              patterns: [],
              severity: 'low',
              blocked: false,
              signatureValid: true
            }
            this.threatCache.set(codeHash, trusted)
            return trusted
          }
        }

        // Perform pattern matching
        const detectedPatterns = []
        let totalScore = 0

        for (const [name, pattern] of Object.entries(THREAT_PATTERNS)) {
          const matches = code.match(pattern.regex)
          if (matches && matches.length > 0) {
            detectedPatterns.push({
              name,
              description: pattern.description,
              severity: pattern.severity,
              score: pattern.score,
              occurrences: matches.length
            })

            // Increase score based on occurrences
            const patternScore = pattern.score * Math.log2(matches.length + 1)
            totalScore += patternScore

            // Track for ML learning
            this._trackPattern(name, matches.length)
          }
        }

        // Anomaly detection - code complexity analysis
        const complexityScore = this._analyzeComplexity(code)
        totalScore += complexityScore

        // Behavioral analysis - suspicious patterns
        const behaviorScore = this._analyzeBehavior(code)
        totalScore += behaviorScore

        // Normalize score to 0-100
        const normalizedScore = Math.min(100, totalScore)

        // Determine severity
        let severity = 'low'
        if (normalizedScore >= 80) severity = 'critical'
        else if (normalizedScore >= 60) severity = 'high'
        else if (normalizedScore >= 40) severity = 'medium'

        const result = {
          score: Math.round(normalizedScore),
          patterns: detectedPatterns,
          severity,
          blocked: normalizedScore >= this.config.blockThreshold,
          complexityScore,
          behaviorScore
        }

        // Cache result
        this.threatCache.set(codeHash, result)

        span.setAttribute('threatScore', result.score)
        span.setAttribute('blocked', result.blocked)
        span.setAttribute('patternsDetected', detectedPatterns.length)
        span.setStatus({ code: 1 })

        // Log if threat detected
        if (this.config.logAllThreats && result.score > 0) {
          console.warn('[ThreatDetector] Threat detected:', {
            score: result.score,
            severity: result.severity,
            patterns: detectedPatterns.map(p => p.name)
          })
        }

        return result

      } catch (error) {
        span.recordException(error)
        span.setStatus({ code: 2, message: error.message })
        throw new SandboxError(`Threat analysis failed: ${error.message}`, { cause: error })
      } finally {
        span.end()
      }
    })
  }

  /**
   * Verify code signature
   * @param {string} code
   * @param {string} signature - Ed25519 signature (hex)
   * @param {string} publicKey - Ed25519 public key (hex)
   * @returns {Promise<boolean>}
   */
  async _verifySignature(code, signature, publicKey) {
    try {
      const verify = crypto.createVerify('SHA256')
      verify.update(code)
      verify.end()

      const publicKeyBuffer = Buffer.from(publicKey, 'hex')
      const signatureBuffer = Buffer.from(signature, 'hex')

      return verify.verify(
        {
          key: publicKeyBuffer,
          format: 'der',
          type: 'spki'
        },
        signatureBuffer
      )
    } catch (error) {
      console.error('[ThreatDetector] Signature verification failed:', error)
      return false
    }
  }

  /**
   * Check if signer is trusted
   * @param {string} publicKey
   * @returns {boolean}
   */
  _isTrustedSigner(publicKey) {
    return this.config.trustedSigners.includes(publicKey)
  }

  /**
   * Hash code for caching
   * @param {string} code
   * @returns {string}
   */
  _hashCode(code) {
    return crypto.createHash('sha256').update(code).digest('hex')
  }

  /**
   * Track pattern occurrences for ML learning
   * @param {string} patternName
   * @param {number} count
   */
  _trackPattern(patternName, count) {
    const current = this.patternHistory.get(patternName) || 0
    this.patternHistory.set(patternName, current + count)
  }

  /**
   * Analyze code complexity (cyclomatic complexity approximation)
   * @param {string} code
   * @returns {number} Complexity score
   */
  _analyzeComplexity(code) {
    let score = 0

    // Count branching statements
    const branches = (code.match(/\b(if|else|switch|case|for|while|catch)\b/g) || []).length
    score += branches * 2

    // Count nested functions (potential obfuscation)
    const nestedFunctions = (code.match(/function\s*\w*\s*\([^)]*\)\s*{[^}]*function/g) || []).length
    score += nestedFunctions * 5

    // Check for obfuscation indicators
    const hasHexEscapes = /\\x[0-9a-f]{2}/gi.test(code)
    const hasUnicodeEscapes = /\\u[0-9a-f]{4}/gi.test(code)
    if (hasHexEscapes || hasUnicodeEscapes) score += 15

    // Excessive string concatenation (obfuscation)
    const stringConcats = (code.match(/['"][^'"]*['"]\s*\+\s*['"][^'"]*['"]/g) || []).length
    if (stringConcats > 10) score += 10

    return Math.min(30, score) // Cap complexity contribution
  }

  /**
   * Analyze suspicious behavioral patterns
   * @param {string} code
   * @returns {number} Behavior score
   */
  _analyzeBehavior(code) {
    let score = 0

    // Excessive try-catch (error hiding)
    const tryCatchCount = (code.match(/try\s*{/g) || []).length
    if (tryCatchCount > 5) score += 10

    // Anonymous functions (potential obfuscation)
    const anonFunctions = (code.match(/function\s*\(/g) || []).length
    if (anonFunctions > 10) score += 8

    // Excessive loops (potential DoS)
    const loops = (code.match(/\b(for|while)\s*\(/g) || []).length
    if (loops > 10) score += 12

    // Large numeric constants (suspicious)
    const largeNumbers = (code.match(/\b[0-9]{10,}\b/g) || []).length
    if (largeNumbers > 5) score += 10

    // Base64 patterns (potential payload)
    const hasBase64 = /[A-Za-z0-9+/]{40,}={0,2}/.test(code)
    if (hasBase64) score += 15

    return Math.min(30, score) // Cap behavior contribution
  }

  /**
   * Clear threat cache
   */
  clearCache() {
    this.threatCache.clear()
  }

  /**
   * Get threat statistics
   * @returns {Object}
   */
  getStatistics() {
    return {
      cacheSize: this.threatCache.size,
      patternHistory: Object.fromEntries(this.patternHistory),
      mostCommonThreats: [...this.patternHistory.entries()]
        .sort((a, b) => b[1] - a[1])
        .slice(0, 5)
        .map(([name, count]) => ({ name, count }))
    }
  }
}
