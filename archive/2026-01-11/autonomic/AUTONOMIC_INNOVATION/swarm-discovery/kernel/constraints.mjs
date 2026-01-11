/**
 * @file constraints.mjs - Admissibility predicates (Q1-Q5 invariants)
 * @description Guards for valid compositions and experiments
 *
 * Invariants:
 * Q1: Determinism - DETERMINISTIC=1 ⇒ hash(LEDGER) constant
 * Q2: Provenance - Every claim links to evidenceHash and file pointers
 * Q3: Receipts - verify(ρ)=true for all experiment receipts
 * Q4: Poka-yoke - Invalid operations rejected or unreachable
 * Q5: SLA honesty - Latency/error claims measured and stored; no claims without data
 */

/**
 * Check determinism constraint
 * @param {object} ledger1 - First run ledger
 * @param {object} ledger2 - Second run ledger
 * @param {string} hash1 - Hash of first ledger
 * @param {string} hash2 - Hash of second ledger
 * @returns {object} - {valid, witness?}
 */
export function checkDeterminism(ledger1, ledger2, hash1, hash2) {
  if (hash1 !== hash2) {
    return {
      valid: false,
      witness: {
        constraint: 'Q1_Determinism',
        hash1,
        hash2,
        message: 'Ledger hashes differ across runs'
      }
    }
  }

  return { valid: true }
}

/**
 * Check provenance constraint
 * @param {object} atom - Atom or claim
 * @returns {object} - {valid, witness?}
 */
export function checkProvenance(atom) {
  const hasEvidenceHash = typeof atom.evidenceHash === 'string' && atom.evidenceHash.length === 64
  const hasFilePointer = atom.source && typeof atom.source.file === 'string'

  if (!hasEvidenceHash || !hasFilePointer) {
    return {
      valid: false,
      witness: {
        constraint: 'Q2_Provenance',
        atom: atom.name || 'unknown',
        hasEvidenceHash,
        hasFilePointer,
        message: 'Missing evidenceHash or file pointer'
      }
    }
  }

  return { valid: true }
}

/**
 * Check receipt constraint
 * @param {object} receipt - Receipt object with verify method
 * @param {object} inputs - Experiment inputs
 * @param {object} outputs - Experiment outputs
 * @returns {object} - {valid, witness?}
 */
export function checkReceipt(receipt, inputs, outputs) {
  if (!receipt.hash || typeof receipt.hash !== 'string') {
    return {
      valid: false,
      witness: {
        constraint: 'Q3_Receipts',
        message: 'Receipt missing hash'
      }
    }
  }

  // Verify receipt integrity
  const isValid = receipt.verify && receipt.verify(inputs, outputs)
  if (!isValid) {
    return {
      valid: false,
      witness: {
        constraint: 'Q3_Receipts',
        message: 'Receipt verification failed',
        receiptHash: receipt.hash
      }
    }
  }

  return { valid: true }
}

/**
 * Check poka-yoke constraint (invalid operations unreachable)
 * @param {object} composition - Composition object
 * @returns {object} - {valid, witness?}
 */
export function checkPokaYoke(composition) {
  // Check for required fields
  const requiredFields = ['atoms', 'edges', 'constraints']
  for (const field of requiredFields) {
    if (!composition[field]) {
      return {
        valid: false,
        witness: {
          constraint: 'Q4_PokaYoke',
          message: `Missing required field: ${field}`
        }
      }
    }
  }

  // Check edge closure
  const atomIds = new Set(composition.atoms.map(a => a.id))
  for (const edge of composition.edges) {
    if (!atomIds.has(edge.from) || !atomIds.has(edge.to)) {
      return {
        valid: false,
        witness: {
          constraint: 'Q4_PokaYoke',
          message: 'Invalid edge references unknown atom',
          edge
        }
      }
    }
  }

  return { valid: true }
}

/**
 * Check SLA honesty constraint
 * @param {object} claim - Performance claim
 * @param {object} measurements - Measured data
 * @returns {object} - {valid, witness?}
 */
export function checkSLAHonesty(claim, measurements) {
  if (!measurements || Object.keys(measurements).length === 0) {
    return {
      valid: false,
      witness: {
        constraint: 'Q5_SLA_Honesty',
        message: 'Claim without measured data',
        claim
      }
    }
  }

  // Check that claim matches measurements
  if (claim.latencyMs && !measurements.latency) {
    return {
      valid: false,
      witness: {
        constraint: 'Q5_SLA_Honesty',
        message: 'Latency claim without latency measurement',
        claim
      }
    }
  }

  return { valid: true }
}

/**
 * Master admissibility check
 * @param {object} composition - Candidate composition
 * @param {object} options - Check options
 * @returns {object} - {valid, failedConstraints: []}
 */
export function checkAdmissibility(composition, options = {}) {
  const failed = []

  // Q2: Check provenance for all atoms
  if (composition.atoms) {
    for (const atom of composition.atoms) {
      const prov = checkProvenance(atom)
      if (!prov.valid) {
        failed.push(prov.witness)
      }
    }
  }

  // Q4: Check poka-yoke
  const poka = checkPokaYoke(composition)
  if (!poka.valid) {
    failed.push(poka.witness)
  }

  return {
    valid: failed.length === 0,
    failedConstraints: failed
  }
}
