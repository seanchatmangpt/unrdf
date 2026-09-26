/**
 * @file Mandatory interchangeable-part admission wrapper for AtomVM brokers.
 * @module @unrdf/atomvm/part-admission-broker
 */
import { PartSubstitutionRefusal, assertSubstitutable } from '@unrdf/core';
import { verifySubstitutionReceipt } from '@unrdf/receipts';

export class PartAdmissionBrokerRefusal extends Error {
  constructor(code, message, details = {}) {
    super(message);
    this.name = 'PartAdmissionBrokerRefusal';
    this.code = code;
    this.details = Object.freeze({ ...details });
  }
}

/**
 * Deep-copy a data value and freeze every nested object of the copy.
 * @param {*} value - Plain data (requirement, passport, receipt or context)
 * @returns {*} Frozen snapshot, or the value itself when null/undefined
 */
function frozenSnapshot(value) {
  if (value === undefined || value === null) return value;
  const copy = structuredClone(value);
  const freeze = node => {
    if (node && typeof node === 'object') {
      Object.values(node).forEach(freeze);
      Object.freeze(node);
    }
    return node;
  };
  return freeze(copy);
}

/**
 * Non-bypassable decorator around an existing broker.execute boundary.
 *
 * The delegate is called only after:
 * 1. fresh substitution evaluation against the current host intersection; and
 * 2. verification of a receipt bound to the same requirement, passport, and context.
 *
 * Bindings are fixed at construction: requirement, candidate, receipt and context
 * are deep-frozen snapshots held in private fields, the delegate reference is
 * private, and the instance itself is frozen, so neither reassignment nor caller
 * mutation after construction can change what execute() admits.
 */
export class PartAdmissionBroker {
  #requirement;
  #candidate;
  #substitutionReceipt;
  #context;
  #delegate;

  constructor({ requirement, candidate, substitutionReceipt, context = {}, delegate } = {}) {
    if (!requirement || !candidate) throw new TypeError('requirement and candidate are required');
    this.#requirement = frozenSnapshot(requirement);
    this.#candidate = frozenSnapshot(candidate);
    this.#substitutionReceipt = frozenSnapshot(substitutionReceipt);
    this.#context = frozenSnapshot(context);
    this.#delegate = delegate;
    Object.freeze(this);
  }

  /**
   * @returns {object|undefined} Frozen requirement snapshot bound at construction
   */
  get requirement() {
    return this.#requirement;
  }

  /**
   * @returns {object|undefined} Frozen candidate passport snapshot bound at construction
   */
  get candidate() {
    return this.#candidate;
  }

  /**
   * @returns {object|undefined} Frozen substitution receipt snapshot bound at construction
   */
  get substitutionReceipt() {
    return this.#substitutionReceipt;
  }

  /**
   * @returns {object|undefined} Frozen host context snapshot bound at construction
   */
  get context() {
    return this.#context;
  }

  async execute(request) {
    let judgement;
    try {
      judgement = assertSubstitutable(this.#requirement, this.#candidate, this.#context);
    } catch (error) {
      if (error instanceof PartSubstitutionRefusal) {
        throw new PartAdmissionBrokerRefusal(
          'PART_SUBSTITUTION_REFUSED',
          'Candidate part is not substitutable at the current host boundary',
          { judgement: error.judgement }
        );
      }
      throw error;
    }

    if (!this.#substitutionReceipt) {
      throw new PartAdmissionBrokerRefusal(
        'PART_SUBSTITUTION_RECEIPT_REQUIRED',
        'Zero unreceipted actuation: a substitution receipt is required',
        {
          requirementDigest: this.#requirement.digest,
          candidateDigest: this.#candidate.digest,
        }
      );
    }

    const verification = verifySubstitutionReceipt(this.#substitutionReceipt, {
      requirement: this.#requirement,
      candidate: this.#candidate,
      context: this.#context,
    });
    if (!verification.valid || verification.state !== 'ADMITTED') {
      throw new PartAdmissionBrokerRefusal(
        'PART_SUBSTITUTION_RECEIPT_INVALID',
        'Substitution receipt does not prove current admission',
        { errors: verification.errors, state: verification.state }
      );
    }

    if (!this.#delegate || typeof this.#delegate.execute !== 'function') {
      throw new PartAdmissionBrokerRefusal(
        'PART_DELEGATE_REQUIRED',
        'An admitted broker delegate with execute() is required'
      );
    }

    const result = await this.#delegate.execute(request);
    const admission = Object.freeze({
      requirementDigest: this.#requirement.digest,
      candidateDigest: this.#candidate.digest,
      judgementDigest: judgement.digest,
      substitutionReceiptHash: this.#substitutionReceipt.receiptHash,
    });

    if (result && typeof result === 'object' && !Array.isArray(result)) {
      return Object.freeze({ ...result, partAdmission: admission });
    }
    return Object.freeze({ result, partAdmission: admission });
  }
}

export function createPartAdmissionBroker(options) {
  return new PartAdmissionBroker(options);
}
