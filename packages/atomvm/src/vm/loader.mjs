/**
 * Constitutional Bytecode Loader
 * Validates cryptographic receipts before executing AtomVM bytecode.
 */
import { verifyPQReceipt } from '@unrdf/receipts';
import { validate } from '@unrdf/oxigraph'; // Using Oxigraph's internal SHACL engine

export class ConstitutionalLoader {
  constructor(vm) {
    this.vm = vm;
  }

  async loadBytecode(bytecode, receipt) {
    if (!receipt) {
      throw new Error('ConstitutionalViolationError: Bytecode requires a valid SpecKit receipt.');
    }

    const isValid = await verifyPQReceipt(receipt);

    if (!isValid) {
      throw new Error('ConstitutionalViolationError: Invalid cryptographic receipt signature.');
    }

    await this._validateShaclHeaders(bytecode, receipt);

    return this.vm.load(bytecode);
  }

  async _validateShaclHeaders(bytecode, receipt) {
    // Validate the bytecode blob conformance against the SHACL shape
    // defined in the SpecKit header using the @unrdf/shacl engine.
    const conforms = await validate(bytecode, receipt.shape);
    if (!conforms) {
      throw new Error('ConstitutionalViolationError: SHACL header validation failed.');
    }
  }
}
