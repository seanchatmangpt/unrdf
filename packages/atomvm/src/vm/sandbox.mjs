/**
 * Safety Sandbox
 * Dynamically filters unsafe opcodes based on the ODRL policy
 */

const UNSAFE_OPCODES = new Set([
  'SYS_EXEC',
  'FS_WRITE_RAW',
  'NET_LISTEN',
  'MEM_INSPECT'
]);

export class SecuritySandbox {
  /**
   * @param {Object} executionInvocation - The invocation context
   * @param {Object} executionInvocation.policy - The ODRL policy graph
   */
  constructor(executionInvocation) {
    this.context = executionInvocation || {};
  }

  /**
   * Intercept opcodes and validate against policy
   * @param {Array} opcodes - The opcodes to process
   * @returns {Array} The validated opcodes
   * @throws {Error} ConstitutionalViolationError if policy check fails
   */
  interceptOpcodes(opcodes) {
    // Only intercept if we have opcodes to check
    if (!Array.isArray(opcodes)) return [];

    return opcodes.filter(op => {
      // Check if it's a privileged/unsafe opcode
      if (UNSAFE_OPCODES.has(op.name)) {
        const permission = `mcpp:${op.name}`;
        if (!this._evaluateOdrlPolicy(permission)) {
          throw new Error(`ConstitutionalViolationError: Opcode ${op.name} is prohibited by current sandbox policy.`);
        }
      }
      return true;
    });
  }

  _evaluateOdrlPolicy(permission) {
    // In production, this queries the DfLSS Policy Graph (RDFStore)
    // currently bound to the execution context.
    const policy = this.context.policy;
    if (!policy || typeof policy.hasPermission !== 'function') {
      // Fail-safe: No policy or missing hasPermission method means no unsafe access
      return false;
    }
    
    return policy.hasPermission(permission);
  }
}