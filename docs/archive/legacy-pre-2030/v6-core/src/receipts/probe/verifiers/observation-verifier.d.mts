/**
 * Verify a probe observation receipt
 *
 * Checks:
 * 1. Schema validation
 * 2. Observation hash integrity
 * 3. Determinism checks
 * 4. Receipt chain integrity
 *
 * @param {Object} receipt - ProbeObservationReceipt to verify
 * @returns {Promise<Object>} Verification result
 *
 * @example
 * const result = await verifyProbeObservation(receipt);
 * console.log(result.valid); // true if all checks pass
 * console.log(result.checks); // Details of each check
 */
export function verifyProbeObservation(receipt: any): Promise<any>;
/**
 * Verify observation timestamp is valid
 *
 * @param {Object} receipt - ProbeObservationReceipt
 * @returns {Object} Verification result
 */
export function verifyObservationTimestamp(receipt: any): any;
/**
 * Verify observation index sequence
 *
 * @param {Array<Object>} receipts - Array of receipts to check
 * @returns {Object} Verification result
 */
export function verifyObservationSequence(receipts: Array<any>): any;
declare namespace _default {
    export { verifyProbeObservation };
    export { verifyObservationTimestamp };
    export { verifyObservationSequence };
}
export default _default;
