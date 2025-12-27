/**
 * CLI Commands - Individual command implementations
 *
 * @module @unrdf/v6-core/cli/commands
 */

import { createReceipt, verifyReceipt } from '../../receipts/index.mjs';
import { createDeltaProposal, applyDelta } from '../../delta/index.mjs';

/**
 * Receipt creation command
 * @param {Object} args - Command arguments
 * @returns {Promise<Object>}
 */
export async function receiptCreate(args) {
  const receipt = createReceipt(args.operation || 'unknown', args.data || {});
  return { receipt };
}

/**
 * Receipt verification command
 * @param {Object} args - Command arguments
 * @returns {Promise<Object>}
 */
export async function receiptVerify(args) {
  const isValid = verifyReceipt(args.receipt);
  return { valid: isValid };
}

/**
 * Delta proposal command
 * @param {Object} args - Command arguments
 * @returns {Promise<Object>}
 */
export async function deltaPropose(args) {
  const proposal = createDeltaProposal(
    args.from || 'v0',
    args.to || 'v1',
    args.operations || []
  );
  return { proposal };
}

/**
 * Delta apply command
 * @param {Object} args - Command arguments
 * @returns {Promise<Object>}
 */
export async function deltaApply(args) {
  await applyDelta(args.store, args.proposal);
  return { status: 'applied' };
}

export default {
  'receipt:create': receiptCreate,
  'receipt:verify': receiptVerify,
  'delta:propose': deltaPropose,
  'delta:apply': deltaApply,
};
