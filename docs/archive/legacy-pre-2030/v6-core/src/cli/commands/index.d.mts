/**
 * Receipt creation command
 * @param {Object} args - Command arguments
 * @returns {Promise<Object>}
 */
export function receiptCreate(args: any): Promise<any>;
/**
 * Receipt verification command
 * @param {Object} args - Command arguments
 * @returns {Promise<Object>}
 */
export function receiptVerify(args: any): Promise<any>;
/**
 * Delta proposal command
 * @param {Object} args - Command arguments
 * @returns {Promise<Object>}
 */
export function deltaPropose(args: any): Promise<any>;
/**
 * Delta apply command
 * @param {Object} args - Command arguments
 * @returns {Promise<Object>}
 */
export function deltaApply(args: any): Promise<any>;
declare const _default: {
    'receipt:create': typeof receiptCreate;
    'receipt:verify': typeof receiptVerify;
    'delta:propose': typeof deltaPropose;
    'delta:apply': typeof deltaApply;
};
export default _default;
