/**
 * @file KGC Manager Access
 * @description Singleton access to initialized KGC managers
 */

/**
 * @typedef {Object} Managers
 * @property {Object} hookManager - Knowledge Hook Manager
 * @property {Object} transactionManager - Transaction Manager
 * @property {Object} policyPack - Policy Pack
 * @property {Object} effectSandbox - Effect Sandbox
 * @property {Object} lockchainWriter - Lockchain Writer
 * @property {Object} resolutionLayer - Resolution Layer
 * @property {Object} observability - Observability Manager
 */

/** @type {Managers|null} */
let managers = null

/**
 * Set manager instances
 * @param {Managers} managerInstances - Manager instances
 */
export function setManagers(managerInstances) {
  managers = managerInstances
}

/**
 * Get manager instances
 * @returns {Managers} Manager instances
 * @throws {Error} If managers not initialized
 */
export function getManagers() {
  if (!managers) {
    throw new Error('KGC managers not initialized. Ensure server/plugins/00.managers.mjs ran.')
  }
  return managers
}

/**
 * Check if managers are initialized
 * @returns {boolean} Whether managers are initialized
 */
export function hasManagers() {
  return managers !== null
}
