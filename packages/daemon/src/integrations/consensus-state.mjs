/**
 * @file Consensus State Constants
 * @module @unrdf/daemon/integrations/consensus-state
 * @description State enums and constants for consensus management
 */

/**
 * Consensus operation state enum
 * @enum {string}
 */
export const ConsensusOperationState = {
  PENDING: 'pending',
  REPLICATED: 'replicated',
  COMMITTED: 'committed',
  FAILED: 'failed',
};

/**
 * Network partition state enum
 * @enum {string}
 */
export const PartitionState = {
  HEALTHY: 'healthy',
  PARTITIONED: 'partitioned',
  RECOVERING: 'recovering',
};
