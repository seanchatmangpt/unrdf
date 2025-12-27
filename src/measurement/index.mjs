/**
 * @fileoverview Measurement Module - System Health Metrics
 *
 * Provides comprehensive measurement of system health through four core quantities:
 *
 * - **D_t (Dimension)**: Representable dimensions per package and system-wide
 * - **TC (Total Correlation)**: Coupling between packages/partitions
 * - **TE (Transfer Entropy)**: Causal influence paths between partitions
 * - **C_t (Capacity)**: Admissible channel capacity per epoch
 *
 * @module measurement
 *
 * @example
 * import {
 *   HealthDashboard,
 *   DimensionComputer,
 *   CorrelationComputer,
 *   TransferEntropyComputer,
 *   CapacityComputer,
 *   CertificateGenerator
 * } from './measurement/index.mjs';
 *
 * // Create dashboard for real-time monitoring
 * const dashboard = new HealthDashboard();
 * dashboard.start(universe);
 *
 * // Or use individual computers
 * const dim = await DimensionComputer.computeSystemDimension(universe);
 */

// Core Computers
export {
  DimensionComputer,
  createDimensionComputer,
  computeDimension,
  checkDimensionHealth
} from './dimension-computer.mjs';

export {
  CorrelationComputer,
  createCorrelationComputer,
  computeTotalCorrelation,
  checkCorrelationHealth
} from './correlation-computer.mjs';

export {
  TransferEntropyComputer,
  createTransferEntropyComputer,
  computeTransferEntropy,
  checkTEHealth
} from './transfer-entropy-computer.mjs';

export {
  CapacityComputer,
  createCapacityComputer,
  computeCapacity,
  checkCapacityHealth
} from './capacity-computer.mjs';

// Certificate Generator
export {
  CertificateGenerator,
  createCertificateGenerator,
  generateCertificate
} from './certificate-generator.mjs';

// Health Dashboard
export {
  HealthDashboard,
  createHealthDashboard,
  startDashboard
} from './health-dashboard.mjs';

/**
 * Create a complete measurement system with all components
 *
 * @param {Object} [config] - Configuration for all components
 * @returns {Object} Measurement system with all computers and dashboard
 *
 * @example
 * const system = createMeasurementSystem();
 * system.dashboard.start(universe);
 *
 * // Access individual computers
 * const dimension = await system.dimension.computeSystemDimension(universe);
 */
export function createMeasurementSystem(config = {}) {
  const dimensionComputer = new (await import('./dimension-computer.mjs')).DimensionComputer(config.dimension);
  const correlationComputer = new (await import('./correlation-computer.mjs')).CorrelationComputer(config.correlation);
  const transferEntropyComputer = new (await import('./transfer-entropy-computer.mjs')).TransferEntropyComputer(config.transferEntropy);
  const capacityComputer = new (await import('./capacity-computer.mjs')).CapacityComputer(config.capacity);
  const certificateGenerator = new (await import('./certificate-generator.mjs')).CertificateGenerator(config.certificate);
  const dashboard = new (await import('./health-dashboard.mjs')).HealthDashboard(config.dashboard);

  return {
    dimension: dimensionComputer,
    correlation: correlationComputer,
    transferEntropy: transferEntropyComputer,
    capacity: capacityComputer,
    certificate: certificateGenerator,
    dashboard,

    /**
     * Compute all four metrics at once
     * @param {Object} universe - Universe to measure
     * @returns {Promise<Object>} All measurements
     */
    async computeAll(universe) {
      const dimension = await dimensionComputer.computeSystemDimension(universe);
      const correlation = correlationComputer.computeTotalCorrelation();
      const transferEntropy = transferEntropyComputer.computeCausalGraph();
      const capacity = capacityComputer.computeSystemCapacity();

      return {
        dimension,
        correlation,
        transferEntropy,
        capacity,
        timestamp: new Date().toISOString()
      };
    },

    /**
     * Generate a certificate from current state
     * @param {Object} universe - Universe to measure
     * @returns {Promise<Object>} Dimension certificate
     */
    async generateCertificate(universe) {
      const measurements = await this.computeAll(universe);
      return certificateGenerator.generateCertificate(measurements);
    },

    /**
     * Clear all history and caches
     */
    clear() {
      dimensionComputer.clearCache();
      dimensionComputer.clearHistory();
      correlationComputer.clear();
      transferEntropyComputer.clear();
      capacityComputer.clear();
      certificateGenerator.clearHistory();
    },

    /**
     * Export all data for persistence
     * @returns {Object} Exportable state
     */
    exportAll() {
      return {
        dimension: dimensionComputer.exportData(),
        correlation: correlationComputer.exportData(),
        transferEntropy: transferEntropyComputer.exportData(),
        capacity: capacityComputer.exportData(),
        certificates: certificateGenerator.exportState(),
        dashboard: dashboard.exportState(),
        exportedAt: new Date().toISOString()
      };
    }
  };
}

/**
 * Default export - HealthDashboard for convenient access
 */
export default HealthDashboard;
