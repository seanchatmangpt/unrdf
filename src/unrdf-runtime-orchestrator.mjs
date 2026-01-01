import { getPackageSystem } from './unrdf-package-system.mjs';
import { getHealthMonitor } from './unrdf-package-health-monitor.mjs';
import { getModuleFederation } from './unrdf-module-federation.mjs';
import { getPackageOptimizer } from './unrdf-package-optimizer.mjs';
import { VerificationState } from './unrdf-verification-engine.mjs';
import { getCoherenceRepair } from './unrdf-coherence-repair.mjs';

export class UnrdfRuntimeOrchestrator {
  constructor() {
    this.packageSystem = null;
    this.healthMonitor = null;
    this.moduleFederation = null;
    this.optimizer = null;
    this.verificationState = null;
    this.coherenceRepair = null;
    this.state = {
      initialized: false,
      running: false,
      startTime: null,
      uptime: 0,
    };
    this.operations = [];
  }

  async initialize() {
    if (this.state.initialized) return;

    this.packageSystem = await getPackageSystem();
    this.healthMonitor = await getHealthMonitor(this.packageSystem);
    this.moduleFederation = await getModuleFederation(this.packageSystem);
    this.optimizer = await getPackageOptimizer(
      this.packageSystem,
      this.moduleFederation,
      this.healthMonitor
    );

    this.verificationState = new VerificationState(
      this.packageSystem,
      this.moduleFederation,
      this.healthMonitor,
      this.optimizer
    );

    this.coherenceRepair = await getCoherenceRepair(
      this.verificationState,
      this.packageSystem,
      this.moduleFederation,
      this.optimizer
    );

    this.state.initialized = true;
    this.state.startTime = Date.now();
    this.state.running = true;

    this._recordOperation('initialize', { success: true });
  }

  async loadEssentialTier() {
    if (!this.state.initialized) await this.initialize();

    try {
      const modules = await this.moduleFederation.loadEssential({
        cache: true,
        validateTier: true,
        preloadDeps: true,
      });

      const loadedCount = Array.from(modules.values()).filter((m) => !m.error).length;

      this._recordOperation('loadEssentialTier', {
        success: true,
        modulesLoaded: loadedCount,
        totalModules: modules.size,
      });

      return {
        success: true,
        modules,
        loadedCount,
      };
    } catch (error) {
      this._recordOperation('loadEssentialTier', { success: false, error: error.message });
      throw error;
    }
  }

  async loadModule(packageName, options = {}) {
    if (!this.state.initialized) await this.initialize();

    try {
      const module = await this.moduleFederation.loadModule(packageName, {
        cache: true,
        validateTier: true,
        preloadDeps: options.preloadDeps || false,
      });

      this._recordOperation('loadModule', {
        success: true,
        package: packageName,
        hasError: false,
      });

      return module;
    } catch (error) {
      this._recordOperation('loadModule', {
        success: false,
        package: packageName,
        error: error.message,
      });

      throw error;
    }
  }

  async loadDependencySet(packageName) {
    if (!this.state.initialized) await this.initialize();

    try {
      const modules = await this.moduleFederation.loadDependencySet(packageName, {
        cache: true,
        validateTier: true,
      });

      const loadedCount = Array.from(modules.values()).filter((m) => !m.error).length;

      this._recordOperation('loadDependencySet', {
        success: true,
        package: packageName,
        modulesLoaded: loadedCount,
        totalModules: modules.size,
      });

      return modules;
    } catch (error) {
      this._recordOperation('loadDependencySet', {
        success: false,
        package: packageName,
        error: error.message,
      });

      throw error;
    }
  }

  async assessHealth() {
    if (!this.state.initialized) await this.initialize();

    const health = await this.healthMonitor.assessSystemHealth();

    this._recordOperation('assessHealth', {
      success: true,
      status: health.overallStatus,
      metrics: health.summary,
    });

    return health;
  }

  async optimizePackages() {
    if (!this.state.initialized) await this.initialize();

    try {
      const report = await this.optimizer.generateOptimizationReport();

      this._recordOperation('optimizePackages', {
        success: true,
        overallScore: report.scores.overallScore,
        issues: {
          circular: report.analysis.circular.length,
          unused: report.analysis.unused.length,
          heavy: report.analysis.heavy.length,
          deepPaths: report.analysis.deepPaths.length,
        },
      });

      return report;
    } catch (error) {
      this._recordOperation('optimizePackages', { success: false, error: error.message });
      throw error;
    }
  }

  async validateBoundaries() {
    if (!this.state.initialized) await this.initialize();

    const validation = await this.moduleFederation.validateBoundaries();

    this._recordOperation('validateBoundaries', {
      success: validation.valid,
      violations: validation.violations.length,
    });

    return validation;
  }

  async getSystemStatus() {
    if (!this.state.initialized) await this.initialize();

    this.state.uptime = Date.now() - this.state.startTime;

    const status = {
      state: this.state,
      packageSystem: {
        totalPackages: this.packageSystem.registry.getPackageCount(),
        tierSummary: this.packageSystem.registry.getTierSummary(),
      },
      federation: this.moduleFederation.getLoadStatus(),
      cache: this.moduleFederation.getCacheStats(),
      health: await this.healthMonitor.assessSystemHealth(),
      operations: {
        total: this.operations.length,
        lastOperations: this.operations.slice(-10),
      },
      timestamp: new Date().toISOString(),
    };

    return status;
  }

  async generateFullReport() {
    if (!this.state.initialized) await this.initialize();

    const report = {
      timestamp: new Date().toISOString(),
      system: await this.getSystemStatus(),
      optimization: await this.optimizer.generateOptimizationReport(),
      boundaries: await this.moduleFederation.validateBoundaries(),
      caching: await this.optimizer.suggestCachingStrategy(),
      parallelization: await this.optimizer.suggestParallelLoadingGroups(),
    };

    this._recordOperation('generateFullReport', { success: true });

    return report;
  }

  async runDiagnostics() {
    if (!this.state.initialized) await this.initialize();

    const diagnostics = {
      timestamp: new Date().toISOString(),
      checks: {
        circular: await this.optimizer.detectCircularDependencies(),
        unused: await this.optimizer.findUnusedDependencies(),
        heavy: await this.optimizer.detectHeavyDependencies(),
        tierViolations: await this.optimizer.suggestTierReorganization(),
        boundaries: (await this.moduleFederation.validateBoundaries()).violations,
      },
      summary: {
        hasCircularDeps: false,
        hasUnusedPackages: false,
        hasHeavyDeps: false,
        hasTierViolations: false,
        isBoundaryValid: true,
      },
    };

    diagnostics.summary.hasCircularDeps = diagnostics.checks.circular.length > 0;
    diagnostics.summary.hasUnusedPackages = diagnostics.checks.unused.length > 0;
    diagnostics.summary.hasHeavyDeps = diagnostics.checks.heavy.length > 0;
    diagnostics.summary.hasTierViolations = diagnostics.checks.tierViolations.length > 0;
    diagnostics.summary.isBoundaryValid = diagnostics.checks.boundaries.length === 0;

    this._recordOperation('runDiagnostics', {
      success: true,
      issues: Object.values(diagnostics.summary).filter((v) => v === true).length,
    });

    return diagnostics;
  }

  async checkCoherence() {
    if (!this.state.initialized) await this.initialize();

    const report = await this.verificationState.generateCoherenceReport();

    this._recordOperation('checkCoherence', {
      success: true,
      stateConsistency: report.coherence.stateConsistency,
      contradictions: report.coherence.contradictionCount,
      failureModes: report.coherence.failureModeCount,
      canContinue: this.verificationState.canContinue(),
      atFixedPoint: this.verificationState.hasReachedFixedPoint(),
    });

    return report;
  }

  getVerificationState() {
    return this.verificationState;
  }

  async getDetailedStatus() {
    if (!this.state.initialized) await this.initialize();

    const baseStatus = await this.getSystemStatus();
    const coherence = await this.checkCoherence();

    return {
      ...baseStatus,
      coherence: {
        stateConsistency: coherence.coherence.stateConsistency,
        contradictions: coherence.coherence.contradictionCount,
        failureModes: coherence.coherence.failureModeCount,
        limitationsAcknowledged: coherence.coherence.limitationAcknowledgement,
        canContinue: this.verificationState.canContinue(),
        atFixedPoint: this.verificationState.hasReachedFixedPoint(),
      },
    };
  }

  async runRepairCycle(maxAttempts = 5) {
    if (!this.state.initialized) await this.initialize();

    try {
      const results = await this.coherenceRepair.runRepairCycle(maxAttempts);

      this._recordOperation('runRepairCycle', {
        success: true,
        repairsAttempted: results.repairsAttempted,
        repairsSucceeded: results.repairsSucceeded,
        contradictionsResolved: results.contradictionsResolved,
      });

      return results;
    } catch (error) {
      this._recordOperation('runRepairCycle', { success: false, error: error.message });
      throw error;
    }
  }

  getRepairStatus() {
    if (!this.coherenceRepair) return null;
    return this.coherenceRepair.getRepairStats();
  }

  getRepairHistory() {
    if (!this.coherenceRepair) return null;
    return this.coherenceRepair.getRepairHistory();
  }

  _recordOperation(name, details) {
    this.operations.push({
      name,
      timestamp: new Date().toISOString(),
      details,
    });

    if (this.operations.length > 100) {
      this.operations = this.operations.slice(-100);
    }
  }

  async shutdown() {
    await this.moduleFederation.clearAllModules();
    this.state.running = false;
    this._recordOperation('shutdown', { success: true });
  }

  export() {
    return {
      state: this.state,
      operations: this.operations,
      timestamp: new Date().toISOString(),
    };
  }
}

let orchestratorInstance = null;

export async function getOrchestrator() {
  if (!orchestratorInstance) {
    orchestratorInstance = new UnrdfRuntimeOrchestrator();
    await orchestratorInstance.initialize();
  }

  return orchestratorInstance;
}

export default UnrdfRuntimeOrchestrator;
