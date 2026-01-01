import { getPackageSystem } from './unrdf-package-system.mjs';
import { lifecycleHooks } from './unrdf-package-hooks.mjs';

class PackageMetricsCollector {
  constructor() {
    this.metrics = {
      discovery: {
        startTime: null,
        endTime: null,
        duration: null,
        packageCount: 0,
      },
      loading: {
        attempts: 0,
        successful: 0,
        failed: 0,
        totalTime: 0,
        byPackage: new Map(),
      },
      validation: {
        checks: 0,
        passed: 0,
        failed: 0,
        violations: [],
      },
      resolution: {
        attempts: 0,
        successful: 0,
        failed: 0,
        totalTime: 0,
        byPackage: new Map(),
      },
      tiers: {
        Essential: { loaded: 0, failed: 0, totalTime: 0 },
        Extended: { loaded: 0, failed: 0, totalTime: 0 },
        Optional: { loaded: 0, failed: 0, totalTime: 0 },
        Internal: { loaded: 0, failed: 0, totalTime: 0 },
      },
      timeline: [],
    };
  }

  recordDiscoveryStart() {
    this.metrics.discovery.startTime = performance.now();
  }

  recordDiscoveryComplete(packageCount) {
    this.metrics.discovery.endTime = performance.now();
    this.metrics.discovery.duration = this.metrics.discovery.endTime - this.metrics.discovery.startTime;
    this.metrics.discovery.packageCount = packageCount;

    this.metrics.timeline.push({
      event: 'discovery:complete',
      timestamp: new Date().toISOString(),
      duration: this.metrics.discovery.duration,
      packageCount,
    });
  }

  recordPackageLoad(packageName, tier, durationMs, success = true) {
    const loadData = { packageName, tier, duration: durationMs, timestamp: new Date().toISOString() };

    if (success) {
      this.metrics.loading.successful++;
      this.metrics.tiers[tier].loaded++;
    } else {
      this.metrics.loading.failed++;
      this.metrics.tiers[tier].failed++;
    }

    this.metrics.loading.attempts++;
    this.metrics.loading.totalTime += durationMs;
    this.metrics.loading.byPackage.set(packageName, {
      duration: durationMs,
      success,
      timestamp: new Date().toISOString(),
    });

    this.metrics.tiers[tier].totalTime += durationMs;
    this.metrics.timeline.push({ ...loadData, event: 'package:load' });
  }

  recordValidation(packageName, passed, violations = []) {
    this.metrics.validation.checks++;
    if (passed) {
      this.metrics.validation.passed++;
    } else {
      this.metrics.validation.failed++;
      this.metrics.validation.violations.push({
        package: packageName,
        violations,
        timestamp: new Date().toISOString(),
      });
    }

    this.metrics.timeline.push({
      event: 'validation',
      packageName,
      passed,
      violationCount: violations.length,
      timestamp: new Date().toISOString(),
    });
  }

  recordResolution(packageName, resolvedCount, durationMs, success = true) {
    if (success) {
      this.metrics.resolution.successful++;
    } else {
      this.metrics.resolution.failed++;
    }

    this.metrics.resolution.attempts++;
    this.metrics.resolution.totalTime += durationMs;
    this.metrics.resolution.byPackage.set(packageName, {
      resolved: resolvedCount,
      duration: durationMs,
      success,
      timestamp: new Date().toISOString(),
    });

    this.metrics.timeline.push({
      event: 'resolution',
      packageName,
      resolvedCount,
      duration: durationMs,
      timestamp: new Date().toISOString(),
    });
  }

  getMetrics() {
    const loadingByPackage = Object.fromEntries(this.metrics.loading.byPackage);
    const resolutionByPackage = Object.fromEntries(this.metrics.resolution.byPackage);

    return {
      ...this.metrics,
      loading: {
        ...this.metrics.loading,
        byPackage: loadingByPackage,
        averageTime: this.metrics.loading.attempts > 0 ? this.metrics.loading.totalTime / this.metrics.loading.attempts : 0,
        successRate: this.metrics.loading.attempts > 0 ? (this.metrics.loading.successful / this.metrics.loading.attempts) * 100 : 0,
      },
      resolution: {
        ...this.metrics.resolution,
        byPackage: resolutionByPackage,
        averageTime: this.metrics.resolution.attempts > 0 ? this.metrics.resolution.totalTime / this.metrics.resolution.attempts : 0,
        successRate: this.metrics.resolution.attempts > 0 ? (this.metrics.resolution.successful / this.metrics.resolution.attempts) * 100 : 0,
      },
      validation: {
        ...this.metrics.validation,
        passRate: this.metrics.validation.checks > 0 ? (this.metrics.validation.passed / this.metrics.validation.checks) * 100 : 0,
      },
      timeline: this.metrics.timeline,
    };
  }

  reset() {
    this.metrics = {
      discovery: { startTime: null, endTime: null, duration: null, packageCount: 0 },
      loading: { attempts: 0, successful: 0, failed: 0, totalTime: 0, byPackage: new Map() },
      validation: { checks: 0, passed: 0, failed: 0, violations: [] },
      resolution: { attempts: 0, successful: 0, failed: 0, totalTime: 0, byPackage: new Map() },
      tiers: {
        Essential: { loaded: 0, failed: 0, totalTime: 0 },
        Extended: { loaded: 0, failed: 0, totalTime: 0 },
        Optional: { loaded: 0, failed: 0, totalTime: 0 },
        Internal: { loaded: 0, failed: 0, totalTime: 0 },
      },
      timeline: [],
    };
  }
}

export class PackageHealthMonitor {
  constructor() {
    this.metrics = new PackageMetricsCollector();
    this.thresholds = {
      loadTimeMs: 100,
      resolutionTimeMs: 50,
      diskReadErrorRate: 0.05,
      circularDepTimeout: 1000,
    };
    this.healthStatus = new Map();
  }

  async initialize(system) {
    this.system = system;
    this._attachHooks();
  }

  _attachHooks() {
    this.metrics.recordDiscoveryStart();

    lifecycleHooks.onDiscoveryComplete(async (data) => {
      this.metrics.recordDiscoveryComplete(data.packagesFound);
    });

    lifecycleHooks.onPackageLoaded(async (pkg) => {
      const startTime = performance.now();
      const duration = performance.now() - startTime;
      this.metrics.recordPackageLoad(pkg.name, pkg.tier || 'Unknown', duration, true);
    });

    lifecycleHooks.onPackageError(async (error) => {
      this.metrics.recordPackageLoad(error.package, 'Unknown', 0, false);
    });

    lifecycleHooks.onDependencyResolved(async (result) => {
      const duration = 0;
      this.metrics.recordResolution(result.package, result.resolved.length, duration, true);
    });

    lifecycleHooks.onTierViolation(async (violation) => {
      this.metrics.recordValidation(violation.package, false, [violation.violation]);
    });
  }

  async assessPackageHealth(packageName) {
    const loadMetric = this.metrics.metrics.loading.byPackage.get(packageName);
    const resolutionMetric = this.metrics.metrics.resolution.byPackage.get(packageName);

    const health = {
      package: packageName,
      status: 'healthy',
      issues: [],
      timestamp: new Date().toISOString(),
    };

    if (loadMetric && loadMetric.duration > this.thresholds.loadTimeMs) {
      health.status = 'warning';
      health.issues.push({
        type: 'slow_load',
        severity: 'warning',
        message: `Load time ${loadMetric.duration.toFixed(2)}ms exceeds threshold ${this.thresholds.loadTimeMs}ms`,
      });
    }

    if (resolutionMetric && resolutionMetric.duration > this.thresholds.resolutionTimeMs) {
      health.status = 'warning';
      health.issues.push({
        type: 'slow_resolution',
        severity: 'warning',
        message: `Resolution time ${resolutionMetric.duration.toFixed(2)}ms exceeds threshold ${this.thresholds.resolutionTimeMs}ms`,
      });
    }

    if (loadMetric && !loadMetric.success) {
      health.status = 'unhealthy';
      health.issues.push({
        type: 'load_failure',
        severity: 'critical',
        message: 'Package failed to load',
      });
    }

    this.healthStatus.set(packageName, health);
    return health;
  }

  async assessSystemHealth() {
    const metrics = this.metrics.getMetrics();
    const allPackages = this.system.registry.getAllPackages();

    const systemHealth = {
      timestamp: new Date().toISOString(),
      overallStatus: 'healthy',
      summary: {
        totalPackages: metrics.discovery.packageCount,
        loadedPackages: metrics.loading.successful,
        failedPackages: metrics.loading.failed,
        validPackages: metrics.validation.passed,
        invalidPackages: metrics.validation.failed,
      },
      performance: {
        avgLoadTime: metrics.loading.averageTime.toFixed(2),
        avgResolutionTime: metrics.resolution.averageTime.toFixed(2),
        discoveryTime: metrics.discovery.duration?.toFixed(2) || 'N/A',
      },
      rates: {
        loadSuccessRate: metrics.loading.successRate.toFixed(2),
        validationPassRate: metrics.validation.passRate.toFixed(2),
        resolutionSuccessRate: metrics.resolution.successRate.toFixed(2),
      },
      tierBreakdown: metrics.tiers,
      packageHealths: [],
    };

    for (const pkg of allPackages) {
      const health = await this.assessPackageHealth(pkg.name);
      systemHealth.packageHealths.push(health);

      if (health.status === 'unhealthy') {
        systemHealth.overallStatus = 'unhealthy';
      } else if (health.status === 'warning' && systemHealth.overallStatus !== 'unhealthy') {
        systemHealth.overallStatus = 'warning';
      }
    }

    return systemHealth;
  }

  getPerformanceProfile() {
    const metrics = this.metrics.getMetrics();
    const profile = {
      bottlenecks: [],
      optimizations: [],
      timeline: metrics.timeline,
    };

    const loadingByPackage = Array.from(metrics.loading.byPackage.values());
    const slowLoads = loadingByPackage.filter((l) => l.duration > this.thresholds.loadTimeMs);

    if (slowLoads.length > 0) {
      const avgSlowLoad = slowLoads.reduce((a, b) => a + b.duration, 0) / slowLoads.length;
      profile.bottlenecks.push({
        type: 'slow_package_loads',
        count: slowLoads.length,
        avgDuration: avgSlowLoad.toFixed(2),
        recommendation: 'Consider code splitting or lazy loading',
      });
    }

    if (metrics.discovery.duration > 5000) {
      profile.bottlenecks.push({
        type: 'slow_discovery',
        duration: metrics.discovery.duration.toFixed(2),
        recommendation: 'Cache discovery results or implement progressive loading',
      });
    }

    if (metrics.loading.successRate >= 95) {
      profile.optimizations.push({
        type: 'high_reliability',
        metric: metrics.loading.successRate.toFixed(2),
        note: 'Package loading is very reliable',
      });
    }

    if (metrics.validation.passRate >= 90) {
      profile.optimizations.push({
        type: 'good_validation',
        metric: metrics.validation.passRate.toFixed(2),
        note: 'Most packages pass validation constraints',
      });
    }

    return profile;
  }

  export() {
    return {
      metrics: this.metrics.getMetrics(),
      health: Array.from(this.healthStatus.values()),
      performance: this.getPerformanceProfile(),
      timestamp: new Date().toISOString(),
    };
  }
}

export const healthMonitor = new PackageHealthMonitor();

export async function getHealthMonitor(system) {
  await healthMonitor.initialize(system);
  return healthMonitor;
}

export default healthMonitor;
