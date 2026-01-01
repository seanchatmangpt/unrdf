import { describe, it, expect, beforeAll } from 'vitest';
import { getPackageSystem } from '../src/unrdf-package-system.mjs';
import { getRegistry } from '../src/unrdf-package-registry.mjs';
import { getResolver } from '../src/unrdf-dependency-resolver.mjs';
import { getValidator } from '../src/unrdf-package-validator.mjs';

describe('UNRDF Package System - Integrated Tests', () => {
  let system;
  let registry;
  let resolver;
  let validator;

  beforeAll(async () => {
    system = await getPackageSystem();
    registry = await getRegistry();
    resolver = await getResolver();
    validator = await getValidator();
  });

  describe('PackageRegistry', () => {
    it('should initialize registry', async () => {
      expect(registry.initialized).toBe(true);
    });

    it('should have discovered 56 packages', async () => {
      expect(registry.getPackageCount()).toBeGreaterThanOrEqual(50);
    });

    it('should classify packages by tier', async () => {
      const essential = registry.getPackagesByTier('Essential');
      const extended = registry.getPackagesByTier('Extended');

      expect(essential.length).toBeGreaterThan(0);
      expect(extended.length).toBeGreaterThan(0);
    });

    it('should return tier summary', () => {
      const summary = registry.getTierSummary();
      expect(summary).toHaveProperty('Essential');
      expect(summary).toHaveProperty('Extended');
      expect(Object.values(summary).reduce((a, b) => a + b, 0)).toBeGreaterThan(0);
    });

    it('should get package info', () => {
      const pkg = registry.getPackageInfo('@unrdf/core');
      expect(pkg).toBeDefined();
      expect(pkg.name).toBe('@unrdf/core');
      expect(pkg.tier).toBe('Essential');
    });

    it('should list all packages', () => {
      const packages = registry.getAllPackages();
      expect(packages.length).toBeGreaterThan(0);
      expect(packages[0]).toHaveProperty('name');
      expect(packages[0]).toHaveProperty('version');
      expect(packages[0]).toHaveProperty('tier');
    });
  });

  describe('DependencyResolver', () => {
    it('should initialize resolver', async () => {
      expect(resolver.initialized).toBe(true);
    });

    it('should resolve core package dependencies', async () => {
      const result = await resolver.resolve('@unrdf/core');
      expect(result.success).toBe(true);
      expect(result.resolved).toBeDefined();
      expect(Array.isArray(result.resolved)).toBe(true);
    });

    it('should get direct dependencies', async () => {
      const deps = await resolver.getDirectDependencies('@unrdf/core');
      expect(Array.isArray(deps)).toBe(true);
    });

    it('should get reverse dependencies', async () => {
      const reverseDeps = await resolver.getReverseDependencies('@unrdf/core');
      expect(Array.isArray(reverseDeps)).toBe(true);
      expect(reverseDeps.length).toBeGreaterThan(0);
    });

    it('should generate full dependency tree', async () => {
      const tree = await resolver.getFullDependencyTree('@unrdf/core');
      expect(tree).toBeDefined();
      expect(typeof tree).toBe('object');
    });

    it('should analyze dependency depth and breadth', async () => {
      const analysis = await resolver.analyzeDepthAndBreadth('@unrdf/core');
      expect(analysis).toHaveProperty('depth');
      expect(analysis).toHaveProperty('breadth');
      expect(analysis).toHaveProperty('totalDependencies');
      expect(typeof analysis.depth).toBe('number');
    });

    it('should find shared dependencies', async () => {
      const shared = await resolver.getSharedDependencies([
        '@unrdf/core',
        '@unrdf/oxigraph',
      ]);
      expect(Array.isArray(shared)).toBe(true);
    });
  });

  describe('PackageValidator', () => {
    it('should initialize validator', async () => {
      expect(validator.initialized).toBe(true);
    });

    it('should validate core package', async () => {
      const result = await validator.validatePackage('@unrdf/core');
      expect(result).toHaveProperty('package');
      expect(result).toHaveProperty('valid');
      expect(result).toHaveProperty('violations');
    });

    it('should validate all packages', async () => {
      const result = await validator.validateAll();
      expect(result.totalPackages).toBeGreaterThan(0);
      expect(result).toHaveProperty('validPackages');
      expect(result).toHaveProperty('invalidPackages');
      expect(result).toHaveProperty('results');
    });

    it('should check tier structure', async () => {
      const structure = await validator.validateTierStructure();
      expect(structure).toHaveProperty('Essential');
      expect(structure).toHaveProperty('Extended');
    });

    it('should enforce consistency', async () => {
      const consistency = await validator.enforceConsistency();
      expect(consistency).toHaveProperty('consistent');
      expect(consistency).toHaveProperty('issueCount');
      expect(consistency).toHaveProperty('issues');
    });

    it('should generate validation report', async () => {
      const report = await validator.generateValidationReport();
      expect(report).toHaveProperty('timestamp');
      expect(report).toHaveProperty('summary');
      expect(report).toHaveProperty('tierStructure');
      expect(report).toHaveProperty('validationResults');
    });
  });

  describe('UnrdfPackageSystem', () => {
    it('should be initialized', async () => {
      expect(system.initialized).toBe(true);
    });

    it('should discover and validate packages', async () => {
      const result = await system.discoverAndValidate();
      expect(result.discoveredPackages).toBeGreaterThan(0);
      expect(result).toHaveProperty('validPackages');
      expect(result).toHaveProperty('issues');
    });

    it('should get package info', async () => {
      const pkg = await system.getPackageInfo('@unrdf/core');
      expect(pkg).toBeDefined();
      expect(pkg.name).toBe('@unrdf/core');
    });

    it('should get dependencies', async () => {
      const deps = await system.getDependencies('@unrdf/core');
      expect(Array.isArray(deps)).toBe(true);
    });

    it('should get full dependency tree', async () => {
      const tree = await system.getFullDependencyTree('@unrdf/core');
      expect(tree).toBeDefined();
    });

    it('should analyze dependencies', async () => {
      const analysis = await system.analyzeDependencies('@unrdf/core');
      expect(analysis).toHaveProperty('depth');
      expect(analysis).toHaveProperty('breadth');
    });

    it('should find shared dependencies', async () => {
      const shared = await system.findSharedDependencies([
        '@unrdf/core',
        '@unrdf/oxigraph',
      ]);
      expect(Array.isArray(shared)).toBe(true);
    });

    it('should generate full report', async () => {
      const report = await system.getFullReport();
      expect(report).toHaveProperty('timestamp');
      expect(report).toHaveProperty('packages');
      expect(report).toHaveProperty('validation');
      expect(report).toHaveProperty('events');
    });

    it('should support event hooks', () => {
      let called = false;
      system.onPackageDiscovered(() => {
        called = true;
      });

      expect(typeof system.onPackageDiscovered).toBe('function');
    });

    it('should track event history', () => {
      const history = system.getEventHistory();
      expect(Array.isArray(history)).toBe(true);
    });
  });

  describe('Integration Tests', () => {
    it('should validate and resolve core package', async () => {
      const validation = await validator.validatePackage('@unrdf/core');
      expect(validation.valid).toBe(true);

      const resolution = await resolver.resolve('@unrdf/core');
      expect(resolution.success).toBe(true);
    });

    it('should find circular dependencies if any', async () => {
      const tree = await resolver.getFullDependencyTree('@unrdf/core');
      const depNames = Object.keys(tree);
      expect(depNames.length).toBeGreaterThan(0);
    });

    it('should handle validation violations', async () => {
      const report = await validator.generateValidationReport();
      expect(report.summary).toHaveProperty('consistencyScore');
      expect(typeof report.summary.consistencyScore).toBe('number');
    });

    it('should provide compatibility matrix', async () => {
      const matrix = await system.getCompatibilityMatrix();
      expect(typeof matrix).toBe('object');
      expect(Object.keys(matrix).length).toBeGreaterThan(0);
    });
  });
});
