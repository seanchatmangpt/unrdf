/**
 * Package graph validator over the admitted ggen projection.
 *
 * Validation laws are correspondence laws, not a second package ontology:
 *   manifest observation -> RDF O* -> ggen projection -> registry/resolver.
 * SCCs are topology. A cycle is surfaced as evidence, never rejected merely
 * because it is cyclic. Tier names are projections, not ambient dependency law.
 */
import { getRegistry } from './unrdf-package-registry.mjs';
import { getResolver } from './unrdf-dependency-resolver.mjs';

const VALID_TIERS = new Set(['Essential', 'Extended', 'Optional', 'Internal']);
const REQUIRED_FIELDS = ['name', 'version', 'path', 'tier', 'manifestDigest', 'dependencies', 'reverseDependencies', 'sccId', 'sccSize', 'cyclic'];

function violation(code, message, evidence = {}) {
  return { code, message, ...evidence };
}

export class PackageValidator {
  constructor() {
    this.registry = null;
    this.resolver = null;
  }

  async initialize() {
    this.registry = await getRegistry();
    this.resolver = await getResolver();
  }

  async validatePackage(packageName) {
    if (!this.registry) await this.initialize();
    const result = { package: packageName, valid: true, violations: [], warnings: [], topology: null };
    const pkg = this.registry.getPackageInfo(packageName);
    if (!pkg) {
      result.valid = false;
      result.violations.push(violation('PACKAGE_NOT_FOUND', `Package ${packageName} is absent from the admitted projection`));
      return result;
    }

    result.violations.push(...this._validateProjectionShape(pkg));
    result.violations.push(...await this._validateDependencyCorrespondence(pkg));

    const resolution = await this.resolver.resolve(packageName, { checkConflicts: true });
    if (!resolution.success) {
      result.violations.push(...resolution.conflicts.map(conflict => violation(
        'DEPENDENCY_GRAPH_INVALID',
        conflict.message,
        { type: conflict.type, dependency: conflict.package },
      )));
    }

    const component = resolution.cycles?.find(cycle => cycle.includes(packageName)) || null;
    result.topology = {
      sccId: pkg.sccId,
      sccSize: pkg.sccSize,
      cyclic: pkg.cyclic,
      component,
      dependencyCount: pkg.dependencies.length,
      reverseDependencyCount: pkg.reverseDependencies.length,
    };
    if (component) {
      result.warnings.push({
        code: 'SCC_TOPOLOGY',
        message: `${packageName} participates in a strongly connected component`,
        component,
      });
    }

    result.valid = result.violations.length === 0;
    return result;
  }

  _validateProjectionShape(pkg) {
    const violations = [];
    for (const field of REQUIRED_FIELDS) {
      if (pkg[field] === undefined || pkg[field] === null) {
        violations.push(violation('PROJECTION_FIELD_MISSING', `${pkg.name} is missing projected field ${field}`, { field }));
      }
    }
    if (!VALID_TIERS.has(pkg.tier)) {
      violations.push(violation('PROJECTION_TIER_INVALID', `${pkg.name} has unknown tier ${pkg.tier}`, { tier: pkg.tier }));
    }
    if (typeof pkg.path !== 'string' || !pkg.path.startsWith('packages/')) {
      violations.push(violation('PROJECTION_PATH_INVALID', `${pkg.name} has invalid package path`, { path: pkg.path }));
    }
    if (!Array.isArray(pkg.dependencies) || !Array.isArray(pkg.reverseDependencies)) {
      violations.push(violation('PROJECTION_ADJACENCY_INVALID', `${pkg.name} dependency adjacency must be arrays`));
    }
    if (!Number.isInteger(pkg.sccSize) || pkg.sccSize < 1) {
      violations.push(violation('PROJECTION_SCC_INVALID', `${pkg.name} has invalid SCC size`, { sccSize: pkg.sccSize }));
    }
    if (typeof pkg.manifestDigest !== 'string' || !/^[a-f0-9]{64}$/i.test(pkg.manifestDigest)) {
      violations.push(violation('MANIFEST_DIGEST_INVALID', `${pkg.name} manifest digest is not SHA-256`, { manifestDigest: pkg.manifestDigest }));
    }
    if (pkg.cyclic !== (pkg.sccSize > 1 || pkg.dependencies.includes(pkg.name))) {
      violations.push(violation('SCC_PROJECTION_MISMATCH', `${pkg.name} cyclic flag disagrees with SCC topology`, {
        cyclic: pkg.cyclic,
        sccSize: pkg.sccSize,
      }));
    }
    return violations;
  }

  async _validateDependencyCorrespondence(pkg) {
    const violations = [];
    const direct = await this.resolver.getDirectDependencies(pkg.name);
    const projected = [...pkg.dependencies].sort();
    if (JSON.stringify([...direct].sort()) !== JSON.stringify(projected)) {
      violations.push(violation('DEPENDENCY_PROJECTION_DRIFT', `${pkg.name} resolver adjacency differs from registry projection`, {
        registry: projected,
        resolver: [...direct].sort(),
      }));
    }

    for (const dep of projected) {
      const depPkg = this.registry.getPackageInfo(dep);
      if (!depPkg) {
        violations.push(violation('DEPENDENCY_NOT_ADMITTED', `${pkg.name} depends on non-admitted package ${dep}`, { dependency: dep }));
        continue;
      }
      if (!depPkg.reverseDependencies.includes(pkg.name)) {
        violations.push(violation('REVERSE_EDGE_MISSING', `${dep} does not record reverse edge from ${pkg.name}`, { dependency: dep }));
      }
    }

    for (const dependent of [...pkg.reverseDependencies].sort()) {
      const dependentPkg = this.registry.getPackageInfo(dependent);
      if (!dependentPkg) {
        violations.push(violation('REVERSE_DEPENDENCY_NOT_ADMITTED', `${pkg.name} names non-admitted reverse dependency ${dependent}`, { dependent }));
        continue;
      }
      if (!dependentPkg.dependencies.includes(pkg.name)) {
        violations.push(violation('FORWARD_EDGE_MISSING', `${dependent} does not record forward edge to ${pkg.name}`, { dependent }));
      }
    }
    return violations;
  }

  async validateAll() {
    if (!this.registry) await this.initialize();
    const packages = this.registry.getAllPackages();
    const results = [];
    for (const pkg of packages) results.push(await this.validatePackage(pkg.name));
    return {
      totalPackages: packages.length,
      validPackages: results.filter(result => result.valid).length,
      invalidPackages: results.filter(result => !result.valid).length,
      cyclicPackages: results.filter(result => result.topology?.cyclic).length,
      results,
    };
  }

  async validateTierStructure() {
    if (!this.registry) await this.initialize();
    const summary = this.registry.getTierSummary();
    return Object.fromEntries(Object.entries(summary).map(([tier, count]) => [tier, {
      count,
      semantics: tier === 'Internal'
        ? 'private/internal package projection'
        : 'graph-derived compatibility classification; not dependency authority',
      packages: this.registry.getPackagesByTier(tier),
    }]));
  }

  async validateGraphCorrespondence() {
    const all = await this.validateAll();
    const components = await this.resolver.getStronglyConnectedComponents();
    const projectedCycles = this.registry.getAllPackages()
      .filter(pkg => pkg.cyclic)
      .map(pkg => pkg.name)
      .sort();
    const resolvedCycles = components
      .filter(component => component.length > 1 || (this.registry.getPackageInfo(component[0])?.dependencies || []).includes(component[0]))
      .flat()
      .sort();
    const sccCorresponds = JSON.stringify(projectedCycles) === JSON.stringify(resolvedCycles);
    return {
      consistent: all.invalidPackages === 0 && sccCorresponds,
      packageCount: all.totalPackages,
      invalidPackages: all.invalidPackages,
      projectedCyclicPackages: projectedCycles,
      resolvedCyclicPackages: resolvedCycles,
      sccCorresponds,
    };
  }

  async enforceConsistency() {
    const all = await this.validateAll();
    const issues = all.results.filter(result => !result.valid);
    const graph = await this.validateGraphCorrespondence();
    if (!graph.sccCorresponds) {
      issues.push({
        package: '<graph>',
        valid: false,
        violations: [violation('SCC_GRAPH_DRIFT', 'Projected SCC membership differs from resolver SCC membership')],
        warnings: [],
      });
    }
    return { consistent: issues.length === 0, issueCount: issues.length, issues };
  }

  async generateValidationReport() {
    const all = await this.validateAll();
    const tierStructure = await this.validateTierStructure();
    const graphCorrespondence = await this.validateGraphCorrespondence();
    return {
      timestamp: new Date().toISOString(),
      summary: {
        totalPackages: all.totalPackages,
        validPackages: all.validPackages,
        invalidPackages: all.invalidPackages,
        cyclicPackages: all.cyclicPackages,
        consistencyScore: all.totalPackages === 0 ? 0 : (all.validPackages / all.totalPackages) * 100,
      },
      tierStructure,
      graphCorrespondence,
      validationResults: all.results,
    };
  }
}

export const validator = new PackageValidator();
export async function getValidator() { await validator.initialize(); return validator; }
export default validator;
