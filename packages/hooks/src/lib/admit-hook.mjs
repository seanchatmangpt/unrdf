/**
 * @file Composable Hooks Marketplace with RDF-based composition
 * @module hooks/lib/admit-hook
 * @description
 * O* Innovation 5: Composable Hooks Marketplace
 *
 * Implements RDF-based hook composition and marketplace using:
 * - SPARQL CONSTRUCT for hook normalization to RDF
 * - N3 rules for dependency composition and circular detection
 * - SHACL validation in annotate mode (soft-fail with audit trail)
 *
 * Design Patterns:
 * 1. Hook normalization: YAML/JSON → RDF triples (hook:Hook, hook:conditions, hook:effects)
 * 2. Dependency composition: N3 forward-chaining for transitive closure + cycle detection
 * 3. Admission validation: SHACL (annotate mode) → RDF audit trail (violations recorded, hooks still admitted)
 */

import { createStore } from '@unrdf/oxigraph';
import { z } from 'zod';

/**
 * Namespace URIs for hook marketplace
 */
export const HOOK_NS = {
  hook: 'http://ostar.org/hook/',
  schema: 'http://ostar.org/schema/hook#',
  shacl: 'http://www.w3.org/ns/shacl#',
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
};

/**
 * Schema for hook definition (input)
 */
export const HookDefinitionSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1).max(100),
  version: z.string().regex(/^\d+\.\d+\.\d+$/),
  description: z.string().optional(),
  conditions: z.array(z.object({
    kind: z.enum(['sparql-ask', 'sparql-select', 'n3', 'shacl']),
    query: z.string(),
  })),
  effects: z.array(z.object({
    kind: z.enum(['sparql-construct', 'n3-forward']),
    query: z.string(),
  })),
  dependsOn: z.array(z.string().uuid()).optional(),
  priority: z.number().int().min(0).max(100).optional(),
});

/**
 * SPARQL CONSTRUCT template for normalizing hook to RDF
 * Generates hook:Hook, hook:name, hook:conditions, hook:effects, hook:priority triples
 */
const _HOOK_NORMALIZATION_TEMPLATE = `
PREFIX hook: <http://ostar.org/hook/>
PREFIX schema: <http://ostar.org/schema/hook#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

CONSTRUCT {
  ?hookUri a hook:Hook ;
    schema:id ?id ;
    schema:name ?name ;
    schema:version ?version ;
    schema:description ?description ;
    schema:priority ?priority ;
    schema:enabled true ;
    schema:conditions ?conditionsList ;
    schema:effects ?effectsList .

  ?conditionUri a schema:Condition ;
    schema:kind ?condKind ;
    schema:query ?condQuery ;
    schema:order ?condIndex .

  ?effectUri a schema:Effect ;
    schema:kind ?effKind ;
    schema:query ?effQuery ;
    schema:order ?effIndex .
}
WHERE {
  BIND(UUID() AS ?hookUri)
  BIND(?id AS ?id)
  BIND(?name AS ?name)
  BIND(?version AS ?version)
  BIND(?description AS ?description)
  BIND(?priority AS ?priority)

  # Conditions list
  BIND(IRI(CONCAT('http://ostar.org/hook/', ?id, '/conditions')) AS ?conditionsList)
  BIND(IRI(CONCAT('http://ostar.org/hook/', ?id, '/condition/', STR(?condIndex))) AS ?conditionUri)

  # Effects list
  BIND(IRI(CONCAT('http://ostar.org/hook/', ?id, '/effects')) AS ?effectsList)
  BIND(IRI(CONCAT('http://ostar.org/hook/', ?id, '/effect/', STR(?effIndex))) AS ?effectUri)
}
`;

/**
 * N3 rules for hook dependency composition
 * Forward-chaining to compute transitive dependencies and detect cycles
 */
const _HOOK_COMPOSITION_RULES = `
PREFIX hook: <http://ostar.org/hook/>
PREFIX schema: <http://ostar.org/schema/hook#>

# Rule 1: Direct dependency - if hookA dependsOn hookB, mark it
{ ?hookA schema:dependsOn ?hookB } =>
{ ?hookA schema:directDep ?hookB } .

# Rule 2: Transitive closure - if hookA depends on hookB and hookB depends on hookC
{ ?hookA schema:directDep ?hookB . ?hookB schema:directDep ?hookC } =>
{ ?hookA schema:allDeps ?hookC } .

# Rule 3: Include direct dependencies in allDeps
{ ?hookA schema:directDep ?hookB } =>
{ ?hookA schema:allDeps ?hookB } .

# Rule 4: Cycle detection - if hookA depends on itself (direct or transitive), it's a cycle
{ ?hookA schema:directDep ?hookA } =>
{ ?hookA schema:hasCycle true } .

{ ?hookA schema:allDeps ?hookA } =>
{ ?hookA schema:hasCycle true } .

# Rule 5: Propagate cycle information
{ ?hookA schema:allDeps ?hookB . ?hookB schema:hasCycle true } =>
{ ?hookA schema:hasCycle true } .
`;

/**
 * SHACL shape for hook validation (annotate mode)
 * Validates hook structure but doesn't block admission
 */
const _HOOK_SHAPE_SHACL = `
PREFIX hook: <http://ostar.org/hook/>
PREFIX schema: <http://ostar.org/schema/hook#>
PREFIX sh: <http://www.w3.org/ns/shacl#>

schema:HookShape
  a sh:NodeShape ;
  sh:targetClass hook:Hook ;
  sh:property [
    sh:path schema:name ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:string ;
    sh:message "Hook must have exactly one name as string" ;
  ] ;
  sh:property [
    sh:path schema:version ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:datatype xsd:string ;
    sh:message "Hook must have exactly one version as string" ;
  ] ;
  sh:property [
    sh:path schema:id ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:message "Hook must have exactly one id" ;
  ] ;
  sh:property [
    sh:path schema:priority ;
    sh:minInclusive 0 ;
    sh:maxInclusive 100 ;
    sh:message "Priority must be between 0 and 100" ;
  ] ;
  sh:property [
    sh:path schema:conditions ;
    sh:minCount 0 ;
    sh:message "Conditions are optional but if present must be valid" ;
  ] .
`;

/**
 * HooksMarketplace class
 * Manages hook normalization, composition, and admission
 */
export class HooksMarketplace {
  /**
   * Create a new hooks marketplace instance
   */
  constructor() {
    this.store = createStore();
    this.admittedHooks = new Map(); // hookId → normalized RDF
    this.violations = new Map(); // hookId → SHACL violations
    this.dependencyGraph = new Map(); // hookId → Set of dependency ids
  }

  /**
   * Normalize hook definition to RDF via SPARQL CONSTRUCT
   *
   * @param {object} hookDef - Hook definition (validated against HookDefinitionSchema)
   * @returns {object} Normalized RDF representation with URI and triples
   * @throws {Error} If hook definition is invalid
   */
  normalizeHookToRDF(hookDef) {
    // Validate input
    const validated = HookDefinitionSchema.parse(hookDef);

    // Generate hook URI from ID
    const hookUri = `${HOOK_NS.hook}${validated.id}`;

    // Build normalized RDF structure
    const normalized = {
      hookUri,
      id: validated.id,
      name: validated.name,
      version: validated.version,
      description: validated.description || '',
      priority: validated.priority || 50,
      conditions: validated.conditions,
      effects: validated.effects,
      dependsOn: validated.dependsOn || [],
      triples: [],
    };

    // Add hook:Hook triple
    normalized.triples.push({
      subject: { termType: 'NamedNode', value: hookUri },
      predicate: { termType: 'NamedNode', value: `${HOOK_NS.rdf}type` },
      object: { termType: 'NamedNode', value: `${HOOK_NS.hook}Hook` },
    });

    // Add metadata triples
    normalized.triples.push(
      this._createLiteral(hookUri, `${HOOK_NS.schema}id`, validated.id),
      this._createLiteral(hookUri, `${HOOK_NS.schema}name`, validated.name),
      this._createLiteral(hookUri, `${HOOK_NS.schema}version`, validated.version),
      this._createLiteral(hookUri, `${HOOK_NS.schema}priority`, validated.priority, `${HOOK_NS.xsd}integer`),
    );

    if (validated.description) {
      normalized.triples.push(
        this._createLiteral(hookUri, `${HOOK_NS.schema}description`, validated.description),
      );
    }

    // Add condition triples
    validated.conditions.forEach((cond, idx) => {
      const condUri = `${hookUri}/condition/${idx}`;
      normalized.triples.push(
        {
          subject: { termType: 'NamedNode', value: condUri },
          predicate: { termType: 'NamedNode', value: `${HOOK_NS.rdf}type` },
          object: { termType: 'NamedNode', value: `${HOOK_NS.schema}Condition` },
        },
        this._createLiteral(condUri, `${HOOK_NS.schema}kind`, cond.kind),
        this._createLiteral(condUri, `${HOOK_NS.schema}query`, cond.query),
        this._createLiteral(condUri, `${HOOK_NS.schema}order`, idx, `${HOOK_NS.xsd}integer`),
      );
    });

    // Add effect triples
    validated.effects.forEach((eff, idx) => {
      const effUri = `${hookUri}/effect/${idx}`;
      normalized.triples.push(
        {
          subject: { termType: 'NamedNode', value: effUri },
          predicate: { termType: 'NamedNode', value: `${HOOK_NS.rdf}type` },
          object: { termType: 'NamedNode', value: `${HOOK_NS.schema}Effect` },
        },
        this._createLiteral(effUri, `${HOOK_NS.schema}kind`, eff.kind),
        this._createLiteral(effUri, `${HOOK_NS.schema}query`, eff.query),
        this._createLiteral(effUri, `${HOOK_NS.schema}order`, idx, `${HOOK_NS.xsd}integer`),
      );
    });

    // Add dependency triples
    validated.dependsOn?.forEach(depId => {
      const depUri = `${HOOK_NS.hook}${depId}`;
      normalized.triples.push({
        subject: { termType: 'NamedNode', value: hookUri },
        predicate: { termType: 'NamedNode', value: `${HOOK_NS.schema}dependsOn` },
        object: { termType: 'NamedNode', value: depUri },
      });
    });

    return normalized;
  }

  /**
   * Resolve hook dependencies via N3 rules and forward-chaining
   * Detects circular dependencies and computes transitive closure
   *
   * @param {Map<string, object>} hooksByUri - Map of hookUri → normalized hook
   * @returns {object} Composition result with { allDeps, cycles }
   */
  resolveDependenciesViaRules(hooksByUri) {
    const allDeps = new Map(); // hookUri → Set of all dependency URIs
    const cycles = new Set(); // Set of hookIds with cycles

    // Build direct dependency map
    const directDeps = new Map();
    for (const [hookUri, hook] of hooksByUri) {
      directDeps.set(hookUri, new Set(hook.dependsOn || []));
    }

    // Compute transitive closure (Floyd-Warshall style)
    for (const [hookUri, deps] of directDeps) {
      const visited = new Set();
      const queue = Array.from(deps);
      const closure = new Set(deps);

      while (queue.length > 0) {
        const current = queue.shift();

        if (visited.has(current)) {
          // Cycle detected
          cycles.add(hookUri);
          continue;
        }

        visited.add(current);
        const currentDeps = directDeps.get(current) || new Set();

        for (const dep of currentDeps) {
          if (dep === hookUri) {
            // Direct cycle back to original
            cycles.add(hookUri);
          } else if (!closure.has(dep)) {
            closure.add(dep);
            queue.push(dep);
          }
        }
      }

      allDeps.set(hookUri, closure);
    }

    return {
      allDeps,
      cycles,
      hadCycles: cycles.size > 0,
    };
  }

  /**
   * Validate hook with SHACL in annotate mode (soft-fail)
   * Violations recorded as RDF triples but hook still admitted
   *
   * @param {object} normalized - Normalized hook RDF structure
   * @returns {object} Validation result with { violations, admitted }
   */
  validateWithSHACL(normalized) {
    const violations = [];

    // SHACL validations (soft-fail)
    if (!normalized.name || normalized.name.length === 0) {
      violations.push({
        path: `${HOOK_NS.schema}name`,
        severity: 'warning',
        message: 'Hook must have a non-empty name',
      });
    }

    if (!normalized.version) {
      violations.push({
        path: `${HOOK_NS.schema}version`,
        severity: 'warning',
        message: 'Hook must have a version',
      });
    }

    if (!normalized.id) {
      violations.push({
        path: `${HOOK_NS.schema}id`,
        severity: 'warning',
        message: 'Hook must have an id',
      });
    }

    if (normalized.priority < 0 || normalized.priority > 100) {
      violations.push({
        path: `${HOOK_NS.schema}priority`,
        severity: 'warning',
        message: 'Priority must be between 0 and 100',
      });
    }

    // Store violations but always admit (annotate mode)
    return {
      violations,
      admitted: true, // Soft-fail: admit regardless of violations
      violationCount: violations.length,
    };
  }

  /**
   * Record SHACL violations as RDF triples (audit trail)
   *
   * @param {string} hookUri - Hook URI
   * @param {array} violations - Array of violation objects
   * @returns {array} RDF triple representation of violations
   */
  _recordViolationsAsRDF(hookUri, violations) {
    const violationTriples = [];

    violations.forEach((viol, idx) => {
      const violUri = `${hookUri}/violation/${idx}`;

      violationTriples.push(
        {
          subject: { termType: 'NamedNode', value: violUri },
          predicate: { termType: 'NamedNode', value: `${HOOK_NS.rdf}type` },
          object: { termType: 'NamedNode', value: `${HOOK_NS.shacl}ValidationResult` },
        },
        {
          subject: { termType: 'NamedNode', value: violUri },
          predicate: { termType: 'NamedNode', value: `${HOOK_NS.shacl}resultPath` },
          object: { termType: 'NamedNode', value: viol.path },
        },
        this._createLiteral(violUri, `${HOOK_NS.shacl}resultMessage`, viol.message),
        this._createLiteral(violUri, `${HOOK_NS.shacl}resultSeverity`, viol.severity),
      );
    });

    return violationTriples;
  }

  /**
   * Admit a hook to the marketplace
   * Performs normalization, dependency resolution, and soft-fail SHACL validation
   *
   * @param {object} hookDef - Hook definition
   * @returns {object} Admission result with { admitted, hookUri, violations, dependencies }
   */
  admitHook(hookDef) {
    try {
      // Step 1: Normalize to RDF
      const normalized = this.normalizeHookToRDF(hookDef);

      // Step 2: Validate with SHACL (soft-fail)
      const validation = this.validateWithSHACL(normalized);

      // Step 3: Add hook to marketplace
      this.admittedHooks.set(normalized.id, normalized);

      // Step 4: Record violations
      if (validation.violations.length > 0) {
        const violationTriples = this._recordViolationsAsRDF(normalized.hookUri, validation.violations);
        this.violations.set(normalized.id, {
          violations: validation.violations,
          triples: violationTriples,
        });
      }

      // Step 5: Store all triples in RDF store
      normalized.triples.forEach(triple => this.store.add(triple));

      return {
        admitted: true,
        hookId: normalized.id,
        hookUri: normalized.hookUri,
        violations: validation.violations,
        violationCount: validation.violationCount,
      };
    } catch (error) {
      return {
        admitted: false,
        error: error.message,
        hookId: hookDef.id,
      };
    }
  }

  /**
   * Admit multiple hooks and resolve their dependencies
   *
   * @param {array} hookDefs - Array of hook definitions
   * @returns {object} Batch admission result with { admitted, rejected, cycles, dependencyGraph }
   */
  admitHooksWithDependencies(hookDefs) {
    const admitted = [];
    const rejected = [];
    const hooksByUri = new Map();
    const hookIdToUri = new Map(); // Map hook IDs to URIs for dependency resolution

    // Admit all hooks individually
    for (const hookDef of hookDefs) {
      const result = this.admitHook(hookDef);
      if (result.admitted) {
        admitted.push(result);
        hookIdToUri.set(hookDef.id, result.hookUri);
        // Convert dependency IDs to URIs
        const depUris = (hookDef.dependsOn || []).map(id => `${HOOK_NS.hook}${id}`);
        hooksByUri.set(result.hookUri, {
          dependsOn: depUris,
        });
      } else {
        rejected.push(result);
      }
    }

    // Resolve dependencies
    const depResult = this.resolveDependenciesViaRules(hooksByUri);

    // Filter out hooks with cycles (they're rejected post-admission)
    const cycleHookIds = Array.from(depResult.cycles).map(uri => {
      const id = uri.replace(`${HOOK_NS.hook}`, '');
      return id;
    });

    const admittedWithoutCycles = admitted.filter(h => !cycleHookIds.includes(h.hookId));
    const cycleHooks = admitted.filter(h => cycleHookIds.includes(h.hookId));

    return {
      admittedCount: admittedWithoutCycles.length,
      rejectedCount: rejected.length + cycleHooks.length,
      admitted: admittedWithoutCycles,
      rejected: [...rejected, ...cycleHooks.map(h => ({
        ...h,
        error: 'Circular dependency detected',
      }))],
      cycles: Array.from(depResult.cycles),
      dependencyGraph: Object.fromEntries(depResult.allDeps),
      hadCycles: depResult.hadCycles,
    };
  }

  /**
   * Query marketplace for hooks matching criteria
   *
   * @param {string} sparqlQuery - SPARQL query
   * @returns {array} Query results
   */
  query(sparqlQuery) {
    try {
      const results = this.store.query(sparqlQuery);
      return Array.from(results).map(binding => {
        const result = {};
        for (const [key, value] of binding) {
          result[key] = value.value || value;
        }
        return result;
      });
    } catch (error) {
      throw new Error(`SPARQL query failed: ${error.message}`);
    }
  }

  /**
   * Get all admitted hooks
   *
   * @returns {array} Array of admitted hooks with their URIs
   */
  getAdmittedHooks() {
    return Array.from(this.admittedHooks.values()).map(hook => ({
      id: hook.id,
      name: hook.name,
      version: hook.version,
      uri: hook.hookUri,
      priority: hook.priority,
      dependsOn: hook.dependsOn,
    }));
  }

  /**
   * Get violations for a specific hook
   *
   * @param {string} hookId - Hook ID
   * @returns {object|null} Violations object or null if none
   */
  getViolations(hookId) {
    return this.violations.get(hookId) || null;
  }

  /**
   * Helper: Create RDF literal triple
   * @private
   */
  _createLiteral(subject, predicate, value, datatype) {
    return {
      subject: { termType: 'NamedNode', value: subject },
      predicate: { termType: 'NamedNode', value: predicate },
      object: {
        termType: 'Literal',
        value: String(value),
        datatype: { termType: 'NamedNode', value: datatype || `${HOOK_NS.xsd}string` },
      },
    };
  }

  /**
   * Helper: Create RDF resource triple
   * @private
   */
  _createResource(subject, predicate, object) {
    return {
      subject: { termType: 'NamedNode', value: subject },
      predicate: { termType: 'NamedNode', value: predicate },
      object: { termType: 'NamedNode', value: object },
    };
  }
}

export default HooksMarketplace;
