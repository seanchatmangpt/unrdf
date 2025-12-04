/**
 * @file Implementation Reference - Machine-Executable
 * @module agents/reference/implementation
 * 
 * @description
 * Machine-executable implementation reference for agents.
 * Provides functions to discover, navigate, and reference actual codebase implementations.
 * 
 * @targetAudience AutonomicAgent, HyperintelligentSystem
 * @format machine-executable
 */

import { readFile, readdir, stat } from 'fs/promises';
import { join, relative, dirname } from 'path';
import { fileURLToPath } from 'url';
import { createHash } from 'crypto';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const PROJECT_ROOT = join(__dirname, '../../..');

/**
 * Implementation reference registry
 * Maps documentation concepts to actual codebase files
 */
export const IMPLEMENTATION_REGISTRY = {
  mapek: {
    core: {
      file: 'src/project-engine/autonomic-mapek.mjs',
      functions: {
        runMapekIteration: { lineStart: 47, lineEnd: 280 },
        createAutonomicHooks: { lineStart: 289, lineEnd: 395 },
        runContinuousMapekLoop: { lineStart: 407, lineEnd: 461 },
        reportMapekStatus: { lineStart: 469, lineEnd: 507 }
      },
      phases: {
        monitor: { lineStart: 76, lineEnd: 99 },
        analyze: { lineStart: 101, lineEnd: 136 },
        plan: { lineStart: 138, lineEnd: 198 },
        execute: { lineStart: 200, lineEnd: 234 },
        knowledge: { lineStart: 236, lineEnd: 264 }
      }
    },
    supporting: {
      gapFinder: {
        file: 'src/project-engine/gap-finder.mjs',
        exports: ['findMissingRoles', 'scoreMissingRole']
      },
      typeAuditor: {
        file: 'src/project-engine/type-auditor.mjs',
        exports: ['auditTypeConsistency', 'compareTypes']
      },
      hotspotAnalyzer: {
        file: 'src/project-engine/hotspot-analyzer.mjs',
        exports: ['analyzeHotspots', 'scoreFeature']
      },
      driftSnapshot: {
        file: 'src/project-engine/drift-snapshot.mjs',
        exports: ['computeDrift', 'createStructureSnapshot']
      }
    },
    entryPoint: {
      file: 'src/project-engine/index.mjs',
      exports: [
        'runMapekIteration',
        'createAutonomicHooks',
        'runContinuousMapekLoop',
        'reportMapekStatus'
      ]
    }
  },
  knowledgeHooks: {
    definition: {
      file: 'src/knowledge-engine/define-hook.mjs',
      functions: {
        defineHook: { lineStart: 126, lineEnd: 127 }
      }
    },
    management: {
      file: 'src/knowledge-engine/knowledge-hook-manager.mjs',
      class: 'KnowledgeHookManager',
      methods: {
        registerHook: { approximateLine: 100 },
        removeHook: { approximateLine: 150 },
        executeKnowledgeHook: { lineStart: 171, lineEnd: 187 },
        executeAllHooks: { approximateLine: 190 }
      }
    },
    execution: {
      file: 'src/knowledge-engine/hook-executor.mjs',
      functions: {
        executeHook: { lineStart: 28, lineEnd: 84 },
        _executeHookPhases: { lineStart: 156, lineEnd: 373 }
      },
      lifecycle: {
        before: { lineStart: 240, lineEnd: 270 },
        run: { lineStart: 272, lineEnd: 300 },
        after: { lineStart: 302, lineEnd: 335 }
      }
    },
    schemas: {
      file: 'src/knowledge-engine/schemas.mjs',
      schemas: [
        'HookMetaSchema',
        'FileRefSchema',
        'ConditionSchema',
        'HookEventSchema',
        'HookResultSchema'
      ]
    },
    entryPoint: {
      file: 'src/knowledge-engine/index.mjs',
      exports: [
        'KnowledgeHookManager',
        'TransactionManager',
        'defineHook',
        'createDarkMatterCore'
      ]
    }
  },
  systemCore: {
    darkMatter: {
      file: 'src/knowledge-engine/dark-matter-core.mjs',
      class: 'KnowledgeSubstrateCore',
      methods: ['query', 'executeTransaction', 'executeHook', 'registerHook', 'getComponent']
    },
    transaction: {
      file: 'src/knowledge-engine/transaction.mjs',
      class: 'TransactionManager',
      states: ['PENDING', 'ACTIVE', 'HOOKS_PASSED', 'APPLYING', 'POST_HOOKS', 'COMMITTED', 'ROLLING_BACK', 'ROLLED_BACK']
    }
  },
  projectInitialization: {
    pipeline: {
      file: 'src/project-engine/initialize.mjs',
      function: 'createProjectInitializationPipeline',
      phases: {
        scan: { approximateLine: 100 },
        stackDetection: { approximateLine: 150 },
        projectModel: { approximateLine: 200 },
        fileRoles: { approximateLine: 250 },
        domainInference: { approximateLine: 300 },
        codeComplexity: { lineStart: 569, lineEnd: 615 },
        templateInference: { approximateLine: 350 },
        hooks: { approximateLine: 400 },
        snapshot: { approximateLine: 450 }
      }
    },
    codeComplexity: {
      file: 'src/project-engine/code-complexity-js.mjs',
      function: 'analyzeJsComplexity'
    },
    capabilities: {
      file: 'src/project-engine/capabilities-manifest.mjs',
      exports: [
        'CODE_COMPLEXITY_JS',
        'CAPABILITIES',
        'FEATURE_FLAGS',
        'isCapabilityEnabled',
        'getCapabilityMetadata',
        'getEnabledCapabilities',
        'setCapabilityEnabled'
      ]
    }
  },
  entryPoints: {
    main: {
      file: 'src/index.mjs',
      export: 'unrdf'
    },
    knowledgeEngine: {
      file: 'src/knowledge-engine/index.mjs',
      export: 'unrdf/knowledge-engine'
    },
    projectEngine: {
      file: 'src/project-engine/index.mjs',
      export: 'unrdf/project-engine'
    },
    cli: {
      file: 'src/cli/index.mjs',
      export: 'unrdf/cli'
    }
  },
  tests: {
    mapek: {
      file: 'test/project-engine/mapek-consolidated.test.mjs',
      description: 'MAPEK integration tests'
    },
    hooks: {
      definition: 'test/knowledge-engine/define-hook.test.mjs',
      manager: 'test/knowledge-engine/knowledge-hook-manager.test.mjs',
      executor: 'test/knowledge-engine/hook-executor.test.mjs'
    },
    initialization: {
      pipeline: 'test/project-engine/initialize.test.mjs',
      complexity: 'test/project-engine/initialize-with-complexity.test.mjs',
      capabilities: 'test/project-engine/capabilities-manifest.test.mjs'
    }
  },
  ontologies: {
    unmetric: {
      mjs: 'src/ontologies/unmetric-ontology.mjs',
      ttl: 'src/ontologies/unmetric-ontology.ttl',
      namespace: 'http://example.org/unrdf/metrics#'
    }
  }
};

/**
 * Get implementation details for a concept
 * 
 * @param {string} concept - Concept name (e.g., 'mapek', 'knowledgeHooks')
 * @param {string} [subconcept] - Sub-concept (e.g., 'core', 'definition')
 * @returns {Object|null} Implementation details or null if not found
 */
export function getImplementation(concept, subconcept = null) {
  const registry = IMPLEMENTATION_REGISTRY[concept];
  if (!registry) {
    return null;
  }
  
  if (subconcept && registry[subconcept]) {
    return {
      concept,
      subconcept,
      ...registry[subconcept]
    };
  }
  
  return {
    concept,
    ...registry
  };
}

/**
 * Get file path for an implementation
 * 
 * @param {string} concept - Concept name
 * @param {string} [subconcept] - Sub-concept
 * @returns {string|null} Absolute file path or null
 */
export function getImplementationPath(concept, subconcept = null) {
  const impl = getImplementation(concept, subconcept);
  if (!impl || !impl.file) {
    return null;
  }
  
  return join(PROJECT_ROOT, impl.file);
}

/**
 * Read implementation file content
 * 
 * @param {string} concept - Concept name
 * @param {string} [subconcept] - Sub-concept
 * @param {Object} [options] - Options
 * @param {number} [options.lineStart] - Start line (1-based)
 * @param {number} [options.lineEnd] - End line (1-based)
 * @returns {Promise<string>} File content or selected lines
 */
export async function readImplementation(concept, subconcept = null, options = {}) {
  const path = getImplementationPath(concept, subconcept);
  if (!path) {
    throw new Error(`Implementation not found: ${concept}${subconcept ? '.' + subconcept : ''}`);
  }
  
  const content = await readFile(path, 'utf-8');
  const lines = content.split('\n');
  
  if (options.lineStart || options.lineEnd) {
    const start = (options.lineStart || 1) - 1;
    const end = options.lineEnd || lines.length;
    return lines.slice(start, end).join('\n');
  }
  
  return content;
}

/**
 * Get function implementation details
 * 
 * @param {string} concept - Concept name
 * @param {string} subconcept - Sub-concept
 * @param {string} functionName - Function name
 * @returns {Promise<Object>} Function details with code
 */
export async function getFunctionImplementation(concept, subconcept, functionName) {
  const impl = getImplementation(concept, subconcept);
  if (!impl || !impl.functions || !impl.functions[functionName]) {
    throw new Error(`Function not found: ${concept}.${subconcept}.${functionName}`);
  }
  
  const funcInfo = impl.functions[functionName];
  const code = await readImplementation(concept, subconcept, {
    lineStart: funcInfo.lineStart,
    lineEnd: funcInfo.lineEnd
  });
  
  return {
    concept,
    subconcept,
    function: functionName,
    file: impl.file,
    lineStart: funcInfo.lineStart,
    lineEnd: funcInfo.lineEnd,
    code
  };
}

/**
 * List all available implementations
 * 
 * @returns {Object} Complete registry structure
 */
export function listImplementations() {
  return IMPLEMENTATION_REGISTRY;
}

/**
 * Search implementations by keyword
 * 
 * @param {string} keyword - Search keyword
 * @returns {Array} Matching implementations
 */
export function searchImplementations(keyword) {
  const results = [];
  const lowerKeyword = keyword.toLowerCase();
  
  function searchRecursive(obj, path = []) {
    for (const [key, value] of Object.entries(obj)) {
      const currentPath = [...path, key];
      
      if (typeof value === 'string') {
        if (value.toLowerCase().includes(lowerKeyword) || key.toLowerCase().includes(lowerKeyword)) {
          results.push({
            path: currentPath.join('.'),
            value
          });
        }
      } else if (typeof value === 'object' && value !== null) {
        if (key.toLowerCase().includes(lowerKeyword)) {
          results.push({
            path: currentPath.join('.'),
            value
          });
        }
        searchRecursive(value, currentPath);
      }
    }
  }
  
  searchRecursive(IMPLEMENTATION_REGISTRY);
  return results;
}

/**
 * Verify implementation file exists
 * 
 * @param {string} concept - Concept name
 * @param {string} [subconcept] - Sub-concept
 * @returns {Promise<boolean>} True if file exists
 */
export async function verifyImplementation(concept, subconcept = null) {
  const path = getImplementationPath(concept, subconcept);
  if (!path) {
    return false;
  }
  
  try {
    const stats = await stat(path);
    return stats.isFile();
  } catch {
    return false;
  }
}

/**
 * Get implementation metadata
 * 
 * @param {string} concept - Concept name
 * @param {string} [subconcept] - Sub-concept
 * @returns {Promise<Object>} Metadata including file stats
 */
export async function getImplementationMetadata(concept, subconcept = null) {
  const impl = getImplementation(concept, subconcept);
  if (!impl) {
    return null;
  }
  
  const path = getImplementationPath(concept, subconcept);
  if (!path) {
    return { ...impl, exists: false };
  }
  
  try {
    const stats = await stat(path);
    const content = await readFile(path, 'utf-8');
    const hash = createHash('sha256').update(content).digest('hex');
    
    return {
      ...impl,
      path,
      exists: true,
      size: stats.size,
      modified: stats.mtime.toISOString(),
      sha256: hash,
      lineCount: content.split('\n').length
    };
  } catch (error) {
    return {
      ...impl,
      path,
      exists: false,
      error: error.message
    };
  }
}

/**
 * Get all entry points
 * 
 * @returns {Object} Entry point mappings
 */
export function getEntryPoints() {
  return IMPLEMENTATION_REGISTRY.entryPoints;
}

/**
 * Get test files for a concept
 * 
 * @param {string} concept - Concept name
 * @returns {Array} Test file paths
 */
export function getTestFiles(concept) {
  const tests = IMPLEMENTATION_REGISTRY.tests[concept];
  if (!tests) {
    return [];
  }
  
  if (typeof tests === 'string') {
    return [join(PROJECT_ROOT, tests)];
  }
  
  return Object.values(tests).map(test => join(PROJECT_ROOT, test));
}

/**
 * Complete implementation discovery
 * 
 * @param {string} [concept] - Optional concept to focus on
 * @returns {Promise<Object>} Complete discovery results
 */
export async function discoverImplementations(concept = null) {
  const results = {
    timestamp: new Date().toISOString(),
    concepts: {},
    entryPoints: getEntryPoints(),
    summary: {
      totalConcepts: 0,
      verified: 0,
      missing: 0
    }
  };
  
  const conceptsToCheck = concept 
    ? [concept] 
    : Object.keys(IMPLEMENTATION_REGISTRY).filter(k => k !== 'entryPoints' && k !== 'tests' && k !== 'ontologies');
  
  for (const conceptName of conceptsToCheck) {
    const metadata = await getImplementationMetadata(conceptName);
    results.concepts[conceptName] = metadata;
    
    if (metadata?.exists) {
      results.summary.verified++;
    } else {
      results.summary.missing++;
    }
    results.summary.totalConcepts++;
  }
  
  return results;
}

// Export default for machine execution
export default {
  IMPLEMENTATION_REGISTRY,
  getImplementation,
  getImplementationPath,
  readImplementation,
  getFunctionImplementation,
  listImplementations,
  searchImplementations,
  verifyImplementation,
  getImplementationMetadata,
  getEntryPoints,
  getTestFiles,
  discoverImplementations
};







