/**
 * @file CLI Noun-Verb Router
 * @module cli-v2/core/router
 *
 * @description
 * Implements kubectl-style noun-verb routing for CLI commands.
 */

/**
 * Create a router for noun-verb command dispatch
 * @param {Object} config - Router configuration
 * @returns {Object} Router instance
 */
export function createRouter(config = {}) {
  const routes = new Map();
  const aliases = new Map();

  return {
    /**
     * Register a noun with its verb handlers
     * @param {string} noun - Resource noun (e.g., 'graph', 'hook')
     * @param {Object} verbs - Map of verb handlers
     */
    register(noun, verbs) {
      routes.set(noun, verbs);
    },

    /**
     * Register an alias for a noun
     * @param {string} alias - Alias name (e.g., 'g' for 'graph')
     * @param {string} noun - Target noun
     */
    alias(alias, noun) {
      aliases.set(alias, noun);
    },

    /**
     * Route a command to the appropriate handler
     * @param {string} noun - Resource noun
     * @param {string} verb - Action verb
     * @returns {Function|null} Command handler
     */
    route(noun, verb) {
      // Resolve alias
      const resolvedNoun = aliases.get(noun) || noun;

      // Get verb handlers for noun
      const verbs = routes.get(resolvedNoun);
      if (!verbs) {
        return null;
      }

      // Get verb handler
      return verbs[verb] || null;
    },

    /**
     * Get all registered nouns
     * @returns {Array<string>} List of nouns
     */
    getNouns() {
      return Array.from(routes.keys());
    },

    /**
     * Get all verbs for a noun
     * @param {string} noun - Resource noun
     * @returns {Array<string>} List of verbs
     */
    getVerbs(noun) {
      const resolvedNoun = aliases.get(noun) || noun;
      const verbs = routes.get(resolvedNoun);
      return verbs ? Object.keys(verbs) : [];
    }
  };
}

/**
 * Common resource verbs (CRUD + custom)
 */
export const CommonVerbs = {
  LIST: 'list',
  GET: 'get',
  CREATE: 'create',
  UPDATE: 'update',
  DELETE: 'delete',
  APPLY: 'apply',
  VALIDATE: 'validate',
  DESCRIBE: 'describe',
  EXPORT: 'export',
  IMPORT: 'import'
};

/**
 * Verb aliases
 */
export const VerbAliases = {
  ls: 'list',
  rm: 'delete',
  del: 'delete'
};
