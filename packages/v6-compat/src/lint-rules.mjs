/**
 * ESLint Rules for v5 → v6 Migration
 *
 * Detects deprecated patterns and suggests v6 replacements.
 *
 * @module @unrdf/v6-compat/lint-rules
 */

/**
 * ESLint rule: no-n3-imports
 *
 * Prevents direct imports from 'n3' package.
 * Use centralized adapter in @unrdf/core/rdf/n3-justified-only
 *
 * @type {import('eslint').Rule.RuleModule}
 */
export const noN3Imports = {
  meta: {
    type: 'problem',
    docs: {
      description: 'Disallow direct imports from "n3" package',
      category: 'Best Practices',
      recommended: true
    },
    fixable: 'code',
    schema: [],
    messages: {
      noN3Import: 'Direct imports from "n3" are forbidden. Use @unrdf/core/rdf/n3-justified-only instead.',
      useOxigraph: 'Use createStore() from @unrdf/oxigraph instead of new Store() from n3.'
    }
  },

  create(context) {
    return {
      ImportDeclaration(node) {
        if (node.source.value === 'n3') {
          context.report({
            node,
            messageId: 'noN3Import',
            fix(fixer) {
              // Suggest replacement (won't auto-fix, requires manual review)
              return null;
            }
          });
        }
      },

      NewExpression(node) {
        if (
          node.callee.type === 'Identifier' &&
          node.callee.name === 'Store'
        ) {
          context.report({
            node,
            messageId: 'useOxigraph',
            fix(fixer) {
              return fixer.replaceText(node, 'await createStore()');
            }
          });
        }
      }
    };
  }
};

/**
 * ESLint rule: no-workflow-run
 *
 * Prevents workflow.run(), requires workflow.execute() with receipts.
 *
 * @type {import('eslint').Rule.RuleModule}
 */
export const noWorkflowRun = {
  meta: {
    type: 'suggestion',
    docs: {
      description: 'Disallow workflow.run(), use workflow.execute() with receipts',
      category: 'Best Practices',
      recommended: true
    },
    fixable: 'code',
    schema: [],
    messages: {
      useExecute: 'Use workflow.execute() instead of workflow.run() to generate receipts.'
    }
  },

  create(context) {
    return {
      CallExpression(node) {
        if (
          node.callee.type === 'MemberExpression' &&
          node.callee.property.name === 'run' &&
          node.callee.object.name?.includes('workflow')
        ) {
          context.report({
            node,
            messageId: 'useExecute',
            fix(fixer) {
              return fixer.replaceText(node.callee.property, 'execute');
            }
          });
        }
      }
    };
  }
};

/**
 * ESLint rule: require-zod-validation
 *
 * Requires Zod schema validation on function parameters.
 *
 * @type {import('eslint').Rule.RuleModule}
 */
export const requireZodValidation = {
  meta: {
    type: 'suggestion',
    docs: {
      description: 'Require Zod schema validation on exported functions',
      category: 'Best Practices',
      recommended: false
    },
    schema: [],
    messages: {
      missingZod: 'Exported function "{{name}}" should validate parameters with Zod schema.'
    }
  },

  create(context) {
    return {
      ExportNamedDeclaration(node) {
        if (
          node.declaration?.type === 'FunctionDeclaration' &&
          node.declaration.params.length > 0
        ) {
          const fnName = node.declaration.id.name;
          const hasZodValidation = context
            .getSourceCode()
            .getText(node.declaration.body)
            .includes('.parse(');

          if (!hasZodValidation) {
            context.report({
              node,
              messageId: 'missingZod',
              data: { name: fnName }
            });
          }
        }
      }
    };
  }
};

/**
 * ESLint rule: require-timeout
 *
 * Requires timeout guards on async operations.
 *
 * @type {import('eslint').Rule.RuleModule}
 */
export const requireTimeout = {
  meta: {
    type: 'suggestion',
    docs: {
      description: 'Require timeout guards on async I/O operations',
      category: 'Best Practices',
      recommended: true
    },
    schema: [],
    messages: {
      missingTimeout: 'Async operation should have timeout guard (default 5s).'
    }
  },

  create(context) {
    const ioFunctions = ['fetch', 'query', 'execute', 'connect'];

    return {
      AwaitExpression(node) {
        if (
          node.argument.type === 'CallExpression' &&
          node.argument.callee.type === 'MemberExpression'
        ) {
          const methodName = node.argument.callee.property.name;
          if (ioFunctions.includes(methodName)) {
            const sourceCode = context.getSourceCode();
            const parentText = sourceCode.getText(node.parent);

            // Check if Promise.race or timeout wrapper exists
            if (
              !parentText.includes('Promise.race') &&
              !parentText.includes('timeout')
            ) {
              context.report({
                node,
                messageId: 'missingTimeout'
              });
            }
          }
        }
      }
    };
  }
};

/**
 * ESLint rule: no-date-now
 *
 * Prevents Date.now() and Math.random() in business logic.
 *
 * @type {import('eslint').Rule.RuleModule}
 */
export const noDateNow = {
  meta: {
    type: 'problem',
    docs: {
      description: 'Disallow Date.now() and Math.random() in business logic (breaks determinism)',
      category: 'Best Practices',
      recommended: true
    },
    schema: [],
    messages: {
      noDeterministic: 'Use injected timestamp/random source for deterministic execution.'
    }
  },

  create(context) {
    return {
      CallExpression(node) {
        if (
          (node.callee.type === 'MemberExpression' &&
            node.callee.object.name === 'Date' &&
            node.callee.property.name === 'now') ||
          (node.callee.object?.name === 'Math' &&
            node.callee.property?.name === 'random')
        ) {
          context.report({
            node,
            messageId: 'noDeterministic'
          });
        }
      }
    };
  }
};

/**
 * ESLint plugin export
 *
 * Usage in eslint.config.mjs:
 *
 * import { plugin as unrdfV6 } from '@unrdf/v6-compat/lint-rules';
 *
 * export default [
 *   {
 *     plugins: { 'unrdf-v6': unrdfV6 },
 *     rules: {
 *       'unrdf-v6/no-n3-imports': 'error',
 *       'unrdf-v6/no-workflow-run': 'warn',
 *       'unrdf-v6/require-zod-validation': 'warn',
 *       'unrdf-v6/require-timeout': 'error',
 *       'unrdf-v6/no-date-now': 'error'
 *     }
 *   }
 * ];
 */
export const plugin = {
  meta: {
    name: '@unrdf/v6-compat',
    version: '6.0.0-alpha.1'
  },
  rules: {
    'no-n3-imports': noN3Imports,
    'no-workflow-run': noWorkflowRun,
    'require-zod-validation': requireZodValidation,
    'require-timeout': requireTimeout,
    'no-date-now': noDateNow
  }
};

export default plugin;
