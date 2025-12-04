/**
 * @file Infrastructure - Project configuration and templates
 * @module @unrdf/project-engine/infrastructure
 */

import { z } from 'zod';

/**
 * Project config schema
 */
const ProjectConfigSchema = z.object({
  name: z.string(),
  type: z.enum(['library', 'application', 'monorepo']),
  runtime: z.enum(['node', 'browser', 'both']),
  testing: z.object({
    framework: z.enum(['vitest', 'jest', 'mocha']),
    coverage: z.number().min(0).max(100),
  }),
  linting: z.object({
    enabled: z.boolean(),
    rules: z.string(),
  }),
  formatting: z.object({
    enabled: z.boolean(),
    tool: z.enum(['prettier', 'eslint']),
  }),
});

/**
 * Create project configuration
 * @param {string} template - Template name (basic, library, monorepo)
 * @returns {Object} Project configuration object
 *
 * @throws {TypeError} If template is not a string
 * @throws {Error} If template is invalid
 *
 * @example
 * const config = createProjectConfig('library');
 * console.log('Config:', config);
 */
export function createProjectConfig(template) {
  if (typeof template !== 'string') {
    throw new TypeError('createProjectConfig: template must be a string');
  }

  const templates = {
    basic: {
      name: 'basic-project',
      type: 'library',
      runtime: 'node',
      testing: {
        framework: 'vitest',
        coverage: 80,
      },
      linting: {
        enabled: true,
        rules: 'recommended',
      },
      formatting: {
        enabled: true,
        tool: 'prettier',
      },
    },
    library: {
      name: 'my-library',
      type: 'library',
      runtime: 'both',
      testing: {
        framework: 'vitest',
        coverage: 90,
      },
      linting: {
        enabled: true,
        rules: 'strict',
      },
      formatting: {
        enabled: true,
        tool: 'prettier',
      },
    },
    monorepo: {
      name: 'my-monorepo',
      type: 'monorepo',
      runtime: 'both',
      testing: {
        framework: 'vitest',
        coverage: 80,
      },
      linting: {
        enabled: true,
        rules: 'recommended',
      },
      formatting: {
        enabled: true,
        tool: 'prettier',
      },
    },
  };

  const config = templates[template];

  if (!config) {
    throw new Error(`createProjectConfig: invalid template "${template}"`);
  }

  return ProjectConfigSchema.parse(config);
}

/**
 * Setup development environment configuration
 * @returns {Object} Development environment config
 *
 * @example
 * const devConfig = setupDevEnvironment();
 * console.log('Dev tools:', devConfig.tools);
 */
export function setupDevEnvironment() {
  return {
    tools: {
      packageManager: 'pnpm',
      nodeVersion: '>=18.0.0',
      editor: {
        vscode: {
          extensions: ['dbaeumer.vscode-eslint', 'esbenp.prettier-vscode', 'vitest.explorer'],
          settings: {
            'editor.formatOnSave': true,
            'editor.codeActionsOnSave': {
              'source.fixAll.eslint': true,
            },
          },
        },
      },
      git: {
        hooks: {
          'pre-commit': 'pnpm lint && pnpm test',
          'pre-push': 'pnpm build',
        },
      },
    },
    scripts: {
      dev: 'vitest --watch',
      build: 'node build.config.mjs',
      test: 'vitest run --coverage',
      lint: 'eslint src/ test/ --max-warnings=0',
      format: 'prettier --write src/ test/',
    },
  };
}

/**
 * Create deployment configuration
 * @param {string} environment - Environment name (development, staging, production)
 * @returns {Object} Deployment configuration
 *
 * @throws {TypeError} If environment is not a string
 * @throws {Error} If environment is invalid
 *
 * @example
 * const deployConfig = createDeploymentConfig('production');
 * console.log('Deploy to:', deployConfig.target);
 */
export function createDeploymentConfig(environment) {
  if (typeof environment !== 'string') {
    throw new TypeError('createDeploymentConfig: environment must be a string');
  }

  const configs = {
    development: {
      environment: 'development',
      target: 'local',
      build: {
        minify: false,
        sourceMaps: true,
      },
      env: {
        NODE_ENV: 'development',
        DEBUG: 'true',
      },
    },
    staging: {
      environment: 'staging',
      target: 'staging-server',
      build: {
        minify: true,
        sourceMaps: true,
      },
      env: {
        NODE_ENV: 'staging',
        DEBUG: 'false',
      },
    },
    production: {
      environment: 'production',
      target: 'production-server',
      build: {
        minify: true,
        sourceMaps: false,
      },
      env: {
        NODE_ENV: 'production',
        DEBUG: 'false',
      },
    },
  };

  const config = configs[environment];

  if (!config) {
    throw new Error(`createDeploymentConfig: invalid environment "${environment}"`);
  }

  return config;
}
