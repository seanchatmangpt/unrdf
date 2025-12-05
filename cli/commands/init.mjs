#!/usr/bin/env node

/**
 * @file UNRDF Init Command - Interactive Project Scaffolding
 * @module cli-v2/commands/init
 *
 * @description
 * Interactive command for initializing new UNRDF projects from templates.
 * Supports starter, governance, and analytics project types with
 * customization options.
 */

import { defineCommand } from 'citty';
import { readFile, writeFile, mkdir, cp, readdir } from 'node:fs/promises';
import { resolve, join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createInterface } from 'node:readline/promises';
import { stdin as input, stdout as output } from 'node:process';
import { executeInitTransaction } from '../utils/transactional-init.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const TEMPLATES_DIR = resolve(__dirname, '../../../templates/projects');

/**
 * Prompt user for input with default value
 * @param {Object} readline - Readline interface
 * @param {string} question - Question to ask
 * @param {string} defaultValue - Default value if user presses enter
 * @returns {Promise<string>} User's answer
 */
async function prompt(readline, question, defaultValue = '') {
  const answer = await readline.question(
    defaultValue ? `${question} (${defaultValue}): ` : `${question}: `
  );
  return answer.trim() || defaultValue;
}

/**
 * Prompt user to select from multiple choices
 * @param {Object} readline - Readline interface
 * @param {string} question - Question to ask
 * @param {string[]} choices - Array of choices
 * @param {string} defaultChoice - Default choice
 * @returns {Promise<string>} User's selection
 */
async function select(readline, question, choices, defaultChoice) {
  console.log(`\n${question}`);
  choices.forEach((choice, index) => {
    const marker = choice === defaultChoice ? '❯' : ' ';
    console.log(`${marker} ${index + 1}. ${choice}`);
  });

  const answer = await readline.question(`Select (1-${choices.length}): `);
  const index = parseInt(answer.trim()) - 1;

  if (index >= 0 && index < choices.length) {
    return choices[index];
  }

  return defaultChoice;
}

/**
 * Copy template directory to destination
 * @param {string} templateName - Name of template
 * @param {string} destination - Destination directory
 */
async function copyTemplate(templateName, destination) {
  const templatePath = join(TEMPLATES_DIR, templateName);

  try {
    // Check if template exists
    await readdir(templatePath);

    // Create destination directory
    await mkdir(destination, { recursive: true });

    // Copy template files recursively
    await cp(templatePath, destination, { recursive: true });

    console.log(`✅ Copied ${templateName} template to ${destination}`);
  } catch (error) {
    if (error.code === 'ENOENT') {
      throw new Error(`Template "${templateName}" not found at ${templatePath}`);
    }
    throw error;
  }
}

/**
 * Update package.json with project name
 * @param {string} projectPath - Path to project
 * @param {string} projectName - Name of project
 */
async function updatePackageJson(projectPath, projectName) {
  const packageJsonPath = join(projectPath, 'package.json');

  try {
    const content = await readFile(packageJsonPath, 'utf-8');
    const packageJson = JSON.parse(content);

    // Update name
    packageJson.name = projectName;

    await writeFile(packageJsonPath, JSON.stringify(packageJson, null, 2));
    console.log(`✅ Updated package.json with project name: ${projectName}`);
  } catch (error) {
    console.warn(`⚠️  Could not update package.json: ${error.message}`);
  }
}

/**
 * Update unrdf.config.mjs with base IRI
 * @param {string} projectPath - Path to project
 * @param {string} baseIRI - Base IRI for project
 */
async function updateConfig(projectPath, baseIRI) {
  const configPath = join(projectPath, 'unrdf.config.mjs');

  try {
    let content = await readFile(configPath, 'utf-8');

    // Replace base IRI
    content = content.replace(
      /baseIRI:\s*['"]http:\/\/example\.org\//,
      `baseIRI: '${baseIRI}'`
    );

    await writeFile(configPath, content);
    console.log(`✅ Updated configuration with base IRI: ${baseIRI}`);
  } catch (error) {
    console.warn(`⚠️  Could not update config: ${error.message}`);
  }
}

/**
 * Initialize git repository
 * @param {string} projectPath - Path to project
 * @param {boolean} initGit - Whether to initialize git
 */
async function initializeGit(projectPath, initGit) {
  if (!initGit) return;

  try {
    const { execSync } = await import('node:child_process');

    // Initialize git repo
    execSync('git init', { cwd: projectPath, stdio: 'ignore' });

    // Create .gitignore
    const gitignorePath = join(projectPath, '.gitignore');
    const gitignoreContent = `# Dependencies
node_modules/
pnpm-lock.yaml
package-lock.json

# UNRDF storage
.unrdf/

# Environment
.env
.env.local

# Logs
*.log
npm-debug.log*

# OS
.DS_Store
Thumbs.db

# IDE
.vscode/
.idea/
*.swp
*.swo

# Test coverage
coverage/

# Build output
dist/
build/
`;

    await writeFile(gitignorePath, gitignoreContent);

    console.log('✅ Initialized git repository');
  } catch (error) {
    console.warn(`⚠️  Could not initialize git: ${error.message}`);
  }
}

/**
 * Display next steps for user
 * @param {string} projectPath - Path to project
 * @param {string} projectName - Name of project
 * @param {string} templateType - Type of template used
 */
function displayNextSteps(projectPath, projectName, templateType) {
  console.log('\n🎉 Project created successfully!\n');
  console.log('📁 Project structure:');
  console.log(`   ${projectPath}/`);
  console.log(`   ├── src/              # Source code`);
  console.log(`   ├── data/             # RDF data files`);
  console.log(`   ├── test/             # Test files`);
  if (templateType === 'governance') {
    console.log(`   ├── policy-packs/     # Policy pack definitions`);
  }
  if (templateType === 'analytics') {
    console.log(`   ├── queries/          # SPARQL analytics queries`);
  }
  console.log(`   └── unrdf.config.mjs  # Configuration\n`);

  console.log('📚 Next steps:\n');
  console.log(`   cd ${projectName}`);
  console.log(`   npm install`);
  console.log(`   npm run dev\n`);

  console.log('🚀 Quick commands:');
  console.log(`   npm run dev           # Run the project`);
  console.log(`   npm test              # Run tests`);
  console.log(`   npm run lint          # Lint code`);

  if (templateType === 'governance') {
    console.log(`   npm run audit         # Run governance audit`);
    console.log(`   npm run compliance:report  # Generate compliance report`);
  }

  if (templateType === 'analytics') {
    console.log(`   npm run analyze       # Run analytics`);
    console.log(`   npm run metrics       # Collect metrics`);
    console.log(`   npm run dashboard     # View analytics dashboard`);
  }

  console.log('\n📖 Documentation:');
  console.log(`   README.md             # Project overview`);
  console.log(`   https://github.com/unrdf/unrdf  # UNRDF docs\n`);
}

/**
 * Main init command
 */
export const initCommand = defineCommand({
  meta: {
    name: 'init',
    description: 'Initialize a new UNRDF project from template'
  },
  args: {
    name: {
      type: 'positional',
      description: 'Project name',
      required: false
    },
    template: {
      type: 'string',
      description: 'Template type (starter, governance, analytics)',
      required: false
    },
    'base-iri': {
      type: 'string',
      description: 'Base IRI for the project',
      required: false
    },
    'no-git': {
      type: 'boolean',
      description: 'Skip git initialization',
      default: false
    },
    'no-install': {
      type: 'boolean',
      description: 'Skip npm install',
      default: false
    }
  },
  async run(ctx) {
    console.log('🚀 UNRDF Project Initialization\n');

    const readline = createInterface({ input, output });

    try {
      // Get project name
      let projectName = ctx.args.name;
      if (!projectName) {
        projectName = await prompt(readline, 'Project name', 'my-unrdf-project');
      }

      // Validate project name
      if (!projectName || !/^[a-z0-9-]+$/.test(projectName)) {
        throw new Error('Project name must contain only lowercase letters, numbers, and hyphens');
      }

      // Get template type
      let templateType = ctx.args.template;
      if (!templateType) {
        templateType = await select(
          readline,
          'Select project template:',
          ['starter', 'governance', 'analytics'],
          'starter'
        );
      }

      // Validate template type
      if (!['starter', 'governance', 'analytics'].includes(templateType)) {
        throw new Error(`Unknown template type: ${templateType}`);
      }

      // Get base IRI
      let baseIRI = ctx.args['base-iri'];
      if (!baseIRI) {
        baseIRI = await prompt(
          readline,
          'Base IRI',
          `http://example.org/${projectName}/`
        );
      }

      // Get git initialization preference
      const initGit = !ctx.args['no-git'];

      // Get install preference
      const runInstall = !ctx.args['no-install'];

      console.log('\n📋 Configuration:');
      console.log(`   Project name: ${projectName}`);
      console.log(`   Template: ${templateType}`);
      console.log(`   Base IRI: ${baseIRI}`);
      console.log(`   Initialize git: ${initGit ? 'yes' : 'no'}`);
      console.log(`   Install dependencies: ${runInstall ? 'yes' : 'no'}\n`);

      const confirm = await prompt(readline, 'Continue?', 'yes');
      if (!['y', 'yes'].includes(confirm.toLowerCase())) {
        console.log('❌ Cancelled');
        process.exit(0);
      }

      // Create project
      const projectPath = resolve(process.cwd(), projectName);
      console.log(`\n📦 Creating project at ${projectPath}...\n`);

      // FM-CLI-008: Use transactional init with rollback capability
      const initResult = await executeInitTransaction(async (txn) => {
        // Copy template
        await copyTemplate(templateType, projectPath);

        // Update files
        await updatePackageJson(projectPath, projectName);
        await updateConfig(projectPath, baseIRI);

        // Initialize git
        await initializeGit(projectPath, initGit);

        return {
          projectPath,
          projectName,
          templateType
        };
      }, { onFailure: 'rollback' });

      if (!initResult.success) {
        console.error(`\n❌ ${initResult.error}`);
        if (initResult.rollback) {
          console.log(`✅ Rolled back ${initResult.rollback.rollbackCount} operations`);
          if (initResult.rollback.errors.length > 0) {
            console.warn('⚠️  Errors during rollback:');
            initResult.rollback.errors.forEach(err => console.warn(`   - ${err}`));
          }
        }
        throw new Error('Project initialization failed and was rolled back');
      }

      // Install dependencies
      if (runInstall) {
        console.log('\n📦 Installing dependencies...');
        const { execSync } = await import('node:child_process');
        try {
          execSync('npm install', { cwd: projectPath, stdio: 'inherit' });
          console.log('✅ Dependencies installed');
        } catch (error) {
          console.warn('⚠️  Could not install dependencies. Run "npm install" manually.');
        }
      }

      // Display next steps
      displayNextSteps(projectPath, projectName, templateType);

    } catch (error) {
      console.error(`\n❌ Error: ${error.message}`);
      process.exit(1);
    } finally {
      readline.close();
    }
  }
});

export default initCommand;
