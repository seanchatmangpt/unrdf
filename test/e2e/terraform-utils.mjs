/**
 * @file Terraform Utilities for KGC Sidecar E2E Tests
 * @module terraform-utils
 * 
 * @description
 * Utilities for managing Terraform infrastructure in E2E tests
 * for the KGC sidecar Kubernetes deployment.
 */

import { execSync, spawn } from 'child_process';
import { readFileSync, writeFileSync, existsSync, mkdirSync, rmSync } from 'fs';
import { join, dirname } from 'path';
import { randomUUID } from 'crypto';

/**
 * Terraform utilities for KGC sidecar E2E tests
 */
export class TerraformManager {
  constructor(terraformDir, options = {}) {
    this.terraformDir = terraformDir;
    this.options = {
      autoApprove: true,
      stateFile: null,
      varFile: null,
      ...options
    };
    this.initialized = false;
    this.applied = false;
    this.outputs = null;
  }

  /**
   * Initialize Terraform
   */
  async init() {
    if (this.initialized) {
      return;
    }

    console.log('🔧 Initializing Terraform...');
    
    const initCommand = ['terraform', 'init'];
    if (this.options.stateFile) {
      initCommand.push('-state', this.options.stateFile);
    }
    
    execSync(initCommand.join(' '), {
      cwd: this.terraformDir,
      stdio: 'inherit'
    });
    
    this.initialized = true;
    console.log('✅ Terraform initialized');
  }

  /**
   * Plan Terraform deployment
   */
  async plan(variables = {}) {
    console.log('📋 Planning Terraform deployment...');
    
    const planCommand = ['terraform', 'plan'];
    
    if (this.options.stateFile) {
      planCommand.push('-state', this.options.stateFile);
    }
    
    if (this.options.varFile) {
      planCommand.push('-var-file', this.options.varFile);
    }
    
    // Add variables
    for (const [key, value] of Object.entries(variables)) {
      planCommand.push('-var', `${key}=${value}`);
    }
    
    planCommand.push('-out', 'terraform.tfplan');
    
    const planOutput = execSync(planCommand.join(' '), {
      cwd: this.terraformDir,
      stdio: 'pipe',
      encoding: 'utf8'
    });
    
    console.log('✅ Terraform plan created');
    return planOutput;
  }

  /**
   * Apply Terraform deployment
   */
  async apply(variables = {}) {
    if (this.applied) {
      return this.outputs;
    }

    console.log('🚀 Applying Terraform deployment...');
    
    // First plan
    await this.plan(variables);
    
    // Then apply
    const applyCommand = ['terraform', 'apply'];
    
    if (this.options.autoApprove) {
      applyCommand.push('-auto-approve');
    }
    
    if (this.options.stateFile) {
      applyCommand.push('-state', this.options.stateFile);
    }
    
    applyCommand.push('terraform.tfplan');
    
    execSync(applyCommand.join(' '), {
      cwd: this.terraformDir,
      stdio: 'inherit'
    });
    
    // Get outputs
    this.outputs = await this.getOutputs();
    this.applied = true;
    
    console.log('✅ Terraform deployment applied');
    return this.outputs;
  }

  /**
   * Destroy Terraform deployment
   */
  async destroy(variables = {}) {
    console.log('🗑️ Destroying Terraform deployment...');
    
    const destroyCommand = ['terraform', 'destroy'];
    
    if (this.options.autoApprove) {
      destroyCommand.push('-auto-approve');
    }
    
    if (this.options.stateFile) {
      destroyCommand.push('-state', this.options.stateFile);
    }
    
    if (this.options.varFile) {
      destroyCommand.push('-var-file', this.options.varFile);
    }
    
    // Add variables
    for (const [key, value] of Object.entries(variables)) {
      destroyCommand.push('-var', `${key}=${value}`);
    }
    
    execSync(destroyCommand.join(' '), {
      cwd: this.terraformDir,
      stdio: 'inherit'
    });
    
    this.applied = false;
    this.outputs = null;
    
    console.log('✅ Terraform deployment destroyed');
  }

  /**
   * Get Terraform outputs
   */
  async getOutputs() {
    console.log('📊 Getting Terraform outputs...');
    
    const outputCommand = ['terraform', 'output', '-json'];
    
    if (this.options.stateFile) {
      outputCommand.push('-state', this.options.stateFile);
    }
    
    const output = execSync(outputCommand.join(' '), {
      cwd: this.terraformDir,
      stdio: 'pipe',
      encoding: 'utf8'
    });
    
    return JSON.parse(output);
  }

  /**
   * Get specific output value
   */
  getOutput(name) {
    if (!this.outputs) {
      throw new Error('Terraform not applied yet');
    }
    
    return this.outputs[name]?.value;
  }

  /**
   * Validate Terraform configuration
   */
  async validate() {
    console.log('✅ Validating Terraform configuration...');
    
    const validateCommand = ['terraform', 'validate'];
    
    if (this.options.stateFile) {
      validateCommand.push('-state', this.options.stateFile);
    }
    
    execSync(validateCommand.join(' '), {
      cwd: this.terraformDir,
      stdio: 'inherit'
    });
    
    console.log('✅ Terraform configuration is valid');
  }

  /**
   * Format Terraform files
   */
  async format() {
    console.log('🎨 Formatting Terraform files...');
    
    execSync('terraform fmt -recursive', {
      cwd: this.terraformDir,
      stdio: 'inherit'
    });
    
    console.log('✅ Terraform files formatted');
  }

  /**
   * Show Terraform state
   */
  async show() {
    console.log('📋 Showing Terraform state...');
    
    const showCommand = ['terraform', 'show'];
    
    if (this.options.stateFile) {
      showCommand.push('-state', this.options.stateFile);
    }
    
    const output = execSync(showCommand.join(' '), {
      cwd: this.terraformDir,
      stdio: 'pipe',
      encoding: 'utf8'
    });
    
    console.log(output);
    return output;
  }

  /**
   * List Terraform resources
   */
  async list() {
    console.log('📋 Listing Terraform resources...');
    
    const listCommand = ['terraform', 'state', 'list'];
    
    if (this.options.stateFile) {
      listCommand.push('-state', this.options.stateFile);
    }
    
    const output = execSync(listCommand.join(' '), {
      cwd: this.terraformDir,
      stdio: 'pipe',
      encoding: 'utf8'
    });
    
    return output.trim().split('\n');
  }

  /**
   * Import existing resource
   */
  async import(address, id) {
    console.log(`📥 Importing resource ${address} with ID ${id}...`);
    
    const importCommand = ['terraform', 'import', address, id];
    
    if (this.options.stateFile) {
      importCommand.push('-state', this.options.stateFile);
    }
    
    execSync(importCommand.join(' '), {
      cwd: this.terraformDir,
      stdio: 'inherit'
    });
    
    console.log(`✅ Resource ${address} imported`);
  }

  /**
   * Remove resource from state
   */
  async remove(address) {
    console.log(`🗑️ Removing resource ${address} from state...`);
    
    const removeCommand = ['terraform', 'state', 'rm', address];
    
    if (this.options.stateFile) {
      removeCommand.push('-state', this.options.stateFile);
    }
    
    execSync(removeCommand.join(' '), {
      cwd: this.terraformDir,
      stdio: 'inherit'
    });
    
    console.log(`✅ Resource ${address} removed from state`);
  }

  /**
   * Refresh Terraform state
   */
  async refresh() {
    console.log('🔄 Refreshing Terraform state...');
    
    const refreshCommand = ['terraform', 'refresh'];
    
    if (this.options.stateFile) {
      refreshCommand.push('-state', this.options.stateFile);
    }
    
    if (this.options.varFile) {
      refreshCommand.push('-var-file', this.options.varFile);
    }
    
    execSync(refreshCommand.join(' '), {
      cwd: this.terraformDir,
      stdio: 'inherit'
    });
    
    console.log('✅ Terraform state refreshed');
  }

  /**
   * Create variables file
   */
  createVariablesFile(variables, filename = 'terraform.tfvars') {
    const filePath = join(this.terraformDir, filename);
    
    let content = '';
    for (const [key, value] of Object.entries(variables)) {
      if (typeof value === 'string') {
        content += `${key} = "${value}"\n`;
      } else if (typeof value === 'number') {
        content += `${key} = ${value}\n`;
      } else if (typeof value === 'boolean') {
        content += `${key} = ${value}\n`;
      } else if (Array.isArray(value)) {
        content += `${key} = [${value.map(v => `"${v}"`).join(', ')}]\n`;
      } else if (typeof value === 'object') {
        content += `${key} = {\n`;
        for (const [subKey, subValue] of Object.entries(value)) {
          content += `  ${subKey} = "${subValue}"\n`;
        }
        content += `}\n`;
      }
    }
    
    writeFileSync(filePath, content);
    this.options.varFile = filename;
    
    console.log(`✅ Variables file created: ${filePath}`);
    return filePath;
  }

  /**
   * Create state file path
   */
  createStateFile(suffix = null) {
    const stateFilename = suffix ? `terraform.tfstate.${suffix}` : 'terraform.tfstate';
    const statePath = join(this.terraformDir, stateFilename);
    this.options.stateFile = statePath;
    
    console.log(`✅ State file path set: ${statePath}`);
    return statePath;
  }

  /**
   * Cleanup Terraform files
   */
  async cleanup() {
    console.log('🧹 Cleaning up Terraform files...');
    
    const filesToRemove = [
      'terraform.tfplan',
      'terraform.tfvars',
      'terraform.tfstate.backup',
      '.terraform.lock.hcl'
    ];
    
    for (const file of filesToRemove) {
      const filePath = join(this.terraformDir, file);
      if (existsSync(filePath)) {
        rmSync(filePath, { force: true });
        console.log(`✅ Removed ${file}`);
      }
    }
    
    // Remove .terraform directory
    const terraformDir = join(this.terraformDir, '.terraform');
    if (existsSync(terraformDir)) {
      rmSync(terraformDir, { recursive: true, force: true });
      console.log('✅ Removed .terraform directory');
    }
    
    console.log('✅ Terraform cleanup completed');
  }

  /**
   * Get Terraform version
   */
  getVersion() {
    const output = execSync('terraform version', {
      cwd: this.terraformDir,
      stdio: 'pipe',
      encoding: 'utf8'
    });
    
    return output.trim();
  }

  /**
   * Check if Terraform is installed
   */
  static isInstalled() {
    try {
      execSync('terraform version', { stdio: 'pipe' });
      return true;
    } catch (error) {
      return false;
    }
  }

  /**
   * Get Terraform working directory
   */
  getWorkingDirectory() {
    return this.terraformDir;
  }

  /**
   * Get Terraform options
   */
  getOptions() {
    return { ...this.options };
  }

  /**
   * Update Terraform options
   */
  updateOptions(newOptions) {
    this.options = { ...this.options, ...newOptions };
  }
}

/**
 * Create Terraform variables for E2E tests
 */
export function createE2ETerraformVariables(options = {}) {
  const {
    namespace = `kgc-e2e-test-${randomUUID().substring(0, 8)}`,
    environment = 'e2e-test',
    imageTag = 'latest',
    replicas = 2,
    enableIngress = true,
    enableHPA = true,
    enableNetworkPolicy = true,
    enablePDB = true,
    observabilityEndpoint = 'http://jaeger:14268/api/traces',
    databaseUrl = 'postgresql://test:test@postgres:5432/kgc_test',
    logLevel = 'debug',
    enableObservability = true,
    enableMetrics = true,
    enableTracing = true,
    samplingRatio = 1.0,
    maxHooks = 10000,
    timeoutMs = 2000,
    cacheSize = 10000,
    batchSize = 1000,
    maxConcurrency = 5,
    enableFastPath = true,
    enableCaching = true,
    enableBatchProcessing = true,
    enableSandboxing = true,
    sandboxTimeout = 30000,
    sandboxMemoryLimit = 67108864,
    enableLockchain = false,
    enableResolution = false,
    hpaMinReplicas = 1,
    hpaMaxReplicas = 5,
    hpaCpuTarget = 70,
    hpaMemoryTarget = 80,
    pdbMinAvailable = 1,
    ingressClass = 'nginx',
    ingressHost = '',
    ingressTlsEnabled = false,
    ingressTlsSecret = '',
    nodeSelector = {},
    tolerations = [],
    affinity = {},
    labels = {},
    annotations = {}
  } = options;

  return {
    namespace,
    environment,
    image_tag: imageTag,
    replicas,
    enable_ingress: enableIngress,
    enable_hpa: enableHPA,
    enable_network_policy: enableNetworkPolicy,
    enable_pdb: enablePDB,
    observability_endpoint: observabilityEndpoint,
    database_url: databaseUrl,
    log_level: logLevel,
    enable_observability: enableObservability,
    enable_metrics: enableMetrics,
    enable_tracing: enableTracing,
    sampling_ratio: samplingRatio,
    max_hooks: maxHooks,
    timeout_ms: timeoutMs,
    cache_size: cacheSize,
    batch_size: batchSize,
    max_concurrency: maxConcurrency,
    enable_fast_path: enableFastPath,
    enable_caching: enableCaching,
    enable_batch_processing: enableBatchProcessing,
    enable_sandboxing: enableSandboxing,
    sandbox_timeout: sandboxTimeout,
    sandbox_memory_limit: sandboxMemoryLimit,
    enable_lockchain: enableLockchain,
    enable_resolution: enableResolution,
    hpa_min_replicas: hpaMinReplicas,
    hpa_max_replicas: hpaMaxReplicas,
    hpa_cpu_target: hpaCpuTarget,
    hpa_memory_target: hpaMemoryTarget,
    pdb_min_available: pdbMinAvailable,
    ingress_class: ingressClass,
    ingress_host: ingressHost,
    ingress_tls_enabled: ingressTlsEnabled,
    ingress_tls_secret: ingressTlsSecret,
    node_selector: nodeSelector,
    tolerations,
    affinity,
    labels,
    annotations
  };
}

/**
 * Create a new Terraform manager instance
 */
export function createTerraformManager(terraformDir, options = {}) {
  return new TerraformManager(terraformDir, options);
}

/**
 * Default Terraform manager instance
 */
export const terraformManager = createTerraformManager('./terraform');

export default TerraformManager;
