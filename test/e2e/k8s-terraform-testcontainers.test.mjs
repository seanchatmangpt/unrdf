/**
 * @file Kubernetes Terraform E2E Test with Testcontainers
 * @module k8s-terraform-testcontainers-e2e
 * 
 * @description
 * Comprehensive E2E test suite for KGC sidecar using Kubernetes,
 * Terraform, and Testcontainers for infrastructure provisioning.
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { execSync } from 'child_process';
import { readFileSync, writeFileSync, existsSync, mkdirSync } from 'fs';
import { join } from 'path';
import { randomUUID } from 'crypto';

// Testcontainers imports
import { GenericContainer, StartedTestContainer } from 'testcontainers';
import { PostgreSqlContainer } from '@testcontainers/postgresql';
import { RedisContainer } from '@testcontainers/redis';

// Kubernetes client
import * as k8s from '@kubernetes/client-node';

// Terraform utilities
import { TerraformStack, TerraformOutput } from 'cdktf';
import { KubernetesProvider } from '@cdktf/provider-kubernetes/lib/provider';
import { Namespace } from '@cdktf/provider-kubernetes/lib/namespace';
import { Deployment } from '@cdktf/provider-kubernetes/lib/deployment';
import { Service } from '@cdktf/provider-kubernetes/lib/service';

describe('KGC Sidecar K8s Terraform E2E with Testcontainers', () => {
  let k8sClient: k8s.KubeConfig;
  let k8sApi: k8s.CoreV1Api;
  let k8sAppsApi: k8s.AppsV1Api;
  let k8sNetworkingApi: k8s.NetworkingV1Api;
  
  let testNamespace: string;
  let terraformDir: string;
  let terraformStateFile: string;
  
  // Testcontainers
  let postgresContainer: StartedTestContainer;
  let redisContainer: StartedTestContainer;
  let jaegerContainer: StartedTestContainer;
  
  // Test data
  let testData: any;
  let testConfig: any;

  beforeAll(async () => {
    console.log('🚀 Setting up K8s Terraform E2E test environment...');
    
    // Setup Kubernetes client
    k8sClient = new k8s.KubeConfig();
    k8sClient.loadFromDefault();
    k8sApi = k8sClient.makeApiClient(k8s.CoreV1Api);
    k8sAppsApi = k8sClient.makeApiClient(k8s.AppsV1Api);
    k8sNetworkingApi = k8sClient.makeApiClient(k8s.NetworkingV1Api);
    
    // Generate unique test namespace
    testNamespace = `kgc-e2e-test-${randomUUID().substring(0, 8)}`;
    terraformDir = join(process.cwd(), 'terraform');
    terraformStateFile = join(terraformDir, `terraform.tfstate.${testNamespace}`);
    
    // Setup test data
    testData = {
      namespace: testNamespace,
      environment: 'e2e-test',
      imageTag: 'latest',
      replicas: 2,
      resources: {
        requests: { cpu: '100m', memory: '128Mi' },
        limits: { cpu: '200m', memory: '256Mi' }
      }
    };
    
    testConfig = {
      basePath: '/app/data',
      strictMode: true,
      enableObservability: true,
      enableMetrics: true,
      enableTracing: true,
      logLevel: 'debug',
      performance: {
        enableFastPath: true,
        enableCaching: true,
        maxConcurrency: 5,
        timeoutMs: 1000
      },
      observability: {
        endpoint: 'http://jaeger:14268/api/traces',
        samplingRatio: 1.0
      },
      database: {
        url: 'postgresql://test:test@postgres:5432/kgc_test'
      },
      e2e: {
        enabled: true,
        testMode: true,
        mockExternalServices: true
      }
    };
    
    console.log(`📦 Test namespace: ${testNamespace}`);
    console.log(`📁 Terraform directory: ${terraformDir}`);
  });

  afterAll(async () => {
    console.log('🧹 Cleaning up K8s Terraform E2E test environment...');
    
    try {
      // Cleanup Terraform resources
      if (existsSync(terraformStateFile)) {
        console.log('🗑️ Destroying Terraform resources...');
        execSync(`cd ${terraformDir} && terraform destroy -auto-approve -state=${terraformStateFile}`, {
          stdio: 'inherit',
          env: { ...process.env, TF_VAR_namespace: testNamespace }
        });
      }
      
      // Cleanup testcontainers
      if (postgresContainer) {
        await postgresContainer.stop();
      }
      if (redisContainer) {
        await redisContainer.stop();
      }
      if (jaegerContainer) {
        await jaegerContainer.stop();
      }
      
      // Cleanup namespace
      try {
        await k8sApi.deleteNamespace(testNamespace);
        console.log(`✅ Namespace ${testNamespace} deleted`);
      } catch (error) {
        console.warn(`⚠️ Failed to delete namespace ${testNamespace}:`, error.message);
      }
      
    } catch (error) {
      console.error('❌ Cleanup failed:', error.message);
    }
  });

  beforeEach(() => {
    // Reset test state
    testData.namespace = testNamespace;
  });

  describe('Testcontainers Setup', () => {
    it('should start PostgreSQL container', async () => {
      console.log('🐘 Starting PostgreSQL container...');
      
      postgresContainer = await new PostgreSqlContainer('postgres:15-alpine')
        .withDatabase('kgc_test')
        .withUsername('test')
        .withPassword('test')
        .withExposedPorts(5432)
        .withEnvironment({
          POSTGRES_DB: 'kgc_test',
          POSTGRES_USER: 'test',
          POSTGRES_PASSWORD: 'test'
        })
        .start();
      
      expect(postgresContainer).toBeDefined();
      expect(postgresContainer.getMappedPort(5432)).toBeDefined();
      
      console.log(`✅ PostgreSQL container started on port ${postgresContainer.getMappedPort(5432)}`);
    });

    it('should start Redis container', async () => {
      console.log('🔴 Starting Redis container...');
      
      redisContainer = await new RedisContainer('redis:7-alpine')
        .withExposedPorts(6379)
        .withCommand(['redis-server', '--appendonly', 'yes'])
        .start();
      
      expect(redisContainer).toBeDefined();
      expect(redisContainer.getMappedPort(6379)).toBeDefined();
      
      console.log(`✅ Redis container started on port ${redisContainer.getMappedPort(6379)}`);
    });

    it('should start Jaeger container', async () => {
      console.log('🔍 Starting Jaeger container...');
      
      jaegerContainer = await new GenericContainer('jaegertracing/all-in-one:latest')
        .withExposedPorts(14268, 16686)
        .withEnvironment({
          COLLECTOR_OTLP_ENABLED: 'true'
        })
        .start();
      
      expect(jaegerContainer).toBeDefined();
      expect(jaegerContainer.getMappedPort(14268)).toBeDefined();
      expect(jaegerContainer.getMappedPort(16686)).toBeDefined();
      
      console.log(`✅ Jaeger container started on ports ${jaegerContainer.getMappedPort(14268)} and ${jaegerContainer.getMappedPort(16686)}`);
    });
  });

  describe('Terraform Infrastructure', () => {
    it('should initialize Terraform', async () => {
      console.log('🔧 Initializing Terraform...');
      
      // Create terraform.tfvars file
      const tfvars = `
namespace = "${testNamespace}"
environment = "e2e-test"
image_tag = "latest"
replicas = 2
enable_ingress = true
enable_hpa = true
enable_network_policy = true
enable_pdb = true
observability_endpoint = "http://jaeger:14268/api/traces"
database_url = "postgresql://test:test@postgres:5432/kgc_test"
log_level = "debug"
enable_observability = true
enable_metrics = true
enable_tracing = true
sampling_ratio = 1.0
max_hooks = 10000
timeout_ms = 2000
cache_size = 10000
batch_size = 1000
max_concurrency = 5
enable_fast_path = true
enable_caching = true
enable_batch_processing = true
enable_sandboxing = true
sandbox_timeout = 30000
sandbox_memory_limit = 67108864
enable_lockchain = false
enable_resolution = false
hpa_min_replicas = 1
hpa_max_replicas = 5
hpa_cpu_target = 70
hpa_memory_target = 80
pdb_min_available = 1
ingress_class = "nginx"
ingress_host = "kgc-sidecar-${testNamespace}.local"
ingress_tls_enabled = false
`;
      
      writeFileSync(join(terraformDir, 'terraform.tfvars'), tfvars);
      
      // Initialize Terraform
      execSync(`cd ${terraformDir} && terraform init`, {
        stdio: 'inherit'
      });
      
      console.log('✅ Terraform initialized successfully');
    });

    it('should plan Terraform deployment', async () => {
      console.log('📋 Planning Terraform deployment...');
      
      const planOutput = execSync(`cd ${terraformDir} && terraform plan -out=terraform.tfplan`, {
        stdio: 'pipe',
        encoding: 'utf8'
      });
      
      expect(planOutput).toContain('Plan:');
      expect(planOutput).toContain('to add');
      
      console.log('✅ Terraform plan created successfully');
    });

    it('should apply Terraform deployment', async () => {
      console.log('🚀 Applying Terraform deployment...');
      
      execSync(`cd ${terraformDir} && terraform apply -auto-approve terraform.tfplan`, {
        stdio: 'inherit'
      });
      
      // Wait for deployment to be ready
      await waitForDeploymentReady(testNamespace, 'kgc-sidecar');
      
      console.log('✅ Terraform deployment applied successfully');
    });

    it('should verify Terraform outputs', async () => {
      console.log('📊 Verifying Terraform outputs...');
      
      const outputs = execSync(`cd ${terraformDir} && terraform output -json`, {
        stdio: 'pipe',
        encoding: 'utf8'
      });
      
      const parsedOutputs = JSON.parse(outputs);
      
      expect(parsedOutputs.namespace.value).toBe(testNamespace);
      expect(parsedOutputs.deployment_name.value).toBe('kgc-sidecar');
      expect(parsedOutputs.service_name.value).toBe('kgc-sidecar-service');
      expect(parsedOutputs.replicas.value).toBe(2);
      
      console.log('✅ Terraform outputs verified successfully');
    });
  });

  describe('Kubernetes Resources', () => {
    it('should create namespace', async () => {
      console.log('📦 Verifying namespace creation...');
      
      const namespace = await k8sApi.readNamespace(testNamespace);
      expect(namespace.body.metadata.name).toBe(testNamespace);
      expect(namespace.body.metadata.labels.app).toBe('kgc-sidecar');
      expect(namespace.body.metadata.labels.env).toBe('e2e-test');
      
      console.log('✅ Namespace created and verified');
    });

    it('should create deployment', async () => {
      console.log('🚀 Verifying deployment creation...');
      
      const deployment = await k8sAppsApi.readNamespacedDeployment('kgc-sidecar', testNamespace);
      expect(deployment.body.metadata.name).toBe('kgc-sidecar');
      expect(deployment.body.spec.replicas).toBe(2);
      expect(deployment.body.spec.template.spec.containers[0].image).toContain('unrdf/kgc-sidecar:latest');
      
      console.log('✅ Deployment created and verified');
    });

    it('should create service', async () => {
      console.log('🌐 Verifying service creation...');
      
      const service = await k8sApi.readNamespacedService('kgc-sidecar-service', testNamespace);
      expect(service.body.metadata.name).toBe('kgc-sidecar-service');
      expect(service.body.spec.ports).toHaveLength(2);
      expect(service.body.spec.ports[0].port).toBe(3000);
      expect(service.body.spec.ports[1].port).toBe(8080);
      
      console.log('✅ Service created and verified');
    });

    it('should create configmap', async () => {
      console.log('⚙️ Verifying ConfigMap creation...');
      
      const configMap = await k8sApi.readNamespacedConfigMap('kgc-sidecar-config', testNamespace);
      expect(configMap.body.metadata.name).toBe('kgc-sidecar-config');
      expect(configMap.body.data).toHaveProperty('config.mjs');
      
      console.log('✅ ConfigMap created and verified');
    });

    it('should create secret', async () => {
      console.log('🔐 Verifying Secret creation...');
      
      const secret = await k8sApi.readNamespacedSecret('kgc-sidecar-secrets', testNamespace);
      expect(secret.body.metadata.name).toBe('kgc-sidecar-secrets');
      expect(secret.body.data).toHaveProperty('api-key');
      expect(secret.body.data).toHaveProperty('encryption-key');
      expect(secret.body.data).toHaveProperty('database-url');
      
      console.log('✅ Secret created and verified');
    });

    it('should create service account', async () => {
      console.log('👤 Verifying ServiceAccount creation...');
      
      const serviceAccount = await k8sApi.readNamespacedServiceAccount('kgc-sidecar-sa', testNamespace);
      expect(serviceAccount.body.metadata.name).toBe('kgc-sidecar-sa');
      
      console.log('✅ ServiceAccount created and verified');
    });

    it('should create role and role binding', async () => {
      console.log('🔑 Verifying Role and RoleBinding creation...');
      
      const role = await k8sApi.readNamespacedRole('kgc-sidecar-role', testNamespace);
      expect(role.body.metadata.name).toBe('kgc-sidecar-role');
      
      const roleBinding = await k8sApi.readNamespacedRoleBinding('kgc-sidecar-binding', testNamespace);
      expect(roleBinding.body.metadata.name).toBe('kgc-sidecar-binding');
      
      console.log('✅ Role and RoleBinding created and verified');
    });

    it('should create ingress', async () => {
      console.log('🌍 Verifying Ingress creation...');
      
      const ingress = await k8sNetworkingApi.readNamespacedIngress('kgc-sidecar-ingress', testNamespace);
      expect(ingress.body.metadata.name).toBe('kgc-sidecar-ingress');
      expect(ingress.body.spec.rules[0].host).toContain('kgc-sidecar');
      
      console.log('✅ Ingress created and verified');
    });

    it('should create HPA', async () => {
      console.log('📈 Verifying HPA creation...');
      
      const hpa = await k8sApi.readNamespacedHorizontalPodAutoscaler('kgc-sidecar-hpa', testNamespace);
      expect(hpa.body.metadata.name).toBe('kgc-sidecar-hpa');
      expect(hpa.body.spec.minReplicas).toBe(1);
      expect(hpa.body.spec.maxReplicas).toBe(10);
      
      console.log('✅ HPA created and verified');
    });

    it('should create network policy', async () => {
      console.log('🛡️ Verifying NetworkPolicy creation...');
      
      const networkPolicy = await k8sNetworkingApi.readNamespacedNetworkPolicy('kgc-sidecar-netpol', testNamespace);
      expect(networkPolicy.body.metadata.name).toBe('kgc-sidecar-netpol');
      expect(networkPolicy.body.spec.policyTypes).toContain('Ingress');
      expect(networkPolicy.body.spec.policyTypes).toContain('Egress');
      
      console.log('✅ NetworkPolicy created and verified');
    });

    it('should create PDB', async () => {
      console.log('🛡️ Verifying PDB creation...');
      
      const pdb = await k8sApi.readNamespacedPodDisruptionBudget('kgc-sidecar-pdb', testNamespace);
      expect(pdb.body.metadata.name).toBe('kgc-sidecar-pdb');
      expect(pdb.body.spec.minAvailable).toBe(1);
      
      console.log('✅ PDB created and verified');
    });
  });

  describe('Pod Status and Health', () => {
    it('should have running pods', async () => {
      console.log('🏃 Verifying pod status...');
      
      const pods = await k8sApi.listNamespacedPod(testNamespace);
      const kgcPods = pods.body.items.filter(pod => 
        pod.metadata.labels.app === 'kgc-sidecar'
      );
      
      expect(kgcPods.length).toBeGreaterThan(0);
      
      for (const pod of kgcPods) {
        expect(pod.status.phase).toBe('Running');
        expect(pod.status.containerStatuses[0].ready).toBe(true);
        expect(pod.status.containerStatuses[0].restartCount).toBe(0);
      }
      
      console.log(`✅ ${kgcPods.length} pods are running and healthy`);
    });

    it('should have healthy containers', async () => {
      console.log('🏥 Verifying container health...');
      
      const pods = await k8sApi.listNamespacedPod(testNamespace);
      const kgcPods = pods.body.items.filter(pod => 
        pod.metadata.labels.app === 'kgc-sidecar'
      );
      
      for (const pod of kgcPods) {
        const containerStatus = pod.status.containerStatuses[0];
        expect(containerStatus.ready).toBe(true);
        expect(containerStatus.started).toBe(true);
        expect(containerStatus.restartCount).toBe(0);
        
        // Check liveness and readiness probes
        if (containerStatus.state.running) {
          expect(containerStatus.state.running.startedAt).toBeDefined();
        }
      }
      
      console.log('✅ All containers are healthy');
    });

    it('should have correct resource limits', async () => {
      console.log('📊 Verifying resource limits...');
      
      const pods = await k8sApi.listNamespacedPod(testNamespace);
      const kgcPods = pods.body.items.filter(pod => 
        pod.metadata.labels.app === 'kgc-sidecar'
      );
      
      for (const pod of kgcPods) {
        const container = pod.spec.containers[0];
        expect(container.resources.requests.cpu).toBe('250m');
        expect(container.resources.requests.memory).toBe('256Mi');
        expect(container.resources.limits.cpu).toBe('500m');
        expect(container.resources.limits.memory).toBe('512Mi');
      }
      
      console.log('✅ Resource limits are correct');
    });
  });

  describe('Service Connectivity', () => {
    it('should expose service ports', async () => {
      console.log('🔌 Verifying service ports...');
      
      const service = await k8sApi.readNamespacedService('kgc-sidecar-service', testNamespace);
      const ports = service.body.spec.ports;
      
      expect(ports).toHaveLength(2);
      expect(ports[0].port).toBe(3000);
      expect(ports[0].name).toBe('http');
      expect(ports[1].port).toBe(8080);
      expect(ports[1].name).toBe('metrics');
      
      console.log('✅ Service ports are correctly exposed');
    });

    it('should have cluster IP', async () => {
      console.log('🌐 Verifying cluster IP...');
      
      const service = await k8sApi.readNamespacedService('kgc-sidecar-service', testNamespace);
      expect(service.body.spec.clusterIP).toBeDefined();
      expect(service.body.spec.type).toBe('ClusterIP');
      
      console.log(`✅ Cluster IP: ${service.body.spec.clusterIP}`);
    });

    it('should have correct selectors', async () => {
      console.log('🎯 Verifying service selectors...');
      
      const service = await k8sApi.readNamespacedService('kgc-sidecar-service', testNamespace);
      expect(service.body.spec.selector.app).toBe('kgc-sidecar');
      
      console.log('✅ Service selectors are correct');
    });
  });

  describe('Configuration and Secrets', () => {
    it('should have correct environment variables', async () => {
      console.log('🔧 Verifying environment variables...');
      
      const deployment = await k8sAppsApi.readNamespacedDeployment('kgc-sidecar', testNamespace);
      const container = deployment.body.spec.template.spec.containers[0];
      
      const envVars = container.env.reduce((acc, env) => {
        acc[env.name] = env.value;
        return acc;
      }, {});
      
      expect(envVars.NODE_ENV).toBe('production');
      expect(envVars.LOG_LEVEL).toBe('info');
      expect(envVars.ENABLE_OBSERVABILITY).toBe('true');
      expect(envVars.SERVICE_NAME).toBe('kgc-sidecar');
      expect(envVars.NAMESPACE).toBe(testNamespace);
      expect(envVars.ENVIRONMENT).toBe('e2e-test');
      
      console.log('✅ Environment variables are correct');
    });

    it('should have secret references', async () => {
      console.log('🔐 Verifying secret references...');
      
      const deployment = await k8sAppsApi.readNamespacedDeployment('kgc-sidecar', testNamespace);
      const container = deployment.body.spec.template.spec.containers[0];
      
      const secretRefs = container.env.filter(env => env.valueFrom?.secretKeyRef);
      expect(secretRefs).toHaveLength(3);
      
      const secretNames = secretRefs.map(env => env.valueFrom.secretKeyRef.name);
      expect(secretNames).toContain('kgc-sidecar-secrets');
      
      console.log('✅ Secret references are correct');
    });

    it('should have volume mounts', async () => {
      console.log('💾 Verifying volume mounts...');
      
      const deployment = await k8sAppsApi.readNamespacedDeployment('kgc-sidecar', testNamespace);
      const container = deployment.body.spec.template.spec.containers[0];
      
      const volumeMounts = container.volumeMounts;
      expect(volumeMounts).toHaveLength(2);
      
      const configMount = volumeMounts.find(mount => mount.name === 'config');
      expect(configMount).toBeDefined();
      expect(configMount.mountPath).toBe('/app/config');
      expect(configMount.readOnly).toBe(true);
      
      const dataMount = volumeMounts.find(mount => mount.name === 'data');
      expect(dataMount).toBeDefined();
      expect(dataMount.mountPath).toBe('/app/data');
      
      console.log('✅ Volume mounts are correct');
    });
  });

  describe('E2E Functionality Tests', () => {
    it('should handle health checks', async () => {
      console.log('🏥 Testing health checks...');
      
      // Port forward to access the service
      const portForward = execSync(
        `kubectl port-forward -n ${testNamespace} service/kgc-sidecar-service 3000:3000`,
        { stdio: 'pipe', detached: true }
      );
      
      // Wait for port forward to be ready
      await new Promise(resolve => setTimeout(resolve, 5000));
      
      try {
        // Test health endpoint
        const healthResponse = await fetch('http://localhost:3000/health');
        expect(healthResponse.status).toBe(200);
        
        const healthData = await healthResponse.json();
        expect(healthData.status).toBe('healthy');
        
        console.log('✅ Health checks are working');
      } finally {
        // Clean up port forward
        try {
          process.kill(portForward.pid);
        } catch (error) {
          // Ignore cleanup errors
        }
      }
    });

    it('should handle readiness checks', async () => {
      console.log('✅ Testing readiness checks...');
      
      // Port forward to access the service
      const portForward = execSync(
        `kubectl port-forward -n ${testNamespace} service/kgc-sidecar-service 3000:3000`,
        { stdio: 'pipe', detached: true }
      );
      
      // Wait for port forward to be ready
      await new Promise(resolve => setTimeout(resolve, 5000));
      
      try {
        // Test ready endpoint
        const readyResponse = await fetch('http://localhost:3000/ready');
        expect(readyResponse.status).toBe(200);
        
        const readyData = await readyResponse.json();
        expect(readyData.status).toBe('ready');
        
        console.log('✅ Readiness checks are working');
      } finally {
        // Clean up port forward
        try {
          process.kill(portForward.pid);
        } catch (error) {
          // Ignore cleanup errors
        }
      }
    });

    it('should handle metrics endpoint', async () => {
      console.log('📊 Testing metrics endpoint...');
      
      // Port forward to access the service
      const portForward = execSync(
        `kubectl port-forward -n ${testNamespace} service/kgc-sidecar-service 8080:8080`,
        { stdio: 'pipe', detached: true }
      );
      
      // Wait for port forward to be ready
      await new Promise(resolve => setTimeout(resolve, 5000));
      
      try {
        // Test metrics endpoint
        const metricsResponse = await fetch('http://localhost:8080/metrics');
        expect(metricsResponse.status).toBe(200);
        
        const metricsData = await metricsResponse.text();
        expect(metricsData).toContain('kgc_');
        
        console.log('✅ Metrics endpoint is working');
      } finally {
        // Clean up port forward
        try {
          process.kill(portForward.pid);
        } catch (error) {
          // Ignore cleanup errors
        }
      }
    });
  });

  describe('Performance and Scaling', () => {
    it('should handle HPA scaling', async () => {
      console.log('📈 Testing HPA scaling...');
      
      const hpa = await k8sApi.readNamespacedHorizontalPodAutoscaler('kgc-sidecar-hpa', testNamespace);
      expect(hpa.body.spec.minReplicas).toBe(1);
      expect(hpa.body.spec.maxReplicas).toBe(10);
      
      // Check HPA status
      expect(hpa.body.status.currentReplicas).toBeDefined();
      expect(hpa.body.status.desiredReplicas).toBeDefined();
      
      console.log('✅ HPA is configured correctly');
    });

    it('should handle PDB disruption', async () => {
      console.log('🛡️ Testing PDB configuration...');
      
      const pdb = await k8sApi.readNamespacedPodDisruptionBudget('kgc-sidecar-pdb', testNamespace);
      expect(pdb.body.spec.minAvailable).toBe(1);
      
      // Check PDB status
      expect(pdb.body.status.currentHealthy).toBeDefined();
      expect(pdb.body.status.desiredHealthy).toBeDefined();
      
      console.log('✅ PDB is configured correctly');
    });
  });

  describe('Security and Network Policies', () => {
    it('should have network policies', async () => {
      console.log('🛡️ Testing network policies...');
      
      const networkPolicy = await k8sNetworkingApi.readNamespacedNetworkPolicy('kgc-sidecar-netpol', testNamespace);
      expect(networkPolicy.body.spec.policyTypes).toContain('Ingress');
      expect(networkPolicy.body.spec.policyTypes).toContain('Egress');
      
      // Check ingress rules
      expect(networkPolicy.body.spec.ingress).toHaveLength(1);
      expect(networkPolicy.body.spec.ingress[0].from[0].podSelector.matchLabels.app).toBe('test-client');
      
      // Check egress rules
      expect(networkPolicy.body.spec.egress).toHaveLength(2);
      
      console.log('✅ Network policies are configured correctly');
    });

    it('should have RBAC permissions', async () => {
      console.log('🔑 Testing RBAC permissions...');
      
      const role = await k8sApi.readNamespacedRole('kgc-sidecar-role', testNamespace);
      expect(role.body.rules).toHaveLength(2);
      
      const roleBinding = await k8sApi.readNamespacedRoleBinding('kgc-sidecar-binding', testNamespace);
      expect(roleBinding.body.roleRef.name).toBe('kgc-sidecar-role');
      expect(roleBinding.body.subjects[0].name).toBe('kgc-sidecar-sa');
      
      console.log('✅ RBAC permissions are configured correctly');
    });
  });
});

// Helper functions
async function waitForDeploymentReady(namespace: string, deploymentName: string, timeout = 300000) {
  const startTime = Date.now();
  
  while (Date.now() - startTime < timeout) {
    try {
      const deployment = await k8sAppsApi.readNamespacedDeployment(deploymentName, namespace);
      
      if (deployment.body.status.readyReplicas === deployment.body.spec.replicas) {
        console.log(`✅ Deployment ${deploymentName} is ready`);
        return;
      }
      
      console.log(`⏳ Waiting for deployment ${deploymentName} to be ready...`);
      await new Promise(resolve => setTimeout(resolve, 5000));
    } catch (error) {
      console.log(`⏳ Waiting for deployment ${deploymentName} to be created...`);
      await new Promise(resolve => setTimeout(resolve, 5000));
    }
  }
  
  throw new Error(`Deployment ${deploymentName} did not become ready within ${timeout}ms`);
}

async function waitForPodReady(namespace: string, podName: string, timeout = 300000) {
  const startTime = Date.now();
  
  while (Date.now() - startTime < timeout) {
    try {
      const pod = await k8sApi.readNamespacedPod(podName, namespace);
      
      if (pod.body.status.phase === 'Running' && pod.body.status.containerStatuses[0].ready) {
        console.log(`✅ Pod ${podName} is ready`);
        return;
      }
      
      console.log(`⏳ Waiting for pod ${podName} to be ready...`);
      await new Promise(resolve => setTimeout(resolve, 5000));
    } catch (error) {
      console.log(`⏳ Waiting for pod ${podName} to be created...`);
      await new Promise(resolve => setTimeout(resolve, 5000));
    }
  }
  
  throw new Error(`Pod ${podName} did not become ready within ${timeout}ms`);
}



