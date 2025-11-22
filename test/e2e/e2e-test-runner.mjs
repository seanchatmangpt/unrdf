/**
 * @file E2E Test Runner for KGC Sidecar
 * @module e2e-test-runner
 *
 * @description
 * Comprehensive E2E test runner for KGC sidecar using Kubernetes,
 * Terraform, and Testcontainers for infrastructure provisioning.
 */

import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest';
import { createTestcontainersManager } from './testcontainers-setup.mjs';
import { createTerraformManager, createE2ETerraformVariables } from './terraform-utils.mjs';
import { createKubernetesManager, _createK8sResources } from './k8s-utils.mjs';
import { randomUUID } from 'crypto';

describe('KGC Sidecar E2E Test Suite', () => {
  let testcontainersManager;
  let terraformManager;
  let kubernetesManager;

  let testNamespace;
  let testConfig;
  let testResults;

  beforeAll(async () => {
    console.log('🚀 Starting KGC Sidecar E2E Test Suite...');

    // Initialize managers
    testcontainersManager = createTestcontainersManager();
    terraformManager = createTerraformManager('./terraform');
    kubernetesManager = createKubernetesManager();

    // Generate unique test namespace
    testNamespace = `kgc-e2e-test-${randomUUID().substring(0, 8)}`;

    // Test configuration
    testConfig = {
      namespace: testNamespace,
      environment: 'e2e-test',
      imageTag: 'latest',
      replicas: 2,
      timeout: 300000, // 5 minutes
      retryInterval: 5000, // 5 seconds
    };

    // Test results tracking
    testResults = {
      passed: 0,
      failed: 0,
      skipped: 0,
      startTime: Date.now(),
      endTime: null,
      duration: 0,
    };

    console.log(`📦 Test namespace: ${testNamespace}`);
    console.log(`⏱️ Test timeout: ${testConfig.timeout}ms`);
  });

  afterAll(async () => {
    console.log('🧹 Cleaning up E2E test environment...');

    try {
      // Cleanup Terraform resources
      if (terraformManager && testNamespace) {
        console.log('🗑️ Destroying Terraform resources...');
        await terraformManager.destroy({
          namespace: testNamespace,
        });
      }

      // Cleanup Kubernetes resources
      if (kubernetesManager && testNamespace) {
        console.log('🗑️ Cleaning up Kubernetes resources...');
        await kubernetesManager.cleanupNamespace(testNamespace);
      }

      // Cleanup testcontainers
      if (testcontainersManager) {
        console.log('🗑️ Stopping testcontainers...');
        await testcontainersManager.stopAll();
      }
    } catch (error) {
      console.error('❌ Cleanup failed:', error.message);
    }

    // Calculate test results
    testResults.endTime = Date.now();
    testResults.duration = testResults.endTime - testResults.startTime;

    console.log('📊 E2E Test Results:');
    console.log(`   Passed: ${testResults.passed}`);
    console.log(`   Failed: ${testResults.failed}`);
    console.log(`   Skipped: ${testResults.skipped}`);
    console.log(`   Duration: ${testResults.duration}ms`);
    console.log(
      `   Success Rate: ${((testResults.passed / (testResults.passed + testResults.failed)) * 100).toFixed(2)}%`
    );
  });

  beforeEach(() => {
    // Reset test state
    testConfig.namespace = testNamespace;
  });

  describe('Infrastructure Setup', () => {
    it('should start testcontainers', async () => {
      console.log('🐳 Starting testcontainers...');

      await testcontainersManager.startMinimal();

      // Verify containers are running
      const postgres = testcontainersManager.getContainer('postgres');
      const redis = testcontainersManager.getContainer('redis');
      const jaeger = testcontainersManager.getContainer('jaeger');

      expect(postgres).toBeDefined();
      expect(redis).toBeDefined();
      expect(jaeger).toBeDefined();

      console.log('✅ Testcontainers started successfully');
      testResults.passed++;
    });

    it('should initialize Terraform', async () => {
      console.log('🔧 Initializing Terraform...');

      // Create state file for this test
      terraformManager.createStateFile(testNamespace);

      // Initialize Terraform
      await terraformManager.init();

      // Validate configuration
      await terraformManager.validate();

      console.log('✅ Terraform initialized successfully');
      testResults.passed++;
    });

    it('should create Kubernetes namespace', async () => {
      console.log('📦 Creating Kubernetes namespace...');

      const namespace = await kubernetesManager.createNamespace(testNamespace, {
        app: 'kgc-sidecar',
        env: 'e2e-test',
        test: 'true',
      });

      expect(namespace.metadata.name).toBe(testNamespace);
      expect(namespace.metadata.labels.app).toBe('kgc-sidecar');

      console.log('✅ Kubernetes namespace created successfully');
      testResults.passed++;
    });
  });

  describe('Terraform Deployment', () => {
    it('should plan Terraform deployment', async () => {
      console.log('📋 Planning Terraform deployment...');

      const variables = createE2ETerraformVariables({
        namespace: testNamespace,
        environment: 'e2e-test',
        imageTag: 'latest',
        replicas: 2,
        enableIngress: true,
        enableHPA: true,
        enableNetworkPolicy: true,
        enablePDB: true,
      });

      const planOutput = await terraformManager.plan(variables);

      expect(planOutput).toContain('Plan:');
      expect(planOutput).toContain('to add');

      console.log('✅ Terraform plan created successfully');
      testResults.passed++;
    });

    it('should apply Terraform deployment', async () => {
      console.log('🚀 Applying Terraform deployment...');

      const variables = createE2ETerraformVariables({
        namespace: testNamespace,
        environment: 'e2e-test',
        imageTag: 'latest',
        replicas: 2,
        enableIngress: true,
        enableHPA: true,
        enableNetworkPolicy: true,
        enablePDB: true,
      });

      const outputs = await terraformManager.apply(variables);

      expect(outputs.namespace.value).toBe(testNamespace);
      expect(outputs.deployment_name.value).toBe('kgc-sidecar');
      expect(outputs.service_name.value).toBe('kgc-sidecar-service');

      console.log('✅ Terraform deployment applied successfully');
      testResults.passed++;
    });

    it('should verify Terraform outputs', async () => {
      console.log('📊 Verifying Terraform outputs...');

      const outputs = await terraformManager.getOutputs();

      expect(outputs.namespace.value).toBe(testNamespace);
      expect(outputs.deployment_name.value).toBe('kgc-sidecar');
      expect(outputs.service_name.value).toBe('kgc-sidecar-service');
      expect(outputs.replicas.value).toBe(2);
      expect(outputs.environment.value).toBe('e2e-test');

      console.log('✅ Terraform outputs verified successfully');
      testResults.passed++;
    });
  });

  describe('Kubernetes Resources', () => {
    it('should verify namespace exists', async () => {
      console.log('📦 Verifying namespace exists...');

      const namespace = await kubernetesManager.getResourceStatus(
        testNamespace,
        'namespace',
        testNamespace
      );

      expect(namespace.metadata.name).toBe(testNamespace);
      expect(namespace.metadata.labels.app).toBe('kgc-sidecar');
      expect(namespace.metadata.labels.env).toBe('e2e-test');

      console.log('✅ Namespace verified successfully');
      testResults.passed++;
    });

    it('should verify deployment exists', async () => {
      console.log('🚀 Verifying deployment exists...');

      const deployment = await kubernetesManager.getResourceStatus(
        testNamespace,
        'deployment',
        'kgc-sidecar'
      );

      expect(deployment.metadata.name).toBe('kgc-sidecar');
      expect(deployment.spec.replicas).toBe(2);
      expect(deployment.spec.template.spec.containers[0].image).toContain(
        'unrdf/kgc-sidecar:latest'
      );

      console.log('✅ Deployment verified successfully');
      testResults.passed++;
    });

    it('should verify service exists', async () => {
      console.log('🌐 Verifying service exists...');

      const service = await kubernetesManager.getResourceStatus(
        testNamespace,
        'service',
        'kgc-sidecar-service'
      );

      expect(service.metadata.name).toBe('kgc-sidecar-service');
      expect(service.spec.ports).toHaveLength(2);
      expect(service.spec.ports[0].port).toBe(3000);
      expect(service.spec.ports[1].port).toBe(8080);

      console.log('✅ Service verified successfully');
      testResults.passed++;
    });

    it('should verify configmap exists', async () => {
      console.log('⚙️ Verifying ConfigMap exists...');

      const configMap = await kubernetesManager.getResourceStatus(
        testNamespace,
        'configmap',
        'kgc-sidecar-config'
      );

      expect(configMap.metadata.name).toBe('kgc-sidecar-config');
      expect(configMap.data).toHaveProperty('config.mjs');

      console.log('✅ ConfigMap verified successfully');
      testResults.passed++;
    });

    it('should verify secret exists', async () => {
      console.log('🔐 Verifying Secret exists...');

      const secret = await kubernetesManager.getResourceStatus(
        testNamespace,
        'secret',
        'kgc-sidecar-secrets'
      );

      expect(secret.metadata.name).toBe('kgc-sidecar-secrets');
      expect(secret.data).toHaveProperty('api-key');
      expect(secret.data).toHaveProperty('encryption-key');
      expect(secret.data).toHaveProperty('database-url');

      console.log('✅ Secret verified successfully');
      testResults.passed++;
    });

    it('should verify service account exists', async () => {
      console.log('👤 Verifying ServiceAccount exists...');

      const serviceAccount = await kubernetesManager.getResourceStatus(
        testNamespace,
        'serviceaccount',
        'kgc-sidecar-sa'
      );

      expect(serviceAccount.metadata.name).toBe('kgc-sidecar-sa');

      console.log('✅ ServiceAccount verified successfully');
      testResults.passed++;
    });

    it('should verify role and role binding exist', async () => {
      console.log('🔑 Verifying Role and RoleBinding exist...');

      const role = await kubernetesManager.getResourceStatus(
        testNamespace,
        'role',
        'kgc-sidecar-role'
      );
      const roleBinding = await kubernetesManager.getResourceStatus(
        testNamespace,
        'rolebinding',
        'kgc-sidecar-binding'
      );

      expect(role.metadata.name).toBe('kgc-sidecar-role');
      expect(roleBinding.metadata.name).toBe('kgc-sidecar-binding');

      console.log('✅ Role and RoleBinding verified successfully');
      testResults.passed++;
    });

    it('should verify ingress exists', async () => {
      console.log('🌍 Verifying Ingress exists...');

      const ingress = await kubernetesManager.getResourceStatus(
        testNamespace,
        'ingress',
        'kgc-sidecar-ingress'
      );

      expect(ingress.metadata.name).toBe('kgc-sidecar-ingress');
      expect(ingress.spec.rules[0].host).toContain('kgc-sidecar');

      console.log('✅ Ingress verified successfully');
      testResults.passed++;
    });

    it('should verify HPA exists', async () => {
      console.log('📈 Verifying HPA exists...');

      const hpa = await kubernetesManager.getResourceStatus(
        testNamespace,
        'hpa',
        'kgc-sidecar-hpa'
      );

      expect(hpa.metadata.name).toBe('kgc-sidecar-hpa');
      expect(hpa.spec.minReplicas).toBe(1);
      expect(hpa.spec.maxReplicas).toBe(10);

      console.log('✅ HPA verified successfully');
      testResults.passed++;
    });

    it('should verify network policy exists', async () => {
      console.log('🛡️ Verifying NetworkPolicy exists...');

      const networkPolicy = await kubernetesManager.getResourceStatus(
        testNamespace,
        'networkpolicy',
        'kgc-sidecar-netpol'
      );

      expect(networkPolicy.metadata.name).toBe('kgc-sidecar-netpol');
      expect(networkPolicy.spec.policyTypes).toContain('Ingress');
      expect(networkPolicy.spec.policyTypes).toContain('Egress');

      console.log('✅ NetworkPolicy verified successfully');
      testResults.passed++;
    });

    it('should verify PDB exists', async () => {
      console.log('🛡️ Verifying PDB exists...');

      const pdb = await kubernetesManager.getResourceStatus(
        testNamespace,
        'pdb',
        'kgc-sidecar-pdb'
      );

      expect(pdb.metadata.name).toBe('kgc-sidecar-pdb');
      expect(pdb.spec.minAvailable).toBe(1);

      console.log('✅ PDB verified successfully');
      testResults.passed++;
    });
  });

  describe('Pod Status and Health', () => {
    it('should wait for deployment to be ready', async () => {
      console.log('⏳ Waiting for deployment to be ready...');

      const deployment = await kubernetesManager.waitForDeploymentReady(
        testNamespace,
        'kgc-sidecar',
        testConfig.timeout
      );

      expect(deployment.status.readyReplicas).toBe(deployment.spec.replicas);
      expect(deployment.status.availableReplicas).toBe(deployment.spec.replicas);

      console.log('✅ Deployment is ready');
      testResults.passed++;
    });

    it('should verify pods are running', async () => {
      console.log('🏃 Verifying pods are running...');

      const pods = await kubernetesManager.getPodsByLabel(testNamespace, 'app=kgc-sidecar');

      expect(pods.length).toBeGreaterThan(0);

      for (const pod of pods) {
        expect(pod.status.phase).toBe('Running');
        expect(pod.status.containerStatuses[0].ready).toBe(true);
        expect(pod.status.containerStatuses[0].restartCount).toBe(0);
      }

      console.log(`✅ ${pods.length} pods are running and healthy`);
      testResults.passed++;
    });

    it('should verify container health', async () => {
      console.log('🏥 Verifying container health...');

      const pods = await kubernetesManager.getPodsByLabel(testNamespace, 'app=kgc-sidecar');

      for (const pod of pods) {
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
      testResults.passed++;
    });

    it('should verify resource limits', async () => {
      console.log('📊 Verifying resource limits...');

      const pods = await kubernetesManager.getPodsByLabel(testNamespace, 'app=kgc-sidecar');

      for (const pod of pods) {
        const container = pod.spec.containers[0];
        expect(container.resources.requests.cpu).toBe('250m');
        expect(container.resources.requests.memory).toBe('256Mi');
        expect(container.resources.limits.cpu).toBe('500m');
        expect(container.resources.limits.memory).toBe('512Mi');
      }

      console.log('✅ Resource limits are correct');
      testResults.passed++;
    });
  });

  describe('Service Connectivity', () => {
    it('should verify service ports', async () => {
      console.log('🔌 Verifying service ports...');

      const service = await kubernetesManager.getResourceStatus(
        testNamespace,
        'service',
        'kgc-sidecar-service'
      );
      const ports = service.spec.ports;

      expect(ports).toHaveLength(2);
      expect(ports[0].port).toBe(3000);
      expect(ports[0].name).toBe('http');
      expect(ports[1].port).toBe(8080);
      expect(ports[1].name).toBe('metrics');

      console.log('✅ Service ports are correctly exposed');
      testResults.passed++;
    });

    it('should verify cluster IP', async () => {
      console.log('🌐 Verifying cluster IP...');

      const service = await kubernetesManager.getResourceStatus(
        testNamespace,
        'service',
        'kgc-sidecar-service'
      );

      expect(service.spec.clusterIP).toBeDefined();
      expect(service.spec.type).toBe('ClusterIP');

      console.log(`✅ Cluster IP: ${service.spec.clusterIP}`);
      testResults.passed++;
    });

    it('should verify service selectors', async () => {
      console.log('🎯 Verifying service selectors...');

      const service = await kubernetesManager.getResourceStatus(
        testNamespace,
        'service',
        'kgc-sidecar-service'
      );

      expect(service.spec.selector.app).toBe('kgc-sidecar');

      console.log('✅ Service selectors are correct');
      testResults.passed++;
    });
  });

  describe('Configuration and Secrets', () => {
    it('should verify environment variables', async () => {
      console.log('🔧 Verifying environment variables...');

      const deployment = await kubernetesManager.getResourceStatus(
        testNamespace,
        'deployment',
        'kgc-sidecar'
      );
      const container = deployment.spec.template.spec.containers[0];

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
      testResults.passed++;
    });

    it('should verify secret references', async () => {
      console.log('🔐 Verifying secret references...');

      const deployment = await kubernetesManager.getResourceStatus(
        testNamespace,
        'deployment',
        'kgc-sidecar'
      );
      const container = deployment.spec.template.spec.containers[0];

      const secretRefs = container.env.filter(env => env.valueFrom?.secretKeyRef);
      expect(secretRefs).toHaveLength(3);

      const secretNames = secretRefs.map(env => env.valueFrom.secretKeyRef.name);
      expect(secretNames).toContain('kgc-sidecar-secrets');

      console.log('✅ Secret references are correct');
      testResults.passed++;
    });

    it('should verify volume mounts', async () => {
      console.log('💾 Verifying volume mounts...');

      const deployment = await kubernetesManager.getResourceStatus(
        testNamespace,
        'deployment',
        'kgc-sidecar'
      );
      const container = deployment.spec.template.spec.containers[0];

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
      testResults.passed++;
    });
  });

  describe('E2E Functionality Tests', () => {
    it('should test health checks', async () => {
      console.log('🏥 Testing health checks...');

      // Port forward to access the service
      const portForward = await kubernetesManager.portForward(
        testNamespace,
        'kgc-sidecar-service',
        3000,
        3000
      );

      try {
        // Wait for port forward to be ready
        await kubernetesManager.sleep(5000);

        // Test health endpoint
        const healthResponse = await fetch('http://localhost:3000/health');
        expect(healthResponse.status).toBe(200);

        const healthData = await healthResponse.json();
        expect(healthData.status).toBe('healthy');

        console.log('✅ Health checks are working');
        testResults.passed++;
      } catch (error) {
        console.error('❌ Health check failed:', error.message);
        testResults.failed++;
        throw error;
      } finally {
        // Clean up port forward
        try {
          process.kill(portForward.pid);
        } catch (error) {
          // Ignore cleanup errors
        }
      }
    });

    it('should test readiness checks', async () => {
      console.log('✅ Testing readiness checks...');

      // Port forward to access the service
      const portForward = await kubernetesManager.portForward(
        testNamespace,
        'kgc-sidecar-service',
        3000,
        3000
      );

      try {
        // Wait for port forward to be ready
        await kubernetesManager.sleep(5000);

        // Test ready endpoint
        const readyResponse = await fetch('http://localhost:3000/ready');
        expect(readyResponse.status).toBe(200);

        const readyData = await readyResponse.json();
        expect(readyData.status).toBe('ready');

        console.log('✅ Readiness checks are working');
        testResults.passed++;
      } catch (error) {
        console.error('❌ Readiness check failed:', error.message);
        testResults.failed++;
        throw error;
      } finally {
        // Clean up port forward
        try {
          process.kill(portForward.pid);
        } catch (error) {
          // Ignore cleanup errors
        }
      }
    });

    it('should test metrics endpoint', async () => {
      console.log('📊 Testing metrics endpoint...');

      // Port forward to access the service
      const portForward = await kubernetesManager.portForward(
        testNamespace,
        'kgc-sidecar-service',
        8080,
        8080
      );

      try {
        // Wait for port forward to be ready
        await kubernetesManager.sleep(5000);

        // Test metrics endpoint
        const metricsResponse = await fetch('http://localhost:8080/metrics');
        expect(metricsResponse.status).toBe(200);

        const metricsData = await metricsResponse.text();
        expect(metricsData).toContain('kgc_');

        console.log('✅ Metrics endpoint is working');
        testResults.passed++;
      } catch (error) {
        console.error('❌ Metrics endpoint failed:', error.message);
        testResults.failed++;
        throw error;
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
    it('should verify HPA configuration', async () => {
      console.log('📈 Verifying HPA configuration...');

      const hpa = await kubernetesManager.getResourceStatus(
        testNamespace,
        'hpa',
        'kgc-sidecar-hpa'
      );

      expect(hpa.spec.minReplicas).toBe(1);
      expect(hpa.spec.maxReplicas).toBe(10);

      // Check HPA status
      expect(hpa.status.currentReplicas).toBeDefined();
      expect(hpa.status.desiredReplicas).toBeDefined();

      console.log('✅ HPA is configured correctly');
      testResults.passed++;
    });

    it('should verify PDB configuration', async () => {
      console.log('🛡️ Verifying PDB configuration...');

      const pdb = await kubernetesManager.getResourceStatus(
        testNamespace,
        'pdb',
        'kgc-sidecar-pdb'
      );

      expect(pdb.spec.minAvailable).toBe(1);

      // Check PDB status
      expect(pdb.status.currentHealthy).toBeDefined();
      expect(pdb.status.desiredHealthy).toBeDefined();

      console.log('✅ PDB is configured correctly');
      testResults.passed++;
    });
  });

  describe('Security and Network Policies', () => {
    it('should verify network policies', async () => {
      console.log('🛡️ Verifying network policies...');

      const networkPolicy = await kubernetesManager.getResourceStatus(
        testNamespace,
        'networkpolicy',
        'kgc-sidecar-netpol'
      );

      expect(networkPolicy.spec.policyTypes).toContain('Ingress');
      expect(networkPolicy.spec.policyTypes).toContain('Egress');

      // Check ingress rules
      expect(networkPolicy.spec.ingress).toHaveLength(1);
      expect(networkPolicy.spec.ingress[0].from[0].podSelector.matchLabels.app).toBe('test-client');

      // Check egress rules
      expect(networkPolicy.spec.egress).toHaveLength(2);

      console.log('✅ Network policies are configured correctly');
      testResults.passed++;
    });

    it('should verify RBAC permissions', async () => {
      console.log('🔑 Verifying RBAC permissions...');

      const role = await kubernetesManager.getResourceStatus(
        testNamespace,
        'role',
        'kgc-sidecar-role'
      );
      const roleBinding = await kubernetesManager.getResourceStatus(
        testNamespace,
        'rolebinding',
        'kgc-sidecar-binding'
      );

      expect(role.rules).toHaveLength(2);
      expect(roleBinding.roleRef.name).toBe('kgc-sidecar-role');
      expect(roleBinding.subjects[0].name).toBe('kgc-sidecar-sa');

      console.log('✅ RBAC permissions are configured correctly');
      testResults.passed++;
    });
  });

  describe('Test Results Summary', () => {
    it('should display test results', () => {
      console.log('📊 E2E Test Results Summary:');
      console.log(
        `   Total Tests: ${testResults.passed + testResults.failed + testResults.skipped}`
      );
      console.log(`   Passed: ${testResults.passed}`);
      console.log(`   Failed: ${testResults.failed}`);
      console.log(`   Skipped: ${testResults.skipped}`);
      console.log(
        `   Success Rate: ${((testResults.passed / (testResults.passed + testResults.failed)) * 100).toFixed(2)}%`
      );
      console.log(`   Duration: ${testResults.duration}ms`);

      expect(testResults.passed).toBeGreaterThan(0);
      expect(testResults.failed).toBe(0);
    });
  });
});

export default {
  testcontainersManager: null,
  terraformManager: null,
  kubernetesManager: null,
  testNamespace: null,
  testConfig: null,
  testResults: null,
};
