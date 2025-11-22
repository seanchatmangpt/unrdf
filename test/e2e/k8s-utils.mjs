/**
 * @file Kubernetes Utilities for KGC Sidecar E2E Tests
 * @module k8s-utils
 *
 * @description
 * Utilities for managing Kubernetes resources in E2E tests
 * for the KGC sidecar deployment.
 */

import * as k8s from '@kubernetes/client-node';
import { execSync } from 'child_process';
import { randomUUID } from 'crypto';

/**
 * Kubernetes utilities for KGC sidecar E2E tests
 */
export class KubernetesManager {
  /**
   *
   */
  constructor(options = {}) {
    this.kubeConfig = new k8s.KubeConfig();
    this.kubeConfig.loadFromDefault();

    this.coreApi = this.kubeConfig.makeApiClient(k8s.CoreV1Api);
    this.appsApi = this.kubeConfig.makeApiClient(k8s.AppsV1Api);
    this.networkingApi = this.kubeConfig.makeApiClient(k8s.NetworkingV1Api);
    this.autoscalingApi = this.kubeConfig.makeApiClient(k8s.AutoscalingV2Api);
    this.rbacApi = this.kubeConfig.makeApiClient(k8s.RbacAuthorizationV1Api);

    this.options = {
      timeout: 300000, // 5 minutes
      retryInterval: 5000, // 5 seconds
      ...options,
    };
  }

  /**
   * Create namespace
   */
  async createNamespace(name, labels = {}, annotations = {}) {
    console.log(`📦 Creating namespace: ${name}`);

    const namespace = {
      metadata: {
        name,
        labels: {
          app: 'kgc-sidecar',
          env: 'e2e-test',
          managed: 'k8s-utils',
          ...labels,
        },
        annotations,
      },
    };

    try {
      const result = await this.coreApi.createNamespace(namespace);
      console.log(`✅ Namespace created: ${name}`);
      return result.body;
    } catch (error) {
      if (error.statusCode === 409) {
        console.log(`⚠️ Namespace already exists: ${name}`);
        return await this.coreApi.readNamespace(name);
      }
      throw error;
    }
  }

  /**
   * Delete namespace
   */
  async deleteNamespace(name) {
    console.log(`🗑️ Deleting namespace: ${name}`);

    try {
      await this.coreApi.deleteNamespace(name);
      console.log(`✅ Namespace deleted: ${name}`);
    } catch (error) {
      if (error.statusCode === 404) {
        console.log(`⚠️ Namespace not found: ${name}`);
        return;
      }
      throw error;
    }
  }

  /**
   * Wait for namespace to be deleted
   */
  async waitForNamespaceDeletion(name, timeout = 300000) {
    console.log(`⏳ Waiting for namespace deletion: ${name}`);

    const startTime = Date.now();

    while (Date.now() - startTime < timeout) {
      try {
        await this.coreApi.readNamespace(name);
        console.log(`⏳ Namespace still exists: ${name}`);
        await this.sleep(this.options.retryInterval);
      } catch (error) {
        if (error.statusCode === 404) {
          console.log(`✅ Namespace deleted: ${name}`);
          return;
        }
        throw error;
      }
    }

    throw new Error(`Namespace ${name} was not deleted within ${timeout}ms`);
  }

  /**
   * Create deployment
   */
  async createDeployment(namespace, deployment) {
    console.log(`🚀 Creating deployment: ${deployment.metadata.name}`);

    try {
      const result = await this.appsApi.createNamespacedDeployment(namespace, deployment);
      console.log(`✅ Deployment created: ${deployment.metadata.name}`);
      return result.body;
    } catch (error) {
      if (error.statusCode === 409) {
        console.log(`⚠️ Deployment already exists: ${deployment.metadata.name}`);
        return await this.appsApi.readNamespacedDeployment(deployment.metadata.name, namespace);
      }
      throw error;
    }
  }

  /**
   * Wait for deployment to be ready
   */
  async waitForDeploymentReady(namespace, name, timeout = 300000) {
    console.log(`⏳ Waiting for deployment to be ready: ${name}`);

    const startTime = Date.now();

    while (Date.now() - startTime < timeout) {
      try {
        const deployment = await this.appsApi.readNamespacedDeployment(name, namespace);

        if (deployment.body.status.readyReplicas === deployment.body.spec.replicas) {
          console.log(`✅ Deployment ready: ${name}`);
          return deployment.body;
        }

        console.log(
          `⏳ Deployment not ready: ${name} (${deployment.body.status.readyReplicas}/${deployment.body.spec.replicas})`
        );
        await this.sleep(this.options.retryInterval);
      } catch (error) {
        if (error.statusCode === 404) {
          console.log(`⏳ Deployment not found: ${name}`);
          await this.sleep(this.options.retryInterval);
          continue;
        }
        throw error;
      }
    }

    throw new Error(`Deployment ${name} did not become ready within ${timeout}ms`);
  }

  /**
   * Create service
   */
  async createService(namespace, service) {
    console.log(`🌐 Creating service: ${service.metadata.name}`);

    try {
      const result = await this.coreApi.createNamespacedService(namespace, service);
      console.log(`✅ Service created: ${service.metadata.name}`);
      return result.body;
    } catch (error) {
      if (error.statusCode === 409) {
        console.log(`⚠️ Service already exists: ${service.metadata.name}`);
        return await this.coreApi.readNamespacedService(service.metadata.name, namespace);
      }
      throw error;
    }
  }

  /**
   * Create configmap
   */
  async createConfigMap(namespace, configMap) {
    console.log(`⚙️ Creating ConfigMap: ${configMap.metadata.name}`);

    try {
      const result = await this.coreApi.createNamespacedConfigMap(namespace, configMap);
      console.log(`✅ ConfigMap created: ${configMap.metadata.name}`);
      return result.body;
    } catch (error) {
      if (error.statusCode === 409) {
        console.log(`⚠️ ConfigMap already exists: ${configMap.metadata.name}`);
        return await this.coreApi.readNamespacedConfigMap(configMap.metadata.name, namespace);
      }
      throw error;
    }
  }

  /**
   * Create secret
   */
  async createSecret(namespace, secret) {
    console.log(`🔐 Creating Secret: ${secret.metadata.name}`);

    try {
      const result = await this.coreApi.createNamespacedSecret(namespace, secret);
      console.log(`✅ Secret created: ${secret.metadata.name}`);
      return result.body;
    } catch (error) {
      if (error.statusCode === 409) {
        console.log(`⚠️ Secret already exists: ${secret.metadata.name}`);
        return await this.coreApi.readNamespacedSecret(secret.metadata.name, namespace);
      }
      throw error;
    }
  }

  /**
   * Create service account
   */
  async createServiceAccount(namespace, serviceAccount) {
    console.log(`👤 Creating ServiceAccount: ${serviceAccount.metadata.name}`);

    try {
      const result = await this.coreApi.createNamespacedServiceAccount(namespace, serviceAccount);
      console.log(`✅ ServiceAccount created: ${serviceAccount.metadata.name}`);
      return result.body;
    } catch (error) {
      if (error.statusCode === 409) {
        console.log(`⚠️ ServiceAccount already exists: ${serviceAccount.metadata.name}`);
        return await this.coreApi.readNamespacedServiceAccount(
          serviceAccount.metadata.name,
          namespace
        );
      }
      throw error;
    }
  }

  /**
   * Create role
   */
  async createRole(namespace, role) {
    console.log(`🔑 Creating Role: ${role.metadata.name}`);

    try {
      const result = await this.rbacApi.createNamespacedRole(namespace, role);
      console.log(`✅ Role created: ${role.metadata.name}`);
      return result.body;
    } catch (error) {
      if (error.statusCode === 409) {
        console.log(`⚠️ Role already exists: ${role.metadata.name}`);
        return await this.rbacApi.readNamespacedRole(role.metadata.name, namespace);
      }
      throw error;
    }
  }

  /**
   * Create role binding
   */
  async createRoleBinding(namespace, roleBinding) {
    console.log(`🔗 Creating RoleBinding: ${roleBinding.metadata.name}`);

    try {
      const result = await this.rbacApi.createNamespacedRoleBinding(namespace, roleBinding);
      console.log(`✅ RoleBinding created: ${roleBinding.metadata.name}`);
      return result.body;
    } catch (error) {
      if (error.statusCode === 409) {
        console.log(`⚠️ RoleBinding already exists: ${roleBinding.metadata.name}`);
        return await this.rbacApi.readNamespacedRoleBinding(roleBinding.metadata.name, namespace);
      }
      throw error;
    }
  }

  /**
   * Create ingress
   */
  async createIngress(namespace, ingress) {
    console.log(`🌍 Creating Ingress: ${ingress.metadata.name}`);

    try {
      const result = await this.networkingApi.createNamespacedIngress(namespace, ingress);
      console.log(`✅ Ingress created: ${ingress.metadata.name}`);
      return result.body;
    } catch (error) {
      if (error.statusCode === 409) {
        console.log(`⚠️ Ingress already exists: ${ingress.metadata.name}`);
        return await this.networkingApi.readNamespacedIngress(ingress.metadata.name, namespace);
      }
      throw error;
    }
  }

  /**
   * Create HPA
   */
  async createHPA(namespace, hpa) {
    console.log(`📈 Creating HPA: ${hpa.metadata.name}`);

    try {
      const result = await this.autoscalingApi.createNamespacedHorizontalPodAutoscaler(
        namespace,
        hpa
      );
      console.log(`✅ HPA created: ${hpa.metadata.name}`);
      return result.body;
    } catch (error) {
      if (error.statusCode === 409) {
        console.log(`⚠️ HPA already exists: ${hpa.metadata.name}`);
        return await this.autoscalingApi.readNamespacedHorizontalPodAutoscaler(
          hpa.metadata.name,
          namespace
        );
      }
      throw error;
    }
  }

  /**
   * Create network policy
   */
  async createNetworkPolicy(namespace, networkPolicy) {
    console.log(`🛡️ Creating NetworkPolicy: ${networkPolicy.metadata.name}`);

    try {
      const result = await this.networkingApi.createNamespacedNetworkPolicy(
        namespace,
        networkPolicy
      );
      console.log(`✅ NetworkPolicy created: ${networkPolicy.metadata.name}`);
      return result.body;
    } catch (error) {
      if (error.statusCode === 409) {
        console.log(`⚠️ NetworkPolicy already exists: ${networkPolicy.metadata.name}`);
        return await this.networkingApi.readNamespacedNetworkPolicy(
          networkPolicy.metadata.name,
          namespace
        );
      }
      throw error;
    }
  }

  /**
   * Create PDB
   */
  async createPDB(namespace, pdb) {
    console.log(`🛡️ Creating PDB: ${pdb.metadata.name}`);

    try {
      const result = await this.policyApi.createNamespacedPodDisruptionBudget(namespace, pdb);
      console.log(`✅ PDB created: ${pdb.metadata.name}`);
      return result.body;
    } catch (error) {
      if (error.statusCode === 409) {
        console.log(`⚠️ PDB already exists: ${pdb.metadata.name}`);
        return await this.policyApi.readNamespacedPodDisruptionBudget(pdb.metadata.name, namespace);
      }
      throw error;
    }
  }

  /**
   * Get pods by label selector
   */
  async getPodsByLabel(namespace, labelSelector) {
    console.log(`🔍 Getting pods by label: ${labelSelector}`);

    const pods = await this.coreApi.listNamespacedPod(
      namespace,
      undefined,
      undefined,
      undefined,
      undefined,
      labelSelector
    );
    console.log(`✅ Found ${pods.body.items.length} pods`);
    return pods.body.items;
  }

  /**
   * Wait for pods to be ready
   */
  async waitForPodsReady(namespace, labelSelector, timeout = 300000) {
    console.log(`⏳ Waiting for pods to be ready: ${labelSelector}`);

    const startTime = Date.now();

    while (Date.now() - startTime < timeout) {
      const pods = await this.getPodsByLabel(namespace, labelSelector);

      if (pods.length === 0) {
        console.log(`⏳ No pods found: ${labelSelector}`);
        await this.sleep(this.options.retryInterval);
        continue;
      }

      const readyPods = pods.filter(
        pod =>
          pod.status.phase === 'Running' &&
          pod.status.containerStatuses?.every(container => container.ready)
      );

      if (readyPods.length === pods.length) {
        console.log(`✅ All pods ready: ${labelSelector} (${readyPods.length}/${pods.length})`);
        return readyPods;
      }

      console.log(`⏳ Pods not ready: ${labelSelector} (${readyPods.length}/${pods.length})`);
      await this.sleep(this.options.retryInterval);
    }

    throw new Error(`Pods with label ${labelSelector} did not become ready within ${timeout}ms`);
  }

  /**
   * Get pod logs
   */
  async getPodLogs(namespace, podName, containerName = null, tailLines = 100) {
    console.log(`📋 Getting logs for pod: ${podName}`);

    const logs = await this.coreApi.readNamespacedPodLog(
      podName,
      namespace,
      containerName,
      false,
      undefined,
      undefined,
      false,
      undefined,
      tailLines
    );
    console.log(`✅ Retrieved logs for pod: ${podName}`);
    return logs.body;
  }

  /**
   * Port forward to service
   */
  async portForward(namespace, serviceName, localPort, remotePort) {
    console.log(`🔌 Port forwarding: ${serviceName}:${remotePort} -> localhost:${localPort}`);

    const command = `kubectl port-forward -n ${namespace} service/${serviceName} ${localPort}:${remotePort}`;
    const process = execSync(command, { stdio: 'pipe', detached: true });

    // Wait for port forward to be ready
    await this.sleep(5000);

    console.log(
      `✅ Port forward established: ${serviceName}:${remotePort} -> localhost:${localPort}`
    );
    return process;
  }

  /**
   * Execute command in pod
   */
  async execInPod(namespace, podName, containerName, command) {
    console.log(`💻 Executing command in pod: ${podName}`);

    const exec = new k8s.Exec(this.kubeConfig);
    const result = await exec.exec(
      namespace,
      podName,
      containerName,
      command,
      process.stdout,
      process.stderr,
      process.stdin,
      true
    );

    console.log(`✅ Command executed in pod: ${podName}`);
    return result;
  }

  /**
   * Get resource status
   */
  async getResourceStatus(namespace, resourceType, resourceName) {
    console.log(`📊 Getting status for ${resourceType}: ${resourceName}`);

    let result;

    switch (resourceType) {
      case 'deployment':
        result = await this.appsApi.readNamespacedDeployment(resourceName, namespace);
        break;
      case 'service':
        result = await this.coreApi.readNamespacedService(resourceName, namespace);
        break;
      case 'pod':
        result = await this.coreApi.readNamespacedPod(resourceName, namespace);
        break;
      case 'configmap':
        result = await this.coreApi.readNamespacedConfigMap(resourceName, namespace);
        break;
      case 'secret':
        result = await this.coreApi.readNamespacedSecret(resourceName, namespace);
        break;
      case 'ingress':
        result = await this.networkingApi.readNamespacedIngress(resourceName, namespace);
        break;
      case 'hpa':
        result = await this.autoscalingApi.readNamespacedHorizontalPodAutoscaler(
          resourceName,
          namespace
        );
        break;
      case 'networkpolicy':
        result = await this.networkingApi.readNamespacedNetworkPolicy(resourceName, namespace);
        break;
      default:
        throw new Error(`Unknown resource type: ${resourceType}`);
    }

    console.log(`✅ Status retrieved for ${resourceType}: ${resourceName}`);
    return result.body;
  }

  /**
   * List resources
   */
  async listResources(namespace, resourceType, labelSelector = null) {
    console.log(`📋 Listing ${resourceType} in namespace: ${namespace}`);

    let result;

    switch (resourceType) {
      case 'deployments':
        result = await this.appsApi.listNamespacedDeployment(
          namespace,
          undefined,
          undefined,
          undefined,
          undefined,
          labelSelector
        );
        break;
      case 'services':
        result = await this.coreApi.listNamespacedService(
          namespace,
          undefined,
          undefined,
          undefined,
          undefined,
          labelSelector
        );
        break;
      case 'pods':
        result = await this.coreApi.listNamespacedPod(
          namespace,
          undefined,
          undefined,
          undefined,
          undefined,
          labelSelector
        );
        break;
      case 'configmaps':
        result = await this.coreApi.listNamespacedConfigMap(
          namespace,
          undefined,
          undefined,
          undefined,
          undefined,
          labelSelector
        );
        break;
      case 'secrets':
        result = await this.coreApi.listNamespacedSecret(
          namespace,
          undefined,
          undefined,
          undefined,
          undefined,
          labelSelector
        );
        break;
      case 'ingresses':
        result = await this.networkingApi.listNamespacedIngress(
          namespace,
          undefined,
          undefined,
          undefined,
          undefined,
          labelSelector
        );
        break;
      case 'hpas':
        result = await this.autoscalingApi.listNamespacedHorizontalPodAutoscaler(
          namespace,
          undefined,
          undefined,
          undefined,
          undefined,
          labelSelector
        );
        break;
      case 'networkpolicies':
        result = await this.networkingApi.listNamespacedNetworkPolicy(
          namespace,
          undefined,
          undefined,
          undefined,
          undefined,
          labelSelector
        );
        break;
      default:
        throw new Error(`Unknown resource type: ${resourceType}`);
    }

    console.log(`✅ Listed ${result.body.items.length} ${resourceType}`);
    return result.body.items;
  }

  /**
   * Delete resource
   */
  async deleteResource(namespace, resourceType, resourceName) {
    console.log(`🗑️ Deleting ${resourceType}: ${resourceName}`);

    let result;

    switch (resourceType) {
      case 'deployment':
        result = await this.appsApi.deleteNamespacedDeployment(resourceName, namespace);
        break;
      case 'service':
        result = await this.coreApi.deleteNamespacedService(resourceName, namespace);
        break;
      case 'pod':
        result = await this.coreApi.deleteNamespacedPod(resourceName, namespace);
        break;
      case 'configmap':
        result = await this.coreApi.deleteNamespacedConfigMap(resourceName, namespace);
        break;
      case 'secret':
        result = await this.coreApi.deleteNamespacedSecret(resourceName, namespace);
        break;
      case 'ingress':
        result = await this.networkingApi.deleteNamespacedIngress(resourceName, namespace);
        break;
      case 'hpa':
        result = await this.autoscalingApi.deleteNamespacedHorizontalPodAutoscaler(
          resourceName,
          namespace
        );
        break;
      case 'networkpolicy':
        result = await this.networkingApi.deleteNamespacedNetworkPolicy(resourceName, namespace);
        break;
      default:
        throw new Error(`Unknown resource type: ${resourceType}`);
    }

    console.log(`✅ Deleted ${resourceType}: ${resourceName}`);
    return result.body;
  }

  /**
   * Cleanup namespace resources
   */
  async cleanupNamespace(namespace) {
    console.log(`🧹 Cleaning up namespace: ${namespace}`);

    try {
      // Delete all resources in the namespace
      const resourceTypes = [
        'deployments',
        'services',
        'pods',
        'configmaps',
        'secrets',
        'ingresses',
        'hpas',
        'networkpolicies',
      ];

      for (const resourceType of resourceTypes) {
        try {
          const resources = await this.listResources(namespace, resourceType);
          for (const resource of resources) {
            await this.deleteResource(namespace, resourceType.slice(0, -1), resource.metadata.name);
          }
        } catch (error) {
          console.warn(`⚠️ Failed to cleanup ${resourceType}:`, error.message);
        }
      }

      // Delete namespace
      await this.deleteNamespace(namespace);

      console.log(`✅ Namespace cleaned up: ${namespace}`);
    } catch (error) {
      console.error(`❌ Failed to cleanup namespace ${namespace}:`, error.message);
      throw error;
    }
  }

  /**
   * Sleep utility
   */
  async sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  /**
   * Get Kubernetes client
   */
  getKubeConfig() {
    return this.kubeConfig;
  }

  /**
   * Get API clients
   */
  getApiClients() {
    return {
      core: this.coreApi,
      apps: this.appsApi,
      networking: this.networkingApi,
      autoscaling: this.autoscalingApi,
      rbac: this.rbacApi,
    };
  }
}

/**
 * Create Kubernetes resource definitions for E2E tests
 */
export function createK8sResources(options = {}) {
  const {
    namespace = `kgc-e2e-test-${randomUUID().substring(0, 8)}`,
    imageTag = 'latest',
    replicas = 2,
    resources = {
      requests: { cpu: '100m', memory: '128Mi' },
      limits: { cpu: '200m', memory: '256Mi' },
    },
  } = options;

  return {
    namespace: {
      metadata: {
        name: namespace,
        labels: {
          app: 'kgc-sidecar',
          env: 'e2e-test',
          managed: 'k8s-utils',
        },
      },
    },

    deployment: {
      metadata: {
        name: 'kgc-sidecar',
        labels: {
          app: 'kgc-sidecar',
          env: 'e2e-test',
        },
      },
      spec: {
        replicas,
        selector: {
          matchLabels: {
            app: 'kgc-sidecar',
          },
        },
        template: {
          metadata: {
            labels: {
              app: 'kgc-sidecar',
              env: 'e2e-test',
            },
          },
          spec: {
            containers: [
              {
                name: 'kgc-sidecar',
                image: `unrdf/kgc-sidecar:${imageTag}`,
                ports: [
                  { containerPort: 3000, name: 'http' },
                  { containerPort: 8080, name: 'metrics' },
                ],
                env: [
                  { name: 'NODE_ENV', value: 'production' },
                  { name: 'LOG_LEVEL', value: 'info' },
                  { name: 'ENABLE_OBSERVABILITY', value: 'true' },
                  { name: 'SERVICE_NAME', value: 'kgc-sidecar' },
                  { name: 'NAMESPACE', value: namespace },
                  { name: 'ENVIRONMENT', value: 'e2e-test' },
                ],
                resources,
                livenessProbe: {
                  httpGet: {
                    path: '/health',
                    port: 3000,
                  },
                  initialDelaySeconds: 30,
                  periodSeconds: 10,
                },
                readinessProbe: {
                  httpGet: {
                    path: '/ready',
                    port: 3000,
                  },
                  initialDelaySeconds: 5,
                  periodSeconds: 5,
                },
              },
            ],
          },
        },
      },
    },

    service: {
      metadata: {
        name: 'kgc-sidecar-service',
        labels: {
          app: 'kgc-sidecar',
        },
      },
      spec: {
        selector: {
          app: 'kgc-sidecar',
        },
        ports: [
          { name: 'http', port: 3000, targetPort: 3000 },
          { name: 'metrics', port: 8080, targetPort: 8080 },
        ],
        type: 'ClusterIP',
      },
    },
  };
}

/**
 * Create a new Kubernetes manager instance
 */
export function createKubernetesManager(options = {}) {
  return new KubernetesManager(options);
}

/**
 * Default Kubernetes manager instance
 */
export const kubernetesManager = createKubernetesManager();

export default KubernetesManager;
