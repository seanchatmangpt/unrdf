# KGC Sidecar Terraform Outputs

output "namespace" {
  description = "Kubernetes namespace name"
  value       = kubernetes_namespace.kgc_sidecar.metadata[0].name
}

output "namespace_id" {
  description = "Kubernetes namespace ID"
  value       = kubernetes_namespace.kgc_sidecar.metadata[0].uid
}

output "deployment_name" {
  description = "Deployment name"
  value       = kubernetes_deployment.kgc_sidecar.metadata[0].name
}

output "deployment_id" {
  description = "Deployment ID"
  value       = kubernetes_deployment.kgc_sidecar.metadata[0].uid
}

output "service_name" {
  description = "Service name"
  value       = kubernetes_service.kgc_sidecar.metadata[0].name
}

output "service_id" {
  description = "Service ID"
  value       = kubernetes_service.kgc_sidecar.metadata[0].uid
}

output "service_port" {
  description = "Service HTTP port"
  value       = kubernetes_service.kgc_sidecar.spec[0].port[0].port
}

output "service_metrics_port" {
  description = "Service metrics port"
  value       = kubernetes_service.kgc_sidecar.spec[0].port[1].port
}

output "service_cluster_ip" {
  description = "Service cluster IP"
  value       = kubernetes_service.kgc_sidecar.spec[0].cluster_ip
}

output "config_map_name" {
  description = "ConfigMap name"
  value       = kubernetes_config_map.kgc_sidecar_config.metadata[0].name
}

output "config_map_id" {
  description = "ConfigMap ID"
  value       = kubernetes_config_map.kgc_sidecar_config.metadata[0].uid
}

output "secret_name" {
  description = "Secret name"
  value       = kubernetes_secret.kgc_sidecar_secrets.metadata[0].name
}

output "secret_id" {
  description = "Secret ID"
  value       = kubernetes_secret.kgc_sidecar_secrets.metadata[0].uid
}

output "service_account_name" {
  description = "Service account name"
  value       = kubernetes_service_account.kgc_sidecar.metadata[0].name
}

output "service_account_id" {
  description = "Service account ID"
  value       = kubernetes_service_account.kgc_sidecar.metadata[0].uid
}

output "role_name" {
  description = "Role name"
  value       = kubernetes_role.kgc_sidecar.metadata[0].name
}

output "role_binding_name" {
  description = "Role binding name"
  value       = kubernetes_role_binding.kgc_sidecar.metadata[0].name
}

output "ingress_name" {
  description = "Ingress name"
  value       = kubernetes_ingress_v1.kgc_sidecar.metadata[0].name
}

output "ingress_host" {
  description = "Ingress host"
  value       = "kgc-sidecar-${random_id.suffix.hex}.local"
}

output "ingress_url" {
  description = "Ingress URL"
  value       = "http://kgc-sidecar-${random_id.suffix.hex}.local"
}

output "hpa_name" {
  description = "Horizontal Pod Autoscaler name"
  value       = kubernetes_horizontal_pod_autoscaler_v2.kgc_sidecar.metadata[0].name
}

output "hpa_min_replicas" {
  description = "HPA minimum replicas"
  value       = kubernetes_horizontal_pod_autoscaler_v2.kgc_sidecar.spec[0].min_replicas
}

output "hpa_max_replicas" {
  description = "HPA maximum replicas"
  value       = kubernetes_horizontal_pod_autoscaler_v2.kgc_sidecar.spec[0].max_replicas
}

output "network_policy_name" {
  description = "Network policy name"
  value       = kubernetes_network_policy.kgc_sidecar.metadata[0].name
}

output "pdb_name" {
  description = "Pod Disruption Budget name"
  value       = kubernetes_pod_disruption_budget_v1.kgc_sidecar.metadata[0].name
}

output "pdb_min_available" {
  description = "PDB minimum available pods"
  value       = kubernetes_pod_disruption_budget_v1.kgc_sidecar.spec[0].min_available
}

output "replicas" {
  description = "Number of replicas"
  value       = kubernetes_deployment.kgc_sidecar.spec[0].replicas
}

output "image_tag" {
  description = "Docker image tag"
  value       = var.image_tag
}

output "environment" {
  description = "Environment name"
  value       = var.environment
}

output "random_suffix" {
  description = "Random suffix for unique resource names"
  value       = random_id.suffix.hex
}

output "resource_labels" {
  description = "Common resource labels"
  value = {
    app     = "kgc-sidecar"
    env     = var.environment
    managed = "terraform"
    version = var.image_tag
  }
}

output "connection_info" {
  description = "Connection information for E2E testing"
  value = {
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
    service   = kubernetes_service.kgc_sidecar.metadata[0].name
    port      = kubernetes_service.kgc_sidecar.spec[0].port[0].port
    host      = "kgc-sidecar-${random_id.suffix.hex}.local"
    url       = "http://kgc-sidecar-${random_id.suffix.hex}.local"
  }
}

output "health_check_url" {
  description = "Health check URL"
  value       = "http://kgc-sidecar-${random_id.suffix.hex}.local/health"
}

output "metrics_url" {
  description = "Metrics URL"
  value       = "http://kgc-sidecar-${random_id.suffix.hex}.local/metrics"
}

output "ready_check_url" {
  description = "Readiness check URL"
  value       = "http://kgc-sidecar-${random_id.suffix.hex}.local/ready"
}

output "kubectl_commands" {
  description = "Useful kubectl commands for debugging"
  value = {
    get_pods     = "kubectl get pods -n ${kubernetes_namespace.kgc_sidecar.metadata[0].name}"
    get_services = "kubectl get services -n ${kubernetes_namespace.kgc_sidecar.metadata[0].name}"
    get_ingress  = "kubectl get ingress -n ${kubernetes_namespace.kgc_sidecar.metadata[0].name}"
    get_hpa      = "kubectl get hpa -n ${kubernetes_namespace.kgc_sidecar.metadata[0].name}"
    logs         = "kubectl logs -n ${kubernetes_namespace.kgc_sidecar.metadata[0].name} -l app=kgc-sidecar"
    describe     = "kubectl describe deployment -n ${kubernetes_namespace.kgc_sidecar.metadata[0].name} kgc-sidecar"
    port_forward = "kubectl port-forward -n ${kubernetes_namespace.kgc_sidecar.metadata[0].name} service/kgc-sidecar-service 3000:3000"
  }
}

output "test_commands" {
  description = "Test commands for E2E validation"
  value = {
    health_check = "curl -f http://kgc-sidecar-${random_id.suffix.hex}.local/health"
    metrics      = "curl -f http://kgc-sidecar-${random_id.suffix.hex}.local/metrics"
    ready_check  = "curl -f http://kgc-sidecar-${random_id.suffix.hex}.local/ready"
    api_test     = "curl -X POST http://kgc-sidecar-${random_id.suffix.hex}.local/api/v1/transactions -H 'Content-Type: application/json' -d '{\"delta\":{\"additions\":[],\"removals\":[]}}'"
  }
}

output "monitoring_info" {
  description = "Monitoring and observability information"
  value = {
    observability_endpoint = var.observability_endpoint
    metrics_port          = kubernetes_service.kgc_sidecar.spec[0].port[1].port
    tracing_enabled       = var.enable_tracing
    metrics_enabled       = var.enable_metrics
    sampling_ratio        = var.sampling_ratio
  }
}

output "security_info" {
  description = "Security configuration information"
  value = {
    network_policy_enabled = var.enable_network_policy
    service_account        = kubernetes_service_account.kgc_sidecar.metadata[0].name
    role                   = kubernetes_role.kgc_sidecar.metadata[0].name
    role_binding           = kubernetes_role_binding.kgc_sidecar.metadata[0].name
    sandboxing_enabled     = var.enable_sandboxing
  }
}

output "scaling_info" {
  description = "Scaling configuration information"
  value = {
    hpa_enabled     = var.enable_hpa
    min_replicas    = var.hpa_min_replicas
    max_replicas    = var.hpa_max_replicas
    cpu_target      = var.hpa_cpu_target
    memory_target   = var.hpa_memory_target
    pdb_enabled     = var.enable_pdb
    pdb_min_available = var.pdb_min_available
  }
}

output "performance_info" {
  description = "Performance configuration information"
  value = {
    max_hooks           = var.max_hooks
    timeout_ms          = var.timeout_ms
    cache_size          = var.cache_size
    batch_size          = var.batch_size
    max_concurrency     = var.max_concurrency
    fast_path_enabled   = var.enable_fast_path
    caching_enabled     = var.enable_caching
    batch_processing    = var.enable_batch_processing
  }
}

output "feature_flags" {
  description = "Feature flags and toggles"
  value = {
    observability_enabled = var.enable_observability
    lockchain_enabled     = var.enable_lockchain
    resolution_enabled    = var.enable_resolution
    sandboxing_enabled    = var.enable_sandboxing
    ingress_enabled       = var.enable_ingress
    hpa_enabled          = var.enable_hpa
    network_policy_enabled = var.enable_network_policy
    pdb_enabled          = var.enable_pdb
  }
}
