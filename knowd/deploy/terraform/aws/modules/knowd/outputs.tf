output "service_name" {
  description = "knowd service name"
  value       = helm_release.knowd.name
}

output "service_namespace" {
  description = "knowd service namespace"
  value       = kubernetes_namespace.knowd.metadata[0].name
}

output "service_load_balancer_hostname" {
  description = "Load balancer hostname for knowd service"
  value       = kubernetes_service.knowd.status[0].load_balancer[0].ingress[0].hostname
}

output "service_port" {
  description = "knowd service port"
  value       = 80
}

output "helm_release_name" {
  description = "Helm release name"
  value       = helm_release.knowd.name
}

output "helm_release_namespace" {
  description = "Helm release namespace"
  value       = helm_release.knowd.namespace
}

# Note: The service resource reference is commented out as it would require
# data sources or additional configuration. In a real deployment, you would
# use a kubernetes_service data source or resource to get the service details.
