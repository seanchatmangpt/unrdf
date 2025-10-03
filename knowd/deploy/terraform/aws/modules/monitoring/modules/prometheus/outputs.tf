output "prometheus_url" {
  description = "Prometheus server URL"
  value       = "http://${helm_release.prometheus.metadata[0].name}-server.${var.namespace}.svc.cluster.local:9090"
}
