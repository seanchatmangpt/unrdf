output "prometheus_url" {
  description = "Prometheus server URL"
  value       = var.prometheus_enabled ? module.prometheus[0].prometheus_url : null
}

output "grafana_url" {
  description = "Grafana URL"
  value       = var.grafana_enabled ? module.grafana[0].grafana_url : null
}

output "jaeger_url" {
  description = "Jaeger UI URL"
  value       = var.jaeger_enabled ? module.jaeger[0].jaeger_url : null
}
