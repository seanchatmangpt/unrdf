variable "cluster_name" {
  description = "EKS cluster name"
  type        = string
}

variable "cluster_endpoint" {
  description = "EKS cluster endpoint"
  type        = string
}

variable "cluster_ca_cert" {
  description = "EKS cluster CA certificate"
  type        = string
}

variable "monitoring_namespace" {
  description = "Namespace for monitoring components"
  type        = string
  default     = "monitoring"
}

variable "prometheus_enabled" {
  description = "Enable Prometheus monitoring"
  type        = bool
  default     = true
}

variable "jaeger_enabled" {
  description = "Enable Jaeger tracing"
  type        = bool
  default     = true
}

variable "grafana_enabled" {
  description = "Enable Grafana dashboards"
  type        = bool
  default     = true
}

variable "tags" {
  description = "Tags to apply to resources"
  type        = map(string)
  default     = {}
}
