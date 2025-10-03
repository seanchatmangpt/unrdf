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

variable "namespace" {
  description = "Kubernetes namespace for knowd"
  type        = string
  default     = "knowd"
}

variable "chart_version" {
  description = "Helm chart version for knowd"
  type        = string
  default     = "1.0.0"
}

variable "image_registry" {
  description = "Docker image registry"
  type        = string
  default     = "docker.io"
}

variable "image_repository" {
  description = "Docker image repository"
  type        = string
  default     = "knowd/knowd"
}

variable "knowd_image_tag" {
  description = "knowd Docker image tag"
  type        = string
  default     = "latest"
}

variable "replica_count" {
  description = "Number of knowd replicas"
  type        = number
  default     = 3
}

variable "storage_class" {
  description = "Storage class for persistent volumes"
  type        = string
  default     = "gp3"
}

variable "storage_size" {
  description = "Storage size for knowd data"
  type        = string
  default     = "10Gi"
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

variable "config" {
  description = "knowd configuration"
  type = object({
    telemetry = object({
      serviceName    = string
      serviceVersion = string
      environment    = string
    })
    vector = object({
      enabled    = bool
      dimensions = number
    })
    wasm = object({
      enabled = bool
    })
  })
}

variable "tags" {
  description = "Tags to apply to resources"
  type        = map(string)
  default     = {}
}
