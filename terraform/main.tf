# KGC Sidecar Kubernetes Infrastructure
# Terraform configuration for E2E testing with Testcontainers

terraform {
  required_version = ">= 1.0"
  required_providers {
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.23"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.11"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.5"
    }
  }
}

# Configure Kubernetes provider
provider "kubernetes" {
  config_path = var.kubeconfig_path
}

provider "helm" {
  kubernetes {
    config_path = var.kubeconfig_path
  }
}

# Variables
variable "kubeconfig_path" {
  description = "Path to kubeconfig file"
  type        = string
  default     = "~/.kube/config"
}

variable "namespace" {
  description = "Kubernetes namespace for KGC sidecar"
  type        = string
  default     = "kgc-sidecar"
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "e2e-test"
}

variable "image_tag" {
  description = "Docker image tag for KGC sidecar"
  type        = string
  default     = "latest"
}

variable "replicas" {
  description = "Number of replicas"
  type        = number
  default     = 3
}

variable "resources" {
  description = "Resource requirements"
  type = object({
    requests = object({
      cpu    = string
      memory = string
    })
    limits = object({
      cpu    = string
      memory = string
    })
  })
  default = {
    requests = {
      cpu    = "250m"
      memory = "256Mi"
    }
    limits = {
      cpu    = "500m"
      memory = "512Mi"
    }
  }
}

# Random suffix for unique resource names
resource "random_id" "suffix" {
  byte_length = 4
}

# Namespace
resource "kubernetes_namespace" "kgc_sidecar" {
  metadata {
    name = "${var.namespace}-${random_id.suffix.hex}"
    labels = {
      app     = "kgc-sidecar"
      env     = var.environment
      managed = "terraform"
    }
  }
}

# ConfigMap for KGC sidecar configuration
resource "kubernetes_config_map" "kgc_sidecar_config" {
  metadata {
    name      = "kgc-sidecar-config"
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
  }

  data = {
    "config.mjs" = templatefile("${path.module}/config/config.mjs", {
      environment = var.environment
      namespace   = kubernetes_namespace.kgc_sidecar.metadata[0].name
    })
  }
}

# Secret for sensitive configuration
resource "kubernetes_secret" "kgc_sidecar_secrets" {
  metadata {
    name      = "kgc-sidecar-secrets"
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
  }

  type = "Opaque"

  data = {
    "api-key"         = base64encode("test-api-key-${random_id.suffix.hex}")
    "encryption-key"  = base64encode("test-encryption-key-${random_id.suffix.hex}")
    "database-url"    = base64encode("postgresql://test:test@postgres:5432/kgc_test")
  }
}

# Service Account
resource "kubernetes_service_account" "kgc_sidecar" {
  metadata {
    name      = "kgc-sidecar-sa"
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
  }
}

# Role for KGC sidecar
resource "kubernetes_role" "kgc_sidecar" {
  metadata {
    name      = "kgc-sidecar-role"
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
  }

  rule {
    api_groups = [""]
    resources  = ["configmaps", "secrets", "pods"]
    verbs      = ["get", "list", "watch"]
  }

  rule {
    api_groups = ["apps"]
    resources  = ["deployments", "replicasets"]
    verbs      = ["get", "list", "watch"]
  }
}

# RoleBinding
resource "kubernetes_role_binding" "kgc_sidecar" {
  metadata {
    name      = "kgc-sidecar-binding"
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
  }

  role_ref {
    api_group = "rbac.authorization.k8s.io"
    kind      = "Role"
    name      = kubernetes_role.kgc_sidecar.metadata[0].name
  }

  subject {
    kind      = "ServiceAccount"
    name      = kubernetes_service_account.kgc_sidecar.metadata[0].name
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
  }
}

# Deployment
resource "kubernetes_deployment" "kgc_sidecar" {
  metadata {
    name      = "kgc-sidecar"
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
    labels = {
      app     = "kgc-sidecar"
      env     = var.environment
      version = var.image_tag
    }
  }

  spec {
    replicas = var.replicas

    selector {
      match_labels = {
        app = "kgc-sidecar"
      }
    }

    template {
      metadata {
        labels = {
          app     = "kgc-sidecar"
          env     = var.environment
          version = var.image_tag
        }
      }

      spec {
        service_account_name = kubernetes_service_account.kgc_sidecar.metadata[0].name

        container {
          name  = "kgc-sidecar"
          image = "unrdf/kgc-sidecar:${var.image_tag}"

          port {
            container_port = 3000
            name          = "http"
          }

          port {
            container_port = 8080
            name          = "metrics"
          }

          env {
            name  = "NODE_ENV"
            value = "production"
          }

          env {
            name  = "LOG_LEVEL"
            value = "info"
          }

          env {
            name  = "ENABLE_OBSERVABILITY"
            value = "true"
          }

          env {
            name  = "OBSERVABILITY_ENDPOINT"
            value = "http://jaeger:14268/api/traces"
          }

          env {
            name  = "SERVICE_NAME"
            value = "kgc-sidecar"
          }

          env {
            name  = "SERVICE_VERSION"
            value = var.image_tag
          }

          env {
            name  = "NAMESPACE"
            value = kubernetes_namespace.kgc_sidecar.metadata[0].name
          }

          env {
            name  = "ENVIRONMENT"
            value = var.environment
          }

          env {
            name = "API_KEY"
            value_from {
              secret_key_ref {
                name = kubernetes_secret.kgc_sidecar_secrets.metadata[0].name
                key  = "api-key"
              }
            }
          }

          env {
            name = "ENCRYPTION_KEY"
            value_from {
              secret_key_ref {
                name = kubernetes_secret.kgc_sidecar_secrets.metadata[0].name
                key  = "encryption-key"
              }
            }
          }

          env {
            name = "DATABASE_URL"
            value_from {
              secret_key_ref {
                name = kubernetes_secret.kgc_sidecar_secrets.metadata[0].name
                key  = "database-url"
              }
            }
          }

          resources {
            requests = var.resources.requests
            limits   = var.resources.limits
          }

          volume_mount {
            name       = "config"
            mount_path = "/app/config"
            read_only  = true
          }

          volume_mount {
            name       = "data"
            mount_path = "/app/data"
          }

          liveness_probe {
            http_get {
              path = "/health"
              port = 3000
            }
            initial_delay_seconds = 30
            period_seconds        = 10
            timeout_seconds       = 5
            failure_threshold     = 3
          }

          readiness_probe {
            http_get {
              path = "/ready"
              port = 3000
            }
            initial_delay_seconds = 5
            period_seconds        = 5
            timeout_seconds       = 3
            failure_threshold     = 3
          }

          startup_probe {
            http_get {
              path = "/health"
              port = 3000
            }
            initial_delay_seconds = 10
            period_seconds        = 5
            timeout_seconds       = 3
            failure_threshold     = 10
          }
        }

        volume {
          name = "config"
          config_map {
            name = kubernetes_config_map.kgc_sidecar_config.metadata[0].name
          }
        }

        volume {
          name = "data"
          empty_dir {}
        }

        restart_policy = "Always"
      }
    }
  }
}

# Service
resource "kubernetes_service" "kgc_sidecar" {
  metadata {
    name      = "kgc-sidecar-service"
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
    labels = {
      app = "kgc-sidecar"
    }
  }

  spec {
    selector = {
      app = "kgc-sidecar"
    }

    port {
      name        = "http"
      port        = 3000
      target_port = 3000
      protocol    = "TCP"
    }

    port {
      name        = "metrics"
      port        = 8080
      target_port = 8080
      protocol    = "TCP"
    }

    type = "ClusterIP"
  }
}

# Ingress
resource "kubernetes_ingress_v1" "kgc_sidecar" {
  metadata {
    name      = "kgc-sidecar-ingress"
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
    annotations = {
      "nginx.ingress.kubernetes.io/rewrite-target" = "/"
      "nginx.ingress.kubernetes.io/ssl-redirect"   = "false"
    }
  }

  spec {
    ingress_class_name = "nginx"

    rule {
      host = "kgc-sidecar-${random_id.suffix.hex}.local"
      http {
        path {
          path      = "/"
          path_type = "Prefix"
          backend {
            service {
              name = kubernetes_service.kgc_sidecar.metadata[0].name
              port {
                number = 3000
              }
            }
          }
        }
      }
    }
  }
}

# Horizontal Pod Autoscaler
resource "kubernetes_horizontal_pod_autoscaler_v2" "kgc_sidecar" {
  metadata {
    name      = "kgc-sidecar-hpa"
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
  }

  spec {
    scale_target_ref {
      api_version = "apps/v1"
      kind        = "Deployment"
      name        = kubernetes_deployment.kgc_sidecar.metadata[0].name
    }

    min_replicas = 1
    max_replicas = 10

    metric {
      type = "Resource"
      resource {
        name = "cpu"
        target {
          type                = "Utilization"
          average_utilization = 70
        }
      }
    }

    metric {
      type = "Resource"
      resource {
        name = "memory"
        target {
          type                = "Utilization"
          average_utilization = 80
        }
      }
    }
  }
}

# Network Policy
resource "kubernetes_network_policy" "kgc_sidecar" {
  metadata {
    name      = "kgc-sidecar-netpol"
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
  }

  spec {
    pod_selector {
      match_labels = {
        app = "kgc-sidecar"
      }
    }

    policy_types = ["Ingress", "Egress"]

    ingress {
      from {
        pod_selector {
          match_labels = {
            app = "test-client"
          }
        }
      }
      ports {
        port     = "3000"
        protocol = "TCP"
      }
    }

    egress {
      to {
        pod_selector {
          match_labels = {
            app = "jaeger"
          }
        }
      }
      ports {
        port     = "14268"
        protocol = "TCP"
      }
    }

    egress {
      to {
        pod_selector {
          match_labels = {
            app = "postgres"
          }
        }
      }
      ports {
        port     = "5432"
        protocol = "TCP"
      }
    }
  }
}

# Pod Disruption Budget
resource "kubernetes_pod_disruption_budget_v1" "kgc_sidecar" {
  metadata {
    name      = "kgc-sidecar-pdb"
    namespace = kubernetes_namespace.kgc_sidecar.metadata[0].name
  }

  spec {
    min_available = 1

    selector {
      match_labels = {
        app = "kgc-sidecar"
      }
    }
  }
}

# Outputs
output "namespace" {
  description = "Kubernetes namespace"
  value       = kubernetes_namespace.kgc_sidecar.metadata[0].name
}

output "service_name" {
  description = "Service name"
  value       = kubernetes_service.kgc_sidecar.metadata[0].name
}

output "service_port" {
  description = "Service port"
  value       = kubernetes_service.kgc_sidecar.spec[0].port[0].port
}

output "ingress_host" {
  description = "Ingress host"
  value       = "kgc-sidecar-${random_id.suffix.hex}.local"
}

output "deployment_name" {
  description = "Deployment name"
  value       = kubernetes_deployment.kgc_sidecar.metadata[0].name
}

output "config_map_name" {
  description = "ConfigMap name"
  value       = kubernetes_config_map.kgc_sidecar_config.metadata[0].name
}

output "secret_name" {
  description = "Secret name"
  value       = kubernetes_secret.kgc_sidecar_secrets.metadata[0].name
}



