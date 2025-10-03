resource "kubernetes_namespace" "knowd" {
  metadata {
    name = var.namespace
    labels = {
      name = var.namespace
    }
  }
}

resource "helm_release" "knowd" {
  name       = "knowd"
  repository = "https://charts.unrdf.com"
  chart      = "knowd"
  version    = var.chart_version
  namespace  = kubernetes_namespace.knowd.metadata[0].name

  values = [
    yamlencode({
      replicaCount = var.replica_count

      image = {
        registry   = var.image_registry
        repository = var.image_repository
        tag        = var.knowd_image_tag
      }

      service = {
        type = "LoadBalancer"
        annotations = {
          "service.beta.kubernetes.io/aws-load-balancer-type" = "nlb"
        }
      }

      persistence = {
        enabled      = true
        storageClass = var.storage_class
        size         = var.storage_size
      }

      config = var.config

      observability = {
        prometheus = {
          enabled = var.prometheus_enabled
          serviceMonitor = {
            enabled = true
          }
        }

        jaeger = {
          enabled = var.jaeger_enabled
        }
      }

      resources = {
        limits = {
          cpu    = "1000m"
          memory = "1Gi"
        }
        requests = {
          cpu    = "500m"
          memory = "512Mi"
        }
      }

      autoscaling = {
        enabled                        = true
        minReplicas                    = 3
        maxReplicas                    = 10
        targetCPUUtilizationPercentage = 80
      }

      podDisruptionBudget = {
        enabled      = true
        minAvailable = 2
      }
    })
  ]

  depends_on = [
    kubernetes_namespace.knowd
  ]
}

# Service Monitor for Prometheus
resource "kubernetes_manifest" "service_monitor" {
  count = var.prometheus_enabled ? 1 : 0

  manifest = {
    apiVersion = "monitoring.coreos.com/v1"
    kind       = "ServiceMonitor"

    metadata = {
      name      = "knowd-servicemonitor"
      namespace = kubernetes_namespace.knowd.metadata[0].name
      labels = {
        "app.kubernetes.io/name" = "knowd"
      }
    }

    spec = {
      selector = {
        matchLabels = {
          "app.kubernetes.io/name" = "knowd"
        }
      }

      endpoints = [
        {
          port     = "http"
          path     = "/metrics"
          interval = "30s"
        }
      ]
    }
  }

  depends_on = [
    helm_release.knowd
  ]
}

# Horizontal Pod Autoscaler
resource "kubernetes_horizontal_pod_autoscaler_v2" "knowd" {
  metadata {
    name      = "knowd-hpa"
    namespace = kubernetes_namespace.knowd.metadata[0].name
  }

  spec {
    scale_target_ref {
      api_version = "apps/v1"
      kind        = "Deployment"
      name        = "knowd"
    }

    min_replicas = 3
    max_replicas = 10

    metric {
      type = "Resource"
      resource {
        name = "cpu"
        target {
          type                = "Utilization"
          average_utilization = 80
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

  depends_on = [
    helm_release.knowd
  ]
}

# Pod Disruption Budget
resource "kubernetes_pod_disruption_budget_v1" "knowd" {
  metadata {
    name      = "knowd-pdb"
    namespace = kubernetes_namespace.knowd.metadata[0].name
  }

  spec {
    min_available = 2

    selector {
      match_labels = {
        "app.kubernetes.io/name" = "knowd"
      }
    }
  }

  depends_on = [
    helm_release.knowd
  ]
}
