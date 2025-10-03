resource "helm_release" "prometheus" {
  name       = "prometheus"
  repository = "https://prometheus-community.github.io/helm-charts"
  chart      = "prometheus"
  version    = "25.0.0"
  namespace  = var.namespace

  values = [
    file("${path.module}/values.yaml")
  ]
}
