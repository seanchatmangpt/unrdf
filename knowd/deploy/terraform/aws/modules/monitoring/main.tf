module "prometheus" {
  count  = var.prometheus_enabled ? 1 : 0
  source = "./modules/prometheus"

  cluster_name     = var.cluster_name
  cluster_endpoint = var.cluster_endpoint
  cluster_ca_cert  = var.cluster_ca_cert

  namespace = var.monitoring_namespace

  tags = var.tags
}

module "jaeger" {
  count  = var.jaeger_enabled ? 1 : 0
  source = "./modules/jaeger"

  cluster_name     = var.cluster_name
  cluster_endpoint = var.cluster_endpoint
  cluster_ca_cert  = var.cluster_ca_cert

  namespace = var.monitoring_namespace

  tags = var.tags
}

module "grafana" {
  count  = var.grafana_enabled ? 1 : 0
  source = "./modules/grafana"

  cluster_name     = var.cluster_name
  cluster_endpoint = var.cluster_endpoint
  cluster_ca_cert  = var.cluster_ca_cert

  namespace = var.monitoring_namespace

  prometheus_url = var.prometheus_enabled ? module.prometheus[0].prometheus_url : ""

  tags = var.tags
}
