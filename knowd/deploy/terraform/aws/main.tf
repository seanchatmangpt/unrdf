terraform {
  required_version = ">= 1.0"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.0"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.0"
    }
  }
}

provider "aws" {
  region = var.region
}

provider "kubernetes" {
  host                   = data.aws_eks_cluster.cluster.endpoint
  cluster_ca_certificate = base64decode(data.aws_eks_cluster.cluster.certificate_authority[0].data)
  token                  = data.aws_eks_cluster_auth.cluster.token
}

provider "helm" {
  kubernetes {
    host                   = data.aws_eks_cluster.cluster.endpoint
    cluster_ca_certificate = base64decode(data.aws_eks_cluster.cluster.certificate_authority[0].data)
    token                  = data.aws_eks_cluster_auth.cluster.token
  }
}

module "vpc" {
  source = "./modules/vpc"

  name            = "${var.project_name}-vpc"
  cidr            = var.vpc_cidr
  azs             = data.aws_availability_zones.available.names
  public_subnets  = var.public_subnets
  private_subnets = var.private_subnets

  enable_nat_gateway = true
  single_nat_gateway = var.environment == "production" ? false : true

  tags = var.tags
}

module "eks" {
  source = "./modules/eks"

  cluster_name    = "${var.project_name}-cluster"
  cluster_version = var.kubernetes_version

  vpc_id     = module.vpc.vpc_id
  subnet_ids = module.vpc.private_subnets

  node_group_name           = "${var.project_name}-node-group"
  node_group_instance_types = var.node_instance_types
  node_group_desired_size   = var.node_desired_size
  node_group_min_size       = var.node_min_size
  node_group_max_size       = var.node_max_size

  tags = var.tags
}

module "rds" {
  source = "./modules/rds"
  count  = var.enable_rds ? 1 : 0

  identifier = "${var.project_name}-postgres"

  engine         = "postgres"
  engine_version = var.postgres_version
  instance_class = var.rds_instance_class

  allocated_storage     = var.rds_allocated_storage
  max_allocated_storage = var.rds_max_allocated_storage

  db_name  = var.db_name
  username = var.db_username
  password = var.db_password

  vpc_security_group_ids = [module.eks.cluster_security_group_id]
  subnet_ids             = module.vpc.private_subnets

  tags = var.tags
}

module "monitoring" {
  source = "./modules/monitoring"

  cluster_name     = module.eks.cluster_name
  cluster_endpoint = module.eks.cluster_endpoint
  cluster_ca_cert  = module.eks.cluster_certificate_authority_data

  prometheus_enabled = var.prometheus_enabled
  jaeger_enabled     = var.jaeger_enabled
  grafana_enabled    = var.grafana_enabled

  tags = var.tags
}

module "knowd" {
  source = "./modules/knowd"

  cluster_name     = module.eks.cluster_name
  cluster_endpoint = module.eks.cluster_endpoint
  cluster_ca_cert  = module.eks.cluster_certificate_authority_data

  namespace = var.knowd_namespace

  knowd_image_tag = var.knowd_image_tag
  replica_count   = var.knowd_replica_count

  storage_class = var.storage_class
  storage_size  = var.storage_size

  prometheus_enabled = var.prometheus_enabled
  jaeger_enabled     = var.jaeger_enabled

  config = {
    telemetry = {
      serviceName    = "${var.project_name}-knowd"
      serviceVersion = var.knowd_version
      environment    = var.environment
    }
    vector = {
      enabled    = var.vector_enabled
      dimensions = var.vector_dimensions
    }
    wasm = {
      enabled = var.wasm_enabled
    }
  }

  tags = var.tags
}

# Outputs
output "cluster_name" {
  description = "EKS cluster name"
  value       = module.eks.cluster_name
}

output "cluster_endpoint" {
  description = "EKS cluster endpoint"
  value       = module.eks.cluster_endpoint
  sensitive   = true
}

output "knowd_service_url" {
  description = "knowd service URL"
  value       = "http://${module.knowd.service_load_balancer_hostname}"
}

output "monitoring_urls" {
  description = "Monitoring service URLs"
  value = {
    prometheus = var.prometheus_enabled ? "http://${module.monitoring.prometheus_url}" : null
    grafana    = var.grafana_enabled ? "http://${module.monitoring.grafana_url}" : null
    jaeger     = var.jaeger_enabled ? "http://${module.monitoring.jaeger_url}" : null
  }
}
