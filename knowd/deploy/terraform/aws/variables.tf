variable "region" {
  description = "AWS region"
  type        = string
  default     = "us-west-2"
}

variable "project_name" {
  description = "Name of the project"
  type        = string
  default     = "knowd"
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "dev"
  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be one of: dev, staging, prod."
  }
}

variable "vpc_cidr" {
  description = "VPC CIDR block"
  type        = string
  default     = "10.0.0.0/16"
}

variable "public_subnets" {
  description = "Public subnet CIDR blocks"
  type        = list(string)
  default     = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
}

variable "private_subnets" {
  description = "Private subnet CIDR blocks"
  type        = list(string)
  default     = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]
}

variable "kubernetes_version" {
  description = "Kubernetes version"
  type        = string
  default     = "1.28"
}

variable "node_instance_types" {
  description = "EC2 instance types for EKS node group"
  type        = list(string)
  default     = ["t3.medium"]
}

variable "node_desired_size" {
  description = "Desired number of nodes in the node group"
  type        = number
  default     = 3
}

variable "node_min_size" {
  description = "Minimum number of nodes in the node group"
  type        = number
  default     = 2
}

variable "node_max_size" {
  description = "Maximum number of nodes in the node group"
  type        = number
  default     = 6
}

variable "enable_rds" {
  description = "Enable RDS PostgreSQL for persistence"
  type        = bool
  default     = false
}

variable "postgres_version" {
  description = "PostgreSQL version for RDS"
  type        = string
  default     = "15.4"
}

variable "rds_instance_class" {
  description = "RDS instance class"
  type        = string
  default     = "db.t3.micro"
}

variable "rds_allocated_storage" {
  description = "RDS allocated storage in GB"
  type        = number
  default     = 20
}

variable "rds_max_allocated_storage" {
  description = "RDS maximum allocated storage in GB"
  type        = number
  default     = 100
}

variable "db_name" {
  description = "Database name"
  type        = string
  default     = "knowd"
}

variable "db_username" {
  description = "Database username"
  type        = string
  default     = "knowd"
  sensitive   = true
}

variable "db_password" {
  description = "Database password"
  type        = string
  sensitive   = true
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

variable "knowd_namespace" {
  description = "Kubernetes namespace for knowd"
  type        = string
  default     = "knowd"
}

variable "knowd_image_tag" {
  description = "knowd Docker image tag"
  type        = string
  default     = "latest"
}

variable "knowd_version" {
  description = "knowd version for telemetry"
  type        = string
  default     = "1.0.0"
}

variable "knowd_replica_count" {
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

variable "vector_enabled" {
  description = "Enable vector search functionality"
  type        = bool
  default     = true
}

variable "vector_dimensions" {
  description = "Vector dimensions for similarity search"
  type        = number
  default     = 384
}

variable "wasm_enabled" {
  description = "Enable WASM execution for hooks"
  type        = bool
  default     = true
}

variable "tags" {
  description = "Tags to apply to all resources"
  type        = map(string)
  default = {
    Environment = "dev"
    Project     = "knowd"
    ManagedBy   = "terraform"
  }
}
