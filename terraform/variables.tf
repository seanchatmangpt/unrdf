# KGC Sidecar Terraform Variables

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
  description = "Environment name (e2e-test, staging, production)"
  type        = string
  default     = "e2e-test"
}

variable "image_tag" {
  description = "Docker image tag for KGC sidecar"
  type        = string
  default     = "latest"
}

variable "replicas" {
  description = "Number of replicas for the deployment"
  type        = number
  default     = 3
}

variable "resources" {
  description = "Resource requirements for containers"
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

variable "enable_ingress" {
  description = "Enable ingress for external access"
  type        = bool
  default     = true
}

variable "enable_hpa" {
  description = "Enable Horizontal Pod Autoscaler"
  type        = bool
  default     = true
}

variable "enable_network_policy" {
  description = "Enable network policies"
  type        = bool
  default     = true
}

variable "enable_pdb" {
  description = "Enable Pod Disruption Budget"
  type        = bool
  default     = true
}

variable "observability_endpoint" {
  description = "OpenTelemetry endpoint URL"
  type        = string
  default     = "http://jaeger:14268/api/traces"
}

variable "database_url" {
  description = "Database connection URL"
  type        = string
  default     = "postgresql://test:test@postgres:5432/kgc_test"
}

# Vault configuration variables
variable "vault_address" {
  description = "HashiCorp Vault server address"
  type        = string
  default     = "http://vault:8200"
}

variable "vault_token" {
  description = "Vault authentication token (use root token for dev, AppRole for prod)"
  type        = string
  sensitive   = true
  default     = ""
}

variable "vault_api_key" {
  description = "Pre-generated API key to store in Vault (auto-generated if empty)"
  type        = string
  sensitive   = true
  default     = ""
}

variable "vault_encryption_key" {
  description = "Pre-generated encryption key to store in Vault (auto-generated if empty)"
  type        = string
  sensitive   = true
  default     = ""
}

variable "vault_db_username" {
  description = "Database username to store in Vault"
  type        = string
  default     = ""
}

variable "vault_db_password" {
  description = "Database password to store in Vault (auto-generated if empty)"
  type        = string
  sensitive   = true
  default     = ""
}

variable "enable_vault_audit" {
  description = "Enable Vault audit logging for secret access"
  type        = bool
  default     = true
}

variable "enable_auto_rotation" {
  description = "Enable automatic secret rotation (30 days)"
  type        = bool
  default     = true
}

variable "vault_quorum_shares" {
  description = "Number of Shamir secret shares for Vault unsealing"
  type        = number
  default     = 5
  validation {
    condition     = var.vault_quorum_shares >= 3 && var.vault_quorum_shares <= 10
    error_message = "Quorum shares must be between 3 and 10."
  }
}

variable "vault_quorum_threshold" {
  description = "Number of shares required to unseal Vault (3-of-5 quorum)"
  type        = number
  default     = 3
  validation {
    condition     = var.vault_quorum_threshold >= 2 && var.vault_quorum_threshold <= var.vault_quorum_shares
    error_message = "Quorum threshold must be between 2 and total shares."
  }
}

variable "log_level" {
  description = "Log level (error, warn, info, debug)"
  type        = string
  default     = "info"
  validation {
    condition     = contains(["error", "warn", "info", "debug"], var.log_level)
    error_message = "Log level must be one of: error, warn, info, debug."
  }
}

variable "enable_observability" {
  description = "Enable observability features"
  type        = bool
  default     = true
}

variable "enable_metrics" {
  description = "Enable metrics collection"
  type        = bool
  default     = true
}

variable "enable_tracing" {
  description = "Enable distributed tracing"
  type        = bool
  default     = true
}

variable "sampling_ratio" {
  description = "Tracing sampling ratio (0.0 to 1.0)"
  type        = number
  default     = 1.0
  validation {
    condition     = var.sampling_ratio >= 0.0 && var.sampling_ratio <= 1.0
    error_message = "Sampling ratio must be between 0.0 and 1.0."
  }
}

variable "max_hooks" {
  description = "Maximum number of hooks"
  type        = number
  default     = 10000
  validation {
    condition     = var.max_hooks > 0 && var.max_hooks <= 100000
    error_message = "Max hooks must be between 1 and 100000."
  }
}

variable "timeout_ms" {
  description = "Default timeout in milliseconds"
  type        = number
  default     = 2000
  validation {
    condition     = var.timeout_ms > 0 && var.timeout_ms <= 300000
    error_message = "Timeout must be between 1 and 300000 milliseconds."
  }
}

variable "cache_size" {
  description = "Cache size for performance optimization"
  type        = number
  default     = 10000
  validation {
    condition     = var.cache_size > 0 && var.cache_size <= 1000000
    error_message = "Cache size must be between 1 and 1000000."
  }
}

variable "batch_size" {
  description = "Batch size for processing"
  type        = number
  default     = 1000
  validation {
    condition     = var.batch_size > 0 && var.batch_size <= 10000
    error_message = "Batch size must be between 1 and 10000."
  }
}

variable "max_concurrency" {
  description = "Maximum concurrency for processing"
  type        = number
  default     = 10
  validation {
    condition     = var.max_concurrency > 0 && var.max_concurrency <= 100
    error_message = "Max concurrency must be between 1 and 100."
  }
}

variable "enable_fast_path" {
  description = "Enable fast path optimization"
  type        = bool
  default     = true
}

variable "enable_caching" {
  description = "Enable caching"
  type        = bool
  default     = true
}

variable "enable_batch_processing" {
  description = "Enable batch processing"
  type        = bool
  default     = true
}

variable "enable_sandboxing" {
  description = "Enable effect sandboxing"
  type        = bool
  default     = true
}

variable "sandbox_timeout" {
  description = "Sandbox timeout in milliseconds"
  type        = number
  default     = 30000
  validation {
    condition     = var.sandbox_timeout > 0 && var.sandbox_timeout <= 300000
    error_message = "Sandbox timeout must be between 1 and 300000 milliseconds."
  }
}

variable "sandbox_memory_limit" {
  description = "Sandbox memory limit in bytes"
  type        = number
  default     = 67108864 # 64MB
  validation {
    condition     = var.sandbox_memory_limit > 0 && var.sandbox_memory_limit <= 1073741824
    error_message = "Sandbox memory limit must be between 1 and 1GB."
  }
}

variable "enable_lockchain" {
  description = "Enable lockchain audit trail"
  type        = bool
  default     = false
}

variable "enable_resolution" {
  description = "Enable multi-agent resolution"
  type        = bool
  default     = false
}

variable "resolution_strategy" {
  description = "Resolution strategy (voting, merging, priority, random, crdt)"
  type        = string
  default     = "voting"
  validation {
    condition     = contains(["voting", "merging", "priority", "random", "crdt"], var.resolution_strategy)
    error_message = "Resolution strategy must be one of: voting, merging, priority, random, crdt."
  }
}

variable "consensus_threshold" {
  description = "Consensus threshold for resolution (0.0 to 1.0)"
  type        = number
  default     = 0.6
  validation {
    condition     = var.consensus_threshold >= 0.0 && var.consensus_threshold <= 1.0
    error_message = "Consensus threshold must be between 0.0 and 1.0."
  }
}

variable "hpa_min_replicas" {
  description = "Minimum replicas for HPA"
  type        = number
  default     = 1
  validation {
    condition     = var.hpa_min_replicas >= 1
    error_message = "HPA min replicas must be at least 1."
  }
}

variable "hpa_max_replicas" {
  description = "Maximum replicas for HPA"
  type        = number
  default     = 10
  validation {
    condition     = var.hpa_max_replicas >= var.hpa_min_replicas
    error_message = "HPA max replicas must be greater than or equal to min replicas."
  }
}

variable "hpa_cpu_target" {
  description = "CPU target utilization for HPA"
  type        = number
  default     = 70
  validation {
    condition     = var.hpa_cpu_target > 0 && var.hpa_cpu_target <= 100
    error_message = "HPA CPU target must be between 1 and 100."
  }
}

variable "hpa_memory_target" {
  description = "Memory target utilization for HPA"
  type        = number
  default     = 80
  validation {
    condition     = var.hpa_memory_target > 0 && var.hpa_memory_target <= 100
    error_message = "HPA memory target must be between 1 and 100."
  }
}

variable "pdb_min_available" {
  description = "Minimum available pods for PDB"
  type        = number
  default     = 1
  validation {
    condition     = var.pdb_min_available >= 0
    error_message = "PDB min available must be non-negative."
  }
}

variable "ingress_class" {
  description = "Ingress class name"
  type        = string
  default     = "nginx"
}

variable "ingress_host" {
  description = "Ingress host name"
  type        = string
  default     = ""
}

variable "ingress_tls_enabled" {
  description = "Enable TLS for ingress"
  type        = bool
  default     = false
}

variable "ingress_tls_secret" {
  description = "TLS secret name for ingress"
  type        = string
  default     = ""
}

variable "node_selector" {
  description = "Node selector for pod placement"
  type        = map(string)
  default     = {}
}

variable "tolerations" {
  description = "Tolerations for pod placement"
  type = list(object({
    key      = string
    operator = string
    value    = optional(string)
    effect   = string
  }))
  default = []
}

variable "affinity" {
  description = "Affinity rules for pod placement"
  type = object({
    node_affinity = optional(object({
      required_during_scheduling_ignored_during_execution = optional(object({
        node_selector_terms = list(object({
          match_expressions = list(object({
            key      = string
            operator = string
            values   = optional(list(string))
          }))
        }))
      }))
    }))
    pod_affinity = optional(object({
      preferred_during_scheduling_ignored_during_execution = optional(list(object({
        weight = number
        pod_affinity_term = object({
          label_selector = object({
            match_expressions = list(object({
              key      = string
              operator = string
              values   = optional(list(string))
            }))
          })
          topology_key = string
        })
      })))
    }))
    pod_anti_affinity = optional(object({
      preferred_during_scheduling_ignored_during_execution = optional(list(object({
        weight = number
        pod_affinity_term = object({
          label_selector = object({
            match_expressions = list(object({
              key      = string
              operator = string
              values   = optional(list(string))
            }))
          })
          topology_key = string
        })
      })))
    }))
  })
  default = {}
}

variable "labels" {
  description = "Additional labels for resources"
  type        = map(string)
  default     = {}
}

variable "annotations" {
  description = "Additional annotations for resources"
  type        = map(string)
  default     = {}
}




