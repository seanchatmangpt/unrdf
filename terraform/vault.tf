# HashiCorp Vault Configuration for KGC Sidecar
# Implements dynamic secrets management with automatic rotation

terraform {
  required_providers {
    vault = {
      source  = "hashicorp/vault"
      version = "~> 4.0"
    }
  }
}

provider "vault" {
  address = var.vault_address
  token   = var.vault_token
}

# Enable KV v2 secrets engine for KGC
resource "vault_mount" "kgc_secrets" {
  path        = "kgc"
  type        = "kv-v2"
  description = "KGC Sidecar secrets storage"

  options = {
    max_versions = 10
  }
}

# API Key secret with automatic rotation
resource "vault_kv_secret_v2" "api_key" {
  mount = vault_mount.kgc_secrets.path
  name  = "api-credentials"

  data_json = jsonencode({
    api_key    = var.vault_api_key != "" ? var.vault_api_key : random_password.api_key[0].result
    created_at = timestamp()
    rotated_at = timestamp()
  })

  custom_metadata {
    max_versions = 10
    data = {
      environment     = var.environment
      rotation_days   = "30"
      quorum_required = "3"
    }
  }
}

# Encryption key secret with automatic rotation
resource "vault_kv_secret_v2" "encryption_key" {
  mount = vault_mount.kgc_secrets.path
  name  = "encryption-credentials"

  data_json = jsonencode({
    encryption_key = var.vault_encryption_key != "" ? var.vault_encryption_key : random_password.encryption_key[0].result
    algorithm      = "AES-256-GCM"
    created_at     = timestamp()
    rotated_at     = timestamp()
  })

  custom_metadata {
    max_versions = 10
    data = {
      environment     = var.environment
      rotation_days   = "30"
      quorum_required = "3"
    }
  }
}

# Database credentials with dynamic generation
resource "vault_kv_secret_v2" "database" {
  mount = vault_mount.kgc_secrets.path
  name  = "database-credentials"

  data_json = jsonencode({
    url        = var.database_url
    username   = var.vault_db_username != "" ? var.vault_db_username : "kgc_user"
    password   = var.vault_db_password != "" ? var.vault_db_password : random_password.db_password[0].result
    created_at = timestamp()
  })

  custom_metadata {
    max_versions = 5
    data = {
      environment = var.environment
      ttl         = "86400" # 24 hours
    }
  }
}

# Generate random passwords if not provided
resource "random_password" "api_key" {
  count   = var.vault_api_key == "" ? 1 : 0
  length  = 64
  special = true
}

resource "random_password" "encryption_key" {
  count   = var.vault_encryption_key == "" ? 1 : 0
  length  = 64
  special = true
}

resource "random_password" "db_password" {
  count   = var.vault_db_password == "" ? 1 : 0
  length  = 32
  special = true
}

# Policy for KGC application access
resource "vault_policy" "kgc_read" {
  name = "kgc-read-${var.environment}"

  policy = <<EOT
# Allow reading KGC secrets
path "kgc/data/*" {
  capabilities = ["read", "list"]
}

# Allow reading metadata
path "kgc/metadata/*" {
  capabilities = ["read", "list"]
}

# Allow token renewal
path "auth/token/renew-self" {
  capabilities = ["update"]
}

# Allow token lookup
path "auth/token/lookup-self" {
  capabilities = ["read"]
}
EOT
}

# Policy for secret rotation
resource "vault_policy" "kgc_rotate" {
  name = "kgc-rotate-${var.environment}"

  policy = <<EOT
# Allow reading and updating KGC secrets
path "kgc/data/*" {
  capabilities = ["read", "create", "update"]
}

# Allow reading and updating metadata
path "kgc/metadata/*" {
  capabilities = ["read", "update"]
}

# Allow destroying old versions
path "kgc/destroy/*" {
  capabilities = ["update"]
}
EOT
}

# Audit device for secret access logging
resource "vault_audit" "kgc_audit" {
  count = var.enable_vault_audit ? 1 : 0
  type  = "file"

  options = {
    file_path = "/vault/logs/kgc-audit.log"
  }
}

# Periodic secret rotation configuration
resource "vault_generic_endpoint" "rotate_api_key" {
  depends_on           = [vault_kv_secret_v2.api_key]
  path                 = "kgc/config"
  ignore_absent_fields = true

  data_json = jsonencode({
    rotation_period = "720h" # 30 days
    auto_rotate     = var.enable_auto_rotation
  })
}

# Shamir's Secret Sharing configuration for quorum unsealing
# This is configured during vault initialization, not via Terraform
# See scripts/vault-init.sh for quorum setup

# Outputs for application configuration
output "vault_kgc_mount_path" {
  description = "Vault mount path for KGC secrets"
  value       = vault_mount.kgc_secrets.path
}

output "vault_kgc_policy_name" {
  description = "Vault policy name for KGC read access"
  value       = vault_policy.kgc_read.name
}

output "vault_kgc_rotation_policy_name" {
  description = "Vault policy name for KGC secret rotation"
  value       = vault_policy.kgc_rotate.name
}
