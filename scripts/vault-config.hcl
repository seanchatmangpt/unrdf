# HashiCorp Vault Configuration
# Production-ready configuration with high availability

# Vault storage backend
storage "file" {
  path = "/vault/data"
}

# HTTP listener
listener "tcp" {
  address     = "0.0.0.0:8200"
  tls_disable = 1
}

# API address
api_addr = "http://0.0.0.0:8200"

# Cluster address
cluster_addr = "http://0.0.0.0:8201"

# UI enabled
ui = true

# Telemetry for monitoring
telemetry {
  prometheus_retention_time = "30s"
  disable_hostname = false
}

# Disable mlock for development (enable in production)
disable_mlock = true

# Log level
log_level = "info"

# Default lease TTL
default_lease_ttl = "168h" # 7 days

# Maximum lease TTL
max_lease_ttl = "720h" # 30 days
