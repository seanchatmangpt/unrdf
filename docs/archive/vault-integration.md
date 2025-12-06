# HashiCorp Vault Integration with Quorum-Based Secret Recovery

## Overview


## Architecture

```
┌─────────────────┐
│                 │
│  ┌───────────┐  │
│  │ Vault     │  │──┐
│  │ Client    │  │  │ HTTPS
│  └───────────┘  │  │
└─────────────────┘  │
                     │
                     ▼
           ┌──────────────────┐
           │  Vault Server    │
           │                  │
           │  ┌────────────┐  │
           │  │ KV v2      │  │
           │  │ Secrets    │  │
           │  └────────────┘  │
           │                  │
           │  ┌────────────┐  │
           │  │ Audit Log  │  │
           │  └────────────┘  │
           └──────────────────┘
                     │
                     │ Shamir's Secret Sharing
                     │ (3-of-5 Quorum)
                     │
        ┌────────────┼────────────┐
        ▼            ▼            ▼
    Key Share 1  Key Share 2  Key Share 3
    (Member A)   (Member B)   (Member C)
```

## Shamir's Secret Sharing (Quorum Unsealing)

### Configuration

- **Total Shares**: 5 key shares generated
- **Quorum Threshold**: 3 shares required to unseal
- **Recovery**: Any 3 of 5 members can unseal Vault

### Key Distribution

```bash
# After initialization, distribute keys to quorum members:
Member 1: /vault/keys/key-share-1.txt
Member 2: /vault/keys/key-share-2.txt
Member 3: /vault/keys/key-share-3.txt
Member 4: /vault/keys/key-share-4.txt
Member 5: /vault/keys/key-share-5.txt
```

### Unsealing Process

```bash
# Vault starts sealed (encrypted)
vault status
# Output: Sealed: true

# Member A provides share 1
vault operator unseal $(cat key-share-1.txt)
# Progress: 1/3

# Member B provides share 2
vault operator unseal $(cat key-share-2.txt)
# Progress: 2/3

# Member C provides share 3
vault operator unseal $(cat key-share-3.txt)
# Progress: 3/3 - UNSEALED ✅
```

## Secrets Stored in Vault

### 1. API Credentials
```bash
vault kv get kgc/api-credentials
# Fields:
# - api_key: Auto-generated 64-byte secure key
# - created_at: ISO timestamp
# - rotated_at: ISO timestamp
```

### 2. Encryption Credentials
```bash
vault kv get kgc/encryption-credentials
# Fields:
# - encryption_key: Auto-generated 64-byte key
# - algorithm: AES-256-GCM
# - created_at: ISO timestamp
# - rotated_at: ISO timestamp
```

### 3. Database Credentials
```bash
vault kv get kgc/database-credentials
# Fields:
# - url: PostgreSQL connection URL
# - username: Database user
# - password: Auto-generated secure password
# - created_at: ISO timestamp
```

## Setup Instructions

### 1. Start Vault Infrastructure

```bash
# Development mode with auto-initialization
docker compose -f docker-compose.vault.yml up -d

# Production mode (manual unsealing required)
VAULT_DEV_MODE=false docker compose -f docker-compose.vault.yml up -d
```

### 2. Initialize Vault (Development)

```bash
# Automatic initialization with quorum
docker compose -f docker-compose.vault.yml run --rm vault-init
```

### 3. Initialize Vault (Production)

```bash
# Manual initialization
docker exec -it kgc-vault vault operator init \
  -key-shares=5 \
  -key-threshold=3

# Save output:
# - Root Token: Store securely
# - Unseal Key 1-5: Distribute to quorum members

# Unseal with quorum (run 3 times with different keys)
docker exec -it kgc-vault vault operator unseal <key-1>
docker exec -it kgc-vault vault operator unseal <key-2>
docker exec -it kgc-vault vault operator unseal <key-3>
```


```bash
# Environment variables
export VAULT_ENABLED=true
export VAULT_ADDR=http://vault:8200
export VAULT_TOKEN=<app-token>
export VAULT_MOUNT_PATH=kgc
export VAULT_ENABLE_QUORUM=true
export VAULT_QUORUM_SHARES=5
export VAULT_QUORUM_THRESHOLD=3

# Start knowledge-engine
docker compose -f docker-compose.vault.yml up knowledge-engine
```

## Terraform Integration

### Apply Vault Configuration

```bash
cd terraform

# Initialize Terraform
terraform init

# Set Vault token
export TF_VAR_vault_token=<root-token>

# Apply configuration
terraform apply \
  -var="vault_address=http://vault:8200" \
  -var="environment=production" \
  -var="enable_auto_rotation=true"
```

### Terraform Variables

```hcl
# terraform.tfvars
vault_address = "http://vault:8200"
vault_token = "s.xxxxxxxxxxxxx"
environment = "production"
enable_vault_audit = true
enable_auto_rotation = true
vault_quorum_shares = 5
vault_quorum_threshold = 3
```

## Secret Rotation

### Automatic Rotation (Every 30 Days)

```bash
# Enabled via Terraform
variable "enable_auto_rotation" {
  default = true
}

# Rotation happens automatically
# Old versions are kept (max 10 versions)
```

### Manual Rotation

```bash
# Using Vault CLI
vault kv put kgc/api-credentials \
  api_key="<new-key>" \
  rotated_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

# Using VaultClient API
const vaultClient = globalThis.__vaultClient
await vaultClient.rotateSecret('api-credentials', {
  api_key: generateNewKey()
})
```

### Rollback to Previous Version

```bash
# List versions
vault kv metadata get kgc/api-credentials

# Get specific version
vault kv get -version=2 kgc/api-credentials

# Restore old version
vault kv rollback -version=2 kgc/api-credentials
```

## Audit Trail

### Enable Audit Logging

```bash
# File-based audit log
vault audit enable file file_path=/vault/logs/vault-audit.log

# Query audit log
vault read sys/audit/file
```

### Audit Log Format

```json
{
  "time": "2025-10-02T00:00:00Z",
  "type": "request",
  "auth": {
    "token_type": "service"
  },
  "request": {
    "operation": "read",
    "path": "kgc/data/api-credentials",
    "data": null
  },
  "response": {
    "data": "***sensitive***"
  }
}
```

## Vault Client API

### Initialize Client

```javascript
import { createVaultClient } from './server/utils/vault-client.mjs'

const vaultClient = await createVaultClient({
  endpoint: 'http://vault:8200',
  token: process.env.VAULT_TOKEN,
  mountPath: 'kgc',
  enableQuorum: true,
  quorumShares: 5,
  quorumThreshold: 3
})
```

### Get Secret

```javascript
// Single secret
const apiCreds = await vaultClient.getSecret('api-credentials')
console.log(apiCreds.api_key)

// All secrets
const secrets = await vaultClient.getAllSecrets()
console.log(secrets.apiKey)
console.log(secrets.encryptionKey)
console.log(secrets.database.url)
```

### Write Secret

```javascript
await vaultClient.writeSecret('custom-secret', {
  key: 'value',
  created_at: new Date().toISOString()
})
```

### Rotate Secret

```javascript
const result = await vaultClient.rotateSecret('api-credentials', {
  api_key: generateNewKey(),
  rotated_at: new Date().toISOString()
})
console.log(`Rotated to version ${result.version}`)
```

## Security Best Practices

### 1. Quorum Distribution

- Distribute key shares to geographically distributed members
- Use different communication channels for each share
- Store shares in separate secure locations

### 2. Access Control

```bash
# Use least-privilege policies
vault policy write kgc-read - <<EOF
path "kgc/data/*" {
  capabilities = ["read"]
}
EOF

# Create tokens with limited TTL
vault token create -policy=kgc-read -ttl=24h
```

### 3. Token Renewal

```javascript
// Automatic token renewal
const vaultClient = await createVaultClient({
  endpoint: 'http://vault:8200',
  token: process.env.VAULT_TOKEN
})

// Token auto-renews at 50% of TTL
// No manual renewal required
```

### 4. Secret Caching

```javascript
// Secrets cached for 5 minutes by default
const vaultClient = await createVaultClient({
  cacheTTL: 300000 // 5 minutes
})

// Clear cache manually
vaultClient.clearCache()
```

## Monitoring

### Vault Health Check

```bash
# Health endpoint
curl http://vault:8200/v1/sys/health

# Status
vault status
```

### Metrics

```bash
# Prometheus metrics
curl http://vault:8200/v1/sys/metrics

# Key metrics:
# - vault.core.unsealed (1 = unsealed, 0 = sealed)
# - vault.secrets.kv.count (number of secrets)
# - vault.token.count (active tokens)
```

### Logs

```bash
# Vault server logs
docker logs kgc-vault

# Audit logs
docker exec kgc-vault cat /vault/logs/vault-audit.log
```

## Troubleshooting

### Vault Sealed After Restart

```bash
# Normal behavior - Vault seals on restart for security
# Unseal with quorum keys
vault operator unseal <key-1>
vault operator unseal <key-2>
vault operator unseal <key-3>
```

### Token Expired

```bash
# Create new token
vault token create -policy=kgc-read

# Update environment variable
export VAULT_TOKEN=<new-token>

# Restart knowledge-engine
docker compose -f docker-compose.vault.yml restart knowledge-engine
```

### Secret Not Found

```bash
# Verify secret exists
vault kv list kgc/

# Check path
vault kv get kgc/api-credentials

# Verify mount path
vault secrets list
```

## Production Deployment

### High Availability Setup

```yaml
# docker-compose.vault-ha.yml
services:
  vault-1:
    image: hashicorp/vault:1.15
    environment:
      VAULT_CLUSTER_ADDR: http://vault-1:8201

  vault-2:
    image: hashicorp/vault:1.15
    environment:
      VAULT_CLUSTER_ADDR: http://vault-2:8201

  vault-3:
    image: hashicorp/vault:1.15
    environment:
      VAULT_CLUSTER_ADDR: http://vault-3:8201
```

### Backup Strategy

```bash
# Backup Vault data
docker exec kgc-vault vault operator raft snapshot save backup.snap

# Restore from backup
docker exec kgc-vault vault operator raft snapshot restore backup.snap
```

## References

- [HashiCorp Vault Documentation](https://www.vaultproject.io/docs)
- [Shamir's Secret Sharing](https://en.wikipedia.org/wiki/Shamir%27s_Secret_Sharing)
- [KV Secrets Engine v2](https://www.vaultproject.io/docs/secrets/kv/kv-v2)
- [Vault Security Model](https://www.vaultproject.io/docs/internals/security)
