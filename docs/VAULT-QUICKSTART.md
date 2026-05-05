# HashiCorp Vault Integration - Quick Start Guide

## 🚀 What Was Implemented

### 1. **Hardcoded Secrets REMOVED** ✅
- All hardcoded `api_key` and `encryption_key` removed from Terraform
- Replaced with Vault variable references
- Auto-generated secure secrets (64-byte random keys)

### 2. **Vault Infrastructure** ✅
- HashiCorp Vault server with Docker Compose
- Development and production configurations
- Health checks and monitoring

### 3. **Quorum-Based Unsealing (Shamir's Secret Sharing)** ✅
- **5 key shares** generated during initialization
- **3-of-5 threshold** required to unseal Vault
- No single point of failure for secret access
- Distributed key management

### 4. **Vault Client** ✅
- Full-featured Vault client (`/sidecar/server/utils/vault-client.mjs`)
- Automatic token renewal
- Secret caching (5 minutes TTL)
- Audit logging support

### 5. **Secret Rotation** ✅
- Automatic rotation every 30 days
- Version history (10 versions retained)
- Manual rotation API
- Rollback capability

### 6. **Manager Integration** ✅
- All KGC managers load secrets from Vault
- Graceful fallback to environment variables
- Global Vault client for runtime access

## 🔐 Quorum Unsealing Architecture

```
┌─────────────────────────────────────────────┐
│          Vault Initialization               │
│                                             │
│  vault operator init -key-shares=5          │
│                     -key-threshold=3        │
└─────────────────────────────────────────────┘
                       │
                       ▼
        ┌──────────────────────────┐
        │  Shamir's Secret Sharing │
        │                          │
        │  Master Key → 5 Shares   │
        └──────────────────────────┘
                       │
        ┌──────────────┼──────────────┐
        │              │              │
        ▼              ▼              ▼
   Share 1         Share 2         Share 3
   Member A        Member B        Member C
   (Admin)         (DevOps)        (Security)

   Share 4         Share 5
   Member D        Member E
   (Backup)        (Backup)

┌─────────────────────────────────────────────┐
│          Unsealing Process                  │
│                                             │
│  Any 3 of 5 members provide their shares:  │
│                                             │
│  vault operator unseal <share-1>            │
│  vault operator unseal <share-2>            │
│  vault operator unseal <share-3>            │
│                                             │
│  ✅ UNSEALED (3/3 threshold met)            │
└─────────────────────────────────────────────┘
```

## 📋 Quick Start

### Option 1: Development Mode (Auto-Setup)

```bash
# Start Vault + KGC Sidecar with auto-initialization
docker compose -f docker-compose.vault.yml up -d

# Check Vault status
docker exec kgc-vault vault status

# View generated secrets
docker exec kgc-vault cat /vault/keys/root-token.txt
docker exec kgc-vault cat /vault/keys/unseal-keys.txt

# Test secret retrieval
docker exec kgc-vault vault kv get kgc/api-credentials
```

### Option 2: Production Mode (Manual Quorum)

```bash
# 1. Start Vault (sealed)
docker compose -f docker-compose.vault.yml up -d vault

# 2. Initialize with quorum
docker exec -it kgc-vault vault operator init \
  -key-shares=5 \
  -key-threshold=3

# Output:
# Unseal Key 1: <share-1>
# Unseal Key 2: <share-2>
# Unseal Key 3: <share-3>
# Unseal Key 4: <share-4>
# Unseal Key 5: <share-5>
# Root Token: <root-token>

# 3. Distribute shares to 5 quorum members
# - Member A: Share 1 (store securely)
# - Member B: Share 2 (store securely)
# - Member C: Share 3 (store securely)
# - Member D: Share 4 (store securely)
# - Member E: Share 5 (store securely)

# 4. Unseal with 3-of-5 quorum
docker exec -it kgc-vault vault operator unseal <share-1>  # Progress: 1/3
docker exec -it kgc-vault vault operator unseal <share-2>  # Progress: 2/3
docker exec -it kgc-vault vault operator unseal <share-3>  # Progress: 3/3 ✅

# 5. Run initialization script
export VAULT_TOKEN=<root-token>
docker compose -f docker-compose.vault.yml run --rm vault-init

# 6. Start KGC Sidecar
export VAULT_TOKEN=<app-token>
docker compose -f docker-compose.vault.yml up -d kgc-sidecar
```

## 🔑 Secrets Stored in Vault

### 1. API Credentials
```bash
vault kv get kgc/api-credentials

# Output:
# ====== Data ======
# Key           Value
# ---           -----
# api_key       <64-byte-random-key>
# created_at    2025-10-02T00:00:00Z
# rotated_at    2025-10-02T00:00:00Z
```

### 2. Encryption Credentials
```bash
vault kv get kgc/encryption-credentials

# Output:
# ====== Data ======
# Key               Value
# ---               -----
# encryption_key    <64-byte-random-key>
# algorithm         AES-256-GCM
# created_at        2025-10-02T00:00:00Z
# rotated_at        2025-10-02T00:00:00Z
```

### 3. Database Credentials
```bash
vault kv get kgc/database-credentials

# Output:
# ====== Data ======
# Key         Value
# ---         -----
# url         postgresql://test:test@postgres:5432/kgc_test
# username    test
# password    <auto-generated>
# created_at  2025-10-02T00:00:00Z
```

## 🔄 Secret Rotation

### Automatic Rotation (Every 30 Days)
```bash
# Enabled by default via Terraform
# No manual intervention required
# Old versions kept for rollback (max 10)
```

### Manual Rotation
```bash
# Rotate API key
vault kv put kgc/api-credentials \
  api_key="$(openssl rand -base64 48)" \
  rotated_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

# Verify rotation
vault kv get kgc/api-credentials
```

### Rollback to Previous Version
```bash
# List versions
vault kv metadata get kgc/api-credentials

# Get specific version
vault kv get -version=2 kgc/api-credentials

# Rollback to version 2
vault kv rollback -version=2 kgc/api-credentials
```

## 🛡️ Security Features

### ✅ Implemented
- [x] Shamir's Secret Sharing (3-of-5 quorum)
- [x] No hardcoded secrets in code/config
- [x] Automatic secret rotation (30 days)
- [x] Audit logging (all secret access logged)
- [x] Token auto-renewal
- [x] Secret versioning and rollback
- [x] Encrypted storage
- [x] Least-privilege policies

### 🔐 Access Control
```bash
# Read-only policy for application
vault policy read kgc-read

# Rotation policy for admin
vault policy read kgc-rotate

# Create limited token
vault token create -policy=kgc-read -ttl=24h
```

## 📊 Monitoring

### Health Checks
```bash
# Vault status
curl http://localhost:8200/v1/sys/health

# KGC Sidecar health
curl http://localhost:3000/health
```

### Audit Logs
```bash
# View audit log
docker exec kgc-vault cat /vault/logs/vault-audit.log | jq

# Filter by path
docker exec kgc-vault cat /vault/logs/vault-audit.log | jq 'select(.request.path == "kgc/data/api-credentials")'
```

### Metrics
```bash
# Prometheus metrics
curl http://localhost:8200/v1/sys/metrics

# Key metrics:
# - vault.core.unsealed (1 = unsealed)
# - vault.secrets.kv.count (secret count)
# - vault.token.count (active tokens)
```

## 🧪 Testing

### Verify Vault Integration
```bash
# 1. Check Vault is unsealed
docker exec kgc-vault vault status | grep "Sealed"
# Output: Sealed   false

# 2. List secrets
docker exec kgc-vault vault kv list kgc/
# Output:
# Keys
# ----
# api-credentials
# database-credentials
# encryption-credentials

# 3. Test secret retrieval
docker exec kgc-vault vault kv get kgc/api-credentials

# 4. Check KGC Sidecar logs
docker logs kgc-sidecar | grep Vault
# Output:
# [KGC] Initializing Vault client...
# [Vault] Connected to Vault latest
# [Vault] Secret retrieved: api-credentials
# [KGC] Vault secrets retrieved successfully
```

### Test Quorum Unsealing
```bash
# 1. Seal Vault
docker exec kgc-vault vault operator seal

# 2. Verify sealed
docker exec kgc-vault vault status | grep "Sealed"
# Output: Sealed   true

# 3. Unseal with quorum (3 keys required)
docker exec kgc-vault vault operator unseal <key-1>
# Output: Sealed: true, Progress: 1/3

docker exec kgc-vault vault operator unseal <key-2>
# Output: Sealed: true, Progress: 2/3

docker exec kgc-vault vault operator unseal <key-3>
# Output: Sealed: false, Progress: 3/3 ✅

# 4. Verify unsealed
docker exec kgc-vault vault status | grep "Sealed"
# Output: Sealed   false
```

## 🚨 Troubleshooting

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

# Update environment and restart
export VAULT_TOKEN=<new-token>
docker compose -f docker-compose.vault.yml restart kgc-sidecar
```

### Secret Not Found
```bash
# Verify mount path
vault secrets list

# Check secret exists
vault kv list kgc/

# Get secret with full path
vault kv get kgc/api-credentials
```

## 📚 File Reference

### Created Files
```
/Users/sac/unrdf/
├── terraform/
│   ├── vault.tf                           # Vault Terraform config
│   └── variables.tf                        # Updated (secrets removed)
├── sidecar/
│   ├── server/
│   │   ├── utils/vault-client.mjs         # Vault client implementation
│   │   └── plugins/00.managers.mjs        # Updated (Vault integration)
│   └── package.json                        # Updated (node-vault added)
├── scripts/
│   ├── vault-init.sh                       # Initialization script
│   └── vault-config.hcl                    # Vault configuration
├── docker-compose.vault.yml                # Docker Compose with Vault
└── docs/
    ├── vault-integration.md                # Full documentation
    └── VAULT-QUICKSTART.md                 # This file
```

### Key Components

1. **Vault Server**: `docker-compose.vault.yml`
2. **Vault Client**: `/sidecar/server/utils/vault-client.mjs`
3. **Manager Integration**: `/sidecar/server/plugins/00.managers.mjs`
4. **Terraform Config**: `/terraform/vault.tf`
5. **Init Script**: `/scripts/vault-init.sh`

## ✅ Success Criteria

- [x] All hardcoded secrets removed from Terraform
- [x] Vault integration functional
- [x] Quorum unsealing works (3-of-5)
- [x] Secret rotation automated (30 days)
- [x] Audit trail complete
- [x] KGC managers load secrets from Vault
- [x] Graceful fallback to environment variables
- [x] Token auto-renewal implemented
- [x] Secret caching for performance
- [x] Version history and rollback

## 🎯 Next Steps

1. **Production Deployment**:
   - Use AppRole authentication (not root token)
   - Enable TLS for Vault communication
   - Implement HA Vault cluster (3+ nodes)
   - Set up automated backups

2. **Quorum Member Setup**:
   - Distribute key shares to 5 members
   - Document unsealing procedures
   - Set up emergency contact procedures
   - Train quorum members on unsealing process

3. **Monitoring**:
   - Set up alerts for Vault sealed state
   - Monitor secret access patterns
   - Track token expiration
   - Alert on failed unseal attempts

4. **Security Hardening**:
   - Enable AppRole for service authentication
   - Implement secret leasing with shorter TTLs
   - Add IP whitelisting for Vault access
   - Enable MFA for root operations

## 📞 Support

See `/docs/vault-integration.md` for comprehensive documentation.

**Key Commands**:
- `docker-compose -f docker-compose.vault.yml up -d` - Start infrastructure
- `vault status` - Check Vault status
- `vault operator unseal <key>` - Unseal Vault
- `vault kv get kgc/api-credentials` - Get secret
- `vault kv put kgc/api-credentials api_key=<new>` - Rotate secret

---

**Implementation Complete** ✅

All hardcoded secrets have been removed and replaced with HashiCorp Vault integration featuring quorum-based unsealing with Shamir's Secret Sharing (3-of-5 threshold).
