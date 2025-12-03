# HashiCorp Vault Integration - Testing Guide

## 🧪 Complete Testing Suite

This guide provides comprehensive testing procedures for the Vault integration with quorum-based secret recovery.

---

## 1️⃣ Development Quick Test

### Start Infrastructure
```bash
cd /Users/sac/unrdf

# Start Vault + KGC Sidecar
docker compose -f docker-compose.vault.yml up -d

# Wait for initialization
sleep 10

# Check all services
docker compose -f docker-compose.vault.yml ps
```

### Expected Output
```
NAME                IMAGE                          STATUS
kgc-vault          hashicorp/vault:1.15           healthy
kgc-sidecar        unrdf-sidecar:latest          healthy
kgc-postgres       postgres:16-alpine             healthy
kgc-jaeger         jaegertracing/all-in-one:1.52  running
```

### Verify Vault Status
```bash
# Check Vault is unsealed
docker exec kgc-vault vault status

# Expected:
# Sealed: false
# Initialized: true
```

### Verify Secrets
```bash
# List all secrets
docker exec kgc-vault vault kv list kgc/

# Expected output:
# Keys
# ----
# api-credentials
# database-credentials
# encryption-credentials

# Get API credentials
docker exec kgc-vault vault kv get kgc/api-credentials

# Expected output:
# ====== Data ======
# Key           Value
# ---           -----
# api_key       <64-byte-random-key>
# created_at    2025-10-02T...
# rotated_at    2025-10-02T...
```

### Verify KGC Sidecar Integration
```bash
# Check sidecar logs for Vault initialization
docker logs kgc-sidecar 2>&1 | grep -A 5 "Vault"

# Expected output:
# [KGC] Initializing Vault client...
# [Vault] Connected to Vault 1.15.0
# [Vault] Initialized: true, Sealed: false
# [Vault] Secret retrieved: api-credentials
# [Vault] Secret retrieved: encryption-credentials
# [Vault] Secret retrieved: database-credentials
# [KGC] Vault secrets retrieved successfully
# [KGC] Managers initialized successfully
```

### Test Health Endpoints
```bash
# Vault health
curl -s http://localhost:8200/v1/sys/health | jq

# Expected:
# {
#   "initialized": true,
#   "sealed": false,
#   "standby": false,
#   ...
# }

# KGC Sidecar health
curl -s http://localhost:3000/health | jq

# Expected:
# {
#   "status": "ok",
#   "vault": "connected",
#   ...
# }
```

---

## 2️⃣ Quorum Unsealing Test

### Test Vault Sealing/Unsealing

```bash
# 1. Seal the Vault
docker exec kgc-vault vault operator seal

# 2. Verify sealed state
docker exec kgc-vault vault status | grep Sealed
# Expected: Sealed   true

# 3. Get unseal keys
docker exec kgc-vault cat /vault/keys/unseal-keys.txt

# 4. Unseal with first key (progress 1/3)
KEY1=$(docker exec kgc-vault head -n 1 /vault/keys/unseal-keys.txt)
docker exec kgc-vault vault operator unseal "$KEY1"

# Expected output:
# Sealed:  true
# Progress: 1/3

# 5. Unseal with second key (progress 2/3)
KEY2=$(docker exec kgc-vault sed -n '2p' /vault/keys/unseal-keys.txt)
docker exec kgc-vault vault operator unseal "$KEY2"

# Expected output:
# Sealed:  true
# Progress: 2/3

# 6. Unseal with third key (unsealed!)
KEY3=$(docker exec kgc-vault sed -n '3p' /vault/keys/unseal-keys.txt)
docker exec kgc-vault vault operator unseal "$KEY3"

# Expected output:
# Sealed:  false
# Progress: 3/3

# 7. Verify unsealed
docker exec kgc-vault vault status | grep Sealed
# Expected: Sealed   false
```

### Test Quorum Threshold

```bash
# Attempt to unseal with only 2 keys (should fail)
docker exec kgc-vault vault operator seal

# Unseal attempt 1
KEY1=$(docker exec kgc-vault head -n 1 /vault/keys/unseal-keys.txt)
docker exec kgc-vault vault operator unseal "$KEY1"
# Progress: 1/3

# Unseal attempt 2
KEY2=$(docker exec kgc-vault sed -n '2p' /vault/keys/unseal-keys.txt)
docker exec kgc-vault vault operator unseal "$KEY2"
# Progress: 2/3

# Check still sealed (need 3 keys)
docker exec kgc-vault vault status | grep Sealed
# Expected: Sealed   true ✅ (correct - threshold not met)

# Now unseal with third key
KEY3=$(docker exec kgc-vault sed -n '3p' /vault/keys/unseal-keys.txt)
docker exec kgc-vault vault operator unseal "$KEY3"
# Expected: Sealed   false ✅ (unsealed!)
```

---

## 3️⃣ Secret Rotation Test

### Manual Secret Rotation

```bash
# 1. Get current API key
docker exec kgc-vault vault kv get -format=json kgc/api-credentials | \
  jq -r '.data.data.api_key'

# Save for comparison
CURRENT_KEY=$(docker exec kgc-vault vault kv get -format=json kgc/api-credentials | \
  jq -r '.data.data.api_key')

# 2. Rotate the secret
NEW_KEY=$(docker exec kgc-vault openssl rand -base64 48)
docker exec kgc-vault vault kv put kgc/api-credentials \
  api_key="$NEW_KEY" \
  rotated_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

# 3. Verify rotation
ROTATED_KEY=$(docker exec kgc-vault vault kv get -format=json kgc/api-credentials | \
  jq -r '.data.data.api_key')

# Keys should be different
if [ "$CURRENT_KEY" != "$ROTATED_KEY" ]; then
  echo "✅ Secret rotation successful"
else
  echo "❌ Secret rotation failed"
fi

# 4. Check version history
docker exec kgc-vault vault kv metadata get kgc/api-credentials

# Expected:
# ====== Metadata ======
# Key                Value
# ---                -----
# created_time       2025-10-02T...
# current_version    2
# max_versions       10
# oldest_version     1
```

### Test Version Rollback

```bash
# 1. Check current version
docker exec kgc-vault vault kv metadata get kgc/api-credentials | grep current_version

# 2. Get version 1 (original)
docker exec kgc-vault vault kv get -version=1 kgc/api-credentials

# 3. Rollback to version 1
docker exec kgc-vault vault kv rollback -version=1 kgc/api-credentials

# 4. Verify rollback
docker exec kgc-vault vault kv get kgc/api-credentials | grep api_key

# Should match version 1 value ✅
```

---

## 4️⃣ Security & Audit Test

### Test Access Policies

```bash
# 1. Create limited token with read-only policy
APP_TOKEN=$(docker exec kgc-vault vault token create \
  -policy=kgc-read \
  -ttl=1h \
  -format=json | jq -r '.auth.client_token')

# 2. Try to read secret (should succeed)
docker exec -e VAULT_TOKEN="$APP_TOKEN" kgc-vault \
  vault kv get kgc/api-credentials

# Expected: ✅ Success (read allowed)

# 3. Try to write secret (should fail)
docker exec -e VAULT_TOKEN="$APP_TOKEN" kgc-vault \
  vault kv put kgc/api-credentials api_key="test"

# Expected: ❌ Error: permission denied (correct!)
```

### Test Audit Logging

```bash
# 1. Enable audit logging (if not already enabled)
docker exec kgc-vault vault audit list

# 2. Perform a secret read
docker exec kgc-vault vault kv get kgc/api-credentials

# 3. Check audit log
docker exec kgc-vault cat /vault/logs/vault-audit.log | \
  tail -n 5 | jq

# Expected: JSON logs showing:
# - request.path: "kgc/data/api-credentials"
# - request.operation: "read"
# - auth.token_type: "service"

# 4. Verify all access is logged
docker exec kgc-vault cat /vault/logs/vault-audit.log | \
  jq 'select(.request.path | contains("kgc/data"))' | \
  wc -l

# Should show multiple entries ✅
```

---

## 5️⃣ Failure Recovery Test

### Test Vault Restart

```bash
# 1. Restart Vault container
docker restart kgc-vault

# 2. Wait for restart
sleep 5

# 3. Check status (should be sealed)
docker exec kgc-vault vault status | grep Sealed
# Expected: Sealed   true ✅

# 4. Auto-unseal with initialization script
docker compose -f docker-compose.vault.yml run --rm vault-init

# 5. Verify unsealed
docker exec kgc-vault vault status | grep Sealed
# Expected: Sealed   false ✅
```

### Test KGC Sidecar Recovery

```bash
# 1. Restart KGC Sidecar
docker restart kgc-sidecar

# 2. Wait for startup
sleep 10

# 3. Check logs for Vault reconnection
docker logs kgc-sidecar 2>&1 | grep -A 3 "Vault"

# Expected:
# [KGC] Initializing Vault client...
# [Vault] Connected to Vault 1.15.0
# [Vault] Secret retrieved: api-credentials
# [KGC] Vault secrets retrieved successfully ✅

# 4. Test health endpoint
curl -s http://localhost:3000/health | jq '.vault'
# Expected: "connected" ✅
```

### Test Vault Unavailable (Fallback)

```bash
# 1. Stop Vault
docker stop kgc-vault

# 2. Restart sidecar (should fallback to env vars)
docker restart kgc-sidecar

# 3. Check logs for fallback
docker logs kgc-sidecar 2>&1 | grep -A 2 "Vault initialization failed"

# Expected:
# [KGC] Vault initialization failed: connect ECONNREFUSED
# [KGC] Falling back to environment variables ✅

# 4. Verify sidecar still functional (degraded mode)
curl -s http://localhost:3000/health
# Expected: 200 OK (but vault: "disconnected")

# 5. Restart Vault and sidecar
docker start kgc-vault
sleep 5
docker restart kgc-sidecar
sleep 10

# 6. Verify Vault reconnection
docker logs kgc-sidecar 2>&1 | tail -n 20 | grep Vault
# Expected: Vault secrets retrieved successfully ✅
```

---

## 6️⃣ Performance Test

### Secret Retrieval Latency

```bash
# Create test script
cat > /tmp/vault-perf-test.sh <<'EOF'
#!/bin/bash
echo "Testing secret retrieval performance..."

# Cache miss (first retrieval)
START=$(date +%s%N)
docker exec kgc-vault vault kv get kgc/api-credentials > /dev/null
END=$(date +%s%N)
CACHE_MISS=$((($END - $START) / 1000000))
echo "Cache miss: ${CACHE_MISS}ms"

# Cache hit (subsequent retrievals)
TOTAL=0
for i in {1..10}; do
  START=$(date +%s%N)
  docker exec kgc-vault vault kv get kgc/api-credentials > /dev/null 2>&1
  END=$(date +%s%N)
  DURATION=$((($END - $START) / 1000000))
  TOTAL=$(($TOTAL + $DURATION))
done
AVG=$(($TOTAL / 10))
echo "Cache hit average (10 runs): ${AVG}ms"

echo ""
echo "Expected:"
echo "  Cache miss: 10-20ms ✅"
echo "  Cache hit:  < 5ms ✅"
EOF

chmod +x /tmp/vault-perf-test.sh
/tmp/vault-perf-test.sh
```

### Token Renewal Performance

```bash
# Test token renewal
START=$(date +%s%N)
docker exec kgc-vault vault token renew-self > /dev/null
END=$(date +%s%N)
DURATION=$((($END - $START) / 1000000))

echo "Token renewal: ${DURATION}ms"
echo "Expected: 10-50ms ✅"
```

---

## 7️⃣ Integration Test (End-to-End)

### Complete Workflow Test

```bash
#!/bin/bash
set -e

echo "=== VAULT INTEGRATION E2E TEST ==="
echo ""

# Test 1: Infrastructure startup
echo "Test 1: Starting infrastructure..."
docker compose -f docker-compose.vault.yml up -d
sleep 15
docker compose -f docker-compose.vault.yml ps | grep -q "healthy" || exit 1
echo "✅ Infrastructure started"

# Test 2: Vault unsealed
echo "Test 2: Checking Vault status..."
docker exec kgc-vault vault status | grep -q "Sealed.*false" || exit 1
echo "✅ Vault unsealed"

# Test 3: Secrets exist
echo "Test 3: Verifying secrets..."
docker exec kgc-vault vault kv list kgc/ | grep -q "api-credentials" || exit 1
docker exec kgc-vault vault kv list kgc/ | grep -q "encryption-credentials" || exit 1
docker exec kgc-vault vault kv list kgc/ | grep -q "database-credentials" || exit 1
echo "✅ All secrets present"

# Test 4: KGC Sidecar connected
echo "Test 4: Checking sidecar integration..."
docker logs kgc-sidecar 2>&1 | grep -q "Vault secrets retrieved successfully" || exit 1
echo "✅ Sidecar connected to Vault"

# Test 5: Secret rotation
echo "Test 5: Testing secret rotation..."
BEFORE=$(docker exec kgc-vault vault kv get -format=json kgc/api-credentials | jq -r '.data.data.api_key')
docker exec kgc-vault vault kv put kgc/api-credentials \
  api_key="$(docker exec kgc-vault openssl rand -base64 48)" \
  rotated_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)" > /dev/null
AFTER=$(docker exec kgc-vault vault kv get -format=json kgc/api-credentials | jq -r '.data.data.api_key')
[ "$BEFORE" != "$AFTER" ] || exit 1
echo "✅ Secret rotation works"

# Test 6: Quorum unsealing
echo "Test 6: Testing quorum unsealing..."
docker exec kgc-vault vault operator seal > /dev/null
docker exec kgc-vault vault status | grep -q "Sealed.*true" || exit 1
KEY1=$(docker exec kgc-vault head -n 1 /vault/keys/unseal-keys.txt)
KEY2=$(docker exec kgc-vault sed -n '2p' /vault/keys/unseal-keys.txt)
KEY3=$(docker exec kgc-vault sed -n '3p' /vault/keys/unseal-keys.txt)
docker exec kgc-vault vault operator unseal "$KEY1" > /dev/null
docker exec kgc-vault vault operator unseal "$KEY2" > /dev/null
docker exec kgc-vault vault operator unseal "$KEY3" > /dev/null
docker exec kgc-vault vault status | grep -q "Sealed.*false" || exit 1
echo "✅ Quorum unsealing works"

# Test 7: Audit logging
echo "Test 7: Checking audit logs..."
docker exec kgc-vault vault kv get kgc/api-credentials > /dev/null
docker exec kgc-vault cat /vault/logs/vault-audit.log | grep -q "kgc/data/api-credentials" || exit 1
echo "✅ Audit logging works"

# Test 8: Health endpoints
echo "Test 8: Testing health endpoints..."
curl -sf http://localhost:8200/v1/sys/health > /dev/null || exit 1
curl -sf http://localhost:3000/health > /dev/null || exit 1
echo "✅ Health endpoints responding"

echo ""
echo "=== ALL TESTS PASSED ✅ ==="
```

### Save and run the test
```bash
# Save test script
cat > /tmp/vault-e2e-test.sh <<'EOF'
[paste the script above]
EOF

chmod +x /tmp/vault-e2e-test.sh

# Run the test
/tmp/vault-e2e-test.sh
```

---

## 8️⃣ Cleanup

### Stop All Services
```bash
docker compose -f docker-compose.vault.yml down

# Remove volumes (optional - destroys all data)
docker compose -f docker-compose.vault.yml down -v
```

### Reset Vault
```bash
# Remove Vault data and keys
docker compose -f docker-compose.vault.yml down -v
docker volume rm unrdf_vault-data unrdf_vault-keys

# Restart fresh
docker compose -f docker-compose.vault.yml up -d
```

---

## 📊 Test Results Summary

### Expected Test Outcomes

| Test | Expected Result | Verification |
|------|----------------|--------------|
| Infrastructure Startup | All services healthy | `docker ps` shows 4 healthy containers |
| Vault Status | Unsealed, Initialized | `vault status` shows `Sealed: false` |
| Secret Existence | 3 secrets present | `vault kv list kgc/` shows all 3 |
| KGC Integration | Connected successfully | Logs show "Vault secrets retrieved" |
| Quorum Unsealing | 3-of-5 threshold works | Unseal with 3 keys succeeds |
| Secret Rotation | New key != old key | Version increments, key changes |
| Audit Logging | All access logged | Audit log contains read operations |
| Policy Enforcement | Read allowed, write denied | App token can read but not write |
| Fallback Mechanism | Uses env vars when Vault down | Sidecar starts with env vars |
| Performance | Cache < 1ms, uncached < 20ms | Latency within expected range |

---

## 🐛 Troubleshooting Tests

### If Vault Won't Unseal
```bash
# Check unseal progress
docker exec kgc-vault vault status

# Reset unseal process
docker exec kgc-vault vault operator unseal -reset

# Try again with correct keys
docker compose -f docker-compose.vault.yml run --rm vault-init
```

### If Secrets Don't Exist
```bash
# Reinitialize secrets
docker compose -f docker-compose.vault.yml run --rm vault-init

# Verify creation
docker exec kgc-vault vault kv list kgc/
```

### If KGC Sidecar Can't Connect
```bash
# Check Vault is accessible
curl http://localhost:8200/v1/sys/health

# Check environment variables
docker exec kgc-sidecar env | grep VAULT

# Check sidecar logs for errors
docker logs kgc-sidecar 2>&1 | grep -i error
```

---

## ✅ Test Validation Checklist

- [ ] Infrastructure starts successfully
- [ ] Vault is unsealed and initialized
- [ ] All 3 secrets exist (api, encryption, database)
- [ ] KGC Sidecar retrieves secrets from Vault
- [ ] Quorum unsealing works (3-of-5 threshold)
- [ ] Secret rotation updates version
- [ ] Secret rollback restores old version
- [ ] Audit logging captures all access
- [ ] Read-only policy enforced
- [ ] Vault restart requires unsealing
- [ ] KGC Sidecar recovers after restart
- [ ] Fallback to env vars when Vault unavailable
- [ ] Performance meets expectations (< 20ms)
- [ ] Health endpoints respond correctly

---

**Testing Status**: Comprehensive suite provided
**Coverage**: Infrastructure, Security, Performance, Recovery
**Automation**: Scripts provided for all tests
**Expected Duration**: 10-15 minutes for full suite
