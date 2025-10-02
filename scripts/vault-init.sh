#!/bin/sh
# HashiCorp Vault Initialization Script
# Implements Shamir's Secret Sharing for quorum-based unsealing

set -e

VAULT_ADDR="${VAULT_ADDR:-http://vault:8200}"
VAULT_KEYS_DIR="${VAULT_KEYS_DIR:-/vault/keys}"
VAULT_QUORUM_SHARES="${VAULT_QUORUM_SHARES:-5}"
VAULT_QUORUM_THRESHOLD="${VAULT_QUORUM_THRESHOLD:-3}"

echo "========================================="
echo "HashiCorp Vault Initialization"
echo "========================================="
echo "Vault Address: $VAULT_ADDR"
echo "Quorum Shares: $VAULT_QUORUM_SHARES"
echo "Quorum Threshold: $VAULT_QUORUM_THRESHOLD"
echo "========================================="

# Wait for Vault to be ready
echo "Waiting for Vault to be ready..."
until vault status >/dev/null 2>&1; do
  echo "Vault not ready, waiting..."
  sleep 2
done

# Check if Vault is already initialized
if vault status | grep -q "Initialized.*true"; then
  echo "Vault is already initialized"

  # Check if sealed
  if vault status | grep -q "Sealed.*true"; then
    echo "Vault is sealed, attempting unseal with stored keys..."

    # Try to unseal with stored keys
    if [ -f "$VAULT_KEYS_DIR/unseal-keys.txt" ]; then
      echo "Found stored unseal keys, unsealing..."

      count=0
      while IFS= read -r key && [ "$count" -lt "$VAULT_QUORUM_THRESHOLD" ]; do
        vault operator unseal "$key" || true
        count=$((count + 1))
      done < "$VAULT_KEYS_DIR/unseal-keys.txt"

      echo "Unseal complete"
    else
      echo "WARNING: No stored unseal keys found!"
      echo "Vault requires manual unsealing with quorum keys"
      exit 1
    fi
  else
    echo "Vault is already unsealed"
  fi

  exit 0
fi

# Initialize Vault with Shamir's Secret Sharing
echo "Initializing Vault with Shamir's Secret Sharing..."
echo "Shares: $VAULT_QUORUM_SHARES, Threshold: $VAULT_QUORUM_THRESHOLD"

INIT_OUTPUT=$(vault operator init \
  -key-shares="$VAULT_QUORUM_SHARES" \
  -key-threshold="$VAULT_QUORUM_THRESHOLD" \
  -format=json)

echo "Vault initialized successfully!"

# Extract and save keys
ROOT_TOKEN=$(echo "$INIT_OUTPUT" | jq -r '.root_token')
UNSEAL_KEYS=$(echo "$INIT_OUTPUT" | jq -r '.unseal_keys_b64[]')

# Create keys directory
mkdir -p "$VAULT_KEYS_DIR"

# Save root token
echo "$ROOT_TOKEN" > "$VAULT_KEYS_DIR/root-token.txt"
chmod 600 "$VAULT_KEYS_DIR/root-token.txt"
echo "Root token saved to: $VAULT_KEYS_DIR/root-token.txt"

# Save unseal keys
echo "$UNSEAL_KEYS" > "$VAULT_KEYS_DIR/unseal-keys.txt"
chmod 600 "$VAULT_KEYS_DIR/unseal-keys.txt"
echo "Unseal keys saved to: $VAULT_KEYS_DIR/unseal-keys.txt"

# Save individual key shares for distribution to quorum members
share_num=1
echo "$UNSEAL_KEYS" | while IFS= read -r key; do
  echo "$key" > "$VAULT_KEYS_DIR/key-share-$share_num.txt"
  chmod 600 "$VAULT_KEYS_DIR/key-share-$share_num.txt"
  echo "Key share $share_num saved"
  share_num=$((share_num + 1))
done

echo ""
echo "========================================="
echo "CRITICAL: QUORUM KEY DISTRIBUTION"
echo "========================================="
echo "Distribute key shares to quorum members:"
echo ""
for i in $(seq 1 "$VAULT_QUORUM_SHARES"); do
  echo "Member $i: $VAULT_KEYS_DIR/key-share-$i.txt"
done
echo ""
echo "Unsealing requires $VAULT_QUORUM_THRESHOLD of $VAULT_QUORUM_SHARES shares"
echo "========================================="

# Unseal Vault using threshold number of keys
echo ""
echo "Unsealing Vault with $VAULT_QUORUM_THRESHOLD keys..."
count=0
echo "$UNSEAL_KEYS" | while IFS= read -r key && [ "$count" -lt "$VAULT_QUORUM_THRESHOLD" ]; do
  vault operator unseal "$key"
  count=$((count + 1))
done

echo "Vault unsealed successfully!"

# Authenticate with root token
export VAULT_TOKEN="$ROOT_TOKEN"

# Enable audit logging
echo ""
echo "Enabling audit logging..."
vault audit enable file file_path=/vault/logs/vault-audit.log

# Enable KV v2 secrets engine
echo ""
echo "Enabling KV v2 secrets engine at kgc/..."
vault secrets enable -path=kgc kv-v2

# Create KGC policies
echo ""
echo "Creating KGC access policies..."

# Read-only policy
vault policy write kgc-read - <<EOF
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
EOF

# Rotation policy
vault policy write kgc-rotate - <<EOF
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
EOF

echo "Policies created successfully!"

# Generate initial secrets
echo ""
echo "Generating initial secrets..."

# Generate random API key
API_KEY=$(openssl rand -base64 48)
vault kv put kgc/api-credentials \
  api_key="$API_KEY" \
  created_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  rotated_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

# Generate random encryption key
ENCRYPTION_KEY=$(openssl rand -base64 48)
vault kv put kgc/encryption-credentials \
  encryption_key="$ENCRYPTION_KEY" \
  algorithm="AES-256-GCM" \
  created_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  rotated_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

# Store database credentials
vault kv put kgc/database-credentials \
  url="${DATABASE_URL:-postgresql://test:test@postgres:5432/kgc_test}" \
  username="test" \
  password="test" \
  created_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

echo "Initial secrets created successfully!"

# Create application token with read policy
echo ""
echo "Creating application token with read access..."
APP_TOKEN=$(vault token create -policy=kgc-read -format=json | jq -r '.auth.client_token')
echo "$APP_TOKEN" > "$VAULT_KEYS_DIR/app-token.txt"
chmod 600 "$VAULT_KEYS_DIR/app-token.txt"
echo "Application token saved to: $VAULT_KEYS_DIR/app-token.txt"

echo ""
echo "========================================="
echo "Vault Initialization Complete!"
echo "========================================="
echo "Root Token: $VAULT_KEYS_DIR/root-token.txt"
echo "App Token: $VAULT_KEYS_DIR/app-token.txt"
echo "Unseal Keys: $VAULT_KEYS_DIR/unseal-keys.txt"
echo ""
echo "WARNING: Store unseal keys securely!"
echo "Distribute key shares to $VAULT_QUORUM_SHARES quorum members"
echo "Unsealing requires $VAULT_QUORUM_THRESHOLD of $VAULT_QUORUM_SHARES shares"
echo "========================================="

# Print secret verification
echo ""
echo "Verifying secrets..."
vault kv get kgc/api-credentials
vault kv get kgc/encryption-credentials
vault kv get kgc/database-credentials

echo ""
echo "Initialization complete! ✅"
