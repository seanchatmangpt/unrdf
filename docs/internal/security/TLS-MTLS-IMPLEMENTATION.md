# TLS 1.3 & mTLS Implementation Guide

## Overview

This document describes the TLS 1.3 and mutual TLS (mTLS) implementation for the UNRDF Sidecar, including Byzantine fault-tolerant certificate validation.

## Architecture

### Components

1. **HTTPS Enforcement Middleware** (`00.https-enforce.mjs`)
   - Redirects HTTP to HTTPS
   - Enforces TLS 1.3 minimum version
   - Sets HSTS headers

2. **Security Headers Middleware** (`01.security-headers.mjs`)
   - Applies comprehensive security headers
   - CSP, HSTS, X-Frame-Options, etc.

3. **mTLS Validation Middleware** (`02.mtls-validate.mjs`)
   - Validates client certificates
   - Byzantine consensus for trust decisions

4. **Byzantine Validator Pool** (`mtls-validator.mjs`)
   - 5 independent validators
   - 3-of-5 consensus requirement
   - Certificate expiry, revocation, pinning checks

## Configuration

### Environment Variables

```bash
# TLS configuration
TLS_CERT_PATH=./certs/server-cert.pem
TLS_KEY_PATH=./certs/server-key.pem
TLS_CA_PATH=./certs/ca-cert.pem
TLS_DH_PARAMS=./certs/dhparam.pem
TLS_MIN_VERSION=TLSv1.3
TLS_CIPHERS=TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256

# mTLS configuration
MTLS_ENABLED=true
MTLS_REQUIRE_CLIENT_CERT=true

# HTTPS enforcement
ENFORCE_HTTPS=true
HSTS_MAX_AGE=31536000
HSTS_INCLUDE_SUBDOMAINS=true
HSTS_PRELOAD=true
```

### Nuxt Configuration

The `nuxt.config.mjs` automatically loads TLS certificates and configures HTTPS:

```javascript
https: {
  key: fs.readFileSync(process.env.TLS_KEY_PATH, 'utf-8'),
  cert: fs.readFileSync(process.env.TLS_CERT_PATH, 'utf-8'),
  ca: fs.readFileSync(process.env.TLS_CA_PATH, 'utf-8'),
  minVersion: 'TLSv1.3',
  maxVersion: 'TLSv1.3',
  requestCert: true,
  rejectUnauthorized: true
}
```

## Development Setup

### Generate Self-Signed Certificates

```bash
# Generate development certificates
./scripts/generate-certs.sh

# Certificates will be created in sidecar/certs/
```

This creates:
- `ca-cert.pem` - Certificate Authority
- `server-cert.pem` - Server certificate
- `server-key.pem` - Server private key
- `client-cert.pem` - Client certificate for mTLS
- `client-key.pem` - Client private key
- `dhparam.pem` - DH parameters for Perfect Forward Secrecy

### Configure Environment

```bash
# Copy example environment
cp sidecar/.env.certs.example sidecar/.env

# Start server with HTTPS
cd sidecar
pnpm dev
```

## Production Deployment

### AWS Certificate Manager

Use Terraform to provision certificates:

```bash
cd terraform

# Configure variables
cat > terraform.tfvars <<EOF
sidecar_domain = "sidecar.example.com"
route53_zone_id = "Z1234567890ABC"
environment = "production"
enable_mtls = true
sns_topic_arn = "arn:aws:sns:us-east-1:123456789012:cert-alerts"
EOF

# Deploy
terraform init
terraform plan
terraform apply
```

This provisions:
- ACM certificate with auto-renewal
- DNS validation records
- Private CA for mTLS (optional)
- CloudWatch alarms for expiry

### Let's Encrypt

For Let's Encrypt certificates:

```bash
# Install certbot
apt-get install certbot

# Generate certificate
certbot certonly --standalone -d sidecar.example.com

# Certificates will be in /etc/letsencrypt/live/sidecar.example.com/
```

Update environment:

```bash
TLS_CERT_PATH=/etc/letsencrypt/live/sidecar.example.com/fullchain.pem
TLS_KEY_PATH=/etc/letsencrypt/live/sidecar.example.com/privkey.pem
```

## Byzantine Consensus Validation

### How It Works

The mTLS validator uses 5 independent validators with 3-of-5 consensus:

1. **Expiry Validator** - Checks certificate is not expired
2. **Revocation Validator** - Checks certificate is not revoked
3. **Public Key Pin Validator** - Validates pinned public keys
4. **Chain Validator** - Validates certificate chain
5. **Cryptographic Strength Validator** - Checks key sizes

A certificate is only accepted if at least 3 validators approve.

### Example

```javascript
const validation = await validateClientCertificate(cert);

if (!validation.valid) {
  // votes: [1, 0, 1, 0, 1] = 3 approvals, 2 rejections
  // consensusReached: true (3 >= 3)
  console.log(validation.reason);
  console.log(validation.votes);
}
```

### Public Key Pinning

Pin specific public keys for enhanced security:

```javascript
import { pinPublicKey } from '../utils/mtls-validator.mjs';

// Pin public key for agent certificate
pinPublicKey('unrdf-agent', 'sha256-hash-of-public-key');
```

### Certificate Revocation

Revoke compromised certificates:

```javascript
import { revokeCertificate } from '../utils/mtls-validator.mjs';

// Revoke by serial number
revokeCertificate('01:23:45:67:89:AB:CD:EF');
```

## Security Headers

### Applied Headers

- `Strict-Transport-Security` - HSTS with preload
- `Content-Security-Policy` - Restrict resource loading
- `X-Frame-Options` - Prevent clickjacking
- `X-Content-Type-Options` - Prevent MIME sniffing
- `X-XSS-Protection` - Enable XSS protection
- `Referrer-Policy` - Control referrer information
- `Permissions-Policy` - Restrict browser features
- `Cross-Origin-*-Policy` - CORS protection

### Customization

Override default headers:

```javascript
import { applySecurityHeaders } from '../utils/security-headers.mjs';

applySecurityHeaders(event, {
  contentSecurityPolicy: {
    directives: {
      'default-src': ["'self'"],
      'script-src': ["'self'", "'unsafe-inline'", 'cdn.example.com']
    }
  }
});
```

## Testing

### Test HTTPS Enforcement

```bash
# Should redirect to HTTPS
curl -I http://localhost:3000

# Should return 301 with Location header
```

### Test TLS Version

```bash
# Should succeed with TLS 1.3
openssl s_client -connect localhost:3000 -tls1_3

# Should fail with TLS 1.2
openssl s_client -connect localhost:3000 -tls1_2
```

### Test mTLS

```bash
# Without client certificate (should fail)
curl https://localhost:3000

# With client certificate (should succeed)
curl --cert certs/client-cert.pem \
     --key certs/client-key.pem \
     --cacert certs/ca-cert.pem \
     https://localhost:3000
```

### Test Security Headers

```bash
# Check headers
curl -I https://localhost:3000

# Should include:
# Strict-Transport-Security: max-age=31536000; includeSubDomains; preload
# Content-Security-Policy: ...
# X-Frame-Options: DENY
```

## Monitoring

### Certificate Expiry

CloudWatch alarms monitor certificate expiry:

```bash
# Check alarm status
aws cloudwatch describe-alarms \
  --alarm-names sidecar-cert-expiry-warning-production
```

### TLS Metrics

Log TLS connections:

```javascript
event.context.tls = {
  protocol: req.socket.getProtocol(),
  cipher: req.socket.getCipher(),
  clientCertVerified: event.context.clientCert ? true : false
};
```

## Troubleshooting

### Certificate Errors

**Error: Certificate not found**
```bash
# Check file exists
ls -la $TLS_CERT_PATH

# Check permissions
chmod 644 certs/*.pem
chmod 600 certs/*-key.pem
```

**Error: TLS version mismatch**
```bash
# Check server supports TLS 1.3
openssl s_client -connect localhost:3000 -tls1_3 -msg
```

**Error: mTLS validation failed**
```bash
# Check client certificate validity
openssl x509 -in certs/client-cert.pem -noout -dates

# Check certificate chain
openssl verify -CAfile certs/ca-cert.pem certs/client-cert.pem
```

### Byzantine Consensus Issues

Enable debug logging:

```javascript
// In mtls-validator.mjs
console.log('Validator results:', results);
console.log('Votes:', votes);
console.log('Approvals:', approvals);
```

## Security Best Practices

1. **Always use TLS 1.3** - No fallback to older versions
2. **Enable HSTS preload** - Register domain in HSTS preload list
3. **Use strong ciphers** - AES-256-GCM, ChaCha20-Poly1305
4. **Rotate certificates** - Before expiry (AWS ACM auto-renews)
5. **Monitor expiry** - Set alerts for 30 days before expiry
6. **Pin public keys** - For critical certificates
7. **Use mTLS** - For agent-to-sidecar communication
8. **Perfect Forward Secrecy** - Use DHE/ECDHE ciphers
9. **Disable session resumption** - For maximum security
10. **Regular security audits** - Test with SSL Labs, etc.

## References

- [RFC 8446 - TLS 1.3](https://www.rfc-editor.org/rfc/rfc8446)
- [OWASP TLS Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Transport_Layer_Protection_Cheat_Sheet.html)
- [Mozilla SSL Configuration Generator](https://ssl-config.mozilla.org/)
- [AWS Certificate Manager](https://aws.amazon.com/certificate-manager/)
