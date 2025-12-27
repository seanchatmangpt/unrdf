# Claude Code Hooks Security Guide

**Research Agent**: Agent 02 - Hooks & Tool Governance Explorer
**Date**: 2025-12-27
**Status**: ✅ Security best practices and threat model

## Threat Model

### Threat Actors

1. **Malicious User**: Intentionally bypassing hooks
2. **Compromised AI**: AI model acting maliciously or erratically
3. **Insider Threat**: Developer with legitimate access
4. **External Attacker**: Remote attacker exploiting vulnerabilities

### Attack Vectors

| Vector | Description | Mitigation |
|--------|-------------|------------|
| Hook Bypass | Disable or circumvent hooks | File permissions, process monitoring |
| Command Injection | Inject malicious commands | Input validation, safe parsing |
| Pattern Evasion | Craft commands to evade patterns | Comprehensive patterns, allowlist approach |
| Resource Exhaustion | Overload hooks with requests | Rate limiting, timeouts |
| Information Disclosure | Leak sensitive data via hooks | Audit log encryption, access controls |
| Privilege Escalation | Escalate from hook to system access | Principle of least privilege |

---

## Defense in Depth

### Layer 1: File System Security

**Protect hook files from tampering**:

```bash
# Set restrictive permissions
chmod 700 .claude/hooks/
chmod 700 .claude/hooks/*.sh
chown $USER:$USER .claude/hooks/*

# Verify permissions
ls -la .claude/hooks/

# Expected output:
# drwx------ .claude/hooks/
# -rwx------ safety-check.sh
# -rwx------ audit-log.sh
```

**Prevent unauthorized modification**:

```bash
# Make hooks immutable (Linux)
sudo chattr +i .claude/hooks/*.sh

# Verify
lsattr .claude/hooks/*.sh
# Expected: ----i--------e---

# To modify (when needed):
sudo chattr -i .claude/hooks/*.sh
```

### Layer 2: Input Validation

**NEVER execute raw input**:

```bash
# ❌ DANGEROUS:
INPUT=$(cat)
eval "$INPUT"  # NEVER DO THIS

# ✅ SAFE:
INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command // empty')
# Then validate COMMAND before use
```

**Validate all input fields**:

```bash
#!/bin/bash
INPUT=$(cat)

# Validate JSON structure
if ! echo "$INPUT" | jq empty 2>/dev/null; then
  echo "ERROR: Invalid JSON input" >&2
  exit 1
fi

# Validate required fields
TOOL=$(echo "$INPUT" | jq -r '.tool_name // empty')
if [ -z "$TOOL" ]; then
  echo "ERROR: Missing tool_name field" >&2
  exit 1
fi

# Validate field types
if ! echo "$INPUT" | jq -e '.tool_input | type == "object"' >/dev/null 2>&1; then
  echo "ERROR: tool_input must be an object" >&2
  exit 1
fi
```

### Layer 3: Safe Command Execution

**Use safe parsing**:

```bash
# ✅ SAFE: Use jq for JSON parsing
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command')

# ❌ UNSAFE: String manipulation
COMMAND=$(echo "$INPUT" | grep -oP '"command":\s*"\K[^"]+')
```

**Avoid dynamic evaluation**:

```bash
# ❌ DANGEROUS:
PATTERN=$(echo "$INPUT" | jq -r '.pattern')
if echo "$COMMAND" | grep -E "$PATTERN"; then  # Pattern injection!
  exit 1
fi

# ✅ SAFE:
# Use fixed patterns only
PATTERNS=("rm -rf" "dd if=" "mkfs")
for pattern in "${PATTERNS[@]}"; do
  if echo "$COMMAND" | grep -F "$pattern"; then
    exit 1
  fi
done
```

### Layer 4: Principle of Least Privilege

**Run hooks with minimal permissions**:

```bash
# Don't run hooks as root
if [ "$EUID" -eq 0 ]; then
  echo "ERROR: Hook should not run as root" >&2
  exit 1
fi

# Drop privileges if needed
if [ -n "$SUDO_USER" ]; then
  exec su - "$SUDO_USER" -c "$0 $*"
fi
```

**Limit hook capabilities**:

```bash
# Can't write to system directories
if echo "$FILE" | grep -qE '^/(etc|var|usr|sys|proc)'; then
  echo "BLOCKED: System directory write" >&2
  exit 1
fi

# Can't access other users' files
if echo "$FILE" | grep -qE "^/home/[^/]+/" && ! echo "$FILE" | grep -qE "^$HOME"; then
  echo "BLOCKED: Access to other user's files" >&2
  exit 1
fi
```

### Layer 5: Audit and Monitoring

**Log security events**:

```bash
#!/bin/bash
# security-audit-log.sh

SECURITY_LOG="/var/log/claude/security.log"
mkdir -p "$(dirname "$SECURITY_LOG")"

INPUT=$(cat)
TOOL=$(echo "$INPUT" | jq -r '.tool_name')
ACTION=$(echo "$INPUT" | jq -r '.tool_input.command // .tool_input.file_path // "unknown"')

# Log with syslog priority
logger -p auth.warning -t claude-hook "Security audit: tool=$TOOL action=$ACTION user=$USER"

# Also log to file
echo "$(date -u +"%Y-%m-%dT%H:%M:%SZ") $USER $TOOL $ACTION" >> "$SECURITY_LOG"
```

**Monitor for anomalies**:

```bash
# Detect unusual patterns
COUNT=$(tail -100 "$SECURITY_LOG" | grep "$USER" | wc -l)
if [ "$COUNT" -gt 50 ]; then
  # Unusual activity - alert
  logger -p auth.alert -t claude-hook "ALERT: Unusual activity detected for $USER"
fi
```

---

## Common Vulnerabilities and Mitigations

### Vulnerability 1: Command Injection

**Attack**:
```bash
# Attacker provides:
command: "ls; rm -rf /"
```

**Vulnerable Hook**:
```bash
# ❌ VULNERABLE:
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command')
bash -c "$COMMAND"  # Executes both ls AND rm -rf /
```

**Mitigation**:
```bash
# ✅ SECURE:
# Don't execute user input - only validate it
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command')

# Validate against patterns
if echo "$COMMAND" | grep -qE ';|&&|\|\||\||`|\$\('; then
  echo "BLOCKED: Command contains injection characters" >&2
  exit 1
fi
```

### Vulnerability 2: Path Traversal

**Attack**:
```bash
# Attacker provides:
file_path: "../../etc/passwd"
```

**Vulnerable Hook**:
```bash
# ❌ VULNERABLE:
FILE=$(echo "$INPUT" | jq -r '.tool_input.file_path')
cat "$FILE"  # Could read /etc/passwd
```

**Mitigation**:
```bash
# ✅ SECURE:
FILE=$(echo "$INPUT" | jq -r '.tool_input.file_path')
REAL_PATH=$(realpath -m "$FILE" 2>/dev/null || echo "INVALID")

# Verify file is in allowed directory
if ! echo "$REAL_PATH" | grep -q "^$HOME"; then
  echo "BLOCKED: Path traversal attempt" >&2
  exit 1
fi
```

### Vulnerability 3: Pattern Bypass

**Attack**:
```bash
# Hook blocks: rm -rf /
# Attacker uses:
command: "RM='rm -rf' && $RM /"
```

**Vulnerable Hook**:
```bash
# ❌ VULNERABLE:
if echo "$COMMAND" | grep -q "rm -rf /"; then
  exit 1  # Won't match variable substitution
fi
```

**Mitigation**:
```bash
# ✅ MORE SECURE:
# Block variable assignments and substitutions
if echo "$COMMAND" | grep -qE '\$|`|{|}'; then
  echo "BLOCKED: Variable or command substitution detected" >&2
  exit 1
fi

# Use allowlist instead of blocklist
ALLOWED_COMMANDS=("npm test" "git status" "git diff")
MATCHED=false
for allowed in "${ALLOWED_COMMANDS[@]}"; do
  if [ "$COMMAND" = "$allowed" ]; then
    MATCHED=true
    break
  fi
done

if [ "$MATCHED" = false ]; then
  echo "BLOCKED: Command not in allowlist" >&2
  exit 1
fi
```

### Vulnerability 4: Race Conditions

**Attack**:
```bash
# Time-of-check to time-of-use (TOCTOU)
# Hook checks file permissions, attacker changes them before write
```

**Mitigation**:
```bash
# ✅ SECURE:
# Validate at execution time, not just in hook
# Use atomic operations
# Check permissions again after hook completes
```

### Vulnerability 5: Information Disclosure

**Attack**:
```bash
# Hook logs sensitive data
audit_log: "User accessed file: /home/user/secrets/api-key.txt"
```

**Mitigation**:
```bash
# ✅ SECURE:
# Sanitize file paths before logging
FILE=$(echo "$INPUT" | jq -r '.tool_input.file_path')
SANITIZED=$(echo "$FILE" | sed -E 's/secrets?|key|password|credential/***REDACTED***/gi')

echo "$(date) User accessed: $SANITIZED" >> "$AUDIT_LOG"
```

---

## Security Configuration Checklist

### Deployment Security

- [ ] Hook files owned by root or dedicated user (not AI)
- [ ] Hook directory permissions: 700 (rwx------)
- [ ] Hook files permissions: 700 or 500 (r-x------)
- [ ] Immutable flag set on hooks (chattr +i)
- [ ] settings.json validated against schema
- [ ] No sensitive data in environment variables
- [ ] Audit logs encrypted at rest
- [ ] Audit logs rotated and backed up
- [ ] Security monitoring enabled

### Code Security

- [ ] All input validated (type, format, range)
- [ ] No eval or exec of user input
- [ ] No variable expansion in patterns
- [ ] Path traversal prevented (realpath checks)
- [ ] Command injection prevented (no bash -c with user input)
- [ ] Safe JSON parsing (jq, not regex)
- [ ] Error messages don't leak sensitive info
- [ ] Timeouts enforced (<5s)
- [ ] Resource limits enforced

### Operational Security

- [ ] Hook execution logged to secure location
- [ ] Failed hook attempts monitored
- [ ] Anomaly detection enabled
- [ ] Incident response plan documented
- [ ] Regular security audits scheduled
- [ ] Hook code reviewed by security team
- [ ] Penetration testing performed
- [ ] Rollback procedure tested

---

## Compliance Considerations

### GDPR

**Requirements**:
- Log access to personal data
- Retain logs for 90 days minimum
- Allow data subject access requests
- Implement right to be forgotten

**Hook Implementation**:
```bash
# Tag PII access
if [[ "$FILE" =~ (customer|user|personal|email) ]]; then
  echo "$TIMESTAMP $USER accessed PII: $FILE" >> "$GDPR_LOG"
fi
```

### SOC 2

**Requirements**:
- Access control logging
- Change management
- Incident response
- Security monitoring

**Hook Implementation**:
- Audit all tool executions (PostToolUse)
- Log all blocked operations (PreToolUse denials)
- Alert on security events
- Maintain tamper-proof logs

### HIPAA

**Requirements**:
- Encrypt data at rest and in transit
- Access control to PHI
- Audit trail for PHI access
- Minimum necessary access

**Hook Implementation**:
```bash
# Block access to PHI by default
if [[ "$FILE" =~ (medical|health|patient|diagnosis) ]]; then
  # Require special authorization
  if [ "$AUTHORIZED_PHI_ACCESS" != "true" ]; then
    echo "BLOCKED: PHI access requires authorization" >&2
    exit 1
  fi

  # Log to HIPAA audit trail
  echo "$TIMESTAMP $USER accessed PHI: $FILE (authorized)" >> "$HIPAA_LOG"
fi
```

---

## Incident Response

### Detection

**Indicators of Compromise**:
- Unusual spike in blocked operations
- Hook bypasses attempted
- Modified hook files
- Disabled hooks
- Abnormal file access patterns

**Monitoring**:
```bash
# Alert on suspicious activity
if grep -c "BLOCKED" "$SECURITY_LOG" | awk '{if ($1 > 10) exit 1}'; then
  # Send alert
  curl -X POST https://alerts.example.com/api/v1/alert \
    -d '{"severity":"high","message":"High rate of blocked operations"}'
fi
```

### Response

**Step 1: Contain**
```bash
# Disable all hooks immediately
mv .claude/settings.json .claude/settings.json.disabled

# Stop AI sessions
pkill -9 claude
```

**Step 2: Investigate**
```bash
# Review audit logs
tail -1000 ~/.claude/audit.jsonl | jq .

# Check for unauthorized modifications
git diff .claude/hooks/

# Review system logs
sudo journalctl -u claude -n 1000
```

**Step 3: Remediate**
```bash
# Restore known-good hooks
git checkout HEAD -- .claude/hooks/

# Re-enable with monitoring
mv .claude/settings.json.disabled .claude/settings.json
```

**Step 4: Document**
- Record incident details
- Timeline of events
- Root cause analysis
- Remediation actions
- Lessons learned

---

## Security Testing

### Static Analysis

```bash
# Check for common vulnerabilities
shellcheck .claude/hooks/*.sh

# Look for dangerous patterns
grep -rE 'eval|exec|bash -c|sh -c' .claude/hooks/

# Check file permissions
find .claude/hooks/ -type f -not -perm 700 -ls
```

### Dynamic Testing

```bash
# Test command injection
echo '{"tool_name":"Bash","tool_input":{"command":"ls; rm -rf /"}}' | \
  bash .claude/hooks/safety-check.sh

# Test path traversal
echo '{"tool_name":"Write","tool_input":{"file_path":"../../etc/passwd"}}' | \
  bash .claude/hooks/file-protection.sh

# Test pattern bypass
echo '{"tool_name":"Bash","tool_input":{"command":"RM=rm && $RM -rf /"}}' | \
  bash .claude/hooks/safety-check.sh
```

### Penetration Testing

**Recommended tests**:
1. Attempt to disable hooks
2. Try to bypass pattern matching
3. Test for race conditions
4. Attempt privilege escalation
5. Try to extract sensitive data
6. Test resource exhaustion
7. Verify fail-safe behavior

---

## Secure Development Lifecycle

### Phase 1: Design
- Threat modeling
- Security requirements
- Fail-safe vs. fail-open decision

### Phase 2: Implementation
- Secure coding guidelines
- Input validation
- Code review

### Phase 3: Testing
- Security testing
- Penetration testing
- Fuzzing

### Phase 4: Deployment
- Secure configuration
- Permission hardening
- Monitoring setup

### Phase 5: Maintenance
- Regular audits
- Patch management
- Incident response

---

## Next Steps

- [04-working-examples.md](./04-working-examples.md) - Secure hook examples
- [05-configuration-guide.md](./05-configuration-guide.md) - Secure configuration
- [README.md](./README.md) - Research summary
