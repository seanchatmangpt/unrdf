# Guard System Specification (SPARC Pseudocode Phase)

**Agent**: Guard System (Agent-4)
**Purpose**: Design poka-yoke enforcement for forbidden observations
**Status**: Specification Phase

---

## Executive Summary

The Guard System implements **poka-yoke** (error-proofing) to prevent observation of forbidden patterns before execution. Rather than detecting violations after they occur, guards intercept ALL probe methods and return deny decisions with structured receipts.

**Design Principle**: Make it impossible to observe secrets, not just punish the observer.

---

## 1. Forbidden Patterns Hierarchy (H)

### 1.1 Environment Variables (H1)

```
CATEGORY: Credentials & API Keys
├── Pattern: *TOKEN, *KEY, *SECRET, *PASSWORD
├── Scope: Process.env, system env vars
├── Impact: Credential exposure
└── Guard: guardEnvVarBlacklist()

CATEGORY: Cloud Provider Credentials (H2)
├── Pattern: AWS_*, AZURE_*, GCP_*
│   ├── AWS_ACCESS_KEY_ID
│   ├── AWS_SECRET_ACCESS_KEY
│   ├── AWS_SESSION_TOKEN
│   ├── AWS_SECURITY_TOKEN
│   └── AZURE_CLIENT_*, GCP_*
├── Scope: process.env (read-only)
├── Impact: Cloud infrastructure compromise
└── Guard: guardCloudCredentials()

CATEGORY: GitHub & Repository Access (H3)
├── Pattern: GITHUB_*, GITLAB_*, BITBUCKET_*
│   ├── GITHUB_TOKEN
│   ├── GITHUB_PAT (Personal Access Token)
│   ├── GIT_CREDENTIALS
│   └── GITLAB_PRIVATE_TOKEN
├── Scope: process.env
├── Impact: Repository compromise
└── Guard: guardVCSCredentials()

CATEGORY: API Services (H4)
├── Pattern: *_API_KEY, *_API_SECRET, *_BEARER_TOKEN
│   ├── DATABASE_*
│   ├── STRIPE_*, SLACK_*, SENDGRID_*
│   └── Any service API credentials
├── Scope: process.env
├── Impact: Third-party service compromise
└── Guard: guardServiceCredentials()

CATEGORY: Encryption Keys (H5)
├── Pattern: ENCRYPTION_KEY, CIPHER_KEY, *_PRIVATE_KEY
├── Scope: process.env, file system
├── Impact: Data encryption bypass
└── Guard: guardEncryptionKeys()

CATEGORY: Database Credentials (H6)
├── Pattern: DB_PASSWORD, *_CONN_STRING, MONGO_URL, DATABASE_*
├── Scope: process.env
├── Impact: Database compromise
└── Guard: guardDatabaseCredentials()

CATEGORY: Sensitive Configuration (H7)
├── Pattern: PRIVATE, SECRET, ADMIN, ROOT, MASTER
│   ├── ADMIN_PASSWORD
│   ├── ROOT_TOKEN
│   ├── MASTER_KEY
│   └── Any env var containing "PRIVATE"
├── Scope: process.env
├── Impact: System-level privilege escalation
└── Guard: guardSensitiveConfig()
```

### 1.2 File Paths (H8-H14)

```
CATEGORY: SSH Keys (H8)
├── Patterns:
│   ├── ~/.ssh/*
│   ├── ~/.ssh/id_rsa, id_dsa, id_ed25519
│   ├── ~/.ssh/known_hosts
│   ├── ~/.ssh/authorized_keys
│   └── /root/.ssh/**
├── Scope: fs.readFile, fs.readdir, stat
├── Impact: SSH access compromise
└── Guard: guardSSHKeys()

CATEGORY: AWS Configuration (H9)
├── Patterns:
│   ├── ~/.aws/credentials
│   ├── ~/.aws/config
│   ├── ~/.aws/credentials.json
│   └── /root/.aws/**
├── Scope: fs operations
├── Impact: AWS credential exposure
└── Guard: guardAWSFiles()

CATEGORY: NPM Registry (H10)
├── Patterns:
│   ├── ~/.npmrc
│   ├── ~/.npm-credentials.json
│   ├── .npmrc (in repos)
│   └── npm-auth.json
├── Scope: fs operations
├── Impact: NPM package pollution/hijacking
└── Guard: guardNPMRegistry()

CATEGORY: Environment Files (H11)
├── Patterns:
│   ├── .env, .env.local, .env.*.local
│   ├── .env.production
│   ├── config/secrets.yml
│   ├── .env.example (allowed if no secrets)
│   └── **/*.env
├── Scope: fs operations
├── Impact: Application-level secret exposure
└── Guard: guardEnvFiles()

CATEGORY: Git Configuration (H12)
├── Patterns:
│   ├── .git/config
│   ├── .git/hooks/*
│   ├── .gitcredentials
│   ├── .git/objects/** (metadata)
│   └── git-credentials-store
├── Scope: fs operations
├── Impact: Repository metadata/credential exposure
└── Guard: guardGitConfig()

CATEGORY: Kubernetes Secrets (H13)
├── Patterns:
│   ├── ~/.kube/config
│   ├── ~/.kube/clusters/**
│   ├── /etc/kubernetes/**
│   ├── kubeconfig.json
│   └── KUBECONFIG env var
├── Scope: fs, env vars
├── Impact: Kubernetes cluster compromise
└── Guard: guardKubernetesConfig()

CATEGORY: Docker Registry (H14)
├── Patterns:
│   ├── ~/.docker/config.json
│   ├── ~/.docker/auth.json
│   ├── ~/.dockercfg
│   └── docker-credentials
├── Scope: fs operations
├── Impact: Docker registry authentication bypass
└── Guard: guardDockerConfig()
```

### 1.3 Network Access (H15-H17)

```
CATEGORY: Allowed URLs Whitelist (H15)
├── Allowed:
│   ├── https://api.github.com/** (no auth)
│   ├── https://npm.pkg.github.com/** (only metadata)
│   ├── https://registry.npmjs.org/** (public)
│   ├── https://github.com/** (public repos)
│   ├── https://docs.* (documentation)
│   └── Internal allowlist config
├── Blocked: Everything else
└── Guard: guardNetworkURL()

CATEGORY: DNS Resolution (H16)
├── Patterns:
│   ├── localhost, 127.0.0.1, ::1 (allowed)
│   ├── Internal IPs: 10.*, 172.16.*, 192.168.* (blocked unless config)
│   ├── Metadata services: 169.254.* (blocked)
│   ├── Any: *.internal (blocked)
│   └── Cloud metadata: metadata.google.internal, 169.254.169.254 (blocked)
├── Impact: Cloud metadata extraction, internal network access
└── Guard: guardDNSResolution()

CATEGORY: Port Scanning (H17)
├── Patterns:
│   ├── Any port < 1024 except (80, 443)
│   ├── Privileged ports (22, 3306, 5432, 6379, etc.)
│   └── Metadata ports (8080, 8888, 8169, 4444)
├── Impact: Service enumeration, internal topology discovery
└── Guard: guardPortAccess()
```

### 1.4 Syscalls & Process Access (H18-H21)

```
CATEGORY: /proc Filesystem (H18)
├── Forbidden Paths:
│   ├── /proc/self/** (process env, memory map)
│   ├── /proc/*/environ (all process env vars)
│   ├── /proc/*/fd/** (file descriptor enumeration)
│   ├── /proc/meminfo, /proc/cpuinfo
│   ├── /proc/net/** (network enumeration)
│   └── /proc/sys/** (system settings)
├── Impact: Process enumeration, system configuration exposure
└── Guard: guardProcFilesystem()

CATEGORY: /etc Filesystem (H19)
├── Forbidden Paths:
│   ├── /etc/passwd, /etc/shadow, /etc/group
│   ├── /etc/hosts, /etc/resolv.conf
│   ├── /etc/sudoers, /etc/sudo**
│   ├── /etc/ssl/**, /etc/pki/**
│   └── /etc/security/**
├── Impact: User enumeration, system compromise
└── Guard: guardEtcFilesystem()

CATEGORY: System User Enumeration (H20)
├── Forbidden Operations:
│   ├── getpwuid(), getpwnam() (user info)
│   ├── getgrgid(), getgrnam() (group info)
│   ├── os.userInfo() (current user)
│   ├── whoami, id, groups commands
│   └── FS stat on user home dirs
├── Impact: User/privilege enumeration
└── Guard: guardUserEnumeration()

CATEGORY: System Command Execution (H21)
├── Forbidden Operations:
│   ├── child_process.execSync with /, /bin, /usr/bin
│   ├── child_process.exec (raw shell commands)
│   ├── Shell metacharacters in args (>, |, &&, ;)
│   ├── env variable injection in execSync
│   └── Commands: env, printenv, set, declare
├── Impact: Arbitrary command execution, env exposure
└── Guard: guardCommandExecution()
```

### 1.5 Module & Import Access (H22-H23)

```
CATEGORY: Module Requires (H22)
├── Forbidden Modules:
│   ├── os (except os.platform())
│   ├── child_process.* (except controlled spawn)
│   ├── fs.* (except whitelisted paths)
│   ├── crypto.* (except public key ops)
│   └── vm, worker_threads, cluster
├── Impact: Process/system manipulation
└── Guard: guardModuleAccess()

CATEGORY: Stack Trace / Error Messages (H23)
├── Forbidden:
│   ├── Exposing full path in error messages
│   ├── Stack traces containing /home/*, /root/*, ~/.*/
│   ├── Error messages with process.cwd()
│   ├── Module paths in stack frames
│   └── __filename, __dirname in public output
├── Impact: Filesystem path enumeration
└── Guard: guardStackTraceExposure()
```

### 1.6 Metadata & Inference Attacks (H24-H25)

```
CATEGORY: Timing Side Channels (H24)
├── Forbidden:
│   ├── Comparing secrets with normal ==
│   ├── Early exit on mismatch (enables timing attack)
│   ├── fs.existsSync for secret paths
│   ├── Timing-dependent error messages
│   └── Variable-time algorithms
├── Impact: Secret inference via timing
└── Guard: guardTimingSideChannels()

CATEGORY: Error Message Leakage (H25)
├── Forbidden:
│   ├── "File not found: ~/.ssh/id_rsa" (implies path checked)
│   ├── "Invalid AWS credential: AKIAIOSFODNN7EXAMPLE" (partial key)
│   ├── Stack traces in API responses
│   ├── Line numbers pointing to secret locations
│   └── Database error queries (e.g., "No user with ID X")
├── Impact: Information leakage attacks
└── Guard: guardErrorMessages()
```

---

## 2. Guard Registry Pattern

### 2.1 Architecture

```
SYSTEM: Guard Registry
├── GuardRegistry (Central registry)
│   ├── _rules: Map<guardId, GuardRule>
│   ├── _interceptors: Map<operation, InterceptorFn>
│   ├── _cache: LRU (decisions, TTL 5min)
│   └── _auditLog: AuditLogger
│
├── GuardRule (Policy enforcement)
│   ├── id: string (e.g., "G-H1-ENV-TOKEN")
│   ├── category: string
│   ├── forbidden: Pattern[] (regex or glob)
│   ├── severity: "critical" | "high" | "medium"
│   ├── handler: (value, context) => AllowDeny
│   └── receipt: ReceiptGenerator
│
├── Interceptor (Operation-level)
│   ├── operation: string (e.g., "env-var-access")
│   ├── guards: GuardRule[]
│   ├── evaluateAll: () => AllowDeny
│   └── audit: () => AuditEntry
│
└── AllowDeny
    ├── allowed: boolean
    ├── reason: string (if denied)
    ├── receipt: DenialReceipt
    └── context: OperationContext
```

### 2.2 Poka-Yoke Design

**Principle**: Make forbidden operations impossible, not just detected.

```
INTERCEPTION LAYERS:

Layer 1: Module Load Time (Earliest)
  └─ Wrap require('process'), Object.freeze(process.env)
     [Prevents modification]

Layer 2: API Access (Before Operation)
  └─ Intercept fs.readFile, process.env[], etc.
     [Prevents read]

Layer 3: Execution Guard (Before Effect)
  └─ Wrap child_process.execSync, execFile
     [Prevents execution]

Layer 4: Audit Log (After Decision)
  └─ Record decision with receipt
     [For accountability]

GUARANTEE: If denied at Layer 1-3, operation NEVER executes
```

---

## 3. Guard Enforcement Rules (Pseudocode)

### 3.1 Environment Variable Guard

```
ALGORITHM: CheckEnvironmentVariable
INPUT: varName (string)
OUTPUT: AllowDeny | DENY with DenialReceipt

CONSTANTS:
    FORBIDDEN_PATTERNS = [
        "*TOKEN", "*KEY", "*SECRET", "*PASSWORD",
        "AWS_*", "AZURE_*", "GCP_*",
        "GITHUB_*", "GITLAB_*",
        "*_API_KEY", "*_API_SECRET",
        "DB_*", "DATABASE_*",
        "ENCRYPTION_*", "CIPHER_*",
        "ADMIN_*", "ROOT_*", "MASTER_*"
    ]
    SEVERITY = "CRITICAL"

BEGIN
    // Input validation
    IF varName is not string OR varName.length = 0 THEN
        RETURN DENY("Invalid variable name format")
    END IF

    // Normalize for comparison
    normalized ← varName.toUpperCase().trim()

    // Check against forbidden patterns
    FOR EACH pattern IN FORBIDDEN_PATTERNS DO
        IF patternMatches(normalized, pattern) THEN
            receipt ← GenerateDenialReceipt(
                operation: "env-var-access",
                target: varName,
                reason: "FORBIDDEN_CREDENTIAL_PATTERN",
                severity: "CRITICAL",
                pattern: pattern
            )

            // Log immediately (synchronous for audit trail)
            AuditLog.record({
                timestamp: NOW(),
                guardId: "G-H1-ENV-TOKEN",
                decision: "DENY",
                operation: "env-var-access",
                target: varName,
                reason: "Pattern matched: " + pattern,
                receipt: receipt.id
            })

            RETURN AllowDeny(
                allowed: false,
                reason: "Environment variable matches forbidden pattern",
                receipt: receipt
            )
        END IF
    END FOR

    // Not forbidden
    AuditLog.record({
        timestamp: NOW(),
        guardId: "G-H1-ENV-TOKEN",
        decision: "ALLOW",
        operation: "env-var-access",
        target: varName
    })

    RETURN AllowDeny(allowed: true)
END


SUBROUTINE: PatternMatches
INPUT: value (string), pattern (string with wildcards)
OUTPUT: matches (boolean)

BEGIN
    // Convert wildcard pattern to regex
    // Example: "AWS_*" → /^AWS_.*$/

    regexPattern ← pattern
        .replace(".", "\\.")
        .replace("*", ".*")
        .replace("?", ".")

    regex ← new RegExp("^" + regexPattern + "$")

    RETURN regex.test(value)
END
```

### 3.2 File Path Guard

```
ALGORITHM: CheckFilePathAccess
INPUT: operation (string: "read"|"write"|"stat"), filePath (string)
OUTPUT: AllowDeny | DENY with DenialReceipt

CONSTANTS:
    FORBIDDEN_PATHS = [
        {pattern: "~/.ssh/**", category: "SSH_KEYS"},
        {pattern: "~/.aws/**", category: "AWS_CONFIG"},
        {pattern: "~/.npmrc", category: "NPM_REGISTRY"},
        {pattern: ".env*", category: "ENV_FILE"},
        {pattern: ".git/config", category: "GIT_CONFIG"},
        {pattern: "~/.kube/config", category: "KUBE_CONFIG"},
        {pattern: "~/.docker/**", category: "DOCKER_CONFIG"},
        {pattern: "/etc/passwd", category: "SYSTEM_USER_DB"},
        {pattern: "/etc/shadow", category: "SYSTEM_PASSWORD_DB"},
        {pattern: "/proc/self/**", category: "PROCESS_MEMORY"},
        {pattern: "/proc/*/environ", category: "PROCESS_ENV"}
    ]

BEGIN
    // Input validation
    IF filePath is not string OR filePath.length = 0 THEN
        RETURN DENY("Invalid file path")
    END IF

    // Normalize path (resolve ~, symlinks)
    normalizedPath ← resolvePath(filePath)

    // Check against forbidden path list
    FOR EACH forbidden IN FORBIDDEN_PATHS DO
        IF pathMatches(normalizedPath, forbidden.pattern) THEN
            receipt ← GenerateDenialReceipt(
                operation: "fs-" + operation,
                target: filePath,
                reason: "FORBIDDEN_FILE_PATH",
                category: forbidden.category,
                resolvedPath: normalizedPath
            )

            AuditLog.record({
                timestamp: NOW(),
                guardId: "G-H8-" + forbidden.category,
                decision: "DENY",
                operation: "fs-" + operation,
                target: filePath,
                resolvedPath: normalizedPath,
                reason: "Forbidden path category: " + forbidden.category,
                receipt: receipt.id
            })

            RETURN DENY(
                reason: "Access to sensitive file path forbidden",
                receipt: receipt
            )
        END IF
    END FOR

    // Not forbidden - allow
    RETURN ALLOW()
END


SUBROUTINE: ResolvePath
INPUT: filePath (string)
OUTPUT: resolvedPath (string)

BEGIN
    // Expand ~ to home directory
    IF filePath.startsWith("~") THEN
        homeDir ← getHomeDirectory()
        filePath ← homeDir + filePath.substring(1)
    END IF

    // Resolve symlinks and relative paths
    resolvedPath ← realPath(filePath)

    // Normalize: forward slashes, no trailing slash
    resolvedPath ← resolvedPath.replace("\\", "/")
    resolvedPath ← resolvedPath.replace(/\/+/g, "/")

    IF resolvedPath.endsWith("/") AND resolvedPath.length > 1 THEN
        resolvedPath ← resolvedPath.substring(0, resolvedPath.length - 1)
    END IF

    RETURN resolvedPath
END


SUBROUTINE: PathMatches
INPUT: resolvedPath (string), pattern (string)
OUTPUT: matches (boolean)

BEGIN
    // Handle ** (any number of directories)
    globPattern ← pattern
        .replace(".", "\\.")
        .replace("/**", "(/.*)?")
        .replace("*", "[^/]*")
        .replace("?", "[^/]")

    regex ← new RegExp("^" + globPattern + "$")
    RETURN regex.test(resolvedPath)
END
```

### 3.3 Network URL Guard

```
ALGORITHM: CheckNetworkURL
INPUT: url (string), method (string: "GET"|"POST"|etc)
OUTPUT: AllowDeny | DENY with DenialReceipt

CONSTANTS:
    ALLOWED_HOSTS = [
        {hostname: "api.github.com", paths: ["/repos/**"], methods: ["GET"]},
        {hostname: "registry.npmjs.org", paths: ["**"], methods: ["GET"]},
        {hostname: "github.com", paths: ["/", "/search/**"], methods: ["GET"]},
        {hostname: "docs.example.com", paths: ["**"], methods: ["GET"]},
        // ... more whitelist entries
    ]

    FORBIDDEN_DOMAINS = [
        "metadata.google.internal",
        "169.254.169.254",  // AWS metadata
        "*.internal",
        "localhost:8080",
        "*.local"
    ]

BEGIN
    // Parse URL
    parsed ← parseURL(url)
    hostname ← parsed.hostname
    pathname ← parsed.pathname

    // Validate URL format
    IF parsed is null THEN
        RETURN DENY("Invalid URL format")
    END IF

    // Check forbidden domains first (most restrictive)
    FOR EACH forbidden IN FORBIDDEN_DOMAINS DO
        IF hostMatches(hostname, forbidden) THEN
            receipt ← GenerateDenialReceipt(
                operation: "network-request",
                target: url,
                reason: "FORBIDDEN_HOST",
                hostname: hostname
            )

            AuditLog.record({
                timestamp: NOW(),
                guardId: "G-H15-NETWORK-URL",
                decision: "DENY",
                operation: "network-request",
                target: url,
                hostname: hostname,
                reason: "Forbidden domain",
                receipt: receipt.id
            })

            RETURN DENY("Network access to forbidden host", receipt)
        END IF
    END FOR

    // Check against whitelist
    matched ← false
    FOR EACH allowed IN ALLOWED_HOSTS DO
        IF hostMatches(hostname, allowed.hostname) AND
           methodAllowed(method, allowed.methods) AND
           pathMatches(pathname, allowed.paths) THEN
            matched ← true
            BREAK
        END IF
    END FOR

    IF NOT matched THEN
        receipt ← GenerateDenialReceipt(
            operation: "network-request",
            target: url,
            reason: "NOT_IN_WHITELIST",
            hostname: hostname
        )

        RETURN DENY("Host not in allowed whitelist", receipt)
    END IF

    RETURN ALLOW()
END
```

### 3.4 Command Execution Guard

```
ALGORITHM: CheckCommandExecution
INPUT: command (string), args (array), options (object)
OUTPUT: AllowDeny | DENY with DenialReceipt

CONSTANTS:
    FORBIDDEN_COMMANDS = [
        "env", "printenv", "set", "declare",
        "cat /etc/passwd", "cat /etc/shadow",
        "whoami", "id", "groups",
        "ps aux", "ps -ef"
    ]

    DANGEROUS_PATTERNS = [
        "|",      // Pipe
        "&",      // Background/AND
        ";",      // Sequence
        ">",      // Redirect
        "<",      // Input redirect
        "$()",    // Command substitution
        "`",      // Backtick substitution
        "&&",     // Conditional AND
        "||"      // Conditional OR
    ]

BEGIN
    // Validate inputs
    IF command is not string THEN
        RETURN DENY("Command must be string")
    END IF

    IF args is not array THEN
        RETURN DENY("Arguments must be array")
    END IF

    // Check against forbidden command whitelist
    FOR EACH forbidden IN FORBIDDEN_COMMANDS DO
        IF command.toLowerCase().contains(forbidden) THEN
            receipt ← GenerateDenialReceipt(
                operation: "command-execution",
                target: command,
                reason: "FORBIDDEN_COMMAND",
                command: command
            )

            RETURN DENY("Forbidden command", receipt)
        END IF
    END FOR

    // Check for shell metacharacters in args (shell injection)
    FOR EACH arg IN args DO
        FOR EACH pattern IN DANGEROUS_PATTERNS DO
            IF arg.contains(pattern) THEN
                receipt ← GenerateDenialReceipt(
                    operation: "command-execution",
                    target: command,
                    reason: "SHELL_INJECTION_ATTEMPT",
                    suspiciousArg: arg,
                    pattern: pattern
                )

                RETURN DENY("Dangerous shell pattern in arguments", receipt)
            END IF
        END FOR
    END FOR

    // Check if environment would expose secrets
    IF options.env is not null THEN
        FOR EACH envKey IN Object.keys(options.env) DO
            guardCheck ← CheckEnvironmentVariable(envKey)
            IF NOT guardCheck.allowed THEN
                receipt ← GenerateDenialReceipt(
                    operation: "command-execution",
                    target: command,
                    reason: "CREDENTIAL_IN_ENV",
                    envVar: envKey
                )

                RETURN DENY("Command would expose credentials via env", receipt)
            END IF
        END FOR
    END IF

    RETURN ALLOW()
END
```

---

## 4. Integration with @unrdf/hooks Framework

### 4.1 Hook-Guard Integration Pattern

```
ARCHITECTURE: Hooks + Guard Interception

Hook Lifecycle:
    1. Hook Definition
       └─> Guard-wrap hook definition

    2. Hook Registration
       └─> Register guards in GuardRegistry
       └─> Create interceptors for hook inputs

    3. Hook Trigger (Event occurs)
       └─> Guard.evaluate(event, context)
       └─> IF denied: return DenialReceipt, skip hook
       └─> IF allowed: execute hook

    4. Hook Execution
       └─> All I/O guarded (fs, env, network)
       └─> Guard.audit(execution)

    5. Hook Result
       └─> Guard.checkOutput(result)
       └─> Return result or DenialReceipt


INTEGRATION CODE (Pseudocode):

CLASS: GuardedHookExecutor IMPLEMENTS HookExecutor

    _guardRegistry: GuardRegistry
    _hooks: Map<hookId, HookDef>

    CONSTRUCTOR(guardRegistry):
        _guardRegistry ← guardRegistry
        _hooks ← new Map()

    registerHook(hookDef):
        // Wrap all hook inputs with guards
        guardedHook ← wrapHookWithGuards(hookDef, _guardRegistry)
        _hooks.set(hookDef.id, guardedHook)

        // Register input guards
        FOR EACH input IN hookDef.inputs DO
            _guardRegistry.registerInputGuard(
                operation: "hook-input-" + hookDef.id,
                inputName: input.name,
                rules: input.securityRules
            )
        END FOR

    ASYNC executeHook(hookId, event, context):
        hook ← _hooks.get(hookId)
        IF hook is null THEN
            RETURN Error("Hook not found: " + hookId)
        END IF

        // Phase 1: Guard event/context
        guardCheck ← _guardRegistry.evaluateAll(
            operation: "hook-trigger",
            event: event,
            context: context
        )

        IF NOT guardCheck.allowed THEN
            // Log denial
            auditEntry ← {
                timestamp: NOW(),
                hookId: hookId,
                decision: "DENY_TRIGGER",
                reason: guardCheck.reason,
                receipt: guardCheck.receipt
            }

            RETURN DenialReceipt(
                message: "Hook execution denied",
                reason: guardCheck.reason,
                receipt: guardCheck.receipt,
                auditEntry: auditEntry
            )
        END IF

        // Phase 2: Execute hook in guarded context
        guardedContext ← createGuardedContext(context, _guardRegistry)

        TRY
            result ← hook.execute(event, guardedContext)

            // Phase 3: Guard output
            outputCheck ← _guardRegistry.evaluateOutput(result)
            IF NOT outputCheck.allowed THEN
                RETURN DenialReceipt(
                    message: "Hook output denied",
                    reason: outputCheck.reason
                )
            END IF

            // Phase 4: Audit success
            AuditLog.record({
                timestamp: NOW(),
                hookId: hookId,
                decision: "ALLOW_EXECUTE",
                operation: "hook-execution",
                status: "success"
            })

            RETURN result

        CATCH error:
            // Ensure error doesn't leak secrets
            sanitized ← sanitizeErrorMessage(error, _guardRegistry)

            AuditLog.record({
                timestamp: NOW(),
                hookId: hookId,
                decision: "ALLOW_EXECUTE",
                operation: "hook-execution",
                status: "error",
                error: sanitized
            })

            RETURN Error(sanitized)
        END TRY

    FUNCTION: CreateGuardedContext
    INPUT: context (HookContext), guardRegistry (GuardRegistry)
    OUTPUT: guardedContext (GuardedHookContext)

    BEGIN
        // Wrap context properties with getters
        guardedContext ← {
            get user():
                userCheck ← guardRegistry.evaluate(
                    operation: "context-user-access",
                    value: context.user
                )
                IF NOT userCheck.allowed THEN
                    THROW Error("User access denied: " + userCheck.reason)
                END IF
                RETURN context.user

            get environment():
                // Return only non-secret env vars
                RETURN filterEnvironment(context.environment, guardRegistry)

            get fileSystem():
                // Wrap fs operations with guards
                RETURN createGuardedFS(context.fileSystem, guardRegistry)

            // ... more guarded properties
        }

        RETURN guardedContext
    END
END
```

### 4.2 Policy Pack Integration

```
STRUCTURE: GuardPolicy within PolicyPack

PolicyPack.manifest:
    {
        "id": "uuid",
        "meta": { "name": "security-policy-v1", ... },
        "guards": [
            {
                "id": "G-H1-ENV-TOKEN",
                "category": "environment-variables",
                "rules": [
                    {
                        "pattern": "*TOKEN",
                        "severity": "CRITICAL",
                        "action": "DENY"
                    },
                    {
                        "pattern": "*PASSWORD",
                        "severity": "CRITICAL",
                        "action": "DENY"
                    }
                ]
            },
            {
                "id": "G-H8-SSH-KEYS",
                "category": "file-paths",
                "rules": [
                    {
                        "pattern": "~/.ssh/**",
                        "severity": "CRITICAL",
                        "action": "DENY"
                    }
                ]
            }
        ],
        "hooks": [
            {
                "name": "on-document-write",
                "file": "on-document-write.mjs",
                "guards": ["G-H11-ENV-FILE", "G-H23-STACK-TRACE"],
                "enabled": true
            }
        ]
    }


LOADER PSEUDOCODE:

ASYNC FUNCTION: LoadGuardPolicies
INPUT: policyPackManager (PolicyPackManager)
OUTPUT: GuardRegistry

BEGIN
    guardRegistry ← new GuardRegistry()

    // Load all active policy packs
    activePacks ← policyPackManager.getActivePolicyPacks()

    FOR EACH pack IN activePacks DO
        // Load guard definitions
        FOR EACH guardDef IN pack.guards DO
            // Create guard rules from policy
            rules ← []
            FOR EACH rule IN guardDef.rules DO
                guardRule ← {
                    id: guardDef.id,
                    pattern: rule.pattern,
                    severity: rule.severity,
                    action: rule.action,
                    handler: compilePattern(rule.pattern)
                }
                rules.append(guardRule)
            END FOR

            // Register in guard registry
            guardRegistry.register(guardDef.id, rules)

            // Link to hooks that use these guards
            FOR EACH hook IN pack.hooks DO
                IF hook.guards.contains(guardDef.id) THEN
                    guardRegistry.linkHook(hook.name, guardDef.id)
                END IF
            END FOR
        END FOR
    END FOR

    RETURN guardRegistry
END
```

---

## 5. Denial Receipt & Audit Schema

### 5.1 Denial Receipt Structure

```json
{
  "id": "uuid-v4",
  "timestamp": "2025-01-15T10:30:00.000Z",
  "operation": "env-var-access|fs-read|fs-write|network-request|command-execution|hook-trigger",
  "severity": "CRITICAL|HIGH|MEDIUM|LOW",

  "guardId": "G-H1-ENV-TOKEN",
  "guardCategory": "environment-variables|file-paths|network|process|module",

  "decision": "DENY",
  "reasonCode": "FORBIDDEN_CREDENTIAL_PATTERN|FORBIDDEN_FILE_PATH|FORBIDDEN_HOST|SHELL_INJECTION_ATTEMPT",
  "message": "Human-readable denial reason",

  "context": {
    "target": "string (what was accessed)",
    "pattern": "string (which rule matched)",
    "resolvedValue": "sanitized (no actual secrets)"
  },

  "remediation": {
    "action": "string (what user should do)",
    "documentation": "string (link to security docs)",
    "steps": [
      "Step 1: ...",
      "Step 2: ..."
    ]
  },

  "auditTrail": {
    "callerStackTrace": [
      "sanitized frame 1",
      "sanitized frame 2"
    ],
    "module": "string (sanitized module path)",
    "caller": "string (function name)"
  },

  "environment": {
    "nodeVersion": "string",
    "platform": "string",
    "environment": "development|test|production"
  }
}
```

### 5.2 Audit Log Schema

```
TABLE: GuardAuditLog

Columns:
├── id: UUID (primary key)
├── timestamp: ISO8601 timestamp
├── guardId: string ("G-H1-*")
├── operation: string (category of operation)
├── decision: string ("ALLOW" | "DENY")
├── severity: string ("CRITICAL" | "HIGH" | "MEDIUM" | "LOW")
├── targetResource: string (what was accessed, sanitized)
├── reasonCode: string (if denied)
├── denialReceiptId: UUID (link to denial receipt)
├── callerModule: string (sanitized module path)
├── callerFunction: string (function name)
├── policyPackId: UUID (which policy pack enforced rule)
├── detailsJSON: JSON (sanitized details)
└── createdAt: timestamp (insertion time)

Indexes:
├── idx_timestamp (for time-range queries)
├── idx_decision (filter allows/denies)
├── idx_guardId (which rule triggered)
├── idx_severity (filter by severity)
└── idx_denialReceiptId (link to denial receipts)


EXAMPLE RECORDS:

Record 1 (Allowed):
{
  "id": "12345-abc",
  "timestamp": "2025-01-15T10:30:00.000Z",
  "guardId": "G-H1-ENV-TOKEN",
  "operation": "env-var-access",
  "decision": "ALLOW",
  "severity": "LOW",
  "targetResource": "NODE_ENV",
  "reasonCode": null,
  "callerModule": "src/config.mjs",
  "callerFunction": "loadConfig"
}

Record 2 (Denied):
{
  "id": "12346-def",
  "timestamp": "2025-01-15T10:30:01.000Z",
  "guardId": "G-H1-ENV-TOKEN",
  "operation": "env-var-access",
  "decision": "DENY",
  "severity": "CRITICAL",
  "targetResource": "AWS_SECRET_ACCESS_KEY",
  "reasonCode": "FORBIDDEN_CREDENTIAL_PATTERN",
  "denialReceiptId": "54321-xyz",
  "callerModule": "src/utils/unsafe.mjs",
  "callerFunction": "getSecret",
  "policyPackId": "policy-pack-v1",
  "detailsJSON": {
    "pattern": "AWS_*",
    "matchedRule": "AWS_SECRET_ACCESS_KEY matches AWS_*"
  }
}
```

### 5.3 Query Capabilities

```
QUERIES:

1. Denial Timeline:
   SELECT * FROM GuardAuditLog
   WHERE decision = 'DENY'
   AND timestamp >= (NOW - INTERVAL 24 HOUR)
   ORDER BY timestamp DESC

   → Shows all attempted forbidden access in last 24h

2. Guard Effectiveness:
   SELECT guardId, COUNT(*) as attempts,
          SUM(CASE WHEN decision='DENY' THEN 1 ELSE 0 END) as blocked
   FROM GuardAuditLog
   GROUP BY guardId
   ORDER BY blocked DESC

   → Which guards are most active/effective

3. Risk Assessment:
   SELECT severity, COUNT(*) as count
   FROM GuardAuditLog
   WHERE decision = 'DENY'
   GROUP BY severity

   → Distribution of denial severity

4. Compliance Report:
   SELECT DISTINCT callerModule
   FROM GuardAuditLog
   WHERE decision = 'DENY'
   AND severity IN ('CRITICAL', 'HIGH')

   → Which modules are triggering high-severity denials

5. False Positive Detection:
   SELECT operation, targetResource, COUNT(*) as count
   FROM GuardAuditLog
   WHERE decision = 'DENY'
   AND severity = 'LOW'
   GROUP BY operation, targetResource
   HAVING count > 10

   → Potential false positives to review
```

---

## 6. Checklist: Guard Policy Specification

### 6.1 Forbidden Patterns Complete Checklist

- [x] **H1: Environment Token Variables** - *TOKEN, *KEY, *SECRET, *PASSWORD
- [x] **H2: Cloud Credentials** - AWS_*, AZURE_*, GCP_*
- [x] **H3: VCS Credentials** - GITHUB_*, GITLAB_*, BITBUCKET_*
- [x] **H4: Service API Keys** - *_API_KEY, *_API_SECRET
- [x] **H5: Encryption Keys** - ENCRYPTION_*, CIPHER_*
- [x] **H6: Database Credentials** - DB_*, DATABASE_*, *_CONN_STRING
- [x] **H7: Sensitive Config** - ADMIN_*, ROOT_*, MASTER_*
- [x] **H8: SSH Keys** - ~/.ssh/*, /root/.ssh/**
- [x] **H9: AWS Files** - ~/.aws/credentials, ~/.aws/config
- [x] **H10: NPM Registry** - ~/.npmrc, npm-auth.json
- [x] **H11: Environment Files** - .env, .env.local, .env.*.local
- [x] **H12: Git Configuration** - .git/config, .gitcredentials
- [x] **H13: Kubernetes Config** - ~/.kube/config, kubeconfig.json
- [x] **H14: Docker Registry** - ~/.docker/config.json
- [x] **H15: Network Allowlist** - URL whitelist enforcement
- [x] **H16: DNS Resolution** - Metadata service blocking
- [x] **H17: Port Access** - Privileged port blocking
- [x] **H18: /proc Filesystem** - Process memory/env enumeration
- [x] **H19: /etc Filesystem** - System file protection
- [x] **H20: User Enumeration** - getpwuid, getpwnam blocking
- [x] **H21: Command Execution** - execSync/exec guard, shell injection
- [x] **H22: Module Requires** - os, child_process, fs whitelisting
- [x] **H23: Stack Trace Exposure** - Path sanitization in errors
- [x] **H24: Timing Channels** - Side-channel protection
- [x] **H25: Error Messages** - Information leakage prevention

### 6.2 Guard Registry Pattern Checklist

- [x] **Central Registry** - GuardRegistry with _rules, _interceptors, _cache
- [x] **Poka-Yoke Layers** - 4-layer interception (Module, API, Execution, Audit)
- [x] **Interceptor Design** - Operation-level guards with decision caching
- [x] **Receipt Generation** - Structured denial receipts with remediation
- [x] **Audit Logging** - All decisions logged (allow/deny) with context
- [x] **Hook Integration** - GuardedHookExecutor wraps hook lifecycle
- [x] **Policy Pack Linking** - Guards defined in policy packs

### 6.3 Pseudocode Rules Checklist

- [x] **Guard H1: CheckEnvironmentVariable** - Pattern matching, audit logging
- [x] **Guard H8: CheckFilePathAccess** - Path resolution, glob matching
- [x] **Guard H15: CheckNetworkURL** - URL whitelist enforcement
- [x] **Guard H21: CheckCommandExecution** - Shell metacharacter detection
- [x] **Integration: GuardedHookExecutor** - Hook wrapping, context guarding
- [x] **Policy Pack Loader** - Guard registration from policy pack manifest

### 6.4 Audit Schema Checklist

- [x] **Denial Receipt** - 19 fields covering operation, reason, remediation, context
- [x] **Audit Log** - 13 columns for decision tracking, queryable
- [x] **Example Records** - Both allow and deny scenarios documented
- [x] **Query Templates** - 5 compliance/forensics queries specified

---

## 7. Deployment & Activation

### 7.1 Guard System Initialization

```
SEQUENCE: System Startup

1. Load Guard Policies (EARLIEST)
   └─ policyPackManager.loadAllPolicies()
   └─ guardRegistry.registerAllGuards()
   └─ guardRegistry.setupInterceptors()

2. Wrap Standard Libraries (IMMEDIATE)
   └─ Object.freeze(process.env)
   └─ Intercept require('fs').readFile
   └─ Intercept child_process.execSync
   └─ Intercept http.request

3. Initialize Hook Executor
   └─ GuardedHookExecutor(guardRegistry)
   └─ Register all hooks with guards

4. Setup Audit Logger
   └─ AuditLogger.initialize(persistenceBackend)
   └─ AuditLogger.connect(guardRegistry)

5. Verification
   └─ guardRegistry.selfTest()
   └─ Verify all guards responding
```

### 7.2 Testing Verification

```
TEST SUITE: GuardSystemVerification

Test Case: T1 - Environment Variable Blocking
    Input: process.env.AWS_SECRET_ACCESS_KEY
    Expected: DENY with receipt
    Verify: Audit log entry + receipt file exists

Test Case: T2 - File Path Blocking
    Input: fs.readFile("~/.ssh/id_rsa")
    Expected: DENY + Error thrown
    Verify: Operation never executed

Test Case: T3 - Network Allowlist
    Input: fetch("https://internal.metadata.service/")
    Expected: DENY
    Verify: Request never sent

Test Case: T4 - Command Execution Guarding
    Input: execSync("cat /etc/passwd")
    Expected: DENY
    Verify: Command never executed

Test Case: T5 - Error Sanitization
    Input: fs.readFile with PERMISSION_DENIED
    Expected: Error message without full path
    Verify: Stack trace contains no /.../
```

---

## 8. Performance & Complexity Analysis

### 8.1 Decision Latency

```
OPERATION: Guard.evaluate(operation, target)

Time Complexity: O(n) where n = number of rules
    - Pattern matching: O(n * m) where m = pattern length
    - Cache hit: O(1)
    - Audit logging: O(1) with async queue

Space Complexity: O(k) where k = cache size
    - LRU cache: configurable, default 10,000 entries
    - Each entry: ~200 bytes (pattern + decision + receipt)
    - Total memory: ~2MB for default cache

Optimization: Cache frequently accessed values
    - NODE_ENV (always allowed)
    - Safe paths (/usr/local/lib, /opt, etc.)
    - Allowed hosts (api.github.com, registry.npmjs.org)

Expected Latency:
    - Cache hit: <0.1ms
    - Cache miss, allowed: 0.5-2ms
    - Cache miss, denied: 1-5ms (includes receipt generation)
    - Audit write (async): 5-20ms (non-blocking)
```

### 8.2 Audit Log Growth

```
METRICS: Audit Log Scaling

Assumptions:
    - 1000 guards active
    - 10,000 operations per hour
    - 90% ALLOW, 10% DENY
    - Receipt JSON ~500 bytes average

Daily Growth:
    - Operations: 240,000/day
    - Denial receipts: 24,000/day
    - Denial receipt storage: 24,000 * 500B = 12MB/day
    - Audit log entries: 240,000 * ~150B = 36MB/day
    - Total: ~48MB/day

Retention Policy:
    - Denial receipts: Keep 90 days (1.08GB)
    - Audit logs: Compress and archive after 30 days
    - Full retention: 1.5TB/year

Recommended: Weekly archival to cold storage, queryable index in hot storage
```

---

## 9. Example: Guard Enforcement in Action

### 9.1 Scenario 1: Environment Variable Access Attempt

```
EVENT: Code attempts to access AWS credentials

Code:
    const secret = process.env.AWS_SECRET_ACCESS_KEY;

Execution Flow:

1. Module Load Time
   └─ GuardRegistry.setupInterceptors()
   └─ Wrap process.env with Proxy

2. Variable Access
   └─ process.env.AWS_SECRET_ACCESS_KEY
   └─ Proxy trap: get('AWS_SECRET_ACCESS_KEY')
   └─ Call: guardRegistry.checkEnvVar('AWS_SECRET_ACCESS_KEY')

3. Guard Evaluation (G-H1-ENV-TOKEN)
   └─ Pattern: "AWS_*"
   └─ Rule: matches "AWS_SECRET_ACCESS_KEY"? YES
   └─ Action: DENY

4. Denial Receipt Generation
   Receipt:
   {
     "id": "uuid-1234",
     "timestamp": "2025-01-15T10:30:00Z",
     "operation": "env-var-access",
     "guardId": "G-H1-ENV-TOKEN",
     "reasonCode": "FORBIDDEN_CREDENTIAL_PATTERN",
     "message": "Access to AWS credentials forbidden",
     "context": {
       "target": "AWS_SECRET_ACCESS_KEY",
       "pattern": "AWS_*"
     },
     "remediation": {
       "action": "Use IAM roles or temporary credentials",
       "documentation": "https://docs.example.com/security/env-vars",
       "steps": [
         "Remove hardcoded credentials from code",
         "Use AWS_ASSUME_ROLE instead",
         "Inject credentials via deployment system"
       ]
     }
   }

5. Audit Logging
   Entry:
   {
     "id": "audit-1234",
     "timestamp": "2025-01-15T10:30:00Z",
     "guardId": "G-H1-ENV-TOKEN",
     "operation": "env-var-access",
     "decision": "DENY",
     "severity": "CRITICAL",
     "targetResource": "AWS_SECRET_ACCESS_KEY",
     "reasonCode": "FORBIDDEN_CREDENTIAL_PATTERN",
     "denialReceiptId": "uuid-1234",
     "callerModule": "src/config.mjs",
     "callerFunction": "loadSecrets"
   }

6. Exception to Caller
   Throw Error:
   {
     message: "Access to secrets forbidden by security policy",
     receipt: "uuid-1234"
   }

Result: Access DENIED, denial receipt available, audit logged
```

### 9.2 Scenario 2: File Access Attempt

```
EVENT: Code attempts to read SSH key

Code:
    const key = fs.readFileSync(path.join(homeDir, '.ssh/id_rsa'));

Execution Flow:

1. Guard Check (G-H8-SSH-KEYS)
   └─ checkFilePathAccess('read', '~/.ssh/id_rsa')
   └─ ResolvePath('~/.ssh/id_rsa') → '/home/user/.ssh/id_rsa'
   └─ Pattern Match: '/home/user/.ssh/id_rsa' vs '~/.ssh/**'
   └─ Match: YES
   └─ Action: DENY

2. Denial Receipt
   {
     "id": "uuid-5678",
     "operation": "fs-read",
     "guardId": "G-H8-SSH-KEYS",
     "reasonCode": "FORBIDDEN_FILE_PATH",
     "context": {
       "target": "~/.ssh/id_rsa",
       "resolvedPath": "/home/user/.ssh/id_rsa",
       "category": "SSH_KEYS"
     }
   }

3. Exception Thrown (Operation Never Executed)
   {
     message: "Access to SSH keys forbidden",
     receipt: "uuid-5678"
   }

Result: File NEVER read, access denied with receipt
```

---

## 10. Summary & Integration Points

### 10.1 What Guards Provide

| Aspect | Coverage |
|--------|----------|
| **Breadth** | 25 categories of forbidden patterns (H1-H25) |
| **Depth** | 4-layer poka-yoke interception (module, API, execution, audit) |
| **Proactivity** | Denies BEFORE execution (impossible to observe) |
| **Auditability** | Every decision logged with receipt & remediation |
| **Hookable** | Integrates with @unrdf/hooks policy packs |
| **Compliance** | Supports regulatory audits (PCI-DSS, SOC2, etc.) |

### 10.2 Integration Points with Existing System

| Component | Integration |
|-----------|-------------|
| **PolicyPack** | Guards defined in policy pack manifest + loader |
| **GuardRegistry** | Central registry + interceptor setup |
| **HookExecutor** | GuardedHookExecutor wraps all hooks |
| **AuditLog** | Guard decisions written to queryable log |
| **DenialReceipts** | Structured receipts for every denial |
| **@unrdf/hooks** | Hooks register guards on definition |

### 10.3 Deployment Sequence

```
1. Initialize policyPackManager
2. Load guard policies from all policy packs
3. Setup GuardRegistry with all rules
4. Install interceptors (freeze process.env, wrap fs/child_process)
5. Initialize GuardedHookExecutor
6. Start AuditLogger
7. Self-test all guards
8. Ready for operation
```

---

## Files & Modules to Implement

Based on this specification, create:

1. **`guards/guard-registry.mjs`** - Central registry + poka-yoke interceptors
2. **`guards/guard-rules.mjs`** - All 25 guard implementations
3. **`guards/guard-executor.mjs`** - Execution in guarded context
4. **`guards/denial-receipt.mjs`** - Receipt generation + schema
5. **`guards/audit-logger.mjs`** - Audit log persistence + queries
6. **`hooks/guarded-hook-executor.mjs`** - Hook-guard integration
7. **`policy-packs/security-base-policy.json`** - Default guard policy pack

---

**End of SPARC Pseudocode Specification**

Status: Ready for Architecture & Implementation phases

Next: Architecture phase will define module interfaces, dependency graph, and state machines.
