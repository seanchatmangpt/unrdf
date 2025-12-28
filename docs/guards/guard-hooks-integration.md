# Guard System + Hooks Framework Integration

**Phase**: Architecture & Design
**Purpose**: Detailed integration patterns for guards within @unrdf/hooks

---

## 1. Conceptual Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                          Policy Pack System                          │
│                                                                       │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │ PolicyPack Manifest (manifest.json)                          │   │
│  │                                                               │   │
│  │ {                                                             │   │
│  │   "guards": [                                                │   │
│  │     {                                                         │   │
│  │       "id": "G-H1-ENV-TOKEN",                                │   │
│  │       "category": "environment-variables",                   │   │
│  │       "rules": [{pattern: "*TOKEN", action: "DENY"}]         │   │
│  │     }                                                         │   │
│  │   ],                                                          │   │
│  │   "hooks": [                                                 │   │
│  │     {                                                         │   │
│  │       "name": "on-document-write",                           │   │
│  │       "guards": ["G-H1-ENV-TOKEN", "G-H11-ENV-FILE"]        │   │
│  │     }                                                         │   │
│  │   ]                                                           │   │
│  │ }                                                             │   │
│  └──────────────────────────────────────────────────────────────┘   │
│                              │                                        │
│                              ▼                                        │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │ PolicyPackManager.loadAllPolicies()                          │   │
│  │                                                               │   │
│  │ • Parse all policy pack manifests                            │   │
│  │ • Extract guard definitions                                 │   │
│  │ • Extract hook-guard links                                  │   │
│  └──────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────┐
│                        Guard Registry System                         │
│                                                                       │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │ GuardRegistry                                                │   │
│  │                                                               │   │
│  │ • _rules: Map<guardId, GuardRule[]>                         │   │
│  │ • _hooks: Map<hookId, Set<guardId>>                         │   │
│  │ • _interceptors: Map<operation, InterceptorFn>              │   │
│  │ • _cache: LRU<decision>                                     │   │
│  │ • _auditLog: AuditLogger                                    │   │
│  │                                                               │   │
│  │ Methods:                                                     │   │
│  │ • register(guardId, rules)                                  │   │
│  │ • setupInterceptors()                                       │   │
│  │ • evaluate(operation, target) → AllowDeny                   │   │
│  │ • linkHook(hookId, guardId)                                 │   │
│  └──────────────────────────────────────────────────────────────┘   │
│                                                                       │
│  ┌────────────────────┬──────────────────┬──────────────────────┐   │
│  │                    │                  │                      │   │
│  │ Layer 1: Module    │ Layer 2: API     │ Layer 3: Execution   │   │
│  │ ────────────────── │ ────────────────│ ──────────────────   │   │
│  │                    │                  │                      │   │
│  │ Object.freeze(     │ Proxy trap       │ GuardedContext       │   │
│  │   process.env      │   .get()         │   wrapper for        │   │
│  │ )                  │ .readFile()      │   hook execution     │   │
│  │                    │ .execSync()      │                      │   │
│  │                    │ .request()       │                      │   │
│  │ (CANNOT modify)    │ (MUST check)     │ (ALL I/O guarded)    │   │
│  └────────────────────┴──────────────────┴──────────────────────┘   │
│                                                                       │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │ Layer 4: Audit Logging                                       │   │
│  │                                                               │   │
│  │ All decisions → AuditLog (allow/deny + receipt)             │   │
│  │ Denials → Denial Receipt file (structured, queryable)       │   │
│  └──────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    Guarded Hook Executor                             │
│                                                                       │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │ GuardedHookExecutor(guardRegistry)                           │   │
│  │                                                               │   │
│  │ registerHook(hookDef):                                       │   │
│  │   • Get guards from policy pack                              │   │
│  │   • Link guards to hook in registry                          │   │
│  │                                                               │   │
│  │ executeHook(hookId, event, context):                         │   │
│  │   ┌──────────────────────────────────────────────────────┐   │   │
│  │   │ Phase 1: Guard Event/Context                         │   │   │
│  │   │ guardRegistry.evaluate(hook-trigger, event)          │   │   │
│  │   │ IF denied → return DenialReceipt                      │   │   │
│  │   └──────────────────────────────────────────────────────┘   │   │
│  │                    │                                          │   │
│  │                    ▼                                          │   │
│  │   ┌──────────────────────────────────────────────────────┐   │   │
│  │   │ Phase 2: Create Guarded Context                      │   │   │
│  │   │ guardedContext = createGuardedContext(context)       │   │   │
│  │   │ • All fs operations wrapped                          │   │   │
│  │   │ • All env access filtered                            │   │   │
│  │   │ • All network calls guarded                          │   │   │
│  │   └──────────────────────────────────────────────────────┘   │   │
│  │                    │                                          │   │
│  │                    ▼                                          │   │
│  │   ┌──────────────────────────────────────────────────────┐   │   │
│  │   │ Phase 3: Execute Hook                               │   │   │
│  │   │ result = hook.execute(event, guardedContext)         │   │   │
│  │   │ (ALL I/O in this context is guarded)                 │   │   │
│  │   └──────────────────────────────────────────────────────┘   │   │
│  │                    │                                          │   │
│  │                    ▼                                          │   │
│  │   ┌──────────────────────────────────────────────────────┐   │   │
│  │   │ Phase 4: Guard Output                               │   │   │
│  │   │ guardRegistry.evaluate(hook-output, result)          │   │   │
│  │   │ IF denied → return DenialReceipt                      │   │   │
│  │   └──────────────────────────────────────────────────────┘   │   │
│  │                    │                                          │   │
│  │                    ▼                                          │   │
│  │   ┌──────────────────────────────────────────────────────┐   │   │
│  │   │ Phase 5: Audit Success                              │   │   │
│  │   │ AuditLog.record({decision: ALLOW, status: success})  │   │   │
│  │   │ Return result                                         │   │   │
│  │   └──────────────────────────────────────────────────────┘   │   │
│  │                                                               │   │
│  └──────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 2. Hook Lifecycle with Guard Integration

### 2.1 Hook Definition Phase

```javascript
// Before: Traditional hook definition
defineHook({
  id: 'on-document-write',
  meta: {name: 'on-document-write'},
  execute: async (event, context) => {
    // May access sensitive data
    const secret = process.env.API_KEY;  // NO GUARD!
    const config = fs.readFileSync('.env'); // NO GUARD!
  }
});

// After: Hook with guard declarations
defineHook({
  id: 'on-document-write',
  meta: {
    name: 'on-document-write',
    version: '1.0.0'
  },
  inputs: [
    {
      name: 'event',
      type: 'DocumentWriteEvent',
      securityRules: [
        {guard: 'G-H25-ERROR-MESSAGES', check: 'output-sanitization'}
      ]
    }
  ],
  outputs: [
    {
      name: 'result',
      type: 'WriteResult',
      securityRules: [
        {guard: 'G-H23-STACK-TRACE', check: 'path-exposure'},
        {guard: 'G-H25-ERROR-MESSAGES', check: 'info-leakage'}
      ]
    }
  ],
  execute: async (event, context) => {
    // context is already a GuardedContext
    // Any attempt to access secrets is blocked at API layer

    // This will throw with DenialReceipt:
    // const secret = process.env.API_KEY;

    // These will succeed (allowed paths):
    const mode = process.env.NODE_ENV;    // ✓ allowed
    const data = context.document.content; // ✓ guarded by framework
  }
});
```

### 2.2 Hook Registration Phase

```
SEQUENCE: Hook Registration with Guards

1. PolicyPackManager.loadAllPolicies()
   │
   ├─ Parse manifest.json
   │  ├─ Extract guards[] (25 definitions)
   │  └─ Extract hooks[] (hook-guard links)
   │
   └─ FOR EACH policy pack:
      ├─ GuardRegistry.register(guardId, rules)
      ├─ GuardRegistry.linkHook(hookId, guardIds)
      └─ HookManager.registerHook(hookDef)
          │
          └─ GuardedHookExecutor wraps hook
             └─ Hook execution now guarded


EXAMPLE MANIFEST:

{
  "id": "policy-pack-security-v1",
  "meta": {
    "name": "security-policy-v1",
    "version": "1.0.0"
  },
  "guards": [
    {
      "id": "G-H1-ENV-TOKEN",
      "category": "environment-variables",
      "rules": [
        {"pattern": "*TOKEN", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "*KEY", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "*SECRET", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "*PASSWORD", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "AWS_*", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "GITHUB_*", "severity": "CRITICAL", "action": "DENY"}
      ]
    },
    {
      "id": "G-H11-ENV-FILE",
      "category": "file-paths",
      "rules": [
        {"pattern": ".env*", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "**/.env", "severity": "CRITICAL", "action": "DENY"}
      ]
    },
    {
      "id": "G-H23-STACK-TRACE",
      "category": "output-sanitization",
      "rules": [
        {"pattern": "/__filename/", "severity": "HIGH", "action": "SANITIZE"},
        {"pattern": "/\\/home\\//", "severity": "HIGH", "action": "SANITIZE"}
      ]
    }
  ],
  "hooks": [
    {
      "name": "on-document-write",
      "file": "hooks/on-document-write.mjs",
      "guards": ["G-H1-ENV-TOKEN", "G-H11-ENV-FILE", "G-H23-STACK-TRACE"],
      "enabled": true,
      "priority": 50
    },
    {
      "name": "on-document-read",
      "file": "hooks/on-document-read.mjs",
      "guards": ["G-H1-ENV-TOKEN", "G-H15-NETWORK-URL"],
      "enabled": true,
      "priority": 50
    }
  ]
}
```

---

## 3. Hook Execution with Guard Integration

### 3.1 Execution Flow (Step-by-Step)

```
EVENT: Application calls await engine.executeHook('on-document-write', event)

STEP 1: Hook Lookup
┌────────────────────────────────────────┐
│ GuardedHookExecutor.executeHook(       │
│   hookId: 'on-document-write',         │
│   event: DocumentWriteEvent,           │
│   context: ExecutionContext            │
│ )                                      │
└────────────────────────────────────────┘
           │
           ▼

STEP 2: Get Guards for Hook
┌────────────────────────────────────────┐
│ guards = registry.getHookGuards(       │
│   'on-document-write'                  │
│ )                                      │
│ // Returns: [G-H1-ENV-TOKEN,           │
│ //           G-H11-ENV-FILE,           │
│ //           G-H23-STACK-TRACE]        │
└────────────────────────────────────────┘
           │
           ▼

STEP 3: Evaluate Hook Trigger
┌────────────────────────────────────────┐
│ FOR EACH guard IN guards:              │
│   guardCheck = guard.evaluate(         │
│     operation: 'hook-trigger',         │
│     target: event                      │
│   )                                    │
│                                        │
│   IF NOT guardCheck.allowed:           │
│     receipt = guardCheck.receipt       │
│     AuditLog.record(DENY)              │
│     RETURN DenialReceipt(receipt)      │
│   END IF                               │
│ END FOR                                │
│                                        │
│ // All guards passed - event OK        │
└────────────────────────────────────────┘
           │
           ▼

STEP 4: Create Guarded Context
┌────────────────────────────────────────┐
│ guardedContext = {                     │
│   user: event.user,                    │
│   document: event.document,            │
│   get logger():                        │
│     return guardedLogger(guards)       │
│   ,                                    │
│   get fileSystem():                    │
│     return guardedFS(guards)           │
│   ,                                    │
│   get environment():                   │
│     return filterEnv(guards)           │
│   ,                                    │
│   get httpClient():                    │
│     return guardedHTTP(guards)         │
│ }                                      │
│                                        │
│ // All I/O will be guarded             │
└────────────────────────────────────────┘
           │
           ▼

STEP 5: Execute Hook
┌────────────────────────────────────────┐
│ TRY:                                   │
│   result = hook.execute(               │
│     event,                             │
│     guardedContext  // All I/O guarded │
│   )                                    │
│                                        │
│   // During execution, any attempt to: │
│   // • Access forbidden env vars       │
│   // • Read forbidden files            │
│   // • Call forbidden commands         │
│   // → Blocked immediately             │
│   // → DenialReceipt returned           │
│   // → Execution never happens         │
│                                        │
│ CATCH error:                           │
│   sanitized = sanitizeError(error)     │
│   AuditLog.record(                     │
│     {decision: ALLOW, status: error}   │
│   )                                    │
│   RETURN Error(sanitized)              │
│ END TRY                                │
└────────────────────────────────────────┘
           │
           ▼

STEP 6: Evaluate Output
┌────────────────────────────────────────┐
│ FOR EACH guard IN guards:              │
│   IF guard has output rules:           │
│     outputCheck = guard.evaluate(      │
│       operation: 'hook-output',        │
│       target: result                   │
│     )                                  │
│                                        │
│     IF NOT outputCheck.allowed:        │
│       AuditLog.record(DENY)            │
│       RETURN DenialReceipt             │
│     END IF                             │
│   END IF                               │
│ END FOR                                │
│                                        │
│ // Output passed all guards            │
└────────────────────────────────────────┘
           │
           ▼

STEP 7: Audit Success & Return
┌────────────────────────────────────────┐
│ AuditLog.record({                      │
│   guardId: guards[].id,                │
│   decision: ALLOW,                     │
│   operation: 'hook-execution',         │
│   hookId: 'on-document-write',         │
│   status: 'success'                    │
│ })                                     │
│                                        │
│ RETURN result                          │
└────────────────────────────────────────┘
```

### 3.2 Denial Receipt Example

```javascript
// Code in hook:
execute: async (event, context) => {
  // Attempt to access forbidden environment variable
  const apiKey = process.env.GITHUB_TOKEN;
  // ↓ This triggers the guard
}

// Result: DenialReceipt returned instead of executing hook

DenialReceipt {
  id: "d7a3b2c1-4567-8901-2345-6789abcdef01",
  timestamp: "2025-01-15T10:30:00.000Z",
  operation: "env-var-access",
  severity: "CRITICAL",
  guardId: "G-H1-ENV-TOKEN",
  guardCategory: "environment-variables",
  decision: "DENY",
  reasonCode: "FORBIDDEN_CREDENTIAL_PATTERN",
  message: "Access to GitHub credentials forbidden by security policy",
  context: {
    target: "GITHUB_TOKEN",
    pattern: "GITHUB_*",
    resolvedValue: "[redacted]"
  },
  remediation: {
    action: "Use authenticated context from hook framework instead of env vars",
    documentation: "https://docs.unrdf.io/security/hooks/environment-access",
    steps: [
      "Remove direct process.env access from hook",
      "Use context.credentials provided by framework",
      "If credentials needed, request through policy approval"
    ]
  },
  auditTrail: {
    callerStackTrace: [
      "at execute (on-document-write.mjs:42)",
      "at GuardedHookExecutor.executeHook (guarded-hook-executor.mjs:85)"
    ],
    module: "[sanitized]/hooks/on-document-write.mjs",
    caller: "execute"
  },
  environment: {
    nodeVersion: "v18.0.0",
    platform: "linux",
    environment: "production"
  }
}
```

---

## 4. GuardedContext Implementation Pattern

```javascript
/**
 * Create a guarded execution context for hooks
 * All I/O operations are intercepted and checked against guards
 */
function createGuardedContext(baseContext, guardRegistry) {
  return new Proxy(baseContext, {
    // Trap: property access
    get(target, prop, receiver) {
      const value = Reflect.get(target, prop, receiver);

      // Special handling for I/O-related properties
      if (prop === 'fileSystem') {
        return createGuardedFS(value, guardRegistry);
      }

      if (prop === 'environment') {
        return createGuardedEnvironment(value, guardRegistry);
      }

      if (prop === 'httpClient') {
        return createGuardedHTTP(value, guardRegistry);
      }

      if (prop === 'childProcess') {
        return createGuardedChildProcess(value, guardRegistry);
      }

      // For regular properties, return as-is
      return value;
    },

    // Trap: property modification (prevent tampering)
    set(target, prop, value) {
      // Only allow specific mutable properties
      if (['result', 'output', 'data'].includes(prop)) {
        target[prop] = value;
        return true;
      }

      // Prevent modification of core context
      throw new Error(`Cannot modify read-only property: ${prop}`);
    },

    // Trap: property enumeration
    ownKeys(target) {
      return Object.keys(target);
    },

    getOwnPropertyDescriptor(target, prop) {
      return Object.getOwnPropertyDescriptor(target, prop);
    }
  });
}


/**
 * Create a guarded file system interface
 * All fs operations checked against G-H8 through G-H14 guards
 */
function createGuardedFS(fs, guardRegistry) {
  return {
    async readFile(path, options) {
      // Check path against G-H8 (SSH), G-H9 (AWS), etc.
      const pathCheck = guardRegistry.evaluate(
        'fs-read',
        path
      );

      if (!pathCheck.allowed) {
        throw new DenialReceipt(pathCheck.receipt);
      }

      return fs.readFile(path, options);
    },

    async writeFile(path, content, options) {
      const pathCheck = guardRegistry.evaluate(
        'fs-write',
        path
      );

      if (!pathCheck.allowed) {
        throw new DenialReceipt(pathCheck.receipt);
      }

      return fs.writeFile(path, content, options);
    },

    async readdir(path) {
      const pathCheck = guardRegistry.evaluate(
        'fs-readdir',
        path
      );

      if (!pathCheck.allowed) {
        throw new DenialReceipt(pathCheck.receipt);
      }

      return fs.readdir(path);
    }

    // ... more fs methods
  };
}


/**
 * Create a guarded environment variable interface
 * All env var access checked against G-H1 through G-H7 guards
 */
function createGuardedEnvironment(env, guardRegistry) {
  return new Proxy(env, {
    get(target, prop) {
      // Check variable name against guards
      const varCheck = guardRegistry.evaluate(
        'env-var-access',
        String(prop)
      );

      if (!varCheck.allowed) {
        throw new DenialReceipt(varCheck.receipt);
      }

      return target[prop];
    },

    set() {
      throw new Error('Environment variables are read-only in guarded context');
    },

    has(target, prop) {
      const varCheck = guardRegistry.evaluate(
        'env-var-check',
        String(prop)
      );

      return varCheck.allowed && prop in target;
    }
  });
}
```

---

## 5. Policy Pack Example: Security Policy v1

```json
{
  "id": "policy-pack-security-v1",
  "meta": {
    "name": "security-policy-v1",
    "version": "1.0.0",
    "description": "Production security policy with comprehensive guard rules",
    "author": "UNRDF Security Team",
    "license": "MIT",
    "tags": ["security", "production", "compliance"],
    "createdAt": "2025-01-15T00:00:00Z"
  },
  "config": {
    "enabled": true,
    "priority": 100,
    "strictMode": true,
    "timeout": 30000,
    "retries": 1,
    "conditions": {
      "environment": ["production", "staging"],
      "version": ">=1.0.0"
    }
  },
  "guards": [
    {
      "id": "G-H1-ENV-TOKEN",
      "category": "environment-variables",
      "rules": [
        {"pattern": "*TOKEN", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "*KEY", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "*SECRET", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "*PASSWORD", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "AWS_*", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "GITHUB_*", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "GITLAB_*", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "*_API_KEY", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "*_API_SECRET", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "DB_*", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "DATABASE_*", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "ENCRYPTION_*", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "ADMIN_*", "severity": "CRITICAL", "action": "DENY"}
      ]
    },
    {
      "id": "G-H11-ENV-FILE",
      "category": "file-paths",
      "rules": [
        {"pattern": ".env", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": ".env.local", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": ".env.*.local", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "**/.env", "severity": "CRITICAL", "action": "DENY"}
      ]
    },
    {
      "id": "G-H8-SSH-KEYS",
      "category": "file-paths",
      "rules": [
        {"pattern": "~/.ssh/**", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "/root/.ssh/**", "severity": "CRITICAL", "action": "DENY"}
      ]
    },
    {
      "id": "G-H9-AWS-CONFIG",
      "category": "file-paths",
      "rules": [
        {"pattern": "~/.aws/**", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "/root/.aws/**", "severity": "CRITICAL", "action": "DENY"}
      ]
    },
    {
      "id": "G-H15-NETWORK-URL",
      "category": "network",
      "allowlist": [
        {
          "hostname": "api.github.com",
          "paths": ["/**"],
          "methods": ["GET"],
          "description": "GitHub API (public)"
        },
        {
          "hostname": "registry.npmjs.org",
          "paths": ["/**"],
          "methods": ["GET"],
          "description": "NPM registry (public)"
        },
        {
          "hostname": "docs.unrdf.io",
          "paths": ["/**"],
          "methods": ["GET"],
          "description": "Documentation"
        }
      ],
      "rules": [
        {"pattern": "metadata.google.internal", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "169.254.169.254", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "*.internal", "severity": "HIGH", "action": "DENY"}
      ]
    },
    {
      "id": "G-H21-COMMAND-EXECUTION",
      "category": "command-execution",
      "rules": [
        {"pattern": "env", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "printenv", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "cat /etc/passwd", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "whoami", "severity": "CRITICAL", "action": "DENY"},
        {"pattern": "id", "severity": "CRITICAL", "action": "DENY"}
      ]
    },
    {
      "id": "G-H23-STACK-TRACE",
      "category": "output-sanitization",
      "rules": [
        {
          "pattern": "/__filename/",
          "severity": "HIGH",
          "action": "SANITIZE",
          "replacement": "[sanitized-path]"
        },
        {
          "pattern": "/\\//home\\//",
          "severity": "HIGH",
          "action": "SANITIZE",
          "replacement": "[sanitized-path]"
        }
      ]
    }
  ],
  "hooks": [
    {
      "name": "on-document-write",
      "file": "hooks/on-document-write.mjs",
      "guards": ["G-H1-ENV-TOKEN", "G-H11-ENV-FILE", "G-H23-STACK-TRACE"],
      "enabled": true,
      "priority": 50
    },
    {
      "name": "on-document-read",
      "file": "hooks/on-document-read.mjs",
      "guards": ["G-H1-ENV-TOKEN", "G-H15-NETWORK-URL"],
      "enabled": true,
      "priority": 50
    },
    {
      "name": "on-document-validate",
      "file": "hooks/on-document-validate.mjs",
      "guards": ["G-H21-COMMAND-EXECUTION", "G-H23-STACK-TRACE"],
      "enabled": true,
      "priority": 40
    }
  ]
}
```

---

## 6. Testing Guard Integration

### 6.1 Test Structure

```javascript
/**
 * Test: Guard Integration with Hooks
 * File: test/guards/guard-hook-integration.test.mjs
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { PolicyPackManager } from '@unrdf/hooks/policy-pack.mjs';
import { GuardRegistry } from '@unrdf/guards/guard-registry.mjs';
import { GuardedHookExecutor } from '@unrdf/hooks/guarded-hook-executor.mjs';
import { DenialReceipt } from '@unrdf/guards/denial-receipt.mjs';

describe('Guard System + Hooks Integration', () => {
  let policyPackManager;
  let guardRegistry;
  let hookExecutor;

  beforeEach(async () => {
    // Initialize policy pack system
    policyPackManager = new PolicyPackManager();

    // Load security policy
    await policyPackManager.loadPolicyPack(
      './policy-packs/security-policy-v1/manifest.json'
    );

    // Activate policy pack
    policyPackManager.activatePolicyPack('security-policy-v1');

    // Create guard registry from policy packs
    guardRegistry = new GuardRegistry();
    const policies = policyPackManager.getActivePolicyPacks();
    for (const pack of policies) {
      for (const guard of pack.guards) {
        guardRegistry.register(guard.id, guard.rules);
      }
    }

    // Setup guarded hook executor
    hookExecutor = new GuardedHookExecutor(guardRegistry);
  });

  describe('Guard: G-H1-ENV-TOKEN (Environment Variables)', () => {
    it('should DENY access to AWS_SECRET_ACCESS_KEY', async () => {
      const result = await hookExecutor.executeHook(
        'test-hook',
        { event: 'test' },
        {
          // Hook attempts to access forbidden env var
          execute: async (event, context) => {
            const secret = context.environment.AWS_SECRET_ACCESS_KEY;
            return { secret };
          }
        }
      );

      expect(result).toBeInstanceOf(DenialReceipt);
      expect(result.guardId).toBe('G-H1-ENV-TOKEN');
      expect(result.reasonCode).toBe('FORBIDDEN_CREDENTIAL_PATTERN');
      expect(result.message).toContain('AWS credentials');
    });

    it('should ALLOW access to NODE_ENV', async () => {
      process.env.NODE_ENV = 'test';

      const result = await hookExecutor.executeHook(
        'test-hook',
        { event: 'test' },
        {
          execute: async (event, context) => {
            const env = context.environment.NODE_ENV;
            return { env };
          }
        }
      );

      expect(result.allowed).toBe(true);
      expect(result.env).toBe('test');
    });
  });

  describe('Guard: G-H11-ENV-FILE (File Paths)', () => {
    it('should DENY access to .env file', async () => {
      const result = await hookExecutor.executeHook(
        'test-hook',
        { event: 'test' },
        {
          execute: async (event, context) => {
            const content = await context.fileSystem.readFile('.env');
            return { content };
          }
        }
      );

      expect(result).toBeInstanceOf(DenialReceipt);
      expect(result.guardId).toBe('G-H11-ENV-FILE');
      expect(result.reasonCode).toBe('FORBIDDEN_FILE_PATH');
    });
  });

  describe('Audit Trail', () => {
    it('should log all guard decisions', async () => {
      await hookExecutor.executeHook(
        'test-hook',
        { event: 'test' },
        {
          execute: async (event, context) => {
            // Try to access NODE_ENV (allowed)
            const env = context.environment.NODE_ENV;

            // Try to access AWS_SECRET (denied)
            try {
              const secret = context.environment.AWS_SECRET_ACCESS_KEY;
            } catch (e) {
              // Expected denial
            }

            return { ok: true };
          }
        }
      );

      // Query audit log
      const entries = guardRegistry.auditLog.query({
        operation: 'hook-execution',
        hookId: 'test-hook'
      });

      // Should have both allow and deny entries
      const allows = entries.filter(e => e.decision === 'ALLOW');
      const denies = entries.filter(e => e.decision === 'DENY');

      expect(allows.length).toBeGreaterThan(0);
      expect(denies.length).toBeGreaterThan(0);
    });
  });
});
```

---

## 7. Deployment Checklist

- [x] **Guard Registry Implementation**
  - [ ] GuardRegistry class created
  - [ ] _rules, _interceptors, _cache initialized
  - [ ] evaluate() method implemented
  - [ ] setupInterceptors() installed

- [x] **Guard Rules Implementation (25 rules)**
  - [ ] H1-H7: Environment variables
  - [ ] H8-H14: File paths
  - [ ] H15-H17: Network access
  - [ ] H18-H21: Process/syscalls
  - [ ] H22-H25: Module/output

- [x] **Denial Receipt System**
  - [ ] DenialReceipt class created
  - [ ] Schema validation (Zod)
  - [ ] File storage implemented
  - [ ] Remediation guidance templates

- [x] **Audit Logger**
  - [ ] AuditLog table created
  - [ ] Query interface implemented
  - [ ] Compliance report queries
  - [ ] Log rotation/archival

- [x] **GuardedHookExecutor**
  - [ ] Extends HookExecutor
  - [ ] 5-phase execution flow
  - [ ] GuardedContext creation
  - [ ] Error sanitization

- [x] **Policy Pack Integration**
  - [ ] Guard definitions in manifest
  - [ ] Hook-guard linking
  - [ ] PolicyPackManager integration
  - [ ] Guard loader

- [ ] **Testing Suite**
  - [ ] Unit tests for each guard
  - [ ] Integration tests for hooks
  - [ ] Audit trail verification
  - [ ] False positive detection

- [ ] **Documentation**
  - [ ] Guard policy reference
  - [ ] Hook security guide
  - [ ] Integration examples
  - [ ] Remediation procedures

---

**End of Integration Guide**

Next: Create implementation modules following these patterns.
