# Sidecar Dashboard Architecture Diagrams

## 1. System Context Diagram (C4 Level 1)

```
┌─────────────────────────────────────────────────────────────────┐
│                         User / Developer                         │
└───────────────┬─────────────────────────────┬──────────────────┘
                │                             │
                │ HTTPS/CLI                   │ HTTPS/Browser
                ▼                             ▼
        ┌───────────────┐           ┌─────────────────┐
        │  unrdf CLI    │           │   Dashboard UI  │
        │  (Node.js)    │           │   (Nuxt 4)      │
        └───────┬───────┘           └────────┬────────┘
                │                            │
                │ HTTP API                   │ HTTP API + SSE
                │                            │
                └────────────┬───────────────┘
                             ▼
                ┌─────────────────────────┐
                │   KGC Sidecar Server    │
                │   (Nuxt 4 + Nitro)      │
                └────────┬────────────────┘
                         │
         ┌───────────────┼───────────────┐
         │               │               │
         ▼               ▼               ▼
    ┌────────┐     ┌──────────┐    ┌─────────┐
    │ RDF    │     │ Lockchain│    │  OTEL   │
    │ Store  │     │ (Git)    │    │Collector│
    └────────┘     └──────────┘    └─────────┘
```

## 2. Container Diagram (C4 Level 2)

```
┌─────────────────────────────────────────────────────────────────┐
│                      Dashboard UI (Browser)                      │
├─────────────────────────────────────────────────────────────────┤
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐          │
│  │ Vue Pages    │  │ Composables  │  │ Components   │          │
│  │ (/hooks)     │──│ (useState)   │──│ (HooksList)  │          │
│  │ (/policies)  │  │              │  │              │          │
│  └──────────────┘  └──────────────┘  └──────────────┘          │
│         │                  │                  │                 │
│         └──────────────────┼──────────────────┘                 │
│                            │ $fetch                             │
│                            ▼                                    │
├─────────────────────────────────────────────────────────────────┤
│                    Nuxt 4 SSR Layer                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐          │
│  │ Layouts      │  │ Middleware   │  │ Utils        │          │
│  │ (dashboard)  │  │ (auth)       │  │ (validators) │          │
│  └──────────────┘  └──────────────┘  └──────────────┘          │
└─────────────────────────────────────────────────────────────────┘
                            │
                            │ Internal HTTP
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Nitro Server (Node.js)                        │
├─────────────────────────────────────────────────────────────────┤
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                    API Routes                             │  │
│  │  /api/hooks/*  /api/policies/*  /api/transactions/*      │  │
│  └───────────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                   Middleware                              │  │
│  │  Auth │ RBAC │ Telemetry │ Error Handler │ Rate Limit   │  │
│  └───────────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                   Core Managers                           │  │
│  │  KnowledgeHookManager │ PolicyPackManager │ LockchainMgr │  │
│  └───────────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │                Scheduled Tasks (SAFLA)                    │  │
│  │  evaluate-hooks │ refresh-policies │ self-heal           │  │
│  └───────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

## 3. Component Diagram (C4 Level 3)

```
┌─────────────────────────────────────────────────────────────────┐
│                       Frontend Components                        │
├─────────────────────────────────────────────────────────────────┤
│  Pages Layer                                                     │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐          │
│  │ /hooks       │  │ /policies    │  │ /transactions│          │
│  │ index.vue    │  │ index.vue    │  │ index.vue    │          │
│  │ [id].vue     │  │ [id].vue     │  │ [id].vue     │          │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘          │
│         │                  │                  │                  │
│         ▼                  ▼                  ▼                  │
│  ┌──────────────────────────────────────────────────┐           │
│  │           Composables (State Management)         │           │
│  │  useKnowledgeHooks() │ usePolicies() │ etc.     │           │
│  └──────────────────────────────────────────────────┘           │
│         │                                                        │
│         ▼ Zod Validation                                        │
│  ┌──────────────────────────────────────────────────┐           │
│  │              Zod Schemas                         │           │
│  │  KnowledgeHookSchema │ PolicyPackSchema          │           │
│  └──────────────────────────────────────────────────┘           │
│         │                                                        │
│         ▼ Vue Components                                        │
│  ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐   │
│  │ HooksList.vue   │ │ HookEditor.vue  │ │ PolicyEditor    │   │
│  │ - Table         │ │ - Form          │ │ - SHACL Editor  │   │
│  │ - Pagination    │ │ - Validation    │ │ - Syntax Highlight│   │
│  └─────────────────┘ └─────────────────┘ └─────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
                            │
                            │ $fetch() / apiClient()
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Backend API Layer                           │
├─────────────────────────────────────────────────────────────────┤
│  API Routes (Nitro Event Handlers)                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐          │
│  │ hooks/       │  │ policies/    │  │ transactions/│          │
│  │ index.get    │  │ index.get    │  │ index.get    │          │
│  │ index.post   │  │ index.post   │  │ [id].get     │          │
│  │ [id].put     │  │ [id].put     │  │ apply.post   │          │
│  │ [id].delete  │  │ [id].delete  │  └──────┬───────┘          │
│  └──────┬───────┘  └──────┬───────┘         │                  │
│         │                  │                  │                  │
│         └──────────────────┼──────────────────┘                  │
│                            ▼                                    │
│  ┌──────────────────────────────────────────────────┐           │
│  │           Core Domain Managers                   │           │
│  │  ┌────────────────────────────────────────────┐  │           │
│  │  │ KnowledgeHookManager                       │  │           │
│  │  │ - registerHook()                           │  │           │
│  │  │ - evaluateHooks()                          │  │           │
│  │  │ - deleteHook()                             │  │           │
│  │  └────────────────────────────────────────────┘  │           │
│  │  ┌────────────────────────────────────────────┐  │           │
│  │  │ PolicyPackManager                          │  │           │
│  │  │ - registerPolicyPack()                     │  │           │
│  │  │ - validatePolicyPack()                     │  │           │
│  │  └────────────────────────────────────────────┘  │           │
│  │  ┌────────────────────────────────────────────┐  │           │
│  │  │ TransactionManager                         │  │           │
│  │  │ - applyTransaction()                       │  │           │
│  │  │ - getTransactionLog()                      │  │           │
│  │  └────────────────────────────────────────────┘  │           │
│  └──────────────────────────────────────────────────┘           │
│         │                  │                  │                  │
│         ▼                  ▼                  ▼                  │
│  ┌──────────┐      ┌──────────┐      ┌──────────┐              │
│  │ RDF Store│      │Lockchain │      │  OTEL    │              │
│  │ (N-Quads)│      │ (Git)    │      │(Metrics) │              │
│  └──────────┘      └──────────┘      └──────────┘              │
└─────────────────────────────────────────────────────────────────┘
```

## 4. Data Flow Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                      User Interaction                            │
└───────────────┬─────────────────────────────────────────────────┘
                │
                │ 1. Click "Create Hook"
                ▼
        ┌───────────────────┐
        │  HookEditor.vue   │
        │  (Vue Component)  │
        └───────┬───────────┘
                │
                │ 2. Fill form, click "Submit"
                │
                ▼ useForm()
        ┌───────────────────┐
        │ Zod Validation    │
        │ HookSchema.parse()│
        └───────┬───────────┘
                │
                │ 3. If valid, call composable
                ▼
        ┌─────────────────────────┐
        │ useKnowledgeHooks()     │
        │ createHook(hookData)    │
        └───────┬─────────────────┘
                │
                │ 4. $fetch('/api/hooks', { method: 'POST', body })
                ▼
        ┌─────────────────────────┐
        │ API Route Handler       │
        │ hooks/index.post.mjs    │
        └───────┬─────────────────┘
                │
                │ 5. Middleware chain
                ├──► [Auth] requireAuth(event)
                ├──► [RBAC] requireRole(event, 'admin')
                ├──► [Validation] Zod schema parse
                │
                ▼ 6. Business logic
        ┌─────────────────────────┐
        │ KnowledgeHookManager    │
        │ registerHook(hookData)  │
        └───────┬─────────────────┘
                │
                ├──► 7a. Store in RDF
                │    ┌─────────────┐
                │    │ RDF Store   │
                │    └─────────────┘
                │
                ├──► 7b. Log to Lockchain
                │    ┌─────────────┐
                │    │ Lockchain   │
                │    │ (Git commit)│
                │    └─────────────┘
                │
                └──► 7c. Emit OTEL metric
                     ┌─────────────┐
                     │ OTEL        │
                     │ hook.created│
                     └─────────────┘
                │
                │ 8. Return response
                ▼
        ┌─────────────────────────┐
        │ { success: true,        │
        │   hookId: 'hook-123' }  │
        └───────┬─────────────────┘
                │
                │ 9. Update UI state
                ▼
        ┌─────────────────────────┐
        │ useKnowledgeHooks()     │
        │ hooks.value.push(...)   │
        └───────┬─────────────────┘
                │
                │ 10. Re-render component
                ▼
        ┌───────────────────┐
        │  HooksList.vue    │
        │  (shows new hook) │
        └───────────────────┘
```

## 5. Authentication & Authorization Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                      User Login Request                          │
└───────────────┬─────────────────────────────────────────────────┘
                │
                │ POST /api/auth/login { email, password }
                ▼
        ┌───────────────────────┐
        │ auth/login.post.mjs   │
        └───────┬───────────────┘
                │
                ├──► 1. Validate input (Zod)
                │    ┌──────────────────┐
                │    │ LoginSchema      │
                │    │ .parse(body)     │
                │    └──────────────────┘
                │
                ├──► 2. Verify credentials
                │    ┌──────────────────┐
                │    │ bcrypt.compare() │
                │    │ password hash    │
                │    └──────────────────┘
                │
                ├──► 3. Generate JWT
                │    ┌──────────────────┐
                │    │ jwt.sign()       │
                │    │ { userId, role } │
                │    │ exp: 24h         │
                │    └──────────────────┘
                │
                ├──► 4. Set HTTP-only cookie
                │    ┌──────────────────┐
                │    │ Set-Cookie:      │
                │    │ auth_token=...   │
                │    │ HttpOnly; Secure │
                │    └──────────────────┘
                │
                └──► 5. Return success
                     { success: true }
                │
                ▼
┌─────────────────────────────────────────────────────────────────┐
│               Subsequent Authenticated Request                   │
└───────────────┬─────────────────────────────────────────────────┘
                │
                │ GET /api/hooks (with cookie)
                ▼
        ┌───────────────────────┐
        │ Auth Middleware       │
        │ 00.auth.mjs           │
        └───────┬───────────────┘
                │
                ├──► 1. Extract token from cookie
                │    ┌──────────────────┐
                │    │ cookie: auth_tok │
                │    └──────────────────┘
                │
                ├──► 2. Verify JWT
                │    ┌──────────────────┐
                │    │ jwt.verify()     │
                │    │ Check signature  │
                │    │ Check expiration │
                │    └──────────────────┘
                │
                ├──► 3. Check RBAC
                │    ┌──────────────────┐
                │    │ RBAC Engine      │
                │    │ checkPermission()│
                │    │ user.role → perm │
                │    └──────────────────┘
                │
                └──► 4. Allow or Deny
                     ┌─────────┬─────────┐
                     │ ✅ Allow│ ❌ Deny │
                     │ Continue│ 403     │
                     └─────────┴─────────┘
```

## 6. Real-Time Updates (SSE) Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                    Client Subscribes to SSE                      │
└───────────────┬─────────────────────────────────────────────────┘
                │
                │ GET /api/events
                ▼
        ┌───────────────────────┐
        │ events.get.mjs        │
        └───────┬───────────────┘
                │
                ├──► 1. Set SSE headers
                │    ┌────────────────────────┐
                │    │ Content-Type:          │
                │    │ text/event-stream      │
                │    │ Cache-Control: no-cache│
                │    └────────────────────────┘
                │
                ├──► 2. Create readable stream
                │    ┌────────────────────────┐
                │    │ new ReadableStream()   │
                │    └────────────────────────┘
                │
                └──► 3. Subscribe to events
                     ┌────────────────────────┐
                     │ TransactionManager     │
                     │ .on('transaction', ...)│
                     └────────┬───────────────┘
                              │
                              │ 4. Transaction applied
                              ▼
                     ┌────────────────────────┐
                     │ Format SSE message     │
                     │ data: {...}\n\n        │
                     └────────┬───────────────┘
                              │
                              │ 5. Send to client
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Client Receives Event                       │
├─────────────────────────────────────────────────────────────────┤
│  const eventSource = new EventSource('/api/events')             │
│  eventSource.onmessage = (event) => {                           │
│    const transaction = JSON.parse(event.data)                   │
│    // Update UI with new transaction                            │
│    transactions.value.unshift(transaction)                      │
│  }                                                               │
└─────────────────────────────────────────────────────────────────┘
```

## 7. Deployment Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         Load Balancer                            │
│                         (HTTPS/TLS)                              │
└───────────────┬─────────────────────────────────────────────────┘
                │
                ├─────────────────┬─────────────────┐
                ▼                 ▼                 ▼
        ┌───────────────┐ ┌───────────────┐ ┌───────────────┐
        │ Sidecar       │ │ Sidecar       │ │ Sidecar       │
        │ Instance 1    │ │ Instance 2    │ │ Instance 3    │
        │ (Nuxt + API)  │ │ (Nuxt + API)  │ │ (Nuxt + API)  │
        └───────┬───────┘ └───────┬───────┘ └───────┬───────┘
                │                 │                 │
                └─────────────────┼─────────────────┘
                                  │
                ┌─────────────────┼─────────────────┐
                │                 │                 │
                ▼                 ▼                 ▼
        ┌───────────┐     ┌───────────┐     ┌───────────┐
        │ RDF Store │     │ Lockchain │     │   OTEL    │
        │ (Shared)  │     │ (Git Repo)│     │ Collector │
        └───────────┘     └───────────┘     └───────────┘
```

## 8. Testing Pyramid

```
                        ┌───────┐
                        │  E2E  │ 10%
                        │ Tests │
                        └───┬───┘
                    ┌───────┴───────┐
                    │  Nuxt/Vue     │ 20%
                    │  Component    │
                    │     Tests     │
                    └───────┬───────┘
            ┌───────────────┴───────────────┐
            │       Unit Tests              │ 70%
            │  Composables, Utils, Schemas  │
            └───────────────────────────────┘
```

---

## Key Architectural Patterns

### 1. Repository Pattern
```
API Route → Manager (Domain Logic) → RDF Store (Persistence)
```

### 2. Command Query Responsibility Segregation (CQRS)
```
Write: POST /api/hooks → applyTransaction() → Lockchain
Read:  GET /api/hooks → fetchFromRDF()
```

### 3. Event Sourcing (via Lockchain)
```
Every mutation → Git commit → Immutable audit trail
```

### 4. Defense in Depth (Security)
```
HTTPS → JWT Auth → RBAC → Zod Validation → SHACL Policy
```

---

**Related Documents**:
- Full Architecture: `/docs/architecture/sidecar-dashboard-architecture.md`
- Summary: `/docs/architecture/ARCHITECTURE-SUMMARY.md`
