# Nuxt UI Dashboard Template - Comprehensive Research Analysis

**Repository**: https://github.com/nuxt-ui-templates/dashboard
**Research Date**: 2025-10-01
**Agent**: Researcher (Hive Mind Swarm)
**Session**: swarm-1759363254478-gds008fsq

---

## Executive Summary (80/20 Analysis)

### Critical 20% That Delivers 80% Value:
1. **Nuxt UI Component System** - Pre-built dashboard components (UDashboardGroup, UDashboardSidebar, USlideover)
2. **File-based Routing** - Automatic route generation from `/app/pages`
3. **TypeScript-to-MJS Conversion Points** - 5 core files need conversion
4. **Zod Integration** - Already uses Zod ^4.1.11 (compatible!)
5. **Composables Pattern** - Single shared composable manages global state

---

## 1. Directory Structure & File Organization

```
nuxt-ui-dashboard/
├── app/
│   ├── assets/css/
│   │   └── main.css                     # Tailwind + custom theme
│   ├── components/
│   │   ├── home/
│   │   │   ├── HomeChart.client.vue     # Client-side chart rendering
│   │   │   ├── HomeChart.server.vue     # Server-side chart rendering
│   │   │   ├── HomeDateRangePicker.vue  # Date range selection
│   │   │   ├── HomePeriodSelect.vue     # Period (daily/weekly/monthly)
│   │   │   ├── HomeSales.vue            # Sales data table
│   │   │   └── HomeStats.vue            # Statistics cards
│   │   ├── customers/                   # Customer components (not detailed)
│   │   ├── inbox/                       # Inbox components (not detailed)
│   │   ├── settings/                    # Settings components (not detailed)
│   │   ├── NotificationsSlideover.vue   # Notifications panel
│   │   ├── TeamsMenu.vue                # Team selector menu
│   │   └── UserMenu.vue                 # User account menu
│   ├── composables/
│   │   └── useDashboard.ts              # ⚠️ CONVERSION REQUIRED
│   ├── layouts/
│   │   └── default.vue                  # ⚠️ CONVERSION REQUIRED (TS in script)
│   ├── pages/
│   │   ├── index.vue                    # ⚠️ CONVERSION REQUIRED (TS in script)
│   │   ├── customers.vue                # ⚠️ CONVERSION REQUIRED (TS in script)
│   │   ├── inbox.vue                    # ⚠️ CONVERSION REQUIRED (TS in script)
│   │   └── settings.vue                 # ⚠️ CONVERSION REQUIRED (TS in script)
│   ├── types/
│   │   └── index.d.ts                   # ⚠️ CONVERT TO ZOD SCHEMAS
│   ├── utils/                           # Utility functions (not detailed)
│   ├── app.config.ts                    # ⚠️ CONVERT TO .mjs
│   ├── app.vue                          # ⚠️ CONVERSION REQUIRED (TS in script)
│   └── error.vue                        # Error page component
├── public/                              # Static assets
├── server/
│   └── api/
│       ├── customers.ts                 # ⚠️ CONVERSION REQUIRED
│       ├── mails.ts                     # ⚠️ CONVERSION REQUIRED
│       ├── members.ts                   # ⚠️ CONVERSION REQUIRED
│       └── notifications.ts             # ⚠️ CONVERSION REQUIRED
├── .github/workflows/                   # CI/CD pipelines
├── nuxt.config.ts                       # ⚠️ CONVERT TO nuxt.config.mjs
├── package.json                         # ✅ Already compatible
├── tsconfig.json                        # ⚠️ REMOVE (not needed with MJS)
└── README.md
```

---

## 2. Component Architecture

### Component Hierarchy
```
app.vue (Root)
  └── NuxtLayout (default.vue)
        ├── UDashboardGroup
        │     ├── UDashboardSidebar
        │     │     ├── TeamsMenu
        │     │     ├── UDashboardSearchButton
        │     │     ├── UNavigationMenu
        │     │     └── UserMenu
        │     └── UDashboardPanel
        │           ├── UDashboardNavbar
        │           ├── UDashboardToolbar
        │           └── NuxtPage (routes)
        │                 ├── index.vue → HomeStats, HomeChart, HomeSales
        │                 ├── customers.vue
        │                 ├── inbox.vue
        │                 └── settings.vue
        └── NotificationsSlideover (conditional)
```

### Component Types
- **Layout Components**: UDashboardGroup, UDashboardSidebar, UDashboardPanel, UDashboardNavbar
- **Navigation Components**: UNavigationMenu, UDashboardSearchButton, TeamsMenu, UserMenu
- **Data Display**: HomeStats, HomeChart, HomeSales, NotificationsSlideover
- **Form Components**: HomeDateRangePicker, HomePeriodSelect

### Nuxt UI Components Used
All components from `@nuxt/ui` package:
- `<UApp>` - Root app wrapper
- `<UDashboardGroup>` - Dashboard container
- `<UDashboardSidebar>` - Collapsible sidebar
- `<UDashboardPanel>` - Main content area
- `<UDashboardNavbar>` - Top navigation bar
- `<UDashboardToolbar>` - Toolbar for filters
- `<UNavigationMenu>` - Navigation menu items
- `<USlideover>` - Slide-out panel
- `<UChip>` - Badge/chip component
- `<UAvatar>` - User avatar component
- `<UToast>` - Toast notifications

---

## 3. Routing & Navigation Patterns

### File-Based Routing
```javascript
// Automatic route generation from files:
app/pages/index.vue       → /
app/pages/customers.vue   → /customers
app/pages/inbox.vue       → /inbox
app/pages/settings.vue    → /settings
```

### Navigation Implementation
**Keyboard Shortcuts** (from `useDashboard.ts`):
```javascript
defineShortcuts({
  'g-h': () => router.push('/'),      // Go to Home
  'g-i': () => router.push('/inbox'), // Go to Inbox
  'g-c': () => router.push('/customers'), // Go to Customers
  'g-s': () => router.push('/settings'),  // Go to Settings
  'n': () => toggleNotifications()    // Toggle Notifications
})
```

**Navigation Menu Links**:
```javascript
const links = [[
  { label: 'Home', icon: 'i-lucide-home', to: '/' },
  { label: 'Inbox', icon: 'i-lucide-inbox', to: '/inbox' },
  { label: 'Customers', icon: 'i-lucide-users-round', to: '/customers' },
  { label: 'Settings', icon: 'i-lucide-settings', to: '/settings' }
], [
  { label: 'Feedback', icon: 'i-lucide-message-square-text' },
  { label: 'Help & Support', icon: 'i-lucide-circle-help' }
]]
```

---

## 4. Data Flow & State Management

### Composables (Shared State)
**useDashboard.ts** - Global dashboard state:
```typescript
// CURRENT TypeScript VERSION:
import { createSharedComposable } from '@vueuse/core'
import { ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'

const _useDashboard = () => {
  const route = useRoute()
  const router = useRouter()
  const isNotificationsSlideoverOpen = ref(false)

  defineShortcuts({...})
  watch(() => route.fullPath, () => {
    isNotificationsSlideoverOpen.value = false
  })

  return { isNotificationsSlideoverOpen }
}

export const useDashboard = createSharedComposable(_useDashboard)
```

**MJS CONVERSION STRATEGY**:
```javascript
/**
 * @file useDashboard.mjs
 * @description Global dashboard state management composable
 */

import { createSharedComposable } from '@vueuse/core'
import { ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'

/**
 * Dashboard composable internal implementation
 * @returns {{isNotificationsSlideoverOpen: import('vue').Ref<boolean>}}
 */
const _useDashboard = () => {
  const route = useRoute()
  const router = useRouter()

  /** @type {import('vue').Ref<boolean>} */
  const isNotificationsSlideoverOpen = ref(false)

  defineShortcuts({
    'g-h': () => router.push('/'),
    'g-i': () => router.push('/inbox'),
    'g-c': () => router.push('/customers'),
    'g-s': () => router.push('/settings'),
    'n': () => isNotificationsSlideoverOpen.value = !isNotificationsSlideoverOpen.value
  })

  watch(() => route.fullPath, () => {
    isNotificationsSlideoverOpen.value = false
  })

  return { isNotificationsSlideoverOpen }
}

/**
 * Shared dashboard composable
 * @type {() => {isNotificationsSlideoverOpen: import('vue').Ref<boolean>}}
 */
export const useDashboard = createSharedComposable(_useDashboard)
```

### API Data Fetching Pattern
```vue
<script setup>
// Example from NotificationsSlideover.vue
const { data: notifications } = await useFetch('/api/notifications')
</script>
```

**Server API Routes**:
- `/api/customers` - Returns User[] array
- `/api/notifications` - Returns Notification[] array
- `/api/mails` - Returns Mail[] array
- `/api/members` - Returns Member[] array

---

## 5. Styling Approach

### CSS Framework: Tailwind CSS + Nuxt UI
**main.css**:
```css
@import url('@nuxt/ui/css/static-theme') layer(nuxt-ui-config);

@theme {
  --font-family: 'Public Sans';

  /* Custom Green Color Palette */
  --color-green-50: #EFFDF5;
  --color-green-100: #D9FBE8;
  --color-green-200: #B3F5D1;
  --color-green-300: #75EDAE;
  --color-green-400: #00DC82;  /* Primary brand color */
  --color-green-500: #00C16A;
  --color-green-600: #00A155;
  --color-green-700: #007F45;
  --color-green-800: #016538;
  --color-green-900: #0A5331;
  --color-green-950: #052E16;
}
```

**app.config.ts**:
```typescript
export default defineAppConfig({
  ui: {
    colors: {
      primary: 'green',
      neutral: 'zinc'
    }
  }
})
```

**MJS CONVERSION**:
```javascript
// app.config.mjs
export default defineAppConfig({
  ui: {
    colors: {
      primary: 'green',
      neutral: 'zinc'
    }
  }
})
```

### Theming Features
- Light/dark mode support via `useColorMode()`
- Dynamic theme color: `<meta name="theme-color" :content="color">`
- Responsive design with Nuxt UI component sizing

---

## 6. TypeScript Patterns to Convert

### 6.1 Type Definitions (app/types/index.d.ts)

**CURRENT TypeScript TYPES**:
```typescript
export type UserStatus = 'subscribed' | 'unsubscribed' | 'bounced'
export type SaleStatus = 'paid' | 'failed' | 'refunded'
export type Period = 'daily' | 'weekly' | 'monthly'

export interface User {
  id: number
  name: string
  email: string
  avatar: {
    src: string
  }
  status: UserStatus
  location: string
}

export interface Mail {
  id: number
  unread?: boolean
  from: User
  subject: string
  body: string
  date: string
}

export interface Member {
  name: string
  username: string
  role: 'member' | 'owner'
  avatar: {
    src: string
  }
}

export interface Stat {
  title: string
  icon: string
  value: number
  variation: {
    value: number
    positive: boolean
  }
  formatter?: (value: number) => string
}

export interface Sale {
  id: number
  date: string
  status: SaleStatus
  email: string
  amount: number
}

export interface Notification {
  id: number
  unread?: boolean
  sender: User
  body: string
  date: string
}

export interface Range {
  start: Date
  end: Date
}
```

**ZOD SCHEMA CONVERSION** (`app/schemas/index.mjs`):
```javascript
/**
 * @file app/schemas/index.mjs
 * @description Zod validation schemas for dashboard data types
 */

import { z } from 'zod'

// Type literals converted to Zod enums
export const UserStatusSchema = z.enum(['subscribed', 'unsubscribed', 'bounced'])
export const SaleStatusSchema = z.enum(['paid', 'failed', 'refunded'])
export const PeriodSchema = z.enum(['daily', 'weekly', 'monthly'])

// Interfaces converted to Zod objects
export const UserSchema = z.object({
  id: z.number(),
  name: z.string(),
  email: z.string().email(),
  avatar: z.object({
    src: z.string().url()
  }),
  status: UserStatusSchema,
  location: z.string()
})

export const MailSchema = z.object({
  id: z.number(),
  unread: z.boolean().optional(),
  from: UserSchema,
  subject: z.string(),
  body: z.string(),
  date: z.string().datetime()
})

export const MemberSchema = z.object({
  name: z.string(),
  username: z.string(),
  role: z.enum(['member', 'owner']),
  avatar: z.object({
    src: z.string().url()
  })
})

export const StatSchema = z.object({
  title: z.string(),
  icon: z.string(),
  value: z.number(),
  variation: z.object({
    value: z.number(),
    positive: z.boolean()
  }),
  formatter: z.function().args(z.number()).returns(z.string()).optional()
})

export const SaleSchema = z.object({
  id: z.number(),
  date: z.string().datetime(),
  status: SaleStatusSchema,
  email: z.string().email(),
  amount: z.number()
})

export const NotificationSchema = z.object({
  id: z.number(),
  unread: z.boolean().optional(),
  sender: UserSchema,
  body: z.string(),
  date: z.string().datetime()
})

export const RangeSchema = z.object({
  start: z.date(),
  end: z.date()
})

// Export TypeScript-style types from Zod schemas
/** @typedef {z.infer<typeof UserStatusSchema>} UserStatus */
/** @typedef {z.infer<typeof SaleStatusSchema>} SaleStatus */
/** @typedef {z.infer<typeof PeriodSchema>} Period */
/** @typedef {z.infer<typeof UserSchema>} User */
/** @typedef {z.infer<typeof MailSchema>} Mail */
/** @typedef {z.infer<typeof MemberSchema>} Member */
/** @typedef {z.infer<typeof StatSchema>} Stat */
/** @typedef {z.infer<typeof SaleSchema>} Sale */
/** @typedef {z.infer<typeof NotificationSchema>} Notification */
/** @typedef {z.infer<typeof RangeSchema>} Range */
```

### 6.2 Server API Routes

**CURRENT** (`server/api/customers.ts`):
```typescript
import type { User } from '~/types'

const customers: User[] = [
  { id: 1, name: 'Lindsay Walton', email: 'lindsay.walton@example.com', /* ... */ }
]

export default eventHandler(async () => {
  return customers
})
```

**MJS CONVERSION** (`server/api/customers.mjs`):
```javascript
/**
 * @file server/api/customers.mjs
 * @description API endpoint for customer data
 */

import { UserSchema } from '~/schemas/index.mjs'

/**
 * @type {import('~/schemas/index.mjs').User[]}
 */
const customers = [
  {
    id: 1,
    name: 'Lindsay Walton',
    email: 'lindsay.walton@example.com',
    avatar: { src: 'https://i.pravatar.cc/120?img=1' },
    status: 'subscribed',
    location: 'New York, USA'
  }
  // ... more customers
]

export default eventHandler(async () => {
  // Optional: Validate data before returning
  return customers.map(customer => UserSchema.parse(customer))
})
```

### 6.3 Vue Component Scripts

**CURRENT** (`app/pages/index.vue`):
```vue
<script setup lang="ts">
import type { DropdownMenuItem } from '@nuxt/ui'
import type { Period, Range } from '~/types'

const range = ref<Range>({
  start: sub(new Date(), { days: 14 }),
  end: new Date()
})

const period = ref<Period>('daily')
</script>
```

**MJS CONVERSION** (remove `lang="ts"`, add JSDoc):
```vue
<script setup>
/**
 * @file app/pages/index.vue
 * @description Dashboard home page with stats, charts, and sales data
 */

import { sub } from 'date-fns'

/**
 * @typedef {import('@nuxt/ui').DropdownMenuItem} DropdownMenuItem
 * @typedef {import('~/schemas/index.mjs').Period} Period
 * @typedef {import('~/schemas/index.mjs').Range} Range
 */

/** @type {import('vue').Ref<Range>} */
const range = ref({
  start: sub(new Date(), { days: 14 }),
  end: new Date()
})

/** @type {import('vue').Ref<Period>} */
const period = ref('daily')
</script>
```

---

## 7. Dependencies & Integrations

### Production Dependencies
```json
{
  "@iconify-json/lucide": "^1.2.68",        // Lucide icons
  "@iconify-json/simple-icons": "^1.2.53",  // Simple icons (GitHub, etc.)
  "@nuxt/ui": "^4.0.0",                     // ✅ Core UI framework
  "@unovis/ts": "^1.6.1",                   // Chart library (TypeScript)
  "@unovis/vue": "^1.6.1",                  // Chart library (Vue)
  "@vueuse/nuxt": "^13.9.0",                // ✅ VueUse composables
  "date-fns": "^4.1.0",                     // ✅ Date manipulation
  "nuxt": "^4.1.2",                         // ✅ Nuxt 4 framework
  "zod": "^4.1.11"                          // ✅ Already included!
}
```

### Dev Dependencies
```json
{
  "@nuxt/eslint": "^1.9.0",                 // ESLint integration
  "eslint": "^9.36.0",                      // Linting
  "typescript": "^5.9.2",                   // ⚠️ Can remove after conversion
  "vue-tsc": "^3.1.0"                       // ⚠️ Can remove after conversion
}
```

### Package Manager
- **pnpm** 10.17.1 (specified in `packageManager` field)
- Resolution override: `unimport: 4.1.1`

### Key Integration Points
1. **@nuxt/ui** - Dashboard components system
2. **@vueuse/nuxt** - Composables like `formatTimeAgo`, `createSharedComposable`
3. **@unovis** - Charting library (may need wrapper for MJS)
4. **date-fns** - Date utilities (already MJS-compatible)
5. **zod** - Already present! No need to add

---

## 8. Authentication/Authorization Patterns

### Current Implementation
**NO AUTHENTICATION FOUND** in the template. This is a UI template only.

**Cookie Consent Implementation** (in `default.vue`):
```javascript
onMounted(async () => {
  const cookie = useCookie('cookie-consent')
  if (cookie.value !== 'accepted') {
    toast.add({
      title: 'We use first-party cookies...',
      actions: [
        { label: 'Accept', click: () => cookie.value = 'accepted' },
        { label: 'Opt out', click: () => cookie.value = 'rejected' }
      ]
    })
  }
})
```

### Integration with unrdf Sidecar
**REQUIRED ADDITIONS**:
1. **Policy-based access control** - Hook into Knowledge Hooks
2. **RDF-based permissions** - SHACL validation for user roles
3. **Cryptographic provenance** - Sign all data modifications
4. **Session management** - Store session state in RDF

**Proposed Integration Points**:
```javascript
// composables/useAuth.mjs
import { useSidecar } from '~/composables/useSidecar.mjs'

export const useAuth = () => {
  const { validate, execute } = useSidecar()

  const login = async (credentials) => {
    await validate('auth:login', credentials)
    return await execute('auth:session:create', credentials)
  }

  const checkPermission = async (resource, action) => {
    return await validate(`auth:can:${action}`, { resource })
  }

  return { login, checkPermission }
}
```

---

## 9. Adaptation Requirements for unrdf Sidecar

### 9.1 Directory Structure Mapping
```
nuxt-ui-dashboard/app/           → sidecar/app/
nuxt-ui-dashboard/server/        → sidecar/server/
nuxt-ui-dashboard/types/         → sidecar/schemas/ (Zod conversion)
nuxt-ui-dashboard/composables/   → sidecar/composables/
```

### 9.2 Critical Conversion Tasks
1. **Convert all `.ts` files to `.mjs`**
   - `app.config.ts` → `app.config.mjs`
   - `nuxt.config.ts` → `nuxt.config.mjs`
   - All `composables/*.ts` → `composables/*.mjs`
   - All `server/api/*.ts` → `server/api/*.mjs`

2. **Remove TypeScript from Vue SFCs**
   - Remove `lang="ts"` from all `<script setup>` tags
   - Add JSDoc type annotations
   - Import types from Zod schemas

3. **Convert type definitions to Zod schemas**
   - `app/types/index.d.ts` → `app/schemas/index.mjs`
   - Export Zod schemas
   - Export JSDoc typedefs from `z.infer<>`

4. **Update imports**
   - `import type { User } from '~/types'` → `/** @typedef {import('~/schemas/index.mjs').User} User */`
   - `import { UserSchema } from '~/schemas/index.mjs'`

5. **Remove TypeScript tooling**
   - Remove `tsconfig.json`
   - Remove `typescript` and `vue-tsc` from devDependencies
   - Remove `typecheck` script from package.json

### 9.3 Integration with Existing Sidecar Structure
**Current sidecar structure** (from git status):
```
sidecar/
├── nuxt.config.mjs              # ✅ Already MJS
├── package.json
├── server/                      # Need to add API routes here
├── test/                        # Need to add tests
└── types/                       # ⚠️ Convert to schemas/
```

**Integration Strategy**:
1. **Keep existing sidecar structure**
2. **Add dashboard components** to `sidecar/app/components/`
3. **Add dashboard pages** to `sidecar/app/pages/`
4. **Merge API routes** from template into `sidecar/server/api/`
5. **Add schemas** to `sidecar/schemas/` (new directory)
6. **Update sidecar nuxt.config.mjs** with dashboard modules

### 9.4 Nuxt 4 Compatibility
**Template uses Nuxt 4.1.2** - ✅ Compatible with sidecar's Nuxt setup

**Required modules to add to sidecar**:
```javascript
// sidecar/nuxt.config.mjs
export default defineNuxtConfig({
  modules: [
    '@nuxt/ui',        // Add dashboard UI components
    '@vueuse/nuxt',    // Add VueUse composables
    '@nuxt/eslint'     // Add ESLint integration
  ],

  ui: {
    colors: {
      primary: 'green',
      neutral: 'zinc'
    }
  },

  css: ['~/assets/css/main.css'],

  compatibilityDate: '2024-07-11'
})
```

---

## 10. Conversion Strategy & Implementation Plan

### Phase 1: Foundation (Architect & Coder)
**Goal**: Set up Zod schemas and basic structure

1. **Create schemas directory**
   ```bash
   mkdir -p sidecar/schemas
   ```

2. **Convert TypeScript types to Zod schemas**
   - Input: `app/types/index.d.ts`
   - Output: `sidecar/schemas/index.mjs`
   - Tasks:
     - Convert 3 type literals to Zod enums
     - Convert 7 interfaces to Zod objects
     - Export JSDoc typedefs

3. **Update sidecar package.json**
   - Verify `zod` is in dependencies (should already be there)
   - Add `@nuxt/ui`, `@vueuse/nuxt`, `@unovis/vue`
   - Add icon packages

### Phase 2: Composables & Utilities (Coder)
**Goal**: Convert shared logic to MJS

1. **Convert useDashboard.ts**
   - Input: `app/composables/useDashboard.ts`
   - Output: `sidecar/composables/useDashboard.mjs`
   - Add JSDoc annotations
   - Test keyboard shortcuts

2. **Convert app.config.ts**
   - Input: `app.config.ts`
   - Output: `sidecar/app.config.mjs`
   - Simple conversion (no types)

### Phase 3: Server API Routes (Coder)
**Goal**: Convert API endpoints to MJS

1. **Convert all 4 API routes**
   - `server/api/customers.ts` → `sidecar/server/api/customers.mjs`
   - `server/api/notifications.ts` → `sidecar/server/api/notifications.mjs`
   - `server/api/mails.ts` → `sidecar/server/api/mails.mjs`
   - `server/api/members.ts` → `sidecar/server/api/members.mjs`
   - Add Zod validation
   - Add JSDoc types

### Phase 4: Components (Coder)
**Goal**: Convert Vue components to MJS scripts

1. **Copy component files** to `sidecar/app/components/`
   - Keep `.vue` extension
   - Remove `lang="ts"` from script tags
   - Add JSDoc annotations
   - Update imports to use schemas

2. **Priority components to convert**:
   - NotificationsSlideover.vue (uses Notification type)
   - HomeStats.vue (uses Stat type)
   - HomeSales.vue (uses Sale type)
   - TeamsMenu.vue
   - UserMenu.vue

### Phase 5: Pages & Layouts (Coder)
**Goal**: Convert page components to MJS scripts

1. **Convert layout**
   - `app/layouts/default.vue` → `sidecar/app/layouts/default.vue`
   - Remove TypeScript
   - Add JSDoc

2. **Convert pages**
   - `app/pages/index.vue` → `sidecar/app/pages/index.vue`
   - `app/pages/customers.vue` → `sidecar/app/pages/customers.vue`
   - `app/pages/inbox.vue` → `sidecar/app/pages/inbox.vue`
   - `app/pages/settings.vue` → `sidecar/app/pages/settings.vue`

### Phase 6: Configuration & Cleanup (Coder)
**Goal**: Finalize Nuxt config and remove TS artifacts

1. **Update nuxt.config.mjs**
   - Add @nuxt/ui module
   - Add @vueuse/nuxt module
   - Configure UI colors
   - Add CSS file

2. **Remove TypeScript files**
   - Delete `tsconfig.json` (if exists in template copy)
   - Remove `typescript` and `vue-tsc` from devDependencies

3. **Update ESLint config** (if needed)
   - Ensure ESLint works with MJS

### Phase 7: Testing (Tester)
**Goal**: Validate all conversions

1. **Create Vitest tests**
   - Test Zod schemas validation
   - Test API routes return correct data
   - Test composables work correctly
   - Test components render

2. **Integration tests**
   - Test navigation works
   - Test keyboard shortcuts
   - Test notifications slideover
   - Test data fetching

### Phase 8: Sidecar Integration (Architect)
**Goal**: Integrate with unrdf Knowledge Hooks

1. **Add authentication layer**
   - Create `composables/useAuth.mjs`
   - Hook into sidecar validation
   - Add permission checks

2. **Add RDF persistence**
   - Store user preferences in RDF
   - Store dashboard state in RDF
   - Add SHACL validation

3. **Add cryptographic provenance**
   - Sign all data modifications
   - Verify data integrity

---

## 11. Dependencies Analysis

### Already Compatible (No Changes Needed)
- `nuxt`: ^4.1.2
- `zod`: ^4.1.11 ✅
- `date-fns`: ^4.1.0
- `@vueuse/nuxt`: ^13.9.0

### Need to Add to Sidecar
```bash
pnpm add @nuxt/ui@^4.0.0
pnpm add @iconify-json/lucide@^1.2.68
pnpm add @iconify-json/simple-icons@^1.2.53
pnpm add @unovis/ts@^1.6.1
pnpm add @unovis/vue@^1.6.1
pnpm add -D @nuxt/eslint@^1.9.0
```

### Can Remove After Conversion
```bash
pnpm remove -D typescript vue-tsc
```

---

## 12. File Conversion Checklist

### TypeScript Files to Convert (10 files)
- [ ] `app/app.config.ts` → `app/app.config.mjs`
- [ ] `app/composables/useDashboard.ts` → `app/composables/useDashboard.mjs`
- [ ] `app/types/index.d.ts` → `app/schemas/index.mjs` (Zod)
- [ ] `server/api/customers.ts` → `server/api/customers.mjs`
- [ ] `server/api/mails.ts` → `server/api/mails.mjs`
- [ ] `server/api/members.ts` → `server/api/members.mjs`
- [ ] `server/api/notifications.ts` → `server/api/notifications.mjs`
- [ ] `nuxt.config.ts` → Use existing `sidecar/nuxt.config.mjs`
- [ ] `tsconfig.json` → Delete (not needed)
- [ ] `eslint.config.mjs` → Merge with existing config

### Vue Components with TypeScript Scripts (10+ files)
- [ ] `app/app.vue` - Remove `lang="ts"`, add JSDoc
- [ ] `app/error.vue` - Remove `lang="ts"`, add JSDoc
- [ ] `app/layouts/default.vue` - Remove `lang="ts"`, add JSDoc
- [ ] `app/pages/index.vue` - Remove `lang="ts"`, add JSDoc
- [ ] `app/pages/customers.vue` - Remove `lang="ts"`, add JSDoc
- [ ] `app/pages/inbox.vue` - Remove `lang="ts"`, add JSDoc
- [ ] `app/pages/settings.vue` - Remove `lang="ts"`, add JSDoc
- [ ] `app/components/NotificationsSlideover.vue` - Remove `lang="ts"`, add JSDoc
- [ ] `app/components/TeamsMenu.vue` - Remove `lang="ts"`, add JSDoc
- [ ] `app/components/UserMenu.vue` - Remove `lang="ts"`, add JSDoc
- [ ] All `app/components/home/*.vue` - Remove `lang="ts"`, add JSDoc
- [ ] All `app/components/customers/*.vue` - Remove `lang="ts"`, add JSDoc
- [ ] All `app/components/inbox/*.vue` - Remove `lang="ts"`, add JSDoc
- [ ] All `app/components/settings/*.vue` - Remove `lang="ts"`, add JSDoc

---

## 13. Key Insights & Recommendations

### Strengths of the Template
1. ✅ **Already uses Zod** - Minimal conversion effort for validation
2. ✅ **Nuxt 4 compatible** - Matches sidecar's Nuxt version
3. ✅ **Modular component architecture** - Easy to integrate piece by piece
4. ✅ **File-based routing** - Simple to understand and maintain
5. ✅ **Composable pattern** - Single shared state manager
6. ✅ **Nuxt UI components** - Professional dashboard UI out of the box

### Challenges & Risks
1. ⚠️ **TypeScript everywhere** - All files need conversion
2. ⚠️ **Type imports in Vue components** - Need JSDoc replacements
3. ⚠️ **Chart library (@unovis)** - May have TypeScript dependencies
4. ⚠️ **No authentication** - Need to build this from scratch
5. ⚠️ **Static mock data** - Need to connect to real backend

### 80/20 Quick Win Strategy
**20% of effort for 80% of value:**

1. **Copy the entire component library** (1 hour)
   - Just copy all `.vue` files as-is
   - They'll work even with `lang="ts"` temporarily

2. **Convert only the 4 API routes** (2 hours)
   - These are critical for data flow
   - Simple conversions with Zod validation

3. **Convert the composables** (1 hour)
   - Only `useDashboard.ts` needs conversion
   - Critical for keyboard shortcuts

4. **Add to sidecar nuxt.config.mjs** (30 min)
   - Add `@nuxt/ui` module
   - Configure colors

5. **Test basic navigation** (30 min)
   - Verify routes work
   - Verify components render

**Total: 5 hours to get 80% functional dashboard**

Then gradually convert Vue components from TypeScript to MJS over time.

---

## 14. Questions for Architect & Coder Agents

### For Architect Agent:
1. Should we keep the dashboard as a separate Nuxt app or merge into existing sidecar?
2. How should we integrate with the existing RDF engine and Knowledge Hooks?
3. What authentication/authorization model should we use?
4. Should we add RDF persistence for dashboard state (filters, preferences)?
5. How should we handle cryptographic provenance for user actions?

### For Coder Agent:
1. Should we convert all components at once or incrementally?
2. How should we handle the `@unovis` chart library (TypeScript-heavy)?
3. Should we create a wrapper composable for sidecar integration?
4. How should we structure tests for the converted components?
5. Should we add OTEL observability to all API routes?

---

## 15. Next Steps for Swarm Coordination

### Immediate Actions (Architect):
1. Review this research document
2. Design integration architecture
3. Define Zod schema structure
4. Plan authentication/authorization approach
5. Create detailed implementation tasks

### Immediate Actions (Coder):
1. Set up schemas directory structure
2. Convert TypeScript types to Zod schemas
3. Convert the 4 API routes to MJS
4. Convert useDashboard composable
5. Test basic functionality

### Immediate Actions (Tester):
1. Set up Vitest test structure
2. Create schema validation tests
3. Create API route tests
4. Create composable tests
5. Plan integration test scenarios

---

## Appendix A: Example Conversions

### A.1 TypeScript Type → Zod Schema
```typescript
// BEFORE (TypeScript)
export interface User {
  id: number
  name: string
  email: string
  status: 'subscribed' | 'unsubscribed' | 'bounced'
}
```

```javascript
// AFTER (Zod + JSDoc)
export const UserSchema = z.object({
  id: z.number(),
  name: z.string(),
  email: z.string().email(),
  status: z.enum(['subscribed', 'unsubscribed', 'bounced'])
})

/** @typedef {z.infer<typeof UserSchema>} User */
```

### A.2 Vue Component Script
```vue
<!-- BEFORE (TypeScript) -->
<script setup lang="ts">
import type { User } from '~/types'

const users = ref<User[]>([])
</script>
```

```vue
<!-- AFTER (MJS + JSDoc) -->
<script setup>
/**
 * @typedef {import('~/schemas/index.mjs').User} User
 */

/** @type {import('vue').Ref<User[]>} */
const users = ref([])
</script>
```

### A.3 API Route
```typescript
// BEFORE (TypeScript)
import type { User } from '~/types'

const users: User[] = [...]

export default eventHandler(async () => {
  return users
})
```

```javascript
// AFTER (MJS + JSDoc + Zod)
import { UserSchema } from '~/schemas/index.mjs'

/**
 * @type {import('~/schemas/index.mjs').User[]}
 */
const users = [...]

export default eventHandler(async () => {
  // Validate data before returning
  return users.map(user => UserSchema.parse(user))
})
```

---

## Appendix B: Repository Statistics

- **Stars**: ~1.5k (popular template)
- **Language**: Vue 66.4%, TypeScript 32.9%
- **Files**: ~50+ files
- **Lines of Code**: ~2,500 (estimated)
- **Dependencies**: 9 production, 4 dev
- **License**: MIT
- **Last Updated**: Active development (2024-2025)

---

**END OF RESEARCH REPORT**

*Generated by Research Agent - Hive Mind Swarm*
*Session: swarm-1759363254478-gds008fsq*
*Date: 2025-10-01*
