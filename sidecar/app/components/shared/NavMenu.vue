<script setup>
/**
 * @typedef {Object} NavItem
 * @property {string} path - Route path
 * @property {string} label - Display label
 * @property {string} [icon] - Optional icon class
 */

/**
 * Navigation menu items
 * @type {NavItem[]}
 */
const navItems = [
  { path: '/', label: 'Dashboard', icon: '🏠' },
  { path: '/hooks', label: 'Hooks', icon: '🔗' },
  { path: '/policies', label: 'Policies', icon: '📋' },
  { path: '/transactions', label: 'Transactions', icon: '💳' },
  { path: '/lockchain', label: 'Lockchain', icon: '🔒' },
  { path: '/settings', label: 'Settings', icon: '⚙️' }
]

/**
 * Check if current route matches the nav item path
 * @param {string} path
 * @returns {boolean}
 */
const isActive = (path) => {
  // Use Nuxt's useRoute if available
  if (typeof useRoute === 'function') {
    const route = useRoute()
    return route.path === path
  }
  // Fallback for development
  return false
}
</script>

<template>
  <nav class="nav-menu">
    <div class="nav-header">
      <h1 class="nav-title">UNRDF</h1>
      <p class="nav-subtitle">Knowledge Hooks</p>
    </div>

    <ul class="nav-list">
      <li v-for="item in navItems" :key="item.path" class="nav-item">
        <NuxtLink
          :to="item.path"
          :class="['nav-link', { 'nav-link-active': isActive(item.path) }]"
        >
          <span v-if="item.icon" class="nav-icon">{{ item.icon }}</span>
          <span class="nav-label">{{ item.label }}</span>
        </NuxtLink>
      </li>
    </ul>

    <div class="nav-footer">
      <div class="status-indicator">
        <span class="status-dot"></span>
        <span class="status-text">Runtime Active</span>
      </div>
    </div>
  </nav>
</template>

<style scoped>
.nav-menu {
  display: flex;
  flex-direction: column;
  height: 100%;
  background: #1e293b;
  color: white;
  padding: 1.5rem 0;
}

.nav-header {
  padding: 0 1.5rem 1.5rem;
  border-bottom: 1px solid rgba(255, 255, 255, 0.1);
}

.nav-title {
  font-size: 1.5em;
  font-weight: 700;
  margin: 0 0 0.25rem 0;
  color: white;
  letter-spacing: 0.05em;
}

.nav-subtitle {
  font-size: 0.85em;
  color: #94a3b8;
  margin: 0;
}

.nav-list {
  flex: 1;
  list-style: none;
  padding: 1rem 0;
  margin: 0;
}

.nav-item {
  margin: 0;
}

.nav-link {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 0.75rem 1.5rem;
  color: #cbd5e1;
  text-decoration: none;
  transition: all 0.2s;
  border-left: 3px solid transparent;
}

.nav-link:hover {
  background: rgba(255, 255, 255, 0.05);
  color: white;
}

.nav-link-active {
  background: rgba(59, 130, 246, 0.1);
  color: white;
  border-left-color: #3b82f6;
}

.nav-icon {
  font-size: 1.2em;
  width: 1.5em;
  text-align: center;
}

.nav-label {
  font-size: 0.95em;
  font-weight: 500;
}

.nav-footer {
  padding: 1rem 1.5rem;
  border-top: 1px solid rgba(255, 255, 255, 0.1);
}

.status-indicator {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 0.85em;
  color: #94a3b8;
}

.status-dot {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: #22c55e;
  animation: pulse 2s ease-in-out infinite;
}

@keyframes pulse {
  0%, 100% {
    opacity: 1;
  }
  50% {
    opacity: 0.5;
  }
}

.status-text {
  font-weight: 500;
}

/* Responsive design */
@media (max-width: 768px) {
  .nav-menu {
    flex-direction: row;
    height: auto;
    padding: 0;
  }

  .nav-header {
    display: none;
  }

  .nav-list {
    display: flex;
    flex-direction: row;
    padding: 0;
    flex: 1;
    overflow-x: auto;
  }

  .nav-item {
    flex-shrink: 0;
  }

  .nav-link {
    flex-direction: column;
    padding: 0.75rem 1rem;
    gap: 0.25rem;
    border-left: none;
    border-bottom: 3px solid transparent;
  }

  .nav-link-active {
    border-left-color: transparent;
    border-bottom-color: #3b82f6;
  }

  .nav-icon {
    font-size: 1.5em;
  }

  .nav-label {
    font-size: 0.75em;
  }

  .nav-footer {
    display: none;
  }
}
</style>
