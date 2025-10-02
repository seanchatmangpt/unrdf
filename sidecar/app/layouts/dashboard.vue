<template>
  <div class="dashboard-layout">
    <header class="dashboard-header">
      <h1>UNRDF Observability Dashboard</h1>
      <nav class="dashboard-nav">
        <NuxtLink to="/observability">Metrics</NuxtLink>
        <NuxtLink to="/observability/traces">Traces</NuxtLink>
        <NuxtLink to="/observability/logs">Logs</NuxtLink>
      </nav>
    </header>

    <main class="dashboard-main">
      <slot />
    </main>

    <footer class="dashboard-footer">
      <p>UNRDF Sidecar v1.0.0 | OpenTelemetry Enabled</p>
    </footer>
  </div>
</template>

<script setup>
/**
 * @file app/layouts/dashboard.vue
 * @description Dashboard layout for observability interface
 */
</script>

<style scoped>
.dashboard-layout {
  display: flex;
  flex-direction: column;
  min-height: 100vh;
  background: #1a1a1a;
  color: #e0e0e0;
}

.dashboard-header {
  background: #2a2a2a;
  padding: 1rem 2rem;
  border-bottom: 2px solid #3a3a3a;
}

.dashboard-header h1 {
  margin: 0 0 1rem 0;
  font-size: 1.5rem;
  color: #60a5fa;
}

.dashboard-nav {
  display: flex;
  gap: 1.5rem;
}

.dashboard-nav a {
  color: #9ca3af;
  text-decoration: none;
  padding: 0.5rem 1rem;
  border-radius: 4px;
  transition: all 0.2s;
}

.dashboard-nav a:hover {
  color: #60a5fa;
  background: #3a3a3a;
}

.dashboard-nav a.router-link-active {
  color: #60a5fa;
  background: #3a3a3a;
}

.dashboard-main {
  flex: 1;
  padding: 2rem;
  overflow-y: auto;
}

.dashboard-footer {
  background: #2a2a2a;
  padding: 1rem 2rem;
  border-top: 2px solid #3a3a3a;
  text-align: center;
  color: #6b7280;
  font-size: 0.875rem;
}
</style>
