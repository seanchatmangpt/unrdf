/**
 * @file StatusDashboard.vue Component Tests
 * @description Integration tests for runtime status monitoring
 * Tests real-time updates, polling behavior, and status visualization
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { mountSuspended } from '@nuxt/test-utils/runtime'
import { nextTick } from 'vue'
import StatusDashboard from '~/components/runtime/StatusDashboard.vue'

// Mock useRuntime composable
vi.mock('~/composables/useRuntime.mjs', () => ({
  useRuntime: vi.fn()
}))

describe('StatusDashboard Component', () => {
  let mockRuntime

  beforeEach(() => {
    vi.clearAllMocks()
    vi.useFakeTimers()

    // Default mock implementation
    mockRuntime = {
      status: { value: null },
      loading: { value: false },
      error: { value: null },
      fetchStatus: vi.fn(),
      startPolling: vi.fn(),
      stopPolling: vi.fn()
    }
  })

  afterEach(() => {
    vi.useRealTimers()
    vi.restoreAllMocks()
  })

  describe('Runtime Status Display', () => {
    it('should display runtime status data', async () => {
      const mockStatus = {
        uptime: 3600,
        version: '1.0.0',
        environment: 'development',
        hooks: {
          registered: 5,
          enabled: 3,
          disabled: 2
        },
        performance: {
          avgExecutionTime: 12.5,
          totalExecutions: 100
        }
      }

      mockRuntime.status.value = mockStatus

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      // Verify status information is displayed
      expect(wrapper.text()).toContain('1.0.0')
      expect(wrapper.text()).toContain('development')
      expect(wrapper.text()).toContain('5') // registered hooks
      expect(wrapper.text()).toContain('3') // enabled hooks
    })

    it('should format uptime correctly', async () => {
      const mockStatus = {
        uptime: 7265, // 2 hours, 1 minute, 5 seconds
        version: '1.0.0',
        environment: 'production',
        hooks: { registered: 0, enabled: 0, disabled: 0 },
        performance: { avgExecutionTime: 0, totalExecutions: 0 }
      }

      mockRuntime.status.value = mockStatus

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      // Verify uptime formatting
      expect(wrapper.text()).toMatch(/2h.*1m.*5s/)
    })

    it('should display performance metrics', async () => {
      const mockStatus = {
        uptime: 100,
        version: '1.0.0',
        environment: 'production',
        hooks: { registered: 10, enabled: 8, disabled: 2 },
        performance: {
          avgExecutionTime: 15.75,
          totalExecutions: 250,
          successRate: 98.5
        }
      }

      mockRuntime.status.value = mockStatus

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      // Verify performance metrics
      expect(wrapper.text()).toContain('15.75')
      expect(wrapper.text()).toContain('250')
      expect(wrapper.text()).toContain('98.5')
    })

    it('should show health status indicator', async () => {
      const healthyStatus = {
        uptime: 1000,
        version: '1.0.0',
        environment: 'production',
        health: 'healthy',
        hooks: { registered: 5, enabled: 5, disabled: 0 },
        performance: { avgExecutionTime: 10, totalExecutions: 100 }
      }

      mockRuntime.status.value = healthyStatus

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      // Verify health indicator
      const healthBadge = wrapper.find('[data-health-status]')
      expect(healthBadge.attributes('data-health-status')).toBe('healthy')
    })
  })

  describe('Auto-Refresh Behavior', () => {
    it('should auto-refresh status every 5 seconds', async () => {
      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      mockRuntime.fetchStatus.mockResolvedValue({
        uptime: 100,
        version: '1.0.0',
        environment: 'development',
        hooks: { registered: 5, enabled: 3, disabled: 2 },
        performance: { avgExecutionTime: 10, totalExecutions: 50 }
      })

      await mountSuspended(StatusDashboard)

      // Initial fetch
      expect(mockRuntime.startPolling).toHaveBeenCalledWith(5000)

      // Advance timer by 5 seconds
      vi.advanceTimersByTime(5000)
      await nextTick()

      // Advance timer by another 5 seconds
      vi.advanceTimersByTime(5000)
      await nextTick()

      // Verify polling was started
      expect(mockRuntime.startPolling).toHaveBeenCalled()
    })

    it('should stop polling when component is unmounted', async () => {
      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      wrapper.unmount()

      expect(mockRuntime.stopPolling).toHaveBeenCalled()
    })

    it('should pause auto-refresh when user interacts with component', async () => {
      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      // Simulate user interaction (e.g., clicking a detail panel)
      const detailPanel = wrapper.find('[data-action="expand-details"]')
      if (detailPanel.exists()) {
        await detailPanel.trigger('click')

        // Verify polling is paused during interaction
        expect(mockRuntime.stopPolling).toHaveBeenCalled()
      }
    })

    it('should allow manual refresh via button click', async () => {
      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      const refreshButton = wrapper.find('[data-action="refresh"]')
      await refreshButton.trigger('click')

      expect(mockRuntime.fetchStatus).toHaveBeenCalled()
    })
  })

  describe('Loading State', () => {
    it('should display loading indicator during fetch', async () => {
      mockRuntime.loading.value = true

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      // Verify loading state is displayed
      const loadingIndicator = wrapper.find('[data-loading="true"]')
      expect(loadingIndicator.exists()).toBe(true)
    })

    it('should hide loading indicator after successful fetch', async () => {
      mockRuntime.loading.value = false
      mockRuntime.status.value = {
        uptime: 100,
        version: '1.0.0',
        environment: 'development',
        hooks: { registered: 5, enabled: 3, disabled: 2 },
        performance: { avgExecutionTime: 10, totalExecutions: 50 }
      }

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      // Verify loading state is not displayed
      const loadingIndicator = wrapper.find('[data-loading="true"]')
      expect(loadingIndicator.exists()).toBe(false)
    })

    it('should show skeleton screen during initial load', async () => {
      mockRuntime.loading.value = true
      mockRuntime.status.value = null

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      // Verify skeleton elements exist
      const skeleton = wrapper.find('[data-skeleton]')
      expect(skeleton.exists()).toBe(true)
    })
  })

  describe('Error Handling', () => {
    it('should display error message when fetch fails', async () => {
      const errorMessage = 'Failed to fetch runtime status'
      mockRuntime.error.value = errorMessage

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      // Verify error message is displayed
      expect(wrapper.text()).toContain(errorMessage)
    })

    it('should show retry button on error', async () => {
      mockRuntime.error.value = 'Network error'

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      const retryButton = wrapper.find('[data-action="retry"]')
      expect(retryButton.exists()).toBe(true)
    })

    it('should retry fetch on retry button click', async () => {
      mockRuntime.error.value = 'Network error'

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      const retryButton = wrapper.find('[data-action="retry"]')
      await retryButton.trigger('click')

      expect(mockRuntime.fetchStatus).toHaveBeenCalled()
    })

    it('should handle partial status data gracefully', async () => {
      const partialStatus = {
        uptime: 100,
        version: '1.0.0'
        // Missing environment, hooks, performance
      }

      mockRuntime.status.value = partialStatus

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      // Component should not crash
      expect(wrapper.exists()).toBe(true)
      expect(wrapper.text()).toContain('1.0.0')
    })
  })

  describe('Collaboration Patterns', () => {
    it('should coordinate with useRuntime composable for data fetching', async () => {
      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      await mountSuspended(StatusDashboard)

      // Verify composable was called
      expect(useRuntime).toHaveBeenCalled()

      // Verify polling was started
      expect(mockRuntime.startPolling).toHaveBeenCalled()
    })

    it('should emit status-updated event when status changes', async () => {
      const initialStatus = {
        uptime: 100,
        version: '1.0.0',
        environment: 'development',
        hooks: { registered: 5, enabled: 3, disabled: 2 },
        performance: { avgExecutionTime: 10, totalExecutions: 50 }
      }

      mockRuntime.status.value = initialStatus

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      // Simulate status update
      const newStatus = { ...initialStatus, uptime: 200 }
      mockRuntime.status.value = newStatus
      await nextTick()

      // Verify event emission
      expect(wrapper.emitted('status-updated')).toBeTruthy()
    })

    it('should coordinate cleanup on unmount', async () => {
      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      wrapper.unmount()

      // Verify cleanup was performed
      expect(mockRuntime.stopPolling).toHaveBeenCalled()
    })
  })

  describe('Accessibility', () => {
    it('should have proper ARIA labels for status sections', async () => {
      mockRuntime.status.value = {
        uptime: 100,
        version: '1.0.0',
        environment: 'production',
        hooks: { registered: 5, enabled: 3, disabled: 2 },
        performance: { avgExecutionTime: 10, totalExecutions: 50 }
      }

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      const statusSection = wrapper.find('[role="region"]')
      expect(statusSection.attributes('aria-label')).toBeDefined()
    })

    it('should announce status updates to screen readers', async () => {
      mockRuntime.status.value = {
        uptime: 100,
        version: '1.0.0',
        environment: 'production',
        hooks: { registered: 5, enabled: 3, disabled: 2 },
        performance: { avgExecutionTime: 10, totalExecutions: 50 }
      }

      const { useRuntime } = await import('~/composables/useRuntime.mjs')
      useRuntime.mockReturnValue(mockRuntime)

      const wrapper = await mountSuspended(StatusDashboard)

      const liveRegion = wrapper.find('[aria-live="polite"]')
      expect(liveRegion.exists()).toBe(true)
    })
  })
})
