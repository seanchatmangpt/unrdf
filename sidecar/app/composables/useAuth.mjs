/**
 * @file app/composables/useAuth.mjs
 * @description Authentication composable for user login/logout/registration
 */

import { ref, computed } from 'vue'
import { z } from 'zod'

/**
 * User schema for authentication
 */
const UserSchema = z.object({
  id: z.string(),
  email: z.string().email(),
  username: z.string().optional(),
  roles: z.array(z.string()).default([]),
  createdAt: z.string().optional()
})

/**
 * Login credentials schema
 */
const LoginCredentialsSchema = z.object({
  email: z.string().email(),
  password: z.string().min(8)
})

/**
 * Registration data schema
 */
const RegisterDataSchema = z.object({
  email: z.string().email(),
  password: z.string().min(8),
  username: z.string().optional()
})

/**
 * Auth response schema
 */
const AuthResponseSchema = z.object({
  user: UserSchema,
  token: z.string(),
  refreshToken: z.string().optional()
})

/**
 * Authentication composable - Internal implementation
 * @returns {{
 *   user: import('vue').Ref<Object|null>,
 *   token: import('vue').Ref<string|null>,
 *   isAuthenticated: import('vue').ComputedRef<boolean>,
 *   loading: import('vue').Ref<boolean>,
 *   error: import('vue').Ref<Error|null>,
 *   login: Function,
 *   register: Function,
 *   logout: Function,
 *   refresh: Function,
 *   fetchUser: Function
 * }}
 */
const _useAuth = () => {
  /** @type {import('vue').Ref<Object|null>} */
  const user = ref(null)

  /** @type {import('vue').Ref<string|null>} */
  const token = ref(null)

  /** @type {import('vue').Ref<boolean>} */
  const loading = ref(false)

  /** @type {import('vue').Ref<Error|null>} */
  const error = ref(null)

  /**
   * Computed: Whether user is authenticated
   */
  const isAuthenticated = computed(() => !!user.value && !!token.value)

  /**
   * Login user
   * @param {Object} credentials - Login credentials
   * @param {string} credentials.email - User email
   * @param {string} credentials.password - User password
   * @returns {Promise<boolean>}
   */
  async function login(credentials) {
    loading.value = true
    error.value = null

    try {
      // Validate input
      const validatedInput = LoginCredentialsSchema.parse(credentials)

      const data = await $fetch('/api/auth/login', {
        method: 'POST',
        body: validatedInput
      })

      // Validate response
      const validated = AuthResponseSchema.parse(data)

      user.value = validated.user
      token.value = validated.token

      // Store token in localStorage for persistence
      if (import.meta.client) {
        localStorage.setItem('auth_token', validated.token)
        if (validated.refreshToken) {
          localStorage.setItem('refresh_token', validated.refreshToken)
        }
      }

      return true
    } catch (err) {
      error.value = err
      console.error('[useAuth] Login failed:', err)
      user.value = null
      token.value = null
      return false
    } finally {
      loading.value = false
    }
  }

  /**
   * Register new user
   * @param {Object} registrationData - Registration data
   * @param {string} registrationData.email - User email
   * @param {string} registrationData.password - User password
   * @param {string} [registrationData.username] - Optional username
   * @returns {Promise<boolean>}
   */
  async function register(registrationData) {
    loading.value = true
    error.value = null

    try {
      // Validate input
      const validatedInput = RegisterDataSchema.parse(registrationData)

      const data = await $fetch('/api/auth/register', {
        method: 'POST',
        body: validatedInput
      })

      // Validate response
      const validated = AuthResponseSchema.parse(data)

      user.value = validated.user
      token.value = validated.token

      // Store token in localStorage
      if (import.meta.client) {
        localStorage.setItem('auth_token', validated.token)
        if (validated.refreshToken) {
          localStorage.setItem('refresh_token', validated.refreshToken)
        }
      }

      return true
    } catch (err) {
      error.value = err
      console.error('[useAuth] Registration failed:', err)
      user.value = null
      token.value = null
      return false
    } finally {
      loading.value = false
    }
  }

  /**
   * Logout user
   * @returns {Promise<void>}
   */
  async function logout() {
    loading.value = true
    error.value = null

    try {
      await $fetch('/api/auth/logout', {
        method: 'POST',
        headers: token.value ? { Authorization: `Bearer ${token.value}` } : {}
      })
    } catch (err) {
      console.error('[useAuth] Logout error:', err)
    } finally {
      // Clear state regardless of API success
      user.value = null
      token.value = null

      if (import.meta.client) {
        localStorage.removeItem('auth_token')
        localStorage.removeItem('refresh_token')
      }

      loading.value = false
    }
  }

  /**
   * Refresh authentication token
   * @returns {Promise<boolean>}
   */
  async function refresh() {
    loading.value = true
    error.value = null

    try {
      let refreshToken = null
      if (import.meta.client) {
        refreshToken = localStorage.getItem('refresh_token')
      }

      if (!refreshToken) {
        throw new Error('No refresh token available')
      }

      const data = await $fetch('/api/auth/refresh', {
        method: 'POST',
        body: { refreshToken }
      })

      // Validate response
      const validated = AuthResponseSchema.parse(data)

      user.value = validated.user
      token.value = validated.token

      if (import.meta.client) {
        localStorage.setItem('auth_token', validated.token)
        if (validated.refreshToken) {
          localStorage.setItem('refresh_token', validated.refreshToken)
        }
      }

      return true
    } catch (err) {
      error.value = err
      console.error('[useAuth] Token refresh failed:', err)
      user.value = null
      token.value = null
      return false
    } finally {
      loading.value = false
    }
  }

  /**
   * Fetch current user profile
   * @returns {Promise<Object|null>}
   */
  async function fetchUser() {
    loading.value = true
    error.value = null

    try {
      // Try to get token from localStorage if not in state
      if (!token.value && import.meta.client) {
        token.value = localStorage.getItem('auth_token')
      }

      if (!token.value) {
        throw new Error('No authentication token')
      }

      const data = await $fetch('/api/auth/me', {
        headers: {
          Authorization: `Bearer ${token.value}`
        }
      })

      // Validate response
      const validated = UserSchema.parse(data)
      user.value = validated

      return validated
    } catch (err) {
      error.value = err
      console.error('[useAuth] Failed to fetch user:', err)
      user.value = null
      token.value = null
      return null
    } finally {
      loading.value = false
    }
  }

  // Auto-fetch user on composable creation if token exists
  if (import.meta.client) {
    const storedToken = localStorage.getItem('auth_token')
    if (storedToken && !user.value) {
      token.value = storedToken
      fetchUser()
    }
  }

  return {
    user,
    token,
    isAuthenticated,
    loading,
    error,
    login,
    register,
    logout,
    refresh,
    fetchUser
  }
}

/**
 * Authentication composable with shared state
 * @type {() => {
 *   user: import('vue').Ref<Object|null>,
 *   token: import('vue').Ref<string|null>,
 *   isAuthenticated: import('vue').ComputedRef<boolean>,
 *   loading: import('vue').Ref<boolean>,
 *   error: import('vue').Ref<Error|null>,
 *   login: Function,
 *   register: Function,
 *   logout: Function,
 *   refresh: Function,
 *   fetchUser: Function
 * }}
 */
export const useAuth = createSharedComposable(_useAuth)
