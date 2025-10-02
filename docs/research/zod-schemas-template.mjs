/**
 * @file sidecar/schemas/index.mjs
 * @description Zod validation schemas for dashboard data types
 * @template This is a TEMPLATE file for the Coder agent to use as reference
 */

import { z } from 'zod'

// ============================================================================
// ENUMS (Type Literals → Zod Enums)
// ============================================================================

/**
 * User subscription status
 * @type {z.ZodEnum<['subscribed', 'unsubscribed', 'bounced']>}
 */
export const UserStatusSchema = z.enum(['subscribed', 'unsubscribed', 'bounced'])

/**
 * Sale transaction status
 * @type {z.ZodEnum<['paid', 'failed', 'refunded']>}
 */
export const SaleStatusSchema = z.enum(['paid', 'failed', 'refunded'])

/**
 * Time period for data aggregation
 * @type {z.ZodEnum<['daily', 'weekly', 'monthly']>}
 */
export const PeriodSchema = z.enum(['daily', 'weekly', 'monthly'])

// ============================================================================
// OBJECT SCHEMAS (Interfaces → Zod Objects)
// ============================================================================

/**
 * User entity schema
 * @type {z.ZodObject}
 */
export const UserSchema = z.object({
  id: z.number().int().positive(),
  name: z.string().min(1),
  email: z.string().email(),
  avatar: z.object({
    src: z.string().url()
  }),
  status: UserStatusSchema,
  location: z.string()
})

/**
 * Email message schema
 * @type {z.ZodObject}
 */
export const MailSchema = z.object({
  id: z.number().int().positive(),
  unread: z.boolean().optional(),
  from: UserSchema,
  subject: z.string(),
  body: z.string(),
  date: z.string().datetime() // ISO 8601 format
})

/**
 * Team member schema
 * @type {z.ZodObject}
 */
export const MemberSchema = z.object({
  name: z.string().min(1),
  username: z.string().min(1),
  role: z.enum(['member', 'owner']),
  avatar: z.object({
    src: z.string().url()
  })
})

/**
 * Dashboard statistic schema
 * @type {z.ZodObject}
 */
export const StatSchema = z.object({
  title: z.string(),
  icon: z.string(),
  value: z.number(),
  variation: z.object({
    value: z.number(),
    positive: z.boolean()
  }),
  // Function types in Zod are complex, make optional
  formatter: z.function().args(z.number()).returns(z.string()).optional()
})

/**
 * Sale transaction schema
 * @type {z.ZodObject}
 */
export const SaleSchema = z.object({
  id: z.number().int().positive(),
  date: z.string().datetime(),
  status: SaleStatusSchema,
  email: z.string().email(),
  amount: z.number().nonnegative()
})

/**
 * Notification message schema
 * @type {z.ZodObject}
 */
export const NotificationSchema = z.object({
  id: z.number().int().positive(),
  unread: z.boolean().optional(),
  sender: UserSchema,
  body: z.string(),
  date: z.string().datetime()
})

/**
 * Date range schema
 * @type {z.ZodObject}
 */
export const RangeSchema = z.object({
  start: z.date(),
  end: z.date()
}).refine(
  (data) => data.start <= data.end,
  { message: 'Start date must be before or equal to end date' }
)

// ============================================================================
// JSDOC TYPEDEFS (for IDE autocomplete)
// ============================================================================

/**
 * User subscription status type
 * @typedef {z.infer<typeof UserStatusSchema>} UserStatus
 */

/**
 * Sale transaction status type
 * @typedef {z.infer<typeof SaleStatusSchema>} SaleStatus
 */

/**
 * Time period type
 * @typedef {z.infer<typeof PeriodSchema>} Period
 */

/**
 * User entity type
 * @typedef {z.infer<typeof UserSchema>} User
 */

/**
 * Email message type
 * @typedef {z.infer<typeof MailSchema>} Mail
 */

/**
 * Team member type
 * @typedef {z.infer<typeof MemberSchema>} Member
 */

/**
 * Dashboard statistic type
 * @typedef {z.infer<typeof StatSchema>} Stat
 */

/**
 * Sale transaction type
 * @typedef {z.infer<typeof SaleSchema>} Sale
 */

/**
 * Notification message type
 * @typedef {z.infer<typeof NotificationSchema>} Notification
 */

/**
 * Date range type
 * @typedef {z.infer<typeof RangeSchema>} Range
 */

// ============================================================================
// VALIDATION HELPERS
// ============================================================================

/**
 * Validate and parse user data
 * @param {unknown} data - Data to validate
 * @returns {User} Validated user object
 * @throws {z.ZodError} If validation fails
 */
export function validateUser(data) {
  return UserSchema.parse(data)
}

/**
 * Safely validate user data without throwing
 * @param {unknown} data - Data to validate
 * @returns {{success: true, data: User} | {success: false, error: z.ZodError}}
 */
export function safeValidateUser(data) {
  return UserSchema.safeParse(data)
}

/**
 * Validate array of users
 * @param {unknown[]} data - Array of user data
 * @returns {User[]} Validated users array
 */
export function validateUsers(data) {
  return z.array(UserSchema).parse(data)
}

/**
 * Validate notification data
 * @param {unknown} data - Data to validate
 * @returns {Notification} Validated notification
 */
export function validateNotification(data) {
  return NotificationSchema.parse(data)
}

/**
 * Validate sale data
 * @param {unknown} data - Data to validate
 * @returns {Sale} Validated sale
 */
export function validateSale(data) {
  return SaleSchema.parse(data)
}

// ============================================================================
// USAGE EXAMPLES (for documentation)
// ============================================================================

/**
 * Example usage in API routes:
 *
 * ```javascript
 * import { validateUsers, UserSchema } from '~/schemas/index.mjs'
 *
 * export default eventHandler(async () => {
 *   const users = [
 *     { id: 1, name: 'John', email: 'john@example.com', ... }
 *   ]
 *
 *   // Validate all users before returning
 *   return validateUsers(users)
 * })
 * ```
 *
 * Example usage in Vue components:
 *
 * ```vue
 * <script setup>
 * /**
 *  * @typedef {import('~/schemas/index.mjs').User} User
 *  *\/
 *
 * /** @type {import('vue').Ref<User[]>} *\/
 * const users = ref([])
 *
 * const { data } = await useFetch('/api/customers')
 * users.value = data.value
 * </script>
 * ```
 */
