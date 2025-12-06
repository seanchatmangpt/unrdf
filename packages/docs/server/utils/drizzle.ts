import { PGlite } from '@electric-sql/pglite'
import { drizzle } from 'drizzle-orm/pglite'

import * as schema from '../database/schema'

export { sql, eq, and, or, desc } from 'drizzle-orm'

export const tables = schema

let _db: ReturnType<typeof drizzle> | null = null

export function useDrizzle() {
  if (_db) return _db

  const dataDir = process.env.DATABASE_URL || './data/db'
  const client = new PGlite(dataDir)

  _db = drizzle(client, { schema })
  return _db
}

export type Chat = typeof schema.chats.$inferSelect
export type Message = typeof schema.messages.$inferSelect
