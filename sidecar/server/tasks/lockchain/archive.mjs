// @ts-check
/**
 * Lockchain Archive Task
 *
 * Scheduled: Daily at midnight (0 0 * * *)
 *
 * Archives old lockchain entries to:
 * - Free up database space
 * - Compress historical data
 * - Store in cold storage (S3/filesystem)
 * - Maintain query performance
 */

import { trace } from '@opentelemetry/api'
import { circuitBreakerRegistry } from '../../utils/circuit-breaker.mjs'
import { writeFile, mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { createGzip } from 'node:zlib'
import { pipeline } from 'node:stream/promises'
import { Readable } from 'node:stream'

export default defineTask({
  meta: {
    name: 'lockchain:archive',
    description: 'Archive old lockchain entries with compression',
    version: '1.0.0'
  },

  async run({ payload, context: taskContext }) {
    const tracer = trace.getTracer('nitro-tasks')
    const span = tracer.startSpan('task.lockchain.archive')

    try {
      console.log('[Task:lockchain:archive] Starting lockchain archival...')

      // Get lockchain manager from context
      const lockchainManager = taskContext?.lockchainManager
      if (!lockchainManager) {
        throw new Error('Lockchain manager not available in task context')
      }

      const results = {
        archived: 0,
        compressed: 0,
        bytesCompressed: 0,
        errors: 0,
        timestamp: new Date().toISOString()
      }

      // Archive entries older than 90 days
      const archiveThreshold = Date.now() - (90 * 24 * 60 * 60 * 1000)
      const archiveDir = process.env.KGC_ARCHIVE_DIR || join(process.cwd(), '.archives')

      // Create archive directory if needed
      await mkdir(archiveDir, { recursive: true })

      // Use circuit breaker for archive operations
      const breaker = circuitBreakerRegistry.get('lockchain-archiver', {
        failureThreshold: 3,
        timeout: 120000 // 2 minutes for compression
      })

      await breaker.execute(async () => {
        // Get old entries
        const oldEntries = await lockchainManager.getEntriesBefore(archiveThreshold)
        console.log(`[Task:lockchain:archive] Found ${oldEntries.length} entries to archive`)

        if (oldEntries.length === 0) {
          return
        }

        // Group by month for compression
        const entriesByMonth = groupEntriesByMonth(oldEntries)

        for (const [month, entries] of Object.entries(entriesByMonth)) {
          try {
            // Compress and write to archive
            const archiveFile = join(archiveDir, `lockchain-${month}.json.gz`)
            const jsonData = JSON.stringify(entries, null, 2)

            // Compress with gzip
            const readable = Readable.from([jsonData])
            const gzip = createGzip({ level: 9 }) // Maximum compression
            const writeStream = require('fs').createWriteStream(archiveFile)

            await pipeline(readable, gzip, writeStream)

            results.archived += entries.length
            results.compressed++
            results.bytesCompressed += Buffer.byteLength(jsonData)

            console.log(`[Task:lockchain:archive] Archived ${entries.length} entries to ${archiveFile}`)

            // Delete archived entries from active storage
            await lockchainManager.deleteEntries(entries.map(e => e.id))

          } catch (archiveError) {
            console.error(`[Task:lockchain:archive] Error archiving month ${month}:`, archiveError)
            results.errors++
            span.recordException(archiveError instanceof Error ? archiveError : new Error(String(archiveError)))
          }
        }
      })

      // Record metrics
      span.setAttributes({
        'lockchain.archived': results.archived,
        'lockchain.compressed': results.compressed,
        'lockchain.bytes_compressed': results.bytesCompressed,
        'lockchain.errors': results.errors
      })

      console.log('[Task:lockchain:archive] Archival complete:', results)

      return { result: 'success', details: results }
    } catch (error) {
      span.recordException(error instanceof Error ? error : new Error(String(error)))
      span.setAttribute('task.error', true)
      throw error
    } finally {
      span.end()
    }
  }
})

/**
 * Group lockchain entries by month
 * @param {Array} entries - Lockchain entries
 * @returns {Object<string, Array>}
 */
function groupEntriesByMonth(entries) {
  const grouped = {}

  for (const entry of entries) {
    const date = new Date(entry.timestamp || entry.created_at)
    const monthKey = `${date.getFullYear()}-${String(date.getMonth() + 1).padStart(2, '0')}`

    if (!grouped[monthKey]) {
      grouped[monthKey] = []
    }
    grouped[monthKey].push(entry)
  }

  return grouped
}
