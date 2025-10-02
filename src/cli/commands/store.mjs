/**
 * @fileoverview Store command - RDF store management operations
 *
 * @description
 * CLI commands for backup, restore, and import operations on RDF stores.
 * All operations are instrumented with OpenTelemetry traces and metrics.
 *
 * @module cli/commands/store
 * @version 2.1.1
 * @license MIT
 */

import { defineCommand } from 'citty';
import { backupStore } from '../store-backup.mjs';
import { restoreStore } from '../store-restore.mjs';
import { importStore } from '../store-import.mjs';

/**
 * Store command with subcommands for backup, restore, and import
 */
export const storeCommand = defineCommand({
  meta: {
    name: 'store',
    description: 'RDF store management operations (backup, restore, import)'
  },
  subCommands: {
    backup: defineCommand({
      meta: {
        name: 'backup',
        description: 'Create compressed backup of RDF store'
      },
      args: {
        storePath: {
          type: 'positional',
          description: 'Path to RDF store directory',
          required: true
        },
        output: {
          type: 'string',
          description: 'Output backup file path (default: backup-{timestamp}.tar.gz)',
          alias: 'o'
        },
        incremental: {
          type: 'boolean',
          description: 'Create incremental backup',
          default: false,
          alias: 'i'
        },
        compress: {
          type: 'boolean',
          description: 'Compress backup with gzip',
          default: true,
          alias: 'c'
        }
      },
      async run({ args }) {
        const { storePath, output, incremental, compress } = args;

        try {
          const result = await backupStore(storePath, {
            output,
            incremental,
            compress
          });

          console.log('✅ Backup completed successfully');
          console.log(`📦 Backup file: ${result.backupPath}`);
          console.log(`📊 Size: ${(result.size / 1024 / 1024).toFixed(2)} MB`);
          console.log(`🔢 Quads backed up: ${result.quadCount}`);
          console.log(`📈 Graphs: ${result.graphCount}`);
          console.log(`⏱️  Duration: ${result.duration}ms`);

          return result;
        } catch (error) {
          console.error('❌ Backup failed:', error.message);
          throw error;
        }
      }
    }),

    restore: defineCommand({
      meta: {
        name: 'restore',
        description: 'Restore RDF store from backup'
      },
      args: {
        backupPath: {
          type: 'positional',
          description: 'Path to backup file',
          required: true
        },
        target: {
          type: 'string',
          description: 'Target store directory path',
          required: true,
          alias: 't'
        },
        overwrite: {
          type: 'boolean',
          description: 'Overwrite existing store',
          default: false,
          alias: 'f'
        },
        validate: {
          type: 'boolean',
          description: 'Validate backup before restore',
          default: true,
          alias: 'v'
        }
      },
      async run({ args }) {
        const { backupPath, target, overwrite, validate } = args;

        try {
          const result = await restoreStore(backupPath, {
            target,
            overwrite,
            validate
          });

          console.log('✅ Restore completed successfully');
          console.log(`📂 Store path: ${result.storePath}`);
          console.log(`🔢 Quads restored: ${result.quadCount}`);
          console.log(`📈 Graphs restored: ${result.graphCount}`);
          console.log(`⏱️  Duration: ${result.duration}ms`);

          return result;
        } catch (error) {
          console.error('❌ Restore failed:', error.message);
          throw error;
        }
      }
    }),

    import: defineCommand({
      meta: {
        name: 'import',
        description: 'Bulk import RDF files into store'
      },
      args: {
        files: {
          type: 'positional',
          description: 'RDF files to import (glob patterns supported)',
          required: true,
          isArray: true
        },
        storePath: {
          type: 'string',
          description: 'Target store directory path',
          required: true,
          alias: 's'
        },
        format: {
          type: 'string',
          description: 'RDF format (turtle, n-quads, json-ld, auto)',
          default: 'auto',
          alias: 'f'
        },
        graph: {
          type: 'string',
          description: 'Target named graph IRI',
          alias: 'g'
        },
        skipErrors: {
          type: 'boolean',
          description: 'Continue on file errors',
          default: false,
          alias: 'e'
        }
      },
      async run({ args }) {
        const { files, storePath, format, graph, skipErrors } = args;

        try {
          const result = await importStore(files, {
            storePath,
            format,
            graph,
            skipErrors
          });

          console.log('✅ Import completed successfully');
          console.log(`📂 Store path: ${result.storePath}`);
          console.log(`📄 Files imported: ${result.filesImported}/${result.totalFiles}`);
          console.log(`🔢 Quads imported: ${result.quadCount}`);
          console.log(`📈 Graphs: ${result.graphCount}`);
          console.log(`⏱️  Duration: ${result.duration}ms`);

          if (result.errors.length > 0) {
            console.warn(`⚠️  Errors encountered: ${result.errors.length}`);
            result.errors.forEach(err => {
              console.warn(`  - ${err.file}: ${err.message}`);
            });
          }

          return result;
        } catch (error) {
          console.error('❌ Import failed:', error.message);
          throw error;
        }
      }
    })
  }
});
