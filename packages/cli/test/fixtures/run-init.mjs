/**
 * Runs the real `init` command through citty's runMain (same as the CLI entry point).
 * Used by test/commands/init.test.mjs so exit codes and console output are the real ones.
 */
import { runMain } from 'citty';
import { initCommand } from '../../src/cli/commands/init.mjs';

runMain(initCommand);
