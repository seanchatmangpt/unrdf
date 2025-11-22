import { describe, it } from 'vitest';
import { runCitty } from 'citty-test-utils';

describe('CLI Baseline', () => {
  it('shows help', async () => {
    const result = await runCitty(['--help'], { cliPath: './cli/index.mjs' });
    result.expectSuccess();
  });

  it('runs a wired subcommand: context list (fast mode)', async () => {
    const result = await runCitty(['--fast', 'context', 'list'], {
      cliPath: './cli/index.mjs',
    });
    result.expectSuccess();
  });
});
