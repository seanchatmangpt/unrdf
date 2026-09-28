#!/usr/bin/env node
/**
 * Build the continuum tier fixtures: one .avm per tier from
 * src/erlang/tier_witness.erl (compiled with -DTIER=<tier>).
 *
 *   node scripts/build-tier-fixtures.mjs        # needs erlc (ERLC_BIN overrides)
 *
 * Also builds negative-test programs (test/fixtures/programs) and the
 * hello_world.avm shipped in public/ and playground/public.
 *
 * Output: test/fixtures/tiers/tier_<tier>.avm (committed so the e2e suite runs
 * without a compiler; rebuild after editing tier_witness.erl).
 */
import { execFileSync } from 'node:child_process';
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { packAvm } from '../src/avm-packer.mjs';

export const TIERS = Object.freeze(['browser', 'edge', 'fog', 'cloud']);

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const source = join(root, 'src/erlang/tier_witness.erl');
const outDir = join(root, 'test/fixtures/tiers');

export function buildTierFixtures() {
  const erlc = process.env.ERLC_BIN || 'erlc';
  mkdirSync(outDir, { recursive: true });
  const work = mkdtempSync(join(tmpdir(), 'tier-witness-'));
  const built = [];
  try {
    for (const tier of TIERS) {
      execFileSync(erlc, [`-DTIER=${tier}`, '-o', work, source], { stdio: 'inherit' });
      const beam = readFileSync(join(work, 'tier_witness.beam'));
      const avmPath = join(outDir, `tier_${tier}.avm`);
      writeFileSync(
        avmPath,
        packAvm([{ name: 'tier_witness.beam', data: new Uint8Array(beam), start: true }])
      );
      built.push(avmPath);
    }
    // Negative-test programs (crash, runaway).
    const programsDir = join(root, 'test/fixtures/programs');
    mkdirSync(programsDir, { recursive: true });
    for (const name of ['loop_forever', 'crash_now']) {
      execFileSync(erlc, ['-o', work, join(root, `test/fixtures/erlang/${name}.erl`)], {
        stdio: 'inherit',
      });
      const avmPath = join(programsDir, `${name}.avm`);
      writeFileSync(
        avmPath,
        packAvm([
          {
            name: `${name}.beam`,
            data: new Uint8Array(readFileSync(join(work, `${name}.beam`))),
            start: true,
          },
        ])
      );
      built.push(avmPath);
    }
    // The published smoke-test application shipped in public/ and the playground.
    execFileSync(erlc, ['-o', work, join(root, 'src/erlang/hello_world.erl')], {
      stdio: 'inherit',
    });
    const hello = packAvm([
      {
        name: 'hello_world.beam',
        data: new Uint8Array(readFileSync(join(work, 'hello_world.beam'))),
        start: true,
      },
    ]);
    for (const dir of [join(root, 'public'), join(root, 'playground/public')]) {
      const path = join(dir, 'hello_world.avm');
      writeFileSync(path, hello);
      built.push(path);
    }
  } finally {
    rmSync(work, { recursive: true, force: true });
  }
  return built;
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  for (const path of buildTierFixtures()) console.log(`built ${path}`);
}
