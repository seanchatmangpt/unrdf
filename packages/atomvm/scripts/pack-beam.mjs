/**
 * Pack one BEAM file into a runnable .avm, preferring the native PackBEAM tool
 * and falling back to the dependency-free JS packer (src/avm-packer.mjs).
 *
 * Selection:
 * - PACKBEAM_BIN=/path/to/PackBEAM  -> that binary, no fallback (explicit choice)
 * - otherwise `PackBEAM` on PATH    -> if it can be executed
 * - otherwise                       -> JS packer (so a machine with only erlc can build)
 *
 * PackBEAM's CLI is positional: `PackBEAM out.avm in.beam ...` (there is no -o).
 */
import { execFileSync, spawnSync } from 'node:child_process';
import { readFileSync, writeFileSync } from 'node:fs';
import { basename } from 'node:path';
import { packAvm, parseAvm } from '../src/avm-packer.mjs';

/**
 * @returns {{kind: 'native', binary: string} | {kind: 'js'}} the packer that will be used
 */
export function resolvePacker(env = process.env) {
  if (env.PACKBEAM_BIN) return { kind: 'native', binary: env.PACKBEAM_BIN };
  const probe = spawnSync('PackBEAM', ['-h'], { stdio: 'ignore', env });
  if (probe.error && probe.error.code === 'ENOENT') return { kind: 'js' };
  return { kind: 'native', binary: 'PackBEAM' };
}

/**
 * @param {string} avmFile - output path
 * @param {string} beamFile - input .beam (becomes the start module)
 * @param {NodeJS.ProcessEnv} [env]
 * @returns {{kind: 'native' | 'js'}}
 */
export function packBeamFile(avmFile, beamFile, env = process.env) {
  const packer = resolvePacker(env);
  if (packer.kind === 'native') {
    try {
      execFileSync(packer.binary, [avmFile, beamFile], { stdio: 'inherit' });
    } catch (error) {
      throw new Error(`PackBEAM failed: ${error.message}`, { cause: error });
    }
  } else {
    const bytes = packAvm([
      { name: basename(beamFile), data: new Uint8Array(readFileSync(beamFile)), start: true },
    ]);
    parseAvm(bytes); // never write an archive AtomVM would reject
    writeFileSync(avmFile, bytes);
  }
  return { kind: packer.kind };
}
