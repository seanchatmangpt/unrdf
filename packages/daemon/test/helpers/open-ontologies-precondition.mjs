/**
 * @file Precondition for tests that spawn the `open-ontologies` Rust binary.
 *
 * These tests are NOT skipped. If the binary is missing they fail in beforeAll with an
 * explicit precondition error rather than an opaque `spawn ... ENOENT` deep in a handler.
 *
 * Provide the binary via:
 *   OPEN_ONTOLOGIES_PATH=/abs/path/to/open-ontologies   (preferred)
 * or build it at <repo>/open-ontologies/target/debug/open-ontologies:
 *   git clone https://github.com/fabio-rovai/open-ontologies open-ontologies
 *   (cd open-ontologies && cargo build)
 */
import fs from 'node:fs';
import path from 'node:path';

const REL = 'open-ontologies/target/debug/open-ontologies';

/** @returns {string[]} candidate binary locations, in lookup order */
export function openOntologiesCandidates() {
  const cwd = process.cwd();
  return [
    process.env.OPEN_ONTOLOGIES_PATH,
    path.resolve(cwd, REL),
    path.resolve(cwd, '..', REL),
    path.resolve(cwd, '..', '..', REL),
  ].filter(Boolean);
}

/**
 * Resolve the open-ontologies binary or throw a precondition error.
 * On success also exports OPEN_ONTOLOGIES_PATH so every code path (sidecar, semantic bridge,
 * handlers) uses the same binary.
 * @returns {string} absolute path to the binary
 */
export function requireOpenOntologies() {
  const candidates = openOntologiesCandidates();
  const found = candidates.find(p => fs.existsSync(p));
  if (!found) {
    throw new Error(
      'PRECONDITION FAILED: the `open-ontologies` binary is required by this test but was not found.\n' +
        `Looked in:\n  ${candidates.join('\n  ')}\n` +
        'Set OPEN_ONTOLOGIES_PATH to the built binary, or build it: ' +
        'clone https://github.com/fabio-rovai/open-ontologies into <repo>/open-ontologies and run `cargo build`.'
    );
  }
  process.env.OPEN_ONTOLOGIES_PATH = found;
  return found;
}
