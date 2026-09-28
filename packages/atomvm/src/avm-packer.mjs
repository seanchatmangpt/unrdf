/**
 * @file AVM (AtomVM application archive) packer and validator.
 *
 * Pure JavaScript, browser-safe, dependency-free, so builds and validation do
 * not need the PackBEAM binary. Format (AtomVM "packbeam"):
 *
 *   header  : "#!/usr/bin/env AtomVM\n" + 2 NUL bytes (24 bytes)
 *   entry*  : size:u32be flags:u32be reserved:u32be name\0 (pad4) data (pad4)
 *   trailer : a full ZERO ENTRY - 12 zero bytes (size, flags, reserved)
 *
 * flags (AtomVM avmpack.h): bit0 = BEAM_START_FLAG (the start module),
 * bit1 = BEAM_CODE_FLAG (a BEAM code module). Plain files have flags 0.
 *
 * The trailer must be all 12 bytes: AtomVM's lookup loops read the flags word
 * of the *next* entry, and a 4-byte trailer makes a lookup miss (e.g. a program
 * that spawns without erlang.beam in the pack) read garbage past the buffer and
 * spin forever instead of failing with `undef`.
 */

export const AVM_HEADER = Uint8Array.from([
  ...new TextEncoder().encode('#!/usr/bin/env AtomVM\n'),
  0,
  0,
]);
export const AVM_FLAG_START = 1;
export const AVM_FLAG_CODE = 2;

const HEADER_BYTES = AVM_HEADER.length;
const ENTRY_HEADER_BYTES = 12;
const TRAILER_BYTES = 12;

const pad4 = length => (4 - (length % 4)) % 4;

export class AvmFormatError extends Error {
  constructor(message) {
    super(`[AVM_FORMAT_REFUSED] ${message}`);
    this.name = 'AvmFormatError';
    this.code = 'AVM_FORMAT_REFUSED';
  }
}

function isBeam(bytes) {
  return (
    bytes.length >= 4 &&
    bytes[0] === 0x46 &&
    bytes[1] === 0x4f &&
    bytes[2] === 0x52 &&
    bytes[3] === 0x31
  ); // FOR1
}

/**
 * @param {{name: string, data: Uint8Array, start?: boolean, file?: boolean}[]} modules - BEAM modules
 *   (`file: true` packs a plain data file such as priv/ content instead)
 * @returns {Uint8Array} .avm archive bytes
 */
export function packAvm(modules) {
  if (!Array.isArray(modules) || modules.length === 0) {
    throw new AvmFormatError('at least one BEAM module is required');
  }
  if (modules.filter(entry => entry.start).length > 1) {
    throw new AvmFormatError('at most one start module is allowed');
  }
  const encoder = new TextEncoder();
  const parts = [AVM_HEADER];
  for (const entry of modules) {
    if (!entry.name || !/^[A-Za-z0-9_./-]+$/.test(entry.name)) {
      throw new AvmFormatError(`invalid entry name: ${entry.name}`);
    }
    const data = entry.data;
    if (!(data instanceof Uint8Array)) throw new AvmFormatError(`${entry.name} data must be a Uint8Array`);
    if (!entry.file && !isBeam(data)) {
      throw new AvmFormatError(`${entry.name} is not a BEAM file (missing FOR1 header)`);
    }
    if (entry.file && entry.start) throw new AvmFormatError(`${entry.name}: a plain file cannot be the start module`);
    const name = encoder.encode(`${entry.name}\0`);
    const namePad = pad4(name.length);
    const unpadded = ENTRY_HEADER_BYTES + name.length + namePad + data.length;
    const dataPad = pad4(unpadded);
    const header = new DataView(new ArrayBuffer(ENTRY_HEADER_BYTES));
    header.setUint32(0, unpadded + dataPad);
    header.setUint32(4, entry.file ? 0 : AVM_FLAG_CODE | (entry.start ? AVM_FLAG_START : 0));
    parts.push(
      new Uint8Array(header.buffer),
      name,
      new Uint8Array(namePad),
      data,
      new Uint8Array(dataPad)
    );
  }
  parts.push(new Uint8Array(TRAILER_BYTES));
  const total = parts.reduce((sum, part) => sum + part.length, 0);
  const out = new Uint8Array(total);
  let offset = 0;
  for (const part of parts) {
    out.set(part, offset);
    offset += part.length;
  }
  return out;
}

/**
 * Parse and structurally validate an .avm archive. Throws AvmFormatError on
 * anything the real AtomVM would reject (wrong header, truncated entries,
 * missing trailer, no start module, non-BEAM payload).
 *
 * @param {Uint8Array} bytes
 * @returns {{entries: {name: string, flags: number, size: number}[], startModule: string}}
 */
export function parseAvm(bytes) {
  if (!(bytes instanceof Uint8Array)) throw new AvmFormatError('input must be a Uint8Array');
  if (bytes.length < HEADER_BYTES + TRAILER_BYTES)
    throw new AvmFormatError(`too small (${bytes.length} bytes)`);
  for (let i = 0; i < HEADER_BYTES; i++) {
    if (bytes[i] !== AVM_HEADER[i]) {
      const preview = new TextDecoder().decode(bytes.subarray(0, 24)).replace(/\s+/g, ' ');
      throw new AvmFormatError(`bad header (starts with "${preview}")`);
    }
  }
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const decoder = new TextDecoder();
  const entries = [];
  let offset = HEADER_BYTES;
  for (;;) {
    if (offset + TRAILER_BYTES > bytes.length) {
      throw new AvmFormatError('missing or truncated trailer (needs 12 zero bytes)');
    }
    const size = view.getUint32(offset);
    if (size === 0) {
      if (view.getUint32(offset + 4) !== 0 || view.getUint32(offset + 8) !== 0) {
        throw new AvmFormatError('malformed trailer (flags/reserved must be zero)');
      }
      break;
    }
    if (size < ENTRY_HEADER_BYTES + 4 || offset + size > bytes.length || size % 4 !== 0) {
      throw new AvmFormatError(`entry at offset ${offset} has invalid size ${size}`);
    }
    const flags = view.getUint32(offset + 4);
    const nameStart = offset + ENTRY_HEADER_BYTES;
    const nameEnd = bytes.indexOf(0, nameStart);
    if (nameEnd === -1 || nameEnd >= offset + size)
      throw new AvmFormatError('unterminated entry name');
    const name = decoder.decode(bytes.subarray(nameStart, nameEnd));
    const dataStart = nameStart + (nameEnd + 1 - nameStart) + pad4(nameEnd + 1 - nameStart);
    if (flags & AVM_FLAG_CODE && !isBeam(bytes.subarray(dataStart, offset + size))) {
      throw new AvmFormatError(`${name} is flagged as code but has no FOR1 header`);
    }
    entries.push({ name, flags, size });
    offset += size;
  }
  const start = entries.filter(entry => entry.flags & AVM_FLAG_START);
  if (start.length !== 1) {
    throw new AvmFormatError(`expected exactly one start module, found ${start.length}`);
  }
  return { entries, startModule: start[0].name };
}
