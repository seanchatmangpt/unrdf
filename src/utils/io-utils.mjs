import { promises as fs } from 'node:fs';
import { createReadStream, createWriteStream } from 'node:fs';
import { pipeline as _pipeline } from 'node:stream/promises';
import { Transform as _Transform } from 'node:stream';

/**
 * Read Turtle file
 * @param {string} path - File path to read
 * @returns {Promise<string>} Turtle content
 */
export const readTurtleFile = async path => {
  try {
    return await fs.readFile(path, 'utf8');
  } catch (error) {
    throw new Error(`Failed to read Turtle file at ${path}: ${error.message}`);
  }
};

/**
 * Write Turtle file
 * @param {string} path - File path to write
 * @param {string} ttl - Turtle content
 * @returns {Promise<{path: string, bytes: number}>} Write result
 */
export const writeTurtleFile = async (path, ttl) => {
  try {
    await fs.writeFile(path, ttl, 'utf8');
    return { path, bytes: Buffer.byteLength(ttl) };
  } catch (error) {
    throw new Error(`Failed to write Turtle file at ${path}: ${error.message}`);
  }
};

/**
 * Read JSON-LD file
 * @param {string} path - File path to read
 * @returns {Promise<Object|Object[]>} JSON-LD content
 */
export const readJSONLDFile = async path => {
  try {
    const content = await fs.readFile(path, 'utf8');
    return JSON.parse(content);
  } catch (error) {
    throw new Error(`Failed to read JSON-LD file at ${path}: ${error.message}`);
  }
};

/**
 * Write JSON-LD file
 * @param {string} path - File path to write
 * @param {Object|Object[]} obj - JSON-LD content
 * @param {number} [indent=2] - JSON indentation
 * @returns {Promise<{path: string, bytes: number}>} Write result
 */
export const writeJSONLDFile = async (path, obj, indent = 2) => {
  try {
    const json = JSON.stringify(obj, null, indent);
    await fs.writeFile(path, json, 'utf8');
    return { path, bytes: Buffer.byteLength(json) };
  } catch (error) {
    throw new Error(`Failed to write JSON-LD file at ${path}: ${error.message}`);
  }
};

/**
 * Read N-Triples file
 * @param {string} path - File path to read
 * @returns {Promise<string>} N-Triples content
 */
export const readNTriplesFile = async path => {
  try {
    return await fs.readFile(path, 'utf8');
  } catch (error) {
    throw new Error(`Failed to read N-Triples file at ${path}: ${error.message}`);
  }
};

/**
 * Write N-Triples file
 * @param {string} path - File path to write
 * @param {string} ntriples - N-Triples content
 * @returns {Promise<{path: string, bytes: number}>} Write result
 */
export const writeNTriplesFile = async (path, ntriples) => {
  try {
    await fs.writeFile(path, ntriples, 'utf8');
    return { path, bytes: Buffer.byteLength(ntriples) };
  } catch (error) {
    throw new Error(`Failed to write N-Triples file at ${path}: ${error.message}`);
  }
};

/**
 * Read RDF/XML file
 * @param {string} path - File path to read
 * @returns {Promise<string>} RDF/XML content
 */
export const readRDFXMLFile = async path => {
  try {
    return await fs.readFile(path, 'utf8');
  } catch (error) {
    throw new Error(`Failed to read RDF/XML file at ${path}: ${error.message}`);
  }
};

/**
 * Write RDF/XML file
 * @param {string} path - File path to write
 * @param {string} rdfxml - RDF/XML content
 * @returns {Promise<{path: string, bytes: number}>} Write result
 */
export const writeRDFXMLFile = async (path, rdfxml) => {
  try {
    await fs.writeFile(path, rdfxml, 'utf8');
    return { path, bytes: Buffer.byteLength(rdfxml) };
  } catch (error) {
    throw new Error(`Failed to write RDF/XML file at ${path}: ${error.message}`);
  }
};

/**
 * Check if file exists
 * @param {string} path - File path to check
 * @returns {Promise<boolean>} True if file exists
 */
export const fileExists = async path => {
  try {
    await fs.access(path);
    return true;
  } catch {
    return false;
  }
};

/**
 * Get file stats
 * @param {string} path - File path
 * @returns {Promise<import('fs').Stats>} File stats
 */
export const getFileStats = async path => {
  try {
    return await fs.stat(path);
  } catch (error) {
    throw new Error(`Failed to get file stats for ${path}: ${error.message}`);
  }
};

/**
 * Create directory if it doesn't exist
 * @param {string} path - Directory path
 * @param {boolean} [recursive=true] - Create parent directories
 * @returns {Promise<void>}
 */
export const ensureDir = async (path, recursive = true) => {
  try {
    await fs.mkdir(path, { recursive });
  } catch (error) {
    if (error.code !== 'EEXIST') {
      throw new Error(`Failed to create directory ${path}: ${error.message}`);
    }
  }
};

/**
 * Read file as stream
 * @param {string} path - File path
 * @returns {import('fs').ReadStream} Read stream
 */
export const createFileReadStream = path => {
  try {
    return createReadStream(path, { encoding: 'utf8' });
  } catch (error) {
    throw new Error(`Failed to create read stream for ${path}: ${error.message}`);
  }
};

/**
 * Write file as stream
 * @param {string} path - File path
 * @returns {import('fs').WriteStream} Write stream
 */
export const createFileWriteStream = path => {
  try {
    return createWriteStream(path, { encoding: 'utf8' });
  } catch (error) {
    throw new Error(`Failed to create write stream for ${path}: ${error.message}`);
  }
};

/**
 * Stream file content line by line
 * @param {string} path - File path
 * @param {Function} onLine - Callback for each line
 * @returns {Promise<void>}
 */
export const streamFileLines = async (path, onLine) => {
  const readStream = createFileReadStream(path);
  let buffer = '';

  readStream.on('data', chunk => {
    buffer += chunk;
    const lines = buffer.split('\n');
    buffer = lines.pop() || ''; // Keep incomplete line in buffer

    for (const line of lines) {
      onLine(line);
    }
  });

  readStream.on('end', () => {
    if (buffer) {
      onLine(buffer);
    }
  });

  return new Promise((resolve, reject) => {
    readStream.on('error', reject);
    readStream.on('end', resolve);
  });
};

/**
 * Copy file
 * @param {string} src - Source file path
 * @param {string} dest - Destination file path
 * @returns {Promise<void>}
 */
export const copyFile = async (src, dest) => {
  try {
    await fs.copyFile(src, dest);
  } catch (error) {
    throw new Error(`Failed to copy file from ${src} to ${dest}: ${error.message}`);
  }
};

/**
 * Move/rename file
 * @param {string} src - Source file path
 * @param {string} dest - Destination file path
 * @returns {Promise<void>}
 */
export const moveFile = async (src, dest) => {
  try {
    await fs.rename(src, dest);
  } catch (error) {
    throw new Error(`Failed to move file from ${src} to ${dest}: ${error.message}`);
  }
};

/**
 * Delete file
 * @param {string} path - File path to delete
 * @returns {Promise<void>}
 */
export const deleteFile = async path => {
  try {
    await fs.unlink(path);
  } catch (error) {
    throw new Error(`Failed to delete file ${path}: ${error.message}`);
  }
};

/**
 * List files in directory
 * @param {string} path - Directory path
 * @param {string} [pattern] - Optional glob pattern
 * @returns {Promise<string[]>} Array of file paths
 */
export const listFiles = async (path, pattern) => {
  try {
    const files = await fs.readdir(path);
    if (pattern) {
      const regex = new RegExp(pattern.replace(/\*/g, '.*'));
      return files.filter(file => regex.test(file));
    }
    return files;
  } catch (error) {
    throw new Error(`Failed to list files in ${path}: ${error.message}`);
  }
};

/**
 * Get file extension
 * @param {string} path - File path
 * @returns {string} File extension (without dot)
 */
export const getFileExtension = path => {
  const parts = path.split('.');
  return parts.length > 1 ? parts.pop().toLowerCase() : '';
};

/**
 * Detect RDF format from file extension
 * @param {string} path - File path
 * @returns {string} RDF format name
 */
export const detectRDFFormat = path => {
  const ext = getFileExtension(path);
  const formatMap = {
    ttl: 'turtle',
    turtle: 'turtle',
    nt: 'n-triples',
    n3: 'n3',
    rdf: 'rdf-xml',
    xml: 'rdf-xml',
    jsonld: 'json-ld',
    json: 'json-ld',
  };
  return formatMap[ext] || 'turtle';
};
