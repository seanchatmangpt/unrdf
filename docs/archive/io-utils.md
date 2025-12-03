# I/O Utils

File operations and streaming utilities for RDF formats.

## Overview

The `io-utils` module provides file-level helpers for reading and writing RDF formats (Turtle, JSON-LD, N-Triples), streaming operations, and file system utilities.

## Functions

### `readTurtleFile(path)`

Reads a Turtle file.

```javascript
import { readTurtleFile } from 'unrdf/utils';

const content = await readTurtleFile('/path/to/file.ttl');
// Returns: string
```

**Parameters:**
- `path` (string) - File path to read

**Returns:** Promise<string>

### `writeTurtleFile(path, ttl)`

Writes a Turtle file.

```javascript
import { writeTurtleFile } from 'unrdf/utils';

const result = await writeTurtleFile('/path/to/file.ttl', '@prefix ex: <http://example.org/> . ex:test a ex:Thing .');
// Returns: { path: string, bytes: number }
```

**Parameters:**
- `path` (string) - File path to write
- `ttl` (string) - Turtle content

**Returns:** Promise<{path: string, bytes: number}>

### `readJSONLDFile(path)`

Reads a JSON-LD file.

```javascript
import { readJSONLDFile } from 'unrdf/utils';

const content = await readJSONLDFile('/path/to/file.jsonld');
// Returns: Object
```

**Parameters:**
- `path` (string) - File path to read

**Returns:** Promise<Object>

### `writeJSONLDFile(path, obj)`

Writes a JSON-LD file.

```javascript
import { writeJSONLDFile } from 'unrdf/utils';

const jsonld = {
  '@context': { 'ex': 'http://example.org/' },
  '@id': 'ex:test',
  'ex:name': 'Test'
};

const result = await writeJSONLDFile('/path/to/file.jsonld', jsonld);
// Returns: { path: string, bytes: number }
```

**Parameters:**
- `path` (string) - File path to write
- `obj` (Object) - JSON-LD object

**Returns:** Promise<{path: string, bytes: number}>

### `readNTriplesFile(path)`

Reads an N-Triples file.

```javascript
import { readNTriplesFile } from 'unrdf/utils';

const content = await readNTriplesFile('/path/to/file.nt');
// Returns: string
```

**Parameters:**
- `path` (string) - File path to read

**Returns:** Promise<string>

### `writeNTriplesFile(path, ntriples)`

Writes an N-Triples file.

```javascript
import { writeNTriplesFile } from 'unrdf/utils';

const ntriples = '<http://example.org/s> <http://example.org/p> <http://example.org/o> .';
const result = await writeNTriplesFile('/path/to/file.nt', ntriples);
// Returns: { path: string, bytes: number }
```

**Parameters:**
- `path` (string) - File path to write
- `ntriples` (string) - N-Triples content

**Returns:** Promise<{path: string, bytes: number}>

### `fileExists(path)`

Checks if a file exists.

```javascript
import { fileExists } from 'unrdf/utils';

const exists = await fileExists('/path/to/file.ttl');
// Returns: boolean
```

**Parameters:**
- `path` (string) - File path to check

**Returns:** Promise<boolean>

### `getFileStats(path)`

Gets file statistics.

```javascript
import { getFileStats } from 'unrdf/utils';

const stats = await getFileStats('/path/to/file.ttl');
// Returns: { size: number, mtime: Date, ... } or null if file doesn't exist
```

**Parameters:**
- `path` (string) - File path

**Returns:** Promise<Object|null>

### `ensureDir(path)`

Ensures a directory exists.

```javascript
import { ensureDir } from 'unrdf/utils';

await ensureDir('/path/to/directory');
```

**Parameters:**
- `path` (string) - Directory path

**Returns:** Promise<void>

### `createFileReadStream(path)`

Creates a readable stream for a file.

```javascript
import { createFileReadStream } from 'unrdf/utils';

const stream = createFileReadStream('/path/to/large-file.ttl');
// Returns: ReadableStream
```

**Parameters:**
- `path` (string) - File path

**Returns:** ReadableStream

### `createFileWriteStream(path)`

Creates a writable stream for a file.

```javascript
import { createFileWriteStream } from 'unrdf/utils';

const stream = createFileWriteStream('/path/to/output.ttl');
// Returns: WritableStream
```

**Parameters:**
- `path` (string) - File path

**Returns:** WritableStream

### `streamFileLines(path)`

Streams file lines.

```javascript
import { streamFileLines } from 'unrdf/utils';

for await (const line of streamFileLines('/path/to/file.ttl')) {
  console.log(line);
}
```

**Parameters:**
- `path` (string) - File path

**Returns:** AsyncIterable<string>

### `copyFile(src, dest)`

Copies a file.

```javascript
import { copyFile } from 'unrdf/utils';

await copyFile('/path/to/source.ttl', '/path/to/destination.ttl');
```

**Parameters:**
- `src` (string) - Source file path
- `dest` (string) - Destination file path

**Returns:** Promise<void>

### `moveFile(src, dest)`

Moves a file.

```javascript
import { moveFile } from 'unrdf/utils';

await moveFile('/path/to/source.ttl', '/path/to/destination.ttl');
```

**Parameters:**
- `src` (string) - Source file path
- `dest` (string) - Destination file path

**Returns:** Promise<void>

### `deleteFile(path)`

Deletes a file.

```javascript
import { deleteFile } from 'unrdf/utils';

await deleteFile('/path/to/file.ttl');
```

**Parameters:**
- `path` (string) - File path

**Returns:** Promise<void>

### `listFiles(dir, options?)`

Lists files in a directory.

```javascript
import { listFiles } from 'unrdf/utils';

const files = await listFiles('/path/to/directory');
// Returns: string[] - array of file paths

// With options
const ttlFiles = await listFiles('/path/to/directory', { 
  extension: '.ttl',
  recursive: true 
});
```

**Parameters:**
- `dir` (string) - Directory path
- `options` (Object, optional) - Options object
  - `extension` (string) - Filter by file extension
  - `recursive` (boolean) - Include subdirectories

**Returns:** Promise<string[]>

### `getFileExtension(path)`

Gets file extension.

```javascript
import { getFileExtension } from 'unrdf/utils';

const ext = getFileExtension('/path/to/file.ttl');
// Returns: '.ttl'
```

**Parameters:**
- `path` (string) - File path

**Returns:** string

### `detectRDFFormat(path)`

Detects RDF format from file extension.

```javascript
import { detectRDFFormat } from 'unrdf/utils';

const format = detectRDFFormat('/path/to/file.ttl');
// Returns: 'turtle' | 'jsonld' | 'ntriples' | 'unknown'
```

**Parameters:**
- `path` (string) - File path

**Returns:** string

## Supported Formats

### Turtle (.ttl)
- **Read**: `readTurtleFile()`
- **Write**: `writeTurtleFile()`
- **Extension**: `.ttl`, `.turtle`

### JSON-LD (.jsonld)
- **Read**: `readJSONLDFile()`
- **Write**: `writeJSONLDFile()`
- **Extension**: `.jsonld`, `.json`

### N-Triples (.nt)
- **Read**: `readNTriplesFile()`
- **Write**: `writeNTriplesFile()`
- **Extension**: `.nt`, `.ntriples`

## Examples

### Basic File Operations

```javascript
import { 
  readTurtleFile, writeTurtleFile, 
  readJSONLDFile, writeJSONLDFile,
  fileExists, getFileStats 
} from 'unrdf/utils';

// Read Turtle file
const turtleContent = await readTurtleFile('data.ttl');
console.log(turtleContent);

// Write Turtle file
const result = await writeTurtleFile('output.ttl', turtleContent);
console.log(`Wrote ${result.bytes} bytes to ${result.path}`);

// Read JSON-LD file
const jsonldContent = await readJSONLDFile('data.jsonld');
console.log(jsonldContent);

// Check if file exists
if (await fileExists('data.ttl')) {
  const stats = await getFileStats('data.ttl');
  console.log(`File size: ${stats.size} bytes`);
}
```

### Streaming Operations

```javascript
import { 
  createFileReadStream, createFileWriteStream, 
  streamFileLines 
} from 'unrdf/utils';

// Stream large file
const readStream = createFileReadStream('large-data.ttl');
const writeStream = createFileWriteStream('processed-data.ttl');

// Process line by line
for await (const line of streamFileLines('large-data.ttl')) {
  if (line.includes('http://example.org/')) {
    // Process line
    console.log(line);
  }
}
```

### File Management

```javascript
import { 
  listFiles, copyFile, moveFile, deleteFile,
  getFileExtension, detectRDFFormat 
} from 'unrdf/utils';

// List all Turtle files
const ttlFiles = await listFiles('./data', { 
  extension: '.ttl',
  recursive: true 
});

// Process each file
for (const file of ttlFiles) {
  const format = detectRDFFormat(file);
  const ext = getFileExtension(file);
  
  console.log(`Processing ${file} (${format}, ${ext})`);
  
  // Copy to backup
  await copyFile(file, `backup/${file}`);
}
```

### Batch Processing

```javascript
import { 
  listFiles, readTurtleFile, writeTurtleFile,
  ensureDir 
} from 'unrdf/utils';

async function processRDFFiles(inputDir, outputDir) {
  // Ensure output directory exists
  await ensureDir(outputDir);
  
  // Get all RDF files
  const files = await listFiles(inputDir, { 
    extension: '.ttl',
    recursive: true 
  });
  
  // Process each file
  for (const file of files) {
    try {
      // Read file
      const content = await readTurtleFile(file);
      
      // Process content (example: add prefix)
      const processedContent = `@prefix ex: <http://example.org/> .\n${content}`;
      
      // Write to output directory
      const outputPath = `${outputDir}/${file}`;
      await writeTurtleFile(outputPath, processedContent);
      
      console.log(`Processed ${file}`);
    } catch (error) {
      console.error(`Error processing ${file}:`, error.message);
    }
  }
}

// Run batch processing
await processRDFFiles('./input', './output');
```

### Error Handling

```javascript
import { readTurtleFile, fileExists } from 'unrdf/utils';

async function safeReadFile(path) {
  try {
    // Check if file exists first
    if (!(await fileExists(path))) {
      throw new Error(`File not found: ${path}`);
    }
    
    // Read file
    const content = await readTurtleFile(path);
    return content;
  } catch (error) {
    if (error.code === 'ENOENT') {
      console.error(`File not found: ${path}`);
    } else if (error.code === 'EACCES') {
      console.error(`Permission denied: ${path}`);
    } else {
      console.error(`Error reading file ${path}:`, error.message);
    }
    return null;
  }
}

const content = await safeReadFile('data.ttl');
if (content) {
  console.log('File read successfully');
}
```

## Error Handling

- **File Not Found**: Functions throw errors for non-existent files
- **Permission Errors**: Clear error messages for access issues
- **Format Errors**: Validation errors for malformed content
- **Stream Errors**: Proper error handling for streaming operations

## Performance Notes

- **Streaming**: Large files are handled efficiently with streams
- **Memory Usage**: Minimal memory footprint for file operations
- **Concurrent Operations**: Supports concurrent file operations
- **Error Recovery**: Graceful handling of file system errors

## Related Modules

- [Transform Utils](./transform-utils.md) - Data transformations
- [Validation Utils](./validation-utils.md) - Format validation
- [Debug Utils](./debug-utils.md) - File operation debugging
