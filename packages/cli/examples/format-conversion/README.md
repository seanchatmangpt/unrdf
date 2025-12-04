# Format Conversion Example

RDF format conversion utilities using @unrdf/cli.

## Features

- Convert between RDF formats (Turtle, N-Triples, N-Quads, TriG, JSON-LD)
- Auto-detect input format from file extension
- Validate RDF syntax
- Display format information
- Support multiple input/output formats

## Installation

```bash
pnpm install
```

## Usage

### Convert Formats

```bash
# Auto-detect input format
node src/index.mjs convert data/input.ttl --output data/output.nt

# Specify formats explicitly
node src/index.mjs convert data/input.ttl --output data/output.nt --from turtle --to ntriples

# Convert to JSON-LD
node src/index.mjs convert data/input.ttl --output data/output.jsonld --to jsonld
```

### Validate RDF

```bash
# Auto-detect format
node src/index.mjs validate data/example.ttl

# Specify format
node src/index.mjs validate data/example.ttl --format turtle
```

### Format Information

```bash
# Get format details
node src/index.mjs info turtle

# List all supported formats
node src/index.mjs formats
```

## Supported Formats

| Format | Extensions | Description |
|--------|-----------|-------------|
| turtle | .ttl, .turtle | Turtle syntax |
| ntriples | .nt, .ntriples | N-Triples syntax |
| nquads | .nq, .nquads | N-Quads syntax |
| trig | .trig | TriG syntax |
| jsonld | .jsonld, .json | JSON-LD syntax |

## API

### detectFormat(filepath)

Auto-detect RDF format from file extension.

### parseRDF(content, format)

Parse RDF content into quads.

### serializeRDF(quads, format)

Serialize quads to RDF format.

### convertFormat(content, inputFormat, outputFormat)

Convert RDF from one format to another.

### validateRDF(content, format)

Validate RDF syntax.

## Testing

```bash
pnpm test
```

## Examples

### Convert Turtle to N-Triples

```javascript
import { convertFormat } from './src/converter.mjs';

const turtle = '<http://ex.org/s> <http://ex.org/p> "o" .';
const ntriples = await convertFormat(turtle, 'turtle', 'ntriples');
console.log(ntriples);
```

### Validate RDF File

```javascript
import { readFileSync } from 'fs';
import { validateRDF } from './src/converter.mjs';

const content = readFileSync('data.ttl', 'utf-8');
const isValid = await validateRDF(content, 'turtle');
console.log(isValid ? 'Valid' : 'Invalid');
```
