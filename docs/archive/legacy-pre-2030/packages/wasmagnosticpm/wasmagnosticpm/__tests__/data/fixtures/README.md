# Test Fixtures

This directory contains test data files for integration testing the process_mining_wasm library.

## Files

### sample.xes

A standard XES (eXtensible Event Stream) file containing a simple process event log with:

- **3 traces** (cases): Case001, Case002, Case003
- **Events per trace**: 4-6 events
- **Activities**: Request, Review, Approve, Reject, Complete
- **Resources**: Alice, Bob, Charlie
- **Time range**: 2023-01-01 to 2023-01-02

The log represents a simple approval workflow process with a rejection path that triggers a re-submission.

**Use cases:**

- Testing XES loading and parsing
- Testing basic event statistics
- Testing process discovery algorithms
- Testing workflow analysis

### sample.json

An OCEL 2.0 (Object-Centric Event Log) JSON file representing an e-commerce process with:

- **Object types**: Order, Item, Package
- **10 events**: Create Order, Add Item, Pack Items, Ship Package, Deliver Package
- **Objects**: 2 orders, 3 items, 2 packages
- **Time range**: 2023-01-01 to 2023-01-02

The log represents a multi-object process where orders contain items that are packed into packages and shipped.

**Use cases:**

- Testing OCEL JSON loading and parsing
- Testing object-centric analysis
- Testing multi-object workflow discovery
- Testing object relationship tracking

## Format Specifications

### XES Standard

- **Version**: XES 1.0
- **Features**: nested-attributes
- **Extensions**:
  - Concept (activity names, trace names)
  - Organizational (resources, roles)
  - Time (timestamps)
  - Semantic
  - Cost
- **Classifiers**: Event Name (by concept:name), Resource (by org:resource)

### OCEL Format

- **Version**: OCEL 2.0
- **JSON representation** following the OCEL standard
- **Global attributes**: Concept name, timestamp
- **Object types**: Order, Item, Package
- **Event-object mapping**: Through ocel:omap elements

## Using These Fixtures

### In Tests

```javascript
const fs = require('fs');
const path = require('path');

const xesContent = fs.readFileSync(path.join(__dirname, 'sample.xes'), 'utf-8');

const ocelContent = fs.readFileSync(path.join(__dirname, 'sample.json'), 'utf-8');
```

### In Browser Tests

```javascript
// Using fetch
const xesContent = await fetch('__tests__/data/fixtures/sample.xes').then((r) => r.text());

const ocelContent = await fetch('__tests__/data/fixtures/sample.json').then((r) => r.json());
```

## Adding New Fixtures

When adding new test fixtures:

1. Choose an appropriate name (e.g., `sample-large.xes` for bigger logs)
2. Update this README with format and use case information
3. Ensure files are valid according to their respective standards
4. Add test cases that verify the new fixture loads correctly

## Validation

Both fixture files have been validated against their respective specifications:

- XES: Valid according to XES 1.0 standard
- OCEL: Valid according to OCEL 2.0 JSON schema

To re-validate:

- XES: Use an XES validator like the XES Standard validator
- OCEL: Validate against OCEL JSON schema
