# Reference: SHACL API

All functions and classes are exported from `@unrdf/core`. The SHACL implementation wraps the
`rdf-validate-shacl` library.

---

## `shacl(iri?)`

Factory function. Returns a new `ShapeBuilder`.

```typescript
function shacl(iri?: string): ShapeBuilder;
```

| Param | Type     | Description                                                             |
| ----- | -------- | ----------------------------------------------------------------------- |
| `iri` | `string` | IRI for the shape node (optional; auto-generated blank node if omitted) |

---

## `ShapeBuilder`

Fluent builder for SHACL `NodeShape` definitions. All methods return `this` except `build()`
and `toTurtle()`.

### `.targetClass(classIri)`

Declare the class that this shape targets. All instances of the class in the data graph will
be validated against this shape.

```typescript
.targetClass(classIri: string): ShapeBuilder
```

---

### `.property(propertyPath)`

Open a `PropertyBuilder` for the given property path IRI. Subsequent property constraint
methods apply to this property until the next `.property()` or `.build()` call.

```typescript
.property(propertyPath: string): PropertyBuilder
```

The returned `PropertyBuilder` delegates back to `ShapeBuilder` for the fluent chain, so
you can call `.build()` or `.property()` again from the `PropertyBuilder` instance.

---

### `.build()`

Materialise the shape as an array of RDF quads. The quads encode the SHACL constraints in
the W3C SHACL vocabulary.

```typescript
.build(): Quad[]
```

**Returns** `Quad[]` — suitable for passing to `createValidator` or adding to a store.

---

### `.toTurtle()`

Serialise the shape as a Turtle string, for inspection or saving to a file.

```typescript
.toTurtle(): string
```

---

## `PropertyBuilder`

Chain off `.property(path)` on a `ShapeBuilder`. All methods return `this` (the
`PropertyBuilder`) except when explicitly noted.

### `.minCount(n)`

Minimum cardinality. `n` must be a non-negative integer.

**Throws** `ZodError` if `n < 0` or not an integer.

### `.maxCount(n)`

Maximum cardinality.

**Throws** `ZodError` if `n < 0` or not an integer.

### `.datatype(datatypeIRI)`

Constrain values to literals of the given datatype. Short-form prefixes (`'xsd:string'`,
`'xsd:integer'`, etc.) are expanded automatically.

**Throws** `ZodError` if `datatypeIRI` is an empty string.

### `.nodeKind(kind)`

Constrain the kind of RDF node.

| `kind` value           | Allows                     |
| ---------------------- | -------------------------- |
| `'IRI'`                | Named nodes only           |
| `'BlankNode'`          | Blank nodes only           |
| `'Literal'`            | Literals only              |
| `'BlankNodeOrIRI'`     | Named nodes or blank nodes |
| `'BlankNodeOrLiteral'` | Blank nodes or literals    |
| `'IRIOrLiteral'`       | Named nodes or literals    |

**Throws** `Error: Invalid node kind: …` for unrecognised values.

### `.pattern(regex, flags?)`

Constrain string values to match a regular expression.

```javascript
.pattern('^[a-z]+@[a-z]+\\.[a-z]+$')
.pattern('^[A-Z]', 'i')
```

**Throws** `ZodError` if `regex` is an empty string.

### `.minInclusive(n)` / `.maxInclusive(n)` / `.minExclusive(n)` / `.maxExclusive(n)`

Numeric range constraints.

### `.uniqueLang()`

Require that language-tagged literals for this property have unique language tags.

### `.property(path)` (on PropertyBuilder)

Close the current property and open a new one on the parent `ShapeBuilder`.

```javascript
shacl()
  .property('foaf:name')
  .minCount(1)
  .datatype('xsd:string')
  .property('foaf:age')
  .datatype('xsd:integer')
  .minInclusive(0)
  .build();
```

### `.build()` / `.toTurtle()` (on PropertyBuilder)

Delegates to the parent `ShapeBuilder`.

---

## `CommonShapes`

Factory helpers for common property constraint patterns.

### `CommonShapes.iri(propertyPath, options?)`

Constrain a property to IRI values.

```typescript
CommonShapes.iri(propertyPath: string, options?: { minCount?: number }): PropertyBuilder
```

### `CommonShapes.literal(propertyPath, datatypeIRI, options?)`

Constrain a property to typed literals.

```typescript
CommonShapes.literal(
  propertyPath: string,
  datatypeIRI: string,
  options?: { minCount?: number }
): PropertyBuilder
```

### `CommonShapes.string(propertyPath, options?)`

Shorthand for `xsd:string` literals.

### `CommonShapes.integer(propertyPath, options?)`

Shorthand for `xsd:integer` literals, with optional `minInclusive` / `maxInclusive`.

```typescript
CommonShapes.integer(propertyPath, {
  minCount?: number;
  minInclusive?: number;
  maxInclusive?: number;
})
```

---

## `createValidator(shapes)`

Create a SHACL validator from shapes.

```typescript
async function createValidator(shapes: string | Quad[] | OxigraphStore): Promise<SHACLValidator>;
```

| Argument | Type            | Description                           |
| -------- | --------------- | ------------------------------------- |
| `shapes` | `string`        | Turtle-serialized SHACL shapes        |
| `shapes` | `Quad[]`        | Array of quads (e.g. from `.build()`) |
| `shapes` | `OxigraphStore` | Store containing shape quads          |

**Returns** A `SHACLValidator` instance with a `.validate(data)` method.

**Throws** `Error: Shapes must be a Store, Turtle string, or Quad array` for unsupported types.

---

## `validateGraph(data, shapes, options?)`

Validate a data graph against SHACL shapes. Creates a validator internally.

```typescript
async function validateGraph(
  data: Quad[] | OxigraphStore,
  shapes: string | Quad[] | OxigraphStore,
  options?: ValidationOptions
): Promise<ValidationReport>;
```

**ValidationOptions**

| Field       | Type      | Default | Description                     |
| ----------- | --------- | ------- | ------------------------------- |
| `details`   | `boolean` | `true`  | Include detailed violation info |
| `maxErrors` | `number`  | none    | Stop after this many violations |

**ValidationReport**

```typescript
{
  conforms: boolean;
  results: ValidationResult[];
  details?: any;
}
```

**ValidationResult**

```typescript
{
  path?: string;           // property path that violated the constraint
  message: string;         // human-readable description
  focusNode?: string;      // IRI of the node that failed
  severity?: string;       // violation severity IRI
  sourceConstraintComponent?: string;
  value?: string;          // the offending value
}
```

---

## `validateConstraint(values, constraintType, constraintValue)`

Low-level single-constraint check. Returns `true` or `false`.

```typescript
function validateConstraint(
  values: unknown | unknown[],
  constraintType: string, // ConstraintType enum value
  constraintValue: unknown
): boolean;
```

---

## `generateReport(violations)`

Construct a `ValidationReport` from an array of violation objects.

```typescript
function generateReport(violations: Partial<ValidationResult>[]): ValidationReport;
```

A zero-length `violations` array produces `{ conforms: true, results: [] }`.

---

## `validateNodeKind(node, nodeKindIRI)`

Check whether an RDF term satisfies a SHACL `sh:nodeKind` constraint.

```typescript
function validateNodeKind(node: Term, nodeKindIRI: string): boolean;
```

`nodeKindIRI` is a full IRI string (e.g. `'http://www.w3.org/ns/shacl#IRI'`).

---

## `fastValidate(store, constraints)`

Quick validation without building full SHACL shapes. Useful for simple structure checks.

```typescript
function fastValidate(
  store: OxigraphStore,
  constraints: {
    targetClass?: string;
    properties?: Record<string, { minCount?: number; datatype?: string }>;
  }
): ValidationReport;
```

---

## `ConstraintType` (enum)

String constants for SHACL constraint component IRIs.

| Key             | Value                                     |
| --------------- | ----------------------------------------- |
| `MIN_COUNT`     | `http://www.w3.org/ns/shacl#minCount`     |
| `MAX_COUNT`     | `http://www.w3.org/ns/shacl#maxCount`     |
| `DATATYPE`      | `http://www.w3.org/ns/shacl#datatype`     |
| `NODE_KIND`     | `http://www.w3.org/ns/shacl#nodeKind`     |
| `PATTERN`       | `http://www.w3.org/ns/shacl#pattern`      |
| `MIN_INCLUSIVE` | `http://www.w3.org/ns/shacl#minInclusive` |
| `MAX_INCLUSIVE` | `http://www.w3.org/ns/shacl#maxInclusive` |
| `MIN_EXCLUSIVE` | `http://www.w3.org/ns/shacl#minExclusive` |
| `MAX_EXCLUSIVE` | `http://www.w3.org/ns/shacl#maxExclusive` |
| `UNIQUE_LANG`   | `http://www.w3.org/ns/shacl#uniqueLang`   |

---

## Zod schemas

| Export                    | Validates                         |
| ------------------------- | --------------------------------- |
| `ValidationOptionsSchema` | `validateGraph` options object    |
| `ValidationReportSchema`  | The shape of a `ValidationReport` |
