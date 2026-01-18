# How to Add a New Entity to Your Generated API

**Goal:** Add a Comment entity to an existing blog API ontology and regenerate all outputs
**Time:** 5 minutes
**Difficulty:** Beginner

## Prerequisites

- Existing project with `ggen.toml` configuration
- Blog API ontology with User and Post entities
- `@unrdf/cli` installed

---

## Step 1: Open Your Ontology File

Open your ontology file (e.g., `ontology/blog.ttl`).

```bash
cat ontology/blog.ttl
```

**Expected result:** You see existing User and Post entity definitions.

---

## Step 2: Define the Comment Entity

Add the following Comment class definition after your existing entities:

```turtle
# Comment entity
ex:Comment a owl:Class ;
    rdfs:label "Comment" ;
    rdfs:comment "A comment on a blog post" .
```

---

## Step 3: Add Properties with Types and Descriptions

Add data properties for the Comment entity:

```turtle
ex:body a owl:DatatypeProperty ;
    rdfs:domain ex:Comment ;
    rdfs:range xsd:string ;
    rdfs:label "body" ;
    rdfs:comment "The comment text content" .

ex:commentedAt a owl:DatatypeProperty ;
    rdfs:domain ex:Comment ;
    rdfs:range xsd:dateTime ;
    rdfs:label "commentedAt" ;
    rdfs:comment "When the comment was created" .
```

Add object properties for relationships:

```turtle
ex:commentAuthor a owl:ObjectProperty ;
    rdfs:domain ex:Comment ;
    rdfs:range ex:User ;
    rdfs:label "commentAuthor" ;
    rdfs:comment "The user who wrote the comment" .

ex:commentOn a owl:ObjectProperty ;
    rdfs:domain ex:Comment ;
    rdfs:range ex:Post ;
    rdfs:label "commentOn" ;
    rdfs:comment "The post this comment belongs to" .
```

---

## Step 4: Define CRUD Endpoints (Optional)

Add API endpoint annotations if using endpoint generation:

```turtle
ex:Comment
    ex:endpoint "/comments" ;
    ex:operations "create,read,update,delete" ;
    ex:idField "id" .
```

---

## Step 5: Run `unrdf sync`

Generate all outputs from your updated ontology:

```bash
timeout 10s unrdf sync --config ggen.toml
```

**Expected result:**
```
Sync completed successfully
  Rules processed: 2
  Files generated: 2
  Errors: 0
```

---

## Step 6: Verify New Entity in Output

Check that Comment appears in generated files:

```bash
grep "COMMENT" lib/entities.mjs
```

**Expected result:**
```javascript
export const COMMENT = 'http://example.org/schema#Comment';
```

Verify properties were generated:

```bash
grep "body\|commentedAt\|commentAuthor" lib/properties.mjs
```

**Expected result:** All Comment properties listed.

---

## Troubleshooting

| Problem | Cause | Fix |
|---------|-------|-----|
| Sync reports 0 entities | Missing `owl:Class` type | Add `a owl:Class` to entity |
| Properties not in output | Wrong `rdfs:domain` | Verify domain points to your entity |
| Parse error on sync | Turtle syntax error | Check semicolons and periods |
| Relationships missing | Wrong property type | Use `owl:ObjectProperty` for relations |

### Quick Syntax Check

```bash
timeout 5s unrdf query --file ontology/blog.ttl \
  --query "SELECT (COUNT(*) as ?count) WHERE { ?s a owl:Class }"
```

**Expected result:** Count should increase by 1 after adding Comment.

---

## Related Guides

- [Build SPARQL Queries](./build-sparql-queries.md)
- [Generate RDF from Templates](./generate-rdf-from-templates.md)
- [Validate RDF with SHACL](./validate-rdf-with-shacl.md)
