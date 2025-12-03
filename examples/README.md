# UNRDF Examples

**Start with #01. Add complexity only if needed.**

## Beginner Examples (Start Here)

| #   | Example                                                    | Time  | What You'll Learn                              |
| --- | ---------------------------------------------------------- | ----- | ---------------------------------------------- |
| 01  | [01-minimal-parse-query.mjs](./01-minimal-parse-query.mjs) | 3 min | Parse RDF, execute SPARQL (the pit of success) |

## Intermediate Examples (Only If Needed)

| #   | Example                                                | Time   | When You Need It                     |
| --- | ------------------------------------------------------ | ------ | ------------------------------------ |
| 10  | [basic-knowledge-hook.mjs](./basic-knowledge-hook.mjs) | 15 min | Autonomous behaviors on data changes |
| 11  | [context-example.mjs](./context-example.mjs)           | 10 min | Understanding the context system     |

## Advanced Examples (Skip Unless Required)

| #   | Example                                          | Time   | When You Need It                  |
| --- | ------------------------------------------------ | ------ | --------------------------------- |
| 20  | [dark-matter-80-20.mjs](./dark-matter-80-20.mjs) | 30 min | Performance optimization at scale |
| 21  | [lockchain-demo.mjs](./lockchain-demo.mjs)       | 20 min | Compliance audit trails           |
| 22  | [policy-pack-demo.mjs](./policy-pack-demo.mjs)   | 25 min | Declarative governance            |

## Full Feature Showcase (Reference Only)

| Example                                                        | Lines | Purpose                        |
| -------------------------------------------------------------- | ----- | ------------------------------ |
| [knowledge-engine-example.mjs](./knowledge-engine-example.mjs) | 363   | Complete feature demonstration |
| [sparql-query-advanced.mjs](./sparql-query-advanced.mjs)       | 453   | Complex query patterns         |

---

## Quick Start

```bash
# Run the minimal example
node examples/01-minimal-parse-query.mjs

# Output:
# Query results:
# [
#   { person: 'http://example.org/Alice', friend: 'http://example.org/Bob' },
#   { person: 'http://example.org/Bob', friend: 'http://example.org/Charlie' },
#   { person: 'http://example.org/Charlie', friend: 'http://example.org/Diana' }
# ]
```

---

## Which Example Should I Run?

| Your Goal                | Start With                              |
| ------------------------ | --------------------------------------- |
| Just parse and query RDF | 01-minimal-parse-query.mjs              |
| Add validation           | (use SHACL directly, see how-to guides) |
| Add hooks                | 10-basic-knowledge-hook.mjs             |
| Optimize performance     | 20-dark-matter-80-20.mjs                |
| Add audit trails         | 21-lockchain-demo.mjs                   |

**When in doubt, start with 01.**
