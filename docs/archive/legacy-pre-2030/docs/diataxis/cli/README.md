# @unrdf/cli Documentation

Command-line interface for RDF knowledge graph operations, SPARQL queries, hook execution, and ontology-driven code generation.

## Binary

```
unrdf <command-group> <subcommand> [flags]
```

Install globally with `pnpm add -g @unrdf/cli` or prefix with `pnpm exec`.

## Documentation Map

This documentation follows the [Diataxis](https://diataxis.fr/) framework. Choose the section that matches your current need.

### Tutorials — Learning by doing

Step-by-step guides for newcomers. Run the commands and see results.

- [tutorials/README.md](tutorials/README.md) — Index
- [tutorials/01-query-an-rdf-graph.md](tutorials/01-query-an-rdf-graph.md) — Load a Turtle file, run SPARQL, export results

### How-to Guides — Solving specific problems

Goal-oriented recipes for practitioners who already know the basics.

- [how-to/README.md](how-to/README.md) — Index
- [how-to/01-execute-hooks-from-cli.md](how-to/01-execute-hooks-from-cli.md) — Full hook workflow: template → define → execute
- [how-to/02-validate-rdf-with-hooks.md](how-to/02-validate-rdf-with-hooks.md) — Test a SPARQL ASK condition against a store file

### Reference — Technical specification

Accurate, complete descriptions of commands, flags, and schemas.

- [reference/README.md](reference/README.md) — Index
- [reference/commands.md](reference/commands.md) — All command groups and their flags
- [reference/hook-config-schema.md](reference/hook-config-schema.md) — KnowledgeHookSchema JSON field reference

### Explanation — Understanding concepts

Background reading that explains why the system is designed the way it is.

- [explanation/README.md](explanation/README.md) — Index
- [explanation/01-how-hooks-execute-works.md](explanation/01-how-hooks-execute-works.md) — The two-phase condition/effect model and schema design choices

## Command Groups at a Glance

| Group            | Purpose                                       |
| ---------------- | --------------------------------------------- |
| `unrdf graph`    | Create, load, query, and export RDF graphs    |
| `unrdf sparql`   | Execute SPARQL queries directly               |
| `unrdf context`  | Manage named graphs                           |
| `unrdf hooks`    | Evaluate, define, and execute knowledge hooks |
| `unrdf sync`     | Generate code from RDF ontologies             |
| `unrdf template` | Ad-hoc code generation from a single RDF file |
