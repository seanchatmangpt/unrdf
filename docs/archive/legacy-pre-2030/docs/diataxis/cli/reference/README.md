# Reference

Reference material is information-oriented. It describes what exists precisely and completely. Use this section when you need to know the exact name of a flag, the legal values for a field, or the structure of a JSON schema.

## Reference Documents

| Document                                       | Contents                                                                            |
| ---------------------------------------------- | ----------------------------------------------------------------------------------- |
| [commands.md](commands.md)                     | Every command group, subcommand, and flag with types and defaults                   |
| [hook-config-schema.md](hook-config-schema.md) | KnowledgeHookSchema JSON field reference: required fields, types, valid enum values |

## Conventions

- `<value>` — required argument
- `[value]` — optional argument
- Flags with no `=` take a value: `--store <file>` means pass `--store path/to/file.ttl`
- Boolean flags default to `false` unless noted otherwise
