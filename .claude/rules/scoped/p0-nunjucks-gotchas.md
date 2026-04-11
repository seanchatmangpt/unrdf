# Scoped Rule: Nunjucks Template Gotchas

**Scope**: All `.njk` template files in `/Users/sac/ostar/templates/` used by unrdf rules

## Filter Syntax

**CRITICAL**: Nunjucks uses parentheses `filter(arg)`, not colons `filter: arg` (which is Ruby/Handlebars syntax).

```nunjucks
{# WRONG — Ruby/Handlebars syntax #}
{{ items | join: ', ' }}
{{ name | snake_case }}

{# RIGHT — Nunjucks syntax #}
{{ items | join(', ') }}
{{ name | snakeCase }}
```

## Variable Naming in Templates

SPARQL results are transformed to remove the `?` prefix from variable names. Both forms are available:

```nunjucks
{# Both work (variable 'system' from SPARQL SELECT ?system) #}
{{ system }}
{{ ?system }}

{# Prefer the clean name without ? #}
{{ system | snakeCase }}
```

**Case sensitivity matters**: `{{ system }}` is different from `{{ System }}`. Match the SPARQL SELECT binding exactly.

## Template Output Paths

When a template has a `to:` field in frontmatter AND an unrdf rule specifies `output_file`, the **rule takes precedence**:

```yaml
# Template frontmatter
---
to: '{{ agent_name }}_spec.md'
---
# Rule config
output_file = "docs/agents/{{ agent }}_spec.md"
# Result: Rule's output_file is used, with variable from SPARQL SELECT ?agent
```

This allows rules to override template defaults based on actual SPARQL bindings.

## Auto-Quoting in Frontmatter

The `preprocessFrontmatter()` function auto-quotes values containing `{{ }}` in YAML frontmatter for js-yaml 3.x compatibility. This is transparent — no action needed:

```yaml
# Before preprocessing
to: {{ output_dir }}/generated/file.ex

# After preprocessing (auto-quoted)
to: "{{ output_dir }}/generated/file.ex"

# Both work fine in templates
```

## Common Errors

| Error                                  | Cause                                        | Fix                                                         |
| -------------------------------------- | -------------------------------------------- | ----------------------------------------------------------- |
| `expected variable end` on filter      | Using `filter: arg` syntax                   | Change to `filter(arg)` with parentheses                    |
| Template renders but variable is empty | Variable name doesn't match SPARQL binding   | Check SELECT clause in rule's query; use exact case         |
| Output file has wrong directory        | Template `to:` overriding rule `output_file` | Ensure rule `output_file` is passed as `context.outputPath` |
| `Unknown configuration key` warning    | KNOWN_CONFIG_KEYS map missing a field        | Add field to config-parser.mjs KNOWN_CONFIG_KEYS            |

## Files Affected

- All templates in `/Users/sac/ostar/templates/`
- Rules in `OSA/unrdf.toml`, `BusinessOS/unrdf.toml`, `canopy/unrdf.toml`
- Infrastructure: `unrdf/packages/cli/src/cli/commands/sync/template-renderer.mjs`
