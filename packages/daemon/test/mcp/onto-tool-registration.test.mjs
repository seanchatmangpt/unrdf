/**
 * @file MCP onto_* tool registration
 * Verifies the MCP server actually registers exactly the 15 onto_* tools
 * (.claude/rules/scoped/p2-mcp-tools.md) and that each has name/description/inputSchema.
 */
import { describe, it, expect } from 'vitest';
import { createMCPServer } from '../../src/mcp/index.mjs';
import { ontoRegistry } from '../../src/mcp/open-ontologies-registry.mjs';

const EXPECTED = [
  'onto_align', 'onto_apply', 'onto_clear', 'onto_convert', 'onto_drift',
  'onto_load', 'onto_marketplace', 'onto_plan', 'onto_query', 'onto_reason',
  'onto_save', 'onto_shacl', 'onto_stats', 'onto_validate', 'onto_version',
];

describe('MCP onto_* tool registration', () => {
  const server = createMCPServer();
  const registered = server._registeredTools ?? {};
  const ontoNames = Object.keys(registered).filter(n => n.startsWith('onto_')).sort();

  it('registers exactly 15 onto_* tools on the server', () => {
    expect(ontoNames).toHaveLength(15);
    expect(ontoNames).toEqual(EXPECTED);
  });

  it('registry and server agree', () => {
    expect(Object.keys(ontoRegistry).sort()).toEqual(EXPECTED);
  });

  it('uses snake_case onto_ names and documents every tool', () => {
    for (const name of ontoNames) {
      expect(name).toMatch(/^onto_[a-z]+$/);
      expect(registered[name].description).toBeTruthy();
      expect(registered[name].inputSchema).toBeDefined();
    }
  });
});
